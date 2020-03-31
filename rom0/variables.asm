; Variables on stack
; $3Exx
; 00 end of stack
; 01..1A string array: 2 bytes of length + 1 byte dimensions + content
; 21..3A numeric array: 2 bytes of length + 1 byte dimensions + content
; 41..5A simple string:	2 bytes of max length + 2 bytes of actual length + content
; 61..7A simple numeric: 5 bytes

; 7B REPEAT, 3 bytes: (PPC), (SUBPPC)
; 7C WHILE, 5 bytes: (PPC), (SUBPPC), error
; 7D PROC, 7 bytes: (DATADD)-(PROG), (PPC), (SUBPPC), error
; 7E ON ERROR (reserved)
; 7F ERROR (reserved)

; 81..9A string function (reserved)
; A1..BA numeric function (reserved)
; E1..FA for loop variable: 18 bytes: value, target, step, (PPC), (SUBPPC)


REPEAT_M:	EQU	$7B
WHILE_M:	EQU	$7C
PROC_M:		EQU	$7D

; Skip all local variables, incl. loops
SKIP_LL:CALL	SKIP_LC
SKIPLL:	CP	$E0
	RET	C
	LD	DE,$0013
	ADD	HL,DE
	CALL	LOC_L
	JR	SKIPLL

; Skip local variables excl. loops
SKIP_LC:LD	C,0		; this will never be found

; Look up local variables
; Input: C variable discriminator (a:$61, a():$01, a$:$41)
; Output: CF set, if variable found, HL pointing to variable found or next candidate, A context type $3E for GO SUB
LOOK_LC:LD	HL,(ERR_SP)
	INC	HL
	INC	HL		; skip error address
LOC_L:	LD	A,MM
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	CP	D
	JR	NZ,LOC_GS	; regular GO SUB stack entry, local variable not found
	LD	A,E
	OR	A
	RET	Z		; end-of-stack, local variable not found
	CP	REPEAT_M
	JR	C,LOC_VAR
	CP	PROC_M + 1
	JR	NC,LOC_VAR

	EX	DE,HL
	LD	HL,LOC_TAB - REPEAT_M
	PUSH	BC
	LD	C,A
	LD	B,0
	ADD	HL,BC
	LD	L,(HL)
	LD	H,B
	POP	BC
	ADD	HL,DE
	RET

LOC_VAR:LD	A,E
	AND	$7F		; treat loop variables as simple numerics
LOC_SA:	BIT	5,A
	JR	NZ,LOC_NM	; numeric
	OR	$40		; find string arrays as well
LOC_NM:	BIT	6,C
	JR	NZ,LOC_NA	; not an array
	SUB	$20
LOC_NA:	CP	C
	JR	NZ,LOC_NX
	SCF
	RET			; local variable found

; structure lengths + 1
LOC_TAB:DEFB	$04, $06, $08

LOC_NX:	LD	A,E
	CP	$E0
	RET	NC		; stop at loop variable, not ours
	CP	$60
	JR	C,LOC_SKL	; skip arrays and strings
	ADD	A,A
	LD	DE,$0006
	JR	NC,LOC_SK	; skip numeric variables
LOC_SKL:INC	HL		; skip functions
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
LOC_SK:	ADD	HL,DE
	JR	LOC_L

LOC_GS:	INC	HL		; skip line number
	INC	HL		; skip statement number
	AND	A		; not found!
	RET

; Look up local variables
LOCAL_CONT:
	DEC	A
	JR	NZ,FN_ARG
	LD	HL,7
	ADD	HL,SP
	LD	A,(HL)
	CP	$06		; called from LOAD/SAVE?
	JR	Z,LC_NOTF
	CP	$2C		; called from DIM?
	JR	Z,LC_NOTF

LC_DO:	CALL	LOOK_LC
LC_LL:	JR	C,LC_FND
	OR	A
	JR	Z,LC_NOTF
	ADD	A,A
	JR	Z,LC_LOOP
	JR	NC,LC_LOOP	; not a loop variable
	LD	DE,$0013	; skip loop variable
	ADD	HL,DE
LC_LOOP:CALL	LOC_L
	JR	LC_LL
LC_NOTF:LD	HL,L28EF	; V-RUN-SYN
LC_JP:	EX	(SP),HL		; replace the return address
LC_SW:	RST	$10

FN_ARG:	LD	HL,L2951	; STK-FN-ARK
	JR	LC_JP

LC_FND:	POP	DE		; discard return address
	POP	DE		; discard variable pointer
;;	LD	DE,L28EF	; V-RUN-SYN TODO: ???
	DEC	HL
	LD	A,(HL)
	INC	HL
	RLCA
	RLCA
	RLCA
	INC	A
	AND	$07
	PUSH	BC
	LD	C,A
	LD	B,0
	EX	DE,HL
	LD	HL,LC_TAB
	ADD	HL,BC
	LD	C,(HL)
	ADD	HL,BC
	POP	BC
	OR	H		; reset Z flag
	JP	(HL)

LC_TAB:	DEFB	LC_NUM - $	; FOR loop variable
	DEFB	LC_SARR - $	; string array
	DEFB	LC_NARR - $	; numeric array
	DEFB	LC_STR - $	; simple string
	DEFB	LC_NUM - $	; simple numeric

LC_SARR:LD	C,$7F
LC_NARR:LD	HL,2
	ADD	HL,SP
	LD	A,(HL)
	OR	A
	JR	Z,LC_ARR	; called from DELETE
	EX	DE,HL
	XOR	A		; signal array
	RST	$30
	DEFW	L29AE		; SV-ARRAYS
	OR	H		; clear both ZF and CF
	JR	LC_STRR

LC_STR:	EX	DE,HL
	INC	HL		; skip marker
	INC	HL		; skip first byte of max. length
	POP	DE		; discard pointer
	CP	A		; set Z flag
	RST	$10

LC_ARR:	CP	A		; set Z flag
	LD	C,$7F
LC_NUM:	EX	DE,HL
LC_STRR:POP	DE		; discard pointer
	RST	$10

; new string variable assignment
NSTRNG:	AND	$E0
	JR	Z,RSTRNG	; re-assignment of long-named string
	CP	$40
	JR	Z,LC_SW		; back to ROM1 for short names
	LD	HL,-7
	ADD	HL,BC
	JR	LSTRNG		; first assignment of long-named string
; long-named string variable reassignment
RSTRNG:	POP	AF		; discard return address
	POP	AF		; discard return address
	POP	AF		; discard zero AF
	POP	BC		; discard return address
	POP	BC		; length of old version
	POP	HL		; pointer to old version
	LD	DE,-2		; net variable name length - 2
	DEC	HL		; skip zero byte
	INC	BC		; account for it
RSTR_L:	INC	BC		; increment length
	INC	DE		; increment net variable name length
	DEC	HL		; step back with pointer
	BIT	6,(HL)
	JR	NZ,RSTR_L	; find beginning
	LD	A,(HL)		; first character in long name format
	LD	(DEST),HL	; point DEST to variable name
	PUSH	HL		; put back pointer to old version
	PUSH	BC		; put back length of old version
	LD	HL,L_ADDR
	PUSH	HL		; put back return address
	XOR	$E0		; change to short name format
	PUSH	AF		; put back first character
	LD	HL,L_STRR
	PUSH	HL		; put back return address
	PUSH	HL		; placeholder
	EX	DE,HL		; net length - 2 to HL
; long-named string variable assignment
LSTRNG:	INC	HL
	INC	HL
	PUSH	HL		; save net variable name length
	RST	$30
	DEFW	L2BF1		; STK_FETCH
	LD	(K_CUR),DE	; point cursor to string
	POP	HL		; restore net variable name length
	POP	AF		; discard return address
	POP	DE		; fetch return address
	POP	AF		; fetch first letter
	INC	DE		; skip POP AF
	PUSH	DE		; store return address
	PUSH	BC		; save string length
	PUSH	HL		; save net variable name length
	INC	HL
	LD	C,L
	LD	B,H
	RST	$30
	DEFW	MAKE_STRING
	EX	DE,HL
	INC	DE
	XOR	$E0		; indicate long variable name
	LD	(DE),A		; with the first character
	INC	DE
	XOR	A
	POP	HL		; restore net variable name length
	RST	$30
	DEFW	L_STORE
	POP	BC
	LD	DE,(K_CUR)
	XOR	A
	RST	$10

STRNG_CONT:
	JR	NZ,NSTRNG
; string variable assignment L-DELETE
	AND	A
	SBC	HL,SP
	ADD	HL,SP
	JR	C,SW_STR	; global variable
	BIT	0,(IY+$37)	; FLAGX, complete string
	JR	Z,SW_STR
	PUSH	HL
	RST	$30
	DEFW	L2BF1		; STK-FETCH
	POP	HL
	PUSH	HL
	DEC	HL
	LD	(HL),B
	DEC	HL
	LD	(HL),C
	DEC	HL
	LD	A,(HL)
	DEC	HL
	LD	L,(HL)
	LD	H,A
	DEC	HL
	SCF
	SBC	HL,BC
	JR	C,STRNG_LONG
	POP	HL
	EX	DE,HL
	LD	A,B
	OR	C
	JR	Z,STRNG_Z
	LDIR
STRNG_Z:POP	BC		; discard return value
	LD	A,(T_ADDR)
	CP	$7D		; LET?
	RET	NZ		; return, if not
SW_STR:	RST	$10

STRNG_LONG:
	PUSH	BC		; string length
	PUSH	HL		; room to make (negative)
	LD	A,L
	CPL
	LD	C,A
	LD	A,H
	CPL
	LD	B,A
	INC	BC		; room to make (positive)
	PUSH	DE		; source address
	RST	$30
	DEFW	L1F05		; TEST-ROOM
	POP	HL		; source address
	EXX
	POP	BC		; room to make (negative)
	LD	HL,(ERR_SP)
	ADD	HL,BC
	LD	(ERR_SP),HL
	LD	HL,0
	ADD	HL,SP
	LD	E,L
	LD	D,H
	ADD	HL,BC		; DE=SP, HL=destination
	EXX
	POP	BC		; string length
	EXX
	POP	BC		; target address
	LD	SP,HL
	EX	DE,HL
	PUSH	HL
	LD	L,C
	LD	H,B
	POP	BC
	AND	A
	SBC	HL,BC
	PUSH	HL
	LD	L,C
	LD	H,B
	POP	BC
	LDIR
	PUSH	DE
	EXX
	POP	DE
	PUSH	DE
	LDIR
	POP	HL
	DEC	HL
	LD	B,(HL)
	DEC	HL
	LD	C,(HL)
	DEC	HL
	INC	BC
	INC	BC
	LD	(HL),B
	DEC	HL
	LD	(HL),C
	POP	BC		; discard length
	POP	BC		; discard target address
	POP	BC		; discard return address
	JR	STRNG_Z

; LET substitute for FOR
FOR_CONT:
	LD	BC,18		; enough space for a FOR loop
	RST	$30
	DEFW	L1F05		; TEST-ROOM
	POP	DE		; discard return address
	POP	DE		; discard return address
	POP	DE		; save return address
	POP	BC		; save error address
	LD	HL,-18		; 18 bytes for a FOR loop
	ADD	HL,SP
	LD	(MEM),HL	; set calculator memory area
	LD	SP,HL
	PUSH	HL		; placeholder for marker
	PUSH	BC		; error address
	LD	(ERR_SP),SP
	PUSH	DE		; return address
	INC	HL
	INC	HL
	INC	HL
	INC	HL
	EX	DE,HL
	LD	HL,(STKEND)
	DEC	HL
	LD	BC,5
	LDDR
	INC	HL
	LD	(STKEND),HL
	LD	HL,(DEST)
	EX	DE,HL
	LD	(HL),$3E	; marker
	BIT	1,(IY+$37)
	JR	NZ,LF_GETN	; jump, if not found
	EX	DE,HL
	AND	A
	SBC	HL,SP
	ADD	HL,SP
	JR	NC,LF_GLOB
	DEC	HL
LF_GLOB:DEC	HL
	EX	DE,HL
LF_GETN:LD	A,(DE)
	AND	$1F
	OR	$E0
	DEC	HL
	LD	(HL),A		; marker-discriminator
	LD	DE,$0007
	ADD	HL,DE		; HL points to limit
	LD	DE,L1D34	; F-L-S
	PUSH	DE
	RST	$10

; Check local variables for NEXT
NEXT_CONT:
	AND	A
	SBC	HL,SP
	ADD	HL,SP
	JR	C,LF_SWAP	; not local
	DEC	HL
	BIT	7,(HL)
	JR	Z,LF_SWAP	; not a loop variable
	INC	HL
	EX	(SP),HL
	LD	HL,X1DB9	; continue with NEXT
	EX	(SP),HL
LF_SWAP:RST	$10
