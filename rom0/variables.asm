; Variables on stack
; $3Exx
; 00 end of stack
; 01..1A string array: 2 bytes of length + 1 byte dimensions + content
; 21..3A numeric array: 2 bytes of length + 1 byte dimensions + content
; 41..5A simple string:	2 bytes of max length + 2 bytes of actual length + content
; 61..7A simple numeric: 5 bytes

; 7B REPEAT, 7 bytes: (NXTLIN)-(PROG), (CHADD)-(PROG), (PPC), (SUBPPC)
; 7C WHILE, 9 bytes: (NXTLIN)-(PROG), (CHADD)-(PROG), (PPC), (SUBPPC), error
; 7D PROC, 9 bytes: (DATADD)-(PROG), (NXTLIN)-(PROG), (PPC), (SUBPPC), error
; 7E ON ERROR, 5 bytes (CHADD)-(PROG), (PPC), (SUBPPC)
; 7F ERROR, 4 bytes (ERRNO)+1, (PPC), (SUBPPC)

; 81..9A string function (reserved)
; A1..BA numeric function (reserved)
; E1..FA for loop variable: 22 bytes: value, target, step, (PPC), (SUBPPC), (NXTLIN-PROG), (CHADD)-(PROG)


REPEAT_M:	EQU	$7B
WHILE_M:	EQU	$7C
PROC_M:		EQU	$7D
ONERROR_M:	EQU	$7E
ERROR_M:	EQU	$7F

; Skip all local variables, incl. loops
SKIP_LL:CALL	SKIP_LC
SKIPLL:	CP	ERROR_M + 1
	RET	C
	LD	DE,$0017
	ADD	HL,DE
	CALL	LOC_L
	JR	SKIPLL

; Skip local variables excl. loops
SKIP_LC:LD	C,0		; this will never be found

; Look up local variables
; Input: C variable discriminator (a:$61, a():$01, a$:$41)
; Output: CF set, if variable found, HL pointing to variable found or next candidate, A context type $3F for GO SUB
LOOK_LC:LD	HL,(ERR_SP)
	INC	HL
	INC	HL		; skip error address
LOC_L:	LD	A,$3E + 1
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	D		; this is necessary because GO SUB can come from command line
	CP	D
	JR	NZ,LOC_GS	; regular GO SUB stack entry, local variable not found
	LD	A,E
	OR	A
	RET	Z		; end-of-stack, local variable not found
	CP	REPEAT_M
	JR	C,LOC_VAR
	CP	ERROR_M + 1
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
LOC_TAB:DEFB	$08, $0A, $0A, $06, $05

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
	RET

; Look up local variables
LOCAL_CONT:
	DEC	A
	JR	NZ,FN_ARG
	LD	HL,7
	ADD	HL,SP
	LD	A,(HL)
	CP	$06		; called from LOAD/SAVE?
	JR	Z,LC_SW
	CP	$2C		; called from DIM?
	JR	Z,LC_SW

LC_DO:	CALL	LOOK_LC
LC_LL:	JR	C,LC_FND
	OR	A
	JR	Z,LC_NOTF
	ADD	A,A
	JR	Z,LC_LOOP
	JR	NC,LC_LOOP	; not a loop variable
	LD	DE,$0017	; skip loop variable
	ADD	HL,DE
LC_LOOP:CALL	LOC_L
	JR	LC_LL
LC_NOTF:LD	HL,L28EF
LC_JP:	EX	(SP),HL		; replace the return address with V_RUN_SYN
LC_SW:	JP	SWAP

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
	JR	LC_SW

LC_ARR:	CP	A		; set Z flag
	LD	C,$7F
LC_NUM:	EX	DE,HL
LC_STRR:POP	DE		; discard pointer
	JR	LC_SW

; string variable assignment L-DELETE
STRNG_CONT:
	AND	A
	SBC	HL,SP
	ADD	HL,SP
	JR	C,LC_SW		; global variable
	BIT	0,(IY+$37)	; FLAGX, complete string
	JR	Z,LC_SW
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
	JP	SWAP

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
	LD	BC,22		; enough space for a FOR loop
	RST	$30
	DEFW	L1F05		; TEST-ROOM
	POP	DE		; discard return address
	POP	DE		; discard return address
	POP	DE		; save return address
	POP	BC		; save error address
	LD	HL,-22		; 22 bytes for a FOR loop
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
	JR	LF_SWAP

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
LF_SWAP:JP	SWAP
