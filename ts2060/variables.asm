; new string variable assignment
NSTRNG:	JR	NC,FRSTR	; first assignment
	AND	$E0
	JR	Z,RSTRNG	; re-assignment of long-named string
	RST	$10		; back to ROM1 for short names
FRSTR:	LD	HL,-7
	ADD	HL,BC
	JR	C,LSTRNG	; first assignment of long-named string
	RST	$10		; back to ROM1 for short names

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
	JR	C,SW_STR	; jump if global variable
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

; Long-named string variables
LV_CONT:JR	C,LV_EXIT
	RST	$30
	DEFW	L28B2		; LOOK-VARS
	PUSH	HL
	EX	AF,AF'
	RST	$18
	CP	"$"
	JR	NZ,LV_NSTR
	; long string not found
	RST	$20		; skip over "$"
	RES	6,(IY+FLAGS-ERR_NR)	; indicate string
	CALL	SYNTAX_Z
	SCF
	JR	NZ,SW_LV2	; in runtime, long string not found CF=1,ZF=0
	AND	A		; in syntax check, long string found CF=0,ZF=0
	DEFB	$3E		; LD A,skip next byte
LV_NSTR:EX	AF,AF'
SW_LV2:	POP	HL
LV_EXIT:RST	$10
