; Variables on stack
; $3Exx
; 00 end of stack
; 01..1A string array: 2 bytes of length + 1 byte dimensions + content
; 21..3A numeric array: 2 bytes of length + 1 byte dimensions + content
; 41..5A simple string:	2 bytes of max length + 2 bytes of actual length + content
; 61..7A simple numeric: 5 bytes
; 81..9A string reference: 2 bytes of length + symbolic reference
; A1..BA numeric reference: 2 bytes of length + symbolic reference
; E1..FA for loop variable: 22 bytes: value, target, step, (PPC), (SUBPPC), (NXTLIN-PROG), (CHADD)-(PROG)
; FB REPEAT, 7 bytes: (NXTLIN)-(PROG), (CHADD)-(PROG), (PPC), (SUBPPC)

; Look up local variables
LOOK_LC:LD	HL,(ERR_SP)
	LD	D,$3E
	RES	2,(IY+$37)	; local variable
	JR	LOC_ST
LOC_GS:	SET	2,(IY+$37)	; upvalue of GO SUB
LOC_ST:	INC	HL
	INC	HL
LOC_L:	LD	E,(HL)
	INC	HL
	LD	A,(HL)
	CP	D
	JR	NZ,LOC_GS	; regular GO SUB stack entry
	LD	A,E
	OR	A
	SET	3,(IY+$37)	; indicate
	RET	Z		; no local variable found
	CP	$FB
	JR	Z,LOC_REP	; REPEAT entry
	BIT	7,A
	JR	Z,LOC_SA	; simple variables and arrays
	SUB	$E0
	JR	NC,LOC_SA	; loop variables
	ADD	$A0		; references
LOC_SA:	CP	C
	JR	NZ,LOC_NX
	AND	A		; clear carry
	RES	3,(IY+$37)	; indicate
	RET			; local variable found
LOC_REP:LD	DE,$0008
LOC_UP:	SET	2,(IY+$37)	; upvalue
	JR	LOC_SK
LOC_NX:	LD	A,E
	CP	$E0
	LD	DE,$0017
	JR	NC,LOC_UP	; skip loop variable
	CP	$60
	JR	C,LOC_SKL	; skip arrays and strings
	ADD	A,A
	LD	E,$06
	JR	NC,LOC_SK	; skip numeric variables
LOC_SKL:INC	HL		; skip references
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
LOC_SK:	ADD	HL,DE
	JR	LOC_L

; skip local variables
; Output: HL=marker address
SKIP_LC:LD	HL,(ERR_SP)
	LD	A,$3E
	LD	C,$E0
	INC	HL
	INC	HL
SKIP_L:	INC	HL
	CP	(HL)
	DEC	HL
	RET	NZ		; GO SUB entry
	LD	A,(HL)
	CP	C
	RET	NC		; other local context
	AND	C
	CP	$60
	LD	DE,$0005
	JR	Z,SKIP_N
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
SKIP_N:	ADD	HL,DE
	JR	SKIP_L
