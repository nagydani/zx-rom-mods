LABEL:	CALL	SYNTAX_Z
	JR	NZ,LABEL_R
	LD	BC,$0006
	RST	$30
	DEFW	L2C8D		; ALPHA
	JP	NC,ERROR_C
	INC	HL		; insert pointers after first letter of label
	RST	$30
	DEFW	L1655		; MAKE-ROOM
	LD	DE,(E_LINE)
LABEL_S:LD	A,(DE)
	INC	DE
	CP	" " + 1
	JR	C,LABEL_S	; skip indent
LABEL_N:RST	$30
	DEFW	L2D1B		; NUMERIC
	JR	C,LABEL_C	; if CF then DE = beginning of first statement
	INC	DE
	LD	A,(DE)
	JR	LABEL_N		; skip line number
LABEL_C:LD	A,(SUBPPC)
	INC	HL
	LD	(HL),$0E	; number marker
	INC	HL
	PUSH	HL		; length goes here
	INC	HL
	INC	HL		; skip length
	LD	(HL),A		; statement number
	INC	HL
	EX	DE,HL
	SCF
	SBC	HL,DE		; relative pointer to before first statement
	EX	DE,HL
	LD	(HL),E
	INC	HL
	LD	(HL),D
	LD	(CH_ADD),HL
LABEL_L:RST	$20
	RST	$30
	DEFW	L2C88		; ALPHANUM
	JR	C,LABEL_L
	POP	DE		; length pointer
	SCF
	SBC	HL,DE
	EX	DE,HL
	LD	(HL),E
	INC	HL
	LD	(HL),D
	JP	END05

LABEL_R:INC	HL
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	ADD	HL,DE
	LD	(CH_ADD),HL
	RST	$10

RSTLBLS:LD	HL,(PROG)
	LD	DE,$0005
NX_LIN:	LD	A,(HL)
	AND	$C0
	RET	NZ
	ADD	HL,DE
	DEC	HL		; skip line number and length
NX_INS:	LD	A,(HL)
	INC	HL
	CP	$21
	JR	C,NX_INS	; skip whitespace before instruction
NX_CHR:	LD	A,(HL)
	INC	HL
	CP	$0D
	JR	Z,NX_LIN
	CP	":"
	JR	Z,NX_INS
	CP	THEN_T
	JR	Z,NX_INS
	CP	"\""
	JR	Z,SKQUOT
	CP	$0E
	JR	Z,SK_NUM
	CP	"@"
	JR	NZ,NX_CHR
RST_PR:	LD	A,$0E
SK_LBL:	CP	(HL)
	INC	HL
	JR	NZ,SK_LBL
	LD	B,E
RST_LBL:LD	(HL),D
	INC	HL
	DJNZ	RST_LBL
	JR	NX_CHR

SKQUOT:	CP	(HL)
	INC	HL
	JR	NZ,SKQUOT
	JR	NX_CHR

SK_NUM:	ADD	HL,DE
	JR	NX_CHR
