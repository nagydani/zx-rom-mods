REPORT:	CP	MAX_ERR
	JR	C,REPORTZ
	SUB	$81
	LD	(ERR_NR),A
	EX	DE,HL
REPORTL:LD	A,(DE)		; Find end of command line
	INC	DE
	CP	$80
	JR	Z,MESSAGE
	CP	$0E
	JR	NZ,REPORTL
	INC	DE
	INC	DE
	INC	DE
	INC	DE
	INC	DE
	JR	REPORTL

STDERR_MSG:
	XOR	A
	PUSH	DE
	RST	$30
	DEFW	L1601
	POP	DE
	JR	MESSAGE

REPORTZ:SUB	$1C
	LD	B,A
	INC	B
	ADD	"S"
	RST	$30
	DEFW	L0010
	LD	A," "
	RST	$30
	DEFW	L0010
	LD	DE,REPORTS
TOKEN:	LD	A,(DE)
	ADD	A,A
	INC	DE
	JR	NC,TOKEN
	RET	Z		; end of token table
	DJNZ	TOKEN
TOKEN_S:LD	A,(DE)
	CP	" "
	JR	NZ,MSGNSP
MSG_SP:	BIT	0,(IY+$01)
	JR	NZ,MSGSKIP
MESSAGE:LD	A,(DE)
MSGNSP:	AND	$7F
	RST	$30
	DEFW	L0C3B		; PO-SAVE
	LD	A,(DE)
MSGSKIP:INC	DE
	ADD	A,A
	JR	NC,MESSAGE
	RES	0,(IY+$01)	; allow leading space
	CP	$40		; 2 * " "
	RET	NZ
	INC	A		; clear Z
NOLEAD:	SET	0,(IY+$01)	; suppress leading space
	RET

SYNTAX_Z:EQU	L2530

UNSTACK_Z:
	CALL	SYNTAX_Z
	POP	HL
	RET	Z
	JP	(HL)

