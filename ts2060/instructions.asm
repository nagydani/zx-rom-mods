	DEFB	P_PLAY - $	; PLAY
P_END:	EQU	$

P_PLAY:	DEFB	$05
	DEFW	PLAY

CHECK_END:
	CALL	SYNTAX_Z
	RET	NZ
END05_E:POP	BC		; SCAN-LOOP
END05:	POP	BC		; STMT-RET
STMT_NEXT:
	RST	$18
	CP	$0D		; CR
	JR	Z,LINE_END
	CP	":"
	JR	Z,STMT_LOOP
	JR	ERROR_C_I

STMT_LOOP:
	LD	HL,L1B28	; STMT-LOOP
	PUSH	HL
	RST	$10

LINE_END:
	BIT	7,(IY+$01)
	JR	Z,LE_SWAP
	LD	HL,(NXTLIN)
	LD	A,$C0
	AND	(HL)
	JR	NZ,LE_SWAP	; program finished
	PUSH	HL
	LD	HL,L1BBF - 1	; XOR A, LINE-USE
	EX	(SP),HL
LE_SWAP:RST	$10

CMDCLASS2:
	DEFB	CLASS2_00 - $	; parameterless instruction
	DEFB	CLASS2_01 - $	; do something to a variable
	DEFB	CLASS2_02 - $	; prepare value for assignment
	DEFB	CLASS2_03 - $	; one numeric parameter, defaults to zero
	DEFB	CLASS2_04 - $	; used by FOR & NEXT -- TODO: may be worth replacing
	DEFB	CLASS2_05 - $	; list of items
	DEFB	CLASS2_06 - $	; evaluate single numeric expression
	DEFB	CLASS2_07 - $	; open #2 or other stream before execution
	DEFB	CLASS2_08 - $	; two numeric expressions, separated by comma

CLASS2_03:
	CALL	FETCH_NUM
CLASS2_00:
	CP	A
CLASS2_05:
	POP	BC
	CALL	Z,CHECK_END
	EX	DE,HL
	LD	HL,(T_ADDR)
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	EX	DE,HL
	PUSH	BC
	RET

CLASS2_01:
	RST	$30
	DEFW	L1C1F		; CLASS_01
	RET

CLASS2_02:
	RST	$30
	DEFW	L1C4E		; CLASS_02
	RET

CLASS2_04:
	RST	$30
	DEFW	L1C6C		; CLASS_04
	RET

NEXT_2NUM:
	RST	$20

CLASS2_08:
	CALL	CLASS2_06
	CP	","
	JR	NZ,ERROR_C_I
NEXT_1NUM:
	RST	$20

CLASS2_06:
	RST	$30
	DEFW	L24FB		; SCANNING
	BIT	6,(IY+$01)
	RET	NZ

ERROR_C_I:
	JP	ERROR_C

FETCH_NUM:
	CP	$0D
	JR	Z,USE_ZERO
	CP	":"
	JR	NZ,CLASS2_06

USE_ZERO:
	RST	$30
	DEFW	L1CE6		; USE-ZERO
	RET

CLASS2_07:
	RST	$30
	DEFW	L2070		; STR-ALTER
	RET	NC
	LD	HL,(T_ADDR)
	LD	A,(HL)
	OR	A
	JR	Z,CL7_E
	INC	HL
	LD	(T_ADDR),HL
CL7_E:	CALL	UNSTACK_Z
	LD	A,2
	RST	$30
	DEFW	L1601	; CHAN-OPEN
	RET

; instruction routines
	INCLUDE	"play.asm"
