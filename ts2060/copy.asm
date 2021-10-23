COPY_B:	LD	C,(HL)
	LD	B,4
COPY_BL:RL	C
	RLA
	RL	C
	JR	NC,COPY_B0
	OR	1
COPY_B0:DJNZ	COPY_BL
	RET

COPY32:	DI
	LD	B,$C0
	LD	HL,L0EAF
	PUSH	HL
	RST	$10

ZX_COPY:IN	A,($FF)
	AND	$04
	JR	Z,COPY32
COPY64:	LD	B,$18
COPY64L:PUSH	BC
	RST	$30
	DEFW	L0E9B		; CL-ADDR
	LD	D,(IY+PR_CC+1-ERR_NR)
	LD	E,0
	LD	C,8
C64_LL:	LD	B,$20
	PUSH	HL
C64_LR:	PUSH	BC
	CALL	COPY_B
	SET	5,H
	CALL	COPY_B
	RES	5,H
	LD	(DE),A
	INC	L
	INC	E
	POP	BC
	DJNZ	C64_LR
	POP	HL
	INC	H
	DEC	C
	JR	NZ,C64_LL
	RST	$30
	DEFW	L0ECD
	POP	BC
	DJNZ	COPY64L
	RST	$10
