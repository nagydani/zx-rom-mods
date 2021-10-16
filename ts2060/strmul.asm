D_STRING:
	POP	HL		; discard return address
	POP	HL		; RE-ENTRY
	POP	DE		; discard BREG?
	POP	DE		; discard USR
	LD	DE,$106F	; CHR$, a num-to-string function
	PUSH	DE
	LD	DE,0
	PUSH	DE		; BREG = 0
	PUSH	HL
	RST	$28
	DEFB	$38		; end
	INC	HL
	BIT	7,(HL)		; check sign
	DEC	HL
	PUSH	AF		; Z clear, if negative
	JR	Z,D_NFLIP
	RST	$30
	DEFW	L346E + 4	; NEGATE + 4
D_NFLIP:DEC	HL
	LD	B,(HL)
	DEC	HL
	LD	C,(HL)		; string length to BC
	PUSH	BC
	RST	$30
	DEFW	L2D2B + 4	; STACK-BC
	RST	$28
	DEFB	$04		; multiply
	DEFB	$38		; end
	RST	$30
	DEFW	L1E99		; FIND-INT2
	POP	HL
	SBC	HL,BC
	EX	DE,HL
	DEC	HL
	LD	(HL),B
	DEC	HL
	LD	(HL),C
	DEC	HL
	JR	C,D_SLONG
	LD	D,(HL)
	DEC	HL
	LD	E,(HL)
	POP	AF		; restore sign in Z
	JR	Z,SMUL_E
	PUSH	DE
	RST	$30
	DEFW	L0030		; BC-SPACES
	POP	HL
	PUSH	DE
	PUSH	BC
	LDIR
	POP	BC
	POP	HL
	PUSH	HL
	CALL	MIRROR
	POP	DE
	LD	HL,(STKEND)
	DEC	HL
	DEC	HL
	DEC	HL
	LD	(HL),D
	DEC	HL
	LD	(HL),E
SMUL_E:	LD	DE,(STKEND)
	RST	$10

D_SLONG:PUSH	HL		; address pointer
	PUSH	DE		; excess length
	RST	$30
	DEFW	L0030		; BC-SPACES
	POP	HL
	LD	(MEMBOT+28),HL	; save excess length
	ADD	HL,BC		; HL is old length
	EX	(SP),HL		; retrieve address pointer
	ADD	HL,BC		; stack has moved
	LD	B,(HL)
	LD	(HL),D
	DEC	HL
	LD	C,(HL)
	LD	(HL),E
	LD	H,B
	LD	L,C
	POP	BC
	PUSH	DE
	LDIR
	POP	HL
	LD	A,(MEMBOT+28)
	CPL
	LD	C,A
	LD	A,(MEMBOT+29)
	CPL
	LD	B,A
	INC	BC
	LDIR
	POP	AF
	JR	Z,SMUL_E
	CALL	FETCH
	EX	DE,HL
	CALL	MIRROR
	JR	SMUL_E
