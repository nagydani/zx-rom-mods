; Copy quoted literals verbatim
TOK_Q:	LDI
	LD	A,(HL)
	CP	$0D
	RET	Z
	CP	"\""
	JR	NZ,TOK_Q
	LDI
	LD	A,(HL)
	RET

; Tokenize input line in place
; Input: HL line address
TOKINP:	PUSH	HL
	LD	BC,$100		; only one token
	LD	DE,X017F
	RST	$28
	DEFW	FTOKEN_R1	; search in ROM1
	XOR	A
	CP	C
	POP	DE
	JR	Z,TOK_L2
	LD	A,STOP_T
	LD	(DE),A
	INC	DE
	ADD	HL,BC
	JR	TOK_L2

; Tokenize a program line in place
; Input: HL line address
TOKPRG:	LD	E,L
	LD	D,H
TOK_L1:	LD	A,(HL)
	BIT	7,A
	JR	NZ,TOK_CF
	CP	$40
	JR	NC,TOK_INS
	CP	"\""
	CALL	Z,TOK_Q		; skip quoted expression
	RST	$28
	DEFW	L2D1B		; NUMERIC
	JR	C,TOK_NN	; not a number
	LDI
	CALL	OSPACE
	JR	TOK_L1

TOK_NN:	LDI
	CP	$0D
	JR	NZ,TOK_L1
	RST	$28
	DEFW	L19E5		; RECLAIM-1
	RET
TOK_INS:PUSH	DE
	LD	DE,TOKENS1 + 1
	LD	BC,$2900	; 41 * 256
	CALL	FTOKEN
	LD	DE,X0119	; DEF FN token
	LD	B,$32		; 50
	ADD	A,B
	CP	$42		; POKE ?
	JR	Z,TOK_NP	; use it, don't search in ROM1
	RST	$28
	DEFW	FTOKEN_R1	; search in ROM1
TOK_NP:	LD	B,A
	XOR	A
	CP	C
	LD	A,B
	POP	DE
	JR	C,TOK_CI
	LD	A,(HL)
	JR	TOK_IF

TOK_CI:	DEC	A
	CPL
	LD	(DE),A
	INC	DE
	LD	B,0
	ADD	HL,BC
	CP	ELSE_T
	JR	Z,TOK_OS1
	CALL	OSPACE
TOK_L2:	LD	A,(HL)
	BIT	7,A
TOK_CF:	JR	NZ,TOK_IF
	CP	"\""
	CALL	Z,TOK_Q
	CP	":"
	JR	Z,TOK_L1
	CP	$0D
	JR	Z,TOK_NN
	PUSH	DE
	LD	DE,L0095 + 1
	LD	BC,$2900	; 41 * 256
	RST	$28
	DEFW	FTOKEN_R1	; search in ROM1
	CP	$03		; THEN ?
	JR	NZ,TOK_CX	; jump forward, if not
	LD	A,THEN_T
	LD	B,0
	ADD	HL,BC
	POP	DE
	LD	(DE),A		; store THEN token
	INC	DE
TOK_OS1:CALL	OSPACE
TOK_L11:JR	TOK_L1		; continue with an instruction
TOK_CX:	LD	DE,TOKENS0 + 1
	LD	B,$18		; 24
	ADD	A,B
	CALL	FTOKEN
	LD	B,A
	XOR	A
	CP	C
	LD	A,B
	POP	DE
	JR	C,TOK_CO
TOK_IF:	LDI
	CP	THEN_T
	JR	NZ,TOK_L2
	JR	TOK_L11
TOK_CO:	SUB	A,$E7
	CPL
	LD	(DE),A
	INC	DE
	LD	B,0
	ADD	HL,BC
	CALL	OSPACE
	JR	TOK_L2

; Optional space
OSPACE:	LD	A,(HL)
	CP	" "
	RET	NZ
	INC	HL
	LD	A,(HL)
	RET

; Find token (64 bytes)
; Input: HL text to match, DE token table, B number of tokens in the table, C = 0
; Output: A remaining tokens in the table at longest full match, C length of match
FTOKEN:	PUSH	AF
FTOKENL:PUSH	BC
	PUSH	HL
	LD	BC,$A000
; Test one keyword
; Input: HL text to match, DE keyword to check, BC=$A000
; Output: CF set iff keyword matches fully, C length of match, DE next keyword
TESTKW:	LD	A,(DE)
	CP	B		; final space is not matched
	JR	Z,TESTKW1
	AND	$7F		; remove end marker
	RST	$28
	DEFW	L2C8D		; ALPHA
	ADC	A,A
	RRCA
	XOR	(HL)
	ADD	A,A
	JR	NC,TESTKW0	; not a letter
	AND	$BF		; capitalize
TESTKW0:JR	NZ,SPACEKW
	INC	C		; increment length
	LD	A,(DE)
TESTKW1:INC	DE
	ADD	A
	JR	C,FTOKEN1	; token found
	INC	HL
	JR	TESTKW
SPACEKW:LD	A,(DE)
	INC	DE
	CP	" "		; spaces inside keywords are optional
	JR	Z,TESTKW
	DEC	DE
NEXTKW:	LD	A,(DE)
	INC	DE
	ADD	A,A
	JR	NC,NEXTKW
	POP	HL
	POP	BC
FTOKEN0:DJNZ	FTOKENL
	POP	AF
	RET
FTOKEN1:LD	A,C
	POP	HL
	POP	BC
	CP	C
	JR	C,FTOKEN0
	LD	C,A
	POP	AF
	LD	A,B
	DJNZ	FTOKEN
	RET
