; Find token in either ROM
; Input: B length of text to match, HL address of text to match
; Output: A token code matched or zero, if none; CF token matched fully
TOK_F:	LD	A,(HL)
	CP	" "
	JR	NZ,TOK_IN0
	INC	HL
	DJNZ	TOK_F
	RET

TOK_IN0:LD	DE,T_ELSE
	LD	C,RND_T-$7F	; new tokens
	CALL	FTOKEN
	JR	C,TOK_TF	; full token found
	LD	C,$100-RND_T	; old tokens
	OR	A
	JR	Z,TOK_IN	; no new token found
	ADD	A,C
	ADD	A,A
	JR	C,TOK_EL	; partially matched ELSE
TOK_IN:	EX	AF,AF'
	LD	DE,L0095+1	; RND token
	RST	$30
	DEFW	FTOKENL_R1	; search in ROM1
	RR	D		; save CF
	DEC	A
	CPL
	RL	D
	RET
TOK_TF:	ADD	A,$5A
	CPL
	CP	$7F
	SCF
	RET	NZ
TOK_EL:	LD	A,ELSE_T
	RET


;Find token
; Input: HL text to match, B length of text, DE token table, C number of tokens in the table
; Output: A remaining tokens in the table at full match, CF token matched fully
FTOKEN:	XOR	A
	EX	AF,AF'		; clear A' and CF'
FTOKENL:PUSH	BC
	PUSH	HL
	LD	C,0
	CALL	TESTKW
	RR	H
	EX	AF,AF'
	RL	H
	EX	AF,AF'		; move CF to CF'
	POP	HL
	LD	A,C
	POP	BC
	CP	B
	JR	NZ,FTOK_N	; no full match
	EX	AF,AF'
	BIT	0,A
	JR	NZ,FTOK_F	; do not overwrite full matches
	LD	A,C
	ADC	A,A
FTOK_F:	EX	AF,AF'
FTOK_N:	DEC	C
	JR	NZ,FTOKENL
	EX	AF,AF'
	SRL	A
	RET

; Test one keyword
; Input: HL text to match, B length of text, DE keyword to check, C=0
; Output: CF set iff keyword matches fully, C length of match, DE next keyword
TESTKW:	LD	A,(DE)
	CP	$A0		; final space is not matched
	JR	Z,TESTKW1
	AND	$7F		; remove end marker
	RST	$30
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
	RET	C		; full match
	INC	HL
	DJNZ	TESTKW
	LD	A,(DE)
	INC	DE
	CP	$80 + " "
	CCF
	RET	Z
	ADD	A,A
	CCF
	RET	NC
TESTKW2:LD	A,(DE)
	INC	DE
	ADD	A,A
	JR	NC,TESTKW2
	AND	A		; partial match
	RET

SPACEKW:LD	A,(DE)
	INC	DE
	CP	" "		; spaces inside keywords are optional
	JR	Z,TESTKW
	DEC	DE
	JR	TESTKW2

; First non-whitespace of a token in this ROM
; In: DE=token table-1, B=token code+1
; Out: A=character code
FC_TOKEN_R0:
	LD	A,(DE)
	INC	DE
	ADD	A,A
	JR	NC,FC_TOKEN_R0
	DJNZ	FC_TOKEN_R0
FC_WSP:	LD	A,(DE)
	CP	" "
	RET	NZ
	INC	DE
	LD	A,(DE)
	AND	$7F
	RET

; First non-whitespace of a token in ROM1
; In: DE=token table-1, A=token code
; Out: A=character code
FC_TOKEN_R1:
        RST     $30
        DEFW    L0C41           ; PO-SEARCH
        EX      DE,HL
FC_TR1:	RST     $30
        DEFW    X007B           ; LD A,(HL)
        CP      " "
        RET	NZ
        INC     HL
; works only with these particular tokens
	JR	FC_TR1
; correct, but too long
;	RST     $30
;	DEFW    X007B           ; LD A,(HL)
;	AND     $7F
;	RET

