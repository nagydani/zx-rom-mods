TEMPS_CONT:
	IN	A,($FF)
	LD	HL,BORDCR
	XOR	(HL)
	OR	$38
	XOR	(HL)
	OUT	($FF),A		; Set BORDER for TIMEX HiRes
	RST	RST10

FLASHING:
	LD	A,D
	EXX
	LD	HL,(ATTR_T)
	PUSH	HL
	RES	7,H
	SET	7,L
	LD	(ATTR_T),HL
	LD	HL,(P_FLAG)
	PUSH	HL
	SET	2,L
	LD	(P_FLAG),HL
	RST	$30
	DEFW	$0010
	POP	HL
	LD	(P_FLAG),HL
	POP	HL
	LD	(ATTR_T),HL
	EXX
	RST	$10

K_RST:	RES	5,(IY+TV_FLAG-ERR_NR)
	LD	B,(IY+DF_SZ-ERR_NR)
	CALL	CLLINE
	LD	HL,$5AC0
	LD	A,(ATTR_P)
	DEC	B
	JR	CLS3
CLS1:	LD	C,$20
CLS2:	DEC	HL
	LD	(HL),A
	DEC	C
	JR	NZ,CLS2
CLS3:	DJNZ	CLS1
	LD	(IY+DF_SZ-ERR_NR),2
	LD	BC,$17FF
	JR	SKRST

S_TAB:	DEFW	S_RST
	DEFW	AUTOLIST
	DEFW	PLOT0
	DEFW	DR_LINE
	DEFW	DR_ARC
	DEFW	DR_CIRCLE
S_TAB_E:EQU	$

S_RST:	LD	HL,0
	LD	(COORDS),HL
	RES	0,(IY+FLAGS2-ERR_NR)
	RST	$30
	DEFW	L0D4D		; TEMPS
	LD	B,$18
	CALL	CLLINE
	LD	(IY+SCR_CT-ERR_NR),1
	LD	BC,$18FF
SKRST:	CALL	COL_C
	CALL	CLSET
	RST	$10

P_RST:	LD	HL,RESET_P
	PUSH	HL
	RST	$10

AUTOLIST:
	LD	HL,L1795
	PUSH	HL
	RST	$10

PLOT0:	LD	HL,L22DC		; TODO: 512 columns
	PUSH	HL
	RST	$10

DR_LINE:
	LD	HL,L24B7
	PUSH	HL
	RST	$10

DR_ARC:
	LD	HL,ARC_DRAW
	PUSH	HL
	RST	$10

DR_CIRCLE:
	LD	HL,DCRCLE
	PUSH	HL
	RST	$10

IOCTL:	CP	$0D
	JR	NC,IOCTL0
	EX	AF,AF'
	RST	RST30
	DEFW	L21D6		; IN-CHAN-K
	JR	Z,K_RST
	LD	BC,$1821
	CP	"S"
	JR	NZ,P_RST
	IN	A,($FF)
	AND	$04
	JR	Z,S_IOCTL
	LD	C,$41		; 64 columns
S_IOCTL:EX	AF,AF'
	ADD	A,A
	CP	S_TAB_E-S_TAB
	JR	NC,S_RST
	LD	E,A
	LD	D,0
	LD	HL,S_TAB
	ADD	HL,DE
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	EX	DE,HL
	JP	(HL)

PR_OUT:	JR	NC,IOCTL
PR_OUT1:LD	C,$FF
	IN	C,(C)
	BIT	2,C
	JR	Z,PR_32
	LD	(IY+MASK_T-ERR_NR),$FF ; do not touch attributes in mono mode
PR_32:	RST	$30
	DEFW	L0B03	; PO-FETCH
	CP	" "
	JR	NC,PR_ABLE1
	CP	$06
	JR	C,PR_QUEST
	CP	$18
	JR	NC,PR_QUEST
	CP	$0E
	JR	C,PR_CTRL1	; controls in ROM1
	CP	$16
	JR	C,PR_1_OPER
PR_2_OPER:
	LD	HL,PR_TV_2
	JR	PR_TV_1
PR_1_OPER:
	LD	HL,PR_CONT
PR_TV_1:LD	(TVDATA),A
	JR	PRCHANGE
PR_TV_2:JR	NC,PR_CONT
	LD	HL,PR_CONT
	LD	(TVDATA+1),A
PRCHANGE:
	LD	(P_OUT1),HL
IOCTL0:	RST	$10

COTEMP5:CP	$0E
	JP	Z, FLASHING
	PUSH	HL
	LD	HL,L2211	; CO-TEMP-5
	EX	(SP),HL
	RST	$10

PR_QUEST:
	LD	A,"?"
PR_ABLE1:
	JR	PR_ABLE

PR_CTRL1:
	CP	$0D
	JR	NZ,PR_CTRL
	BIT	1,(IY+FLAGS-ERR_NR)
	JR	NZ,PR_CPB
	CALL	COL_C
	CALL	PRSCR
	DEC	B
CLSET_R:CALL	CLSET
	RST	$10

PR_TAB:	LD	A,H
	RST	$30
	DEFW	L0B03	; PO-FETCH
	ADD	A,C
	DEC	A
	AND	$3F
	JR	Z,PR_TAB_E
	LD	D,A
	IN	A,($FF)
	AND	$04
	JR	NZ,PR_SPACES
	RES	5,D
PR_SPACES:
	PUSH	HL
	LD	HL,PO_SPACES
	EX	(SP),HL
PR_TAB_E:
	RST	$10

PR_CONT:LD	HL,PR_OUT
	LD	(P_OUT1),HL
	JP	NC,IOCTL
	LD	HL,(TVDATA)
	LD	D,A
	LD	A,L
	CP	$16
	JR	C,COTEMP5
	JR	NZ,PR_TAB
PR_AT:	LD	B,H
	CALL	COL_C
	LD	A,C
	SUB	2
	LD	C,D
	SUB	C
	JR	C,PR_AT_ERR
	ADD	A,2
	LD	C,A
	BIT	1,(IY+FLAGS-ERR_NR)
	JR	NZ,CLSET_R
	LD	A,$16
	SUB	B
PR_AT_ERR:
	JP	C,ERROR_B
	INC	A
	LD	B,A
	INC	B
	BIT	0,(IY+TV_FLAG-ERR_NR)
	JR	NZ,PRSCR_R
	CP	(IY+DF_SZ-ERR_NR)
	JP	C,ERROR_5
	JR	CLSET_R

PRSCR_R:CALL	PRSCR
	RST	$10

PR_CTRL:PUSH	HL
	LD	HL,PO_CTRL
	EX	(SP),HL
	RST	$10

PR_CPB:	PUSH	HL
	LD	HL,L0ECD
	EX	(SP),HL
	RST	$10

PR_ABLE:BIT	2,(IY+FLAGS2-ERR_NR)
	JR	NZ,PR_ANY
	CP	$80
	JR	C,PR_ANY
	CP	RND_T
	JP	C,UDG_TOKEN		; TODO: JR
PR_ANY:	CP	$80
	JR	C,PR_CHAR
	CP	$90
	JR	NC,PR_T_UDG
	LD	B,A
	LD	HL,MEMBOT
	PUSH	HL
	RST	$30
	DEFW	LPOGR1
	RST	$30
	DEFW	L0B03		; PO-FETCH
	POP	DE
PRALL:	IN	A,($FF)
	AND	4
	LD	A,C
	JR	NZ,PRALL64
	DEC	A
	LD	A,$21
	JR	PRALLC
PRALL64:DEC	A
	LD	A,$41
PRALLC:	JR	NZ,PRALL1
	DEC	B
	LD	C,A
	BIT	1,(IY+FLAGS-ERR_NR)
	JR	Z,PRALL1
	PUSH	DE
	RST	$30
	DEFW	L0ECD	; COPY-BUFF
	POP	DE
	LD	A,C
PRALL1:	CP	C
	PUSH	DE
	CALL	Z,PRSCR
	POP	DE
	RST	$30
	DEFW	X0B99	; PR-ALL continued

	LD	E,C
	LD	C,$FF
	IN	C,(C)
	BIT	1,C
	JR	Z, PR_ABC
	BIT	2,C
	JR	Z,PR_MULTI
	BIT	0,E
	JR	NZ,PR_ODD
	DEC	HL		; DEC L ?
	SET	5,H
	JR	PR_ABC
PR_ODD:	RES	5,H
PR_ABC:	LD	C,E
	PUSH	HL
	LD	HL,L0ADC	; PO-STORE
	EX	(SP),HL
	RST	$10

PR_T_UDG:
	SUB	$A5
	JR	NC,PR_T
	ADD	A,$15
	PUSH	BC
	LD	BC,(UDG)
	JR	PR_CHAR2

PR_T:	PUSH	HL
	LD	HL,L0B5F	; PO-T
	EX	(SP),HL
	RST	$10

PR_CHAR:PUSH	BC
	LD	BC,(CHARS)
PR_CHAR2:
	EX	DE,HL
	LD	HL,FLAGS
	RES	0,(HL)
	CP	$20
	JR	NZ,PR_CHAR3
	SET	0,(HL)
PR_CHAR3:
	LD	H,0
	ADD	A,A
	LD	L,A
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,BC
	POP	BC
	EX	DE,HL
	JR	PRALL

PR_MULTI:
	PUSH	HL
	DEC	HL
	SET	5,H
	LD	(HL),A
	INC	H
	LD	(HL),A
	INC	H
	LD	(HL),A
	INC	H
	LD	(HL),A
	INC	H
	LD	(HL),A
	INC	H
	LD	(HL),A
	INC	H
	LD	(HL),A
	INC	H
	LD	(HL),A
	POP	HL
	JR	PR_ABC

UDG_TOKEN:
	CP	INSTRUCTION_T
	JR	C,ARG_TOK
	BIT	0,(IY+FLAGS-ERR_NR)	; leading space
	JR	NZ,SKIPSP
	PUSH	AF
	LD	A," "
	RST	$30
	DEFW	L0C3B			; PO-SAVE
	POP	AF
SKIPSP:	CALL	TOKEN_O
	RRCA
	RST	$30
	DEFW	L2C8D			; ALPHA (could be ALPHANUM)
	JR	NC,SKIPTS
	LD	A," "
	LD	HL, L0C3B		; PO-SAVE
	PUSH	HL
SKIPTS:	RST	$10

ARG_TOK:CALL	TOKEN_O
	RST	$10

TOKEN_O:SUB	$7F
	LD	B,A
	LD	DE, TOKENS
	JP	TOKEN

CLSET:	IN	A,($FF)
	AND	$04
	JR	Z,CLSETR1		; in color modes, use ROM1
	BIT	1,(IY+FLAGS-ERR_NR)
	JR	NZ,CLSETR1		; for the printer use ROM1
	LD	A,B
	BIT	0,(IY+TV_FLAG-ERR_NR)
	JR	Z,CLSET1
	ADD	A,(IY+DF_SZ-ERR_NR)
	SUB	$18
CLSET1:	PUSH	BC
	LD	B,A
	RST	$30
	DEFW	L0E9B			; CL-ADDR
	POP	BC
	LD	A,$41
	SUB	C
	RRA
	LD	E,A
	LD	D,0
	JR	NC,CLODD
	LD	D,$20
CLODD:	ADD	HL,DE
	RST	$30
	DEFW	L0ADC			; PO-STORE
	RET

CLSETR1:RST	$30
	DEFW	L0DD9	; CL-SET
	RET

SCR_ALL:LD	B,$17
CL_SCR:	IN	A,($FF)
	AND	$02
	JR	NZ,SCROLL
	RST	$30
	DEFW	L0E00		; CL-SCROLL
	RET
SCROLL:	INC	B
	RST	$30
	DEFW	L0E9B
SCR_L:	PUSH	BC
	PUSH	HL
	JR	SCR_S
SCR_R:	PUSH	BC
	LD	BC,$0020
	LDIR
	SET	5,H
	SET	5,D
	LD	C,$20
	DEC	HL
	DEC	DE
	LDDR
	RES	5,H
	INC	HL
	POP	BC
SCR_S:	LD	D,H
	LD	E,L
	LD	A,L
	ADD	A,$20
	LD	L,A
	JR	NC,SCR_LT
	LD	A,H
	ADD	A,$08
	LD	H,A
SCR_LT:	DJNZ	SCR_R
	POP	HL
	POP	BC
	INC	H
	LD	A,$07
	AND	H
	JR	NZ,SCR_L
	LD	A,H
	SUB	$08
	LD	H,A
	LD	B,1
;	JR	SCRCLL		; speed up, space down

CLLINE:	IN	A,($FF)
	BIT	1,A
	JR	NZ,SCRCLL
	RST	$30
	DEFW	L0E44			; CL-LINE
	RET

SCRCLL:	PUSH	BC
	OR	$FB
	INC	A
	JR	Z,CLLL
	LD	A,(ATTR_P)
	BIT	0,(IY+TV_FLAG-ERR_NR)
	JR	Z,CLLL
	LD	A,(BORDCR)
CLLL:	PUSH	BC
	PUSH	AF
	RST	$30
	DEFW	L0E9B			; CL-ADDR
	POP	AF
SCR_MC:	LD	E,L
	LD	D,H
	INC	DE
	LD	(HL),0
	LD	BC,$001F
	LDIR
	SET	5,H
	LD	E,L
	LD	D,H
	DEC	DE
	LD	(HL),A
	LD	C,$1F
	LDDR
	RES	5,H
	LD	C,A
	INC	H
	LD	A,7
	AND	H
	LD	A,C
	JR	NZ,SCR_MC
	POP	BC
	LD	C,A
	LD	A,(DF_SZ)
	INC	A
	CP	B
	LD	A,C
	JR	Z,CLLLE
	DJNZ	CLLL
CLLLE:	POP	BC
	OR	A
	RET	Z
	EX	AF,AF'
	LD	A,B
	PUSH	BC
	RRCA
	RRCA
	RRCA
	LD	B,A
	AND	$E0
	LD	C,A
	XOR	B
	LD	B,A
	EX	AF,AF'
	DEC	BC
	LD	HL,$5AFF
	LD	DE,$5AFE
	LD	(HL),A
	LDDR
	POP	BC
	RET

PRSCR:	BIT	1,(IY+FLAGS-ERR_NR)
	RET	NZ
	LD	DE,CLSET
	PUSH	DE
	LD	A,B
	BIT	0,(IY+TV_FLAG-ERR_NR)
	JR	NZ,PRSCR4
	CP	(IY+DF_SZ-ERR_NR)
	JR	C,ERROR_5
	RET	NZ
	BIT	4,(IY+TV_FLAG-ERR_NR)
	JR	Z,PRSCR2
	LD	E,(IY+BREG-ERR_NR)
	DEC	E
	JR	Z,PRSCR3
	XOR	A
	RST	$30
	DEFW	L1601	; CHAN-OPEN
	LD	SP,(LIST_SP)
	RES	4,(IY+TV_FLAG-ERR_NR)
	RST	$10

ERROR_5:RST	$30
	DEFW	L0C86

PRSCR2:	DEC	(IY+SCR_CT-ERR_NR)
	JR	NZ,PRSCR3
	LD	A,$18
	SUB	B
	LD	(SCR_CT),A
	LD	HL,(ATTR_T)
	PUSH	HL
	LD	A,(P_FLAG)
	PUSH	AF
	LD      DE,L0CF8
	RST	$30
	DEFW	MSG_WAIT
	CP	" "
	JR	Z,ERROR_D
	CP	STOP_T
	JR	Z,ERROR_D
	OR	$20
	CP	"n"
	JR	Z,ERROR_D
	CP	"1"
	JR	NZ,SCRMANY
SCRONE:	LD	(IY+SCR_CT-ERR_NR),1
SCRMANY:LD	A,$FE
	RST	$30
	DEFW	L1601
	LD	(ATTR_T),HL
	LD	B,2
	CALL	CLLINE
	POP	AF
	LD	(P_FLAG),A
	POP	HL
	LD	(ATTR_T),HL
PRSCR3:	CALL	SCR_ALL
	LD	B,(IY+DF_SZ-ERR_NR)
	INC	B
	CALL	COL_C
	RET	NZ		; no attribute magic in mono mode
SCR32:	RST	$30
	DEFW	PO_SCR_ATTR	; attribute magic TODO: multicolor
	RET

ERROR_D:RST	$30
	DEFW	L0D00		; D BREAK - CONT repeats

PRSCR4:	CP	2
	JR	C,ERROR_5
	ADD	A,(IY+DF_SZ-ERR_NR)
	SUB	$19
	RET	NC
	NEG
	PUSH	BC
	LD	B,A
	LD	HL,(ATTR_T)
	PUSH	HL
	LD	HL,(P_FLAG)
	PUSH	HL
	RST	$30
	DEFW	L0D4D		; TEMPS
	LD	A,B
PRSCR4A:PUSH	AF
	LD	HL,DF_SZ
	LD	B,(HL)
	LD	A,B
	INC	A
	LD	(HL),A
	LD	HL,S_POSN+1
	CP	(HL)
	JR	C,PRSCR4B
	INC	(HL)
	LD	B,$17
PRSCR4B:CALL	CL_SCR
	POP	AF
	DEC	A
	JR	NZ,PRSCR4A
	POP	HL
	LD	(IY+P_FLAG-ERR_NR),L
	POP	HL
	LD	(ATTR_T),HL
	LD	BC,(S_POSN)
	RES	0,(IY+TV_FLAG-ERR_NR)
	CALL	CLSET
	SET	0,(IY+TV_FLAG-ERR_NR)
	POP	BC
	RET

COL_C:	LD	C,$FF
	IN	C,(C)
	BIT	2,C
	LD	C,$21
	RET	Z
	LD	C,$41
	RET
