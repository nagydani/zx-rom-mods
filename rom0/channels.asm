; output instruction token
I_TOKEN:EQU	$0118	; DEF FN in ROM1
TOKEN_I:SUB	FREE_T	; FREE / DEF_FN
	JR	C,TOKEN2I	; new tokens
	LD	DE,I_TOKEN
	RST	$28
	DEFW	L0C10 + 3	; PO-TOKENS + 3
	RET
TOKEN2I:ADD	FREE_T - $80
	EXX
	PUSH	DE
	LD	DE,TOKENS1
	CALL	TOKEN
	POP	DE
	EXX
	RET

; channel K service routine
K_OUT:	LD	HL,K_STATE
	JR	C,KS_OUT
; channel K ioctl
	OR	A
	JR	NZ,KS_NR
K_RST:	LD	(HL),A
	RES	5,(IY+TV_FLAG-ERR_NR)	; no further clearing
	RST	$28
	DEFW	L0D4D		; TEMPS
	LD	B,(IY+$31)	; fetch lower screen display file size DF_SZ
	RST	$28
	DEFW	L0E44		; CL-LINE
	LD      HL,$5ABF	; attribute at 21,31
	LD      DE,$5ABE	; attribute at 21,30
	LD	A,(ATTR_P)
	LD	(HL),A
	XOR	A
	SRL	B
	RRA
	SRL	B
	RRA
	SRL	B
	RRA
	LD	C,A
	DEC	BC
	LDDR
	LD	(IY+$31),$02	; now set DF_SZ lower screen to 2
        LD      BC,$1721        ; TODO: depends on mode; line 23 for lower screen
	LD      (RETADDR),BC
S_IO_E:	RST	$28
	DEFW	L0DD9		; CL-SET
KS_NR:	JP	SWAP

; channel S service routine
S_OUT:	LD	HL,S_STATE
	JR	C,KS_OUT
; channel S ioctl
	OR	A
	JR	NZ,KS_NR
S_RST:	LD	(HL),A
	LD      H,A
	LD	L,A		; Initialize plot coordinates.
	LD      (COORDS),HL	; Set system variable COORDS to 0,0.
	RES	0,(IY+$30)	; update FLAGS2  - signal main screen is clear.
	RST	$28
	DEFW	L0D4D		; TEMPS
	LD	B,$18		; 24 lines
	RST	$28
	DEFW	L0E44		; CL-LINE
	LD      (IY+$52),$01    ; set SCR_CT - scroll count - to default.
	LD	BC,$1821	; TODO: depends on mode; line 24 for upper screen
	JR	S_IO_E

KS_OUT:	BIT	0,(HL)		; direct output
	JR	NZ,KS_IND
	CP	$18
	JR	NC,PABLE
	LD	C,A
	LD	B,0
	EX	DE,HL
	LD	HL,TCTRL
	ADD	HL,BC
	LD	C,(HL)
	ADD	HL,BC
	JP	(HL)

KS_IND:	BIT	1,(HL)
	JR	NZ,KS_IND2
	RES	0,(HL)
	INC	HL	; width
	INC	HL	; control type
	LD	D,A
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	RST	$28
	DEFW	XPOCONT	; TODO: proper handling in this ROM
	JR	TNOP

KS_IND2:RES	1,(HL)
	INC	HL	; width
	INC	HL	; control type
	INC	HL	; first control of two
	LD	(HL),A
	JR	TNOP

TCTRL:	DEFB	TNOP - $	; $00 does nothing
	DEFB	TQUEST - $	; $01 prints question mark
	DEFB	TQUEST - $	; $02 prints question mark
	DEFB	TQUEST - $	; $03 prints question mark
	DEFB	TQUEST - $	; $04 prints question mark
	DEFB	TQUEST - $	; $05 prints question mark
	DEFB	TCOMMA - $	; $06 tabulates with blanks
	DEFB	TQUEST - $	; $07 prints question mark
	DEFB	TBS - $		; $08 back
	DEFB	TFW - $		; $09 forward
	DEFB	TLF - $		; $0A down
	DEFB	TUP - $		; $0B up
	DEFB	TFF - $		; $0C clear screen
	DEFB	TCR - $		; $0D ENTER
	DEFB	TBLINK - $	; $0E cursor character
	DEFB	TRST - $	; $0F restore permanent attrs
	DEFB	T1CTR - $	; $10 INK
	DEFB	T1CTR - $	; $11 PAPER
	DEFB	T1CTR - $	; $12 FLASH
	DEFB	T1CTR - $	; $13 BRIGHT
	DEFB	T1CTR - $	; $14 INVERSE
	DEFB	T1CTR - $	; $15 OVER
	DEFB	T2CTR - $	; $16 AT
	DEFB	T2CTR - $	; $17 TAB

POFETCH:EQU	$0B09
POSTORE:EQU	$0AE2
CLSET:	EQU	$0DE2

TFF:	XOR	A
	BIT	0,(IY+$02)
	JR	NZ,K_RST
	JR	S_RST

TSTORE:	RST	$28
	DEFW	POSTORE
TNOP:	JP	SWAP

TQUEST:	LD	A,"?"
	JR	PABLE

TCOMMA:	RST	$28
	DEFW	POFETCH
	LD	A,C
	DEC	A
	DEC	A
	AND	$10
	JR	POFILL

TBS:	INC	DE
	LD	A,(DE)
	INC	A
	INC	A
	INC	C
	RST	$28
	DEFW	L0A23 + 3	; TODO: continue with PO_BACK_1
	JR	TNOP

TFW:	INC	DE
	RST	$28
	DEFW	POFETCH
	LD	A,C
	DEC	A
	LD	A,(DE)
	JR	NZ,TFW1
	DEC	B
	INC	A
	LD	C,A
TFW1:	CP	C
	CALL	Z,POSCR
	DEC	C
	INC	HL
	JR	TSTORE

TCR:	CALL	TCR0
TCR1:	RST	$28
	DEFW	CLSET
	JR	TNOP

TLF:	LD	A,C
	PUSH	AF
	CALL	TCR0
	POP	AF
	LD	C,A
	JR	TCR1

TCR0:	INC	DE
	LD	A,(DE)
	INC	A
	LD	C,A
	CALL	POSCR
	DEC	B
	RET

TUP:	INC	B
	LD	A,$18	; TODO: 24 lines
	CP	B
	JR	NZ,TCR1
	DEC	B
	JR	TCR1

TBLINK:	EX	DE,HL
	SET	0,(HL)
	SET	2,(HL)
	JR	TNOP

T1CTR:	EX	DE,HL
TCTR:	SET	0,(HL)
	INC	HL
	INC	HL
	LD	(HL),A
	JR	TNOP

T2CTR:	EX	DE,HL
	SET	1,(HL)
	JR	TCTR
