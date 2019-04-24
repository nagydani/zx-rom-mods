; K/S channel state
; byte 0:
; bit 0:	direct output (0), to be saved (1)
; bit 1:	one extra byte (0), two extra bytes (1)
; bit 2:	(blinking) cursor character follows
; bit 3:	instructions (0), functions/operators (1)
; bit 4:	copy of bit 4 of FRAMES
; bit 5:	flashing cursor state
; bit 6;	flashing cursor visible (1)
; bit 7:	K channel input from K_DATA
;
; byte 1:
; width of display (defaults to 32)
;
; byte 2:
; control character saved (see TV_DATA)
;
; byte 3:
; first argument saved (see TV_DATA + 1)

; copy edit area
ED_COPY:SET	6,(HL)
	RES	3,(HL)
	LD	HL,C_SPCC
	LD	(HL),1
	PUSH	HL
	RST	$28
	DEFW	L111D		; ED-COPY
	POP	HL
	RET

; channel K input
K_IN:	LD	HL,K_STATE
	BIT	7,(HL)
	JR	Z,K_IN0
	LD	A,(K_DATA)
	RES	7,(HL)
	SCF
	JP	SWAP

; regular input
K_IN0:	BIT	3,(IY+$02)
	CALL	NZ,ED_COPY
	AND	A
	BIT	5,(IY+$01)
	JR	Z,K_INNK	; no key pressed
	BIT	5,(HL)
	RES	5,(HL)
	CALL	NZ,PCURSOR	; hide cursor
	LD	HL,$00C8	; key click pitch
	LD	D,H
	LD	A,(PIP)		; duration
	OR	A
	JR	Z,NOPIP
	RST	$28
	DEFW	BEEP_PIP
NOPIP:	LD	A,(LAST_K)	; pressed keycode
	RES	5,(IY+$01)	; fetched from buffer
	BIT	5,(IY+$02)
	JR	Z,NOCLSL
	PUSH	AF
	RST	$28
	DEFW	L0D6E		; CLS-LOWER
	POP	AF
NOCLSL:	CP	$88
	JR	C,K_INB
	CP	$90
	JR	NC,K_INW
	LD	BC,$7FFE
	IN	B,(C)
	BIT	1,B		; check Symbol Shift
	JR	NZ,K_ING0
	ADD	A,$100 - $70	; transpose to $18..$1F, set CF
	JR	K_ING0

K_INW:	CP	RND_T		; USR "V" +
	JR	C,K_INB
	BIT	1,(IY+$07)	; mode G?
	JR	Z,K_INB
	ADD	A,$100 - $A4	; transpose to 1..5, set CF
K_ING0:	RES	1,(IY+$07)
	JP	SWAP
K_INB:	BIT	3,(IY+$01)	; mode K?
	JR	NZ,K_MDL	; jump, if not
	LD	HL,K_MODE
	LD	C,A
	CALL	INDEXER
	LD	A,C
	JR	NC,K_MDL
	LD	A,(HL)
K_MDL:	CP	$20
	CCF
	JR	C,K_ING0	; regular key pressed
	CP	$0D
	JR	Z,K_ENT		; ENTER pressed
	CP	$10
	JR	NC,KEY_CONTR0
	CP	$06
	JR	NC,KEY_M_CL0

	LD	B,A
	AND	$01
	LD	C,A
	LD	A,B
	RRA
	ADD	A,$12
	JR	KEY_DATA0

K_INNK:	LD	B,(HL)
	BIT	6,B
	JP	Z,SWAP
	LD	A,(FRAMES)
	XOR	B
	AND	$10
	JP	Z,SWAP
	PUSH	BC
	PUSH	HL
	CALL	PCURSOR
	POP	HL
	POP	BC
	XOR	$20
	XOR	B
	LD	(HL),A
	XOR	A
	JP	SWAP

KEY_M_CL0:
	JR	NZ,KEY_MODE0
	LD	HL,FLAGS2
	LD	A,$08
	XOR	(HL)
	LD	(HL),A		; toggle CAPS LOCK
	JR	KEY_FLAG0

KEY_MODE0:
	CP	$0E
	JP	C,SWAP
	SUB	$0D
	LD	HL,MODE
	CP	(HL)
	LD	(HL),A
	JR	NZ,KEY_FLAG0
	LD	(HL),$00

KEY_FLAG0:
	SET	3,(IY+2)
	CP	A
K_INR2:	JP	SWAP

KEY_CONTR0:
	LD	B,A
	AND	$07
	LD	C,A
	LD	A,$10
	BIT	3,B
	JR	NZ,KEY_DATA0
	INC	A
KEY_DATA0:
	LD	(IY-$2D),C
	LD	HL,K_STATE
	SET	7,(HL)
	SCF
	JR	K_INR2

K_ENT:	LD	HL,K_STATE
	LD	(IY+DEFADD+1-ERR_NR),1	; TODO: this is an ugly hack
	RES	6,(HL)		; turn off blinking cursor
	SCF
	JR	K_INR2

; K-MODE translation table
K_MODE:	DEFB	POKE_T,PEEK_T
	DEFB	"@",LABEL_T
	DEFB	0

; print flashing cursor (invert character)
PCURSOR:PUSH	AF
	RST	$28
	DEFW	L0B03		; PO-FETCH
	PUSH	BC
	PUSH	HL
	LD	HL,(ECHO_E)
	PUSH	HL
	LD	HL,(MASK_T)
	PUSH	HL
	LD	BC,(RETADDR)
	LD	(IY + MASK_T - ERR_NR),$FF	; INK, PAPER, BRIGHT, FLASH 8
	RST	$28
	DEFW	L0DD9		; CL-SET
	SET	0,(IY + P_FLAG - ERR_NR)	; OVER 1
	LD	A,$8F		; full block
	RST	$10
	POP	HL
	LD	(MASK_T),HL
	POP	HL
	LD	(ECHO_E),HL
	POP	HL
	POP	BC
	LD	(S_POSNL),BC
	LD	(DF_CCL),HL
	POP	AF
	RET

; output instruction token
I_TOKEN:EQU	$0118	; DEF FN in ROM1

TOKEN_I:LD	B,A
	BIT	0,(IY+$01)
	JR	NZ,TOKENI1
	PUSH	AF
	EXX
	LD	A," "
	RST	$10
	EXX
	POP	AF
	LD	B,A
	SET	0,(IY+$01)
TOKENI1:SUB	FREE_T - RND_T	; FREE / DEF FN
	JR	C,TOKEN2I	; new tokens
	LD	DE,I_TOKEN
	RST	$28
	DEFW	L0C10 + 3	; PO-TOKENS + 3
	LD	A,D
	CP	3
	RET	NC
	JR	TSPACE

TOKEN2I:INC	B
	LD	DE,TOKENS1
	CALL	TOKEN
	RRA
	RST	$28
	DEFW	L2C8D		; ALPHA
	RET	NC
TSPACE:	LD	A," "
	EXX
	RST	$10
	EXX
	RET

; output operator token
TOKEN_O:CP	FREE_T - RND_T	; FREE / DEF FN
	JR	C,TOKEN2O	; old token
	CP	EFN_T - RND_T
	JR	NC,TOKEN2O	; also old token
	SUB	FREE_T - RND_T - 1
	LD	B,A
	LD	DE,TOKENS0
	JP	TOKEN
TOKEN2O:RST	$28
	DEFW	L0C10
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
	DEC	B
	DEC	B
	JR	Z,K_RST0
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
K_RST0: LD	(IY+$31),$02	; now set DF_SZ lower screen to 2
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

POFETCH:EQU	L0B03 + 6
POSTORE:EQU	L0ADC + 6
CLSET:	EQU	L0DD9 + 9

KS_OUT:	BIT	0,(HL)		; direct output
	JR	NZ,KS_IND
	BIT	2,(HL)
	PUSH	AF
	EX	DE,HL
	RST	$28
	DEFW	POFETCH
	POP	AF
	JP	NZ,E_HEAD
	CP	$18
	JR	C,KS_CTRL
	CP	$20
	JR	C,PR_GR_0
	CP	$80
	JR	NC,P_GR_TK
	BIT	2,(IY+$30)	; inside quotes
	JR	NZ,PR_NQ
	CP	":"
	JR	NZ,PR_NQ
	EX	DE,HL
	RES	3,(HL)		; colon outside of quotes sets instr. mode
	LD	HL,C_SPCC
	INC	(HL)
	EX	DE,HL
PR_NQ:	RST	$28
	DEFW	L0B65	; PO-CHAR
	JR	TSTORE2

; channel K/S indirect output
KS_IND:	BIT	1,(HL)
	JR	NZ,KS_IND2
	RES	0,(HL)
	INC	HL	; width
	INC	HL	; tv1
	LD	D,A
	LD	A,(HL)
	INC	HL	; tv2
	LD	H,(HL)
	PUSH	HL
	LD	HL,XPOCONT	; TODO: proper handling in this ROM
	EX	(SP),HL
	JP	SWAP

KS_IND2:RES	1,(HL)
	INC	HL	; width
	INC	HL	; tv1
	INC	HL	; tv2
	LD	(HL),A
	JP	SWAP

P_GR_TK:CP	$90
	JR	NC,PR_T_UDG
	RST	$28
	DEFW	L0B24 + 8	; block graphics
TSTORE2:JR	TSTORE

PR_T_UDG:
	SUB	RND_T
	JR	NC,PR_TK
	RST	$28
	DEFW	L0B52 + 4	; PO-T-UDG + 4, udg
	JR	TSTORE

PR_TK:	EX	DE,HL
	LD	BC,SWAP
	PUSH	BC
	BIT	3,(HL)
	JR	Z,PR_INST
	CP	ELSE_T - RND_T
	JR	NZ,PR_TK0
	RES	3,(HL)
	LD	HL,C_SPCC
	INC	(HL)
PR_TK0:	JP	TOKEN_O
PR_INST:CP	ELSE_T - RND_T
	JR	Z,PR_TK1
	SET	3,(HL)
	JR	PR_TK2
PR_TK1:	LD	HL,C_SPCC
	INC	(HL)
PR_TK2:	JP	TOKEN_I

KS_CTRL:PUSH	BC
	LD	C,A
	LD	B,0
	LD	HL,TCTRL
	ADD	HL,BC
	LD	C,(HL)
	ADD	HL,BC
	POP	BC
	JP	(HL)

PR_GR_0:LD	C,A
	AND	$06
	LD	E,A
	LD	D,0
	LD	HL,GR_TAB
	ADD	HL,DE
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	LD	HL,MEMBOT
	LD	B,8
	RR	C
	JR	C,PR_GR_R
PR_GR_L:LD	(HL),E
	INC	L
	RL	D
	RL	E
	DJNZ	PR_GR_L
	JR	PR_GR_E
PR_GR_R:RR	E
	RR	D
	LD	(HL),D
	INC	L
	DJNZ	PR_GR_R
PR_GR_E:RST	$28
	DEFW	X0B30		; generated graphics in PO_ANY
TSTORE:	RST	$28
	DEFW	POSTORE
T_SWX:	JP	SWAP

GR_TAB:	DEFB	$00, $FF
	DEFB	$FF, $00
	DEFB	$F0, $00
	DEFB	$00, $0F

E_QUEST:LD	HL,(X_PTR)
	LD	(K_CUR),HL
	JR	T_SWX

; draw editor header
E_HEAD:	EX	DE,HL
	RES	2,(HL)
	CP	"?"
	JR	Z,E_QUEST
	LD	(RETADDR),BC
	PUSH	BC
	PUSH	DE
	EXX
	PUSH	DE
	LD	HL,(ATTR_T)
	LD	H,(IY + P_FLAG - ERR_NR)
	PUSH	HL
	LD	HL,(FLAGS - 1)
	LD	L,(IY+$30)
	SET	2,(IY+$30)
	PUSH	HL
	PUSH	AF
	LD	DE,EDITOR_HEADER0
	CALL	MESSAGE
	BIT	5,(IY+$37)
	JR	NZ,E_HEAD0
	CALL	MESSAGE
	LD	A,(C_SPCC)
	CALL	DECBYTE
E_HEAD0:LD	A,$06		; TAB
	RST	$10
	POP	AF
	RST	$10
	LD	DE,EDITOR_HEADER1
	CALL	MESSAGE
	POP	AF
	JP	PE,E_HEAD1
	RES	2,(IY+$30)
E_HEAD1:LD	(FLAGS),A
	POP	HL
	LD	(IY + ATTR_T - ERR_NR),L
	LD	(IY + P_FLAG - ERR_NR),H
	POP	DE
	EXX
	POP	HL
	POP	BC
TSTORE3:JR	TSTORE

TCTRL:	DEFB	TNOP - $	; $00 does nothing
	DEFB	TQUEST - $	; $01 prints question mark
	DEFB	TQUEST - $	; $02 prints question mark
	DEFB	TQUEST - $	; $03 prints question mark
	DEFB	TQUEST - $	; $04 prints question mark
	DEFB	TRST - $	; $05 restore permanent attrs
	DEFB	TCOMMA - $	; $06 tabulates with blanks
	DEFB	TQUEST - $	; $07 prints question mark
	DEFB	TBS - $		; $08 back
	DEFB	TFW - $		; $09 forward
	DEFB	TLF - $		; $0A down
	DEFB	TUP - $		; $0B up
	DEFB	TFF - $		; $0C clear screen
	DEFB	TCR - $		; $0D ENTER
	DEFB	TBLINK - $	; $0E cursor character
	DEFB	TQUEST - $	; $0F prints question mark
	DEFB	T1CTR - $	; $10 INK
	DEFB	T1CTR - $	; $11 PAPER
	DEFB	T1CTR - $	; $12 FLASH
	DEFB	T1CTR - $	; $13 BRIGHT
	DEFB	T1CTR - $	; $14 INVERSE
	DEFB	T1CTR - $	; $15 OVER
	DEFB	T2CTR - $	; $16 AT
	DEFB	T2CTR - $	; $17 TAB

TFW:	INC	DE
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
	JR	TSTORE3

TFF:	XOR	A
	BIT	0,(IY+$02)
	JP	NZ,K_RST
	JP	S_RST

TQUEST:	LD	A,"?"
	JP	PR_NQ

TCOMMA:	INC	DE
	EX	AF,AF'
	LD	A,C
	DEC	A
	DEC	A
	AND	$10
POFILL: RST	$28
	DEFW	POFETCH
	ADD	A,C
	DEC	A
	LD	C,A
	LD	A,(DE)
	DEC	A
	AND	C
	JR	Z,TNOP
	LD	D,A
	SET	0,(IY+01)
POSPACE:LD	A," "
	PUSH	DE
	EXX
	RST	$10
	EXX
	POP	DE
	DEC	D
	JR	NZ,POSPACE
TNOP:	JP	SWAP

TBS:	INC	DE
	LD	A,(DE)
	INC	A
	INC	A
	INC	C
	PUSH	HL
	LD	HL,L0A23 + 3	; TODO: continue with PO_BACK_1
	EX	(SP),HL
	JR	TNOP

TCR:	PUSH	DE
	CALL	TCR0
	POP	HL
	RES	3,(HL)
TCR1:	LD	HL,CLSET
	PUSH	HL
TNOP2:	JR	TNOP

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
	JR	NC,TCR1
	DEC	B
	JR	TCR1

TBLINK:	EX	DE,HL
	SET	2,(HL)
	JR	TNOP2

T1CTR:	EX	DE,HL
TCTR:	SET	0,(HL)
	INC	HL	; width
	INC	HL	; tv1
	LD	(HL),A
	JR	TNOP2

T2CTR:	EX	DE,HL
	SET	1,(HL)
	JR	TCTR

TRST:	RST	$28
	DEFW	L0D4D	; TEMPS
	JP	SWAP

POSCR:	RST	$28	; TODO: take width into account
	DEFW	L0C55	; PO-SCR
	RET

EDITOR_HEADER0:
	DEFB	$16,$00,$00,$13,$01,$10,$07,$11,$80
	DEFM	"BASIC"
	DEFB	$80 + ":"
EDITOR_HEADER1:
;;	DEFB	$17,$FA,$FF,$10,$02,$18,$11,$06,$1A
	DEFB	$17,$1A,$00,$10,$02,$18,$11,$06,$1A
	DEFB	$10,$04,$18,$11,$05,$1A,$10,$00,$18
	DEFB	$14,$01,$A0
