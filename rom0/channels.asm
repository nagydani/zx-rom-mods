; K/S channel state
; byte 0:
; bit 0:	direct output (0), to be saved (1)
; bit 1:	one extra byte (0), two extra bytes (1)
; bit 2:	(blinking) cursor character follows
; bit 3:	instructions (0), arguments (1)
; bit 4:	copy of bit 4 of FRAMES
; bit 5:	flashing cursor state
; bit 6;	8 pixel font (0), 4 pixel font (1)
; bit 7:	K channel input from K_DATA
;
; byte 1:
; width of display + 1 (defaults to 33)
;
; byte 2:
; control character saved (see TVDATA)
;
; byte 3:
; first argument saved (see TVDATA + 1)

POFETCH:EQU	L0B03 + 6
POSTORE:EQU	L0ADC + 6

; channel K input service routine
K_IN:	CALL	STACKSWAP
	LD	HL,K_STATE
	BIT	7,(HL)
	JR	Z,K_IN0
	LD	A,(K_DATA)
	RES	7,(HL)
	CP	$80		; potential token ending signal
	SCF
	RET	NZ
	LD	A,$0E		; pretend that EXT
	LD	(LAST_K),A	; has been pressed
	JR	NOPIP		; in silence

K_INNK:	BIT	7,(IY+BORDCR-ERR_NR)	; flashing cursor visible?
	RET	Z
	LD	B,(HL)
	LD	A,(FRAMES)
	XOR	B
	AND	$10
	RET	Z
	PUSH	BC
	PUSH	HL
	CALL	PCURSOR
	POP	HL
	POP	BC
	XOR	$20
	XOR	B
	LD	(HL),A
	XOR	A
	RET

; regular input
K_IN0:	BIT	3,(IY+$02)
	CALL	NZ,ED_COPY
	AND	A
	PUSH	HL
	LD	HL,FLAGS
	BIT	5,(IY+$30)	; mode K suppressed?
	JR	Z,K_IN1		; jump, if not
	SET	3,(HL)		; switch off K mode
K_IN1:	BIT	5,(HL)		; key pressed?
	POP	HL
	JR	Z,K_INNK	; no key pressed
	BIT	5,(HL)
	RES	5,(HL)
	CALL	NZ,PCURSOR	; hide cursor
	LD	HL,$00C8	; key click pitch
	LD	D,H
	LD	A,(PIP)		; duration
	OR	A
	JR	Z,NOPIP
	RST	$30
	DEFW	BEEP_PIP
NOPIP:	LD	A,(LAST_K)	; pressed keycode
	RES	5,(IY+$01)	; fetched from buffer
	BIT	5,(IY+$02)
	JR	Z,NOCLSL
	PUSH	AF
	RST	$30
	DEFW	L0D6E		; CLS-LOWER
	POP	AF
NOCLSL:	CP	$88
	JR	C,K_INB		; jump, if definitely not block graphics
	CP	$90
	JR	NC,K_INW	; jump, if UDG or tokens
	LD	BC,$7FFE
	IN	B,(C)
	BIT	1,B		; check Symbol Shift
	JR	NZ,K_ING0	; jump if not block graphics
	ADD	A,$100 - $70	; transpose to $18..$1F, set CF
	JR	K_ING0

K_INW:	CP	RND_T		; USR "V" +
	JR	C,K_INB
	BIT	1,(IY+$07)	; mode G?
	JR	Z,K_INB
	RES	1,(IY+$07)	; these turn mode G off
	ADD	A,$100 - $A4	; transpose to 1..5, set CF
K_ING0:	BIT	5,(IY+$30)	; mode K suppressed?
	JR	Z,K_ING1	; jump, if not
	LD	C,A		; mode L translation
	LD	HL,L_MODE
	CALL	INDEXER
	LD	A,C
	JR	NC,K_ING1
	LD	A,(HL)
K_ING1:	SCF
K_ING3:	BIT	5,(IY+FLAGS2-ERR_NR)	; mode K suppressed?
	RET	Z		; return, if not
	CALL	EDITOR_MODE	; editing?
	RET	NZ		; return, if not
	LD	HL,K_SAV
	BIT	2,(HL)		; inside quotes?
	RET	NZ		; return, if so
	RST	$30
	DEFW	L2C8D		; ALPHA
	RET	C		; return, if so
	LD	HL,TKETAB
	LD	BC,TKETABE-TKETAB
	CPIR
	JP	NZ,K_INSTT
K_TKE:	LD	(IY-$2D),$80	; signal potential token end
	LD	HL,K_STATE
	SET	7,(HL)
	SCF
	RET

K_ENT:	LD	HL,0
	LD	(K_CUR_S),HL	; reset old cursor position
	LD	(IY+DEFADD+1-ERR_NR),1	; TODO: this is an ugly hack
	RES	7,(IY+BORDCR-ERR_NR)	; turn off flashing cursor
K_ENTP:	RES	4,(IY+$37)	; allow ELSE
	RES	2,(IY+FLAGS-ERR_NR)	; K mode output (another ugly hack)
	SCF
	JR	K_ING3

K_INB:
K_MDL:	CP	" "
	JR	Z,KEY_M_K0	; jump, if space
K_NSP:	LD	(K_DATA),A
	CCF
	JR	C,K_ING0	; regular key pressed
; Control keys
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

KEY_M_K0:
	CALL	EDITOR_MODE
	SCF
	RET	NZ		; outside of editor mode, space is a regular key
	RST	$30
	DEFW	X1F5A		; CAPS SHIFT ?
	LD	A," "		; restore A
	CCF
	JR	C,K_TRAD	; in editor mode, BREAK toggles K mode suppression
	LD	HL,MODE
	BIT	0,(HL)		; mode E?
	JR	Z,K_NSP
	DEC	(HL)		; leave mode E
K_TRAD:	LD	A,$20		; toggle bit 5 (K mode suppression)
	JR	TFLAGS2

KEY_M_CL0:
	JR	NZ,KEY_MODE0
	LD	A,$08		; toggle CAPS LOCK
TFLAGS2:LD	HL,FLAGS2
	XOR	(HL)
	LD	(HL),A		; toggle mask in A
	JR	KEY_FLAG0

KEY_MODE0:
	CP	$0E
	JR	C,KEY_CUR	; control keys below EXT
	JR	NZ,KEY_MODE1	; EXT key?
	LD	HL,FLAGS2
	BIT	5,(HL)		; mode K suppressed?
	JR	Z,KEY_MODE1	; jump, if not - toggle mode E
; EXT key in K suppressed-mode
	BIT	2,(HL)		; inside quotes?
	JR	NZ,KEY_MODE1	; jump, if not - toggle mode U
	CALL	EDITOR_MODE
	RET	NZ		; exit, if not editing
	LD	HL,(E_LINE)
	LD	DE,(K_CUR)
	AND	A
	SBC	HL,DE
	JR	Z,K_TRAD	; exit K suppressed mode
	EX	DE,HL
	DEC	HL
	LD	B,0
	LD	A,(HL)
	CP	$80		; is the cursor after a token?
	JR	C,EXT_NT	; jump, if it is not
	LD	HL,K_STATE
	SET	7,(HL)
	SUB	RND_T
	JR	C,CYCOR0
	LD	DE,L0095
CYC_O:	CALL	FC_TOKEN_R1
EXT_CYC:LD	(K_DATA),A
	LD	A,$0C
	SCF
	RET

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
	RET

KEY_MODE2:
	SUB	$12
KEY_MODE1:
	SUB	$0D
	LD	HL,MODE
	CP	(HL)
	LD	(HL),A
	JR	NZ,KEY_FLAG0
	LD	(HL),$00

KEY_FLAG0:
	SET	3,(IY+$02)
	CP	A
	RET

CYCOR0:	LD	DE,TOKENS
	ADD	A,RND_T-$7F
CYCIR0:	LD	B,A
	CALL	FC_TOKEN_R0
	JR	EXT_CYC

KEY_CUR:CALL	EDITOR_MODE	; editor mode?
	SCF
	RET	NZ		; all controls are passed on, if not
	CP	$0A
	RET	C		; pass on what is not an up or down key
	CP	$0C
	CCF
	RET	C		; pass on what is not an up or down key
	CP	$0B		; up arrow
	JR	Z,K_HOME
K_ENDK:	LD	HL,(K_CUR)
	LD	A,$0D
	CP	(HL)		; cursor on CR?
	JR	Z,K_DOWN	; if so, pass on the down arrow
	LD	BC,0		; set the cursor to the line endig CR
	CPIR
	DEC	HL
	EX 	DE,HL
	JR	K_UCUR

EXT_NT:	LD	A,(HL)
	CP	"$"
	JR	Z,EXT_N
	CP	"#"
	JR	Z,EXT_NS
	CP	"<"
	JR	Z,EXT_NR
	CP	">"
	JR	Z,EXT_NR
	CP	"="
	JR	Z,EXT_NR
NOREL:	RST	$30
	DEFW	L2C8D		; ALPHA
	JR	C,EXT_N
	CP	A
	RET

K_DOWN:	LD	A,$0A
	SCF
	RET

K_HOME:	LD	HL,(E_LINE)
	BIT	5,(IY+$37)	; BASIC editing?
	JR	Z,KCUR_ED	; jump, if so
	LD	HL,(WORKSP)
KCUR_ED:EX	DE,HL
	LD	HL,(K_CUR)
	SCF
	SBC	HL,DE
	RET	C		; pass on the key, if at the beginning,
K_UCUR:	LD	(K_CUR),DE	; put it there, if not
	SET	3,(IY+$02)	; update cursor position
	XOR	A		; return empty
	RET

EXT_NS:	LD	B,1
	DEC	HL
	LD	A,(HL)
	DEC	HL
	CP	" "
	JR	Z,EXT_L
	JR	EXT_LS

K_INSTT:LD	HL,(K_CUR)
	DEC	HL
	LD	B,A
	LD	A,(HL)
	RST	$30
	DEFW	L2C8D		; ALPHA
	LD	A,B
	CCF
	RET	C
EXT_N:	LD	B,0
	DEC	HL
EXT_L:	INC	B
	LD	A,(HL)
	DEC	HL
EXT_LS:	RST	$30
	DEFW	L2C8D		; ALPHA
	JR	C,EXT_L
	CP	" "
	JR	NZ,EXT_SP
	BIT	4,(IY+$37)	; instruction mode?
;;	LD	A," "
	JR	Z,EXT_L		; instruction tokens might have spaces
EXT_SP:	INC	HL
	INC	HL
	JR	EXT_OPR

EXT_NR:	DEC	HL
	LD	B,2
EXT_OPR:CALL	TOK_F
EXT_CNT:JR	C,K_INSF
	OR	A
	JR	Z,EXT_NF
	EX	AF,AF'
	LD	A,(LAST_K)
	CP	$0E
	JR	Z,K_INSF1
	SCF
	RET

EXT_NF:	LD	A,(LAST_K)
	CP	$0E
	SCF
	RET	NZ
	LD	HL,RETADDR
	INC	(HL)
	LD	HL,(K_CUR)
	DEC	HL
	LD	A,(HL)
	LD	(LAST_K),A
	LD	(K_DATA),A
	LD	BC,$0001
	RST	$30
	DEFW	L19E8		; RECLAIM-2
	DEC	HL
	JR	EXT_N

K_INSF:	EX	AF,AF'
K_INSF1:LD	A,(HL)
	RST	$30
	DEFW	L2C8D		; ALPHA
	JR	NC,K_INSX
	DEC	HL
	INC	B
	LD	A,(HL)
	CP	" "
	JR	Z,K_INSX
	INC	HL
	DEC	B
K_INSX:	LD	A,(LAST_K)
	CP	$0E
	JR	Z,K_INST
	CP	" "
	JR	Z,K_INST
	LD	A,(K_STATE)
	OR	$80
	LD	(K_STATE),A
K_INST:	LD	C,B
	LD	B,0
	RST	$30
	DEFW	L19E8		; RECLAIM-2
	EX	AF,AF'
	SCF
	RET

; print flashing cursor (invert character)
PCURSOR:PUSH	AF
	RST	$30
	DEFW	POFETCH
	PUSH	BC
	PUSH	HL
	LD	HL,(ECHO_E)
	PUSH	HL
	LD	HL,(MASK_T)
	PUSH	HL
	LD	BC,(RETADDR)
	LD	(IY + MASK_T - ERR_NR),$FF	; INK, PAPER, BRIGHT, FLASH 8
	CALL	CLSET
	LD	(IY + P_FLAG - ERR_NR),$01	; OVER 1
	LD	HL,FLAGS2
	LD	A,(HL)
	SET	2,(HL)
	PUSH	AF
	LD	A,$8F		; full block
	RST	$30
	DEFW	L0010
	POP	AF
	LD	(HL),A
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

; channel K ioctl
K_IOCTL:CP	2
	RET	NC
K_RST:	RES	7,(IY+BORDCR-ERR_NR) ; flashing cursor OFF
	EX	AF,AF'
	LD	A,(HL)
	AND	$40		; preserve font width
	LD	(HL),A
	LD	(K_SAV2),A
	RES	5,(IY+TV_FLAG-ERR_NR)	; no further clearing
	CALL	K_TEMPS
	SCF
	CALL	K_SWAP
	LD	HL,C_PCC
	LD	(HL),1
	LD	HL,DF_SZ
	LD	B,(HL)		; fetch lower screen line count
	EX	AF,AF'
	DEC	A
	JR	Z,K_CLS
	LD	(HL),$02	; now set DF_SZ to 2
	RES	0,(IY+$02)	; clean hidden upper part
	CALL	CLLINE
	LD	B,2
K_CLS:	SET	0,(IY+$02)	; clean lower part
	CALL	NOLEAD		; suppress leading space
	CALL	CLLINE
	LD	BC,(K_WIDTH)
	LD	B,$17		; line 23 for lower screen
	LD	(RETADDR),BC
S_IO_E:	JP	CLSET

AUTOLIST:
	LD	A,(BANK_M)
	AND	$07
	RET	NZ		; no auto-list in X channel
	RES	5,(IY+FLAGX-ERR_NR)	; track mode state
	RES	7,(IY+BORDCR-ERR_NR)	; Flashing cursor OFF
	RST	$30
	DEFW	L1795		; AUTO-LIST
	RET

; check editor mode
; Output: Z is 1 in editor mode, 0 otherwise, CF set
; Corrupts: HL, DE, F
EDITOR_MODE:
	LD	HL,(ERR_SP)
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	EX	DE,HL
	LD	DE,L107F		; ED-ERROR
	AND	A
	SBC	HL,DE
	SCF
	RET

K_SWAP:	LD	HL,ATTR_T
	LD	DE,K_ATTR
	JR	C,K_SAVE
	EX	DE,HL
K_SAVE:	LDI
	LDI
	LDI
	RET

; copy edit area
ED_COPY:PUSH	HL		; save K_STATE address
	RES	3,(HL)		; start in K mode
	RES	7,(IY+BORDCR-ERR_NR)	; flashing cursor OFF
	LD	HL,C_PCC
	LD	(HL),1
	LD	(IY+T_ADDR-ERR_NR),H	; not LLIST
	RST	$30
	DEFW	L111D			; just use ROM1
; DF_CCL correction
	LD	HL,(ECHO_E)
	PUSH	HL
	LD	HL,(DF_CCL)
	SBC	HL, DE			; reverse ADD HL,DE
	CALL	CLSET_K
	POP	HL
	LD	(ECHO_E),HL

	SET	7,(IY+BORDCR-ERR_NR)	; flashing cursor ON
	POP	HL
	RET

; count colons:
PR_COLON:
	BIT	2,(IY+FLAGS2-ERR_NR)
	RET	NZ
	PUSH	HL
	LD	HL,C_PCC
	INC	(HL)
	POP	HL
	RET

; This area must be a data table not to trigger the SAVE trap

TCTRL:	DEFB	TNOP - $	; $00 does nothing
	DEFB	TQUEST - $	; $01 prints question mark
	DEFB	TQUEST - $	; $02 prints question mark
	DEFB	TQUEST - $	; $03 prints question mark
	DEFB	TQUEST - $	; $04 prints question mark
	DEFB	TRST - $	; $05 restore permanent attrs
	DEFB	TCOMMA - $	; $06 tabulates with blanks
	DEFB	T1CTR - $	; $07 STEP
	DEFB	TBS - $		; $08 back
	DEFB	TFW - $		; $09 forward
	DEFB	TLF - $		; $0A down
	DEFB	TUP - $		; $0B up
	DEFB	TFF - $		; $0C clear screen
	DEFB	TCR - $		; $0D ENTER
	DEFB	TBLINK - $	; $0E cursor character
	DEFB	T1CTR - $	; $0F prints inverted character
	DEFB	T1CTR - $	; $10 INK
	DEFB	T1CTR - $	; $11 PAPER
	DEFB	T1CTR - $	; $12 FLASH
	DEFB	T1CTR - $	; $13 BRIGHT
	DEFB	T1CTR - $	; $14 INVERSE
	DEFB	T1CTR - $	; $15 OVER
	DEFB	T2CTR - $	; $16 AT
	DEFB	T2CTR - $	; $17 TAB

TFF:	XOR	A
	BIT	0,(IY+$02)
	JP	NZ,K_RST
	JP	S_RST

TQUEST:	LD	A,"?"
	JP	PR_PR

TCOMMA:	INC	DE
	EX	AF,AF'		; ???
	LD	A,(DE)
	SUB	C
	OR	$0F
	INC	A
POFILL: RST	$30
	DEFW	POFETCH
	ADD	A,C
	DEC	A
	LD	C,A
	LD	A,(DE)
	SUB	A,2
	AND	C
	RET	Z
	LD	D,A
	SET	0,(IY+01)
POSPACE:LD	A," "
	RST	$30
	DEFW	L0C3B		; PO-SAVE
	DEC	D
	JR	NZ,POSPACE
TNOP:	RET

TBLINK:	BIT	0,(IY+TV_FLAG-ERR_NR)
	JR	Z,T1CTR
	RET	Z
	EX	DE,HL
	SET	2,(HL)	; set blinking or inverse
	RET

T1CTR:	EX	DE,HL
TCTR:	SET	0,(HL)
	INC	HL	; width
	INC	HL	; tv1
	LD	(HL),A
	RET

T2CTR:	EX	DE,HL
	SET	1,(HL)
	JR	TCTR

TRST:	RST	$30
	DEFW	L0D4D	; TEMPS
	RET

TBS:	INC	DE
	LD	A,(DE)
	INC	A
	INC	A
	INC	C
	INC	SP		; TODO: continue with PO-BACK-1
	INC	SP
	PUSH	HL
	LD	HL,L0A23 + 3	; PO-BACK-1 + 3
	EX	(SP),HL
	RST	$10

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

TLF:	LD	A,C
	PUSH	AF
	CALL	TCR0
	POP	AF
	LD	C,A
	JR	TCR1

TCR0:	INC	DE
	LD	A,(DE)
	LD	C,A
	CALL	POSCR
	DEC	B
	JP	NOLEAD

; This area must be a data table not to trigger the LOAD trap

; L-MODE translation table
L_MODE:	DEFB	$E2,"~"
	DEFB	$C3,"|"
	DEFB	$CD,"\\"
	DEFB	$CC,"{"
	DEFB	$CB,"}"
	DEFB	$C6,"["
	DEFB	$C5,"]"
	DEFB	$AC,$7F		; copyright
	DEFB	0

TKETAB:	DEFM	"@$#<>="
TKETABE:EQU	$

TUP:	INC	B
	LD	A,$18	; TODO: 24 lines
	CP	B
	JR	NC,TCR1
	DEC	B
	JR	TCR1

TCR:	PUSH	DE
	CALL	TCR0
	POP	HL
	RES	2,(IY+FLAGS-ERR_NR)	; track instruction mode
	RES	3,(HL)		; reset to instruction mode
TCR1:
CLSET:	LD	A,B
	BIT	0,(IY+$02)
	JR	Z,CLSET1
	ADD	A,(IY+$31)
	SUB	$18
CLSET1:	PUSH	BC
	LD	B,A
	RST	$30
	DEFW	L0E9B		; CL-ADDR
	POP	BC
CLSET2:	LD	A,(S_STATE)
	ADD	A,A
	ADD	A,A
	LD	A,(S_WIDTH)
	BIT	0,(IY+$02)	; upper screen?
	JR	Z,CLSET3
CLSET_K:LD	A,(K_STATE)
	ADD	A,A
	ADD	A,A
	LD	A,(K_WIDTH)
CLSET3:	JR	NC,CLSET5
	SUB	C
	SRL	A
	DEFB	$1E		; LD E,skip next byte
CLSET5:	SUB	C
	LD	E,A
	LD	D,0
	LD	A,(S_MODE)
	CP	$10
	JR	C,CLSET4
	SRL	E
	JR	NC,CLSET4
	SET	5,H
CLSET4:	ADD	HL,DE
TSTORE3:JP	TSTORE

; channel S output service routine
S_OUT:
S_OUT1:	LD	HL,S_STATE
	JR	C,KS_OUT
; channel S ioctl
	EX	DE,HL
	LD	HL,S_IOCTL
	ADD	A,A
	CP	S_IOCTL_END-S_IOCTL
	RET	NC
	LD	C,A
	LD	B,0
	ADD	HL,BC
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	JP	(HL)

S_RST:	EX	DE,HL
	LD	A,(HL)
	AND	$40		; preserve font width
	LD	(HL),A
	LD	HL,COORDX-SWAP+INIT_5B00
	LD	DE,COORDX
	LD	BC,2*5
	LDIR			; clear last PLOT coordinates
	RES	0,(IY+$30)	; update FLAGS2 - signal main screen is clear.
	RST	$30
	DEFW	L0D4D		; TEMPS
	LD	B,$18		; 24 lines
	CALL	CLLINE
	LD	(IY+$52),$01	; set SCR_CT - scroll count - to default.
	LD	BC,(S_WIDTH)
	LD	B,$18		; line 24 for upper screen
	JP	CLSET

KS_CTRL:PUSH	HL		; save display address
	LD	HL,TCTRL
	PUSH	BC		; save coordinates
	LD	C,A
	LD	B,0
	ADD	HL,BC
	LD	C,(HL)
	ADD	HL,BC
	POP	BC		; restore coordinates
	EX	(SP),HL		; restore display address, stack destination
	RET

P_GR_TK:BIT	4,(IY+FLAGS2-ERR_NR)	; K channel?
	JR	NZ,P_GRTK		; if so, check quotes
	BIT	6,(IY+TV_FLAG-ERR_NR)	; non-automatic listing?
	JR	NZ,P_GRTK		; if so, ckeck quotes
	BIT	4,(IY+TV_FLAG-ERR_NR)	; automatic listing
	JR	Z,PR_GR_O		; always graphics, if neither of above
P_GRTK:	BIT	2,(IY+FLAGS2-ERR_NR)	; in quotes
	JP	Z, TOKEN_O		; if not, output token
PR_GR_O:SUB	$90
	JR	NC,PR_UDG
PR_GR:	LD	B,A
	LD	HL,MEMBOT
	RST	$30
	DEFW	LPOGR1		; PO-GR-1 mosaic
	JR	PR_GR_E

; channel K output service routine
K_OUT:	CALL	STACKSWAP
	BIT	0,(IY+TV_FLAG-ERR_NR)
	JR	Z,S_OUT
	LD	HL,K_STATE
	JP	NC,K_IOCTL
; channel K/S direct output for coroutines
KS_OUT:	BIT	0,(HL)		; direct output
	JP	NZ,KS_IND
KS_DCT:	BIT	2,(HL)
	PUSH	AF
	INC	L
	LD	A,(HL)
	DEC	L
	LD	(MEMBOT+8),A	; WIDTH
	EX	DE,HL		; save HL into DE
	LD	HL,S_MODE
	BIT	4,(HL)
	JR	Z,KS_COL
	LD	(IY+MASK_T-ERR_NR),$FF	; do not touch attributes in mono mode
KS_COL:	RST	$30
	DEFW	POFETCH
	POP	AF
	JP	NZ,E_HEAD
COR_TK:	CP	$18
	JR	C,KS_CTRL
PR_PR:	EX	AF,AF'
	LD	A,(DE)
	AND	$40
	EX	AF,AF'		; ZF' for 8 pixel font, NZF' for 4 pixel font
	CP	$80
	JR	NC,P_GR_TK
	CP	":"
	CALL	Z,PR_COLON
	CP	" "
	JR	NC,PR_GR_0
	EX	AF,AF'
	JR	NZ,PR_GR_4
	EX	AF,AF'
	LD	C,A
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

PR_UDG:	PUSH	BC
	LD	BC,(UDG)
	EX	DE,HL
	EX	AF,AF'
	SCF
	JR	PR_FULL

PR_GR_R:RR	E
	RR	D
	LD	(HL),D
	INC	L
	DJNZ	PR_GR_R
PR_GR_E:RST	$30
	DEFW	POFETCH
	LD	DE,MEMBOT
	JR	PR_ALL

PR_GR_4:EX	AF,AF'
PR_GR_0:PUSH	BC
	LD	BC,(CHARS)
	EX	AF,AF'
	JR	Z,PR_8
	LD	BC,(CHARS4)
PR_8:	EX	AF,AF'
PR_CH2:	EX	DE,HL		; screen address to DE, S/K_STATE to HL
	BIT	2,(IY+FLAGS-ERR_NR)
	SET	3,(HL)
	JR	NZ,PR_K		; printing arguments
	RES	3,(HL)		; printing printing in K mode
PR_K:	LD	HL,FLAGS
	RES	0,(HL)
	CP	" "
	JR	NZ,PR_CH3
	SET	0,(HL)
PR_CH3:	EX	AF,AF'
	JR	Z,PR_FULL
	EX	AF,AF'
	SRL	A
	RR	H
	EX	AF,AF'
	RL	H
PR_FULL:EX	AF,AF'
	ADD	A,A
	LD	H,$00
	LD	L,A
	ADD	HL,HL
	ADD	HL,HL
	ADD	HL,BC
	POP	BC
	EX	DE,HL
PR_ALL:	LD	A,C
	DEC	A
	LD	A,(MEMBOT+8)
	JR	NZ,PRALL1
	DEC	B
	LD	C,A
PRALL1:	CP	C
	PUSH	DE
	CALL	Z,POSCR
	POP	DE
	EX	AF,AF'
	JP	NZ,PR_COND	; jump, if 4 pixel font
	RST	$30
	DEFW	X0B99
TSTOREA:EX	AF,AF'
	LD	A,(S_MODE)
	AND	$F8
	SUB	$08
	JR	NZ,TSTOREX
	EX	AF,AF'
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
	LD	A,H
	SUB	$27
	LD	H,A
	INC	HL
TSTORE:	RST	$30
	DEFW	POSTORE
	RET

TSTOREX:SUB	$08
	JR	NZ,TSTORE
	LD	A,H
	XOR	$20
	LD	H,A
	AND	$20
	JR	Z,TSTORE
	DEC	HL
	JR	TSTORE

PRTAB:	LD	DE,MEMBOT+8
	LD	A,H
	JP	POFILL

; channel K/S indirect output
KS_IND:	BIT	1,(HL)
	JR	NZ,KS_IND2
	RES	0,(HL)
	INC	HL	; width
	EX	AF,AF'
	LD	A,(HL)
	EX	AF,AF'
	INC	HL	; tv1
	LD	D,A
	LD	A,(HL)
	INC	HL	; tv2
	LD	H,(HL)
	CP	$16
	JR	C,COTEMP5
	JR	NZ,PRTAB
	LD	B,H
	LD	C,D
	EX	AF,AF'
	SUB	$02
	SUB	C
	JR	C,PATERR
	ADD	$02
	LD	C,A
	LD	A,$16
	SUB	B
PATERR:	JP	C,ERROR_B
	INC	A
	LD	B,A
	INC	B
	BIT	0,(IY+$02)
	JP	NZ,POSCR
	CP	(IY+$31)
	JP	NC,CLSET
	JP	ERROR_5

COTEMP5:CP	$07
	JR	Z,PRSTEP
	INC	SP		; TODO; proper handling in this ROM
	INC	SP		; discard SWAP
	PUSH	HL
	LD	HL,L2211	; CO-TEMP-5
PRTABC:	EX	(SP),HL
	RST	$10

KS_IND2:RES	1,(HL)
	INC	HL	; width
	INC	HL	; tv1
	INC	HL	; tv2
	LD	(HL),A
	RET

PRSTEP:	LD	A,D
	JP	TEMPS4

E_QUEST:LD	HL,(X_PTR)
	LD	(K_CUR),HL
	RET

; draw editor header
E_HEAD:	EX	DE,HL
	RES	2,(HL)
	CP	"?"
	JR	Z,E_QUEST
	PUSH	HL		; channel flag address
	LD	H,(HL)
	PUSH	HL		; channel flag content
	LD	HL,FLAGS2
	BIT	2,(HL)
	JR	NZ,Q_HEAD	; jump if inside quotes
	CP	"G"
	JR	NZ,GE_HEAD
	LD	A,"X"
	JR	GE_HEAD
Q_HEAD:	CP	"E"
	JR	NZ,GE_HEAD
	LD	A,"U"
GE_HEAD:BIT	5,(HL)
	JR	Z,K_HEAD
	CP	"K"
	JR	NZ,K_HEAD
	INC	A		; LD A,"L" see $1900 in ROM1
	BIT	3,(HL)
	JR	Z,K_HEAD
	LD	A,"C"
K_HEAD:	LD	(RETADDR),BC
	PUSH	BC
	PUSH	DE
	EXX
	PUSH	BC
	PUSH	DE
	PUSH	HL
	SCF
	CALL	K_SWAP
	LD	HL,(K_CUR)
	LD	(K_CUR_S),HL
	LD	HL,(FLAGS - 1)
	LD	L,(IY+$30)
	SET	2,(IY+$30)		; set quote mode
	LD	(K_SAV),HL		; save FLAGS and FLAGS2
	PUSH	HL
	PUSH	AF			; save cursor character
	LD	DE,EDITOR_HEADER0
	CALL	MESSAGE
	BIT	5,(IY+$37)
	JR	Z,E_HEADB
	BIT	7,(IY+$37)
	JR	NZ,E_HEADT		; INPUT LINE (or EDIT string)
	LD	DE,EDITOR_HEADERS
	BIT	6,(IY+$37)
	JR	Z,E_HEADI
	LD	DE,EDITOR_HEADERN
E_HEADI:CALL	MESSAGE
	JR	E_HEAD0

E_HEADB:CALL	MESSAGE
	LD	A,(C_PCC)
	CALL	DECBYTE
E_HEAD0:LD	A,$06			; tabulation
	RST	$30
	DEFW	L0010
	POP	AF			; restore cursor character
	RST	$30
	DEFW	L0010			; and print it
	BIT	5,(IY+FLAGS2-ERR_NR)	; check K mode suppression
	LD	A,"!"
	CALL	Z,PRINT_A		; and indicate it
	LD	DE,EDITOR_HEADER1
	CALL	MESSAGE
	POP	AF			; restore FLAGS to A and FLAGS2 to F
	JP	PE,E_HEAD1
	RES	2,(IY+$30)		; restore quote mode
E_HEAD1:LD	(FLAGS),A
	AND	A
	CALL	K_SWAP
	POP	HL
	POP	DE
	POP	BC
	EXX
	POP	HL
	POP	BC
	POP	AF			; restore channel flag content into A
	POP	DE			; restore channel flag address into DE
	LD	(DE),A
	LD	(K_SAV2),A		; save channel flag content
	JP	TSTORE

E_HEADT:LD	DE,EDITOR_HEADERT
	CALL	MESSAGE
	LD	HL,(K_CUR)
	LD	DE,(WORKSP)
	AND	A
	SBC	HL,DE
	CALL	DECWORD
	LD	A,"/"
	RST	$30
	DEFW	L0010
	LD	HL,(STKBOT)
	LD	DE,(WORKSP)
	SCF
	SBC	HL,DE
	CALL	DECWORD
	JR	E_HEAD0

; Print condensed (4 pixel wide) character
; BC coordinates, HL target, DE matrix, CF side
PR_COND:EX	AF,AF'		; save CF
	PUSH	BC
	PUSH	HL
	LD	B,8
	RR	C
	LD	C,0
	LD	A,(P_FLAG)
	BIT	2,A
	JR	Z,NOINV
	DEC	C
NOINV:	BIT	0,A
	JR	NZ,POVER
	JR	NC,PRCL2B
	EX	AF,AF'
	JR	C,PRCL1
PRCL1A:	LD	A,(DE)
	XOR	C
	AND	$F0
	XOR	(HL)
	AND	$F0
	XOR	(HL)
	LD	(HL),A
	INC	H
	INC	DE
	DJNZ	PRCL1A
	JR	PRCDR

PRCL1:	RLD
	LD	A,(DE)
	XOR	C
	RRD
	INC	H
	INC	DE
	DJNZ	PRCL1
PRCDR:	DEC	H
	RST	$30
	DEFW	L0BDB		; PO-ATTR
	POP	HL
	JR	PORET
PRCL2B:	EX	AF,AF'
	JR	C,PRCL2
PRCL2A:	RRD
	EX	DE,HL
	RLD
	EX	DE,HL
	XOR	C
	RLD
	INC	H
	INC	DE
	DJNZ	PRCL2A
	JR	PORET1

PRCL2:	RRD
	LD	A,(DE)
	XOR	C
	RLD
	INC	H
	INC	DE
	DJNZ	PRCL2
PORET1:	DEC	H
	RST	$30
	DEFW	L0BDB		; PO-ATTR
	POP	HL
	INC	HL
PORET:	POP	BC
	DEC	C
	BIT	0,C
	JP	NZ,TSTOREA
	JP	TSTORE

POVER:	JR	NC,PROL2B
	EX	AF,AF'
	JR	C,PROL1
PROL1A:	LD	A,(DE)
	XOR	C
	AND	$F0
	XOR	(HL)
	LD	(HL),A
	INC	H
	INC	DE
	DJNZ	PROL1A
	JR	PRCDR

PROL1:	RLD
	EX	DE,HL
	XOR	(HL)
	EX	DE,HL
	XOR	C
	RRD
	INC	H
	INC	DE
	DJNZ	PROL1
	JR	PRCDR

PROL2B:	EX	AF,AF'
	JR	C,PROL2
PROL2A:	LD	A,(DE)
	XOR	C
	RRCA
	RRCA
	RRCA
	RRCA
	AND	$0F
	XOR	(HL)
	LD	(HL),A
	INC	H
	INC	DE
	DJNZ	PROL2A
	JR	PORET1

PROL2:	RRD
	EX	DE,HL
	XOR	(HL)
	EX	DE,HL
	XOR	C
	RLD
	INC	H
	INC	DE
	DJNZ	PROL2
	JR	PORET1

TEMPS_CONT:
	CALL	STACKSWAP
TEMPS3:	LD	A,(TV_FLAG)
	RRCA
	LD	A,(KS_PERM)
	JR	C,TEMPS_K
	RRCA
TEMPS_K:RRCA
	LD	A,4
	JR	C,TEMPS4
	ADD	A,A
TEMPS4:	LD	HL,S_STATE
	LD	DE,S_POSN
	BIT	0,(IY+TV_FLAG-ERR_NR)	; lower screen
	JR	Z,STEP1		; jump for S channel
	LD	C,A
	IN	A,($FF)
	LD	HL,BORDCR
	XOR	(HL)
	OR	$38
	XOR	(HL)
	OUT	($FF),A		; Set BORDER for TIMEX HiRes
	LD	A,C
	LD	HL,K_STATE
	INC	DE
	INC	DE
STEP1:	CP	4
	JR	Z,STEP4
	CP	8
	JP	NZ,ERROR_K
	BIT	6,(HL)
	RET	Z
	LD	A,(DE)
	RRA
	JR	C,STEP2
	EX	DE,HL
	PUSH	HL
	DEC	HL
	DEC	HL
	DEC	HL
	DEC	HL
	INC	(HL)
	POP	HL
	EX	DE,HL
STEP2:	ADC	A,0
	LD	(DE),A
	RES	6,(HL)
	INC	L
	SRL	(HL)
	INC	(HL)
	RET

STEP4:	BIT	6,(HL)
	RET	NZ
	LD	A,(DE)
	ADD	A,A
	DEC	A
	LD	(DE),A
	SET	6,(HL)
	INC	L
	SLA	(HL)
	DEC	(HL)
	RET

; Short routines without relative addresses to fill gaps

K_TEMPS:RST	$30
	DEFW	L0D4D	; TEMPS
	RES	7,(IY+ATTR_T-ERR_NR)	; flash off
	RET

	INCLUDE	"timexscr.asm"
