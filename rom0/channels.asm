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
; width of display + 1 (defaults to 33)
;
; byte 2:
; control character saved (see TV_DATA)
;
; byte 3:
; first argument saved (see TV_DATA + 1)

; Determine whether or not to suppress the leading space at the cursor position
; Pollutes: AF, HL, B
; Output: DE cursor pointer or one before depending on BIT 2,(TV_FLAG)
;;SP_CUR:	RES	0,(IY+$01)	; do not suppress leading space
;;	SCF
;;	RST	$30
;;	DEFW	L1195		; SET-DE
;;	LD	HL,(K_CUR)	; is the cursor
;;	BIT	2,(IY+TV_FLAG-ERR_NR)	; previous character?
;;	JR	Z,SP_CUR1
;;	DEC	HL
;;SP_CUR1:EX	DE,HL		; at the beginning of
;;	AND	A		; the current editor
;;	SBC	HL,DE		; buffer?
;;	JR	Z,ED_SPC	; if so, leading spaces must be suppressed
;;	DEC	DE		; before the cursor
;;	LD	A,(DE)		; do we
;;	INC	DE		; have
;;	CP	" "		; a space?
;;	JR	Z,ED_SPC	; if so, suppress leading space
;;	SUB	RND_T		; or a token?
;;	RET	C		; if not, do not suppress
;;	LD	HL,K_STATE	; if so, is it
;;	BIT	3,(HL)		; an instruction?
;;	JR	Z,ED_SPC	; if so, suppress leading space
;;	LD	B,0		; if it is an operator
;;	RRA			; CF = 0 at this point
;;	RL	B
;;	RRA
;;	RL	B
;;	RRA
;;	RL	B
;;	LD	HL,SPCTAB
;;	ADD	A,L
;;	LD	L,A
;;	JR	NC,SP_CNC
;;	INC	HL
;;SP_CNC:	LD	A,(HL)
;;	INC	B
;;ED_MASK:RLCA
;;	DJNZ	ED_MASK
;;	RET	NC
;;ED_SPC:	SET	0,(IY+$01)	; leading space suppression
;;	RET

; copy edit area
ED_COPY:PUSH	HL		; save K_STATE address
	SET	6,(HL)		; flashing cursor ON
	RES	3,(HL)		; begin with instructions
	RST	$30
	DEFW	L0D4D		; TEMPS set temporary attributes
        RES     3,(IY+$02)      ; update TV_FLAG  - signal no change in mode
        RES     5,(IY+$02)      ; update TV_FLAG  - signal don't clear lower
                                ; screen.
        LD      HL,(S_POSNL)      ; fetch SPOSNL
        PUSH    HL              ; and save on stack.

        LD      HL,(ERR_SP)      ; fetch ERR_SP
        PUSH    HL              ; and save also
        LD      HL,L1167        ; address: ED-FULL
        PUSH    HL              ; is pushed as the error routine
        LD      (ERR_SP),SP      ; and ERR_SP made to point to it.

        LD      HL,(ECHO_E)      ; fetch ECHO_E
        PUSH    HL              ; and push also

	LD	HL,TV_FLAG
	BIT	1,(HL)		; printing tail only?
	JR	Z,ED_ALL
	RES	1,(HL)		; reset flag
ED_TAIL:AND	A
	CALL	K_SWAP		; restore attributes
	LD	HL,(K_SAV)
	LD	DE,$FAFB
	LD	A,(FLAGS)
	AND	D
	XOR	H
	AND	D
	XOR	H
	LD	(FLAGS),A	; restore bits 0 and 2 of FLAGS
	LD	A,(FLAGS2)
	AND	E
	XOR	L
	AND	E
	XOR	L
	LD	(FLAGS2),A	; restore bit 2 of FLAGS2
	LD	A,(K_SAV2)
	AND	$08		; bit 3 of old K_STATE
	LD	HL,K_STATE
	RES	3,(HL)
	OR	(HL)
	LD	(HL),A		; restore bit 3 of K_STATE
	LD	BC,(RETADDR)
	CALL	CLSET
	LD	DE,(K_CUR)	; cursor position
	LD	HL,TV_FLAG
	BIT	2,(HL)		; step back?
	JR	Z,ED_CLP	; don't.
	RES	2,(HL)		; reset flag
	LD	DE,(K_CUR_S)	; old cursor position
	LD	A,D
	OR	E
	JR	NZ,ED_CLP
	SCF
ED_ALL:	SET	0,(IY+$01)	; leading space suppression
	PUSH	HL
	CALL	R_SPCC
	LD	HL,FLAGS
	RES	2,(HL)
	BIT	5,(IY+FLAGX-ERR_NR)
	JR	Z,ED_ALLE
	SET	2,(HL)
ED_ALLE:POP	HL
	RST	$30		; from the very beginning
	DEFW	L1195		; SET-DE
	RES	2,(IY+$30)	; not in quotes
ED_CLP:	LD	HL,(X_PTR)
	AND	A
	SBC	HL,DE
	JR	NZ,ED_CLPC
	LD	A,"?"
	RST	$30
	DEFW	OUT_FLASH
ED_CLPC:RST	$30
	DEFW	L18E1		; OUT-CURS
	LD	A,(DE)
	BIT	6,(IY+TV_FLAG-ERR_NR)	; print only to cursor position?
	JR	Z,ED_ATC	; don't
	LD	HL,(K_CUR)
	AND	A
	SBC	HL,DE
	JR	Z,ED_DONE
ED_ATC:	INC	DE
	CP	$0D
	JR	Z,ED_FIN
	RST	$30
	DEFW	L1937		; OUT-CHAR
	JR	ED_CLP

ED_FIN:	RST	$30
	DEFW	L18E1		; OUT-CURS
	LD	HL,(S_POSNL)
	EX	(SP),HL
	EX	DE,HL
	RST	$30
	DEFW	L0D4D		; TEMPS
ED_BLANK:
	LD	A,(S_POSNL + 1)
	SUB	D
	JR	C,ED_CDN
	JR	NZ,ED_SPC
	LD	A,E
	SUB	(IY+$50)
	JR	NC,ED_CDN
ED_SPC:	LD	A,$80
	PUSH	DE
	CALL	PR_GR
	POP	DE
	JR	ED_BLANK


ED_DONE:RST	$30
	DEFW	L18E1		; OUT-CURS
ED_CDN:	LD	HL,TV_FLAG
	RES	6,(HL)
	POP	DE
	POP	HL
	POP	HL
	LD	(ERR_SP),HL
	POP	BC
	PUSH	DE
	CALL	CLSET
	POP	HL
	LD	(ECHO_E),HL
	LD	(IY+$26),$00
	POP	HL		; discard SWAP
	RET

; channel K input service routine
K_IN:	LD	HL,SWAP
	PUSH	HL
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
	JP	NOPIP		; in silence

K_INNK:	LD	B,(HL)
	BIT	6,B
	RET	Z
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
	BIT	5,(IY+$30)	; mode K suppressed?
	JR	Z,K_IN1		; jump, if not
	SET	3,(IY+$01)	; switch off K mode
K_IN1:	BIT	5,(IY+$01)
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
NOCLSL:	CP	$18		; printable?
	JR	C,K_IN_C	; jump, if not
	SET	1,(IY+TV_FLAG-ERR_NR)
	SET	2,(IY+TV_FLAG-ERR_NR)
K_IN_C:	CP	$88
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
	RES	1,(IY+TV_FLAG-ERR_NR)	; echo from beginning
	LD	HL,TKETAB
	LD	BC,$0006
	CPIR
	JP	NZ,K_INSTT
K_TKE:	LD	(IY-$2D),$80	; signal potential token end
	LD	HL,K_STATE
	SET	7,(HL)
	SCF
	RET

K_ENT:	LD	HL,TV_FLAG
	LD	A,(HL)
	AND	$B5 		; reset bits 1,2 and 6 to echo full line (potential errors)
	LD	(HL),A
	LD	A,$0D		; restore A
	LD	HL,0
	LD	(K_CUR_S),HL	; reset old cursor position
	LD	HL,K_STATE
	LD	(IY+DEFADD+1-ERR_NR),1	; TODO: this is an ugly hack
	RES	6,(HL)		; turn off blinking cursor
K_ENTP:	RES	4,(IY+$37)	; allow ELSE
	SCF
	JR	K_ING3

K_INB:	BIT	3,(IY+$01)	; mode K?
	JR	NZ,K_MDL	; jump, if not
	LD	HL,K_MODE	; mode K translation
	LD	C,A
	CALL	INDEXER
	LD	A,C
	JR	NC,K_MDL
	LD	A,(HL)
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
	BIT	5,(IY+$30)	; mode K suppressed?
	JR	Z,KEY_MODE1	; jump, if not - toggle mode E
; EXT key in K suppressed-mode
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
	SUB	A,RND_T		; is the cursor after a token?
	JR	C,EXT_NT	; jump, if it is not
	LD	C,A
	LD	HL,K_STATE
	SET	7,(HL)
	BIT	3,(IY+$37)	; FLAGX, token type before cursor
	LD	HL,EXTTAB_I
	JR	Z,EXT_TS
	LD	HL,EXTTAB_O
EXT_TS:	ADD	HL,BC
	LD	A,(HL)
	LD	(K_DATA),A
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
	SET	3,(IY+2)
	CP	A
	RET

KEY_CUR:CALL	EDITOR_MODE	; editor mode?
	SCF
	RET	NZ		; all controls are passed on, if not
	CP	$08
	RET	C		; pass on what is not an arrow key
	CP	$0C
	CCF
	RET	C		; pass on what is not an arrow key
	LD	HL,TV_FLAG
	SET	6,(HL)		; stop listing at the cursor for movements
	CP	$0B		; up arrow
	JR	Z,K_HOME
	CP	$0A		; down arrow
	JR	Z,K_ENDK
	CP	$09		; right arrow
	SCF
	RET	NZ		; pass on, if not
	SET	1,(HL)
	SET	2,(HL)		; start listing before the cursor
	RET
K_ENDK:	LD	HL,(K_CUR)
	LD	A,$0D
	CP	(HL)		; cursor on CR?
	JR	Z,K_DOWN	; if so, pass on the down arrow
	LD	BC,0		; set the cursor to the line endig CR
	CPIR
	DEC	HL
	EX 	DE,HL
	JR	K_UCUR

K_DOWN:	LD	A,$0A
	SCF
	RET

EXT_NT:	LD	A,(HL)
	CP	"$"
	JR	Z,EXT_N
	CP	"#"
	JR	Z,EXT_NS
	CP	"@"
	JR	Z,EXT_N
	BIT	4,(IY+$37)	; FLAGX, operator mode
	JR	Z,NOREL
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
	BIT	4,(IY+$37)	; instruction mode?
	JR	NZ,EXT_OPR	; jump, if not
	CALL	TOK_INS
	JR	EXT_CNT

EXT_NR:	DEC	HL
	LD	B,2
EXT_OPR:CALL	TOK_OPR
EXT_CNT:JR	C,K_INSF
	OR	A
	JR	Z,EXT_NF
	EX	AF,AF'
	LD	A,(LAST_K)
	CP	$0E
	JR	Z,K_INST
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
	LD	A,(HL)
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
	JR	K_INSX

; This area must be a data table not to trigger the SAVE trap

; K-MODE translation table
K_MODE:	DEFB	POKE_T,PEEK_T
	DEFB	"@",LABEL_T
	DEFB	0

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
	RST	$30
	DEFW	L0C10 + 3	; PO-TOKENS + 3
	LD	A,D
	CP	3
	RET	NC
	JR	TSPACE

TOKEN2I:INC	B
	LD	DE,TOKENS1
	CALL	TOKEN
	RRA
	RST	$30
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
TOKEN2O:RST	$30
	DEFW	L0C10
	RET

K_SWAP:	LD	HL,ATTR_T
	LD	DE,K_ATTR
	JR	C,K_SAVE
	EX	DE,HL
K_SAVE:	LDI
	LDI
	LDI
	RET

; reset colon counters
R_SPCC:	LD	HL,C_SPCC
	LD	(HL),1
	DEC	L
	LD	(HL),1
	RET

; channel K ioctl
K_IOCTL:OR	A
	RET	NZ
K_RST:	LD	(HL),A
	LD	(K_SAV2),A
	RES	5,(IY+TV_FLAG-ERR_NR)	; no further clearing
	RST	$30
	DEFW	L0D4D		; TEMPS
	SCF
	CALL	K_SWAP
	CALL	R_SPCC
	LD	HL,DF_SZ
	LD	B,(HL)		; fetch lower screen line count
	LD	(HL),$02	; now set DF_SZ to 2
	RES	0,(IY+$02)	; clean hidden upper part
	CALL	CLLINE
	SET	0,(IY+$02)	; clean lower part
	LD	B,2
	CALL	CLLINE
	LD	BC,(K_WIDTH)
	LD	B,$17		; line 23 for lower screen
	LD	(RETADDR),BC
S_IO_E:	JP	CLSET

; channel S output service routine
S_OUT:	LD	HL,SWAP
	PUSH	HL
	BIT	4,(IY+TV_FLAG-ERR_NR)	; auto-list?
	JR	Z,S_OUT1
	EX	AF,AF'
	LD	A,(BANK_M)
	AND	$07
	RET	NZ			; no auto-list in X channel
	EX	AF,AF'
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
	XOR	A
	LD	(HL),A
	LD	HL,COORDX
	LD	DE,COORDX+1
	LD	BC,2*5-1
	LD	(HL),A
	LDIR			; clear last PLOT coordinates
	RES	0,(IY+$30)	; update FLAGS2 - signal main screen is clear.
	RST	$30
	DEFW	L0D4D		; TEMPS
	LD	B,$18		; 24 lines
	CALL	CLLINE
	LD	(IY+$52),$01	; set SCR_CT - scroll count - to default.
	LD	BC,(S_WIDTH)
	LD	B,$18		; line 24 for upper screen
	JR	S_IO_E

POFETCH:EQU	L0B03 + 6
POSTORE:EQU	L0ADC + 6

KS_CTRL:PUSH	HL		; save display address
	PUSH	BC		; save coordinates
	LD	C,A
	LD	B,0
	LD	HL,TCTRL
	ADD	HL,BC
	LD	C,(HL)
	ADD	HL,BC
	POP	BC		; restore coordinates
	EX	(SP),HL		; restore display address, stack destination
	RET

; channel K output service routine
K_OUT:	LD	HL,SWAP
	PUSH	HL
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
	CP	$20
	JP	C,PR_GR_0
	CP	$80
	JP	NC,P_GR_TK
	CP	":"
	JR	NZ,PR_NC	; pr-able except colon leaves mode unchanged
	EX	DE,HL		; restore S/K_STATE into HL and save screen address to DE
	RES	3,(HL)		; colon sets instr. mode
	BIT	2,(IY+$30)	; inside quotes?
	JR	NZ,PR_NQ	; if so, jump over instr. count increment
	PUSH	HL
	LD	HL,C_SPCC
	INC	(HL)
	POP	HL
PR_NQ:	EX	DE,HL		; restore screen address to HL
PR_NC:	PUSH	BC
	LD	BC,(CHARS)
PR_CH2:	EX	DE,HL
	EX	AF,AF'
	LD	A,L
	ADD	A,4
	LD	L,A
	LD	A,(HL)
	EX	AF,AF'
	LD	HL,FLAGS
	RES	0,(HL)
	CP	" "
	JR	NZ,PR_CH3
	SET	0,(HL)
PR_CH3:	LD	H,$00
	ADD	A,A
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
	JP	C,ERROR_5
	JP	CLSET

COTEMP5:INC	SP		; TODO; proper handling in this ROM
	INC	SP		; discard SWAP
	PUSH	HL
	LD	HL,L2211	; CO-TEMP-5
PRTABC:	EX	(SP),HL
	JP	SWAP

PRTAB:	LD	DE,MEMBOT+8
	LD	A,H
	JP	POFILL

KS_IND2:RES	1,(HL)
	INC	HL	; width
	INC	HL	; tv1
	INC	HL	; tv2
	LD	(HL),A
	RET

P_GR_TK:CP	$90
	JR	NC,PR_T_UDG
PR_GR:	LD	B,A
	RST	$30
	DEFW	L0B38		; PO-GR-1 mosaic
	JR	PR_GR_E

PR_T_UDG:
	SUB	RND_T
	JR	NC,PR_TK
	ADD	A,$15
	PUSH	BC
	LD	BC,(UDG)
	JP	PR_CH2

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
PR_GR_E:RST	$30
	DEFW	POFETCH
	LD	DE,MEMBOT
	JP	PR_ALL

PR_TK:	EX	DE,HL
	BIT	3,(HL)
	RES	2,(IY+$37)	; FLAGX, signal instruction token
	JR	Z,PR_INST
	SET	2,(IY+$37)	; FLAGX, signal operator token
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
	BIT	5,(HL)
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
	LD	A,(C_SPCC)
	CALL	DECBYTE
E_HEAD0:LD	A,$06			; tabulation
	RST	$10
	POP	AF			; restore cursor character
	RST	$10			; and print it
	BIT	5,(IY+FLAGS2-ERR_NR)	; check K mode suppression
	LD	A,"!"
	CALL	Z,$0010		; and indicate it
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
TSTORE3:JP	TSTORE

E_HEADT:LD	DE,EDITOR_HEADERT
	CALL	MESSAGE
	LD	HL,(K_CUR)
	LD	DE,(WORKSP)
	AND	A
	SBC	HL,DE
	CALL	DECWORD
	LD	A,"/"
	RST	$10
	LD	HL,(STKBOT)
	LD	DE,(WORKSP)
	SCF
	SBC	HL,DE
	CALL	DECWORD
	JR	E_HEAD0

TKETAB:	DEFM	"@$#<>="

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
	DEFB	T1CTR - $	; $0F prints inverted character
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
	JP	PR_NC

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
	PUSH	DE
	EXX
	RST	$10
	EXX
	POP	DE
	DEC	D
	JR	NZ,POSPACE
TNOP:	RET

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
	JP	SWAP

TCR:	PUSH	DE
	CALL	TCR0
	POP	HL
	RES	3,(HL)
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
CLSET2:	LD	A,(S_WIDTH)
	BIT	0,(IY+$02)
	JR	Z,CLSET3
	LD	A,(K_WIDTH)
CLSET3:	SUB	C
	LD	E,A
	LD	D,0
	LD	A,(S_MODE)
	CP	$10
	JR	C,CLSET4
	SRL	E
	JR	NC,CLSET4
	SET	5,H
CLSET4:	ADD	HL,DE
	JP	TSTORE

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
	RET

TUP:	INC	B
	LD	A,$18	; TODO: 24 lines
	CP	B
	JR	NC,TCR1
	DEC	B
	JR	TCR1

TBLINK:	BIT	0,(IY+TV_FLAG-ERR_NR)
	JR	Z,T1CTR
	RET	Z
	EX	DE,HL
	SET	2,(HL)	; set blinking or inverse
	BIT	3,(HL)
	LD	HL,FLAGX
	RES	4,(HL)
	JR	Z,TBL_I
	SET	4,(HL)
TBL_I:	LD	A,(HL)
	RES	3,(HL)
	AND	$04
	RET	Z
	SET	3,(HL)
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

;;POSCR:	RST	$30	; TODO: take width into account
;;	DEFW	L0C55	; PO-SCR
;;	RET

S_IOCTL:DEFW	S_RST	; reset S channel (clear screen, etc.)
	DEFW	TNOP	; COPY screen to itself (i.e. do nothing)
	DEFW	PLOT1	; PLOT a single point
	DEFW	DRAW2	; DRAW straight line
	DEFW	DRAW3	; DRAW arc
	DEFW	CIRCLE	; draw a CIRCLE
S_IOCTL_END:	EQU	$

EDITOR_HEADER0:
	DEFB	$14,$01,$16,$00,$00,$13,$01,$10,$00
	DEFB	$11,$87
	DEFM	"BASIC"
	DEFB	$80 + ":"
EDITOR_HEADER1:
	DEFB	$17,$7A,$00,$11,$02,$18,$10,$06,$1A
	DEFB	$11,$04,$18,$10,$05,$1A,$11,$00,$18
	DEFB	$10,$00,$9A
EDITOR_HEADERT:
	DEFM	"TEXT"
	DEFB	$80 + " "
EDITOR_HEADERN:
	DEFM	"NUMERI"
	DEFB	$80 + "C"
EDITOR_HEADERS:
	DEFM	"STRIN"
	DEFB	$80 + "G"

GR_TAB:	DEFB	$00, $FF
	DEFB	$FF, $00
	DEFB	$F0, $00
	DEFB	$00, $0F

EXTTAB_I:
	DEFB	$A5		; no change
	DEFB	$A6		; no change
	DEFB	$BA		; RENUM followed by REPEAT
	DEFB	$B9		; DEF PROC followed by DELETE
	DEFB	$A9		; no change
	DEFB	$CD		; STACK followed by STEP
	DEFB	$AB		; @ followed by @
	DEFB	$F5		; POP followed by PRINT
	DEFB	$F6		; PLAY followed by PLOT
	DEFB	$AE		; no change
	DEFB	$AF		; no change
	DEFB	$B0		; no change
	DEFB	$B1		; no change
	DEFB	$B2		; no change
	DEFB	$D2		; END WHILE followed by ERASE
	DEFB	$D3		; ON ERROR followed by OPEN #
	DEFB	$B5		; no change
	DEFB	$B6		; no change
	DEFB	$B7		; no change
	DEFB	$E0		; LOCAL followed by LPRINT
	DEFB	$E9		; DELETE followed by DIM
	DEFB	$E5		; REPEAT followed by RESTORE
	DEFB	$BB		; no change
	DEFB	$BC		; no change
	DEFB	$BD		; no change
	DEFB	$AC		; POKE followed by POP
	DEFB	$BF		; no change
	DEFB	$C2		; USR followed by UNTIL
	DEFB	$C1		; no change
	DEFB	$C0		; UNTIL followed by USR
	DEFB	$C3		; ASSERT followed by ASSERT
	DEFB	$C4		; no change
	DEFB	$CA		; END IF followed by END PROC
	DEFB	$C6		; YIELD followed by YIELD
	DEFB	$DA		; PALETTE followed by PAPER
	DEFB	$CB		; EXIT followed by ELSE
	DEFB	$C9		; WHILE followed by WHILE
	DEFB	$B3		; END PROC followed by END WHILE
	DEFB	$C5		; ELSE followed by END IF
	DEFB	$C7		; PROC followed by PALETTE
	DEFB	$E2		; STEP followed by STOP
	DEFB	$A8		; DEF FN followed by DEF PROC
	DEFB	$D8		; CAT followed by CIRCLE
	DEFB	$DB		; FORMAT followed by FLASH
	DEFB	$D5		; MOVE followed by MERGE
	DEFB	$C8		; ERASE followed by EXIT
	DEFB	$DF		; OPEN # followed by OUT
	DEFB	$FB		; CLOSE # followed by CLS
	DEFB	$D1		; MERGE followed by MOVE
	DEFB	$D6		; VERIFY followed by VERIFY
	DEFB	$E7		; BEEP followed by BORDER
	DEFB	$FD		; CIRCLE followed by CLEAR
	DEFB	$EE		; INK followed by INPUT
	DEFB	$F2		; PAPER followed by PAUSE
	DEFB	$EB		; FLASH followed by FOR
	DEFB	$D7		; BRIGHT followed by BEEP
	DEFB	$FA		; INVERSE followed by IF
	DEFB	$B4		; OVER followed by ON ERROR
	DEFB	$DE		; OUT followed by OVER
	DEFB	$F1		; LPRINT followed by LET
	DEFB	$EF		; LLIST followed by LOAD
	DEFB	$F8		; STOP followed by SAVE
	DEFB	$EA		; READ followed by REM
	DEFB	$CE		; DATA followed by DEF FN
	DEFB	$FE		; RESTORE followed by RETURN
	DEFB	$F3		; NEW followed by NEXT
	DEFB	$DC		; BORDER followed by BRIGHT
	DEFB	$FF		; CONTINUE followed by COPY
	DEFB	$FC		; DIM followed by DRAW
	DEFB	$A7		; REM followed by RENUM
	DEFB	$D0		; FOR followed by FORMAT
	DEFB	$ED		; GO TO followed by GO SUB
	DEFB	$EC		; GO SUB followed by GO TO
	DEFB	$DD		; INPUT followed by INVERSE
	DEFB	$E0		; LOAD followed by LPRINT
	DEFB	$E1		; LIST followed by LLIST
	DEFB	$F0		; LET followed by LIST
	DEFB	$AD		; PAUSE followed by PLAY
	DEFB	$E6		; NEXT followed by NEW
	DEFB	$AC		; POKE followed by POP
	DEFB	$CC		; PRINT followed by PROC
	DEFB	$BE		; PLOT followed by POKE
	DEFB	$F9		; RUN followed by RANDOMIZE
	DEFB	$AA		; SAVE followed by STEP
	DEFB	$E3		; RANDOMIZE followed by READ
	DEFB	$D9		; IF followed by INK
	DEFB	$E8		; CLS followed by CONTINUE
	DEFB	$CE		; DRAW followed by DEF FN
	DEFB	$D4		; CLEAR followed by CLOSE #
	DEFB	$F7		; RETURN followed by RUN
	DEFB	$CF		; COPY followed by CAT

EXTTAB_O:
	DEFB	$D6		; RND followed by REF
	DEFB	$BA		; INKEY$ followed by INK
	DEFB	$A9		; PI followed by POINT
	DEFB	$CE		; FN followed by FREE
	DEFB	$DA		; POINT followed by PAPER
	DEFB	$BC		; SCREEN$ followed by SGN
	DEFB	$BD		; ATTR followed by ABS
	DEFB	$B7		; AT followed by ATN
	DEFB	$B4		; TAB followed by TAN
	DEFB	$B0		; VAL$ followed by VAL
	DEFB	$B3		; CODE followed by COS
	DEFB	$AE		; VAL followed by VAL$
	DEFB	$CA		; LEN followed by LINE
	DEFB	$BB		; SIN followed by SQR
	DEFB	$C2		; COS followed by CHR$
	DEFB	$CB		; TAN followed by THEN
	DEFB	$AC		; ASN followed by AT
	DEFB	$C6		; ACS followed by AND
	DEFB	$AB		; ATN followed by ATTR
	DEFB	$B1		; LN followed by LEN
	DEFB	$D4		; EXP followed by EOF #
	DEFB	$DD		; INT followed by INVERSE
	DEFB	$CD		; SQR followed by STEP
	DEFB	$B2		; SGN followed by SIN
	DEFB	$B6		; ABS followed by ACS
	DEFB	$A7		; PEEK followed by PI
	DEFB	$D9		; IN followed by INK
	DEFB	$C0		; USR followed by USR
	DEFB	$AA		; STR$ followed by SCREEN$
	DEFB	$AF		; CHR$ followed by CODE
	DEFB	$C3		; NOT followed by NOT
	DEFB	$DC		; BIN followed by BRIGHT
	DEFB	$DE		; OR followed by OVER
	DEFB	$B5		; AND followed by ASN
	DEFB	$C8		; <= followed by >=
	DEFB	$C9		; >= followed by <>
	DEFB	$C7		; <> followed by <=
	DEFB	$B8		; LINE followed by LN
	DEFB	$D0		; THEN followed by TIME
	DEFB	$AD		; TO followed by TAB
	DEFB	$D1		; STEP followed by STICK
	DEFB	$DB		; FREE followed by FLASH
	DEFB	$CF		; MEM$ followed by MEM$
	DEFB	$D5		; TIME followed by TIME$
	DEFB	$C1		; STICK followed by STR$
	DEFB	$E4		; DPEEK followed by DATA
	DEFB	$C5		; OPEN # followed by OR
	DEFB	$B9		; EOF # followed by EXP
	DEFB	$D0		; TIME$ followed by TIME
	DEFB	$A5		; REF followed by RND
	DEFB	$D7		; unchanged
	DEFB	$D8		; HEX followed by HEX
	DEFB	$A6		; INK followed by INKEY$
	DEFB	$BE		; PAPER followed by PEEK
	DEFB	$A8		; FLASH followed by FN
	DEFB	$C4		; BRIGHT followed by BIN
	DEFB	$BF		; INVERSE followed by IN
	DEFB	$DF		; OVER followed by OCT
	DEFB	$D3		; OCT followed by OPEN #
	DEFB	$E0		; unchanged
	DEFB	$E1		; unchanged
	DEFB	$E3		; >< followed by <<
	DEFB	$E5		; << followed by >>
	DEFB	$D2		; DATA followed by DPEEK
	DEFB	$E2		; >> followed by ><

	INCLUDE	"timexscr.asm"
