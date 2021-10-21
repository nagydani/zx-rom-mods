
ERR_BRACE:
	POP	HL
	LD	(ERR_SP),HL
	LD	SP,HL
	LD	A,$0D
	LD	HL,(X_PTR)
	LD	BC,0
	CPIR
	DEC	HL
	LD	(HL),"}"
ERRBSW:	RST	$10

F_BRACE:LD	(HL),$0D	; temporarily place an end-of-line marker
	LD	HL,(ERR_SP)
	PUSH	HL		; save ERR_SP
	LD	HL,ERR_BRACE
	PUSH	HL		; set error handler
	CALL	STACKSWAP	; in this ROM
	LD	(ERR_SP),SP
	LD	(CH_ADD),DE
	PUSH	DE		; save beginning-of-expression
	RST	$30
	DEFW	L24FB		; SCANNING
	CP	$0D
	JP	NZ,ERROR_C
	POP	HL		; restore beginning-of-expression
	RST	$30
	DEFW	L11A7		; REMOVE_FP
	LD	(CH_ADD),HL
	DEC	HL
	LD	(HL),"}"	; restore closing brace
	POP	HL
	POP	HL		; discard error handler
	POP	HL
	LD	(ERR_SP),HL
	JR	S_BRCE

SCANFUNC2:
FUNCTAB:DEFB	FPEEK_T
	DEFB	S_FPEEK-$	; FPEEK
	DEFB	DPEEK_T
	DEFB	S_DPEEK-$	; DPEEK
	DEFB	STICK_T
	DEFB	S_STICK-$	; STICK
	DEFB	TIMES_T
	DEFB	S_TIMES-$	; TIME$
	DEFB	TIME_T
	DEFB	S_TIME_J-$	; TIME
	DEFB	REF_T
	DEFB	S_REF_J-$	; REF
	DEFB	MEM_T
	DEFB	S_MEM-$		; MEM$
	DEFB	FREE_T
	DEFB	S_FREE-$	; FREE
	DEFB	"@"
	DEFB	S_LBL - $
	DEFB	"{"
	DEFB	S_BRACE - $
	DEFB	HEX_T
	DEFB	S_HEX - $
	DEFB	OCT_T
	DEFB	S_OCT - $
	DEFB	0

S_TIME_J:JP	S_TIME
S_REF_J:JP	S_REF

S_BRACE:RST	$18		; HL = address of opening brace
	PUSH	HL		; save beginning
	LD	BC,$0000	; nesting depth
S_BRCL1:INC	BC
S_BRCL:	RST	$20
	CP	$0D		; missing closing brace
	JP	Z,ERROR_C
	CP	"{"
	JR	Z,S_BRCL1
	CP	"}"
	JR	NZ,S_BRCL
	DEC	BC
	LD	A,B
	OR	C
	JR	NZ,S_BRCL
	POP	DE		; restore beginning
	INC	DE		; step past opening brace
	CALL	SYNTAX_Z
	JR	Z,F_BRACE
	XOR	A		; referenced string
	SBC	HL,DE
	LD	B,H
	LD	C,L
	RST	$20		; step past the closing brace
S_BRCE:	LD	HL,L25DB	; S-STRING
	JP	HLSWAP

S_TIMES:CALL	SYNTAX_Z
	JR	Z,F_SNUM
	LD	BC,D_TIMES
	JR	S_FUNC

F_STR0:	PUSH	BC
	RST	$10

S_LBL:	CALL	SYNTAX_Z
	JP	Z,S_LBLS
	RST	$20
	LD	(MEMBOT+26),HL	; label start
L_LBL:	LD	A,(HL)
	CP	$0E
	INC	HL
	JR	NZ,L_LBL
	JP	D_LBL

F_NUM:	LD	BC,L270D	; S-PUSH-PO
	PUSH	BC
	LD	BC,$10EB	; like PEEK
RSWAP:	RST	$10

F_STR:	LD	BC,L270D	; S-PUSH-PO
	PUSH	BC
	LD	BC,$109C	; like CODE
	RST	$10

F_SNUM:	LD	BC,L270D	; S-PUSH-PO
	PUSH	BC
	LD	BC,$106E	; like STR$
	RST	$10

S_MEM:	RST	$20
	LD	HL,FLAGS
	RES	6,(HL)
	BIT	7,(HL)
	JR	Z,S_MEM_END
	LD	DE,1
	LD	BC,$FFFF
	RST	$30
	DEFW	L2AB1		; STK-ST-0
S_MEM_END:
	LD	BC,L2712
	PUSH	BC
	RST	$10

S_FPEEK:LD	BC,D_FPEEK
	JR	S_FUNC
S_DPEEK:LD	BC,D_DPEEK
S_FUNC:	LD	A,$10		; priority
S_OPERN:CALL	SYNTAX_Z
	JR	Z,S_OPNS
	PUSH	BC
	LD	BC,FSCAN
	PUSH	AF
	RST	$30
	DEFW	L2D2B + 4	; STACK-BC
	POP	AF
S_OPNS:	LD	B,A
	LD	C,$ED		; USR
	PUSH	BC
	LD	BC,L270D	; S-PUSH-PO
	PUSH	BC
	LD	B,A
	LD	C,$C1		; EXCHANGE number,number
	RST	$10

S_FREE:	CALL	SYNTAX_Z
	JR	Z,S_TEND
	RST	$30
	DEFW	L1F1A		; FREE-MEM
	OR	A
	SBC	HL,BC
	SBC	HL,BC
	LD	C,L
	LD	B,H
	LD	HL,L2630	; S-PI-END
	PUSH	HL
	LD	HL,L2D2B + 4	; STACK-BC
	JR	HLSWAP

S_OCT:	CALL 	SYNTAX_Z
	JR	NZ,S_STK_NUM
	LD	C,$08
	JR	S_NUM

S_HEX:	CALL 	SYNTAX_Z
	JR	NZ,S_STK_NUM
	LD	C,$10
S_NUM:	RST	$20		; skip prefix
	RST	$30
	DEFW	DEC2FP + 2
	RST	$30
	DEFW	L2C9B + 3
	LD	HL,L268D + 8	;  S_DECIMAL
	JR	HLSWAP

S_STICK:RST	$20
	CP	"("
	JR	NZ,ERRCNZ2
	CALL	NEXT_1NUM
	CP	","
	JR	NZ,ERRCNZ2
	RST	$20
	RST	$30
	DEFW	L24FB + 1	; SCANNING + 1
	CP	")"
ERRCNZ2:JP	NZ,ERROR_C
	CALL	SYNTAX_Z
S_TEND:	JR	Z,S_TIME_END
	BIT	6,(IY+$01)
	JP	Z,STICK_STR	; second argument is a string
	RST	$30
	DEFW	L1E94		; FIND-INT1
	CP	STICK_TAB_E - STICK_TAB
	JR	NC,ERROR_B
	LD	HL,STICK_TAB
	LD	B,0
	ADD	HL,BC
	LD	C,(HL)
	ADD	HL,BC
	LD	C,5
	JP	(HL)

S_STK_NUM:
	LD	HL,L26B5
HLSWAP:	PUSH	HL
RSWAP1:	RST	$10

S_TIME:	CALL	SYNTAX_Z
	JR	Z,S_TIME_END
TIME_R:	LD	A,(FRAMES+2)
	LD	B,A
	LD	HL,(FRAMES)
	LD	A,(FRAMES+2)
	LD	DE,(FRAMES)
	CP	B
	JR	NZ,TIME_R
	SBC	HL,DE
	JR	NZ,TIME_R
	OR	A
	JR	Z,TIME_L
	LD	C,E
	LD	E,A
	LD	B,L
	LD	A,$98
TIME_N:	BIT	7,E
	JR	NZ,TIME_D
	SLA	C
	RL	D
	RL	E
	DEC	A
	JR	TIME_N

TIME_D:	RES	7,E
	JR	TIME_S
TIME_L:	LD	C,D
	LD	D,E
	LD	E,L
	LD	B,L
TIME_S:	RST	$30
	DEFW	L2AB6		; STK-STORE
S_TIME_END:
	LD	BC,L2630	; S-PI-END
S_FNC_E:PUSH	BC
S_SWAP:	LD	B,0
	RST	$10

ERROR_B:RST	$30
	DEFW	L1E9F		; B Integer out of range

D_DPEEK:RST	$30
	DEFW	L1E99		; FIND_INT2
	LD	L,C
	LD	H,B
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	RST	$10

D_FPEEK:RST	$30
	DEFW	L1E99		; FIND_INT2
	POP	DE		; discard return address
	LD	DE,(STKEND)
	LD	L,C
	LD	H,B
	RST	$30
	DEFW	L33A9		; TEST-5-SP
	LDIR
	RST	$10

S_LBLS:	LD	B,0
	CALL	S_LBLL
S_LBLE:	LD	BC,L26C3	; S_NUMERIC
	PUSH	BC
CSWAPR:	RST	$10

S_LBLL:	RST	$20
	RST	$30
	DEFW	L2C88		; ALPHANUM
	INC	B
	JR	Z,ERR_CL
S_LBLI:	JR	C,S_LBLL
	DJNZ	S_LBLC
ERR_CL:	JP	ERROR_C
S_LBLC:	LD	BC,$0006
	RST	$30
	DEFW	L1655		; MAKE-ROOM
	INC	HL
	LD	(HL),$0E
	LD	BC,$0500	; TODO: maybe B is enough
S_LBL0:	INC	HL
	LD	(HL),C
	DJNZ	S_LBL0
	RST	$30
	DEFW	L0077		; TEMP-PTR1
	RET

MFNTAB:	DEFB	$6E		; STR$
	DEFB	S_STR-$
	DEFB	$6F		; CHR$
	DEFB	S_CHR-$
	DEFB	0

; STR$() with multiple arguments
S_STR:	CALL	SYNTAX_Z
	JR	Z,S_STR_S
	LD	BC,$0001
	RST	$30
	DEFW	L0030		; BC-SPACES
	LD	(K_CUR),HL
	PUSH	HL		; save cursor
	LD	HL,(CURCHL)
	PUSH	HL		; save channel
	LD	A,$FF
	RST	$30
	DEFW	L1601		; CHAN-OPEN, open R channel
	RST	$28
	DEFB	$38		; end
	INC	HL
	BIT	7,(HL)		; check sign
	JR	Z,S_STR_S	; if positive, do nothing
	LD	A,"-"
	RST	$30
	DEFW	L0010		; print "-"
	RST	$30
	DEFW	L346E		; NEGATE
S_STR_S:RST	$20
	RST	$30
	DEFW	L1C82		; CLASS_06, numeric expression followed by whatever
	CALL	SYNTAX_Z
	JR	Z,S_ST0_S
	PUSH	AF
	RST	$28
	DEFB	$31		; duplicate
	DEFB	$38		; end
	RST	$30
	DEFW	L1E94		; FIND-INT1
	CP	2
	JR	C,ERROR_B_2	; base zero and one prohibited
	CP	37		; maximum base 36, digits [0-9A-Z]
	JR	NC,ERROR_B_2
	LD	(MEMBOT+27),A	; save base
	POP	AF
S_ST0_S:CP	")"
	JR	NZ,S_STR3
	CALL	SYNTAX_Z
	JR	Z,S_STR_D
	PUSH	AF
	RST	$28
	DEFB	$A0		; stk-zero
	DEFB	$38		; end
	POP	AF
	JR	S_STR_D

ERROR_C:RST	$30
	DEFW	L1C8A		; C Nonsense in BASIC

S_STR3:	CP	","
	JR	NZ,ERROR_C
	RST	$20
	RST	$30
	DEFW	L1C82		; CLASS_06, numeric expression followed by whatever
	CP	")"
	JR	NZ,ERROR_C	; must be followed by ")"
	CALL	SYNTAX_Z
S_STR_D:JR	Z,S_STR_END
	RST	$30
	DEFW	L1E94		; FIND-INT1
	OR	A
	JP	M,ERROR_B	; No more than 127 places after dot
	PUSH	AF		; Number of digits on CPU stack, Z set, if 0
	JR	Z,NROUND
	LD	HL,MEMBOT
	RST	$30
	DEFW	L350B		; ZERO to M0
	POP	AF
	PUSH	AF
	RST	$30
	DEFW	L2D60		; E-LOOP
	LD	A,(MEMBOT+27)
	RST	$30
	DEFW	L2D28		; STACK-A
NROUND:	CALL	MOD2A
	CALL	PR_DIGIT
	INC	HL
	INC	DE		; adjust pointers
	POP	AF
	DEC	A
	PUSH	AF
	ADD	A,A
	JR	NZ,STR_DG
	LD	A,"."
	RST	$30
	DEFW	L0010
	INC	HL
	INC	DE		; adjust pointers
STR_DG:	JR	NC,STR_FR	; still fractional
	RST	$30
	DEFW	L34E9		; TEST-ZERO
	JR	C,D_STR_E
STR_FR:	LD	A,(MEMBOT+27)
	RST	$30
	DEFW	L2D28		; STACK-A
	JR	NROUND

S_CHR:	RST	$20
	RST	$30
	DEFW	L1C82		; CLASS_06, numeric expression followed by whatever
	CALL	SYNTAX_Z
	JR	NZ,D_CHR
	CP	")"
	JR	NZ,ERROR_C
	JR	S_STR_END

ERROR_B_2:
	JP	ERROR_B

D_STR_E:POP	AF		; fractional digits, Z set if zero
	LD	(STKEND),HL
	CALL	STEPBACK	; remove zero quotient from stack
	POP	HL		; restore channel
	RST	$30
	DEFW	L1615		; channel flags
	POP	DE		; start pointer
	LD	HL,(K_CUR)
	AND	A
	SBC	HL,DE
	LD	B,H
	LD	C,L
	PUSH	BC
	PUSH	DE
	EX	DE,HL
	LD	A,(HL)
	CP	"-"
	JR	NZ,STR_P
	INC	HL
	DEC	BC
STR_P:	CALL	MIRROR
	POP	DE
	POP	BC
S_STR_END:
	LD	HL,X266E	; advance and stack string
R_CHR:	PUSH	HL
	RST	$10

D_CHR:	RST	$20		; advance past closing bracket
	RST	$30
	DEFW	L1E94		; FIND-INT1
	DEC	A
	JR	NZ,D_CHRN1
DCHR1:	LD	HL,L2712	; S-CONT-2
	PUSH	HL
	LD	HL,L35C9	; CHRS
	JR	R_CHR

D_CHRN1:DEC	A
	JR	NZ,D_CHRN2

	RST	$30
	DEFW	L1E99		; FIND-INT2
D_CHR2:	PUSH	BC
	LD	BC,$0002
	RST	$30
	DEFW	L0030		; BC-SPACES
	EX	DE,HL
	POP	DE
	LD	(HL),E
	INC	HL
	LD	(HL),D
	DEC	HL
	EX	DE,HL
CHR_E:	RST	$30
	DEFW	L2AB2		; STK-STO
	LD	HL,L2712
	JR	R_CHR

D_CHRN2:ADD	A,2
	JR	NZ,D_CHRF

D_CHR0:	RST	$28
	DEFB	$38		; end
	LD	A,(HL)
	OR	A
	JR	NZ,D_CHRL
	RST	$30
	DEFW	L1E99		; FIND-INT2
	LD	A,B
	OR	A
	JR	NZ,D_CHR2
	LD	A,C
D_CHRZ:	LD	HL,L2712
	PUSH	HL
	LD	HL,CHRS_X
	JR	R_CHR

D_CHRL0:RST	$28
	DEFB	$02		; delete
	DEFB	$38		; end
	XOR	A
	JR	D_CHRZ

D_CHRL:	INC	HL
	BIT	7,(HL)
ERROR_B_3:
	JR	NZ,ERROR_B_2
	DEC	HL
	LD	A,(HL)
	SUB	$79
	JR	C,D_CHRL0
	CP	8
	JR	C,D_CHRL0
	RRCA
	RRCA
	RRCA
	AND	$1F
D_CHRF:	LD	C,A
	LD	B,0
	PUSH	BC
	INC	BC
	RST	$30
	DEFW	L0030		; BC-SPACES
	EX	DE,HL
	LD	(MEMBOT+28),HL
	LD	(K_CUR),HL
	LD	(HL),B
	LD	E,L
	LD	D,H
	INC	DE
	DEC	BC
	LDIR
	LD	(HL),$80
CHRL_L:	RST	$28
	DEFB	$34,$80,$B0,$00,$00,$01	; stk-data 256
	DEFB	$38			; end
	CALL	MOD2A
	LD	HL,(K_CUR)
	BIT	7,(HL)
	JR	NZ,ERROR_B_3
	LD	(HL),A
	INC	HL
	LD	(K_CUR),HL
	RST	$28
	DEFB	$38		; end
	LD	A,(HL)
	OR	A
	JR	NZ,CHRL_L
	INC	HL
	INC	HL
	LD	DE,(K_CUR)
	LD	A,(DE)
	OR	A
	JR	NZ,ERROR_B_3
	LDI
	LD	A,(DE)
	OR	A
	JR	NZ,ERROR_B_3
	LDI
	RST	$28
	DEFB	$02		; delete
	DEFB	$38
	LD	DE,(MEMBOT+28)
	POP	BC
	JP	CHR_E

D_TIMES:POP	HL		; discard return address
	POP	HL		; RE-ENTRY
	POP	DE		; discard BREG?
	POP	DE		; discard USR
	LD	DE,$106F	; CHR$
	PUSH	DE		; replace by CHR$
	LD	DE,0
	PUSH	DE		; BREG = 0
	PUSH	HL
	LD	A,(BEAT)
	CP	125		; 50Hz interrupt?
	JR	Z,TIMESC
	RST	$30
	DEFW	L2D28		; STACK-A
	RST	$28
	DEFB	$05			; division
	DEFB	$34,$40,$B0,$00,150	; stk-data
	DEFB	$04			; multiply
	DEFB	$38			; end-calc
TIMESC:	LD	BC,2+1+2+1+2+1+2
	RST	$30
	DEFW	L0030		; BC-SPACES
	EX	DE,HL
	LD	HL,TIMEFE
TIMESL:	LD	A,(HL)
	CP	$0B
	JR	NC,TIMESD
	PUSH	BC
	PUSH	DE
	PUSH	HL
	RST	$30
	DEFW	L2D28		; STACK-A
	CALL	MOD2A
	POP	HL
	POP	DE
	POP	BC
	ADD	"0"
	LD	(DE),A
	LD	A,C
	CP	2+1+2+1+2+1+2
	JR	NZ,TIMESN
	LD	A,(DE)
	ADD	A,A
	SUB	A,"0"
	LD	(DE),A
TIMESN:	DEC	HL
	DEC	DE
	DEC	C
	JR	NZ,TIMESL
	INC	DE
	XOR	A
	LD	HL,(STKEND)
	DEC	HL
	LD	(HL),A
	DEC	HL
	LD	(HL),2+1+2+1+2+1+2
	DEC	HL
	LD	(HL),D
	DEC	HL
	LD	(HL),E
	LD	DE,(STKEND)
TMSW:	RST	$10

TIMESD:	LDD
	JR	TIMESL

TIMEF:	DEFB	10,10,":",6,10,":",6,10,".",10
TIMEFE:	DEFB	5

ERROR_2:RST	$30
	DEFW	L0670		; 2 Variable not found

S_REF:	RST	$20		; advance past the REF token
	RST	$30
	DEFW	L28B2		; LOOK-VARS
	JR	C,ERROR_2
	JR	NZ,NOREF
	RST	$30
	DEFW	L2996		; STK_VAR
	LD	A,(FLAGS)
	ADD	A,A
	JP	NC,S_LBLE
	ADD	A,A
	JR	C,S_REFBC
	RST	$30
	DEFW	L2BF1		; STK-FETCH
	LD	B,D
	LD	C,E
	JR	S_REF_D
NOREF:	LD	A,(FLAGS)
	CP	$C0
S_REF_E:JP	C,S_LBLE
S_REFBC:INC	HL
	LD	B,H
	LD	C,L
S_REF_D:RST	$30
	DEFW	L2D2B + 4	; STACK-BC
	JP	S_LBLE

STICK_TAB:
	DEFB	KEMPSTON - $
	DEFB	TIMEX1 - $
	DEFB	TIMEX2 - $
	DEFB	CURSOR - $
STICK_TAB_E:

KEMPSTON:
	RST	$30
	DEFW	L1E94		; FIND-INT1
	RRA
	JR	NC,KEMPSTON_FIRE
	IN	A,($1F)
	AND	$0F
	LD	B,A
	RRA
	SRL	A
	XOR	B
	AND	$03
	LD	C,A
	ADD	A,A
	ADD	A,A
	OR	C
	XOR	B
STICK_END:
	RST	$30
	DEFW	L2D28		; STACK-A
	JP	S_TIME_END

KEMPSTON_FIRE:
	IN	A,($1F)
	AND	$10
	JR	Z,STICK_END
FIRE:	LD	A,1
	JR	STICK_END

TIMEX1:	LD	B,1
	DEFB	$21		; LD HL, skip next two bytes
TIMEX2:	LD	B,2
TIMEXJ:	PUSH	BC
	RST	$30
	DEFW	L1E94		; FIND-INT1
	POP	BC
	LD	C,A
; Based on TC/TS2068 ROM address $2902
	LD	D,C
	LD	A,$0E
	OUT	($F5),A
	LD	C,$F6
	IN	A,(C)
	CPL
	BIT	0,D
	JR	Z,STICK_F
	AND	$0F
	CP	$0F
	JR	C,STICK_END
	AND	$00
	JR	STICK_END
STICK_F:RLCA
	AND	$01
	JR	STICK_END



CURSOR:	LD	DE,CURSOR_KEYS
	JR	STICK_KBD
STICK_STR:
	CALL	FETCH
	DEC	HL
	LD	(STKEND),HL
STICK_KBD:
	PUSH	DE
	PUSH	BC
	RST	$30
	DEFW	L1E94		; FIND-INT1
	RRA
	POP	BC
	POP	DE
	JR	NC,KEYBOARD_FIRE
	LD	BC,$0400
STICK_L:PUSH	BC
	LD	A,(DE)
	INC	DE
	RST	$30
	DEFW	KEYDOWN
	POP	BC
	CCF
	RL	C
	DJNZ	STICK_L
	LD	A,C
	JR	STICK_END

KEYBOARD_FIRE:
	EX	DE,HL
	ADD	HL,BC
	DEC	HL
	LD	A,(HL)
	RST	$30
	DEFW	KEYDOWN
	JR	NC,FIRE
	XOR	A
	JR	STICK_END

CURSOR_KEYS:
	DEFM	"85670"

PR_DIGIT:
	ADD	"0"
	CP	"9"+1
	JR	C,PRINT_A
	ADD	A,7
PRINT_A:RST	$30
	DEFW	L0010		; print digit
	RET

D_LBL:	LD	A,(HL)
	INC	HL
	OR	(HL)
	INC	HL
	OR	(HL)
	INC	HL
	OR	(HL)
	LD	(IY+MEMBOT+25-ERR_NR),LABEL_T
	CALL	Z,F_LBL
	DEC	HL
	DEC	HL
	DEC	HL
	LD	BC,L26B6 + 7	; S-SD-SKIP + 7
	PUSH	BC
SW_LBL:	RST	$10

F_LBL:	SET	7,(IY+FLAGS2-ERR_NR)	; Mark cache dirty
	LD	HL,(PROG)
F_LBLL:	LD	A,(HL)
	AND	$C0
	JR	NZ,ERROR_T	; T Label not found
	LD	DE,LABEL_T	; Label marker
	RST	$30
	DEFW	X1D91		; inside LOOK-PROG
	JR	C,ERROR_T	; T Label not found
	LD	(LIST_SP),BC
	INC	HL
	LD	BC,(MEMBOT+26)	; label start
NXBC:	LD	A,(BC)
	INC	BC
	CP	$0E
	JR	Z,E_LBL		; label end
	CP	" " + 1
	JR	C,NXBC
	CP	"a"
	JR	C,L_DIG
	AND	$DF		; upper case
L_DIG:	LD	E,A
NXHL:	LD	A,(HL)
	INC	HL
	CP	$0E
	CALL	Z,NXHL1
	CP	" " + 1
	JR	C,NXHL
	RST	$30
	DEFW	L2C88		; ALPHANUM
	JR	NC,NXLBL
	CP	"a"
	JR	C,L_DIG2
	AND	$DF		; upper case
L_DIG2:	CP	E
	JR	Z,NXBC
NXLBL:	LD	HL,(LIST_SP)
	INC	HL
	JR	F_LBLL

NXHL1:	LD	(MEMBOT+28),HL
	INC	HL
	INC	HL
	INC	HL
	INC	HL
	INC	HL
NXHLR:	LD	A,(HL)
	INC	HL
	RET

ERROR_T:CALL	ERROR
	DEFB	$1C		; T Label not found

E_LBL:	LD	A,(HL)
	INC	HL
	CP	$0D
	JR	Z,E_LBL2
	CP	$0E
	CALL	Z,NXHL1
	CP	" " + 1
	JR	C,E_LBL
	RST	$30
	DEFW	L2C88		; ALPHANUM
	JR	C,NXLBL
E_LBL2:	LD	L,C
	LD	H,B
	INC	HL
	INC	HL
	EX	DE,HL
	LD	HL,(PROG)
	LD	BC,(MEMBOT+28)
	AND	A
	SBC	HL,BC
	EX	DE,HL
	LD	(HL),E
	INC	HL
	LD	(HL),D
	RET

