	INCLUDE	"../labels.asm"
	INCLUDE	"sysvars128.asm"

	ORG	$0000
; Cold reset
RST00:	DI
	LD	BC, $692B
RST00L:	DEC	BC
	LD	A,B
	OR	C
	JR	NZ,RST00L	; No instruction fetch from 0008, for IF1 compatibility
	JP	RESET
	DEFS	$10 - $

; Return to ROM1
RST10:	INC	SP
	INC	SP
	JP	SWAP
	DEFS	$18 - $

; Collect a character
RST18:	LD	HL,(CH_ADD)
	LD	A,(HL)
TEST_CHAR:
	CALL	SKIP_OVER
	RET	NC

; Collect next character
RST20:	CALL	CH_ADD_1
	JR	TEST_CHAR
	DEFS	$28 - $

; Calculator restart
RST28:	JP	CALCULATE
	DEFS	$30 - $

; Call routine from ROM1
RST30:	EX	(SP),HL
	PUSH	AF
	LD	A,(HL)
	JP	CALL_ROM1
	DEFS	$38 - 2 - $

PAGEIRQ:OUT	(C),A
; IM1 routine
RST38:	PUSH	AF
	PUSH	BC
	LD	BC,IRQSWAP
	PUSH	BC
	LD	A,(BANK_M)
	XOR	$10
	LD	(BANK_M),A
	OR	$10		; force ROM1, whatever is in BANK_M
	LD	BC,$7FFD
	JR	PAGEIRQ

CALL_ROM1:
	LD	(TARGET),A
	INC	HL
	LD	A,(HL)
	LD	(TARGET+1),A	; target address in TARGET
	POP	AF
	INC	HL
	EX	(SP),HL		; return address on stack
	PUSH	HL
	LD	HL,SWAP
	EX	(SP),HL		; return address, SWAP on stack
	PUSH	HL
	LD	HL,(TARGET)
	EX	(SP),HL		; return address, SWAP, target address on stack
ROM1SW:	JP	SWAP		; this one is performance-critical

	DEFS	$66 - $
NMI:	PUSH	AF
	PUSH	HL
	LD	HL,(NMIADD)
	PUSH	HL
	LD	A,H
	CP	$3F
	RET	NC
	OR	L
	JR	NZ,ROM1SW
NONMI:	POP	HL
	POP	HL
	POP	AF
	RETN

; This positions the compatibility switch to a guaranteed RET in ROM1
	DEFS	X0094 - $0015 - $
SPECTRUM:
	LD	A,$0B
	LD	BC,$FC3B
	OUT	(C),A
	INC	B
	IN	A,(C)
	AND	$3F		; ZX UNO TURBO mode off
	OUT	(C),A
	LD	A,$30		; ROM 1, RAM 0, paging disabled
SPECTRUM_PAGE:
	LD	BC,$7FFD
	OUT	(C),A
SPECTRUM_END:	EQU	$

NEW128:	DI
	XOR	A
	LD	(BANK_M),A
	LD	HL,(RAMTOP)
	JP	STARTN

INIT_5B00:	EQU	$
; This stuff does not run here, it gets copied to $5B00
	ORG	$5B00
SWAP:	PUSH	AF
	PUSH	BC
IRQSWAP:LD	BC,$7FFD
	LD	A,(BANK_M)
	XOR	$10
; Reentrancy fixed, no need to miss interrupts
;;;	DI
	LD	(BANK_M),A
	OUT	(C),A
;;;	EI
	POP	BC
	POP	AF
	RET

ONERR:	LD	L,ONERRJ - $100*(ONERRJ/$100)
	JR	CH_JP
XIN:	LD	L,CH_XI - $100*(CH_XI/$100)
	JR	CH_JP
XOUT:	LD	L,CH_XO - $100*(CH_XO/$100)
	JR	CH_JP
NXIN:	LD	L,CH_NI - $100*(CH_NI/$100)
	JR	CH_JP
NXOUT:	LD	L,CH_NO - $100*(CH_NO/$100)
	JR	CH_JP
PIN:	LD	L,CH_PI - $100*(CH_PI/$100)
	JR	CH_JP
POUT:	LD	L,CH_PO - $100*(CH_PO/$100)
	JR	CH_JP
KIN:	LD	L,CH_KI - $100*(CH_KI/$100)
	JR	CH_JP
KOUT:	LD	L,CH_KO - $100*(CH_KO/$100)
CH_JP:	LD	H,CH_KO/$100
	PUSH	HL
	JR	SWAP

; Renumber
RCLINE:	DEFS	2		; current line being renumbered
RCSTART:DEFW	10		; starting line number for renumbering
RCSTEP:	DEFW	10		; step for renumbering
; PLAY tempo
TEMPO:	DEFB	120		; in BPM for PLAY
PLAY_ST:DEFS	2		; PLAY state
; K/S states
K_STATE:DEFB	$00
K_WIDTH:DEFB	$21
K_TV:	DEFW	0
S_STATE:DEFB	$00
S_WIDTH:DEFB	$21
S_TV:	DEFW	0
; Clipping
NORTH:	DEFB	$00
SOUTH:	DEFB	$15
WEST:	DEFB	$00
EAST:	DEFB	$1F
COORDS2:DEFW	$57A0
KS_PERM:DEFB	0		; additional permanent attributes
CHARS4:	DEFW	CHARSET - $0080
C_PCC:	DEFB	1		; colon counter
BANK_F:	DEFB	$06
	DEFS	$5b58 - $	; minimal Investrónica compatibility
TARGET:	DEFW	0
RETADDR:DEFW	0		; TODO: abused by PoC code
BANK_M:	DEFB	0
; Variables
ERRPTR:	DEFW	0		; PROG offset of active ON ERROR
; RS233
BAUD:	DEFW	$0012		; 9600 BAUD
SERFL:	DEFW	0		; Second byte received flag and value
; Origin
ORIGX:	DEFB	$00,$00,$00,$00,$00	; 0.0
ORIGY:	DEFB	$00,$00,$AF,$00,$00	; 175.0
SCALEX:	DEFB	$00,$00,$01,$00,$00	; 1.0
SCALEY:	DEFB	$00,$FF,$FF,$FF,$00	; -1.0
COORDX:	EQU	$
COORDY:	EQU	COORDX+5
STEPPPC:EQU	COORDY+5
STEPSUB:EQU	STEPPPC+2
K_ATTR:	EQU	STEPSUB+1	; ATTR_T and MASK_T at cursor position
K_PFLAG:EQU	K_ATTR+2	; P_FLAG at cursor position
K_SAV:	EQU	K_PFLAG+1	; FLAGS and FLAGS2 at cursor position
K_SAV2:	EQU	K_SAV+2		; K_STATE at cursor position
K_CUR_S:EQU	K_SAV2+1	; old K_CUR value
S_MODE:	EQU	K_CUR_S+2	; video mode: b7..b3 - resolution, b2..b0 - palette

INIT_5B00_L:	EQU	$ - $5B00

	ORG	INIT_5B00 + INIT_5B00_L

F_SCAN:	LD	HL,10
	ADD	HL,SP
	LD	A,(HL)
	INC	HL
	EX	AF,AF'
	LD	A,(HL)
	LD	E,L
	LD	D,H
	DEC	HL
	DEC	HL
	LD	BC,10
	LDDR
	POP	HL
	LD	H,A
	EX	AF,AF'
	LD	L,A
JP_HL:	JP	(HL)

SKIP_OVER:
	CP	$21
	RET	NC
	CP	$0D
	RET	Z
	CP	1
	RET	C
	CP	6
	CCF
	RET	NC
	CP	$18
	CCF
	RET	C
	INC	HL
	CP	$16
	JR	C,SKIPS2
	INC	HL
SKIPS2:	SCF
	LD	(CH_ADD),HL
	RET

INDEXER_1:
	INC	HL
INDEXER:LD	A,(HL)
	AND	A
	RET	Z
	CP	C
	INC	HL
	JR	NZ,INDEXER_1
	SCF
	RET

	INCLUDE "channels.asm"
	INCLUDE	"xchannel.asm"
	INCLUDE "tokenizer.asm"
	INCLUDE "editorhead.asm"

PR_OUT:
PR_IN:
	RST	$10	

RESET:	LD	A,8		; check and clear all banks
	LD	HL,$FFFF
	LD	BC,$7FFD
	LD	DE,R_LINK+7
TESTL1:	DEC	A
	OUT	(C),A
	LD	(HL),A
	JR	NZ,TESTL1
	LD	A,8
TESTL2:	DEC	A
	LD	BC,$7FFD
	OUT	(C),A
	OUT	($FE),A
	CP	(HL)
	JR	Z,RAMOK
	HALT			; freeze with border showing the faulty RAM
RAMOK:	LD	SP,L3D00	; Two zero bytes for RET in ROM1
	INC	C
	IN	C,(C)
	BIT	0,C		; SPACE key pressed?
	JP	Z,SPECTRUM	; if so, enter compatibility mode
	EX	DE,HL
	LDD
	EX	DE,HL
	LD	BC,$4000
	INC	L
TESTL3:	DEC	L
	LD	(HL),C
	JR	NZ,TESTL3
	DEC	H
	DJNZ	TESTL3
	LD	HL,$FFFF
	OR	A
	JR	NZ,TESTL2

	LD	A,7
	OUT	($FE),A
	LD	SP,TSTACK
	EXX
	LD	HL,INIT_5B00
	LD	DE,$5B00
	LD	BC,INIT_5B00_L
	LDIR
	EXX
	LD	(P_RAMT),HL
	LD	DE,$3C07 + "Z" * 8
	LD	BC,26 * 8
	PUSH	BC
	EX	DE,HL
	RST	$30
	DEFW	LDDRR
	POP	BC
	LD	DE,(26 - 112) * 8 - 1
	LD	HL,$3C07 + "z" * 8
	RST	$30
	DEFW	LDDRR
	EX	DE,HL
	INC	HL
	PUSH	HL
UDGINV:	LD	A,(HL)
	CPL
	LD	(HL),A
	INC	L
	JR	NZ,UDGINV
	INC	H
	JR	NZ,UDGINV
	POP	HL
	LD	(UDG),HL
	DEC	HL
	LD	BC,$0140
	LD	(RASP),BC
	LD	(RAMTOP),HL
STARTN:	LD	A,$10
	LD	(FLAGS),A	; indicate 128k mode
	ADD	A,A
	LD	(FLAGS2),A	; set-letter-by-letter mode
	LD	(HL),$3E
	DEC	HL
	LD	(HL),$00	; mark end of stack
	LD	SP,HL
	DEC	HL
	DEC	HL
	LD	(ERR_SP),HL
	LD	HL,$3C00
	LD	(CHARS),HL
	LD	HL,NMIVEC
	LD	(NMIADD),HL
	LD	IY,ERR_NR
R_KEY:	XOR	A
	IN	A,($FE)
	OR	$E0
	INC	A
	JR	NZ,R_KEY
	CALL	PAL_0		; ULAplus to ZX Spectrum mode
	LD	A,$3F
	LD	I,A
	IM	1
	EI
	LD	HL,CHINFO
	LD	(CHANS),HL
	LD	DE,CHINFO0
	LD	BC,CHINFO0_E - CHINFO0
	EX	DE,HL
	LDIR
	EX	DE,HL
	DEC	HL
	LD	(DATADD),HL
	INC	HL
	LD	(PROG),HL
	LD	(VARS),HL
	LD	(HL),$80
	INC	HL
	LD	(E_LINE),HL
	LD	(HL),$0D
	INC	HL
	LD	(HL),$80
	INC	HL
	LD	(WORKSP),HL
	LD	(STKBOT),HL
	LD	(STKEND),HL
	LD	A,$38
	LD	(ATTR_P),A
	LD	(ATTR_T),A
	LD	(BORDCR),A
	LD	HL,$0223
	LD	(REPDEL),HL
	DEC	(IY-$3A)
	DEC	(IY-$36)
	LD	HL,INIT_STRM
	LD	DE,STRMS
	LD	BC,$0E
	LDIR
	LD	(IY+$31),$02
	RST	$30
	DEFW	L0D6B		; CLS
	LD	DE,COPYRIGHT
	CALL	STDERR_MSG
	XOR	A
	LD	DE,L1539 - 1	; copyright message
	RST	$30
	DEFW	L0C0A		; PO-MSG
	SET	5,(IY+$02)
MAIN1:	LD	DE,L12A9
	PUSH	DE
	RST	$10

CH_ADD_1:
	LD	HL,(CH_ADD)
TEMP_PTR1:
	INC	HL
TEMP_PTR2:
	LD	(CH_ADD),HL
	LD	A,(HL)
	RET

MULS_S:	LD	BC,$104C	; tight multiplication
	LD	HL,L2790	; S-NEXT
MULS_R:	EX	(SP),HL		; replace return address by it
	RST	$10

GOTO_CONT:
	POP	DE		; discard ERROR B
	CALL	STACKSWAP
JP_LBL:	LD	HL,(PROG)
	AND	A
	SBC	HL,BC		; subtract large target from PROG
	LD	E,(HL)
	INC	HL
	LD	D,(HL)		; DE = length
	ADD	HL,DE
	LD	(CH_ADD),HL
	SBC	HL,DE
	INC	HL
	LD	DE,SUBPPC
	LDI			; statement number
	LD	E,(HL)
	INC	HL
	LD	D,(HL)		; relative pointer to before first statement
	ADD	HL,DE
	DEC	HL
	LD	D,(HL)
	DEC	HL
	LD	E,(HL)		; DE = length of line
	EX	DE,HL
	ADD	HL,DE
	INC	HL
	INC	HL
	LD	(NXTLIN),HL
	EX	DE,HL
	DEC	HL
	LD	E,(HL)
	DEC	HL
	LD	D,(HL)
	LD	(PPC),DE
	RET

; Single-argument original function extended to multiple arguments
MULTI_CONT:
	POP	BC		; discard return address
	BIT	6,(IY+FLAGS-ERR_NR)	; type of first argument
	JR	Z,ERRCIDX	; error, if string
	POP	BC
	LD	A,B
	CP	$10
	JR	NZ,ERRCIDX
	PUSH	BC
	LD	HL,MFNTAB
INDEXJP:CALL	INDEXER
	JP	C,INDEXER_JP
ERRCIDX:JP	ERROR_C

SCAN_CONT:
	CP	$40
	JR	Z,DSWAP2	; mismatched function type
	CP	C
	JR	NZ,PREFIX_CONT
	CP	","
	JR	Z,MULTI_CONT
INFIX_CONT:
	CP	$0C		; multiplication?
	JR	NZ,DSWAP2
	CALL	SYNTAX_Z
	JR	Z,MULS_S
	POP	BC		; discard return address
	LD	BC,D_STRING
	JP	S_FUNC

ERR_CONT:
	CP	$07
	JR	Z,ERR7MSG
	CP	$1C
	JR	C,DSWAP2
	CALL	REPORT
ERR_C:	LD	HL,X1349
	EX	(SP),HL
DSWAP2:	RST	$10

ERR7MSG:LD	DE,ERR7TXT
	CALL	MESSAGE
	JR	ERR_C

DIGIT_CONT:
	CALL	DDIGIT
	JR	NC,DSWAP2
	LD	A,C
	RST	$10

DDIGIT:	CP	$A
	CCF
	RET	NC
	SUB	"A" - "0"
	RET	C
	AND	$DF
	CP	26
	CCF
	RET	C
	ADD	$0A
	RET

CL9_CONT:
	RST	$30
	DEFW	L2070 + 4	; STR-ALTER + 4
	RST	$30
	DEFW	L21E2 + 4	; CO-TEMP-2 + 4
	RST	$10

SPARSE:	LD	HL,INST_T1
	CALL	INDEXER
	LD	A,$FF
	JR	C,INST_L
	JP	ERROR_C

PREFIX_CONT:
	RST	$18
	CP	ONERR_T
	JR	NC,DSWAP2
	LD	C,A
	SUB	SQ_T
	LD	HL,SCANFUNC2
	JR	C,IDX_DO
	LD	HL,FUNCTAB
	ADD	A,A
	LD	C,A
	LD	B,0
	ADD	HL,BC
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	POP	BC		; discard return address
	JP	(HL)

OLD_CONT:
	LD	A,(T_ADDR)
	CP	$B2		; POKE ?
	JP	Z,E_POKE
	JR	ERRCNZ3

RUN_CONT:
	POP	HL		; discard return to REPORT C
	INC	B		; B=$00 for instruction mismatch and B=$1B for separator mismatch
	JR	Z,CL9_CONT
	DJNZ	SEP_MISM
	JR	NC,OLD_CONT	; old command extended
 	ADD	A,STEP_T-TURBO_T; shift codes up
	LD	C,A
	JR	C,SPARSE
	DEC	B		; B becomes FF here
	LD	HL,P_END
	ADD	HL,BC
	INC	B		; B becomes 0 again
INST_L:	LD	C,(HL)
	ADD	HL,BC
	CP	INSTRUCTION_T + $53	; TODO: magic number
	JR	NC,GET_PARAM	; jump for tokens
	CALL	SYNTAX_Z
	JR	NZ,ERRCNZ
	CP	"@" + $53	; TODO: magic number
ERRCNZ3:JR	NZ,ERRCNZ
	RST	$30
	DEFW	L19FB		; E-LINE-NO
	RST	$18
	DEC	HL
	LD	A,(SUBPPC)
	LD	E,0
	LD	D,A
	RST	$30
	DEFW	L198B		; EACH-STMT
	RST	$20
	LD	(HL),LABEL_T
	RST	$20
	LD	HL,P_LABEL
	JR	GET_PARAM

SCAN_LOOP:
	LD	HL,(T_ADDR)
GET_PARAM:
	LD	A,(HL)
	INC	HL
	LD	(T_ADDR),HL
	LD	BC,SCAN_LOOP
	PUSH	BC
	LD	C,A
	CP	$20
	JR	NC,SEPARATOR
	LD	HL,CMDCLASS2
	LD	B,$00
	ADD	HL,BC
	LD	C,(HL)
	ADD	HL,BC
	PUSH	HL
	RST	$18
	DEC	B
	RET

INDEX_CONT:
	LD	A,L
	LD	HL,OPERTB
	SUB	$AF
	JR	Z,IDX_DO
	LD	HL,CLOSESTRM2
	DEC	A
	JR	Z,IDX_DO
	LD	HL,OPENSTRM2
IDX_DO:	CALL	INDEXER
SWIDX:	JR	NC,SWERR
	POP	BC		; discard return address
	LD	C,(HL)
	LD	B,0
	ADD	HL,BC
	JP	(HL)

SEP_MISM:			; THEN-less IF and operator update in LET
	CP	$0D
	JR	Z,C_THEN
	CP	":"
	JR	Z,C_THEN
	LD	HL,L1B76	; STMT-RET
OLD_EXT:EX	AF,AF'
	LD	A,(T_ADDR)
	CP	$B2		; EOL in POKE
	JP	Z,N_POKE
	CP	$8E		; EOL in RETURN
	JP	Z,N_RETURN
	CP	$CA		; illegal element in READ
	JP	Z,READ_EXT
	CP	$8B		; EOL in STOP
	JP	Z,STOP
	CP	$7C		; = in LET
ERRCNZ:	JR	NZ,ERROR_C_NZ
	EX	AF,AF'
	JP	UPDATE

SEPARATOR:
	RST	$18
	CP	C
ERROR_C_NZ:
	JP	NZ,ERROR_C
	RST	$20
	RET

C_THEN:	LD	A,THEN_T	; THEN
	CP	C
	JR	NZ,ERROLD
	CALL	SYNTAX_Z	; checking sytax?
	JP	NZ,THENLESS	; if not, execute THENless IF
	RES	4,(IY+$37)	; signal that we're NOT after THEN
	LD	HL,L1B29	; STMT-L-1
ERROLD:	EX	(SP),HL
SWERR:	RST	$10		; we're done here

ERROR:	LD	HL,(CH_ADD)
	LD	(X_PTR),HL
	LD	HL,L0055
	EX	(SP),HL
	LD	L,(HL)
	RST	$10


; Output token
; In: A=token code
TOKEN_O:SUB	$7F
	EX	DE,HL
	BIT	3,(HL)
TOKEN_L:CALL	Z,PR_SPACE
	LD	DE,T_ELSE
	JR	NZ,TOKEN_N	; jump forward in argument mode
	CP	ELSE_T - $7F
	JR	Z,TOKEN_E
	SET	2,(IY+FLAGS-ERR_NR)
TOKEN_N:LD	B,A
	SUB	INSTRUCTION_T - $7F
	JR	C,TOKEN		; jump forward for operators
	SUB	RND_T - INSTRUCTION_T
	JR	NC, TOKEN1	; token in ROM1
	CALL	TOKEN
	RRCA
	RST	$30
	DEFW	L2C8D		; ALPHA -- ALPHANUM would be more correct, but slow and irrelevant
	RET	NC
PR_SPACE:
	BIT	0,(IY+$01)	; space suppressed?
	JR	NZ,ZRET		; return with ZF, if so
	PUSH	HL
	PUSH	AF
	LD	A," "
	RST	$30
	DEFW	L0C3B		; PO_SAVE
	EX	DE,HL
	POP	AF
	POP	HL
ZRET:	CP	A		; set ZF
	RET

TOKEN1:	RST	$30
	DEFW	L0C10
	RET

TOKEN_E:RES	2,(IY+FLAGS-ERR_NR)	; K mode next
	JR	TOKEN_S

	INCLUDE	"reportz.asm"
	INCLUDE	"reports.asm"
	INCLUDE	"functions.asm"

; Mirror a memory area
; Input: HL=start, BC=length
MIRROR:	LD	D,(HL)
	DEC	BC
	LD	A,B
	OR	C
	RET	Z
	ADD	HL,BC
	LD	E,(HL)
	LD	(HL),D
	SBC	HL,BC
	LD	(HL),E
	INC	HL
	DEC	BC
	LD	A,B
	OR	C
	JR	NZ,MIRROR
	RET

; Replace a,b on top of stack by INT(a/b) and return a MOD b in registers BC and A.
MOD2A:	RST	$28		; calc
	DEFB	$32		; mod
	DEFB	$01		; exchange
	DEFB	$38		; end
	RST	$30
	DEFW	L2DA2		; FP-TO-BC (and A)
	RET

; Move both pointers back by one entry
STEPBACK:
	LD	BC,-5
	LD	D,H
	LD	E,L
	ADD	HL,BC
	RET

; print the decimal value of a byte in register A
DECBYTE:LD	C,A
	LD	B,0
	RST	$30
	DEFW	L1A1B
	RET
; print the decimal value of a word in register HL
DECWORD:LD	C,L
	LD	B,H
	RST	$30
	DEFW	L2D2B + 4	; STACK-BC
	RST	$30
	DEFW	L2DE3		; PRINT-FP
	RET

OPERTB:	DEFB	"|"
	DEFB	S_BOR - $
	DEFB	"&"
	DEFB	S_BAND - $
	DEFB	XOR_T
	DEFB	S_XOR - $
	DEFB	"%"
	DEFB	S_MOD - $
	DEFB	"!"
	DEFB	S_CPL - $
	DEFB	"?"
	DEFB	S_ELVIS - $
	DEFB	RR_T
	DEFB	S_RR - $
	DEFB	RL_T
	DEFB	S_RL - $
	DEFB	"$"
	DEFB	S_LSTR - $
	DEFB	0

S_BOR:	LD	A,$02		; priority like OR
	CALL	TIGHTER
	JR	NZ,S_BORN
	LD	BC,D_BORS
	JR	S_OPERS
S_BORN:	LD	BC,D_BOR
S_OPER2:JP	S_OPERN

S_OPERS:CALL	SYNTAX_Z
	JR	Z,S_OPSS
	PUSH	BC
	LD	BC,FSCAN
	PUSH	AF
	RST	$30
	DEFW	L2D2B + 4	; STACK-BC
	POP	AF
S_OPSS:	LD	B,A		; priority
	LD	C,$6D		; USR$
	PUSH	BC
	LD	BC,L270D	; S-PUSH-PO
	PUSH	BC
	LD	B,A
	LD	C,$81		; EXCHANGE number,string
	RST	$10

S_XOR:	LD	A,$02		; priority like OR
	CALL	TIGHTER
	JR	NZ,S_XORN
	LD	BC,D_XORS
	JR	S_OPERS
S_XORN:	LD	BC,D_XOR
	JR	S_OPER2

S_BAND:	LD	A,$03		; priority like AND
	CALL	TIGHTER
	JR	NZ,S_BANDN
	LD	BC,D_BANDS
	JR	S_OPERS
S_BANDN:LD	BC,D_BAND
	JR	S_OPER2

S_MOD:	LD	A,$08		; priority like /
	CALL	TIGHTER
	JP	Z,ERROR_C	; only numeric lefthand
	LD	BC,$08C2	; delete with priority 8
	PUSH	BC
	LD	C,$F2		; mod with same priority
SWNEXT:	LD	HL,L2790	; S-NEXT
SWPUSH:	PUSH	HL
SWAPOP:	RST	$10

S_RL:	CALL	BWISE
	LD	BC,D_RLS
	JR	Z,S_OPERS
	LD	BC,D_RLN
	JR	S_OPER2

S_RR:	CALL	BWISE
	LD	BC,D_RRS
	JR	Z,S_OPERS
	LD	BC,D_RRN
	JR	S_OPER2

S_CPL:	CALL	SYNTAX_Z
	JR	Z,F_CPL
	BIT	6,(IY+$01)
	JR	NZ,S_CPLN
	RST	$30
	DEFW	L2BF1		; STK-FETCH
	PUSH	DE
	RST	$30
	DEFW	L0030		; BC-SPACES
	RST	$30
	DEFW	L2AB2		; STK-STO
	POP	HL
S_CPLL:	LD	A,B
	OR	C
	JR	Z,F_CPL
	LD	A,(HL)
	CPL
	LD	(DE),A
	INC	HL
	INC	DE
	DEC	BC
	JR	S_CPLL

S_CPLN:	RST	$28		; calculate
	DEFB	$A1		; stk-one
	DEFB	$0F		; addition
	DEFB	$1B		; negate
	DEFB	$38		; end
F_CPL:	RST	$20		; advance
F_CPL2:	LD	HL,L2723	; S-OPERTR
	JR	SWPUSH

S_LSTR:	CALL	SYNTAX_Z
	JP	NZ,ERROR_2	; Variable not found in runtime
	LD	HL,FLAGS
	BIT	6,(HL)
	JR	Z,SWAPOP	; must be numeric
	RES	6,(HL)		; indicate string
	LD	A,D
	OR	A
	JR	Z,SWAPOP	; numeric expression
	LD	HL,(STKBOT)
	SBC	HL,DE
	JR	C,SWAPOP	; numeric literal
	JR	F_CPL		; skip "$"

BWISE:	CALL	SYNTAX_Z
	JR	NZ,BWISE2
	POP	BC
	LD	BC,$10C8	; like AND, priority $10
	LD	HL,L2734	; S-LOOP
	JR	SWPUSH

BWISE2:	BIT	6,(IY+$01)
	RET

S_ELVIS:RST	$20
	LD	A,(FLAGS)	; selector type in bit 6
	ADD	A,A
	JR	NC,S_ELVS
	ADD	A,A
	JR	NC,D_ELVS
	RST	$30
	DEFW	L1E99		; FIND-INT2
	RST	$20
D_ELVSK:LD	A,B
	OR	C
	JR	Z,D_ELVZ
	PUSH	BC
	CALL	SKIPEX
	POP	BC
	JR	C,D_ELVZ
	LD	(CH_ADD),HL
	DEC	BC
	JR	D_ELVSK

D_ELVS:	LD	HL,(STKEND)
	DEC	HL
	LD	A,(HL)
	DEC	HL
	OR	(HL)
	JR	Z,D_ELVZ0
	RST	$20
	CALL	SKIPEX
	JR	C,D_ELVR
	DEC	HL
	LD	(CH_ADD),HL

D_ELVZ0:LD	HL,(STKEND)
	LD	DE,-5
	ADD	HL,DE
	LD	(STKEND),HL
	RST	$20
D_ELVZ:	RST	$30
	DEFW	L24FB		; SCANNING
	CP	")"
	JR	Z,F_CPL

D_ELVT:	CALL	SKIPEX
	JR	NC,D_ELVT
D_ELVR:	OR	A
	JR	NZ,ERROR_C_ELV
	LD	(CH_ADD),HL
	RST	$18
	JP	F_CPL2

TIGHT_S:EX	AF,AF'
	LD	HL,FLAGS
	LD	A,E
	XOR	(HL)		; FLAGS, bit 6
	AND	$40
	JR	NZ,ERROR_C_ELV
	LD	A,E
	RRCA
	XOR	(HL)
	AND	$40
	XOR	(HL)
	LD	(HL),A		; set bit 6 according to bit 7 of E
	EX	AF,AF'
TIGHTER:POP	HL		; return address
	POP	DE		; operator
	PUSH	DE
	PUSH	HL
	CP	D
	BIT	6,(IY+$01)	; FLAGS, check string/numeric
	RET	NC
	POP	HL
	POP	DE
	PUSH	HL		; eliminate DE
	CALL	SYNTAX_Z
	JR	Z,TIGHT_S
	PUSH	DE
	PUSH	BC
	PUSH	AF
	LD	A,E
	AND	$3F
	LD	B,A
	RST	$28
	DEFB	$3B		; fp_calc_2
	DEFB	$38		; end
	POP	AF
	POP	BC
	POP	DE
	JR	TIGHT_S

S_ELVS:	PUSH	AF
	RST	$18
	CP	"("
	JR	NZ,ERROR_C_ELV
	RST	$20
	RST	$30
	DEFW	L24FB + 1	; SCANNING + 1
	CP	")"
	JR	NZ,S_ELVL
S_ELVE:	LD	A,(FLAGS)
	ADD	A,A
	POP	BC
	XOR	B
	ADD	A,A
F_CPLNC:JP	NC,F_CPL
ERROR_C_ELV:
	JP	ERROR_C
S_ELVL:	CP	","
	JR	NZ,ERROR_C_ELV
	POP	AF
	ADD	A,A
	LD	A,(FLAGS)
	JR	C,S_ELVN
	PUSH	AF
	RST	$20
	RST	$30
	DEFW	L24FB + 1	; SCANNING + 1
	CP	")"
	JR	NZ,ERROR_C_ELV
	POP	BC
	LD	A,(FLAGS)
	XOR	B
	ADD	A
	ADD	A
	JR	C,ERROR_C_ELV
	JR	F_CPLNC
S_ELVN:	ADD	A,A
	PUSH	AF
S_ELVNL:RST	$20
	RST	$30
	DEFW	L24FB + 1	; SCANNING + 1
	CP	")"
	JR	Z,S_ELVE
	CP	","
	JR	NZ,ERROR_C_ELV
	LD	A,(FLAGS)
	ADD	A,A
	POP	BC
	PUSH	AF
	XOR	B
	ADD	A,A
	JR	C,ERROR_C_ELV
	JR	S_ELVNL

; skip expression pointed by HL. CF cleared, it ends with comma
SKIPEX:	LD	DE,5
	LD	BC,0
SKIPEXL:LD	A,(HL)
	INC	HL
	CP	$0E
	JR	Z,SKIPNN
	CP	"("
	JR	Z,SKIPBR
	CP	"\""
	JR	Z,SKIPQT
	CP	")"
	JR	Z,SKIPCB
	CP	":"
	JR	Z,SKIPEE
	CP	$0D
	JR	Z,SKIPEE
	CP	THEN_T
	JR	Z,SKIPEE
	CP	","
	JR	NZ,SKIPEXL
	LD	A,B
	OR	C
	JR	NZ,SKIPEXL
	RET
SKIPNN:	ADD	HL,DE
	JR	SKIPEXL
SKIPBR:	INC	BC
	JR	SKIPEXL
SKIPQT:	CP	(HL)
	INC	HL
	JR	NZ,SKIPQT
	JR	SKIPEXL
SKIPCB:	LD	A,B
	OR	C
SKIPEE:	SCF
	RET	Z
	DEC	BC
	JR	SKIPEXL

D_BITW1:RST	$28
	DEFW	$38		; end
	CALL	STEPBACK
	LD	B,3
	LD	A,(DE)
	OR	(HL)
	RET	Z
	RST	$30
	DEFW	L3293		; RE-ST-TWO
	EXX
	EX	(SP),HL
	PUSH	HL
	EXX
	EX	DE,HL
	LD	C,L
	LD	B,H
	EX	(SP),HL
	PUSH	DE
	PUSH	HL
	LD	L,C
	LD	H,B
	EX	DE,HL
	RST	$30
	DEFW	L2F9B		; PREP-ADD
	LD	B,A
	EX	DE,HL
	RST	$30
	DEFW	L2F9B		; PREP-ADD
; The two numbers are A,(HL) and B,(DE)
	CP	B
	CCF
	RET	C
	LD	C,A
	LD	A,B
	LD	B,C
	EX	DE,HL
; The two numbers are A,(HL) and B,(DE) where A >= B
	SCF
	RET

D_FCPL:	PUSH	HL
	LD	B,5
D_FCPLL:LD	A,(HL)
	CPL
	LD	(HL),A
	INC	HL
	DJNZ	D_FCPLL
	POP	HL
	RET

UPDTABN:DEFB	"|"
	DEFB	D_BOR - $
	DEFB	"&"
	DEFB	D_BAND - $
	DEFB	XOR_T
	DEFB	D_XOR - $
	DEFB	RR_T
	DEFB	D_RRN - $
	DEFB	RL_T
	DEFB	D_RLN - $
	DEFB	0

D_BAND:	POP	BC
	CALL	D_BITW1
	JR	C,D_FAND
D_ANDL:	INC	HL
	INC	DE
	LD	A,(DE)
	AND	(HL)
	LD	(HL),A
	DJNZ	D_ANDL
	JR	D_BITW2

D_BOR:	POP	BC
	CALL	D_BITW1
	JR	C,D_FOR
D_ORL:	INC	HL
	INC	DE
	LD	A,(DE)
	OR	(HL)
	LD	(HL),A
	DJNZ	D_ORL
D_BITW2:DEC	DE
	DEC	DE
	DEC	DE
D_FSHW:	RST	$10

D_FOR:	EX	DE,HL
	BIT	7,(HL)
	EX	DE,HL
	JR	Z,D_FSH1
	LD	C,A
	LD	A,B
	LD	B,C
	EX	DE,HL
; The two numbers are A,(HL) and B,(DE) where A,(HL) is fixed
D_FSH1:	PUSH	AF
	SUB	B
	RST	$30
	DEFW	L2FBA		; FETCH-TWO
	JR	C,D_FSH2
	RST	$30
	DEFW	L2FDD		; SHIFT-FP
D_FSH4:	POP	AF
	POP	HL
	LD	(HL),A
	PUSH	HL
	LD	A,E
	OR	B
	LD	L,A
	LD	A,D
	OR	C
	LD	H,A
	EXX
	LD	A,H
	OR	L
	LD	L,A
	LD	A,E
	OR	C
	LD	E,A
	LD	A,D
	OR	B
D_FCONT:LD	D,A
	LD	H,$00
	LD	BC,X3069
	PUSH	BC
	RST	$10

D_FSH2:	NEG
	CP	$21
	JR	NC,D_FSH3
	PUSH	BC
	LD	B,A
D_FSHL:	SLA	E
	RL	D
	EXX
	RL	E
	RL	D
	EXX
	DJNZ	D_FSHL
	POP	BC
	JR	D_FSH4

D_FSH3:	RST	$30
	DEFW	L2FF9		; ADDEND-0
	JR	D_FSH4

D_XOR:	POP	BC
	CALL	D_BITW1
	JR	C,D_FXOR
D_XORI:	INC	HL
	INC	DE
	LD	A,(DE)
	XOR	(HL)
	LD	(HL),A
	DJNZ	D_XORI
	JR	D_BITW2

D_FAND:	PUSH	AF
	SUB	B
	RST	$30
	DEFW	L2FBA		; FETCH-TWO
	RST	$30
	DEFW	L2FDD		; SHIFT-FP
	POP	AF
	POP	HL
	LD	(HL),A
	PUSH	HL
	LD	A,E
	AND	B
	LD	L,A
	LD	A,D
	AND	C
	LD	H,A
	EXX
	LD	A,H
	AND	L
	LD	L,A
	LD	A,E
	AND	C
	LD	E,A
	LD	A,D
	AND	B
	JR	D_FCONT

D_FXOR:	PUSH	AF
	SUB	B
	RST	$30
	DEFW	L2FBA		; FETCH-TWO
	RST	$30
	DEFW	L2FDD		; SHIFT-FP
	POP	AF
	POP	HL
	LD	(HL),A
	PUSH	HL
	LD	A,E
	XOR	B
	LD	L,A
	LD	A,D
	XOR	C
	LD	H,A
	EXX
	LD	A,H
	XOR	L
	LD	L,A
	LD	A,E
	XOR	C
	LD	E,A
	LD	A,D
	XOR	B
	JR	D_FCONT

D_RRN:	RST	$28
	DEFB	$1B		; negate
	DEFB	$38

D_RLN:	LD	HL,X2D37
	EX	(SP),HL		; adjust return address
	CALL	STACKSWAP
	RST	$30
	DEFW	L2314		; STK-TO-A
D_SHL:	OR	A
	RET	Z		; shift by zero
	LD	B,A
	LD	A,(HL)
	DEC	C
	JR	Z,D_RLNP
D_RRNP:	OR	A
	JR	NZ,D_RRF	; shift FP number
	CALL	D_SH0
D_RRNL:	INC	HL
	SRA	(HL)
	DEC	HL
	RR	(HL)
	JR	C,D_RRNU	; integer underflow
	DJNZ	D_RRNL
	RET

D_RRNU:	RST	$28
	DEFB	$A2		; stk-half
	DEFB	$0F		; addition
	DEFB	$38		; end
	LD	A,(BREG)
	DEC	A
	RET	Z
	LD	B,A
	LD	A,(HL)
D_RRF:	SUB	A,B
	JR	C,D_RRFU	; FP underflow
	LD	(HL),A
	RET	NZ
D_RRFU:	LD	(HL),$00
	LD	E,L
	LD	D,H
	INC	DE
	LD	BC,4
	LDIR
	RET

D_RLNP:	OR	A
	JR	NZ,D_RLF	; shift FP number
	CALL	D_SH0
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
D_RLNL:	RL	E
	RL	D
	SBC	A,A
	XOR	C
	ADD	A,A
	JR	C,D_RLNO	; integer overflow
	DJNZ	D_RLNL
	LD	(HL),D
	DEC	HL
	LD	(HL),E
	RET

ERROR_6:RST	$30
	DEFW	L31AD		; 6 Number too big

D_RLNO:	DEC	HL
	DEC	HL
	DEC	HL
	LD	A,B
	ADD	$90
JCERR6:	JR	C,ERROR_6	; FP overflow
	LD	(HL),A
	INC	HL
	LD	A,(HL)
	LD	C,A
	XOR	E
	LD	E,A
	LD	A,C
	XOR	D
	RL	C
	RRA
	RR	E
	LD	(HL),A
	INC	HL
	LD	(HL),E
	INC	HL
	LD	(HL),0
	INC	HL
	LD	(HL),0
	RET

D_RLF:	ADD	A,B
	JR	C,JCERR6
	LD	(HL),A
	RET

D_SH0:	INC	HL
	LD	C,(HL)
	INC	HL
	LD	A,(HL)
	INC	HL
	OR	(HL)
	DEC	HL
	RET NZ
	POP	HL		; discard return address
	RET			; don't shift zero

D_TWOS:	POP	BC		; return here
	POP	HL		; discard return address
	POP	HL		; RE-ENTRY
	POP	DE		; discard BREG?
	POP	DE		; discard USR
	LD	DE,$106F	; CHR$, a num-to-string function
	PUSH	DE		; replace by CHR$
	LD	DE,0
	PUSH	DE		; BREG = 0
	PUSH	HL
	PUSH	BC
	RST	$30
	DEFW	L2BF1		; STK-FETCH
	PUSH	DE
	PUSH	BC
	RST	$30
	DEFW	L2BF1		; STK-FETCH
	POP	HL
	PUSH	HL
	AND	A
	SBC	HL,BC
	JR	NC,SSWP
	PUSH	DE
	JR	SNOSWP
SSWP:	POP	HL
	POP	AF
	PUSH	DE
	PUSH	BC
	PUSH	AF
	LD	C,L
	LD	B,H
SNOSWP:	RST	$30
	DEFW	L0030		; BC-SPACES
	RST	$30
	DEFW	L2AB2		; STK-STO
	POP	HL
	LD	A,B
	OR	C
	PUSH	DE
	PUSH	BC
	JR	Z,SEMPTY
	LDIR
SEMPTY:	POP	AF
	POP	DE
	POP	BC
	POP	HL
	RET

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

UPDTABS:DEFB	"|"
	DEFB	D_BORS - $
	DEFB	"&"
	DEFB	D_BANDS - $
	DEFB	XOR_T
	DEFB	D_XORS - $
	DEFB	0

D_BANDS:CALL	D_TWOS
	PUSH	AF
	PUSH	BC
D_BANDL:LD	A,B
	OR	C
	JR	Z,D_FILLS
	LD	A,(DE)
	AND	(HL)
	LD	(DE),A
	INC	HL
	INC	DE
	DEC	BC
	JR	D_BANDL
D_FILLS:POP	BC
	POP	HL
	SBC	HL,BC
	JR	Z,SMUL_E
	EX	DE,HL
D_FILLL:LD	(HL),0
	INC	HL
	DEC	DE
	LD	A,D
	OR	E
	JR	NZ,D_FILLL
	JR	SMUL_E

D_BORS:	CALL	D_TWOS
D_BORL:	LD	A,B
	OR	C
	JR	Z,SMUL_E
	LD	A,(DE)
	OR	(HL)
	LD	(DE),A
	INC	HL
	INC	DE
	DEC	BC
	JR	D_BORL

D_XORS:	CALL	D_TWOS
D_XORL:	LD	A,B
	OR	C
	JP	Z,SMUL_E
	LD	A,(DE)
	XOR	(HL)
	LD	(DE),A
	INC	HL
	INC	DE
	DEC	BC
	JR	D_XORL

D_RLS:	RST	$28
	DEFB	$1B		; negate
	DEFB	$38		; end
D_RRS:	POP	HL		; discard return address
	POP	HL		; RE-ENTRY
	POP	DE		; discard BREG?
	POP	DE		; discard USR
	LD	DE,$106F	; CHR$, a num-to-string function
	PUSH	DE
	LD	DE,0
	PUSH	DE		; BREG = 0
	PUSH	HL		; RE-ENTRY
	RST	$30
	DEFW	L2DA2		; FP-TO-BC
	JP	C,ERROR_B
	JR	Z,D_RR
	DEC	BC
	LD	A,B
	CPL
	LD	B,A
	LD	A,C
	CPL
	LD	C,A
D_RR:	SRA	B
	RR	C
	SRA	B
	RR	C
	SRA	B
	RR	C
	AND	$07
	PUSH	AF
	PUSH	BC
	INC	HL
	INC	HL
	INC	HL
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	LD	A,B
	OR	C
	JR	Z,D_RRE0
	POP	DE
	INC	HL
	LD	(HL),0
	INC	HL
	RL	D
	SBC	A,A
	RR	D
	LD	(HL),A
	INC	HL
	LD	(HL),E
	INC	HL
	LD	(HL),D
	INC	HL
	INC	HL
	LD	(STKEND),HL	; Signed amount by which to rotate
	PUSH	BC		; LEN
	RST	$30
	DEFW	L2D2B + 4	; STACK-BC

	RST	$28
	DEFB	$32		; mod
	DEFB	$38		; end

	RST	$30
	DEFW	L2DA2		; FP-TO-BC
	POP	HL		; LEN
	JR	Z,D_RRPOS
	SBC	HL,BC
	LD	C,L
	LD	B,H
D_RRPOS:PUSH	BC
	RST	$30
	DEFW	L2BF1		; STK-FETCH
	PUSH	DE
	RST	$30
	DEFW	L0030		; BC-SPACES
	RST	$30
	DEFW	L2AB2		; STK-STO
	POP	HL
	LD	(MEMBOT+1),HL
	LD	(MEMBOT+3),BC
	ADD	HL,BC
	POP	BC
	PUSH	BC
	LD	A,B
	OR	C
	JR	Z,D_RR0C
	SBC	HL,BC
	LDIR
D_RR0C:	POP	BC
	LD	HL,(MEMBOT+3)
	SBC	HL,BC
	LD	C,L
	LD	B,H
	LD	HL,(MEMBOT+1)
	LDIR
	POP	AF
	JR	Z,SW_R1
D_RRRL:	EX	AF,AF'
	CALL	FETCH
	PUSH	DE
	EX	DE,HL
	ADD	HL,BC
	DEC	HL
	LD	A,C
	OR	A
	JR	Z,D_RRB
	INC	B
D_RRB:	LD	A,(HL)
	RRA
	POP	HL
D_RRRR:	RR	(HL)
	INC	HL
	DEC	C
	JR	NZ,D_RRRR
	DJNZ	D_RRRR
	EX	AF,AF'
	DEC	A
	JR	NZ,D_RRRL
	JR	SW_R1

D_RRE0:	POP	BC
	POP	AF
SW_R1:	JP	SMUL_E

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

FETCH:	LD	HL,(STKEND)
	DEC	HL
	LD	B,(HL)
	DEC	HL
	LD	C,(HL)
	DEC	HL
	LD	D,(HL)
	DEC	HL
	LD	E,(HL)
	RET

MAIN_ADD_CONT:
	PUSH	BC
;; TODO: find out why it gets reset
;;	BIT	7,(IY+FLAGS2-ERR_NR)
;;	CALL	NZ,RSTLBLS

	CALL	RSTLBLS
	POP	BC
SW_MA:	RST	$10

RSTLBLS:RES	7,(IY+FLAGS2-ERR_NR)
	LD	HL,(PROG)
	LD	DE,$0005
NX_LIN:	LD	A,(HL)
	AND	$C0
	RET	NZ
	ADD	HL,DE
	DEC	HL
NX_INS:	LD	A,(HL)
	INC	HL
	CP	PROC_T
	JR	Z,RST_PR
	CP	ELSE_T
	JR	Z,NX_INS
	DEFB	$01		; LD	BC, skip two bytes
NX_CHR:	LD	A,(HL)
	INC	HL
	CP	$0D
	JR	Z,NX_LIN
	CP	":"
	JR	Z,NX_INS
	CP	THEN_T
	JR	Z,NX_INS
	CP	"\""
	JR	Z,SKQUOT
	CP	$0E
	JR	Z,SK_NUM
	CP	"@"
	JR	NZ,NX_CHR
RST_PR:	LD	A,$0E
SK_LBL:	CP	(HL)
	INC	HL
	JR	NZ,SK_LBL
	LD	B,E
RST_LBL:LD	(HL),D
	INC	HL
	DJNZ	RST_LBL
	JR	NX_CHR
SKQUOT:	CP	(HL)
	INC	HL
	JR	NZ,SKQUOT
	JR	NX_CHR
SK_NUM:	ADD	HL,DE
	JR	NX_CHR

SUB_CONT:
	LD	HL,$000A
	ADD	HL,SP
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	LD	HL,SUB_ER
	AND	A
	SBC	HL,DE
	JR	Z,SW_SUB	; Return, if called from DIM
	POP	DE		; discard one return address
	POP	DE		; discard other return address
	POP	DE		; error reg in D
	POP	HL		; limit in HL
	PUSH	HL		; put it back
	DEC	BC
	AND	A
	SBC	HL,BC
	LD	B,H
	LD	C,L		; BC = HL - (BC - 1)
	POP	HL		; restore limit
	LD	A,D		; error value in A
	POP	DE		; restore DE
	JR	Z,SUB_OOR
	JR	NC,SW_SUB	; return, if we are in range
SUB_OOR:SCF
	DEC	A
SW_SUB:	RST	$10

STACKSWAP:
	LD	HL,SWAP
	EX	(SP),HL
	JP	(HL)


	INCLUDE "variables.asm"
	INCLUDE	"instructions.asm"
	INCLUDE "serial.asm"

	DEFS	LIST_HOOK - 27 - $

; on error handling
ONERRJ:	JP	ONERR_DO
; channel service routines (24 bytes)
CH_PO:	JP	PR_OUT
CH_PI:	JP	PR_IN
CH_KO:	JP	K_OUT
CH_KI:	JP	K_IN
CH_XO:	JP	X_OUT
CH_XI:	JP	X_IN
CH_NO:	JP	NEW_X_OUT
CH_NI:	JP	NEW_X_IN

; jump table from ROM1
	JP	INDEX_CONT
	JP	LIST_CONT
	JP	SCAN_CONT
	JP	SUB_CONT
	JP	STRNG_CONT
	JP	DIGIT_CONT
	JP	GOTO_CONT
	JP	FOR_CONT
	JP	SKIP_FOR_CONT
	JP	NEXT_CONT
	JP	LV_CONT
	JP	RETURN_CONT
	JP	MAIN_ADD_CONT
	JP	ERR_CONT
	JP	RUN_CONT
	JP	LOCAL_CONT
	JP	NEW128
	JP	STEP_CONT
	JP	TEMPS_CONT
	JP	F_SCAN
	DEFS	$3D00 - $

	DEFB	00001111B
	DEFB	00010111B
	DEFB	00010111B
	DEFB	00110011B
	DEFB	00110011B
	DEFB    01110001B
	DEFB	01110001B
	DEFB	11110000B

	DEFB	11110000B
	DEFB	11101000B
	DEFB	11101000B
	DEFB	11001100B
	DEFB	11001100B
	DEFB	10001110B
	DEFB	10001110B
	DEFB	00001111B

	DEFB	11100000B
	DEFB	11000000B
	DEFB	11000000B
	DEFB	10000000B
	DEFB	00001000B
	DEFB	00001100B
	DEFB	00001100B
	DEFB	00001110B

	DEFB	00000111B
	DEFB	00000011B
	DEFB	00000011B
	DEFB	00000001B
	DEFB	00010000B
	DEFB	00110000B
	DEFB	00110000B
	DEFB	01110000B

CHARSET:INCBIN	"64-cond.bin"

	DEFB	00000011B
	DEFB	00000011B
	DEFB	00000011B
	DEFB	00000011B
	DEFB	00000000B
	DEFB	00000000B
	DEFB	00000000B
	DEFB	00000000B

	DEFB	11001111B
	DEFB	11001111B
	DEFB	11001111B
	DEFB	11001111B
	DEFB	00000000B
	DEFB	00000000B
	DEFB	00000000B
	DEFB	00000000B

	DEFB	00000011B
	DEFB	00000011B
	DEFB	00000011B
	DEFB	00000011B
	DEFB	00110011B
	DEFB	00110011B
	DEFB	00110011B
	DEFB	00110011B

	DEFB	11001111B
	DEFB	11001111B
	DEFB	11001111B
	DEFB	11001111B
	DEFB	00110011B
	DEFB	00110011B
	DEFB	00110011B
	DEFB	00110011B

	DEFB	00000011B
	DEFB	00000011B
	DEFB	00000011B
	DEFB	00000011B
	DEFB	11001100B
	DEFB	11001100B
	DEFB	11001100B
	DEFB	11001100B

	DEFB	11001111B
	DEFB	11001111B
	DEFB	11001111B
	DEFB	11001111B
	DEFB	11001100B
	DEFB	11001100B
	DEFB	11001100B
	DEFB	11001100B

	DEFB	00000011B
	DEFB	00000011B
	DEFB	00000011B
	DEFB	00000011B
	DEFB	11111111B
	DEFB	11111111B
	DEFB	11111111B
	DEFB	11111111B

	DEFB	11001111B
	DEFB	11001111B
	DEFB	11001111B
	DEFB	11001111B
	DEFB	11111111B
	DEFB	11111111B
	DEFB	11111111B
	DEFB	11111111B

	INCLUDE	"tokens.asm"
CHINFO0:
K_CH:	DEFW	KOUT
	DEFW	KIN
	DEFB	"K"
S_CH:	DEFW	KOUT
	DEFW	L15C4
	DEFB	"S"
R_CH:	DEFW	L0F81
	DEFW	L15C4
	DEFB	"R"
P_CH:	DEFW	POUT
	DEFW	PIN
	DEFB	"P"
	DEFB	$80
CHINFO0_E:	EQU	$

INIT_STRM:
	DEFW	K_CH - CHINFO0 + 1	; stream $FD offset to channel 'K'
        DEFW    S_CH - CHINFO0 + 1	; stream $FE offset to channel 'S'
        DEFW    R_CH - CHINFO0 + 1	; stream $FF offset to channel 'R'

        DEFW    K_CH - CHINFO0 + 1	; stream $00 offset to channel 'K'
        DEFW    K_CH - CHINFO0 + 1	; stream $01 offset to channel 'K'
        DEFW    S_CH - CHINFO0 + 1	; stream $02 offset to channel 'S'
        DEFW    P_CH - CHINFO0 + 1	; stream $03 offset to channel 'P'

R_LINK:	DEFB	$00, $03, $00, $07, $01, $00, $04, $FF

S_IOCTL:DEFW	S_RST	; reset S channel (clear screen, etc.)
	DEFW	AUTOLIST
	DEFW	PLOT1	; PLOT a single point
	DEFW	DRAW2	; DRAW straight line
	DEFW	DRAW3	; DRAW arc
	DEFW	CIRCLE	; draw a CIRCLE
S_IOCTL_END:	EQU	$

GR_TAB:	DEFB	$00, $FF
	DEFB	$FF, $00
	DEFB	$F0, $00
	DEFB	$00, $0F

	DEFS	$4000 - $
