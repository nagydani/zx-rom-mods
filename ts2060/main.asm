	INCLUDE	"../labels.asm"

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
	JP	SWAP1
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

PAGEIRQ:OUT	($F4), A
; IM1 routine
RST38:	PUSH	AF
	PUSH	HL
	LD	HL,SWAPIRQ
	EX	(SP),HL
	LD	A, 1
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
ROM1SW:	JP	SWAP1		; this one is performance-critical

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

CH_ADD_1:
	LD	HL,(CH_ADD)
TEMP_PTR1:
	INC	HL
TEMP_PTR2:
	LD	(CH_ADD),HL
	LD	A,(HL)
	RET

ZX:	LD	SP,$0051	; 0000
	RST	RST10

RESET:	LD	A,$80
	OUT	($FF),A		; EX-ROM, IRQ, BLACK ON WHITE, SCREEN0
	LD	A,$3F
	LD	I,A
	LD	A,$7F
	IN	A,($FE)
	RRA			; SPACE pressed?
	JR	NC,ZX		; jump if so.
	LD	HL,INIT_5B00
	LD	DE,$5B00
	LD	BC,INIT_5B00_L
	LDIR
	LD	L,E
	LD	H,D
	INC	DE
	LD	(HL),$00
	LD	BC,$FFFF-INIT_5B00_E
	LDIR
	LD	(P_RAMT),HL
	LD	DE,$3EAF		; last byte of U
	LD	BC,$00A8
	EX	DE,HL
	LDDR
	EX	DE,HL
	INC	HL
	LD	(UDG),HL
	DEC	HL
	LD	BC,$0140
	LD	(RASP),BC
	LD	(RAMTOP),HL
NEWTS:	LD	A,7
	OUT	($FE),A
	LD	HL,$3C00
	LD	(CHARS),HL
	LD	HL,(RAMTOP)
	LD	(HL),$3E
	DEC	HL
	LD	SP,HL
	DEC	HL
	DEC	HL
	LD	(ERR_SP),HL
	LD	HL,NMIVEC
	LD	(NMIADD),HL
	IM	1
	LD	IY,ERR_NR
	LD	(IY+FLAGS-ERR_NR),$10	; TS2060 mode
	EI
	LD	HL,CHINFO
	LD	(CHANS),HL
	LD	DE,L15AF
	LD	BC,$0015
	EX	DE,HL
	RST	RST30
	DEFW	LDIRR
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
; measure framerate
	LD	BC,$FFFF
	LD	HL,FRAMES
	HALT
	LD	A,(HL)
FRAMERT:INC	BC
	CP	(HL)
	JR	Z,FRAMERT
	RST	RST30
	DEFW	STACKBC
	RST	RST28
	DEFB	$34,$40,$43,$22,$03	; literal 331800
	DEFB	$01			; exchange
	DEFB	$05			; division
	DEFB	$38			; end_calc
	RST	RST30
	DEFW	L2DD5		; FP-TO-A
	LD	(BEAT),A

	LD	HL,P_OUT	; output service routine in this ROM
	LD	(CHINFO),HL
	LD	(CHINFO+5),HL
	LD	(CHINFO+15),HL
	LD	A,$38
	LD	(ATTR_P),A
	LD	(ATTR_T),A
	LD	(BORDCR),A
	LD	HL,$0523
	LD	(REPDEL),HL
	DEC	(IY+KSTATE-ERR_NR)
	DEC	(IY+KSTATE+4-ERR_NR)
	LD	HL,L15C6
	LD	DE,STRMS
	LD	BC,$000E
	RST	RST30
	DEFW	LDIRR
	LD	(IY+DF_SZ-ERR_NR),$02
	RST	RST30
	DEFW	L0D6B		; CLS
	LD	DE,COPYRIGHT
	CALL	STDERR_MSG
	LD	DE,CPR_MSG
	PUSH	DE
	RST	RST10

INIT_5B00:	EQU	$
; This stuff does not run here, it gets copied to $5B00
	ORG	$5B00
SWAP:	PUSH	AF
	XOR	A
	OUT	($F4),A
	POP	AF
	EX	(SP),HL
	RES	5,H
	EX	(SP),HL
	RET

SWAPIRQ:XOR	A
	OUT	($F4),A
	POP	AF
	RET

P_OUT:	LD	HL,PR_OUT
P_OUT1:	EQU	P_OUT+1
SWAP2:	PUSH	HL
	JR	SWAP

ERRHOOK:LD	HL,ONERROR
	JR	SWAP2

TEMPO:	DEFB	120		; in BPM for PLAY
BEAT:	EQU	$		; framerate constant for PLAY
PLAY_ST:EQU	BEAT+1		; PLAY state
TARGET:	EQU	PLAY_ST+2	; address in ROM1
ERRLN:	EQU	TARGET+2
ERRC:	EQU	ERRLN+2
ERRS:	EQU	ERRC+2
ERRT:	EQU	ERRC+1

INIT_5B00_E:	EQU	$

INIT_5B00_L:	EQU	$ - $5B00

	ORG	INIT_5B00 + INIT_5B00_L



	DEFS	L0554 - $ - 5	; POP AF \ RET in ROM1
SWAP1:	PUSH	AF
	LD	A, 1
	OUT	($F4), A
; Control returned to ROM1

STACKSWAP:
	LD	HL,SWAP1
	EX	(SP),HL
	JP	(HL)

RUN_CONT:
	INC	B
	DJNZ	SWAP1		; separator mismatch
	JR	NC,OLD_CONT
	ADD	A,$C9-PLAY_T
	LD	C,A
	JR	C,SWAP1		; old token instruction
	DEC	B
	POP	HL		; discard REPORT_C
	LD	HL,P_END
	ADD	HL,BC
	INC	B
	LD	C,(HL)
	ADD	HL,BC
	CP	LABEL_T + $52
	JR	NC,GET_PARAM
ERROR_C1:
	RST	$30
	DEFW	REPORT_C

OLD_CONT:
	LD	A,(T_ADDR)
	CP	$B2
	JP	Z,E_POKE
	JR	ERROR_C1

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

SEPARATOR:
	RST	$18
	CP	C
	JR	NZ,ERROR_C1
	RST	$20
	RET

; Same as L16DB in ROM1
INDEXER_1:
	INC	HL
INDEXER:LD	A,(HL)
	OR	A
	RET	Z
	CP	C
	INC	HL
	JR	NZ,INDEXER_1
	SCF
	RET

LV_CONT:PUSH	HL
	LD	HL,L28B2	; LOOK-VARS, no long string names
	EX	(SP),HL
	RST	$10

; Single-argument original function extended to multiple arguments
MULTI_CONT:
	POP	BC	; discard return address
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
	POP	BC
	LD	BC,D_STRING
	JP	S_FUNC

MULS_S:	LD	BC,$104C	; tight multiplication
	LD	HL,L2790
	EX	(SP),HL
	RST	$10

ERR_CONT:
	CP	$1C
	JR	C,DSWAP2
	CALL	REPORT
ERR_C:	LD	HL,X1349
	EX	(SP),HL
DSWAP2:	RST	RST10

PREFIX_CONT:
	RST	$18
	LD	C,A
	LD	HL,SCANFUNC2
IDX_DO:	CALL	INDEXER
SWIDS:	JR	NC,DSWAP2
	POP	BC
	LD	C,(HL)
	LD	B,0
	ADD	HL,BC
	JP	(HL)

ERROR:	LD	HL,(CH_ADD)
	LD	(X_PTR),HL
	LD	HL,L0055
	EX	(SP),HL
	LD	L,(HL)
	RST	$10

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

LIST_CONT:
	POP	BC	; discard return address
	SET	6,(IY+TV_FLAG-ERR_NR)	; LIST active
	BIT	2,(IY+FLAGS-ERR_NR)	; printing in K mode?
	JR	NZ,LIST_L		; jump, if not
	SET	2,(IY+TV_FLAG-ERR_NR)	; signal instruction
	JR	LIST_K
LIST_L:	RES	2,(IY+TV_FLAG-ERR_NR)	; signal arguments
LIST_K:	RST	$30
	DEFW	L1937			; OUT-CHAR
	RES	6,(IY+TV_FLAG-ERR_NR)	; LIST inactive
	RST	$10

	INCLUDE	"strmul.asm"
	INCLUDE	"functions.asm"
	INCLUDE	"instructions.asm"
	INCLUDE	"channels.asm"
	INCLUDE	"reportz.asm"
	INCLUDE	"reports.asm"
	INCLUDE	"tokens.asm"
	INCLUDE	"calculator.asm"

	DEFS	LIST_HOOK - $2000 - $


ONERR_DO:	EQU	SWAP1
INDEX_CONT:	EQU	SWAP1
SUB_CONT:	EQU	SWAP1
STRNG_CONT:	EQU	SWAP1
GOTO_CONT:	EQU	SWAP1
FOR_CONT:	EQU	SWAP1
SKIP_FOR_CONT:	EQU	SWAP1
NEXT_CONT:	EQU	SWAP1
RETURN_CONT:	EQU	SWAP1
MAIN_ADD_CONT:	EQU	SWAP1
LOCAL_CONT:	EQU	SWAP1
STEP_CONT:	EQU	SWAP1

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
	JP	NEWTS
	JP	STEP_CONT
	JP	TEMPS_CONT
	JP	F_SCAN

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
MOD2A:	RST	$28	; calc
	DEFB	$32	; mod
	DEFB	$01	; exchange
	DEFB	$38	; end
	JP	L2DA2   ; FP-TO-BC (and A)

; Move both pointers back by one entry
STEPBACK:
	LD	BC,-5
	LD	D,H
	LD	E,L
	ADD	HL,BC
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

	DEFS	$2000 - $
