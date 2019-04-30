	DEFB	P_PLUG - $	; G8
	DEFB	P_PLUG - $	; G1
	DEFB	P_PLUG - $	; G2
	DEFB	P_PLUG - $	; G3
	DEFB	P_PLUG - $	; G4
	DEFB	P_PLUG - $	; G5
	DEFB	P_PLUG - $	; G6
	DEFB	P_PLUG - $	; G7
	DEFB	P_PLUG - $	; Gs8
	DEFB	P_PLUG - $	; Gs1
	DEFB	P_PLUG - $	; Gs2
	DEFB	P_PLUG - $	; Gs3
	DEFB	P_PLUG - $	; Gs4
	DEFB	P_PLUG - $	; Gs5
	DEFB	P_PLUG - $	; Gs6
	DEFB	P_PLUG - $	; Gs7
	DEFB	P_PLUG - $	; GA
	DEFB	P_PLUG - $	; GB
	DEFB	P_PLUG - $	; GC
	DEFB	P_PLUG - $	; GD
	DEFB	P_PLUG - $	; GE
	DEFB	P_PLUG - $	; GF
	DEFB	P_PLUG - $	; GG
	DEFB	P_PLUG - $	; GH
	DEFB	P_PLUG - $	; GI
	DEFB	P_PLUG - $	; GJ
	DEFB	P_PLUG - $	; GK
	DEFB	P_PLUG - $	; GL
	DEFB	P_PLUG - $	; GM
	DEFB	P_PLUG - $	; GN
	DEFB	P_PLUG - $	; GO
	DEFB	P_PLUG - $	; GP
	DEFB	P_PLUG - $	; GQ
	DEFB	P_PLUG - $	; GR
	DEFB	P_PLUG - $	; GS
	DEFB	P_PLUG - $	; GT
	DEFB	P_PLAY - $	; PLAY
	DEFB	P_PLUG - $	; ET
	DEFB	P_PLUG - $	; EN
	DEFB	P_PLUG - $	; EM
	DEFB	P_PLUG - $	; Es2
	DEFB	P_PLUG - $	; Es8
	DEFB	P_STACK - $	; STACK
	DEFB	P_LABEL - $	; EsL
	DEFB	P_PLUG - $	; sI
	DEFB	P_PLAY - $	; PLAY
	DEFB	P_PLUG - $	; EsJ
	DEFB	P_PLUG - $	; EI
	DEFB	P_PLUG - $	; EJ
	DEFB	P_PLUG - $	; EK
	DEFB	P_PLUG - $	; EQ
	DEFB	P_PLUG - $	; EW
	DEFB	P_PLUG - $	; EE
	DEFB	P_PLUG - $	; EsQ
	DEFB	P_PLUG - $	; EsW
	DEFB	P_PLUG - $	; EsE
	DEFB	P_LOCAL - $	; LOCAL
	DEFB	P_PLUG - $	; EX
	DEFB	P_REPEAT - $	; REPEAT
	DEFB	P_PLUG - $	; EH
	DEFB	P_PLUG - $	; EF
	DEFB	P_PLUG - $	; EG
	DEFB	P_POKE - $	; POKE
	DEFB	P_PLUG - $	; EsI
	DEFB	P_USR - $	; EL
	DEFB	P_PLUG - $	; EY
	DEFB	P_UNTIL - $	; UNTIL
	DEFB	P_PLUG - $	; sS
	DEFB	P_PLUG - $	; EB
	DEFB	P_ENDIF - $	; END IF
	DEFB	P_PLUG - $	; sY
	DEFB	P_PLUG - $	; sQ
	DEFB	P_PLUG - $	; sE
	DEFB	P_PLUG - $	; sW
	DEFB	P_PLUG - $	; Es3
	DEFB	P_ELSE - $	; ELSE
	DEFB	P_PLUG - $	; sF
	DEFB	P_PLUG - $	; sD
P_END:	EQU	$

; No parameters, no action
P_ENDIF:DEFB	$00
	DEFW	ENDIF

P_ELSE:	DEFB	$05
	DEFW	ELSE

P_POKE:	DEFB	$06	; numeric expression
	DEFB	","
	DEFB	$05	; list of items
	DEFW	POKE

P_STACK:DEFB	$07
	DEFW	STACK

P_REPEAT:
	DEFB	$00
	DEFW	REPEAT

P_UNTIL:
	DEFB	$06, $00
	DEFW	UNTIL

P_USR:	DEFB	$06, $00
	DEFW	USR

P_LABEL:DEFB	$05
	DEFW	LABEL

P_LOCAL:DEFB	$05
	DEFW	LOCAL

P_PLAY:
; unimplemented instruction, accepted w/o parameters, but not executed
P_PLUG:
	DEFB	$00
	DEFW	PLUG

CHECK_END:
	BIT	7,(IY+$01)
	RET	NZ
	POP	BC		; SCAN_LOOP
	POP	BC		; STMT_RET
STMT_NEXT:
	RST	$18
	CP	$0D		; CR
	JR	Z,LINE_END
	CP	":"
	JR	Z,STMT_LOOP
	JR	ERROR_C_I

STMT_LOOP:
	LD	HL,L1B28	; STMT_LOOP
	PUSH	HL
	JP	SWAP

LINE_END:
	BIT	7,(IY+$01)
	JP	Z,SWAP
	LD	HL,(NXTLIN)
	LD	A,$0C
	AND	(HL)
	JP	NZ,SWAP		; program finished
	PUSH	HL
	LD	HL,L1BBF - 1	; XOR A, LINE-USE
	EX	(SP),HL
	JP	SWAP

CMDCLASS2:
	DEFB	CLASS2_00 - $	; parameterless instruction
	DEFB	CLASS2_01 - $	; do something to a variable
	DEFB	CLASS2_02 - $	; prepare value for assignment
	DEFB	CLASS2_03 - $	; one numeric parameter, defaults to zero
	DEFB	CLASS2_04 - $	; used by FOR & NEXT -- TODO: may be worth replacing
	DEFB	CLASS2_05 - $	; list of items
	DEFB	CLASS2_06 - $	; evaluate single numeric expression
	DEFB	CLASS2_07 - $	; open #2 or other stream before execution
	DEFB	CLASS2_08 - $	; two numeric expressions, separated by comma

CLASS2_03:
	CALL	FETCH_NUM
CLASS2_00:
	CP	A
CLASS2_05:
	POP	BC
	CALL	Z,CHECK_END
	EX	DE,HL
	LD	HL,(T_ADDR)
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	EX	DE,HL
	PUSH	BC
	RET

CLASS2_01:
	RST	$28
	DEFW	L1C1F		; CLASS_01

CLASS2_02:
	RST	$28
	DEFW	L1C4E		; CLASS_02

CLASS2_04:
	RST	$28
	DEFW	L1C6C		; CLASS_04

NEXT_2NUM:
	RST	$20

CLASS2_08:
	CALL	CLASS2_06
	CP	","
	JR	NZ,ERROR_C_I
	RST	$20

CLASS2_06:
	RST	$28
	DEFW	L24FB
	BIT	6,(IY+$01)
	RET	NZ

ERROR_C_I:
	JP	ERROR_C

FETCH_NUM:
	CP	$0D
	JR	Z,USE_ZERO
	CP	":"
	JR	NZ,CLASS2_06

USE_ZERO:
	RST	$28
	DEFW	L1CE6		; USE-ZERO
	RET

CLASS2_07:
	RST	$28
	DEFW	L2070		; STR-ALTER
	JR	NC,STR_ALTERED
	CALL	SYNTAX_Z
	JR	Z,STR_ALTERED
	LD	A,2
	RST	$28
	DEFW	L1601	; CHAN-OPEN
STR_ALTERED:
	JR	CLASS2_00


; instruction routines
ENDIF:	RES	6,(IY+$37)	; signal true outcome
	JP	SWAP

THENLESS:
	RES	6,(IY+$37)	; signal true outcome
	RST	$28
        DEFW	L35BF	; STK-PNTRS
	CALL	STEPBACK
	EX	DE,HL
	LD	(STKEND),HL
	XOR	A
	CP	(HL)
SWAPNZ:	JP	NZ,SWAP		; Upon true condition, simply continue
	INC	HL
	INC	HL
	CP	(HL)
	JR	NZ,SWAPNZ
	INC	HL
	CP	(HL)
	JR	NZ,SWAPNZ

NESTING:EQU	TSTACK - 2
NEST2:	EQU	NESTING + 1

; Upon false condition start scanning for END IF, ELSE or end of code
	SET	6,(IY+$37)	; signal false outcome
	LD	BC,(NXTLIN)
	LD	DE,T_IF
	CALL	LOOK_PROG2
	LD	(NXTLIN),BC
	JR	C,ERROR_C_I	; TODO: missing END IF
	POP	BC		; discard SCAN-LOOP
	RST	$20
	JP	SWAP

ELSE:	POP	BC		; discard STMT-RET
	BIT	7,(IY+$01)
	JR	Z,ELSE_1
	BIT	4,(IY+$37)	; FLAGX, check if last IF was false
	RES	4,(IY+$37)
	JR	NZ,ELSE_1
	RST	$18
	CP	$0D
	JR	Z,ELSE_3	; multi-line ELSE block
	LD	BC,L1BB3	; LINE-END
	JR	ELSE_2
ELSE_1:	LD	BC,L1B29	; STMT-L-1
ELSE_2:	PUSH	BC
	JP	SWAP
ELSE_3:	PUSH	BC		; put back STMT-RET
	LD	BC,(NXTLIN)
	LD	DE,T_IF
	CALL	LOOK_PROG2
	LD	(NXTLIN),BC
	JR	C,ERROR_T	; missing END IF
	RST	$20
	JP	SWAP

ERROR_T:CALL	ERROR
	DEFB	$1C		; T Missing END IF

POKE:	CALL	SYNTAX_Z
	JR	Z,POKE_S
	RST	$28
	DEFW	L1E99		; FIND-INT2
	DEFB	$3E		; LD A, skip next instruction
POKE_L:	RST	$20		; advance
	PUSH	BC
	RST	$28
	DEFW	L24FB		; SCANNING
	EX	AF,AF'
	BIT	6,(IY+$01)	; numeric?
	JR	Z,SPOKE		; jump, if not
	RST	$28
	DEFW	L2DD5		; FP-TO-A
	JP	C,ERROR_B
	JR	Z,POKEP
	NEG
POKEP:	POP	BC
	LD	(BC),A
	INC	BC
	JR	POKE_L2
SPOKE:	RST	$28
	DEFW	L2BF1		; STK-FETCH
	LD	A,B
	OR	C
	EX	DE,HL
	POP	DE
	JR	Z,EPOKE
	LDIR
EPOKE:	LD	C,E
	LD	B,D
POKE_L2:EX	AF,AF'
	CP	","
	JR	Z,POKE_L
POKE_SWAP:
	JP	SWAP

POKE_S:	LD	HL,L1E2C	; DATA-1
POKE_E:	PUSH	HL
	JR	POKE_SWAP

LABEL:	CALL	SYNTAX_Z
	JR	NZ,LABEL_R
	LD	BC,$0006
	RST	$28
	DEFW	L2C8D		; ALPHA
	JP	NC,ERROR_C
	INC	HL		; insert pointers after first letter of label
	RST	$28
	DEFW	L1655		; MAKE-ROOM
	LD	DE,(E_LINE)
LABEL_S:LD	A,(DE)
	INC	DE
	CP	" " + 1
	JR	C,LABEL_S	; skip indent
LABEL_N:RST	$28
	DEFW	L2D1B		; NUMERIC
	JR	C,LABEL_C
	INC	DE
	LD	A,(DE)
	JR	LABEL_N		; skip line number
LABEL_C:LD	A,(SUBPPC)
	INC	HL
	LD	(HL),$0E	; number marker
	INC	HL
	PUSH	HL		; length goes here
	INC	HL
	INC	HL
	LD	(HL),A
	INC	HL
	EX	DE,HL
	SCF
	SBC	HL,DE
	EX	DE,HL
	LD	(HL),E
	INC	HL
	LD	(HL),D
	RST	$28
	DEFW	L0077
LABEL_L:RST	$20
	RST	$28
	DEFW	L2C88		; ALPHANUM
	JR	C,LABEL_L
	POP	DE		; length pointer
	SCF
	SBC	HL,DE
	EX	DE,HL
	LD	(HL),E
	INC	HL
	LD	(HL),D
	JR	LABEL_E
LABEL_R:INC	HL
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	ADD	HL,DE
	LD	(CH_ADD),HL
LABEL_E:LD	HL,L1BF4	; STMT-NEXT
	JR	POKE_E

USR:	CALL	SYNTAX_Z
	JR	Z,POKE_SWAP
	RST	$28
	DEFW	L1E99		; FIND-INT2
	PUSH	BC
	JR	POKE_SWAP

REPEAT:	POP	DE		; DE = return address
	LD	HL,(SUBPPC - 1)
	EX	(SP),HL		; HL = error address
	INC	SP		; stack SUBPPC (1 byte)
	LD	BC,(PPC)
	PUSH	BC		; stack PPC (2 bytes)
	PUSH	HL
	LD	HL,(CH_ADD)
	AND	A
	LD	BC,(PROG)
	SBC	HL,BC
	EX	(SP),HL		; stack CH_ADD - PROG (2 bytes)
	PUSH	HL
	LD	HL,(NXTLIN)
	SBC	HL,BC
	EX	(SP),HL		; stack NXTLIN - PROG (2 bytes)
	LD	BC,$3E00 + REPEAT_M
	PUSH	BC		; stack marker
	PUSH	HL		; stack error address
	LD	(ERR_SP),SP
	PUSH	DE		; stack return address
	LD	BC,$0014	; why this much? see $1F02 in ROM1
	LD	HL,L1F05	; TEST-ROOM
	PUSH	HL
	JP	SWAP

UNTIL:	RST	$28
	DEFW	L35BF		; STK-PNTRS
	RST	$28
	DEFW	L34E9		; TEST-ZERO
	EX	AF,AF'
	POP	BC		; return address
	POP	HL		; error address
	POP	DE		; marker
	LD	A,D
	CP	$3E
	JR	NZ,ERROR_S	; after GO SUB
	LD	A,E
	CP	REPEAT_M
	JR	Z,UNT_NL	; no local variables
	PUSH	DE		; put back marker
	EXX
	CALL	SKIP_LL
	CP	REPEAT_M
	EXX
	JR	NZ,ERROR2S	; wrong context
	EX	AF,AF'
	JR	NC,UNT_R	; reclaim local context
	PUSH	HL		; error address
	PUSH	BC		; return address
	EXX
	INC	HL
	INC	HL		; skip marker
	JR	UNT_C		; continue loop

UNT_R:	EXX
	LD	SP,HL		; reclaim local variables
	EXX
	EX	DE,HL
	JR	UNT_ER		; continue after UNTIL

UNT_NL:	EX	AF,AF'
	JR	NC,END_REP
	PUSH	DE		; marker
	PUSH	HL		; error address
	PUSH	BC		; return address
	LD	HL,$0006
	ADD	HL,SP
UNT_C:	LD	BC,(PROG)
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	EX	DE,HL
	ADD	HL,BC
	LD	(NXTLIN),HL
	EX	DE,HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	EX	DE,HL
	ADD	HL,BC
	LD	(CH_ADD),HL
	EX	DE,HL
	LD	DE,PPC
	LD	BC,$0003
	LDIR
REPSW:	JP	SWAP
END_REP:EX	DE,HL
	LD	HL,$0007
	ADD	HL,SP
	LD	SP,HL
UNT_ER:	PUSH	DE
UNT_E:	LD	(ERR_SP),SP
	PUSH	BC
	JR	REPSW

ERROR_S:PUSH	DE
ERROR2S:PUSH	HL
	CALL	ERROR
	DEFB	$1B		; S UNTIL without REPEAT

ERROR_2:CALL	ERROR
	DEFB	$01		; 2 Variable not found

; LET with operator update
UPDATE:	LD	C,A
	RST	$20
	CP	"="
	JR	NZ,ERROR_C_J
	CALL	SYNTAX_Z
	JR	Z,UPD_S
	BIT	1,(IY+$37)	; Does the variable exist?
	JR	NZ,ERROR_2	; Report error, if not
	LD	HL,(DEST)
	INC	HL
	LD	DE,(STKEND)
	BIT	6,(IY+$01)	; Numeric variable?
	JR	NZ,UPD_STK	; Jump, if so
	LD	L,E
	LD	H,D		; string parameters already in place
UPD_STK:LD	A,C		; save C
	LD	BC,5
	LDIR
	LD	C,A		; restore C
	LD	(STKEND),DE	; restore the variable on the VM stack
	POP	HL		; drop SCAN-LOOP
	LD	A,(FLAGS)
	LD	HL,L1BEE	; CHECK-END
	PUSH	HL
	PUSH	AF
	LD	HL,L1C59 + 4	; VAL-FET-2 continues
	JR	UPD_X

UPD_S:	LD	HL,L1BEE	; CHECK-END
UPD_X:	PUSH	HL
	LD	H,0		; starting priority marker 0
	LD	B,H		; clear B for OPERTR
	PUSH	HL
	LD	HL,L2795	; OPERATORS table in ROM1
	RST	$28
	DEFW	L16DC		; INDEXER in ROM1
	JR	C,OLDOPR

	BIT	6,(IY+$01)	; are we expecting a string
	JR	Z,UPDSTR	; jump, if so

	LD	A,"%"
	CP	C
	JR	NZ,UPDNUM	; not MOD
	LD      BC,$01C2        ; delete with priority 1
        PUSH    BC
        LD      BC,$01F2        ; mod with priority 1
U_NEXT:	LD      HL,L2790        ; S-NEXT
	PUSH    HL
REPSW1:	JR      REPSW

UPDNUM:	LD	HL,UPDTABN
	CALL	INDEXER
	CALL	SYNTAX_Z
	JR	NZ,UPD_DO
	JR	NC,ERROR_C_J
	LD	BC,$01CF	; numeric addition with priority 1
	JR	U_NEXT

ERROR_C_J:
	JP	ERROR_C

UPDSTR:	LD	HL,UPDTABS
	CALL	INDEXER
	CALL	SYNTAX_Z
	JR	NZ,UPD_DO
	JR	NC,ERROR_C_J
	LD	BC,$0117	; string addition with priority 1
	JR	U_NEXT

OLDOPR:	RST	$28
	DEFW	X007B		; LD A,(HL) in ROM1
	LD	C,A		; operator code in C
	LD	B,1		; lowest possible priority in B
	LD	HL,L2734	; S-LOOP
	PUSH	HL
	JR	REPSW1

UPD_DO:	POP	BC		; discard marker, B=0
	LD	C,(HL)
;;	LD	B,0
	ADD	HL,BC
	LD	BC,L2D2B + 4	; STACK-BC
	PUSH	BC
	PUSH	HL
	RST	$20		; advance
	RST	$28
	DEFW	L24FB + 1	; SCANNING + 1
	POP	HL
	JP	(HL)

LOCAL_S:RST	$28
	DEFW	L2C8D		; ALPHA
	JR	NC,ERROR_C_J
	RST	$20
	CP	"$"
	LD	B,$40
	JR	NZ,LOCAL_N
	LD	B,$00
	RST	$20
LOCAL_N:CP	"("
	JR	NZ,LOCAL_I
	LD	C,$A1
	RST	$28
	DEFW	L2996
LOCAL_E:LD	HL,L1BEE + 5	; CHECK_END + 5; TODO: array initializer?
	PUSH	HL
	JP	SWAP
LOCAL_I:CP	"="
	JR	NZ,LOCAL_E
	RST	$20
	PUSH	BC
	RST	$28
	DEFW	L24FB + 1	; SCANNING + 1
	LD	A,(FLAGS)
	POP	BC
	XOR	B
	AND	$40
	JR	NZ,ERROR_C_J	; type mismatch
	JR	LOCAL_E

LOCAL:	CALL	SYNTAX_Z
	JR	Z,LOCAL_S
	AND	$1F
	OR	$60		; assume simple numeric
	LD	C,A
	RST	$20
	CP	"$"
	JR	NZ,LCL_N
	RES	5,C
	RST	$20
LCL_N:	CP	"("
	JR	Z,LCL_A
	CALL	LOOK_LC
	JR	C,LCL_F
	RST	$18
	CP	"="
	JR	NZ,LCL_E
	PUSH	BC
	RST	$20
	RST	$28
	DEFW	L24FB + 1	; SCANNING + 1
	POP	BC
	POP	DE		; return address
	POP	HL		; error address
	EXX
	RST	$28
	DEFW	L2BF1		; STK-FETCH
	BIT	6,(IY+$01)	; type
	JR	NZ,LCL_NX
	LD	(STRLEN),BC
	PUSH	DE
	RST	$28
	DEFW	L1F05		; TEST-ROOM
	POP	DE
	LD	HL,$0000
	ADD	HL,SP
	AND	A
	SBC	HL,BC
	LD	SP,HL
	EX	DE,HL
	LD	A,B
	OR	C
	JR	Z,LCL_Z
	LDIR
LCL_Z:	LD	BC,(STRLEN)
	PUSH	BC
	INC	BC
	INC	BC
	PUSH	BC
	JR	LCL_EXX

LCL_A:	call	ERROR
	defb	$02		; 3 Subscript wrong

LCL_NX:	PUSH	BC
	PUSH	DE
	PUSH	AF
	JR	LCL_EX

LCL_E:	POP	DE		; return address
	POP	HL		; error address
	BIT	5,C
	EXX
	LD	HL,0
	PUSH	HL
	PUSH	HL
	JR	NZ,LCL_EN
	LD	H,$03
LCL_EN:	PUSH	HL
LCL_EX:	INC	SP
LCL_EXX:EXX
	LD	B,$3E		; marker
	PUSH	BC
	PUSH	HL		; error address
	LD	(ERR_SP),SP
	PUSH	DE		; return address
LCL_SW:	JP	SWAP

LCL_F:	RST	$18
	CP	"="
	JR	NZ,LCL_SW
	RST	$20
	CALL	SKIPEX
	DEC	HL
	LD	(CH_ADD),HL
	JR	LCL_SW

STACKQ:	POP	DE		; discard STACKE
	JR	LCL_SW

STACK:	LD	HL,(ERR_SP)
	INC	HL
	INC	HL		; skip error address
STACKL:	LD	DE,STACKE
	PUSH	DE
	SET	0,(IY+$01)	; suppress leading spaces
	LD	A,$3E
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	CP	B
	INC	HL
	JR	NZ,ST_GOSUB
	LD	A,C
	OR	A
	JR	Z,STACKQ
	ADD	A,A
	JR	NC,ST_NFOR	; not FOR variable
	LD	A,FOR_T
	RST	$10
	CALL	ST_NUM
	LD	A,TO_T
	RST	$10
	CALL	ST_NUMV
	LD	A,STEP_T
	RST	$10
	CALL	ST_NUMV
	LD	A," "
	RST	$10
	CALL	ST_SMTV
	INC	HL
	INC	HL
	INC	HL
	INC	HL		; skip destination cache
	RET
STACKE:	LD	A,$0D
	RST	$10
	JR	STACKL

ST_NFOR:CP	REPEAT_M * 2
	JR	C,ST_VAR
	LD	A,REPEAT_T	; TODO: other contexts
	RST	$10
	INC	HL
	INC	HL
	INC	HL
	INC	HL		; skip destination cache
ST_SMTV:LD	C,(HL)
	INC	HL
	LD	B,(HL)
	INC	HL
ST_STMT:LD	A,AT_T
	RST	$10
	RST	$28
	DEFW	L1A1B		; OUT-NUM-1
	LD	A,":"
	RST	$10
	LD	A,(HL)
	DEC	A
	CALL	DECBYTE
	INC	HL
	RET

ST_GOSUB:
	LD	A,GOSUB_T
	RST	$10
	JR	ST_STMT

ST_VAR:	LD	A,LOCAL_T
	RST	$10
	LD	A,C
	AND	$20
	JR	Z,ST_STR
ST_NUM:	CALL	ST_VARN
	LD	A,"="
	RST	$10
ST_NUMV:RST	$28
	DEFW	L33B4		; STACK-NUM
	PUSH	HL
	RST	$28
	DEFW	L2DE3		; PRINT-FP
	POP	HL
	RET

ST_STR:	CALL	ST_VARN
	LD	A,"$"
	RST	$10
	LD	A,"="
	RST	$10
	LD	A,"\""
	RST	$10
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	DEC	DE
	DEC	DE		; max length in DE
	LD	C,(HL)
	INC	HL
	LD	B,(HL)		; length in BC
	EX	DE,HL
	AND	A
	SBC	HL,BC
	PUSH	HL		; stack remainder
	EX	DE,HL
	INC	HL
	EX	DE,HL
	LD	HL,5		; print at most 5 characters
	SBC	HL,BC
	EX	DE,HL
;;	JR	C,ST_LNG	; long string
	
ST_STRL:LD	A,B
	OR	C
	JR	NZ,ST_NXT
	LD	A,"\""
	RST	$10
	POP	DE
	ADD	HL,DE		; skip reserved space
	RET
ST_NXT:	LD	A,(HL)
	CP	$18
	JR	C,ST_CTRL	; control character
	RST	$10
	CP	"\""
	CALL	Z,$0010
ST_NXT0:DEC	BC
	INC	HL
	JR	ST_STRL
ST_CTRL:LD	A,"\""
	RST	$10
	LD	A,"+"
	RST	$10
	LD	A,CHR_T
	RST	$10
	LD	A,(HL)
	CALL	DECBYTE
	LD	A,"+"
	RST	$10
	LD	A,"\""
	RST	$10
	JR	ST_NXT0

ST_VARN:LD	A,C
	AND	$1F		; bottom 5 bits
	OR	"a"-1
	RST	$10
	RET

PLAY:
; unimplemented instruction, reports error, if executed
PLUG:	JP	ERROR_C

; IF structure table
T_IF:	DEFB	ELSE_T
	DEFB	F_ELSE - $
	DEFB	ENDIF_T
	DEFB	F_ENDIF - $
	DEFB	IF_T
	DEFB	F_IF - $
	DEFB	0

F_ELSE:	LD	A,(NESTING)
	DEC	A
	JR	Z,F_ELSER
	RST	$20
	CP	$0D			; end-of-line ELSE, no implicit END IF
	JR	Z,EACH_COMEBACK
	INC	(IY+NEST2-ERR_NR)	; implicit END IF
	JR	EACH_COMEBACK

F_ENDIF:DEC	(IY+NESTING-ERR_NR)
	JR	NZ,EACH_COMEBACK
F_ELSER:EX	DE,HL
	POP	DE
	POP	BC
	RET

; Very similar to LOOK-PROG (L1D86) but structured, with keyword behavior
; defined by index table in DE

LOOK_PROG2:
	LD	HL,$0001
	LD	(NESTING),HL
	LD	HL,(PPC)
	LD	(NEWPPC),HL
	LD	A,(SUBPPC)
	LD	(NSPPC),A
	RST	$18
	CP	":"
	JR	Z,LOOK_P2
LOOK_P1:INC	HL
	LD	A,(HL)
	AND	$C0
	SCF
	RET	NZ
	LD	B,(HL)
	INC	HL
	LD	C,(HL)
	LD	(NEWPPC),BC
	INC	HL
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	PUSH	HL
	ADD	HL,BC
	LD	B,H
	LD	C,L
	POP	HL
	XOR	A
	LD	(NSPPC),A
LOOK_P2:PUSH	BC

; Inlined EACH-STMT
	LD	(CH_ADD),HL
	LD	BC,$0000
EACH_1:	INC	(IY+NSPPC-ERR_NR)
	JP	M,ERROR_C	; TODO: too many statements
	LD	A,C
	EX	AF,AF'
	RST	$20
	LD	C,A
	PUSH	DE
	EX	DE,HL
	CALL	INDEXER
	JR	NC,EACH_COMEBACK
	LD	C,(HL)
	ADD	HL,BC
	JP	(HL)		; jump to appropriate routine
F_IF:	INC	(IY+NESTING-ERR_NR)
EACH_COMEBACK:
	EX	DE,HL
EACH_COMEBACK2:
	EX	AF,AF'
	LD	C,A
	POP	DE
EACH_2:	INC	HL
	LD	A,(HL)
EACH_3:	CALL	SKIP_NUM
	LD	(CH_ADD),HL
	CP	"\""
	JR	NZ,EACH_4
	DEC	C
EACH_4:	CP	":"
	JR	Z,EACH_5
	CP	$CB		; THEN
	JR	NZ,EACH_6
	INC	(IY+NEST2-ERR_NR)
EACH_5:	BIT	0,C
	JR	Z,EACH_1
EACH_6:	CP	$0D
	JR	NZ,EACH_2
	LD	A,(NESTING)
	SUB	A,(IY+NEST2-ERR_NR)
	LD	(NESTING),A
	XOR	A
	LD	(NEST2),A
EACH_7:	INC	(IY+$0A)
	POP	BC
	JP	LOOK_P1

SKIP_NUM:
	CP	$0
	RET	NZ
	INC	HL
	INC	HL
	INC	HL
	INC	HL
	INC	HL
	INC	HL
	LD	A,(HL)
	RET

; Handling argumentless NEXT
LV_CONT:LD	A,(T_ADDR)
	CP	$99		; interpreting NEXT?
	JP	NZ,SWAP		; return, if not
	RST	$18		; get the character following NEXT
	CP	$0D		; CR?
	JR	Z,NEXT		; if so, it's an argumentless NEXT
	CP	":"		; colon?
	JP	NZ,SWAP		; return, if not
NEXT:	POP	BC
	POP	BC
	POP	BC		; discard return addresses
	CALL	SYNTAX_Z
	JR	Z,NEXT_SW
	CALL	SKIP_LC
	ADD	A,A
	JR	NC,ERROR_1
	PUSH	HL
	RST	$28
	DEFW	X1DB9		; execute NEXT
	LD	HL,MEMBOT
	LD	(MEM),HL	; much faster than RST $28:DEFW X16CB, not worth 3 bytes
	POP	HL
	JP	NC,SWAP		; execute loop body again
	LD	BC,$0017	; jump over loop variable
	ADD	HL,BC
	POP	BC		; save return address
	POP	DE		; save error address
	LD	SP,HL
	PUSH	DE		; restore error address
	LD	(ERR_SP),SP
	PUSH	BC		; restore return address
	JP	SWAP
NEXT_SW:POP	BC		; discard one more return address
	JP	SWAP

ERROR_1:CALL	ERROR
	DEFB	$00		; 1 NEXT without FOR
