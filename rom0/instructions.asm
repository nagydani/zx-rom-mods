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
	DEFB	P_RENUM - $	; RENUM
	DEFB	P_DEFPROC - $	; DEF PROC
	DEFB	P_PLUG - $	; Es8
	DEFB	P_STACK - $	; STACK
	DEFB	P_LABEL - $	; LABEL/@
	DEFB	P_PLUG - $	; sI
	DEFB	P_PLAY - $	; PLAY
	DEFB	P_PLUG - $	; EsJ
	DEFB	P_PLUG - $	; EI
	DEFB	P_PLUG - $	; EJ
	DEFB	P_PLUG - $	; EK
	DEFB	P_PLUG - $	; EQ
	DEFB	P_ENDWHILE - $	; END WHILE
	DEFB	P_PLUG - $	; EE
	DEFB	P_PLUG - $	; EsQ
	DEFB	P_PLUG - $	; EsW
	DEFB	P_PLUG - $	; EsE
	DEFB	P_LOCAL - $	; LOCAL
	DEFB	P_DELETE - $	; DELETE
	DEFB	P_REPEAT - $	; REPEAT
	DEFB	P_PLUG - $	; EH
	DEFB	P_PLUG - $	; EF
	DEFB	P_PLUG - $	; EG
	DEFB	P_POKE - $	; POKE
	DEFB	P_PLUG - $	; EsI
	DEFB	P_USR - $	; EL
	DEFB	P_PLUG - $	; EY
	DEFB	P_UNTIL - $	; UNTIL
	DEFB	P_ASSERT - $	; ASSERT
	DEFB	P_PLUG - $	; EB
	DEFB	P_ENDIF - $	; END IF
	DEFB	P_PLUG - $	; sY
	DEFB	P_PALETTE - $	; PALETTE
	DEFB	P_PLUG - $	; sE
	DEFB	P_WHILE - $	; WHILE
	DEFB	P_ENDPROC - $	; END PROC
	DEFB	P_ELSE - $	; ELSE
	DEFB	P_PROC - $	; PROC
	DEFB	P_STEP - $	; STEP
P_END:	EQU	$

; No parameters, no action
P_ENDIF:DEFB	$00
	DEFW	ENDIF

P_RENUM:DEFB	$00		; TODO: all sorts of arguments for RENUM
	DEFW	RENUM

P_ELSE:	DEFB	$05
	DEFW	ELSE

P_POKE:	DEFB	$06		; numeric expression
	DEFB	","
	DEFB	$05		; list of items
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

P_LABEL:DEFB	$0A, $00	; just a label
	DEFW	LABEL

P_PROC:	DEFB	$05
	DEFW	PROC

P_DEFPROC:
	DEFB	$0A		; label
	DEFB	"("
	DEFB	$05		; list of arguments
	DEFW	DEFPROC

P_ENDPROC:
	DEFB	$00
	DEFW	ENDPROC

P_LOCAL:DEFB	$05
	DEFW	LOCAL

P_PALETTE:
	DEFB	$05
	DEFW	PALETTE

P_ASSERT:
	DEFB	$06, $00
	DEFW	ASSERT

P_STEP:	DEFB	$05
	DEFW	STEP

P_DELETE:
	DEFB	$05
	DEFW	DELETE

P_WHILE:DEFB	$05
	DEFW	WHILE

P_ENDWHILE:
	DEFB	$00
	DEFW	ENDWHILE

P_PLAY:
; unimplemented instruction, accepted w/o parameters, but not executed
P_PLUG:
	DEFB	$00
	DEFW	PLUG

CHECK_END:
	CALL	SYNTAX_Z
	RET	NZ
	POP	BC		; SCAN_LOOP
END05:	POP	BC		; STMT_RET
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
	LD	A,$C0
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
	DEFB	CLASS2_09 - $	; interval
	DEFB	CLASS2_0A - $	; label

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
	RET

CLASS2_02:
	RST	$28
	DEFW	L1C4E		; CLASS_02
	RET

CLASS2_04:
	RST	$28
	DEFW	L1C6C		; CLASS_04
	RET

NEXT_2NUM:
	RST	$20

CLASS2_08:
	CALL	CLASS2_06
	CP	","
	JR	NZ,ERROR_C_I
NEXT_1NUM:
	RST	$20

CLASS2_06:
	RST	$28
	DEFW	L24FB		; SCANNING
	BIT	6,(IY+$01)
	RET	NZ

ERROR_C_I:
	JP	ERROR_C

CLASS2_09:
	RST	$18
	CP	TO_T
	JR	Z,FROM_1
	CALL	CLASS2_06	; beginning
	CP	TO_T
	JR	NZ,TO_SAME
FROM_1R:RST	$20
	LD	HL,DELIM
	LD	BC,DELIM_E - DELIM
	CPIR
	JR	NZ,CLASS2_06
	CALL	SYNTAX_Z
	RET	Z
	RST	$28
	DEFW	L33A9		; TEST-5-SP
	LD	DE,(STKEND)
	LD	HL,CM1
	LD	BC,$0005
	LDIR			; STK-MINUS-ONE
	LD	(STKEND),DE
	RET

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

FROM_1:	CALL	SYNTAX_Z
	JR	Z,FROM_1R
	RST	$28
	DEFW	L1CE6 + 4	; USE-ZERO + 4
	INC	HL
	INC	HL
	LD	(HL),$01	; ONE
	JR	FROM_1R

TO_SAME:CALL	SYNTAX_Z
	RET	Z
	RST	$28
	DEFW	L35BF		; STK-PNTRS
	RST	$28
	DEFW	L33C0		; DUP
	LD	(STKEND),DE
	RET

DELIM:	DEFB	$0D
	DEFM	":,;)"
DELIM_E:


CLASS2_0A:
	CALL	SYNTAX_Z
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
	LD	(CH_ADD),HL
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
	RET

LABEL_R:INC	HL
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	ADD	HL,DE
	LD	(CH_ADD),HL
	RET


NESTING:EQU	TSTACK - 2
NEST2:	EQU	NESTING + 1

; instruction routines
ENDIF:	RES	4,(IY+$37)	; signal true outcome
	JP	SWAP

; Skip FOR block if condition unsatisfied
SKIP_FOR_CONT:
	POP	DE		; discard return address
	POP	DE		; discard return address
	POP	DE		; discard variable name
	LD	DE,T_FOR
	CALL	LOOK_PROG2
	INC	BC		; increment end-of-line pointer
	LD	(NXTLIN),BC
	JP	NC,SWAP
ERROR_I:RST	$28
	DEFW	L1D84		; I FOR without NEXT

THENLESS:
	RES	4,(IY+$37)	; signal true outcome
	CALL	TEST_ZERO
	LD	(STKEND),HL
SWAPNZ:	JP	NZ,SWAP		; Upon true condition, simply continue

; Upon false condition start scanning for END IF, ELSE or end of code
	SET	4,(IY+$37)	; signal false outcome
	LD	BC,(NXTLIN)
	LD	DE,T_IF
	CALL	LOOK_PROG2
	INC	BC		; increment end-of-line pointer
	LD	(NXTLIN),BC
	JR	C,ERROR_S
	POP	BC		; discard SCAN-LOOP
	RST	$20
	JP	SWAP

ELSE:	POP	BC		; discard STMT-RET
	CALL	SYNTAX_Z
	JR	Z,ELSE_S
	BIT	4,(IY+$37)	; FLAGX, check if last IF was false
	RES	4,(IY+$37)
	JR	NZ,ELSE_1
	RST	$18
	CP	$0D
	SCF
	JR	Z,ELSE_3	; multi-line ELSE block
	CP	IF_T
	JR	Z,ELSEIF	; ELSE IF
ELSE_0:	LD	BC,L1BB3	; LINE-END
	JR	ELSE_2
ELSE_S:	BIT	4,(IY+$37)	; after THEN
	JP	NZ,ERROR_C	; ELSE is an error
ELSE_1:	LD	BC,L1B29	; STMT-L-1
ELSE_2:	PUSH	BC
	JP	SWAP
ELSE_3:	PUSH	BC		; put back STMT-RET
	LD	BC,(NXTLIN)
	LD	DE,T_IF
	CALL	LOOK_PROG2
	LD	(NXTLIN),BC
	JR	C,ERROR_S	; missing END IF
	RST	$20
	JP	SWAP

ERROR_S:CALL	ERROR
	DEFB	$1B		; S Missing END

ELSEIF:	INC	HL
	PUSH	BC
	CALL	SKIPEX
	POP	BC
	CP	THEN_T
	JR	Z,ELSE_0
	DEC	HL
	LD	(CH_ADD),HL
	JR	ELSE_3

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
LABEL:
POKE_SWAP:
	JP	SWAP

POKE_S:	LD	HL,L1E2C	; DATA-1
POKE_E:	PUSH	HL
	JR	POKE_SWAP

USR:	CALL	SYNTAX_Z
	JR	Z,POKE_SWAP
	RST	$28
	DEFW	L1E99		; FIND-INT2
	PUSH	BC
	JR	POKE_SWAP

REPEAT:	POP	DE		; DE = return address
	LD	HL,(SUBPPC - 1)
	INC	H
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
REPSW:	JP	SWAP

TEST_ZERO:
	LD	HL,(STKEND)
	DEC	HL
	XOR	A
	DEC	HL
	OR	(HL)
	DEC	HL
	OR	(HL)
	DEC	HL
	DEC	HL
	OR	(HL)		; zero only for small integers
	RET

ASSERT:	CALL	TEST_ZERO
	JR	NZ,REPSW
ERROR_V:CALL	ERROR
	DEFB	$1E		; V ASSERT failed

ERROR_W:CALL	ERROR
	DEFB	$1F		; W END WHILE without WHILE

ENDWHILE:
	CALL	SKIP_LL
	CP	WHILE_M
	JR	NZ,ERROR_W	; wrong context
	PUSH	HL		; context
	LD	DE,(CH_ADD)	; execution pointer
	PUSH	DE
	DEC	HL
	DEC	HL		; skip SUBPPC
	DEC	HL
	DEC	HL		; skip PPC
	LD	D,(HL)
	DEC	HL
	LD	E,(HL)		; condition in DE; TODO: check no caching
	LD	HL,(PROG)
	ADD	HL,DE
	LD	(CH_ADD),HL
	RST	$28
	DEFW	L24FB		; SCANNING
	CALL	TEST_ZERO
	POP	HL		; old execution pointer
	JR	Z,WEND
	POP	HL		; context
	DEC	HL
	LD	DE,SUBPPC
	LDD
	LDD
	LDD			; copy PPC and SUBPPC from context
	DEC	HL
	DEC	HL		; skip CH_ADD in context
	LD	D,(HL)
	DEC	HL
	LD	E,(HL)
	LD	HL,(PROG)
	ADD	HL,DE
	LD	(NXTLIN),HL	; copy NXTLIN from context
	JP	SWAP
WEND:	LD	(CH_ADD),HL
	POP	HL		; context
	POP	BC		; return address
	POP	DE		; error address
	LD	SP,HL		; reclaim locals
	JR	UNT_ER

UNTIL:	CALL	TEST_ZERO
	EX	AF,AF'
	POP	BC		; return address
	POP	HL		; error address
	POP	DE		; marker
	LD	A,D
	CP	$3E
	JR	NZ,ERROR_U	; after GO SUB
	LD	A,E
	CP	REPEAT_M
	JR	Z,UNT_NL	; no local variables
	PUSH	DE		; put back marker
	EXX
	CALL	SKIP_LL
	CP	REPEAT_M
	EXX
	JR	NZ,ERROR2U	; wrong context
	EX	AF,AF'
	JR	NZ,UNT_R	; reclaim local context
	PUSH	HL		; error address
	PUSH	BC		; return address
	EXX
	AND	A
	SBC	HL,DE
	INC	HL		; skip marker
	JR	UNT_C		; continue loop

UNT_R:	EXX
	LD	SP,HL		; reclaim local variables
	EXX
	EX	DE,HL
	JR	UNT_ER		; continue after UNTIL

UNT_NL:	EX	AF,AF'
	JR	NZ,END_REP
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
UNTSW:	JP	SWAP
END_REP:EX	DE,HL
	LD	HL,$0007
	ADD	HL,SP
	LD	SP,HL
UNT_ER:	PUSH	DE
UNT_E:	LD	(ERR_SP),SP
	PUSH	BC
	JR	UNTSW

ERROR_U:PUSH	DE
ERROR2U:PUSH	HL
	CALL	ERROR
	DEFB	$1D		; U UNTIL without REPEAT

ERROR_2:RST	$28
	DEFW	L0670		; 2 Variable not found

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
REPSW1:	JR      UNTSW

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

DEFPROC:RST	$18
	CP	")"
	JR	Z,DP_E
DP_L:	CP	REF_T
	JR	NZ,DP_REF
	RST	$20
DP_REF:	RST	$28
	DEFW	L2C8D		; ALPHA
	JR	NC,ERROR_C_J
	RST	$20
	CP	"$"
	JR	NZ,DP_NUM
	RST	$20
DP_NUM:	CP	","
	JR	NZ,DP_E
	RST	$20
	JR	DP_L

DP_E:	CP	")"
ERRC_NZ:JR	NZ,ERROR_C_J
	RST	$20
	CALL	SYNTAX_Z
	JR	Z,SW_LOC
	LD	DE,T_DP
SKIPEND:CALL	LOOK_PROG2
	INC	BC
	LD	(NXTLIN),BC
	JP	C,ERROR_S	; S Missing END (PROC)
	INC	(IY+$0A)	; advance NSPPC past END PROC
	JR	SW_LOC

LOCAL_R:RST	$20		; advance past the comma
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
	DEFW	L2996		; STK-VAR
LOCAL_E:RST	$18		; TODO: ???
	CP	","
	JR	Z,LOCAL_R
	LD	HL,L1BEE + 5	; CHECK_END + 5; TODO: array initializer?
	PUSH	HL
SW_LOC:	JP	SWAP

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
	JR	NZ,ERRC_NZ	; type mismatch
	JR	LOCAL_E

LOCAL:	CALL	SYNTAX_Z
	JR	Z,LOCAL_S
LOCAL_L:AND	$1F
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
LCL_L:	RST	$18
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
	CALL	LCL_STR
	LD	BC,(STRLEN)
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
LCL_CM:	RST	$18
	CP	","
	JP	NZ,SWAP
	RST	$20
	JR	LOCAL_L


LCL_F:	DEC	HL
	BIT	7,(HL)
	JR	NZ,LCL_L
	RST	$18
	CP	"="
	JR	NZ,LCL_CM
	RST	$20
	CALL	SKIPEX
	DEC	HL
	LD	(CH_ADD),HL
	JR	LCL_CM

LCL_STR:LD	(STRLEN),BC
	PUSH	DE
	RST	$28
	DEFW	L1F05		; TEST-ROOM
	POP	DE
	LD	HL,$0002
	ADD	HL,SP
	AND	A
	SBC	HL,BC
	POP	BC
	LD	SP,HL
	PUSH	BC
	EX	DE,HL
	LD	BC,(STRLEN)
	LD	A,B
	OR	C
	RET	Z
	LDIR
	RET

STACKQ:	POP	DE		; discard STACKE
STCK_SW:JP	SWAP

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


ST_REP:	LD	A,REPEAT_T
	JR	ST_CTX
ST_WHL:	LD	A,WHILE_T
	JR	ST_CTX

ST_NFOR:CP	REPEAT_M * 2
	JR	C,ST_VAR
	JR	Z,ST_REP
	CP	WHILE_M * 2
	JR	Z,ST_WHL
	LD	A,PROC_T
ST_CTX:	RST	$10
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


; WHILE structure table
T_WHILE:DEFB	ENDWHILE_T
	DEFB	F_ENDWHILE - $
	DEFB	WHILE_T
	DEFB	F_WHILE - $
	DEFB	0

; DEF PROC structure table
T_DP:	DEFB	ENDPROC_T
	DEFB	F_ENDPROC - $
	DEFB	DEFPROC_T
	DEFB	F_DEFPROC - $
	DEFB	0

; FOR structure table
T_FOR:	DEFB	NEXT_T
	DEFB	F_NEXT - $
	DEFB	FOR_T
	DEFB	F_FOR - $
	DEFB	0

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

F_NEXT:	RST	$20
	CP	$0D
	JR	Z,F_ENDIF
	CP	":"
	JR	Z,F_ENDIF
; Consider variable
	OR	$20			; lowercase
	CP	(IY+$38)
	JR	NZ,EACH_COMEBACK
	INC	(IY+$0A)		; increment NSPPC to skip update
	JR	F_ELSER			; for backwards compatibility
F_ENDWHILE:
F_ENDPROC:
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
LOOK_PROG2_0:
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
F_WHILE:
F_DEFPROC:
F_FOR:
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
	CP	THEN_T		; THEN
	JR	NZ,EACH_6
	EX	DE,HL
	CP	(HL)
	EX	DE,HL
	JR	NZ,EACH_5
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
	CP	$0E
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

ERROR_1:RST	$28
	DEFW	REPORT1 + 3	; 1 NEXT without FOR

PALETTE:CP	INK_T
	JR	Z,PAL_I
	CP	PAPER_T
	JR	Z,PAL_P
	CP	TO_T
	JR	Z,PAL_T
	RST	$28
	DEFW	L24FB + 1	; SCANNING + 1
	CALL	SYNTAX_Z
	JR	Z,PAL_SW
	RST	$28
	DEFW	L1E94		; FIND-INT1
	CP	5
ERRBNC:	JP	NC,ERROR_B	; B Integer out of range
	LD	(S_MODE),A
	LD	C,A
	LD	B,0
	LD	HL,SWAP
	PUSH	HL
	LD	HL,PAL_J
	ADD	HL,BC
	LD	C,(HL)
	ADD	HL,BC
	JP	(HL)

PAL_T:	CALL	PAL_S
	JR	C,PAL_SW
	PUSH	AF
	RST	$28
	DEFW	L1E94		; FIND-INT1
	CP	$40
	JR	NC,ERRBNC
	LD	BC,$BF3B
	OUT	(C),A
	LD	B,$FF
	POP	AF
	OUT	(C),A
PAL_SW:	JP	SWAP

PAL_P:	SET	6,(IY+$37)
	JR	PAL_ST
PAL_I:	RES	6,(IY+$37)
PAL_ST:	CALL	PAL_S
	JR	C,PAL_SW
	PUSH	AF
	LD	A,(S_MODE)
	OR	A
	JR	Z,ERROR_K
	LD	C,A
	LD	B,0
	LD	HL,INKMAX-1
	BIT	6,(IY+$37)
	JR	Z,PAL_NP
	LD	HL,PAPMAX-1
PAL_NP:	ADD	HL,BC
	PUSH	HL
	RST	$28
	DEFW	L1E94		; FIND-INT1
	POP	HL
	CP	(HL)
	JR	NC,ERROR_K
	LD	B,A
	AND	$18
	ADD	A,A
	LD	C,A
	LD	A,B
	AND	$07
	BIT	6,(IY+$37)
	JR	Z,PAL_NN
	OR	$08
PAL_NN:	OR	C
	POP	DE
PAL_SL:	LD	BC,$BF3B
	OUT	(C),A
	LD	B,$FF
	OUT	(C),D
	ADD	A,(HL)
	ADD	A,(HL)
	CP	$40
	JR	C,PAL_SL
	JP	SWAP

ERROR_K:RST	$08
	DEFW	L2244		; K Invalid colour

PAL_S:	CALL	NEXT_1NUM
	CP	";"
PERRCNZ:JP	NZ,ERROR_C
	CALL	NEXT_1NUM
	CP	","
	JR	NZ,PERRCNZ
	CALL	NEXT_2NUM
	CP	":"
	JP	Z,PAL_IC
	CP	$0D
	JR	NZ,PERRCNZ
PAL_IC:	CALL	SYNTAX_Z
	SCF
	RET	Z

PAL_RGB:CALL	BYTEFP		; BLUE
	RLCA
	RLCA
	AND	$03
	PUSH	AF
	CALL	BYTEFP		; GREEN
	AND	$E0
	PUSH	AF
	CALL	BYTEFP		; RED
	RRCA
	RRCA
	RRCA
	AND	$1C
	POP	BC
	OR	B
	POP	BC
	OR	B
	RET

BYTEFP:	RST	$28
	DEFW	L35BF		; STK-PNTRS
	RST	$28
	DEFW	L3297		; RE-STACK
	LD	A,(HL)
	ADD	8		; multiply by 256
	CP	$89
	JR	NC,BYTEFF
	LD	(HL),A
	INC	HL
	LD	A,(HL)
	ADD	A,A
	JR	C,BYTE00
	RST	$28
	DEFW	L2DD5		; FP-TO-A
	RET

BYTEFF:	INC	HL
	LD	A,(HL)
	ADD	A,A
BYTE00:	CCF
	SBC	A,A
	DEC	HL
	LD	(STKEND),HL
	RET

INKMAX:	DEFB	8,32,16,8
PAPMAX:	DEFB	32,8,16,8

PAL_J:	DEFB	PAL_0 - $
	DEFB	PAL_1 - $
	DEFB	PAL_2 - $
	DEFB	PAL_3 - $
	DEFB	PAL_4 - $

PAL_0:	XOR	A
	JR	PAL_X		; TODO: better ways of skipping 2 bytes

PAL_1:	CALL	PALSUP
	JR	PALL

PAL_2:	CALL	PALSUP
	EXX
PALL:	LD	BC,$BF3B
	OUT	(C),E
	BIT	3,E
	JR	NZ,PALPS
	LD	A,E
	CALL	PAL_M
	JR	PALS
PALPS:	LD	A,E
	EXX
	CALL	PAL_M
	EXX
PALS:	LD	BC,$FF3B
	OUT	(C),A
	DEC	E
	JR	NZ,PALL
PAL_ON:	LD	A,1
PAL_X:	LD	BC,$BF3B
	LD	D,$40
	OUT	(C),D
	LD	B,$FF
	OUT	(C),A
	RET

PAL_3:	LD	E,$00
	LD	HL,COLTAB
	CALL	PAL_34		; 00 INK
	LD	HL,COLTAB
	CALL	PAL_34		; 00 PAPER
	CALL	PAL_34		; 01 INK
	LD	HL,COLTAB
	CALL	PAL_34		; 01 PAPER
	LD	HL,COLTAB
	CALL	PAL_34		; 10 INK
	CALL	PAL_34		; 10 PAPER
	LD	HL,COLTABB
	CALL	PAL_34		; 11 INK
	LD	HL,COLTABB
	CALL	PAL_34		; 11 PAPER
	JR	PAL_ON

PAL_4:	LD	E,$00
	LD	HL,COLTAB
	CALL	PAL_34		; 00 INK
	LD	HL,COLTAB
	CALL	PAL_34		; 00 PAPER
	CALL	PAL_34		; 01 INK
	LD	HL,COLTABB
	CALL	PAL_34		; 01 PAPER
	LD	HL,COLTAB
	CALL	PAL_34		; 10 INK
	CALL	PAL_34		; 10 PAPER
	LD	HL,COLTABB
	CALL	PAL_34		; 11 INK
	LD	HL,COLTAB
	CALL	PAL_34		; 11 PAPER
	JR	PAL_ON

PAL_34:	LD	D,8
PAL_LL:	LD	BC,$BF3B
	OUT	(C),E
	INC	E
	LD	B,$FF
	LD	A,(HL)
	INC	HL
	OUT	(C),A
	DEC	D
	JR	NZ,PAL_LL
	RET

PALSUP:	LD	HL,COLTAB
	LD	D,$37
	EXX
	LD	HL,COLTABB
	LD	D,$07
	LD	E,$3F
	RET

PAL_M:	AND	D
	LD	C,A
	AND	7
	LD	B,A
	LD	A,C
	RRA
	AND	$18
	OR	B
	LD	C,A
	LD	B,0
	ADD	HL,BC
	LD	A,(HL)
	SBC	HL,BC
	RET

COLTAB:	DEFB	$00		; BLACK
	DEFB	$02		; BLUE
	DEFB	$14		; RED
	DEFB	$16		; MAGENTA
	DEFB	$A0		; GREEN
	DEFB	$A2		; CYAN
	DEFB	$B4		; YELLOW
	DEFB	$B6		; WHITE

COLTABB:DEFB	$00		; BLACK
	DEFB	$03		; BRIGHT BLUE
	DEFB	$1C		; BRIGHT RED
	DEFB	$1F		; BRIGHT MAGENTA
	DEFB	$E0		; BRIGHT GREEN
	DEFB	$E3		; BRIGHT CYAN
	DEFB	$FC		; BRIGHT YELLOW
	DEFB	$FF		; BRIGHT WHITE

	DEFB	$17		; VIOLET
	DEFB	$1E		; PURPLE
	DEFB	$A3		; SKY
	DEFB	$B7		; STEEL
	DEFB	$BC		; ORANGE
	DEFB	$BE		; PEACH
	DEFB	$BF		; PINK
	DEFB	$E2		; TURQUOISE

	DEFB	$F4		; GRASS
	DEFB	$F6		; CRICKET
	DEFB	$F7		; LIGHT BLUE
	DEFB	$FE		; SEPIA
	DEFB	$49		; GRAY
	DEFB	$40		; DARK GREEN
	DEFB	$2c		; BROWN
	DEFB	$28		; DARK BROWN

RENUM:	POP	BC		; return address
	POP	DE		; error address
	LD	HL,(RAMTOP)
	DEC	HL
	DEC	HL		; skip the end-of-stack marker
	LD	SP,HL		; clear the stack
	PUSH	DE		; error address
	LD	(ERR_SP),SP
	PUSH	BC		; return address
	LD	DE,$0000	; line counter
	LD	HL,(PROG)
RENUML0:LD	A,(HL)
	CP	$28
	JR	NC,RENUMC
	INC	HL
	INC	HL		; skip line number
	LD	C,(HL)
	INC	HL
	LD	B,(HL)		; get line length in BC
	SCF			; faster than INC HL
	ADC	HL,BC		; move to next line
	INC	DE
	JR	RENUML0
RENUMC:	LD	HL,(RCSTEP)
	DEC	DE
	RST	$28
	DEFW	L30A9		; HL=HL*DE
	JR	C,ERROR_G
	LD	DE,(RCSTART)
	ADC	HL,DE
	JR	C,ERROR_G
	LD	BC,10000
	SBC	HL,BC
	JR	NC,ERROR_G	; the last line number must not exceed 9999
	LD	HL,(PROG)
RENUML:	LD	A,(HL)
	CP	$28
	JR	NC,RENUME
	LD	B,(HL)
	LD	(HL),D
	INC	HL
	LD	C,(HL)
	LD	(HL),E
	LD	(RCLINE),BC
	EX	DE,HL
	LD	BC,(RCSTEP)
	ADD	HL,BC
	EX	DE,HL
	INC	HL
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	SCF			; faster than INC HL
	ADC	HL,BC		; TODO: print all problematic instructions
	JR	RENUML
RENUME:	RST	$28
	DEFW	L1BB0		; 0 Ok.

ERROR_G:CALL	ERROR
	DEFB	$0F		; G No room for line


; Step syntax check, like PRINT
STEP_S:	RST	$28
	DEFW	L1FDF		; PRINT-2
	LD	HL,L1BEE + 5	; CHECK-END + 5
	PUSH	HL
	JP	SWAP

STEP:	CALL	SYNTAX_Z
	JR	Z,STEP_S
	POP	HL		; discard return address
	LD	HL,(PPC)
	LD	(STEPPPC),HL
	LD	A,(SUBPPC)
	LD	(STEPSUB),A
	CALL	STEP_P		; print STEP arguments, catching errors
STEP_CONT:
	LD	HL,(ERR_SP)
	AND	A
	SBC	HL,SP		; Handling an error?
	JP	NZ,STEP_E	; jump, if so
	BIT	7,(IY+$0A)	; Jump to be made?
	JR	NZ,STEP_NX	; forward, if not
	LD	HL,(NEWPPC)
	BIT	7,H
	JR	Z,STEP_LN
	LD	HL,$FFFE
	LD	(PPC),HL
	LD	HL,(WORKSP)
	DEC	HL
	LD	DE,(E_LINE)
	DEC	DE
	LD	A,(NSPPC)
	JR	STEP_NL

STEP_NX:RST	$18
	CP	":"
	JR	Z,STEP_N
	CP	$0D
	JR	Z,STEP_LE
	JP	ERROR_C

STEP_LN:RST	$28
	DEFW	L196E		; LINE-ADDR
	LD	A,(NSPPC)
	JR	Z,STEP_LC
	OR	A
	JR	NZ,ERROR_N
	DEC	HL
STEP_LE:INC	HL
	LD	A,1
STEP_LC:LD	D,(HL)
	BIT	7,D
	JR	NZ,RENUME	; Program finished
	BIT	6,D
	JR	NZ,RENUME	; Program finished
	INC	HL
	LD	E,(HL)
	LD	(PPC),DE
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	EX	DE,HL
	ADD	HL,DE
	INC	HL
STEP_NL:LD	(NXTLIN),HL
	EX	DE,HL
	LD	(CH_ADD),HL
	LD	D,A
	LD	E,$00
	LD	(IY+$0A),$FF
	DEC	D
	LD	(IY+$0D),D
	JR	Z,STEP_N
	INC	D
	RST	$28
	DEFW	L198B		; EACH-STMT
	JR	NZ,ERROR_N
	RST	$18
	DEFB	$1E		; LD E, skip one byte
; STMT-LOOP
STEP_N:	RST	$20		; read next
	CP	$0D
	JR	Z,STEP_LE	; LINE-END
	INC	(IY+$0D)
	CP	":"
	JR	Z,STEP_N

;;	LD	HL,(CH_ADD)
	LD	(T_ADDR),HL	; Save execution pointer
	RST	$28
	DEFW	L16BF		; SET-WORK
	LD	HL,(STEPPPC)
	BIT	7,H
	JR	NZ,STEP_C	; STEP was a command
	RST	$28
	DEFW	L196E		; LINE-ADDR
	INC	HL
	INC	HL		; step over line number
	INC	HL
	INC	HL		; step over line length
	JR	Z,STEP_0
ERROR_N:RST	$08
	DEFW	L1BEC		; N Statement lost
STEP_C:	LD	HL,(E_LINE)
STEP_0:	DEC	HL
	LD	(CH_ADD),HL
	LD	A,(STEPSUB)
	LD	E,$00
	LD	D,A
	DEC	A
	JR	Z,STEP_1	; STEP is first statement
	RST	$28
	DEFW	L198B		; EACH-STMT
	JR	NZ,ERROR_N
	RST	$18
	CP	":"
	JR	NZ,ERROR_N
STEP_1:	RST	$20		; advance
	RST	$20		; twice
	CALL	STEP_P
	LD	HL,(PPC)
	BIT	7,H		; executing command line?
	JR	Z,STEP_2	; jump, if not
	INC	HL
	INC	HL
STEP_2:	CALL	DECWORD
	LD	A,":"
	RST	$10
	LD	A,(SUBPPC)
	CALL	DECBYTE
	LD	A," "
	RST	$10
	LD	HL,(T_ADDR)
	LD	(CH_ADD),HL	; restore execution pointer
	
	LD	A,(HL)
	RST	$10		; TODO: proper line listing
	
	LD	A,$FD		; re-open system K channel
	RST	$28
	DEFW	L1601		; CHAN-OPEN
	RES	3,(IY+$02)	; no edit line
	RST	$28
	DEFW	L15DE		; WAIT-KEY1
	PUSH	AF
	RST	$28
	DEFW	L0D6E		; CLS-LOWER
	POP	AF
	CP	" "
	JR	Z,ERROR_L	; BREAK
	CP	"c"
	JR	Z,STEP_X
	CP	"C"
	JR	Z,STEP_X
	RST	$18
	LD	B,0
	LD	HL,X1B40
	PUSH	HL
	LD	HL,STEP_HOOK
	JP	SWAP

ERROR_L:RST	$28
	DEFW	L1B7B		; L Break into program

STEP_X:	LD	HL,L1B29	; STMT-L-1
	PUSH	HL
ST_SW:	JP	SWAP

; Print STEP's arguments
STEP_P:	RST	$28
	DEFW	L0D6E		; CLS-LOWER
	LD	A,$16		; AT
	RST	$10
	XOR	A
	RST	$10
	XOR	A
	RST	$10		; AT 0,0
	POP	HL
	LD	(RETADDR),HL
	PUSH	HL
	LD	HL,(ERR_SP)
;;	LD	E,(HL)
	LD	(HL),STEP_HOOK - $100*(STEP_HOOK/$100)
	INC	HL
;;	LD	D,(HL)
	LD	(HL),STEP_HOOK/$100
	RST	$28
	DEFW	L1FDF		; PRINT-2
	LD	HL,(ERR_SP)
	LD	(HL),L1303 - $100*(L1303/$100)
	INC	HL
	LD	(HL),L1303/$100
	RET

; Print any error that might occur during STEP
STEP_E:	LD	HL,L1303
	PUSH	HL
	LD	HL,(RETADDR)
	PUSH	HL
	LD	A,(ERR_NR)
	LD	(IY+$00),$FF
	INC	A
	PUSH	AF
	RST	$28
	DEFW	L15EF		; OUT-CODE
	LD	A," "
	RST	$10
	POP	AF
	CP	$1C
	JP	NC,REPORT
STEP_EO:LD	DE,L1391
	RST	$28
	DEFW	L0C0A		; PO-MSG
	LD	A,$0D
	RST	$10
	RET

DEL_SK:	RST	$18		; TODO: ???
	CP	"("
	JR	NZ,ST_SW
	RST	$20
	CP	")"
	EX	AF,AF'
	RST	$20
	EX	AF,AF'
	JR	Z,ST_SW
	JP	ERROR_C

DEL_E:	LD	HL,(VARS)
	JR	DEL_NE

DELETE:	RST	$28
	DEFW	L2C8D		; ALPHA
	JR	NC,DEL_PR
	RST	$28
	DEFW	L28B2		; LOOK-VARS
	JR	C,DEL_SK	; return, if not found
	RL	C
	JR	C,DEL_SK	; return, if checking syntax
	SBC	HL,SP
	JR	NC,DEL_LC	; DELETE local
	ADD	HL,SP
	PUSH	HL
	RST	$18
	CP	"("
	POP	HL
	JR	Z,DEL_AR	; DELETE array
	BIT	6,C
	JR	Z,DEL_ST	; DELETE string
	LD	BC,$0006	; DELETE numeric
	BIT	7,(HL)
	JR	Z,DEL_N1	; DELETE single letter numeric
DEL_NL:	INC	BC
	DEC	HL
	BIT	7,(HL)
	JR	Z,DEL_NL
DEL_N1:	RST	$28
	DEFW	L19E8		; RECLAIM-2
DEL_SW:	JP	SWAP

DEL_AR:	PUSH	HL
	RST	$20		; skip ")"
	RST	$20
	POP	HL
DEL_ST:	INC	HL
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	DEC	HL
	DEC	HL
	INC	BC
	INC	BC
	INC	BC
	JR	DEL_N1

DEL_PR:	CALL	CLASS2_09
	CALL	SYNTAX_Z
	JR	Z,DEL_SW
	RST	$28
	DEFW	L2DA2		; FP-TO-BC
	JR	NZ,DEL_E
	JR	C,DEL_E
	LD	L,C
	LD	H,B
	RST	$28
	DEFW	L196E		; LINE-ADDR
	JR	NZ,DEL_NE
	INC	HL
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	SCF			; faster than INC HL
	ADC	HL,DE		; skip last line
DEL_NE:	PUSH	HL
	RST	$28
	DEFW	L1E99		; FIND-INT2
	LD	L,C
	LD	H,B
	RST	$28
	DEFW	L196E		; LINE-ADDR
	EX	DE,HL
	POP	HL
	RST	$28
	DEFW	L19DD		; DIFFER
	JR	C,DEL_SW
	RST	$28
	DEFW	L19E8		; RECLAIM-2
	LD	HL,(PPC)
	LD	A,(SUBPPC)
	LD	(NEWPPC),HL
	INC	A
	LD	(NSPPC),A
	JP	MAIN_ADD_CONT

DEL_LC:	LD	A,$BF
	CP	C
	DEC	HL
	JR	NC,DEL_LH
	INC	HL
	INC	HL
DEL_LH:	PUSH	HL		; save stack length
	ADD	HL,SP		; add one more for simple numeric
	LD	A,(HL)
	AND	$60
	CP	$60
	LD	E,L
	LD	D,H
	JR	Z,DEL_LF
	INC	HL
	INC	HL
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
DEL_LF0:ADD	HL,BC
	EX	DE,HL
	POP	BC
	DEC	HL
	DEC	BC
	DEC	BC
	LDDR
	INC	DE
	EX	DE,HL
	LD	SP,HL
	INC	HL
	INC	HL
	LD	(ERR_SP),HL
DELSW:	JR	DEL_SW

DEL_LF:	LD	BC,$0017
	LD	A,(HL)
	CP	$E0
	JR	NC,DEL_LF0
	LD	C,$06
	JR	DEL_LF0

PROC_S:	CALL	S_LBLI		; insert label with empty cache
	RST	$18		; TODO: ???
	CP	"("
	JR	NZ,ERR_PR
	RST	$20
	CP	")"
	JR	Z,PROC_SE
	DEFB	$3E		; LD A, skipping next byte
PROC_SL:RST	$20
	RST	$28
	DEFW	L24FB		; SCANNING
	CP	","
	JR	Z,PROC_SL
	CP	")"
ERR_PR:	JP	NZ,ERROR_C
PROC_SE:RST	$20
	JP	END05

PROC:	CALL	SYNTAX_Z
	JR	Z,PROC_S
	LD	BC,$0014
	RST	$28
	DEFW	L1F05
	POP	DE		; DE = return address
	LD	HL,(SUBPPC - 1)
	INC	H
	EX	(SP),HL		; HL = error address
	INC	SP		; stack SUBPPC (1 byte)
	LD	BC,(PPC)
	PUSH	BC		; stack PPC (2 bytes)
	PUSH	HL
	LD	HL,(NXTLIN)
	AND	A
	LD	BC,(PROG)
	SBC	HL,BC
	EX	(SP),HL		; stack NXTLIN - PROG (2 bytes)
	PUSH	HL
	LD	HL,(DATADD)
	SBC	HL,BC
	EX	(SP),HL		; stack DATADD - PROG (2 bytes)
	LD	BC,$3E00 + PROC_M
	PUSH	BC		; stack marker
	PUSH	HL		; stack error address
; tail call entry point
T_PROC:	LD	(ERR_SP),SP
	PUSH	DE		; stack return address
	RST	$18
	LD	(MEMBOT+26),HL	; label start
L_PROC:	LD	A,(HL)
	CP	$0E
	INC	HL
	JR	NZ,L_PROC
	LD	A,(HL)
	INC	HL
	OR	(HL)
	INC	HL
	OR	(HL)
	INC	HL
	OR	(HL)
	LD	(IY+MEMBOT+25-ERR_NR),DEFPROC_T
	CALL	Z,F_LBL
	LD	(DATADD),HL
	LD	B,(HL)
	DEC	HL
	LD	C,(HL)
	CALL	JP_LBL
	LD	HL,(DATADD)
PROC_L0:INC	HL
	LD	A,(HL)
	CP	"("
	JR	NZ,PROC_L0
	LD	(DATADD),HL	; opening brace for arguments
PROC_L1:INC	HL
	LD	A,(HL)
	CP	$21
	JR	NC,PROC_L1
	CP	")"
	JR	NZ,PROC_L	; non-empty PROC
	LD	(DATADD),HL	; closing brace of PROC
PROC_L:	RST	$20
	CP	")"		; no more default arguments in DEF PROC?
	JR	Z,PROC_E	; jump, if so
	CP	REF_T		; reference?
	JR	Z,PROC_R	; jump if so
	AND	$1F
	OR	$60		; assume simple numeric
	LD	C,A
	RST	$20
	CP	"$"
	JR	NZ,PROC_N	; jump if numeric indeed
	RES	5,C
	RST	$20
PROC_N:	LD	(X_PTR),HL	; save argument pointer
	LD	HL,(DATADD)
	LD	(CH_ADD),HL
	RST	$20		; skip separator
	CP	")"		; empty PROC with non-empty DEF PROC?
	JR	Z,ERROR_Q	; that is an error!
	PUSH	BC		; save variable name and type
	RST	$28
	DEFW	L24FB + 1	; SCANNING + 1
	POP	BC		; variable name and type
	POP	DE		; return address
	POP	HL		; error address
	EXX
	RST	$28
	DEFW	L2BF1		; STK-FETCH
	BIT	6,(IY+$01)	; type
	JR	NZ,PROC_NX	; jump, if numeric
	CALL	LCL_STR		; stack string
	LD	BC,(STRLEN)
	PUSH	BC
	INC	BC
	INC	BC
	PUSH	BC
	EXX
	BIT	5,C
	JR	Z,PROC_X
PROC_R:				; TODO: references
ERROR_Q:RST	$28
	DEFW	L288B		; Q Parameter error

PROC_NX:PUSH	BC
	PUSH	DE
	PUSH	AF
	INC	SP
	EXX
	BIT	5,C
	JR	Z,ERROR_Q
PROC_X:	LD	B,$3E
	PUSH	BC		; marker
	PUSH	HL		; error address
	PUSH	DE		; return address
	RST	$18		; separator of PROC arguments
	LD	(DATADD),HL
	LD	HL,(X_PTR)
	LD	(CH_ADD),HL
	INC	HL
	RST	$18		; separator in DEF PROC
	CP	","
	JR	Z,PROC_L
	POP	DE		; return address
	LD	(ERR_SP),SP
	PUSH	DE
PROC_E:	RST	$20		; skip closing bracket Of DEF PROC
PROC_EE:JP	END05


WHILE:	LD	HL,(CH_ADD)
	LD	(DEST),HL
	CALL	CLASS2_06	; single numeric expression
	CALL	SYNTAX_Z
	JR	Z,PROC_EE
	CALL	TEST_ZERO
	JR	Z,WHILE0
	POP	DE		; DE = return address
	LD	HL,(SUBPPC - 1)
	INC	H
	EX	(SP),HL		; HL = error address
	INC	SP		; stack SUBPPC (1 byte)
	LD	BC,(PPC)
	PUSH	BC		; stack PPC (2 bytes)
	PUSH	HL
	LD	HL,(DEST)
	AND	A
	LD	BC,(PROG)
	SBC	HL,BC
	EX	(SP),HL		; stack old CH_ADD - PROG (2 bytes)
	PUSH	HL
	LD	HL,(NXTLIN)
	SBC	HL,BC
	EX	(SP),HL		; stack NXTLIN - PROG (2 bytes)
	LD	BC,$3E00 + WHILE_M
	PUSH	BC		; stack marker
	PUSH	HL		; stack error address
	LD	(ERR_SP),SP
	PUSH	DE		; stack return address
	LD	BC,$0014	; why this much? see $1F02 in ROM1
	LD	HL,L1F05	; TEST-ROOM
	PUSH	HL
WHILE_E:JP	SWAP

WHILE0:	LD	DE,T_WHILE
	JP	SKIPEND

ERROR_X:CALL	ERROR
	DEFB	$20		; X END PROC without DEF

ENDPROC:CALL	SKIP_LL
	CP	REPEAT_M
	JR	Z,ENDPROC
	CP	PROC_M
	JR	NZ,ERROR_X
RETPROC:PUSH	HL		; new marker address
	DEC	HL
	LD	DE,SUBPPC
	LDD
	LDD
	LDD
	LD	B,(HL)
	DEC	HL
	LD	C,(HL)
	EX	DE,HL
	LD	HL,(PROG)
	PUSH	HL		; save PROG
	ADD	HL,BC
	LD	(NXTLIN),HL
	EX	DE,HL
	DEC	HL
	LD	B,(HL)
	DEC	HL
	LD	C,(HL)
	EX	DE,HL
	POP	HL		; PROG
	ADD	HL,BC
	EX	DE,HL
	LD	HL,(DATADD)
	LD	(DATADD),DE
	LD	A,(HL)
	CP	")"		; closing bracket of PROC
	JR	Z,ENDP_C
	INC	HL
ENDP_L:	CALL	SKIPEX
	JR	NC,ENDP_L	; skip unread arguments of PROC
	DEC	HL
ENDP_C:	LD	(CH_ADD),HL
	POP	HL		; new marker address
	POP	BC		; return address
	POP	DE		; error address
	LD	SP,HL
	PUSH	DE		; error address
	LD	(ERR_SP),SP
	PUSH	BC		; return address
	RST	$20		; advance
	DEC	(IY+$0D)	; adjust SUBPPC
ENDP_SW:JP	SWAP

; Discard local variables before RETURN
RETURN_CONT:
	PUSH	HL
	PUSH	BC
RET_L:	CALL	SKIP_LL
	OR	A
	JR	Z,ENDP_SW	; 7 RETURN without GOSUB
	CP	$3F
	JR	Z,RETURN_GS
	CP	REPEAT_M
	JR	Z,RET_L
	CP	PROC_M
	JR	NZ,ENDP_SW		; TODO: consider other contexts
; returning from a PROC
	PUSH	HL
	RST	$18			; fetch character after RETURN
	POP	HL
	CP	":"
	JR	NZ,RETPROC		; just return from PROC
	PUSH	HL
	RST	$20			; fetch instruction token after RETURN :
	POP	HL
	CP	PROC_T
	JR	NZ,RETPROC		; not a tail call
; tail call
	LD	DE,-9
	ADD	HL,DE			; HL pointing to PROC frame marker
	POP	DE			; return address
	POP	BC			; error address
	LD	SP,HL			; clear local variables and loops
	RST	$20			; advance past PROC
	PUSH	BC			; stack error address
	JP	T_PROC

RETURN_GS:
	POP	BC
	POP	DE
	DEC	HL
	DEC	HL
	DEC	HL
	LD	SP,HL
	EX	DE,HL
	LD	DE,L1F23 + 2	; RETURN + 2
	PUSH	DE
	JR	ENDP_SW		; RETURN again
