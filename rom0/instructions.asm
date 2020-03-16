	DEFB	P_SPECTRUM - $	; SPECTRUM
	DEFB	P_PLAY - $	; PLAY
	DEFB	P_TURBO - $	; TURBO
	DEFB	P_PLUG - $	; EN
	DEFB	P_RENUM - $	; RENUM
	DEFB	P_DEFPROC - $	; DEF PROC
	DEFB	P_PLUG - $	; Es8
	DEFB	P_STACK - $	; STACK
	DEFB	P_LABEL - $	; LABEL/@
	DEFB	P_POP - $	; POP
	DEFB	P_PLAY - $	; PLAY
	DEFB	P_PLUG - $	; EsJ
	DEFB	P_PLUG - $	; EI
	DEFB	P_PLUG - $	; EJ
	DEFB	P_PLUG - $	; EK
	DEFB	P_DISPLAY - $	; DISPLAY
	DEFB	P_ENDWHILE - $	; END WHILE
	DEFB	P_ONERROR - $	; ON ERROR
	DEFB	P_SPECTRUM - $	; SPECTRUM
	DEFB	P_WRITE - $	; WRITE #
	DEFB	P_TRACE - $	; TRACE
	DEFB	P_LOCAL - $	; LOCAL
	DEFB	P_DELETE - $	; DELETE
	DEFB	P_REPEAT - $	; REPEAT
	DEFB	P_PLUG - $	; EH
	DEFB	P_FPOKE - $	; FPOKE
	DEFB	P_PLUG - $	; EG
	DEFB	P_PLUG - $	; EO
	DEFB	P_PLUG - $	; EsI
	DEFB	P_USR - $	; USR
	DEFB	P_PLUG - $	; EY
	DEFB	P_UNTIL - $	; UNTIL
	DEFB	P_ASSERT - $	; ASSERT
	DEFB	P_PLUG - $	; EB
	DEFB	P_ENDIF - $	; END IF
	DEFB	P_YIELD - $	; YIELD
	DEFB	P_PALETTE - $	; PALETTE
	DEFB	P_EXIT - $	; EXIT
	DEFB	P_WHILE - $	; WHILE
	DEFB	P_ENDPROC - $	; END PROC
	DEFB	P_ELSE - $	; ELSE
	DEFB	P_PROC - $	; PROC
	DEFB	P_STEP - $	; STEP
P_END:	EQU	$

P_ENDIF:DEFB	$00
	DEFW	ENDIF

P_RENUM:DEFB	$00		; TODO: all sorts of arguments for RENUM
	DEFW	RENUM

P_EXIT:	DEFB	$00
	DEFW	PLUG	

P_POP:	DEFB	$00
	DEFW	POP

P_ELSE:	DEFB	$05
	DEFW	ELSE

P_STACK:DEFB	$07,$00
	DEFW	STACK

P_REPEAT:
	DEFB	$00
	DEFW	REPEAT

P_FPOKE:DEFB	$05
	DEFW	FPOKE

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
	DEFB	$05
	DEFW	ENDPROC

P_LOCAL:DEFB	$05
	DEFW	LOCAL

P_ASSERT:
	DEFB	$06, $00
	DEFW	ASSERT

P_TRACE:DEFB	$00
	DEFW	TRACE

P_DELETE:
	DEFB	$05
	DEFW	DELETE

P_WHILE:DEFB	$05
	DEFW	WHILE

P_ENDWHILE:
	DEFB	$00
	DEFW	ENDWHILE

P_ONERROR:
	DEFB	$05
	DEFW	ONERROR

P_YIELD:DEFB	$05
	DEFW	YIELD

P_TURBO:DEFB	$06,$00
	DEFW	TURBO_X

P_DISPLAY:
	DEFB	$03
	DEFW	DISPLAY

P_STEP:	DEFB	$07,",",$06,$00
	DEFW	STEP

P_SPECTRUM:
	DEFB	$00
	DEFW	SPECTR

P_WRITE:DEFB	$06,";",$5
	DEFW	WRITE

P_PLAY:
	DEFB	$05
	DEFW	PLAY

P_PALETTE:
; unimplemented instruction, accepted w/o parameters, but not executed
P_PLUG:
	DEFB	$00
	DEFW	PLUG

CHECK_END:
	CALL	SYNTAX_Z
	RET	NZ
END05_E:POP	BC		; SCAN-LOOP
END05:	POP	BC		; STMT-RET
STMT_NEXT:
	RST	$18
	CP	$0D		; CR
	JR	Z,LINE_END
	CP	":"
	JR	Z,STMT_LOOP
	JR	ERROR_C_I

STMT_LOOP:
	LD	HL,L1B28	; STMT-LOOP
	PUSH	HL
	JP	SWAP

LINE_END:
	BIT	7,(IY+$01)
	JP	Z,SWAP
	LD	HL,(NXTLIN)
	LD	A,$C0
	AND	(HL)
	JR	NZ,LE_SWAP	; program finished
	PUSH	HL
	LD	HL,L1BBF - 1	; XOR A, LINE-USE
	EX	(SP),HL
LE_SWAP:JP	SWAP

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
	RST	$30
	DEFW	L1C1F		; CLASS_01
	RET

CLASS2_02:
	RST	$30
	DEFW	L1C4E		; CLASS_02
	RET

CLASS2_04:
	RST	$30
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
	RST	$30
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
	CALL	UNSTACK_Z
	RST	$28
	DEFB	$A1		; stk-one
	DEFB	$1B		; negate
	DEFB	$38		; end
	RET

FETCH_NUM:
	CP	$0D
	JR	Z,USE_ZERO
	CP	":"
	JR	NZ,CLASS2_06

USE_ZERO:
	RST	$30
	DEFW	L1CE6		; USE-ZERO
	RET

CLASS2_07:
	RST	$30
	DEFW	L2070		; STR-ALTER
	RET	NC
	LD	HL,(T_ADDR)
	LD	A,(HL)
	OR	A
	JR	Z,CL7_E
	INC	HL
	LD	(T_ADDR),HL
CL7_E:	CALL	UNSTACK_Z
	LD	A,2
	RST	$30
	DEFW	L1601	; CHAN-OPEN
	RET

FROM_1:	CALL	SYNTAX_Z
	JR	Z,FROM_1R
	RST	$30
	DEFW	L1CE6 + 4	; USE-ZERO + 4
	INC	HL
	INC	HL
	LD	(HL),$01	; ONE
	JR	FROM_1R

TO_SAME:CALL	UNSTACK_Z
	RST	$28
	DEFB	$31		; duplicate
	DEFB	$38		; end
	RET

DELIM:	DEFB	$0D
	DEFM	":,;)"
DELIM_E:

CLASS2_0A:
	CALL	SYNTAX_Z
	JR	NZ,LABEL_R
	LD	BC,$0006
	RST	$30
	DEFW	L2C8D		; ALPHA
	JP	NC,ERROR_C
	INC	HL		; insert pointers after first letter of label
	RST	$30
	DEFW	L1655		; MAKE-ROOM
	LD	DE,(E_LINE)
LABEL_S:LD	A,(DE)
	INC	DE
	CP	" " + 1
	JR	C,LABEL_S	; skip indent
LABEL_N:RST	$30
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
	RST	$30
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
ERROR_I:RST	$30
	DEFW	L1D84		; I FOR without NEXT

THENLESS:
	RES	4,(IY+$37)	; signal true outcome
	CALL	TEST_ZERO
SWAPNZ:	JP	NZ,SWAP		; Upon true condition, simply continue

; Upon false condition start scanning for END IF, ELSE or end of code
THENLESS0:
	SET	4,(IY+$37)	; signal false outcome
	LD	BC,(NXTLIN)
	LD	DE,T_IF
	CALL	LOOK_PROG2
	INC	BC		; increment end-of-line pointer
	LD	(NXTLIN),BC
	JR	C,ERROR_S
	POP	BC		; discard SCAN-LOOP
SW20:	RST	$20
	JP	SWAP

ELSE:	POP	BC		; discard STMT-RET
	CALL	SYNTAX_Z
	JR	Z,ELSE_S
	LD	HL,FLAGX
	BIT	4,(HL)		; FLAGX, check if last IF was false
	RES	4,(HL)
	JR	NZ,ELSE_1
	RST	$18
	CP	$0D
	SCF
	JR	Z,ELSE_3	; multi-line ELSE block
	CP	IF_T
	JR	Z,ELSEIF	; ELSE IF
ELSE_0:	LD	BC,L1BB3 + 4	; LINE-END + 4
	JR	ELSE_2
ELSE_S:	BIT	4,(IY+$37)	; after THEN
ERRCNZ4:JP	NZ,ERROR_C	; ELSE is an error
ELSE_1:	LD	BC,L1B29	; STMT-L-1
ELSE_2:	PUSH	BC
	JP	SWAP
ELSE_3:	PUSH	BC		; put back STMT-RET
	LD	BC,(NXTLIN)
	LD	DE,T_IF
	CALL	LOOK_PROG2
	LD	(NXTLIN),BC
	JR	NC,SW20		; missing END IF?

ERROR_S:CALL	ERROR
	DEFB	$1B		; S Missing END

ELSEIF:	INC	HL
	PUSH	BC
	CALL	SKIPEX
	POP	BC
	CP	THEN_T
	JR	Z,ELSE_0
	DEC	HL
ELSE_4:	LD	(CH_ADD),HL
	JR	ELSE_3

POKE_S:	RST	$18
	BIT	6,(IY+$01)	; numeric?
	LD	HL,L1E2C + 3	; DATA-1 + 3
	JR	Z,POKE_S1	; jump, if string
	LD	HL,L1E2C + 8	; DATA-1 + 8
	CP	","
POKE_S1:JP	Z,TR_SW
	JP	ERROR_C

FPOKE:	RST	$30
	DEFW	L1C82		; CLASS-06 single numeric
	CALL	SYNTAX_Z
	JR	Z,FPOKE_S
	RST	$30
	DEFW	L1E99		; FIND-INT2
	RST	$18
FPOKE_S:PUSH	BC		; address (dummy, if checking syntax)
FPOKE_L:RST	$30
	DEFW	L1C7A + 3	; CLASS-08 + 3 comma, followed by numeric
	CALL	SYNTAX_Z
	JR	Z,FPOKE_Z
	RST	$28
	DEFB	$02		; delete
	DEFB	$38		; end
	EX	DE,HL
	POP	DE
	LD	BC,5
	LDIR
	PUSH	DE
	RST	$18
FPOKE_Z:CP	","
	JR	Z,FPOKE_L
	JP 	END05_E

N_POKE:	PUSH	HL		; STMT_RET or STEP_HOOK
	CALL	SYNTAX_Z
	JR	Z,POKE_S
	LD	HL,(STKEND)
	LD	BC,$0005
	ADD	HL,BC
	LD	(STKEND),HL
	RST	$30
	DEFW	L1E99		; FIND-INT2
	INC	BC

POKE_L:	RST	$20		; advance
	PUSH	BC
	RST	$30
	DEFW	L24FB		; SCANNING
	EX	AF,AF'
	BIT	6,(IY+$01)	; numeric?
	JR	Z,SPOKE		; jump, if not
	RST	$30
	DEFW	L2DD5		; FP-TO-A
	JP	C,ERROR_B
	JR	Z,POKEP
	NEG
POKEP:	POP	BC
	LD	(BC),A
	INC	BC
	JR	POKE_L2

E_POKE:	CALL	SYNTAX_Z
	JR	Z,POKE_S
	RST	$28		; CALCULATE
	DEFB	$01		; exchange
	DEFB	$38		; end
	RST	$30
	DEFW	L1E99		; FIND-INT2
	PUSH	BC
	RST	$18
	EX	AF,AF'
SPOKE:	RST	$30
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

USR:	CALL	SYNTAX_Z
	JR	Z,POKE_SWAP
	RST	$30
	DEFW	L1E99		; FIND-INT2
	PUSH	BC
	JR	POKE_SWAP

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
	LD	(STKEND),HL
	RET

ASSERT:	CALL	TEST_ZERO
	JR	NZ,POKE_SWAP
ERROR_V:CALL	ERROR
	DEFB	$1E		; V ASSERT failed

ERROR_W:CALL	ERROR
	DEFB	$1F		; W END WHILE without WHILE

ERROR_N:RST	$30
	DEFW	L1BEC		; N Statement lost

ENDWHILE:
	CALL	SKIP_LL
	CP	WHILE_M
	JR	NZ,ERROR_W	; wrong context
	DEC	HL
	DEC	HL		; skip outer error address
	PUSH	HL		; context
	LD	DE,(CH_ADD)	; execution pointer
	PUSH	DE		; save it
	DEC	HL

	LD	A,(HL)		; A=(SUBPPC)
	DEC	HL
	LD	D,(HL)
	DEC	HL
	LD	E,(HL)		; DE=(PPC)

	PUSH	AF
	EX	DE,HL
	RST	$30
	DEFW	LINE_ADDR
	JR	NZ,ERROR_N
	INC	HL
	INC	HL		; skip line number
	INC	HL
	INC	HL		; skip line length
	POP	DE
	DEC	D
	LD	E,0
	RST	$30
	DEFW	L198B		; EACH-STMT
	RST	$20		; skip WHILE token
	RST	$30
	DEFW	L24FB		; SCANNING
	CALL	TEST_ZERO
	POP	HL		; old execution pointer
	JR	Z,WEND
	POP	DE		; discard context
	JR	CONT_LOOP	; continue with the loop

WEND:	LD	(CH_ADD),HL
	POP	HL		; context
	LD	(ERR_SP),HL
	POP	BC		; return address
	LD	SP,HL		; reclaim locals
	PUSH	BC
	JR	UNT_SW

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
CONT_LOOP:
	LD	HL,$0006
	ADD	HL,SP
UNT_C:	LD	E,(HL)
	INC	HL
	LD	D,(HL)		; DE = (PPC)
	INC	HL
	LD	A,(HL)		; A = (SUBPPC)
	EX	DE,HL		; HL = (PPC)
	LD	D,A		; D = (SUBPPC)
GOTO_5:	LD	BC,L1E73	; GO-TO-2
	PUSH	BC
UNT_SW:	JP	SWAP

END_REP:EX	DE,HL
	POP	HL
	INC	SP		; SP = SP + 3
UNT_ER:	PUSH	DE
UNT_E:	LD	(ERR_SP),SP
	PUSH	BC
	JR	UNT_SW

ERROR_U:PUSH	DE
ERROR2U:PUSH	HL
	CALL	ERROR
	DEFB	$1D		; U UNTIL without REPEAT

ERROR_2:RST	$30
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
	RST	$30
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
REPSW1:	JR      UNT_SW

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

OLDOPR:	RST	$30
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
	RST	$30
	DEFW	L24FB + 1	; SCANNING + 1
	RET

DEFPROC:RST	$18
	CP	")"
	JR	Z,DP_E
DP_L:	RST	$30
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
	JP	Z,END05
	LD	DE,T_DP
SKIPEND:CALL	LOOK_PROG2
	INC	BC
	LD	(NXTLIN),BC
	JP	C,ERROR_S	; S Missing END (PROC)
	INC	(IY+$0A)	; advance NSPPC past END PROC
	JR	SW_LOC

LOCAL_R:RST	$20		; advance past the comma
LOCAL_S:RST	$30
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
	RST	$30
	DEFW	L2996		; STK-VAR
LOCAL_E:RST	$18		; TODO: ???
	CP	","
	JR	Z,LOCAL_R
	LD	HL,L1BEE + 5	; CHECK-END + 5; TODO: array initializer?
	PUSH	HL
SW_LOC:	JP	SWAP

LOCAL_I:CP	"="
	JR	NZ,LOCAL_E
	RST	$20
	PUSH	BC
	RST	$30
	DEFW	L24FB + 1	; SCANNING + 1
	LD	A,(FLAGS)
	POP	BC
	XOR	B
	AND	$40
	JR	NZ,ERRC_NZ	; type mismatch
	JR	LOCAL_E

LCL_NX:	PUSH	BC
	PUSH	DE
	PUSH	AF
	JR	LCL_EX

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
	RST	$30
	DEFW	L24FB + 1	; SCANNING + 1
	POP	BC
	POP	DE		; return address
	POP	HL		; error address
	EXX
	RST	$30
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
	JR	NZ,SW_LOC
	RST	$20
	JR	LOCAL_L

LCL_F:	DEC	HL
	BIT	7,(HL)
	JR	NZ,LCL_L
	RES	1,(IY+FLAGX-ERR_NR)	; signal existing
	EX	DE,HL
	RST	$18
	CP	"="
	JR	NZ,LCL_CM
	EX	DE,HL
	INC	HL
	BIT	5,C
	JR	NZ,LCL_AN
	INC	HL
	INC	HL
	INC	HL		; skip maxlen
	LD	C,(HL)
	INC	HL
	LD	B,(HL)		; BC=length
	INC	HL
	SET	0,(IY+FLAGX-ERR_NR) ; signal complete
LCL_AN:	LD	(STRLEN),BC
	LD	(DEST),HL
	RST	$20
	RST	$30
	DEFW	L1C56		; VAL-FET-1
	JR	LCL_CM

; Initialize local array
; In: C letter and type
LCL_A:	XOR	A
	LD	(MEMBOT),A	; reset dimension counter
	PUSH	BC		; save variable name and type
	LD	HL,$0001	; string element is 1 byte
	BIT	5,C
	JR	Z,LCL_SA	; string array
	LD	L,$05		; numeric element is 5 bytes
LCL_SA:	PUSH	HL		; save size
	RST	$20		; advance past separator
	RST	$30
	DEFW	L24FB + 1	; SCANNING + 1, read dimension
	PUSH	AF		; save separator
	RST	$28
	DEFB	$31		; duplicate
	DEFB	$38		; end
	RST	$30
	DEFW	L1E99		; FIND-INT2
	LD	E,C
	LD	D,B
	POP	BC		; restore separator
	POP	HL		; restore size
	RST	$30
	DEFW	L2AF4 + 4	; HLxDE + 4
	INC	(IY+MEMBOT-ERR_NR)
	LD	A,B
	CP	","
	JR	Z,LCL_SA	; loop through all dimensions
	EXX
	POP	BC		; restore variable name and type
	POP	DE		; return address
	POP	HL		; error address
	EXX
	LD	(MEMBOT+2),HL
	LD	C,L		; TODO: add dimensions to memory check
	LD	B,H
	RST	$20		; skip through closing bracket
	RST	$30
	DEFW	L1F05		; TEST-ROOM
	AND	A
	LD	HL,$0000
	SBC	HL,BC		; -size
	ADD	HL,SP
	LD	SP,HL
	LD	E,L
	LD	D,H
	INC	DE
	DEC	BC
	XOR	A
	EXX
	BIT	5,C
	EXX
	JR	NZ,LCL_IN
	LD	A," "
LCL_IN:	LD	(HL),A
	LD	A,B
	OR	C
	JR	Z,LCL_1S	; 1 element string array
	LDIR
LCL_1S:	LD	A,(MEMBOT)
	LD	(MEMBOT+1),A
LCL_DM:	EXX
	PUSH	DE
	EXX
	RST	$30
	DEFW	L1E99		; FIND-INT2
	EXX
	POP	DE
	EXX
	PUSH	BC		; save dimension
	DEC	(IY+MEMBOT-ERR_NR)
	JR	NZ,LCL_DM
	LD	BC,(MEMBOT)
	PUSH	BC
	INC	SP		; save number of dims
	LD	C,B
	LD	B,0
	LD	HL,(MEMBOT+2)
	ADD	HL,BC
	ADD	HL,BC
	INC	HL
	PUSH	HL		; save total size
	EXX
	LD	A,C
	AND	$3F
	LD	C,A
	LD	B,$3E
	PUSH	BC		; array name and type
	PUSH	HL		; error address
	LD	(ERR_SP),SP
	PUSH	DE		; return address
	JR	STCK_SW

LCL_STR:LD	(STRLEN),BC
	PUSH	DE
	RST	$30
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
	JR	ST_SMTV

STACKE:	LD	A,$0D
	RST	$10
	JR	STACKL

ST_NFOR:RRA
	SUB	REPEAT_M
	JR	C,ST_VAR
	JR	Z,ST_REP
	DEC	A
	JR	Z,ST_WHL

ST_PRC:	LD	A,PROC_T
	INC	HL
	INC	HL
ST_PRCR:CALL	ST_CTX
	INC	HL
	INC	HL		; skip duplicated error address
	RET

ST_WHL:	LD	A,WHILE_T
	JR	ST_PRCR

ST_GOSUB:
	LD	A,GOSUB_T
	RST	$10
	JR	ST_STMT

ST_REP:	LD	A,REPEAT_T
ST_CTX:	RST	$10
ST_SMTV:LD	C,(HL)
	INC	HL
	LD	B,(HL)
	INC	HL
ST_STMT:LD	A,AT_T
	RST	$10
	PUSH	HL
	RST	$30
	DEFW	L1A1B		; OUT-NUM-1
ST_REF:	LD	A,":"
	RST	$10
	POP	HL
	LD	A,(HL)
	DEC	A
	CALL	DECBYTE
	INC	HL
	RET

ST_VAR:	LD	A,LOCAL_T
	RST	$10
	LD	A,C
	AND	$20
	JR	Z,ST_STR
ST_NUM:	CALL	ST_VARN
	LD	A,"="
	RST	$10
ST_NUMV:RST	$30
	DEFW	L33B4		; STACK-NUM
	PUSH	HL
	RST	$30
	DEFW	L2DE3		; PRINT-FP
	POP	HL
	RET

ST_STR:	CALL	ST_VARN
ST_SKIP:LD	E,(HL)
	INC	HL
	LD	D,(HL)
	ADD	HL,DE
	INC	HL
	RET

ST_VARN:LD	A,C
	AND	$1F		; bottom 5 bits
	OR	"a"-1
	RST	$10
	BIT	5,C
	LD	A,"$"
	CALL	Z,$0010
	BIT	6,C
	RET	NZ
	LD	A,"("
	RST	$10
	LD	A,")"
	RST	$10
	CALL	ST_SKIP
	POP	DE		; discard return address
	RET

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
	CP	":"
	JR	Z,F_ENDIF
	CP	$0D
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

; Long-named string variables
LV_CONT:JR	C,LV_NEXT
	RST	$30
	DEFW	L28B2		; LOOK-VARS
	PUSH	HL
	EX	AF,AF'
	RST	$18
	CP	"$"
	JR	NZ,LV_NSTR
	; long string not found
	RST	$20		; skip over "$"
	RES	6,(IY+FLAGS-ERR_NR)	; indicate string
	CALL	SYNTAX_Z
	SCF
	JR	NZ,SW_LV2	; in runtime, long string not found CF=1,ZF=0
	AND	A		; in syntax check, long string found CF=0,ZF=0
	DEFB	$3E		; LD A,skip next byte
LV_NSTR:EX	AF,AF'
SW_LV2:	POP	HL
	JR	SW_LV

LV_FOR:	CP	$91		; interpreting FOR?
	JR	NZ,LV_DIM	; if not, it's a DIM
LV_SW:	LD	BC,L28B2	; LOOK-VARS
	JR	NEXT_SW

LV_DIM:	XOR	A
	LD	(DEFADD+1),A	; look for globals only
	RST	$30
	DEFW	L28B2		; LOOK-VARS
	LD	(IY+DEFADD+1-ERR_NR),1	; restore without changing flags
	JR	SW_LV		; return to DIM

; Handling DIM and argumentless NEXT
LV_NEXT:LD	A,(T_ADDR)
	CP	$99		; interpreting NEXT?
	JR	NZ,LV_FOR	; check FOR, if not
	RST	$18		; get the character following NEXT
	CP	$0D		; CR?
	JR	Z,NEXT		; if so, it's an argumentless NEXT
	CP	":"		; colon?
	JR	NZ,LV_SW	; return, if not
NEXT:	POP	BC		; discard CLASS-04 return address
	POP	BC		; discard SCAN-LOOP
	CALL	CHECK_END
	CALL	SKIP_LC
	ADD	A,A
	JR	NC,ERROR_1
	PUSH	HL
	INC	HL
	LD	(MEM),HL
	RST	$28		; same calculator command as in NEXT
	DEFB	$E0,$E2,$0F,$C0,$02,$38
	RST	$30
	DEFW	L1DDA		; NEXT-LOOP
	LD	HL,MEMBOT
	LD	(MEM),HL	; much faster than RST	$30:DEFW X16CB, not worth 3 bytes
	POP	HL
	JR	NC,NEXT_LP	; execute loop body again
	LD	BC,$0013	; jump over loop variable
	ADD	HL,BC
	POP	BC		; save return address
	POP	DE		; save error address
	LD	SP,HL
	PUSH	DE		; restore error address
	LD	(ERR_SP),SP
NEXT_SW:PUSH	BC		; restore return address
SW_LV:	JP	SWAP

NEXT_LP:LD	BC,$0010
	ADD	HL,BC
	JP	UNT_C

ERROR_1:RST	$30
	DEFW	REPORT1 + 3	; 1 NEXT without FOR

ERROR_A:RST	$30
	DEFW	L34E7		; A Invalid argument

STOP:	PUSH	BC
DERROR:	RST	$30
	DEFW	L24FB		; SCANNING
	BIT	6,(IY+$01)	; numeric?
	JR	NZ,NERROR
	CALL	SYNTAX_Z
	JR	Z,SERROR
	RST	$30
	DEFW	L2BF1		; STK-FETCH
	LD	A,B
	OR	C
	JR	Z,ERROR_A
	LD	HL,(WORKSP)
	AND	A
	SBC	HL,DE
	ADD	HL,DE
	JR	C,WERROR
	PUSH	HL
	PUSH	DE
	RST	$30
	DEFW	L0030		; BC-SPACES
	POP	HL
	PUSH	BC
	PUSH	DE
	LDIR
	POP	DE
	POP	BC
	POP	HL
WERROR:	EX	DE,HL
	LDIR
	EX	DE,HL
	DEC	HL
	SET	7,(HL)
SERROR:	RST	$18
	CP	","
	JR	NZ,YERROR
	CALL	NEXT_1NUM
	CALL	SYNTAX_Z
	JR	Z,ZERROR
	RST	$30
	DEFW	L1E94		; FIND-INT1
	SUB	$80
ERRBNC:	JP	NC,ERROR_B
	JR	ZERROR
YERROR:	LD	A,$89		; custom report
	JR	ZERROR

NERROR:	CALL	SYNTAX_Z
	JR	Z,ZERROR
	RST	$30
	DEFW	L1E94		; FIND-INT1
	CP	MAX_ERR
	JR	NC,ERRBNC
ZERROR:	DEC	A
	PUSH	AF
	RST	$18
	CP	","
	JR	NZ,XERROR
	RST	$20
	CP	LINE_T
	JR	Z,STOP_LINE
	CALL	CLASS2_08
	CALL	SYNTAX_Z
RERROR:	JP	Z,END05_E
	RST	$30
	DEFW	L1E94		; FIND-INT1
	LD	(SUBPPC),A
	RST	$30
	DEFW	L1E99		; FIND-INT2
	LD	(PPC),BC
XERROR:	CALL	SYNTAX_Z
	JR	Z,RERROR
	POP	AF
	LD	L,A
	RST	$30
	DEFW	L0055		; ERROR-3

STOP_LINE:
	RST	$20
	CALL	SYNTAX_Z
	JR	Z,RERROR
	POP	AF
	LD	(ERR_NR),A
	POP	HL		; discard ESTOP
	LD	HL,L0055 + 3	; ERROR-3 + 3
	EX	(SP),HL
	LD	HL,SUBPPC
	LD	A,1
	JR	POPCTX

POP:	LD	HL,OSPCC
	XOR	A
POPCTX:	LD	(MEMBOT),HL
	LD	(MEMBOT+2),A
	CALL	CALLCTX
	CP	MM
	PUSH	HL
	JR	Z,POP_GS
	DEC	HL
	DEC	HL		; skip old error address
POP_GS:	DEC	HL
	LD	BC,$0003
	LD	DE,(MEMBOT)
	LDDR			; move return pointer to CONTINUE pointer
	DEC	(IY+MEMBOT+2-ERR_NR)
	JR	NZ,POPCNT
	DEC	(IY+SUBPPC-ERR_NR)
POPCNT:	POP	HL		; new context
	POP	BC		; return address
	POP	DE		; error address
	LD	SP,HL
	PUSH	DE		; error address
	LD	(ERR_SP),SP
	PUSH	BC		; return address
	JP	SWAP

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
	RST	$30
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
RENUME:	RST	$30
	DEFW	L1BB0		; 0 Ok.

ERROR_G:CALL	ERROR
	DEFB	$0F		; G No room for line

; TRACE jump
TRACE_J:SET	7,(HL)
	LD	HL,STEP_HOOK
	PUSH	HL
	LD	(ERR_SP),SP
	LD	HL,X1B83
	JR	TR_SW

TR_THENLESS:
	CALL	TEST_ZERO
	JR	NZ,TR_IF2
	LD	HL,STEP_HOOK
	PUSH	HL
	PUSH	HL		; placeholder
	JP	THENLESS0

TR_ELIF:INC	HL
	CALL	SKIPEX
	CP	THEN_T
	JR	Z,TR_REM
	DEC	HL
	JP	ELSE_4

TRACE:	POP	HL		; discard return address
TRACE_D:LD	HL,NSPPC
	BIT	7,(HL)		; jump?
	JR	Z,TRACE_J	; jump, if so
TR_IF2:	RST	$18
	CP	$0D
	JR	Z,TRACE_L
	SET	7,(IY+$0D)	; Signal TRACE for STMT-LOOP
TRACE_R:LD	HL,L1B76	; STMT-RET
TR_SW:	PUSH	HL
	JP	SWAP		; return

TRACE_EX_TAB:
	DEFB	IF_T
	DEFB	TR_IF - $
	DEFB	ELSE_T
	DEFB	TR_ELSE - $
	DEFB	REM_T
	DEFB	TR_REM - $
	DEFB	0

TR_IF:	RST	$20
	RES	4,(IY+$37)	; signal true outcome
	RST	$30
	DEFW	L24FB + 1	; SCANNING + 1
	CP	THEN_T
	JR	NZ,TR_THENLESS
	CALL	TEST_ZERO
	JR	NZ,TR_IF1
	SET	4,(IY+$37)	; signal false outcome

TR_ELSE:LD	HL,FLAGX
	BIT	4,(HL)
	JR	NZ,TR_REM
	RES	4,(HL)
	RST	$20
	CP	$0D
	SCF
	LD	BC,STEP_HOOK
	JP	Z,ELSE_3
	CP	IF_T
	JR	Z,TR_ELIF

TR_REM:	LD	HL,(NXTLIN)
	DEC	HL
TRACE_L:INC	HL
	LD	A,(HL)
	CP	$3E		; end-of program?
	JR	NC,TRACE_R	; jump, if so
TRACE_U:LD	A,$81
	LD	DE,STEP_HOOK
	PUSH	DE
	LD	(ERR_SP),SP
	LD	DE,X1BA9	; LINE-USE with preliminary check
	PUSH	DE
TR_SW1:	JP	SWAP

TR_IF1:	SET	7,(IY+$0D)	; Signal TRACE for STMT-LOOP
	JP	STMT_LOOP

STEP_CONT:
	LD	A,(ERR_NR)
	INC	A		; Handling an error?
	JR	NZ,TRACE_E	; jump, if so

	LD	HL,SUBPPC
	BIT	7,(HL)		; Handling STMT-LOOP?
	JR	Z,TRACE_D	; jump, if not
	RES	7,(HL)

	RST	$30
	DEFW	L0D6E		; CLS-LOWER
	
	LD	HL,(PPC)
	BIT	7,H		; executing command line?
	JR	Z,TRACE_2	; jump, if not
	INC	HL
	INC	HL
TRACE_2:CALL	DECWORD
	LD	A,":"
	RST	$10
	LD	A,(SUBPPC)
	CALL	DECBYTE
	LD	A," "
	RST	$10

	RST	$18
	LD	A,(HL)
	RST	$10		; TODO: proper line listing
	
	RES	3,(IY+$02)	; no edit line
	LD	HL,FLAGX
	LD	C,(HL)
	RST	$30
	DEFW	L15DE		; WAIT-KEY1
	PUSH	AF
	LD	(HL),C
	RST	$30
	DEFW	L0D6E		; CLS-LOWER
	POP	AF
	POP	HL
	CP	" "
	JR	Z,ERROR_L	; BREAK
	CP	"c"
	JR	Z,TRACE_X
	CP	"C"
TRACE_X:JP	Z,ELSE_1
	PUSH	HL
	RST	$18
	LD	C,A
	LD	HL,TRACE_EX_TAB
	CALL	INDEXER
	JP	C,INDEXER_JP
	LD	A,C
	LD	HL,STEP_HOOK
	JR	TR_SW1

TRACE_Q:LD	HL,(NEWPPC)
	RST	$30
	DEFW	L196E		; LINE-ADDR again, we lost the result
	JR	TRACE_U

TRACE_E:LD	(ERR_SP),SP	; restore ERR_SP
	LD	(IY+$00),$FF	; restore ERR_NR
	RST	$18		; HL=(CH_ADD)
	LD	A,(SUBPPC)
	ADD	$81
	JR	NC,TRACE_Q	; LINE-NEW derailed
	CP	2
	JR	C,TRACE_E1
	LD	D,A
	LD	E,0
	RST	$30
	DEFW	L198B		; EACH-STMT
	JP	NZ,ERROR_N
	CP	$0D
	JP	Z,TRACE_L
	RST	$20		; advance
TRACE_E1:
	JP	ELSE_1

; Similar to L1F54 in ROM1
BREAK:	LD	A,$7F
	IN	A,($FE)
	RRA
	RET	C
	LD	A,($FE)
	IN	A,($FE)
	RRA
	RET	C

ERROR_L:RST	$30
	DEFW	L1B7B		; L Break into program

DEL_SK:	RST	$18		; TODO: ???
	CP	"("
	JR	NZ,DEL_SW
	RST	$20
	CP	")"
	EX	AF,AF'
	RST	$20
	EX	AF,AF'
	JR	Z,DEL_SW
	JP	ERROR_C

DEL_E:	LD	HL,(VARS)
	JR	DEL_NE

DELETE:	RST	$30
	DEFW	L2C8D		; ALPHA
	JR	NC,DEL_PR
DEL_NX:	RST	$30
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
DEL_N1:	RST	$30
	DEFW	L19E8		; RECLAIM-2
DEL_SW:	RST	$18
	CP	","
	JP	NZ,SWAP
	RST	$20
	JR	DEL_NX

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

DEL_PR:	CALL	STACKSWAP
	CALL	CLASS2_09
	CALL	UNSTACK_Z
	RST	$30
	DEFW	L2DA2		; FP-TO-BC
	JR	NZ,DEL_E
	JR	C,DEL_E
	LD	L,C
	LD	H,B
	RST	$30
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
	RST	$30
	DEFW	L1E99		; FIND-INT2
	LD	L,C
	LD	H,B
	RST	$30
	DEFW	L196E		; LINE-ADDR
	EX	DE,HL
	POP	HL
	RST	$30
	DEFW	L19DD		; DIFFER
	RET	C
	RST	$30
	DEFW	L19E8		; RECLAIM-2
	LD	HL,(PPC)
	LD	A,(SUBPPC)
	LD	(NEWPPC),HL
	INC	A
	LD	(NSPPC),A
	JP	RSTLBLS

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
	RST	$18
	CP	"("
	JR	NZ,DEL_SW2
	RST	$20
	RST	$20
DEL_SW2:JP	DEL_SW

DEL_LF:	LD	BC,$0017
	LD	A,(HL)
	CP	$E0
	JR	NC,DEL_LF0
	LD	C,$06
	JR	DEL_LF0

PROC_S:	RST	$30
	DEFW	L2C8D		; ALPHA
	LD	B,1
	CALL	S_LBLI		; insert label with empty cache
	RST	$18		; TODO: ???
	CP	"("
	JR	NZ,ERR_PR
	RST	$20
	CP	")"
	JR	Z,PROC_SE
	DEFB	$3E		; LD A, skipping next byte
PROC_SL:RST	$20
	RST	$30
	DEFW	L24FB		; SCANNING
	CP	","
	JR	Z,PROC_SL
	CP	")"
ERR_PR:	JP	NZ,ERROR_C
PROC_SE:RST	$20
	CP	TO_T
	JP	NZ,END05
READ_3:	LD	HL,L1DEC	; READ-3
	JP	SWAP

PROC:	CALL	SYNTAX_Z
	JR	Z,PROC_S
	LD	BC,$0012
	RST	$30
	DEFW	L1F05		; TEST-ROOM
	POP	DE		; DE = return address
	POP	HL		; HL = error address
	PUSH	HL
	PUSH	HL		; replicate
	LD	HL,(SUBPPC - 1)
	INC	H
	EX	(SP),HL		; HL = error address
	INC	SP		; stack SUBPPC (1 byte)
	LD	BC,(PPC)
	PUSH	BC		; stack PPC (2 bytes)
	PUSH	HL
	LD	HL,(DATADD)
	LD	BC,(PROG)
	AND	A
	SBC	HL,BC
	EX	(SP),HL		; stack DATADD
	LD	BC,$3E00 + PROC_M
	PUSH	BC		; stack marker
	PUSH	HL		; stack error address
; tail call entry point
T_PROC:	PUSH	DE		; stack return address
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
	JR	Z,ERROR_Q1	; that is an error!
	PUSH	BC		; save variable name and type
	RST	$30
	DEFW	L24FB + 1	; SCANNING + 1
	POP	BC		; variable name and type
	POP	DE		; return address
	POP	HL		; error address
	EXX
	RST	$30
	DEFW	L2BF1		; STK-FETCH
	BIT	6,(IY+$01)	; type
	JR	NZ,PROC_NX	; jump, if numeric
	CALL	LCL_STR		; stack string
	LD	BC,(STRLEN)
	PUSH	BC
	INC	BC
	INC	BC
	PUSH	BC
	JR	PROC_X

PROC_NX:PUSH	BC
	PUSH	DE
	PUSH	AF
	INC	SP
PROC_X:	EXX
	LD	A,(FLAGS)
	RRCA
	XOR	C
	AND	$20
	JR	NZ,ERROR_Q1
	LD	B,$3E
	PUSH	BC		; marker
	PUSH	HL		; error address
	PUSH	DE		; return address
	RST	$18		; separator of PROC arguments
	LD	E,A
	LD	(DATADD),HL
	LD	HL,(X_PTR)
	LD	(CH_ADD),HL
	INC	HL
	RST	$18		; separator in DEF PROC
	CP	","
	JR	NZ,PROC_E
	XOR	E
	JR	Z,PROC_L

ERROR_Q1:
	LD	HL,(ERR_SP)
	DEC	HL
	DEC	(HL)
	LD	DE,SUBPPC
	LDD
	LDD
	LDD
ERROR_Q:RST	$30
	DEFW	REPORT_Q	; Q Parameter error

PROC_E:	POP	DE		; return address
	LD	(ERR_SP),SP
	PUSH	DE
	RST	$20		; skip closing bracket Of DEF PROC
	JR	WHILE_E

REPEAT:	POP	DE		; DE = return address
	LD	HL,(SUBPPC - 1)
	INC	H
	EX	(SP),HL		; HL = error address
	INC	SP		; stack SUBPPC (1 byte)
	LD	BC,(PPC)
	PUSH	BC		; stack PPC (2 bytes)
	LD	BC,$3E00 + REPEAT_M
	JR	LOOPFRAME

WHILE:	LD	HL,(CH_ADD)
	LD	(DEST),HL
	CALL	CLASS2_06	; single numeric expression
	CALL	SYNTAX_Z
	JP	Z,END05
	CALL	TEST_ZERO
	JR	Z,WHILE0
	POP	DE		; DE = return address
	POP	HL		; HL = error address
	PUSH	HL
	PUSH	HL		; replicate
	LD	HL,(SUBPPC - 1)
	INC	H
	EX	(SP),HL		; HL = error address
	INC	SP		; stack SUBPPC (1 byte)
	LD	BC,(PPC)
	PUSH	BC		; stack PPC (2 bytes)
	LD	BC,$3E00 + WHILE_M
LOOPFRAME:
	PUSH	BC		; stack marker
	PUSH	HL		; stack error address
	LD	(ERR_SP),SP
	PUSH	DE		; stack return address
	LD	BC,$0014	; why this much? see $1F02 in ROM1
	LD	HL,L1F05	; TEST-ROOM
ENDPR_E:PUSH	HL
WHILE_E:JP	SWAP

WHILE0:	LD	DE,T_WHILE
	JP	SKIPEND

ERROR_X:CALL	ERROR
	DEFB	$20		; X END PROC without DEF

N_RETURN:
	PUSH	HL		; return address
	RST	$18

ENDPROC:CP	$0D
	JR	Z,ENDPR
	CP	":"
	JR	Z,ENDPR
	CALL	SYNTAX_Z
	JR	Z,ENDPR_S
	RST	$18
	LD	(RETADDR),HL	; return data pointer
	JR	ENDPR3

WRITE:	CALL	SYNTAX_Z
	JP	NZ,WRITE_DO

ENDPR_S:LD	HL,L1E2C	; DATA-1
	JR	ENDPR_E

ENDPR1:	CALL	SKIPLL
	JR	ENDPR2

ENDPR:	CALL	SYNTAX_Z
	JP	Z,END05
ENDPR3:	CALL	SKIP_LL
ENDPR2:	CP	REPEAT_M
	JR	Z,ENDPR1
	CP	WHILE_M
	JR	Z,ENDPR1
	CP	PROC_M
	JR	NZ,ERROR_X
	PUSH	HL		; new marker address
	DEC	HL
	DEC	HL		; skip saved error address
	LD	(ERR_SP),HL
	LD	DE,SUBPPC
	DEC	HL
	LDD
	LDD
	LDD			; copy return reference to PPC/SUBPPC
	LD	B,(HL)
	DEC	HL
	LD	C,(HL)		; positive PROG offset
	EX	DE,HL
	LD	HL,(PROG)	; PROG
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
	RST	$20		; advance
	CP	TO_T
	JR	Z,ENDP_3
ENDP_SW:POP	HL		; new marker address
	POP	BC		; return address
	POP	DE		; error address
	LD	SP,HL
	PUSH	DE		; error address
	PUSH	BC		; return address
	LD	HL,PPC
	JP	UNT_C

; Very similar to READ-3 except the following:
; - uses RETADDR instead of DATADD
; - reading past data after END PROC or RETURN is an error (Q)
; - data is evaluated in the local context, assigment is done in outer context
ENDP_3:	RST	$20		; advance past TO or comma
	RST	$30
	DEFW	L1C1F		; CLASS-01, prepare assignment
	LD	HL,(ERR_SP)	; save ERR-SP
	PUSH	HL
	LD	HL,6
	ADD	HL,SP
	LD	(ERR_SP),HL	; temporarily restore ERR-SP to local context
	RST	$18
	LD	(X_PTR),HL	; save CH-ADD
	LD	HL,(RETADDR)
	CALL	TEMP_PTR2
	LD	A,(FLAGS)
	PUSH	AF
	RST	$30
	DEFW	L24FB		; SCANNING
	LD	(MEMBOT),A	; save delimiter
	POP	AF
	POP	HL
	LD	(ERR_SP),HL	; restore ERR-SP to outer context
	RST	$30
	DEFW	L1C59 + 5	; VAL_FET_2 + 5
	RST	$20
	LD	(RETADDR),HL
	LD	HL,(X_PTR)
	LD	(IY+X_PTR+1-ERR_SP),0
	CALL	TEMP_PTR2
	CP	","
	JR	NZ,ENDP_SW
	XOR	(IY+MEMBOT-ERR_NR)	; compare delimiter
	JR	Z,ENDP_3
	JP	ERROR_Q		; Q Parameter error


; Discard local variables before RETURN
RETURN_CONT:
	EX	(SP),HL
	PUSH	BC
	CALL	CALLCTX
	CP	MM
	JR	Z,RETURN_GS
	CP	PROC_M
	JR	NZ,ENDP_SW		; TODO: consider other contexts
; returning from a PROC
	RST	$18
	JP	ENDPROC
;; alternative with tailcall
;;	PUSH	HL
;;	RST	$18			; fetch character after RETURN
;;	POP	HL
;;	CP	":"
;;	JR	NZ,RETPROC		; just return from PROC
;;	PUSH	HL
;;	RST	$20			; fetch instruction token after RETURN :
;;	POP	HL
;;	CP	PROC_T
;;	JR	NZ,RETPROC		; not a tail call
; tail call
;;	DEC	HL
;;	DEC	HL
;;	LD	(ERR_SP),HL
;;	PUSH	HL
;;	CALL	DEREF			; dereference
;;	POP	HL
;;	LD	DE,-9
;;	ADD	HL,DE			; HL pointing to PROC frame marker
;;	POP	DE			; return address
;;	POP	BC			; error address
;;	LD	SP,HL			; clear local variables and loops
;;	RST	$20			; advance past PROC
;;	PUSH	BC			; stack error address
;;	JP	T_PROC

; RETURN from GO SUB
RETURN_GS:
	POP	BC
	POP	DE
RET_E:	DEC	HL
	DEC	HL
	DEC	HL
	LD	SP,HL
	EX	DE,HL
	LD	DE,L1F23 + 2	; RETURN + 2
	PUSH	DE
	JP	SWAP		; RETURN again

; Find calling context (GO SUB or PROC)
CALLCTX:CALL	SKIP_LL
	SCF
RET_L:	CALL	NC,LOC_L
	OR	A
	JR	Z,ERROR7
	CP	REPEAT_M
	JR	Z,RET_L
	CP	WHILE_M
	JR	Z,RET_L
	RET

ERROR7:	RST	$30
	DEFW	REP7		; 7 Missing PROC or GO SUB

DISPLAY:CALL	STACKSWAP
	RST	$30
	DEFW	L1E94		; FIND_INT1
	CP	$03
	JR	NC,ERROR_B_NC
DISP:	LD	A,(S_MODE)
	AND	$F8
	RRCA
	RRCA
	RRCA
	CP	C
	RET	Z
	CCF
	SBC	A,0
	RRA
	RL	C
	LD	B,0
	LD	HL,DISPTAB
	ADD	HL,BC
	LD	C,(HL)
	ADD	HL,BC
	LD	C,$FF
	IN	A,(C)
	IN	B,(C)
	OR	B
	INC	A
	JR	Z,ERROR_J
	JP	(HL)

ERROR_J:RST	$30
	DEFW	L15C4		; J Invalid I/O device

YIELD:	CALL	STACKSWAP
	RST	$30
	DEFW	L2070			; STR-ALTER + 4
	CALL	CHECK_END
	JR	C,YIELD_R
	LD	HL,(CURCHL)
	INC	HL
	INC	HL
	INC	HL
	INC	HL
	LD	A,(HL)
	CP	"X"
	INC	HL
	JR	NZ,ERROR_J
YIELD_X:LD	A,(BANK_M)
	OR	A
	JR	NZ,YIELD_C
	INC	HL
	EX	AF,AF'
	LD	A,(HL)
	PUSH	BC
	CALL	SWAPIN
	POP	BC
	RET
YIELD_C:EX	AF,AF'
	PUSH	BC
	CALL	SWAPOUT
	POP	BC
	RET

TURBO_X:RST	$30
	DEFW	L1E94		; FIND_INT1
	CP	$10
ERROR_B_NC:
	JP	NC,ERROR_B	; B Integer out of range
	LD	BC,$8E3B	; ZX PRISM control port, also works with the ZX UNO
	OUT	(C),A
	JP	SWAP

YIELD_R:LD	HL,(CHANS)
	LD	BC,$0014
	ADD	HL,BC			; skip system channels
	LD	C,$04
YIELD_L:EX	DE,HL
	LD	HL,(PROG)
	SCF
	SBC	HL,DE
	RET	Z
	EX	DE,HL
	PUSH	HL
	ADD	HL,BC
	LD	A,(HL)
	INC	HL
	CP	"X"
	JR	Z,YIELDX
	ADD	HL,BC
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
YIELD_N:POP	HL
	ADD	HL,DE
	JR	YIELD_L

YIELDX:	CALL	YIELD_X
	LD	DE,$000B
	JR	YIELD_N

DISPTAB:DEFB	DISP10 - $
	DEFB	DISP20 - $
	DEFB	DISP01 - $
	DEFB	DISP21 - $
	DEFB	DISP02 - $
	DEFB	DISP12 - $

DISP02:	CALL	DISPALLOC
DISP12:	CALL	REFRAME
	LD	HL,SCALEX + 2
	INC	(HL)
	LD	HL,K_WIDTH
	SLA	(HL)
	DEC	(HL)
	LD	L,S_WIDTH-$5B00
	SLA	(HL)
	DEC	(HL)
	LD	HL,$6000
	LD	DE,$6001
	LD	BC,$17FF
	LD	(HL),L
	LDIR
	LD	HL,S_MODE
	LD	A,(HL)
	AND	$07
	OR	$10
	LD	(HL),A
	LD	HL,S_POSN
	SLA	(HL)
	DEC	(HL)
	INC	HL
	INC	HL
	SLA	(HL)
	DEC	(HL)
	LD	A,(BORDCR)
	CPL
	AND	$38
	OR	$06
	OUT	($FF),A
	RET

DISP01:	CALL	DISPALLOC
	CALL	DISP01A
DISPC1:	LD	A,2
DISPC2:	OUT	($FF),A
	ADD	A,A
	ADD	A,A
	LD	HL,S_MODE
	XOR	(HL)
	AND	$F8
	XOR	(HL)
	LD	(HL),A
	RET

DISP10:	LD	HL,$6000
	LD	DE,$5800
	LD	BC,$0100
	LDIR
	INC	B
	LD	H,$68
	LDIR
	INC	B
	LD	H,$70
	LDIR
	CALL	DISPFREE
DISPC0:	XOR	A
	JR	DISPC2
DISP20:	CALL	DISPFREE
	LD	HL,$5800
	LD	BC,$02FF
	CALL	DISP2X
	JR	DISPC0
DISP21:	LD	HL,$6000
	LD	BC,$17FF
	CALL	DISP2X
	JR	DISPC1

DISP2X:	LD	D,H
	LD	A,(BORDCR)
	AND	$38
	LD	E,A
	RRCA
	RRCA
	RRCA
	OUT	($FE),A
	XOR	$07
	OR	E
	LD	E,1
	LD	(BORDCR),A
	LD	(HL),A
	LDIR
	LD	HL,K_WIDTH
	SRL	(HL)
	INC	(HL)
	LD	L,S_WIDTH-$5B00
	SRL	(HL)
	INC	(HL)
REFRAME:LD	HL,INIT_5B00+ORIGX-$5B00
	LD	DE,ORIGX
	LD	BC,4*5
	LDIR
	LD	L,E
	LD	H,D
	INC	DE
	LD	(HL),0
	LD	BC,2*5-1
	LDIR
	RET

DISP01A:LD	HL,$5800
	LD	DE,$6000
	CALL	DISPATT
	CALL	DISPATT
DISPATT:LD	A,8
	LD	B,L
	LD	C,L
DISPAL:	INC	B
	LDIR
	DEC	H
	DEC	A
	JR	NZ,DISPAL
	INC	H
	RET

DISPALLOC:
	LD	DE,(CHANS)
	LD	HL,$7800
	AND	A
	SBC	HL,DE
	EX	DE,HL
	LD	C,E
	LD	B,D
	RST	$30
	DEFW	L1655		; MAKE-ROOM
	INC	DE
	LD	(CHANS),DE
	RET

DISPFREE:
	LD	HL,(CHANS)
	LD	DE,CHINFO
	RST	$30
	DEFW	L19E5		; RECLAIM-1
	LD	(CHANS),HL
	RET

STEP:	LD	A,$07		; STEP control
	RST	$10
	RST	$30
	DEFW	L1E94		; FIND-INT1
	RST	$10
	LD	A,(TV_FLAG)
	RRCA
	LD	B,$01
	LD	A,(K_STATE)
	JR	C,K_STEP
	LD	B,$02
	LD	A,(S_STATE)
K_STEP:	LD	HL,KS_PERM
	ADD	A,A
	ADD	A,A
	SBC	A,A
	XOR	(HL)
	AND	B
	XOR	(HL)
	LD	(HL),A
STP_SW:	JP	SWAP

G_ONERR:CP	GOTO_T
	JR	Z,T_ONERR
; ON ERROR GO SUB
	LD	BC,7*3 + 20	; 3 numeric locals and a GO SUB
	RST	$30
	DEFW	L1F05		; check available memory
	POP	DE		; return address

	LD	A,(SUBPPC)
	PUSH	AF
	INC	SP
	LD	HL,(PPC)
	PUSH	HL		; stack RETURN pointer
	LD	BC,0

	PUSH	BC
	INC	SP
	PUSH	HL
	PUSH	BC		; stack line number
	LD	HL,$3E00 + "l"	; local numeric variable
	PUSH	HL

	PUSH	BC
	PUSH	AF
	INC	SP
	PUSH	BC		; stack statement number
	LD	L,"s"		; local numeric variable
	PUSH	HL

	PUSH	BC
	EX	AF,AF'
	PUSH	AF
	INC	SP
	PUSH	BC		; stack error code
	LD	L,"e"		; local numeric variable
	PUSH	HL
	PUSH	DE		; return address
	LD	(ERR_SP),SP

; ON ERROR GO TO
T_ONERR:RST	$20		; advance past GO TO/GO SUB
	RST	$30
	DEFW	L24FB + 1	; SCANNING + 1 (numeric result ensured by syntax checker)
	LD	HL,L1B7D	; STMT-R-1, TODO: TRACE support
	PUSH	HL
	LD	HL,L1E67	; GO-TO
	PUSH	HL
STP_SW2:JR	STP_SW

ONERROR:CALL	SYNTAX_Z
	JR	Z,ONERR_S
	CP	STOP_T
	POP	BC		; save return address
	POP	HL		; discard error address
	LD	HL,L1303	; MAIN-4
	JR	Z,S_ERRSP	; on stop, simply replace error address
	LD	HL,(CH_ADD)
	LD	DE,(PROG)
	SBC	HL,DE		; CF clear at this point
	LD	(ERRPTR),HL	; store PROG offset of current position
ONERR_C:LD	HL,ONERR	; ON error address
S_ERRSP:PUSH	HL		; set error address
	PUSH	BC
ONERR_S:CP	STOP_T
	JR	Z,ONERR2
	CP	CONTINUE_T
	JR	Z,ONERR2
	SUB	GOTO_T
	JR	Z,ONERR_G
	DEC	A
	JR	NZ,ERRC1
ONERR_G:RST	$20		; skip over GO TO or GO SUB
	RST	$30
	DEFW	L1C82		; CLASS-06, expect numeric
	DEFB	$3E		; LD A,skip one byte
ONERR2:	RST	$20		; skip over STOP or CONTINUE
	CALL	CHECK_END
	JR	STP_SW2


ONERR_DO:
N_ONERR:LD	HL,L1303	; MAIN-4
	PUSH	HL
	LD	HL,ERR_NR
	LD	A,(HL)
	INC	A		; OK ?
	JR	Z,STP_SW2
	CP	9		; STOP ?
	JR	Z,STP_SW2
	EX	AF,AF'		; save error code
	LD	(HL),$FF	; reset ERR_NR
	LD	HL,(PROG)
	LD	DE,(ERRPTR)
	ADD	HL,DE
	LD	(CH_ADD),HL
	RST	$18
	CP	CONTINUE_T
	JP	NZ,G_ONERR
; ON ERROR CONTINUE
	POP	HL		; discard MAIN-4
	LD	HL,SUBPPC
	EX	AF,AF'
	CP	$15		; L BREAK into program?
	JR	Z,L_BREAK
	INC	(HL)
L_BREAK:LD	DE,NSPPC
	LD	BC,3
	LDDR
	LD	BC,L1B7D	; STMT-R-1, TODO: TRACE support
	EX	AF,AF'
	JR	ONERR_C

PAL_0:	XOR	A
PAL_X:	LD	BC,$BF3B
	LD	D,$40
	OUT	(C),D
	LD	B,$FF
	OUT	(C),A
	RET

ERROR_K:RST	$30
	DEFW	L2244		; K Invalid colour

WRITE_DO:
	RST	$30
	DEFW	X207C		; open stream
WRITE_L:RST	$30
	DEFW	L24FB		; SCANNING
	BIT	6,(IY+$01)	; numeric result?
	JR	Z,WRITE_STR	; jump, if string
	RST	$28
	DEFB	$02		; delete
	DEFB	$38		; end
	LD	BC,$0005
WRITE_O:RST	$30
	DEFW	L203C		; PR-STRING
WRITE_N:RST	$18
	CP	","
	JR	NZ,READ05
	RST	$20
	JR	WRITE_L

READ_EXT:
	POP	AF
	POP	AF		; discard return addresses
	EX	AF,AF'
	RST	$30
	DEFW	L2070		; STR-ALTER
	JR	C,ERRC1
	RST	$18
	CP	";"
ERRC1:	JP	NZ,ERROR_C
READ_L:	RST	$20		; advance past delimiter
	RST	$30
	DEFW	L1C1F		; CLASS-01
	CALL	SYNTAX_Z
	JR	Z,READ_S
	BIT	6,(IY+$01)	; numeric result?
	JR	Z,READ_STR	; jump, if string
	RST	$28
	DEFB	$38		; end
	LD	BC,5
	CALL	READ_N
	JR	NZ,ERROR_8
	LD	(STKEND),DE
READ_A:	RST	$30
	DEFW	L2AFF		; LET
READ_S:	RST	$18
	CP	","
	JR	Z,READ_L
READ05:	JP	END05

WRITE_STR:
	RST	$30
	DEFW	L2BF1		; STK-FETCH
	JR	WRITE_O

ERROR_8:RST	$30
	DEFW	L15E4		; 8 End of file

READ_STR:
	LD	A,(FLAGX)
	AND	3		; FLAGX, complete string?
	JR	NZ,READ_C	; jump, if so
	LD	BC,(STRLEN)
	LD	DE,(DEST)
	CALL	READ_ST
	JR	Z,READ_S
	EX	DE,HL
READ_W:	LD	(HL)," "
	INC	HL
	DEC	BC
	LD	A,B
	OR	C
	JR	NZ,READ_W
	JR	READ_S

READ_C:	LD	BC,1
	RST	$30
	DEFW	$0030		; BC-SPACES
	LD	(K_CUR),HL
	PUSH	HL		; start address
READ_CL:RST	$30
	DEFW	L15E6		; INPUT AD
	JR	NC,READ_E
	EX	AF,AF'
	LD	HL,(CURCHL)
	PUSH	HL
	LD	A,$FF		; system channel "R"
	RST	$30
	DEFW	L1601		; CHAN-OPEN
	EX	AF,AF'
	RST	$10		; add to string
	POP	HL
	LD	(CURCHL),HL
	LD	HL,(K_CUR)
	POP	DE
	PUSH	DE		; start
	AND	A
	SBC	HL,DE
	PUSH	HL		; length
	RST	$30
	DEFW	L1F1A		; FREE-MEM
	POP	BC		; length
	SCF
	ADC	HL,BC		; CF set if OOM
	JR	NC,READ_CL
	POP	DE		; start
READEE:	RST	$30
	DEFW	L2AB2		; STK-STO
	JR	READ_A

READ_E:	LD	HL,(K_CUR)
	POP	DE
	AND	A
	SBC	HL,DE
	LD	B,H
	LD	C,L
	JR	READEE

READ_N:	RST	$30
	DEFW	L15E6		; INPUT-AD
	JR	C,READ_B
	RET	NZ
	RST	$30
	DEFW	L1F54		; BREAK-KEY
	JR	READ_N
READ_B:	LD	(DE),A
	INC	DE
	DEC	BC
READ_ST:LD	A,B
	OR	C
	JR	NZ,READ_N
	RET

SPECTR:	LD	A,(BANK_M)
	AND	$07
	JP	NZ,ERROR_J	; not from coroutines
	LD	C,0
	CALL	DISP		; set video mode
	CALL	PAL_0		; turn off ULAplus
	LD	HL,L15AF	; initial channel info
	LD	DE,(CHANS)
	LD	BC,$0014	; do not copy the terminator!
	RST	$30
	DEFW	LDIRR		; reset channel drivers
	RES	4,(IY+FLAGS-ERR_NR)	; signal 48k mode
	JP	SPECTRUM

	INCLUDE	"play.asm"
