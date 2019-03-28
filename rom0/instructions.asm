	DEFB	P_PLUG - $	; G8
	DEFB	P_PLUG - $	; G2
	DEFB	P_PLUG - $	; G3
	DEFB	P_PLUG - $	; G4
	DEFB	P_PLUG - $	; G5
	DEFB	P_PLUG - $	; G6
	DEFB	P_PLUG - $	; G7
	DEFB	P_PLUG - $	; Gs8
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
	DEFB	P_PLUG - $	; EsK
	DEFB	P_PLUG - $	; EsL
	DEFB	P_PLUG - $	; sI
	DEFB	P_PLUG - $	; EP
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
	DEFB	P_PLUG - $	; EZ
	DEFB	P_PLUG - $	; EX
	DEFB	P_PLUG - $	; ER
	DEFB	P_PLUG - $	; EH
	DEFB	P_PLUG - $	; EF
	DEFB	P_PLUG - $	; EG
	DEFB	P_PLUG - $	; EO
	DEFB	P_PLUG - $	; EsI
	DEFB	P_PLUG - $	; EL
	DEFB	P_PLUG - $	; EY
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
	DEFB	CLASS2_07 - $	; TODO: makes no sense to use original
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
	CALL	ERROR
	DEFB	$0B

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
	; something useful

; instruction routines
ENDIF:	RES	6,(IY+$37)
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
	LD	DE,T_ENDIF_ELSE
	CALL	LOOK_PROG2
	LD	(NXTLIN),BC
	JR	C,ERROR_C_I	; TODO: missing END IF
	POP	BC		; discard SCAN-LOOP
	RST	$20
	JP	SWAP

ELSE:	POP	BC		; discard STMT-RET
	BIT	7,(IY+$01)
	JR	Z,ELSE_1
	BIT	6,(IY+$37)
	RES	6,(IY+$37)
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
	LD	DE,T_ENDIF
	CALL	LOOK_PROG2
	LD	(NXTLIN),BC
	JR	C,ERROR_C_J	; TODO: missing END IF
	RST	$20
	JP	SWAP

PLAY:
; unimplemented instruction, reports error, if executed
PLUG:	BIT	7,(IY+$01)
	JP	Z,SWAP
ERROR_C_J:
	JP	ERROR_C

; IF structure table
T_ENDIF_ELSE:
	DEFB	ELSE_T
	DEFB	F_ELSE - $
T_ENDIF:DEFB	ENDIF_T
	DEFB	F_ENDIF - $
	DEFB	IF_T
	DEFB	F_IF - $
	DEFB	0

F_ELSE:	LD	A,(NESTING)
	CP	1
	JR	F_ELSEC

F_ENDIF:LD	A,(NESTING)
	OR	A
	JR	Z,ERROR_C_J	; TODO: nesting error
	DEC	A
	LD	(NESTING),A
F_ELSEC:JR	NZ,EACH_COMEBACK
	EX	DE,HL
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

