	INCLUDE	"../labels.asm"
	org	$8000

ELLIP:	RST	$20
	CALL	L1C7A		; read coordinate pair
	CALL	L3293		; RE-ST-TWO
	RST	$20
	CALL	L1C7A		; read semiaxes
	CALL	L3293		; RE-ST-TWO

	RST	$28		; calculate
	DEFB	$C1		; store ry
	DEFB	$02		; delete
	DEFB	$C0		; store rx
	DEFB	$02		; delete
	DEFB	$01		; exchange
	DEFB	$C2		; store cx
	DEFB	$E0		; get rx
	DEFB	$0F		; add cx + rx
	DEFB	$01		; exchange
	DEFB	$C3		; store cy
	DEFB	$38		; end
	CALL	L2307		; STK-TO-BC
	CALL	L22E5		; PLOT-SUB
	LD	A,(COORDS)
	CALL	L2D28		; STACK-A x
	LD	A,(COORDS+1)
	CALL	L2D28		; STACK-A y
	RST	$28
	DEFB	$E3		; get cy
	DEFB	$03		; sub y - cy
	DEFB	$31		; dup
	DEFB	$31		; dup
	DEFB	$04		; mul SQ(y-cy)
	DEFB	$E1		; get ry
	DEFB	$31		; dup
	DEFB	$04		; mul SQ ry
	DEFB	$C1		; store rry
	DEFB	$05		; div
	DEFB	$C5		; store yy
	DEFB	$02		; delete
	DEFB	$34,$32,$00	; stk 2 (float)
	DEFB	$E1		; get rry
	DEFB	$05		; div
	DEFB	$C3		; store ddy
	DEFB	$04		; mul
	DEFB	$C1		; store dy
	DEFB	$02		; delete
	DEFB	$E2		; get cx
	DEFB	$03		; sub x - cx
	DEFB	$31		; dup
	DEFB	$31		; dup
	DEFB	$04		; mul SQ(x-cx)
	DEFB	$E0		; get rx
	DEFB	$31		; dup
	DEFB	$04		; mul SQ rx
	DEFB	$C0		; store rrx
	DEFB	$05		; div
	DEFB	$E5		; get yy
	DEFB	$0F		; add xx + yy
	DEFB	$C4		; store rr
	DEFB	$02		; delete
	DEFB	$34,$32,$00	; stk 2 (float)
	DEFB	$E0		; get rrx
	DEFB	$05		; div
	DEFB	$C2		; store ddx
	DEFB	$04		; mul
	DEFB	$C0		; store dx
	DEFB	$02		; delete
	DEFB	$38		; end

DX:	EQU	MEMBOT
DY:	EQU	DX + 5
DDX:	EQU	DY + 5
DDY:	EQU	DDX + 5
RR:	EQU	DDY + 5

	LD	B,5
	LD	HL,MEMBOT
ELL0:	PUSH	BC
	CALL	L2F9B		; PREP-ADD
	EX	AF,AF'		; save exponent
	LD	A,(HL)
	INC	HL
	LD	B,(HL)
	INC	HL
	LD	C,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	E,(HL)
	PUSH	HL
	PUSH	BC
	EXX
	POP	DE
	LD	L,A
	EXX
	EX	AF,AF'
	LD	B,A		; restore exponent
	LD	A,$81
	SUB	B		; no overflow possible here?
	CALL	L2FDD		; SHIFT-FP
	EXX
	PUSH	DE
	LD	A,L
	EXX
	POP	BC
	; store in little-endian order
	LD	(HL),A
	DEC	HL
	LD	(HL),B
	DEC	HL
	LD	(HL),C
	DEC	HL
	LD	(HL),D
	DEC	HL
	LD	(HL),E
	POP	HL
	INC	HL
	POP	BC
	DJNZ	ELL0

ELL1:	LD	BC,(DX+2)
	LD	DE,(DX)
	SRA	B
	RR	C
	RR	D
	RR	E
	AND	A
	LD	HL,(RR)
	SBC	HL,DE
	LD	(RR),HL
	LD	HL,(RR+2)
	SBC	HL,BC
	LD	(RR+2),HL
ELL1C:	LD	DE,DX+3
	LD	HL,DY+3
	LD	B,4
ELL2:	LD	A,(DE)
	CP	(HL)
	JR	NZ,ELL2C
	DEC	HL
	DEC	DE
	DJNZ	ELL2
ELL2C:	JP	C,ELL4
	INC	(IY+COORDS+1-ERR_NR)
	LD	BC,(DY+2)
	LD	DE,(DY)
	LD	HL,(RR)
	ADD	HL,DE
	LD	(RR),HL
	LD	HL,(RR+2)
	ADC	HL,BC
	LD	(RR+2),HL
	LD	HL,(DDY)
	ADD	HL,DE
	LD	(DY),HL
	LD	HL,(DDY+2)
	ADC	HL,BC
	LD	(DY+2),HL
	LD	A,(RR+3)
	ADD	A,A
	JR	NC,ELL1P
	DEC	(IY+COORDS-ERR_NR)
	LD	BC,(DDX+2)
	LD	DE,(DDX)
	LD	HL,(DX)
	AND	A
	SBC	HL,DE
	LD	(DX),HL
	EX	DE,HL
	LD	HL,(DX+2)
	SBC	HL,BC
	LD	(DX+2),HL
	LD	B,H
	LD	C,L
	LD	HL,(RR)
	AND	A
	SBC	HL,DE
	LD	(RR),HL
	LD	HL,(RR+2)
	SBC	HL,BC
	LD	(RR+2),HL
	LD	BC,(DDX+2)
	LD	DE,(DDX)
	SRA	B
	RR	C
	RR	D
	RR	E
	AND	A
	LD	HL,(RR)
	SBC	HL,DE
	LD	(RR),HL
	LD	HL,(RR+2)
	SBC	HL,BC
	LD	(RR+2),HL
ELL1P:	LD	BC,(COORDS)
	CALL	L22E5 + 4	; PLOT
	JP	ELL1C

ELL4:	LD	HL,$2758
	EXX
	RET
