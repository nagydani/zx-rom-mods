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

ELL1:	CALL	SDX2
ELL1C:	CALL	DXGDY
ELL2C:	JR	C,ELL3
	CALL	INCY
	LD	A,(RR+3)
	ADD	A,A
	JR	NC,ELL1P
	CALL	DECX
	CALL	ADDX2
ELL1P:	LD	BC,(COORDS)
	CALL	L22E5 + 4	; PLOT
	JR	ELL1C

ELL3:	CALL	ADX2
	CALL	ADY2
ELL3C:	LD	A,(DX+3)
	ADD	A,A
	JR	C,ELL8
	CALL	DECX
	LD	A,(RR+3)
	ADD	A,A
	JR	C,ELL3P
	CALL	INCY
	CALL	ADDY2
ELL3P:	LD	BC,(COORDS)
	CALL	L22E5 + 4	; PLOT
	JR	ELL3C
ELL8:
	LD	HL,$2758
	EXX
	RET

INCY:	INC	(IY+COORDS+1-ERR_NR)
	LD	BC,(DY+2)
	LD	DE,(DY)
	CALL	ADD2RR
	LD	HL,(DDY)
	ADD	HL,DE
	LD	(DY),HL
	LD	HL,(DDY+2)
	ADC	HL,BC
	LD	(DY+2),HL
	RET

DECX:	DEC	(IY+COORDS-ERR_NR)
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
	JR	SUB2RR

SDDY2:	LD	BC,(DDY+2)
	LD	DE,(DDY)
	JR	SDD2
SDY2:	LD	BC,(DY+2)
	LD	DE,(DY)
	JR	SDD2
SDDX2:	LD	BC,(DDX+2)
	LD	DE,(DDX)
	JR	SDD2
SDX2:	LD	BC,(DX+2)
	LD	DE,(DX)
SDD2:	SRA	B
	RR	C
	RR	D
	RR	E
SUB2RR:	AND	A
	LD	HL,(RR)
	SBC	HL,DE
	LD	(RR),HL
	LD	HL,(RR+2)
	SBC	HL,BC
	LD	(RR+2),HL
	RET

ADDY2:	LD	BC,(DDY+2)
	LD	DE,(DDY)
	JR	ADD2
ADY2:	LD	BC,(DY+2)
	LD	DE,(DY)
	JR	ADD2
ADDX2:	LD	BC,(DDX+2)
	LD	DE,(DDX)
	JR	ADD2
ADX2:	LD	BC,(DX+2)
	LD	DE,(DX)
ADD2:	SRA	B
	RR	C
	RR	D
	RR	E
ADD2RR:	LD	HL,(RR)
	ADD	HL,DE
	LD	(RR),HL
	LD	HL,(RR+2)
	ADC	HL,BC
	LD	(RR+2),HL
	RET

DXGDY:	LD	DE,DX+3
	LD	HL,DY+3
	LD	B,4
ELL2:	LD	A,(DE)
	CP	(HL)
	RET	NZ
	DEC	HL
	DEC	DE
	DJNZ	ELL2
	RET

