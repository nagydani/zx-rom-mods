	INCLUDE	"../labels.asm"
	org	64640

ELLIP:	LD	HL,$000C
	ADD	HL,SP
	LD	SP,HL
	POP	BC
	LD	HL,$202F
	AND	A
	SBC	HL,BC
	JP	NZ,L1C8A

	RST	$20
	CALL	L1C7A		; read coordinate pair
	CALL	RESTK2
	RST	$20
	CALL	L1C7A		; read semiaxes
	CALL	RESTK2

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
	LD	(IY + ELDIR - ERR_NR),$00
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
	DEFB	$A2		; stk half
	DEFB	$0F		; add
	DEFB	$31		; dup
	DEFB	$04		; mul SQ (ry+0.5)
	DEFB	$C1		; store rry
	DEFB	$05		; div
	DEFB	$C4		; store yy
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
	DEFB	$A2		; stk half
	DEFB	$0F		; add
	DEFB	$31		; dup
	DEFB	$04		; mul SQ (rx+0.5)
	DEFB	$C0		; store rrx
	DEFB	$05		; div
	DEFB	$E4		; get yy
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
ELDIR:	EQU	RR + 5

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

	LD	DE,(DDY)
	LD	BC,(DDY+2)
	LD	A,(DDY+4)
	SRA	A
	RR	B
	RR	C
	RR	D
	RR	E
	LD	HL,(DY)
	ADD	HL,DE
	LD	(DY),HL
	LD	HL,(DY+2)
	ADC	HL,BC
	LD	(DY+2),HL
	ADC	A,(IY+DY+4-ERR_NR)
	LD	(DY+4),A
	LD	DE,(DDX)
	LD	BC,(DDX+2)
	LD	A,(DDX+4)
	SRA	A
	RR	B
	RR	C
	RR	D
	RR	E
	LD	HL,(DX)
	ADD	HL,DE
	LD	(DX),HL
	LD	HL,(DX+2)
	ADC	HL,BC
	LD	(DX+2),HL
	ADC	A,(IY+DX+4-ERR_NR)
	LD	(DX+4),A

ELL1:	LD	A,(ELDIR)
ELL1N:	CALL	ELSTEP
	LD	A,(RR+3)
	ADD	A,A
	JR	C,ELL1O
	LD	A,(RR+4)
	OR	A
	JR	Z,ELL1C
ELL1O:	LD	A,(ELDIR)
	INC	A
	AND	$03
	CALL	ELSTEP
ELL1C:	LD	A,(ELDIR)
	LD	HL,ELCMP
	CALL	ELIND
	JR	C,ELL2B

ELL1P:	CALL	DO_PLOT
	JR	ELL1

ELL3:	INC	A
	AND	$03
	LD	(HL),A
	JR	NZ,ELL1N
	RET

ELL2B:	LD	A,(RR+3)
	ADD	A,A
	JR	C,ELL2O
	LD	A,(RR+4)
	OR	A
	JR	Z,ELL2P
ELL2O:	LD	A,(ELDIR)
	XOR	$02
	CALL	ELSTEP
	LD	A,(RR+3)
	ADD	A,A
	JR	C,ELL2C
	LD	A,(RR+4)
	OR	A
ELL2P:	CALL	Z,DO_PLOT
ELL2C:	LD	HL,ELDIR
	LD	A,(HL)
	RRCA
;
	LD	A,(DX+4)
	JR	NC,ELL3C
	LD	A,(DY+4)
ELL3C:
;	LD	A,(DDX+3)
;	LD	B,A
;	LD	A,(DX+3)
;	JR	NC,ELL3C
;	LD	A,(DDY+3)
;	LD	B,A
;	LD	A,(DY+3)
;ELL3C:	SRA	B
;	SUB	A,B		; a bit dirty, low precision
	ADD	A,A
	SBC	A,A
	XOR	(HL)
	AND	$02
	LD	A,(HL)
	JR	NZ,ELL3
	PUSH	AF
	INC	A
	AND	$03
	CALL	ELSTEP
	POP	AF
	CALL	ELSTEP
	JR	ELL2B

ELSTEP:	LD	HL,PXSTS
ELIND:	LD	C,A
	LD	B,0
	ADD	HL,BC
	LD	C,(HL)
	ADD	HL,BC
	JP	(HL)

PXSTS:	DEFB	INCY - $
	DEFB	DECX - $
	DEFB	DECY - $
	DEFB	INCX - $

INCX:	INC	(IY+COORDS-ERR_NR)
	LD	A,(DX+4)
	LD	BC,(DX+2)
	LD	DE,(DX)
	CALL	ADD2RR
	LD	HL,(DDX)
	ADD	HL,DE
	LD	(DX),HL
	LD	HL,(DDX+2)
	ADC	HL,BC
	LD	(DX+2),HL
	RET	NC
	INC	(IY+DX+4-ERR_NR)
	RET

INCY:	INC	(IY+COORDS+1-ERR_NR)
	LD	A,(DY+4)
	LD	BC,(DY+2)
	LD	DE,(DY)
	CALL	ADD2RR
	LD	HL,(DDY)
	ADD	HL,DE
	LD	(DY),HL
	LD	HL,(DDY+2)
	ADC	HL,BC
	LD	(DY+2),HL
	RET	NC
	INC	(IY+DY+4-ERR_NR)
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
	LD	A,(DX+4)
	SBC	A,(IY+DDX+4-ERR_NR)
	LD	(DX+4),A
	LD	B,H
	LD	C,L
	JR	SUB2RR

DECY:	DEC	(IY+COORDS+1-ERR_NR)
	LD	BC,(DDY+2)
	LD	DE,(DDY)
	LD	HL,(DY)
	AND	A
	SBC	HL,DE
	LD	(DY),HL
	EX	DE,HL
	LD	HL,(DY+2)
	SBC	HL,BC
	LD	(DY+2),HL
	LD	A,(DY+4)
	SBC	A,(IY+DDY+4-ERR_NR)
	LD	(DY+4),A
	LD	B,H
	LD	C,L
SUB2RR:	AND	A
	LD	HL,(RR)
	SBC	HL,DE
	LD	(RR),HL
	LD	HL,(RR+2)
	SBC	HL,BC
	LD	(RR+2),HL
	LD	L,A
	LD	A,(RR+4)
	SBC	A,L
	LD	(RR+4),A
	RET

ADD2RR:	LD	HL,(RR)
	ADD	HL,DE
	LD	(RR),HL
	LD	HL,(RR+2)
	ADC	HL,BC
	LD	(RR+2),HL
	ADC	A,(IY+RR+4-ERR_NR)
	LD	(RR+4),A
	RET

ELCMP:	DEFB	DXGDY1 - $
	DEFB	MDXGDY1 - $
	DEFB	DXLDY1 - $
	DEFB	MDXLDY1 - $

DXGDY1:	LD	A,(DX+4)
	ADD	A,A
	RET	C
DXGDY:	LD	DE,DX+4
	LD	HL,DY+4
	LD	B,5
ELLC:	LD	A,(DE)
	CP	(HL)
	RET	NZ
	DEC	HL
	DEC	DE
	DJNZ	ELLC
	RET

MDXGDY1:LD	A,(DY+4)
	ADD	A,A
	RET	C
MDXGDY:	CALL	MDXLDY
	CCF
	RET

DXLDY1:	LD	A,(DX+4)
	ADD	A,A
	CCF
	RET	C
DXLDY:	CALL	DXGDY
	CCF
	RET

MDXLDY1:LD	A,(DY+4)
	ADD	A,A
	CCF
	RET	C
MDXLDY:	LD	DE,DX+4
	LD	HL,DY+4
	LD	B,5
ELLMC:	LD	A,(DE)
	CPL
	CP	(HL)
	RET	NZ
	DEC	HL
	DEC	DE
	DJNZ	ELLMC
	RET

RESTK2:	RST	$28
	DEFB	$38
	LD	BC,-5
	LD	D,H
	LD	E,L
	ADD	HL,BC
	JP	L3293		; RE-ST-TWO

DO_PLOT:LD	BC,(COORDS)
	JP	L22E5 + 4	; PLOT-SUB
