SCR_ALL:LD	B,$17
CL_SCR:	LD	A,(S_MODE)
	AND	$F8
	JR	NZ,SCROLL
	RST	$28
	DEFW	L0E00		; CL-SCROLL
	RET
SCROLL:	INC	B
	RST	$28
	DEFW	L0E9B
SCR_L:	PUSH	BC
	PUSH	HL
	JR	SCR_S
SCR_R:	PUSH	BC
	LD	BC,$0020
	LDIR
	SET	5,H
	SET	5,D
	LD	C,$20
	DEC	HL
	DEC	DE
	LDDR
	RES	5,H
	INC	HL
	POP	BC
SCR_S:	LD	D,H
	LD	E,L
	LD	A,L
	ADD	A,$20
	LD	L,A
	JR	NC,SCR_LT
	LD	A,H
	ADD	A,$08
	LD	H,A
SCR_LT:	DJNZ	SCR_R
	POP	HL
	POP	BC
	INC	H
	LD	A,$07
	AND	H
	JR	NZ,SCR_L
	LD	A,H
	SUB	$08
	LD	H,A
	LD	B,1
CLLINE:	LD	A,(S_MODE)
	AND	$F8
	JR	NZ,SCRCLL
	RST	$28
	DEFW	L0E44
	RET

SCRCLL:	PUSH	BC
	SUB	$10
	JR	Z,CLLL
	LD	A,(ATTR_P)
	BIT	0,(IY+$02)	; TV_FLAG, upper screen
	JR	Z,CLLL
	LD	A,(BORDCR)
CLLL:	PUSH	BC
	PUSH	AF
	RST	$28
	DEFW	L0E9B		; CL-ADDR
	POP	AF
SCR_MC:	LD	E,L
	LD	D,H
	INC	DE
	LD	(HL),0
	LD	BC,$001F
	LDIR
	SET	5,H
	LD	E,L
	LD	D,H
	DEC	DE
	LD	(HL),A
	LD	C,$1F
	LDDR
	RES	5,H
	LD	C,A
	INC	H
	LD	A,$07
	AND	H
	LD	A,C
	JR	NZ,SCR_MC
	POP	BC
	LD	C,A
	LD	A,(DF_SZ)
	INC	A
	CP	B
	LD	A,C
	JR	Z,CLLLE
	DJNZ	CLLL
CLLLE:	POP	BC
	OR	A
	RET	Z
	EX	AF,AF'
	LD	A,B
	PUSH	BC
	CALL	BC32A
	EX	AF,AF'
	DEC	BC
	LD	HL,$5AFF
	LD	DE,$5AFE
	LD	(HL),A
	LDDR
	POP	BC
	RET

BC32A:	RRCA
	RRCA
	RRCA
	LD	B,A
	AND	$E0
	LD	C,A
	XOR	B
	LD	B,A
	RET

POSCR:	LD	DE,CLSET
	PUSH	DE
	LD	A,B
	BIT	0,(IY+$02)
	JP	NZ,POSCR4
	CP	(IY+$31)
	JP	C,ERROR_5
	RET	NZ
	BIT	4,(IY+$02)
	JR	Z,POSCR2
	LD	E,(IY+$2D)
	DEC	E
	JR	Z,POSCR3
	XOR	A
	RST	$28
	DEFW	L1601		; CHAN-OPEN
	LD	SP,(LIST_SP)
	RES	4,(IY+$02)
	JP	SWAP
POSCR2:	DEC	(IY+$52)
	JR	NZ,POSCR3
	LD	A,$18
	SUB	B
	LD	(SCR_CT),A
	LD	HL,(ATTR_T)
	PUSH	HL
	LD	A,(P_FLAG)
	PUSH	AF
	LD	DE,L0CF8	; scroll?
	RST	$28
	DEFW	MSG_WAIT
	OR	A
	JR	Z,SCRONE
	CP	" "
	JR	Z,ERROR_D
	CP	$E2
	JR	Z,ERROR_D
	OR	$20
	CP	"n"
	JR	Z,ERROR_D
	CP	"1"
	JR	NZ,SCRMANY
SCRONE:	LD	A,1
	LD	(SCR_CT),A
SCRMANY:LD	A,$FE		; System channel S
	RST	$28
	DEFW	L1601		; CHAN-OPEN
	LD	B,2
	CALL	CLLINE
	POP	AF
	LD	(P_FLAG),A
	POP	HL
	LD	(ATTR_T),HL
POSCR3:	CALL	SCR_ALL
	LD	BC,(K_WIDTH)
	LD	B,(IY+$31)
	INC	B
; TODO: Attribute magic PO-SCR-3A
	RET

ERROR_D:RST	$28
	DEFW	L0D00		; BREAK - CONT repeats

POSCR4:	CP	$02
	JP	C,ERROR_5
	ADD	A,(IY+$31)
	SUB	$19
	RET	NC
	NEG
	PUSH	BC
	LD	B,A
	LD	HL,(ATTR_T)
	PUSH	HL
	LD	HL,(P_FLAG)
	PUSH	HL
	RST	$28
	DEFW	L0D4D		; TEMPS
	LD	A,B

POSCR4A:PUSH	AF
	LD	HL,DF_SZ
	LD	B,(HL)
	LD	A,B
	INC	A
	LD	(HL),A
	LD	HL,S_POSN + 1
	CP	(HL)
	JR	C,POSCR4B
	INC	(HL)
	LD	B,$17
POSCR4B:CALL	CL_SCR
	POP	AF
	DEC	A
	JR	NZ,POSCR4A
	POP	HL
	LD	(IY+$57),L
	POP	HL
	LD	(ATTR_T),HL
	LD	BC,(S_POSN)
	RES	0,(IY+$02)
	CALL	CLSET
	SET	0,(IY+$02)
	POP	BC
	RET

DRAWTO:	LD	A,$FF
	DEFB	$2E		; LD L,skip next byte
PLOT1:	XOR	A
DOPLOT:	LD	(COORDS+1),A
	LD	HL,ORIGX
	LD	(MEM),HL
	CALL	CALCULATE
	DEFB	$E3		; get SCALEY
	DEFB	$04		; multiply
	DEFB	$E1		; get ORIGY
	DEFB	$0F		; add
	DEFB	$C5		; store COORDY
	DEFB	$01		; exchange
	DEFB	$E2		; get SCALEX
	DEFB	$04		; multiply
	DEFB	$E0		; get ORIGX
	DEFB	$0F		; add
	DEFB	$C4		; store COORDX
	DEFB	$38		; end
	LD	HL,MEMBOT
	LD	(MEM),HL
ENDDRAW:XOR	A
	LD	(COORDS),A	; delete mask to identify clipped point
	LD	A,(S_MODE)
	CP	$10
	JR	NC,PLOT_HIRES
	EX	AF,AF'		; save S_MODE
	RST	$28
	DEFW	L2DD5		; FP-TO-A
	RET	C
	RET	NZ
	PUSH	AF
	RST	$28
	DEFW	L2DD5		; FP-TO-A
	POP	BC
	RET	C
	RET	NZ
	CP	$C0
	RET	NC
	LD	C,B
	RST	$28
	DEFW	L22AA+6		; PIXEL-ADD + 6
	CALL	SETPIX
	RET	C		; DRAW endpoint
	EX	AF,AF'		; restore S_MODE
	INC	SP
	INC	SP		; remove SWAP
	CP	$08
	JR	NC,PLOT_HICOLOR
	LD	BC,L0BDB	; find and set attribute
	JR	E_PLOT
PLOT_HICOLOR:
	SET	5,H
	LD	BC,X0BE4	; set attribute
E_PLOT:	PUSH	BC
	JP	SWAP		; return via PO-ATTR

PLOT_HIRES:
	RST	$28
	DEFW	L2DA2		; FP-TO-BC
	RET	C
	RET	NZ
	LD	A,B
	CP	$02
	RET	NC
	PUSH	BC
	RST	$28
	DEFW	L2DA2		; FP-TO-BC
	POP	DE
	RET	C
	RET	NZ
	CP	$C0
	RET	NC
	LD	A,B
	OR	A
	RET	NZ
	CALL	PIXADD
SETPIX:	LD	B,A
	INC	B
	LD	A,$FE
PIXL:	RRCA
	DJNZ	PIXL
	LD	(COORDS),A	; save mask for DRAW
	LD	(COORDS2),HL
	INC	(IY+COORDS+1-ERR_NR)
	SCF
	RET	Z		; DRAW endpoint
MASKPIX:LD	B,A
	LD	A,(HL)
	LD	C,(IY+$57)
	BIT	0,C
	JR	NZ,PIXOVER
	AND	B
PIXOVER:BIT	2,C
	JR	NZ,PIXEND
	XOR	B
	CPL
PIXEND:	LD	(HL),A
	RET

; Pixel address from DE=x, BC=y
PIXADD:	LD	A,E
	SRL	D
	RRA
	RRA
	RRA
	RRA
	LD	L,A
	LD	A,C
	RRA
	SCF
	RRA
	AND	A
	RRA
	XOR	C
	AND	$F8
	XOR	C
	LD	H,A
	LD	A,C
	ADD	A,A
	ADD	A,A
	XOR	L
	AND	$E0
	XOR	L
	LD	L,A
	LD	A,E
	AND	$07
	RET

DRAW2:	LD	HL,COORDX
	LD	DE,MEMBOT
	LD	BC,2*5
	LDIR			; save starting point
	LD	HL,ORIGX
	LD	(MEM),HL
	CALL	CALCULATE
	DEFB	$E3		; get SCALEY
	DEFB	$04		; multiply
	DEFB	$01		; exchange
	DEFB	$E2		; get SCALEX
	DEFB	$04		; multiply
	DEFB	$38		; end
	CALL	STEPBACK
	INC	HL
	LD	BC,PXDOWN
	BIT	7,(HL)
	JR	Z,DDOWN
	LD	HL,MEMBOT+5
	RST	$28
	DEFW	L346E		; negate
	LD	BC,PXUP
DDOWN:	PUSH	BC		; vertical step
	EX	DE,HL
	INC	HL
	LD	BC,PXRIGHT
	BIT	7,(HL)
	JR	Z,DRIGHT
	LD	HL,MEMBOT
	RST	$28
	DEFW	L346E		; negate
	LD	BC,PXLEFT
DRIGHT:	PUSH	BC		; horizontal step
	LD	BC,2*5		; dup2
	RST	$28
	DEFW	L1F05		; TEST-ROOM
	LD	HL,(STKEND)
	LD	E,L
	LD	D,H
	ADD	HL,BC
	LD	(STKEND),HL
	EX	DE,HL
	DEC	HL
	DEC	DE
	LDDR
	CALL	CALCULATE
	DEFB	$E4		; get COORDX	dy,dx,x1
	DEFB	$0F		; add		dy,x2
	DEFB	$C4		; store COORDX
	DEFB	$02		; delete	dy
	DEFB	$E5		; get COORDY	dy,y1
	DEFB	$0F		; add		y2
	DEFB	$C5		; store COORDY
	DEFB	$02		; delete
	DEFB	$38		; end
	LD	HL,MEMBOT
	LD	(MEM),HL
	LD	BC,2*5
	ADD	HL,BC
	EX	DE,HL
	LD	HL,COORDX
	LDIR			; x2,y2 to M2,M3

	RST	$28
	DEFW	L35BF		; STK-PNTRS
	CALL	STEPBACK
	RST	$28
	DEFW	L3293		; RE-ST-TWO
	LD	(STKEND),HL	; remove dx and dy from calculator stack

	LD	A,(HL)
	INC	HL
	LD	B,(HL)
	SET	7,B		; disregard sign
	INC	HL
	LD	C,(HL)

	EX	DE,HL

	SUB	(HL)		; exponent difference
	INC	HL
	LD	D,(HL)
	SET	7,D		; disregard sign
	INC	HL
	LD	E,(HL)
	JR	C,BCSHFT
	JR	NZ,STEEP
	EX	DE,HL
	SBC	HL,BC
	ADD	HL,BC
	EX	DE,HL
	JR	NC,NSHFTDR

STEEP:	EXX
	EX	AF,AF'
	LD	HL,MEMBOT
	LD	DE,MEMBOT+5
	RST	$28
	DEFW	EXCHANGE
	LD	HL,MEMBOT+3*5
	LD	BC,5
	LDIR
	EX	AF,AF'
	POP	HL
	EX	(SP),HL		; swap transversal
	PUSH	HL		; and longitudal steps
	EXX
	LD	L,C
	LD	H,B
	EX	DE,HL
	LD	C,L
	LD	B,H
	NEG
	JR	Z,NSHFTDR

BCSHFT:	CP	$F0
	JR	NC,DSHFTL
	LD	BC,$0000
	JR	NSHFTDR

DSHFTL:	SRL	B
	RR	C
	INC	A
	JR	NZ,DSHFTL

NSHFTDR:SRL	B
	RR	C
	SRL	D
	RR	E
	PUSH	BC		; placeholder for length in pixels
	PUSH	BC		; t
	PUSH	DE		; l
	PUSH	BC
	LD	C,E
	LD	B,D
	RST	$28
	DEFW	L2D2B + 4	; STACK-BC + 4	l
	POP	BC
	RST	$28
	DEFW	L2D2B + 4	; STACK-BC + 4	t
	LD	A,(MEMBOT+1)
	ADD	A,A
	JR	NC,DRFWD
	LD	HL,MEMBOT+2*5
	RST	$28
	DEFW	L346E		; negate
DRFWD:	CALL	CALCULATE
	DEFB	$E0		; get M0	M0
	DEFB	$31		; duplicate	M0,M0
	DEFB	$27		; int		M0,int(M0)
	DEFB	$C3		; store M3	M0,int(M0)
	DEFB	$03		; subtract	frac(M0)
	DEFB	$31		; duplicate	frac(M0),frac(M0)
	DEFB	$E3		; get M3	frac(M0),frac(M0),int(M0)
	DEFB	$E2		; get M2	frac(M0),frac(M0),int(M0),M2
	DEFB	$27		; int		frac(M0),frac(M0),int(M0),int(M2)
	DEFB	$03		; subtract	frac(M0),frac(M0),int(M0)-int(M2)
	DEFB	$38		; end
	RST	$28
	DEFW	L2DA2 + 2	; FP-TO-BC	length in pixels
	PUSH	BC
	RST	$28
	DEFW	L2DA2 + 2	; FP-TO-BC	frac(M0)
	POP	BC
	OR	A
	LD	HL,$0004
	ADD	HL,SP
	LD	(HL),C
	INC	HL
	LD	(HL),B		; save length in pixels at placeholder
	JR	NZ,DRPIX
	INC	HL
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	EXX
	CALL	PXTRV		; actually longitudal step
	LD	(COORDS2),HL
DRPIX:	CALL	CALCULATE
	DEFB	$04		; multiply	l,t*frac(M0)
	DEFB	$01		; exchange	t*frac(M0),l
	DEFB	$A2		; stk-half	t*frac(M0),l,0.5
	DEFB	$E1		; get M1	t*frac(M0),l,0.5,M1
	DEFB	$31		; duplicate	t*frac(M0),l,0.5,M1,M1
	DEFB	$27		; int		t*frac(M0),l,0.5,M1,int(M1)
	DEFB	$03		; subtract	t*frac(M0),l,0.5,frac(M1)
	DEFB	$03		; subtract	t*frac(M0),l,0.5-frac(M1)
	DEFB	$31		; duplicate	t*frac(M0),l,0.5-frac(M1),0.5-frac(M1)
	DEFB	$36		; less-zero	t*frac(M0),l,0.5-frac(M1),'
	DEFB	$03		; subtract	t*frac(M0),l,0.5-frac(M1)'
	DEFB	$04		; multiply	t*frac(M0),l*(0.5-frac(M1))'
	DEFB	$0F		; addition	t*frac(M0)+l*(0.5-frac(M1))'
	DEFB	$38		; end
	RST	$28
	DEFW	L2DA2 + 2	; FP-TO-BC
	JR	Z,DRPOS
	POP	HL
	PUSH	HL
	ADD	HL,HL
	SBC	HL,BC
	JR	DRNON0

DRPOS:	LD	L,C
	LD	H,B		; HL = remainder

DRNON0:	POP	DE		; DE = l
	POP	BC		; BC = t
	EXX
	POP	BC		; BC = length in pixels
	POP	HL		; longitudal step
	POP	DE		; transversal step
	LD	A,B
	OR	C
	JR	Z,DRENDP

; BC=length in pixels
; DE=transversal step pointer
; HL=longitudal step pointer
; BC'=transversal difference
; DE'=longitudal difference
; HL'=remainder
BRESEN:	EXX
	AND	A
	SBC	HL,BC
	PUSH	HL
	LD	HL,(COORDS2)
	JR	NC,STRGHT
	POP	HL
	ADD	HL,DE
	PUSH	HL
	CALL	PXTRV
STRGHT:	PUSH	BC
	LD	A,(COORDS)
	CALL	MASKPIX
	POP	BC
	CALL	PXLON2
	LD	(COORDS2),HL
	POP	HL
	EXX
	DEC	BC
	LD	A,B
	OR	C
	JR	NZ,BRESEN

DRENDP:	LD	A,$FF
	LD	(COORDS+1),A	; signal DRAW endpoint
	LD	HL,COORDY
	LD	DE,(STKEND)
	LD	C,5
	LDIR
	LD	HL,COORDX
	LD	C,5
	LDIR
	LD	(STKEND),DE
	JP	ENDDRAW

PXTRV:	LD	HL,(COORDS2)
	EXX
	PUSH	DE
	EXX
	RET

PXLON:	LD	HL,(COORDS2)
PXLON2:	EXX
	PUSH	HL
	EXX
	RET

PXDOWN:	INC	H
	LD	A,H
	AND	$07
	RET	NZ
	LD	A,L
	ADD	$20
	LD	L,A
	RET	C
	LD	A,H
	SUB	A,$08
	LD	H,A
	RET

PXUP:	LD	A,H
	DEC	H
	AND	$07
	RET	NZ
	LD	A,L
	SUB	$20
	LD	L,A
	RET	C
	LD	A,H
	ADD	A,$08
	LD	H,A
	RET

PXRIGHT:RRC	(IY+COORDS-ERR_NR)
	RET	C
	LD	A,(S_MODE)
	CP	$10
	JR	C,PXRLR
	BIT	5,H
	JR	NZ,PXRHR
	SET	5,H
	RET
PXRHR:	RES	5,H
PXRLR:	INC	L
	RET

PXLEFT:	RLC	(IY+COORDS-ERR_NR)
	RET	C
	LD	A,(S_MODE)
	CP	$10
	JR	C,PXLLR
	BIT	5,H
	JR	Z,PXLHR
	RES	5,H
	RET
PXLHR:	SET	5,H
PXLLR:	DEC	L
	RET

DRAW64:	LD	A,$40
	JR	DRAW3C

DRAW1:	CALL	CALCULATE
	DEFB	$E1		; get M1
	DEFB	$E2		; get M2
	DEFB	$38		; end
	JP	DRAW2

DRAW3:	CALL	CALCULATE
	DEFB	$A2		; stk half
	DEFB	$04		; multiply
	DEFB	$C5		; store M5
	DEFB	$02		; delete
	DEFB	$C2		; store M2
	DEFB	$2A		; abs
	DEFB	$01		; exchange
	DEFB	$C1		; store M1
	DEFB	$2A		; abs
	DEFB	$0F		; addition
	DEFB	$E5		; get M5
	DEFB	$2A		; abs
	DEFB	$04		; multiply
	DEFB	$27		; int
	DEFB	$38		; end
	RST	$28
	DEFW	L2DD5		; FP-TO-A
	JR	C,DRAW64
	RRCA
	RRCA
	AND	$3F
	JR	Z,DRAW1
	INC	A
DRAW3C:	PUSH	AF		; number of segments
	RST	$28
	DEFW	L2D28		; STACK-A
	CALL	CALCULATE
	DEFB	$E5		; get M5	n,a/2
	DEFB	$01		; exchange	a/2,n
	DEFB	$05		; division	a/2n
	DEFB	$C3		; store M3	a/2n
	DEFB	$31		; duplicate	a/2n,a/2n
	DEFB	$0F		; addition	a/n
	DEFB	$C4		; store M4	a/n
	DEFB	$02		; delete
	DEFB	$E2		; get M2	dy
	DEFB	$E1		; get M1	dy,dx
	DEFB	$E5		; get M5	dy,dx,a/2
	DEFB	$E3		; get M3	dy,dx,a/2,a/2n
	DEFB	$03		; subtract	dy,dx,a/2-a/2n
	DEFB	$E5		; get M5	dy,dx,a/2-a/2n,a/2
	DEFB	$E3		; get M3	dy,dx,a/2-a/2n,a/2,a/2n
	DEFB	$1F		; sin		dy,dx,a/2-a/2n,a/2,sin(a/2n)
	DEFB	$01		; exchange	dy,dx,a/2-a/2n,sin(a/2n),a/2
	DEFB	$1F		; sin		dy,dx,a/2-a/2n,sin(a/2n),sin(a/2)
	DEFB	$05		; division	dy,dx,a/2-a/2n,sin(a/2n)/sin(a/2)
	DEFB	$C3		; store M3	dy,dx,a/2-a/2n,sin(a/2n)/sin(a/2)=w
	DEFB	$02		; delete	dy,dx,a/2-a/2n
	DEFB	$31		; duplicate	dy,dx,a/2-a/2n,a/2-a/2n
	DEFB	$1F		; sin		dy,dx,a/2-a/2n,sin(a/2-a/2n)
	DEFB	$01		; exchange	dy,dx,sin(a/2-a/2n),a/2-a/2n
	DEFB	$20		; cos		dy,dx,sin(a/2-a/2n),cos(a/2-a/2n)
	DEFB	$C1		; store M1	dy,dx,sin(a/2-a/2n),cos(a/2-a/2n)
	DEFB	$02		; delete	dy,dx,sin(a/2-a/2n)
	DEFB	$1B		; negate	dy,dx,-sin(a/2-a/2n)
	DEFB	$C2		; store M2	dy,dx,-sin(a/2-a/2n)
	DEFB	$02		; delete	dy,dx
	DEFB	$E3		; get M3	dy,dx,w
	DEFB	$04		; multiply	dy,dx*w
	DEFB	$01		; exchange	dx*w,dy
	DEFB	$E3		; get M3	dx*w,dy,w
	DEFB	$04		; multiply	dx*w,dy*w
	DEFB	$38		; end
	CALL	CPXMUL
	CALL	CALCULATE
	DEFB	$E4		; get M4	dx',dy',a/n
	DEFB	$20		; cos		dx',dy',cos(a/n)
	DEFB	$C3		; store M3	dx',dy',cos(a/n)
	DEFB	$02		; delete	dx',dy'
	DEFB	$E4		; get M4	dx',dy',a/n
	DEFB	$1F		; sin		dx',dy',sin(a/n)
	DEFB	$C4		; store M4	dx',dy',sin(a/n)
	DEFB	$02		; delete	dx',dy'
	DEFB	$C2		; store M2	dx',dy'
	DEFB	$02		; delete	dx'
	DEFB	$C1		; store M1	dx'
; ARC loop
ARCL:	DEFB	$E2		; get M2
	DEFB	$E3		; get M3
	DEFB	$E4		; get M4
	DEFB	$38		; end
	CALL	DRAW1
	LD	HL,(STKEND)
	DEC	HL
	LD	DE,MEMBOT+5*5-1
	LD	BC,4*5
	LDDR
	INC	HL
	LD	(STKEND),HL
	POP	BC
	DJNZ	ARCL1
	RET

ARCL1:	PUSH	BC
	CALL	CALCULATE
	DEFB	$E3		; get M3
	DEFB	$E4		; get M4
	DEFB	$38
	CALL	CPXMUL
	CALL	CALCULATE
	DEFB	$C2		; store M2
	DEFB	$02		; delete
	DEFB	$C1		; store M1
	DEFB	$33		; jump
	DEFB	ARCL - $

; Complex multiplication by M1+i*M2
CPXMUL:	CALL	CALCULATE
	DEFB	$C0		; store M0
	DEFB	$02		; delete
	DEFB	$31		; duplicate
	DEFB	$E2		; get M2
	DEFB	$04		; multiply
	DEFB	$01		; exchange
	DEFB	$E1		; get M1
	DEFB	$04		; multiply
	DEFB	$E0		; get M0
	DEFB	$E2		; get M2
	DEFB	$04		; multiply
	DEFB	$03		; subtract
	DEFB	$01		; exchange
	DEFB	$E0		; get M0
	DEFB	$E1		; get M1
	DEFB	$04		; multiply
	DEFB	$0F		; addition
	DEFB	$38		; end
	RET


	include "calculator.asm"
