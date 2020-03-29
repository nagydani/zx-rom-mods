SCR_ALL:LD	B,$17
CL_SCR:	RES	7,(IY+BORDCR-ERR_NR)	; flashing cursor OFF
	LD	A,(S_MODE)
	AND	$F8
	JR	NZ,SCROLL
	RST	$30
	DEFW	L0E00		; CL-SCROLL
	RET
SCROLL:	INC	B
	RST	$30
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
	RST	$30
	DEFW	L0E44		; CL-LINE
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
	RST	$30
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
	JR	C,ERROR_5
	RET	NZ
	BIT	4,(IY+$02)
	JR	Z,POSCR2
	LD	E,(IY+$2D)
	DEC	E
	JR	Z,POSCR3
	XOR	A
	RST	$30
	DEFW	L1601		; CHAN-OPEN
	LD	SP,(LIST_SP)
	RES	4,(IY+$02)	; end of AUTOLIST
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
	RST	$30
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
	RST	$30
	DEFW	L1601		; CHAN-OPEN
	LD	B,2
	CALL	CLLINE
	POP	AF
	LD	(P_FLAG),A
	POP	HL
	LD	(ATTR_T),HL
POSCR3:	CALL	SCR_ALL
	LD	BC,(S_WIDTH)
	LD	B,(IY+$31)
	INC	B
	LD	A,(S_STATE)
	AND	$40
	EX	AF,AF'
; TODO: Attribute magic PO-SCR-3A
	RET

ERROR_5:RST	$30
	DEFW	L0C86

ERROR_D:RST	$30
	DEFW	L0D00		; BREAK - CONT repeats

POSCR4:	CP	$02
	JR	C,ERROR_5
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
	RST	$30
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

DRAWAT:	LD	A,$FF
	JR	DOPLOT

SETORIG:LD	HL,ORIGX
	LD	(MEM),HL
	RET

PLOT1:	XOR	A
DOPLOT:	LD	(COORDS+1),A
	CALL	SETORIG
	RST	$28		; calculate
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
DOPLOT1:LD	HL,MEMBOT
	LD	(MEM),HL
ENDDRAW:XOR	A
	LD	(COORDS),A	; delete mask to identify clipped point
	LD	A,(S_MODE)
	CP	$10
	JR	NC,PLOT_HIRES
	EX	AF,AF'		; save S_MODE
	RST	$30
	DEFW	L2DD5		; FP-TO-A	x coordinate
	JR	C,DELPLT	; over 255
	JR	NZ,DELPLT	; negative
	LD	B,A
	LD	HL,WEST
	CALL	CLIPPX
	JR	NC,DELPLT	; clipped
	PUSH	BC
	RST	$30
	DEFW	L2DD5		; FP-TO-A	y coordinate
	POP	BC
	RET	C		; over 255
	RET	NZ		; negative
	LD	C,B
	LD	B,A
	LD	HL,NORTH
	CALL	CLIPPX
	RET	NC		; clipped
	LD	A,B
	RST	$30
	DEFW	L22AA+7		; PIXEL-ADD + 6
	CALL	SETPIX
	JR	C,PXATTR	; DRAW endpoint
	EX	AF,AF'		; restore S_MODE
	POP	BC		; remove SWAP
	CP	$08
	JR	NC,PLOT_HICOLOR
	LD	BC,L0BDB	; find and set attribute
	JR	E_PLOT

; Set attribute
; In: HL=display file address, A'=(S_MODE)
; Out: CF clear
PXATTR:	EX	AF,AF'
	AND	$F8
	JR	Z,PXATTRL
PXATTRH:SET	5,H
	PUSH	DE
	RST	$30
	DEFW	X0BE4
	POP	DE
	RES	5,H
	RET
PXATTRL:PUSH	DE
	PUSH	HL
	RST	$30
	DEFW	L0BDB		; PO-ATTR
	POP	HL
	POP	DE
	RET

DELPLT:	RST	$28
	DEFB	$02		; delete y coordinate
	DEFB	$38		; end
	RET

PLOT_HICOLOR:
	SET	5,H
	LD	BC,X0BE4	; set attribute
E_PLOT:	PUSH	BC
	JP	SWAP		; return via PO-ATTR

PLOT_HIRES:
	RST	$30
	DEFW	L2DA2		; FP-TO-BC	x coordinate
	RET	C		; over 65535
	RET	NZ		; negative
	LD	A,B
	SRL	A
	RET	NZ		; over 512
	LD	A,C
	RRA
	LD	HL,WEST
	CALL	CLIPPX
	RET	NC		; clipped
	PUSH	BC
	RST	$30
	DEFW	L2DA2		; FP-TO-BC	y coordinate
	POP	DE
	RET	C
	RET	NZ
	LD	A,B
	OR	A
	RET	NZ		; over 256
	LD	HL,NORTH
	LD	A,C
	CALL	CLIPPX
	RET	NC		; clipped
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

DRAW3:	RST	$28
	DEFB	$3D		; restack
	DEFB	$38		; end
DRAW3R:	CALL	STEPBACK
	CALL	STEPBACK
	RST	$30
	DEFW	L3293		; RE-ST-TWO
	LD	A,(DE)
	CP	(HL)
	JR	NC,DRAW3S
	LD	A,(HL)
DRAW3S:	LD	HL,5
	ADD	HL,DE
	ADD	A,(HL)
	JP	C,DRAW3DO
	LD	(STKEND),HL	; delete a

STCOORD:
DRAW2:	LD	HL,COORDX
	LD	DE,MEMBOT
	LD	BC,2*5
	LDIR			; save starting point
	RET	C
	CALL	SETORIG
	RST	$28		; calculate
	DEFB	$E3		; get SCALEY
	DEFB	$04		; multiply
	DEFB	$01		; exchange
	DEFB	$E2		; get SCALEX
	DEFB	$04		; multiply
	DEFB	$38		; end
; draw from outside the screen
	LD	A,(COORDS)
	OR	A
	JR	NZ,DRINS
	LD	BC,2*5
	RST	$30
	DEFW	L1F05		; TEST-ROOM
	LD	HL,(STKEND)
	LD	E,L
	LD	D,H
	ADD	HL,BC
	LD	(STKEND),HL
	EX	DE,HL
	DEC	DE
	DEC	HL
	LDDR
	INC	HL
	INC	HL
	BIT	7,(HL)
	JR	NZ,CLIPUP
	LD	A,(NORTH)
	ADD	A,A
	ADD	A,A
	ADD	A,A
	PUSH	AF
	RST	$30
	DEFW	L2D28		; STACK-A
	RST	$28
	DEFB	$E5		; get COORDY
	DEFB	$03		; subtract
	DEFB	$31		; duplicate
	DEFB	$36		; less-0
	DEFB	$00		; jump-true
	DEFB	NCDWN - $
	DEFB	$C5		; store COORDY
	DEFB	$02		; delete
	DEFB	$01		; exchange
	DEFB	$05		; division
	DEFB	$E5		; get COORDY
	DEFB	$04		; multiply
	DEFB	$E4		; get COORDX
	DEFB	$0F		; addition
	DEFB	$C4		; store COORDX
	DEFB	$38		; end
	POP	AF
	PUSH	AF
	RST	$30
	DEFW	L2D28		; STACK-A
	RST	$28
	DEFB	$C5		; store COORDY
	DEFB	$E5		; get COORDY
	DEFB	$E4		; get COORDX
	DEFB	$38		; end
	LD	(IY+COORDS+1-ERR_NR),$FF
	CALL	DOPLOT1
	EX	AF,AF'
	CALL	MASKPIX
	RST	$28
	DEFB	$E1		; get old COORDY
	DEFB	$03		; subtract
	DEFB	$C3		; store M3
	DEFB	$02		; delete
	DEFB	$E0		; get old COORDX
	DEFB	$03		; subtract
	DEFB	$03		; subtract
	DEFB	$01		; exchange
	DEFB	$E3		; get M3
	DEFB	$03		; subtract
	DEFB	$01		; exchange
	DEFB	$38		; end
	CALL	SETORIG
	SCF
	CALL	STCOORD
	RST	$28
	DEFB	$31		; duplicate
NCDWN:	DEFB	$02		; delete
	DEFB	$38		; end
	POP	AF
CLIPUP:
DRINS:	CALL	STEPBACK
	INC	HL
	LD	A,(S_MODE)
	AND	$F8
	XOR	$08
	LD	BC,PXDOWN
	JR	NZ,PXDLC
	LD	BC,PXDOWNHC
PXDLC:	BIT	7,(HL)
	JR	Z,DDOWN
	PUSH	AF
	LD	HL,MEMBOT+5
	RST	$30
	DEFW	L346E		; negate
	LD	BC,PXUP
	POP	AF
	OR	A
	JR	NZ,DDOWN
	LD	BC,PXUPHC
DDOWN:	PUSH	BC		; vertical step
	EX	DE,HL
	INC	HL
	LD	BC,PXRIGHT
	BIT	7,(HL)
	JR	Z,DRIGHT
	LD	HL,MEMBOT
	RST	$30
	DEFW	L346E		; negate
	LD	BC,PXLEFT
DRIGHT:	PUSH	BC		; horizontal step
	LD	BC,2*5		; dup2
	RST	$30
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
	RST	$28		; calculate
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
	LD	A,(COORDS)
	OR	A
	JR	NZ,DRAWDO	; jump, if not clipped
	POP	HL		; discard horizontal step
	POP	HL		; discard vertical step
	RST	$28
	DEFB	$02		; delete
	DEFB	$02		; delete
	DEFB	$38		; end
	JP	DRENDP

DRAWDO:	LD	BC,2*5
	ADD	HL,BC
	EX	DE,HL
	LD	HL,COORDX
	LDIR			; x2,y2 to M2,M3
	RST	$28
	DEFB	$38		; end
	CALL	STEPBACK
	RST	$30
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
	RST	$30
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
	RST	$30
	DEFW	L2D2B + 4	; STACK-BC + 4	l
	POP	BC
	RST	$30
	DEFW	L2D2B + 4	; STACK-BC + 4	t
	LD	A,(MEMBOT+1)
	ADD	A,A
	JR	NC,DRFWD
	LD	HL,MEMBOT+2*5
	RST	$30
	DEFW	L346E		; negate
DRFWD:	RST	$28		; calculate
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
	RST	$30
	DEFW	L2DA2 + 2	; FP-TO-BC	length in pixels
	PUSH	BC
	RST	$30
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
DRPIX:	RST	$28		; calculate
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
	RST	$30
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

	AND	A
	EX	AF,AF'

; CF' set if attributes need updating
; BC=length in pixels
; DE=transversal step pointer
; HL=longitudal step pointer
; BC'=transversal difference
; DE'=longitudal difference
; HL'=remainder
	EXX
BRESEN:	AND	A
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
	EX	AF,AF'
	SCF
	EX	AF,AF'
	POP	BC
	EXX
	DEC	BC
	LD	A,B
	OR	C
	EXX
	PUSH	AF
	CALL	NZ,PXLON2
	POP	AF
	LD	(COORDS2),HL
	POP	HL
	JR	NZ,BRESEN
	EXX

DRENDP:	LD	HL,COORDY
	LD	DE,(STKEND)
	LD	C,5
	LDIR
	LD	HL,COORDX
	LD	C,5
	LDIR
	LD	(STKEND),DE
	LD	A,(COORDS)
	LD	HL,(COORDS2)
	LD	L,A
	PUSH	HL
	LD	(IY+COORDS+1-ERR_NR),$FF	; signal DRAW endpoint
	CALL	ENDDRAW
	POP	HL
	LD	A,(COORDS2+1)
	CP	H
	RET	Z
	LD	A,(COORDS)
	CP	L
	RET	Z
	LD	HL,(COORDS2)
	JP	MASKPIX

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

PXDOWNHC:
	EX	AF,AF'
	CALL	C,PXATTRH
	EX	AF,AF'
PXDOWN:	INC	H
	LD	A,H
	AND	$07
	RET	NZ
	DEC	H
	EX	AF,AF'
	CALL	C,PXATTRL
	EX	AF,AF'
	LD	A,(SOUTH)
	XOR	H
	INC	H
	AND	$18
	CALL	Z,PDOWN
	LD	A,L
	ADD	$20
	LD	L,A
	RET	C
	LD	A,H
	SUB	A,$08
	LD	H,A
	RET

PDOWN:	LD	A,(SOUTH)
DDVERT:	RRCA
	RRCA
	RRCA
	XOR	L
	AND	$E0
	RET	NZ
	POP	HL		; discard return address
DABORT:	POP	HL		; discard
	POP	HL		; three
	POP	HL		; entries
	LD	(COORDS),A	; signal clipping
	RET
PUP:	LD	A,(NORTH)
	JR	DDVERT

PXUPHC:	EX	AF,AF'
	CALL	C,PXATTRH
	EX	AF,AF'
PXUP:	LD	A,H
	DEC	H
	AND	$07
	RET	NZ
	INC	H
	EX	AF,AF'
	CALL	C,PXATTRL
	EX	AF,AF'
	LD	A,(NORTH)
	XOR	H
	DEC	H
	AND	$18
	CALL	Z,PUP
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
PXRDO:	LD	A,(EAST)
	XOR	L
	AND	$1F
	JR	Z,DABORT
	INC	L
	RET
PXRLR:	EX	AF,AF'
	CALL	C,PXATTR
	EX	AF,AF'
	JR	PXRDO

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
PXLDO:	LD	A,(WEST)
	XOR	L
	AND	$1F
	JR	Z,DABORT
	DEC	L
	RET
PXLLR:	EX	AF,AF'
	CALL	C,PXATTR
	EX	AF,AF'
	JR	PXLDO

DRAW1:	RST	$28		; calculate
	DEFB	$E1		; get M1
	DEFB	$E2		; get M2
	DEFB	$38		; end
	RST	$30
	DEFW	DRAW_LINE
	RET

DRAW3DO:DEC	(HL)		; a/2
	RST	$28
	DEFB	$C4		; store a/2 to M4
	DEFB	$38
	LD	A,(HL)
	CP	$81
	JR	C,TANSKIP	; tan x = x
	RST	$28
	DEFB	$21		; tan
	DEFB	$38		; end
TANSKIP:DEC	(HL)		; tan(a/2)/2
	RST	$28		; calculate
	DEFB	$C5		; store tan(a/2)/2 to M5
	DEFB	$02		; delete tan(a/2)/2
	DEFB	$38		; end
	CALL	STEPBACK
	RST	$30
	DEFW	L3293		; RE-ST-TWO
	LD	A,(HL)
	OR	A
	JR	Z,DRAW3S1
	DEC	(HL)		; /2
DRAW3S1:EX	DE,HL
	LD	A,(HL)
	OR	A
	JR	Z,DRAW3S2
	DEC	(HL)
DRAW3S2:RST	$28
	DEFB	$C1		; store dy/2 to M1
	DEFB	$E5		; get tan(a/2)/2
	DEFB	$04		; multiply
	DEFB	$C3		; store dy*tan(a/2)/4 to M3
	DEFB	$01		; exchange
	DEFB	$C0		; store dx/2 to M0
	DEFB	$E5		; get tan(a/2)/2
	DEFB	$04		; multiply
	DEFB	$C2		; store dx*tan(a/2)/4 to M2
	DEFB	$01		; exchange
	DEFB	$E0		; get dx/2
	DEFB	$03		; subtract
	DEFB	$1B		; negate
	DEFB	$01		; exchange
	DEFB	$E1		; get dy/2
	DEFB	$0F		; addition
	DEFB	$E4		; get a/2
	DEFB	$E0		; get dx/2
	DEFB	$E3		; get dy*tan(a/2)/4
	DEFB	$0F		; addition
	DEFB	$E1		; get dy/2
	DEFB	$E2		; get dx*tan(a/2)/4
	DEFB	$03		; subtract
	DEFB	$E4		; get a/2
	DEFB	$38		; end
	CALL	DRAW3R
DRAW3J:	JP	DRAW3

CIRCLE:	RST	$28
	DEFB	$C4		; store R
	DEFB	$0F		; addition
	DEFB	$38		; end
	CALL	DRAWAT
	EX	AF,AF'
	CALL	MASKPIX
;;	CALL	PLOT1
	RST	$28
	DEFB	$E4		; get R
	DEFB	$1B		; negate
	DEFB	$E4		; get R
	DEFB	$A3		; stk-pi2
	DEFB	$E4		; get R
	DEFB	$E4		; get R
	DEFB	$A3		; stk-pi2
	DEFB	$E4		; get R
	DEFB	$E4		; get R
	DEFB	$1B		; negate
	DEFB	$A3		; stk-pi2
	DEFB	$E4		; get R
	DEFB	$1B		; negate
	DEFB	$E4		; get R
	DEFB	$1B		; negate
	DEFB	$A3		; stk-pi2
	DEFB	$38		; end
	CALL	CIRCL
CIRCL:	CALL	DRAW3
	JR	DRAW3J

; Clip pixel coordinate in A beteen (HL) and (HL+1)
CLIPPX:	RRA
	RRA
	RRA
	AND	$1F
	CP	(HL)
	CCF
	RET	NC
	RET	Z
	DEC	A
	INC	L
	CP	(HL)
	RET

	include "calculator.asm"
