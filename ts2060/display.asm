DISPLAY:CALL	STACKSWAP
	RST	$30
	DEFW	L1E94		; FIND_INT1
	CP	$03
	JP	NC,ERROR_B
DISP:	IN	A,($FF)
	AND	7
	RRA
	CP	2
	ADC	A, $FF		; A = current video mode
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
	JP	(HL)


DISPTAB:DEFB	DISP10 - $
	DEFB	DISP20 - $
	DEFB	DISP01 - $
	DEFB	DISP21 - $
	DEFB	DISP02 - $
	DEFB	DISP12 - $

DISP02:	CALL	DISPALLOC
DISP12:	LD	HL,$6000
	LD	DE,$6001
	LD	BC,$17FF
	LD	(HL),L
	LDIR
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
	JR	DISPC2

DISP01:	CALL	DISPALLOC
	CALL	DISP01A
DISPC1:	LD	A,2
DISPC2:	OUT	($FF),A
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

