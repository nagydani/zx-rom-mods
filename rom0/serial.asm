; RS232 input service routine
RS232_I:LD	HL,SERFL
	LD	A,(HL)
	AND	A
	JR	RS232_R
	LD	(HL),0
	INC	L
	LD	A,(HL)
	SCF
	RET

; Read byte from RS232
RS232_R:CALL	BREAK
	DI
	LD	HL,(BAUD)
	LD	BC,$FFFD
	LD	A,$0E
	OUT	(C),A
	IN	A,(C)
	OR	$F0
	AND	$FB
	LD	D,A		; store output lines
	LD	B,$BF
	OUT	(C),A		; CTS low, Clear To Send
	LD	B,$FF

SER_I:	IN	A,(C)
	ADD	A,A
	JR	NC,SER_R
SER_X:	DEC	H
	JR	NZ,SER_I
	XOR	A		; timed out, no byte received
	JR	SER_N

SER_R:	IN	A,(C)		; read again
	ADD	A,A
	JR	NC,SER_X	; line noise ignored
; START bit satisfactorily received, now read the rest of the byte
	CALL	SER_B
; Full byte received in A
	SCF
SER_N:	PUSH	AF
	LD	A,D		; restore output lines
	OR	$04		; CTS high, not ready to receive more
	LD	B,$BF
	OUT	(C),A
	LD	H,L
	INC	H
	INC	H
STPB_L:	DEC	H
	JR	NZ,STPB_L	; wait out stop bit
	LD	H,L
	LD	B,$FF
; Look for an additional byte after CTS high, just in case
STRB_L:	IN	A,(C)
	ADD	A,A
	JR	NC,STRB_F
	DEC	H
	JR	NZ,STRB_L
	POP	AF
	EI
	RET
; Additional byte found
STRB_F:	IN	A,(C)
	ADD	A,A
	JR	C,STRB_L	; noise ignored
; START bit satisfactorily received, now read the rest of the byte
	CALL	SER_B
	LD	HL,SERFL
	INC	(HL)
	INC	L
	LD	(HL),A
	POP	AF
	EI
	RET

; Read byte from the serial line
SER_B:	INC	L
	INC	L
	LD	A,L
	ADD	A,3
	SRL	A
	ADD	A,L
	DEC	A
	LD	H,A		; 1.5 bit time to H
	LD	A,$80		; start bit
	DEFB	$1E		; LD E,skip next byte


RXD_L:	LD	H,L		; ( 4, 4)
; This loop needs to time	 180-54= 126 T-states for 19200 BAUD	L=8 	 177 -1.7%
; 				 369-54= 315 T-states for  9600 BAUD	L=20	 369
;				 738-54= 684 T-states for  4800 BAUD	L=43     737 -0.14%
;				1475-54=1421 T-states for  2400 BAUD	L=89	1473 -0.14%
;				2950-54=2896 T-states for  1200 BAUD	L=181	2945 -0.17%
RXD_C:	DEC	H		; (4)
	JR	NZ,RXD_C	; (7/12) 16*L-5

	LD	E,(HL)		; ( 7,11)
	LD	E,(HL)		; ( 7,18
	IN	H,(C)		; (12,30)
	RL	H		; ( 8,38)
	RRA			; ( 4,42)
	JR	NC,RXD_L	; (12,54)
	RET			; byte received in A

; RS232 output service routine
RS232_O:LD	HL,(BAUD)	; fetch BAUD rate
	LD	DE,$FEF6	; RS232 logic is inverted!
	PUSH AF			; Save the byte to send.
	LD	BC,$FFFD
	LD	A,$0E
	OUT	(C),A		; Select AY register 14 to control the RS232 port.

DTR_L:	CALL	BREAK		; Check the BREAK key, and produce error message if it is being pressed.

	IN	A,(C)		; Read status of data register.
	AND	$40		; %01000000. Test the DTR line.
	JR	NZ,DTR_L	; Jump back until device is ready for data.

	POP	AF		; Retrieve the byte to send.
	JR	SER_O		; transmission

; MIDI output service routine
MIDI_O:	LD	L,2		; MIDI timing
	LD	DE,$FAFE	; zero and one bits
SER_O:	LD	H,A		; Store the byte to send.
	LD	BC,$FFFD
	LD	A,$0E
	OUT	(C),A		; Select register 14 - I/O port.
	LD	B,$BF
	LD	A,H		; restore byte to send

	AND	A		; start bit CF=0
	LD	H,10		; 1 START, 8 DATA, 1 STOP
	DI
TXD_L:	JP	C,TXD_1		; (10, 10) branch on CF
TXD_0:	OUT	(C),D		; (12, 22)
	JR	TXD_W		; (12, 34)

TXD_1:	OUT	(C),E		; (12, 22)
	JR	TXD_W		; (12, 34)

TXD_W:	EX	AF,AF'		; ( 4, 38)
	LD	A,L		; ( 4, 42)
; This loop needs to time	 113-86=  27 T-states for MIDI		L=2	 113
;				 180-86=  94 T-states for 19200 BAUD	L=6 	 177 -1.7%
; 				 369-86= 283 T-states for  9600 BAUD	L=18	 369
;				 738-86= 652 T-states for  4800 BAUD	L=41     737 -0.14%
;				1475-86=1389 T-states for  2400 BAUD	L=87	1473 -0.14%
;				2950-86=2864 T-states for  1200 BAUD	L=179	2945 -0.17%
TXD_C:	DEC	A		; (4)
	JR	NZ,TXD_C	; (7/12) 16*L-5

	NOP			; ( 4, 46
	EX	AF,AF'		; ( 4, 50)
	JR	TXD_R		; (12, 62)
TXD_R:	CCF			; ( 4, 66)
	RRA			; ( 4, 70)
	DEC	H		; ( 4, 74)
	JR	NZ,TXD_L	; (12, 86)
	EI
	RET
