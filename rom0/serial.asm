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
; This loop needs to time	 113-86=  27 T-states for MIDI		L=2	  27
;				 180-86=  94 T-states for 19200 BAUD	L=6 	  91 -4.2%
; 				 369-86= 283 T-states for  9600 BAUD	L=18	 283
;				 738-86= 652 T-states for  4800 BAUD	L=41     651 -0.2%
;				1475-86=1389 T-states for  2400 BAUD	L=87	1387 -0.2%
;				2950-86=2864 T-states for  1200 BAUD	L=179	2859 -0.2%
TXD_C:	DEC	A		; (4)
	JR	NZ,TXD_C	; (7/12)

	NOP			; ( 4, 46
	EX	AF,AF'		; ( 4, 50)
	JR	TXD_R		; (12, 62)
TXD_R:	CCF			; ( 4, 66)
	RRA			; ( 4, 70)
	DEC	H		; ( 4, 74)
	JR	NZ,TXD_L	; (12, 86)
	EI
	RET
