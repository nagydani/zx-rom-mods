PLAY_PC:	EQU	-8
PLAY_BC:	EQU	-6
PLAY_DE:	EQU	-4
PLAY_SP:	EQU	-2
DURATION:	EQU	0
COUNTER:	EQU	1
OCTAVE:		EQU	2
ENVELOPE:	EQU	3
BRACKETS:	EQU	4
PLAY_THREAD:	EQU	5
VOLUME:		EQU	6
PSG_CH:		EQU	7

PLAYBIT:	EQU	6

PLAY_START:
	POP	BC
	POP	DE
PLAY_LOOP:
	PUSH	DE
	PUSH	BC
	CALL	PLAY_SCAN
	POP	BC
	POP	DE
	CP	")"
	JR	Z,PLAY_LOOP
	CP	"H"
	JR	Z,PLAY_END
	CALL	AY_VOLUME0
	INC	(IX+PLAY_THREAD)
PLAY_YIELD:
	LD	HL,$0000
	ADD	HL,SP
	LD	(IX+PLAY_SP),L
	LD	(IX+PLAY_SP+1),H
PLAY_SKIP_THREAD:
	BIT	7,(IX+PLAY_THREAD)
	JR	NZ,PLAY_LAST
	DEFB	$DD
	INC	H		; IXH
PLAY_THIS:
	LD	L,(IX+PLAY_SP)
	LD	H,(IX+PLAY_SP+1)
	LD	SP,HL
	LD	A,(IX+PLAY_THREAD)
	RRCA
	JR	C,PLAY_SKIP_THREAD
	SET	PLAYBIT,(IY+FLAGS2-ERR_NR)
	RET
PLAY_LAST:
	LD	HL,FLAGS2
	BIT	PLAYBIT,(HL)
	RES	PLAYBIT,(HL)
	PUSH	AF
	PUSH	IX
	RST	$30
	DEFW	L15E6		; INPUT-AD
	POP	IX
	JR	C,PLAY_END2
	POP	AF
PLAY_CLEAN:
	LD	IX,(PLAY_ST)
	JR	NZ,PLAY_THIS
	CALL	PLAY_SILENCE
	LD	HL,$000A
	ADD	HL,SP
	LD	SP,HL
	RST	$10

PLAY_END:
	LD	A,(IX+PSG_CH)
	OR	A
	JR	Z,PLAY_CLEAN
	DEFB	$DD
	INC	H
PLAY_END2:
	LD	SP,IX
	PUSH	BC
	JR	PLAY_END

PLAY:	XOR	A
	EX	AF,AF'
	DEFB	$3E		; LD A,skip next byte
PLAY_MORE:
	RST	$20		; read next byte
	RST	$30
	DEFW	L1C8C		; CLASS-0A, string expression
	CALL	SYNTAX_Z
	JR	Z,PLAY_S
	LD	BC,$0100	; 256 bytes of stack space per channel
	RST	$30
	DEFW	L1F05		; TEST-ROOM
	RST	$30
	DEFW	L2BF1		; STK-FETCH
	EX	AF,AF'
	LD	H,A
	EX	AF,AF'
	LD	L,$0F		; PGS_CH=A', VOLUME=15
	PUSH	HL
	LD	A,H
	DEC	A
	AND	$80
	LD	H,A
	LD	L,$05	; BRACKETS=4+1
	PUSH	HL
	LD	L,$05		; ENVELOPE=0, OCTAVE=5
	PUSH	HL
	LD	L,$18		; COUNTER=0, DURATION=24
	PUSH	HL
	LD	HL,PLAY_PC
	ADD	HL,SP
	LD	(PLAY_ST),HL
	PUSH	HL
	PUSH	DE
	PUSH	BC
	LD	HL,PLAY_START
	PUSH	HL
	LD	HL,1+PSG_CH-PLAY_PC-$100
	ADD	HL,SP
	LD	SP,HL
	RST	$18		; re-read separator
PLAY_S:	EX	AF,AF'
	INC	A
	JP	Z,ERROR_C	; do not accept more than 255 channels
	EX	AF,AF'
	CP	","
	JR	Z,PLAY_MORE
	CALL	CHECK_END
	SET	PLAYBIT,(IY+FLAGS2-ERR_NR)
	XOR	A
	RST	$30
	DEFW	L1601		; CHAN-OPEN
	CALL	PLAY_SILENCE
	DEC	E
	LD	A,$38
	CALL	OUTAY
	LD	SP,(PLAY_ST)
	LD	IX,-PLAY_PC
	ADD	IX,SP
	LD	(PLAY_ST),IX
	RET

PLAY_MIDIZ:
	CALL	PLAY_NUM
	PUSH	AF
	LD	A,H
	OR	H
	JP	NZ,PLAY_ERROR
	LD	A,L
PLAY_CONTJ:
	POP	AF
	JP	PLAY_CONT

PLAY_MIDIY:
	CALL	PLAY_NUM
	PUSH	AF
	DEC	L
	LD	A,15
	CALL	PLAY_RANGE
	SET	7,L
	LD	(IX+PSG_CH),L
	JR	PLAY_CONTJ

PLAY_TIE:
	CALL	PLAY_NUM
	EX	AF,AF'
	LD	A,9
	CALL	PLAY_RANGE
	PUSH	BC
	LD	BC,DURATIONS
	ADD	HL,BC
	POP	BC
	LD	H,(HL)
	LD	A,(IX+DURATION)
	PUSH	HL		; save last note's duration
	ADD	A,H
	JP	C,PLAY_ERROR
	LD	(IX+DURATION),A
	LD	A,1
	PUSH	AF		; save note counter
	EX	AF,AF'
	CP	"_"
	JP	NZ,PLAY_TI
	POP	HL		; discard note counter
	POP	HL		; discard saved duration
	JR	PLAY_TIE

PLAY_TAB:
	DEFM	"N"
	DEFB	PLAY_SCAN - $
	DEFM	" "
	DEFB	PLAY_SCAN - $
	DEFM	"O"
	DEFB	PLAY_OCTAVE - $
	DEFM	"("
	DEFB	PLAY_REP - $
	DEFM	"H"
	DEFB	PLAY_RET - $
	DEFM	")"
	DEFB	PLAY_RET - $
	DEFM	"V"
	DEFB	PLAY_VOLUME - $
	DEFM	"U"
	DEFB	PLAY_SETENV - $
	DEFM	"W"
	DEFB	PLAY_ENVELOPE - $
	DEFM	"X"
	DEFB	PLAY_ENVDUR - $
	DEFM	"T"
	DEFB	PLAY_TEMPO - $
	DEFM	"_"
	DEFB	PLAY_TIED - $
	DEFM	"Z"
	DEFB	PLAY_Z - $
	DEFM	"Y"
	DEFB	PLAY_Y - $
	DEFM	"M"
	DEFB	PLAY_MIXER - $
	DEFM	"!"
	DEFB	PLAY_COMMENT - $
PLAY_TTAB:
	DEFM	"#"
	DEFB	PLAY_SHARP - $
	DEFM	"&"
	DEFB	PLAY_REST - $
	DEFM	"$"
	DEFB	PLAY_FLAT - $
	DEFB	0

PLAY_Z:	JR	PLAY_MIDIZ
PLAY_Y:	JR	PLAY_MIDIY
PLAY_TIED:
	JR	PLAY_TIE

PLAY_MIXER:
	CALL	PLAY_NUM
	EX	AF,AF'
	PUSH	DE
	LD	E,7
	LD	A,L
	CPL
	PUSH	BC
	JR	PLAY_AY

PLAY_ENVDUR:
	CALL	NO_MIDI
	CALL	PLAY_NUM
	EX	AF,AF'
	PUSH	DE
	LD	E,11
	LD	A,L
	PUSH	BC
	CALL	OUTAY
	INC	E
	LD	A,H
PLAY_AY:CALL	OUTAY
	POP	BC
	POP	DE
	JR	PLAY_CONT_EX2

PLAY_ENVELOPE:
	CALL	NO_MIDI
	CALL	PLAY_NUM
	EX	AF,AF'
	LD	A,7
	CALL	PLAY_RANGE
	PUSH	DE
	LD	DE,ENVELOPES
	ADD	HL,DE
	POP	DE
	LD	A,(HL)
	LD	(IX+ENVELOPE),A
PLAY_CONT_EX2:
	JR	PLAY_CONT_EX

PLAY_SETENV:
	CALL	NO_MIDI
	CALL	PLAY_NEXT
	RET	Z
	EX	AF,AF'
	LD	L,$1F
	JR	PLAY_SETVOL

PLAY_COMMENT:
	CALL	PLAY_NEXT
PLAY_RET:
	RET	Z
	CP	"!"
	JR	Z,PLAY_SCAN
	JR	PLAY_COMMENT

PLAY_REP:
	DEC	(IX+BRACKETS)
	JP	Z,PLAY_ERROR
	PUSH	DE
	PUSH	BC
	CALL	PLAY_SCAN
	POP	BC
	POP	DE
	CALL	PLAY_SCAN
	INC	(IX+BRACKETS)
	JR	PLAY_SCAN

PLAY_OCTAVE:
	CALL	PLAY_NUM
	EX	AF,AF'
	LD	A,8
	CALL	PLAY_RANGE
	LD	(IX+OCTAVE),L
	JR	PLAY_CONT_EX

PLAY_TEMPO:
	CALL	PLAY_NUM
	EX	AF,AF'
	LD	A,240
	CALL	PLAY_RANGE
	LD	A,59
	CP	L
	JR	NC,PLAY_ERROR_NC
	LD	A,L
	LD	(TEMPO),A
	JR	PLAY_CONT_EX

PLAY_TRIPLET:
	PUSH	HL
	LD	A,3
PLAY_TL:PUSH	AF
	CALL	PLAY_STEP
PLAY_TI:SET	6,(IX+PLAY_THREAD)
	CALL	NOTE
	JR	C,PLAY_NOTE
	LD	HL,PLAY_TTAB
	JR	PLAY_INDEX

PLAY_VOLUME:
	CALL	PLAY_NUM
	EX	AF,AF'
	LD	A,15
	CALL	PLAY_RANGE
PLAY_SETVOL:
	LD	(IX+VOLUME),L
PLAY_CONT_EX:
	EX	AF,AF'
	JR	PLAY_CONT

NOTE_LENGTH:
	CALL	PLAY_NUMERIC
	RET	Z
	DEC	DE
	INC	BC
	PUSH	BC
	CALL	PLAY_RANGE12
	LD	BC,DURATIONS
	LD	A,L
	ADD	HL,BC
	CP	10
	LD	A,(HL)
	LD	H,(IX+DURATION)
	LD	(IX+DURATION),A
	POP	BC
	JR	NC,PLAY_TRIPLET
PLAY_SCAN:
	CALL	PLAY_NEXT
PLAY_CONT:
	RET	Z
	CALL	NOTE
	JR	C,PLAY_NOTE
	RST	$30
	DEFW	L2D1B		; NUMERIC
	JR	NC,NOTE_LENGTH
	LD	HL,PLAY_TAB
PLAY_INDEX:
	PUSH	BC
	LD	C,A
	CALL	INDEXER
	JR	NC,PLAY_ERROR_NC
INDEXER_JP:
	LD	C,(HL)
	LD	B,$00
	ADD	HL,BC
	POP	BC
	JP	(HL)

PLAY_REST:
	PUSH	BC
	PUSH	DE
	JP	PLAY_DURATION

PLAY_SHARP:
	CALL	PLAY_STEP
	CALL	NOTE
	JR	NC,PLAY_ERROR_NC
	INC	A
	JR	PLAY_NOTE

PLAY_FLAT:
	CALL	PLAY_STEP
	CALL	NOTE
PLAY_ERROR_NC:
	JP	NC,PLAY_ERROR
	DEC	A
PLAY_NOTE:
	INC	A
PLAY_FNOTE:
	PUSH	BC
	PUSH	DE
	EX	AF,AF'
	LD	A,(IX+PSG_CH)
	CP	3
	JR	NC,PLAY_OTHER
	EX	AF,AF'
	ADD	A,A
	CP	$18
	JR	C,PLAY_LOW
	SUB	A,$18
PLAY_LOW:
	LD	C,A
	LD	A,(IX+OCTAVE)
	SBC	A,$FF
	LD	B,0
	LD	HL,NOTES
	ADD	HL,BC
	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	OR	A
	LD	B,A
	LD	A,C
	RRCA
	JR	Z,NOTE_DONE	; octave 0
	ADD	A,12
	DEC	B
	JR	Z,NOTE_DONE	; octave 1
NOTE_L:	ADD	A,12
PLAY_TOO_LOW:
	SRL	D
	RR	E
	DJNZ	NOTE_L
	JR	NC,NOTE_DONE
	INC	DE
NOTE_DONE:
	INC	B		; ignore DJNZ if too low
	BIT	4,D
	JR	NZ,PLAY_TOO_LOW
	DEC	A
	EX	AF,AF'
	LD	A,E
	LD	E,(IX+PSG_CH)
	SLA	E
	CALL	OUTAY
	INC	E
	LD	A,D
	CALL	OUTAY
	LD	D,(IX+VOLUME)
	CALL	AY_VOLUME
	LD	A,(IX+ENVELOPE)
	LD	E,13
	OR	A
	CALL	NZ,OUTAY
	LD	B,$FF
	LD	A,7
	OUT	(C),A
	IN	A,(C)
	RRCA
	RRCA
	RRCA
	LD	B,(IX+PSG_CH)
	INC	B
PLAY_NOISE:
	RRCA
	DJNZ	PLAY_NOISE
	JR	C,PLAY_DURATION
	EX	AF,AF'
	CPL
	AND	$7C
	RRCA
	RRCA
	LD	E,6
	CALL	OUTAY
PLAY_DURATION:
	XOR	A
PLAY_OTHER:
	ADD	A,A
	CALL	C,PLAY_MIDI
	LD	HL,(BEAT)	; 125 for 50Hz, 150 for 60Hz framerate
	LD	H,0
	LD	E,(IX+DURATION)
	LD	D,H
	RST	$30
	DEFW	L30A9		; HLxDE
	LD	E,(IX+COUNTER)
	AND	A
	SBC	HL,DE
PLAY_HOLD:
	PUSH	HL
	LD	BC,(FRAMES)
PLAY_HALT:
	PUSH	BC
	CALL	PLAY_YIELD
	POP	BC
	LD	HL,(FRAMES)
	AND	A
	SBC	HL,BC
	JR	Z,PLAY_HALT
	LD	A,(TEMPO)
	LD	E,A
	LD	D,0
	RST	$30
	DEFW	L30A9		; HLxDE
	EX	DE,HL
	AND	A
	POP	HL
	SBC	HL,DE
	JR	Z,PLAY_DONE
	JR	NC,PLAY_HOLD
PLAY_DONE:
	CALL	AY_VOLUME0
	POP	DE
	LD	A,L
	NEG
	LD	(IX+COUNTER),A
	BIT	6,(IX+PLAY_THREAD)
	RES	6,(IX+PLAY_THREAD)
	POP	BC
	JR	Z,PLAY_SCAN2
	POP	AF
	DEC	A
	JP	NZ,PLAY_TL
	POP	HL
	LD	(IX+DURATION),H
PLAY_SCAN2:
	JP	PLAY_SCAN

PLAY_STEP:
	CALL	PLAY_NEXT
	RET	NZ
PLAY_ERROR:
	CALL	PLAY_SILENCE
	RST	$30
	DEFW	L34E7		; A Invalid argument

PLAY_NEXT:
	LD	A,B
	OR	C
	RET	Z
	LD	A,(DE)
	INC	DE
	DEC	BC
	RET

PLAY_NUM:
	CALL	PLAY_STEP
	RST	$30
	DEFW	L2D1B		; NUMERIC
	JR	C,PLAY_ERROR
PLAY_NUMERIC:
	SUB	A,"0"
	LD	L,A
	LD	H,0
PLAYNL:	CALL	PLAY_NEXT
	RET	Z		; end-of-string
	RST	$30
	DEFW	L2D1B		; NUMERIC
	RET	C
	SUB	A,"0"
	PUSH	BC
	LD	C,L
	LD	B,H
	ADD	HL,HL		; HL * 2
	ADD	HL,HL		; HL * 4
	ADD	HL,BC		; HL * 5	note 9999*5=49995<65536
	POP	BC
	ADD	HL,HL		; HL * 10
	JR	C,PLAY_ERROR
	ADD	A,L
	LD	L,A
	JR	NC,PLAYNL
	INC	H
	JR	NZ,PLAYNL
	JR	PLAY_ERROR

PLAY_RANGE12:
	LD	A,12
PLAY_RANGE:
	CP	L
	JR	C,PLAY_ERROR
	DEC	H
	INC	H
	RET	Z
	JR	PLAY_ERROR

NO_MIDI:BIT	7,(IX+PSG_CH)
	RET	Z
	JR	PLAY_ERROR

ENVELOPES:
	DEFB	$09, $0F, $0B, $0D, $08, $0C, $0E, $0A

DURATIONS:
	; multiply by	125 for BPM with 50Hz frame counter
	;		150 for BPM with 60Hz frame counter
	DEFB	3, 6, 9, 12, 18, 24, 36, 48, 72, 96, 4, 8, 16

NOTES:	DEFW	$1C0E		; Cb0	15.43 Hz
	DEFW	$1A7B		; C0	16.35 Hz
	DEFW	$18FE		; C#0	17.32 Hz
	DEFW	$1797		; D0	18.35 Hz
	DEFW	$1644		; D#0	19.45 Hz
	DEFW	$1504		; E0	20.60 Hz
	DEFW	$13D6		; F0	21.83 Hz
	DEFW	$12B9		; F#0	23.12 Hz
	DEFW	$11AC		; G0	24.50 Hz
	DEFW	$10AE		; G#0	25.96 Hz
	DEFW	$0FBF		; A0	27.50 Hz
	DEFW	$0EDC		; A#0	29.14 Hz
	DEFW	$0E07		; B0	30.87 Hz
	DEFW	$0D3D		; C1	32.70 Hz

NOTE:	RST	$30
	DEFW	L2C8D		; ALPHA
	RET	NC
	CP	"h"
	RET	NC
	BIT	5,A
	JR	NZ,NOTE1
	CP	"H"
	RET	NC
	OR	$20
	CALL	NOTE1
	SUB	A,-12		; sets CF
	RET

NOTE1:	SUB	A,"c"
	JR	NC,NOTE2
	ADD	A,7
NOTE2:	ADD	A,A
	CP	5
	ADC	A,$FF		; sets CF
	RET

PLAY_SILENCE:
	LD	E,10
	XOR	A
	CALL	OUTAY
	DEC	E
	CALL	OUTAY
	DEC	E
	JR	OUTAY

AY_VOLUME0:
	LD	D,0
AY_VOLUME:
	LD	A,(IX+PSG_CH)
	ADD	8
	LD	E,A
	LD	A,D
OUTAY:	LD	C,$F5
	OUT	(C),E
	OUT	($F6),A
PLAY_MIDI:
	RET
