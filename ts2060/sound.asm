SOUND_L:RST	$20
SOUND:	RST	$30
	DEFW	L1C7A	; CLASS-08, two numbers separated by comma
	CALL	SYNTAX_Z
	JR	Z,SOUND_S
	RST	$30
	DEFW	L1E94	; FIND-INT1
	PUSH	AF
	RST	$30
	DEFW	L1E94	; FIND-INT1
	LD	E,A
	POP	AF
	CALL	OUTAY
	RST	$18
SOUND_S:CP	";"
	JR	Z,SOUND_L
	JP	END05
