REPORTS:EQU	$-1
	DEFM	"Missing EN"
	DEFB	$80+"D"		; S
	DEFM	"Label not foun"
	DEFB	$80+"d"		; T
	DEFM	"UNTIL without REPEA"
	DEFB	$80+"T"		; U
	DEFM	"Assertion faile"
	DEFB	$80+"d"		; V
	DEFM	"END WHILE without WHIL"
	DEFB	$80+"E"		; W
	DEFM	"END PROC without DE"
	DEFB	$80+"F"		; X
MAX_ERR:EQU	$22
ERR7TXT:DEFM	"7 Missing PROC or GO SU"
	DEFB	$80+"B"
