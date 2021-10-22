RESET_I:XOR	A
	OR	(IY+CURCHL+1-ERR_NR)
	JP	Z,$0000
	RST	$30
	DEFW	L15F2	; output service routine
	RST	$10
