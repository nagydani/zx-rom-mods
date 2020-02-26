; MERGE control (6 bytes)
MECTRL:	CALL	DELIDX
	JP	L08B6		; ME-CONTRL

; LOAD BASIC program and delete the index of the previous one (LD-PROG-1 jumps here)
LD_BAS:	CALL	L0802		; LD-BLOCK
; Delete index, if exists (11 bytes)
DELIDX:	CALL	CHKIDX
	RET	Z
	INC	BC
	INC	BC
	CALL	L19E8		; RECLAIM-2
	DEC	(HL)
	RET

; things to do, when there is no index (11 bytes)
NOIDX:	CALL	MKIDX		; attempt to create it
	CALL	CHKIDX		; check, if we succeeded
	JR	NZ,IDXUSE	; use it, if we did
	JP	L196E + 1	; LINE-ADDR + 1, linear search if not

; Rebuild index, if PROG has moved
IDX_RE:	EX	DE,HL		; index start to DE
	CALL	IDXLNS		; rebuild the index
	POP	HL		; restore line number
; Search for line with a given number
; In: HL target line number
; Out: HL start address of the target line (if found) or the first line after, ZF set, if line found
LINE_ADDR:
	PUSH	HL		; save line number
	CALL	CHKIDX
	JR	Z,NOIDX		; do stuff, if there is no index
	PUSH	HL		; save index start
	LD	E,(HL)
	INC	HL
	LD	D,(HL)		; first entry in the index
	LD	HL,(PROG)
	AND	A
	SBC	HL,DE		; check, if it matches PROG
	POP	HL		; restore index start
	JR	NZ,IDX_RE	; rebuild index, if needed
IDXUSE:	LD	A,B
	OR	A
	LD	A,C
	JR	NZ,CHKMID	; index long enough
	CP	5
	JR	C,IDXSHRT	; short index
CHKMID:	PUSH	HL
	POP	IX		; save interval start to IX
	SRL	B
	RRA
	LD	D,B
	LD	E,A		; save half interval length to DE
	AND	$FE
	LD	C,A		; half-size index
	ADD	HL,BC		; midpoint
	POP	BC		; line number searched
	LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	CALL	L1980		; CP-LINES
	RET	Z		; line found
	PUSH	BC		; put line number back
	PUSH	IX
	POP	HL		; restore interval start to HL
	LD	C,E
	LD	B,D
	JR	NC,IDX_LO
	RES	0,E
	ADD	HL,DE		; interval start
	INC	BC
IDX_LO:	RES	0,C
	JR	IDXUSE

; Short index, use linear search (7 bytes)
IDXSHRT:LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	JP	L196E + 4	; LINE-ADDR + 4

; Check if index exists (15 bytes)
; Out:	ZF reset, it index exists, BC index length, HL index start
CHKIDX:	LD	HL,(WORKSP)
	DEC	HL
	BIT	0,(HL)
	RET	Z
	DEC	HL
	LD	B,(HL)
	DEC	HL
	LD	C,(HL)
	AND	A
	SBC	HL,BC
	RET

; Create index, if there's enough room for it
MKIDX:	LD	DE,0
	CALL	IDXLNS
	LD	A,D
	OR	E
	RET	Z		; no empty index
	EX	DE,HL
	INC	HL		; 2 bytes for the length
	ADD	HL,HL
	LD	C,L
	LD	B,H
	RST	$30
	LD	(HL),$81
	INC	HL
	LD	(WORKSP),HL
	DEC	HL
	DEC	HL
	DEC	BC
	DEC	BC
	LD	(HL),B
	DEC	HL
	LD	(HL),C
	DEC	DE

; Index lines
; In: DE=0 for line counting or the address of the index, for building
; Out: DE = number of program lines, if used for counting
IDXLNS:	LD	HL,(PROG)
	LD	A,$3E
IDXLNL:	CP	(HL)
	RET	C
	CP	D
	JR	NC,IDXCNT
	EX	DE,HL
	LD	(HL),E
	INC	HL
	LD	(HL),D		; line pointer to index
	EX	DE,HL
IDXCNT:	INC	DE		; increment counter or advance address
	INC	HL
	INC	HL		; skip line number
	LD	C,(HL)
	INC	HL
	LD	B,(HL)		; BC = line length
	INC	HL
	ADD	HL,BC		; HL points to next line
	JR	IDXLNL


