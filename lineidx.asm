	INCLUDE	"sysvars.asm"
L196E:	EQU	$196E
L1980:	EQU	$1980
	ORG	$8000

; Test code
	call MKIDX		; create index
	ld hl,9999		; check for every possible line number
testl:	push hl			; save line number
	call L196E		; linear search
	ex (sp),hl		; save result, get line number
	push hl			; save line number
	call LINE_ADDR		; binary search
	ex (sp),hl		; save result, get line number
	ex de,hl		; line number to DE
	pop hl			; binary search result to HL
	pop bc			; linear search result to BC
	and a			; clear CF
	sbc hl,bc		; check the two results match
	ex de,hl		; line number to HL
	dec hl
	ld a,h
	or l
	jr nz,testl

; Delete index, if exists
DELIDX:	CALL	CHKIDX
	RET	Z
	PUSH	HL
	SBC	HL,SP
	LD	C,L
	LD	B,H
	POP	HL
	DEC	HL
	LDDR
	EX	DE,HL
	INC	HL
	LD	SP,HL
	POP	DE
	RET

LINE_ADDR:
	PUSH	HL
	CALL	CHKIDX
	JP	Z,L196E + 1	; LINE-ADDR + 1, linear search if no index
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
IDXSHRT:LD	A,(HL)
	INC	HL
	LD	H,(HL)
	LD	L,A
	JP	L196E + 4	; LINE-ADDR + 4

; Check if index exists
; Out:	ZF reset, it index exists, BC index length, HL index start, DE index end
CHKIDX:	LD	HL,(RAMTOP)
	LD	A,$3E
	LD	B,(HL)
	CP	B
	RET	Z
	LD	E,L
	LD	D,H
	DEC	HL
	LD	C,(HL)
	SBC	HL,BC
	RET

; Create index, if there's enough room for it
MKIDX:	LD	DE,0
	CALL	IDXLNS
	LD	HL,(STKEND)
	EX	DE,HL
	ADD	HL,HL
	LD	C,L
	LD	B,H		; BC=index length
	ADD	HL,DE
	RET	C		; no room for index
	LD	DE,$0052	; 80 bytes for stack, 2 bytes for index length
	ADD	HL,DE
	RET	C		; no room for index
	SBC	HL,SP
	RET	NC		; no room for index
	PUSH	BC
	LD	HL,$FFFF
	SBC	HL,BC		; HL = -BC-2
	ADD	HL,SP		; HL = new SP
	LD	SP,HL		; set new SP
	ADC	HL,BC
	INC	HL		; HL = old SP
	EX	DE,HL		; DE = old sp
	LD	HL,(RAMTOP)
	INC	HL
	SBC	HL,DE		; HL = size of stack to move
	LD	C,L
	LD	B,H		; BC = size of stack to move
	LD	HL,0
	ADD	HL,SP		; HL = new SP
	EX	DE,HL
	LDIR			; move stack
	POP	BC
	DEC	HL
	LD	(HL),B
	DEC	HL
	LD	(HL),C

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

