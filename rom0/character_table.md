# Character and Token Tables

## Terminal controls

| hex | symbol | function
|:---:|:-------|:---
| 00 | nil | does nothing
| 01 | ? | TBD
| 02 | ? | TBD
| 03 | ? | TBD
| 04 | ? | TBD
| 05 | RST | temporary attributes reset
| 06 | comma | next tabulator position, with blanking
| 07 | step | next byte selects font width (4 or 8)
| 08 | BS, left | cursor one position to the left 
| 09 | FW, right| cursor one position to the right, paints with temp attrs 
| 0A | LF, down | cursor one position down
| 0B | UP, up   | cursor one position up
| 0C | FF | clear screen
| 0D | CR, enter | cursor to the first position of next row
| 0E | blink | next character flashes
| 0F | mark | next character is inverted
| 10 | ink | next byte selects foreground color
| 11 | paper | next byte selects background color
| 12 | flash | next byte selects flashing (0 off, 1 on)
| 13 | bright | next byte selects brightness (0 normal, 1 bright)
| 14 | inverse | next byte selects inverse (0 normal, 1 inverse)
| 15 | over | next byte selects overwriting (0 replace, 1 xor, 2 or)
| 16 | at | next two bytes select row and column
| 17 | tab | next two bytes select column, modulo screen width

## Diagonal block graphics

| hex | symbol | function
|:---:|:-------|:---
| 18 | ◢ | block graphics
| 19 | ◥ | block graphics 
| 1A | ◤ | block graphics
| 1B | ◣ | block graphics
| 1C | <sup>◤</sup> | block graphics
| 1D | <sub>◣</sub> | block graphics
| 1E | <sub>◢</sub> | block graphics
| 1F | <sup>◥</sup> | block graphics

## Regular printable characters

| hex | symbol | function
|:---:|:-------|:---
| 20 | | whitespace
| 21 | ! | exclamation point
| ... | ... | ...
| 5E | ↑ | upwards arrow
| ... | ... | ...
| 60 | £ | pound sign
| ... | ... | ...
| 7F | © | copyright sign

## Block graphics

| hex | symbol | function
|:---:|:-------|:---
| 80 | □ | empty block, hard blank
| ... | ... | ...
| 8F | ■ | full block

## UDG

| hex | symbol | function
|:---:|:-------|:---
| 90 | a | user defined graphics
| ... | ... | ...
| FF | Z | user defined graphics

## Tokens

| hex | symbol
|:---:|:------
| 80 | ><
| 81 | <<
| 82 | >>
| 83 | SQ
| 84 | FPEEK
| 85 | DPEEK
| 86 | STICK
| 87 | TIME$
| 88 | TIME
| 89 | ITEM
| 8A | REF
| 8B | MEM$
| 8C | FREE
| 8D | @ / LABEL / DEFPROC
| 8E | ON ERROR
| 8F | WRITE #
| 90 | DELETE
| 91 | LOCAL
| 92 | CLIP
| 93 | DISPLAY
| 94 | FPOKE
| 95 | RENUM
| 96 | SCALE
| 97 | END WHILE
| 98 | UNTIL
| 99 | TRACE
| 9A | ENDIF
| 9B | YIELD
| 9C | REPEAT
| 9D | EXIT
| 9E | WHILE
| 9F | END PROC
| A0 | STACK
| A1 | PROC
| A2 | POP
| A3 | SPECTRUM
| A4 | PLAY
| A5 | RND
| A6 | INKEY$
| A7 | PI
| A8 | FN
| A9 | POINT
| AA | SCREEN$
| AB | ATTR
| AC | AT
| AD | TAB
| AE | VAL$
| AF | CODE
| B0 | VAL
| B1 | LEN
| B2 | SIN
| B3 | COS
| B4 | TAN
| B5 | ASN
| B6 | ACS
| B7 | ATN
| B8 | LN
| B9 | EXP
| BA | INT
| BB | SQR
| BC | SGN
| BD | ABS
| BE | PEEK
| BF | IN
| C0 | USR
| C1 | STR$
| C2 | CHR$
| C3 | NOT
| C4 | BIN
| C5 | OR
| C6 | AND
| C7 | <=
| C8 | >=
| C9 | <>
| CA | LINE
| CB | THEN / ELSE
| CC | TO
| CD | STEP
| CE | DEF FN
| CF | CAT
| D0 | FORMAT
| D1 | MOVE
| D2 | ERASE
| D3 | OPEN #
| D4 | CLOSE #
| D5 | MERGE
| D6 | VERIFY
| D7 | BEEP
| D8 | CIRCLE
| D9 | INK
| DA | PAPER
| DB | FLASH
| DC | BRIGHT
| DD | INVERSE
| DE | OVER
| DF | OUT
| E0 | LPRINT
| E1 | LLIST
| E2 | STOP
| E3 | READ
| E4 | DATA
| E5 | RESTORE
| E6 | NEW
| E7 | BORDER
| E8 | CONTINUE
| E9 | DIM
| EA | REM
| EB | FOR
| EC | GO TO
| ED | GO SUB
| EE | INPUT
| EF | LOAD
| F0 | LIST
| F1 | LET
| F2 | PAUSE
| F3 | NEXT
| F4 | POKE
| F5 | PRINT
| F6 | PLOT
| F7 | RUN
| F8 | SAVE
| F9 | RANDOMIZE
| FA | IF
| FB | CLS
| FC | DRAW
| FD | CLEAR
| FE | RETURN
| FF | COPY
