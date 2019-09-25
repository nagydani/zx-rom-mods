; FLAGS unused bits
; 4 - 128k mode

; FLAGX unused bits
; 2 - last printed token's type
; 3 - token type before the cursor
; 4 - in execution: last IF's outcome is false
; 4 - in syntax check: after THEN token
; 4 - in editing: operator mode before the cursor
; 6 - temporary stuff

; FLAGS2 unused bits
; 5 - suppress K mode, if set
; 7 - jump locations are cached

; TV_FLAG unused bits
; 1 - start echo at cursor position
; 2 - start echo at old cursor position
; 6 - echo only to cursor position

CHANZ:		EQU	$5B7F
OLDSP:		EQU	$5B81
TSTACK:		EQU	$5BFF

NEWSP:		EQU	$FFFE
NEWSTRMS:	EQU	$C000
NEWERR_SP:	EQU	$C026
NEWNEWPPC:	EQU	$C028
NEWNSPPC:	EQU	$C02A
NEW_PPC:	EQU	$C02B
NEWSUBPPC:	EQU	$C02D
NEWE_PPC:	EQU	$C02E
NEWVARS:	EQU	$C030
NEWDEST:	EQU	$C032
NEWCHANS:	EQU	$C034
NEWCURCHL:	EQU	$C036
NEWPROG:	EQU	$C038
NEWNXTLIN:	EQU	$C03A
NEWDATADD:	EQU	$C03C
NEWE_LINE:	EQU	$C03E
NEWK_CUR:	EQU	$C040
NEWCH_ADD:	EQU	$C042
NEWX_PTR:	EQU	$C044
NEWWORKSP:	EQU	$C046
NEWSTKBOT:	EQU	$C048
NEWSTKEND:	EQU	$C04A
NEWOLDPPC:	EQU	$C04C
NEWOSPCC:	EQU	$C04E
NEWFLAGX:	EQU	$C04F
NEWRAMTOP:	EQU	$C050
NEWCHINFO:	EQU	$C052

OLDCHANS:	EQU	TSTACK+NEWCHANS-NEWCHINFO
