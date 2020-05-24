    5 DEF FN n$(n)="0123456789ABCDEF"(n+1): DEF FN h$(n)=FN n$(INT (n/16))+FN n$(n-16*INT (n/16))
   10 DIM f(12): LET f=6+PEEK 23627+256*PEEK 23628: LET s=PEEK 1072+256*PEEK 1073
   20 FOR i=1 TO 12
   30 LET f(i)=880/2*2^((i-10)/12)
   40 FOR j=-5 TO -1
   50 LET o=i*5+j: LET a=PEEK (f+o): LET b=PEEK (s+o)
   60 PRINT BRIGHT a<>b;FN h$(a);
   70 NEXT j: PRINT : NEXT i
