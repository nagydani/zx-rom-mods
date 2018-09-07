   10 REM Calculator bug test
   20 DEF FN a(x,y)=x+y: REM add
   30 DEF FN a$(x$,y$)=x$+y$: REM string add
   40 DEF FN f(n)=VAL (("FN f(n-1)" AND n>1)+("1" AND n=1))*n: REM factorial
   50 DEF FN i(x,y,n)=VAL (("FN i(x+y,x,n-1)" AND n>0)+("x" AND n=0)): REM Fibonacci
   60 READ a$
   70 IF VAL a$ THEN PRINT a$
   80 GO TO 60
   90 DATA "1/2<>0.5"
  100 DATA "1/4<>0.25"
  110 DATA "(1/61)*61<>1"
  120 DATA """X""=SCREEN$ (0,0)"
  130 DATA """2""+STR$ 0.5<>""20.5"""
  140 DATA "FN a(2,FN a(1,1))<>4"
  150 DATA "FN f(5)<>120"
  160 DATA "FN i(1,0,10)<>89"
  170 DATA "INT -65536<>-65536"
  180 DATA "FN a$(""a"",FN a$(""b"",""c""))<>""abc"""
