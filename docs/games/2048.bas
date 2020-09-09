   10 LET b$="+----"*4+"+"+CHR$ 13
   20 PRINT (b$+("|    "*4+"|"+CHR$ 13)*4)*4+b$
   30 DIM b(4,4)
   40 REPEAT
   50  PROC drop()
   60  REPEAT
   70   LET action=0
   80   REPEAT
   90    LET k=CODE INKEY$
  100   UNTIL k>=8 AND k<=12
  110   IF k<>12 THEN PROC tilt(k-8)
  150   UNTIL action OR k=12
  160   PROC board()
  170 UNTIL k=12
  180 RUN 

  190 @drop()
  200  LOCAL x,y,k
  210  FOR i=1 TO 4: FOR j=1 TO 4
  220    IF NOT b(i,j)
  230     LET k+=1
  240     IF RND<1/k THEN LET x=i: LET y=j
  250    END IF
  260  NEXT : NEXT 
  270  LET k=1+(RND<.5)
  280  LET b(x,y)=k
  290  PROC tile(x,y,k)
  300 END PROC

  310 @tilt(k)
  315  LOCAL i=k?(1,4,4,1),j=k?(1,4,1,4),x=k?(0,0,-1,1),y=k?(1,-1,0,0)
  320  FOR n=1 TO 4
  330   PROC slide(i,j,x,y)
  340   LET i+=y: LET j-=x
  350  NEXT 
  360 END PROC

  370 @slide(i,j,x,y)
  380  LOCAL k=i,l=j
  390  FOR n=1 TO 3
  400   LET i+=x: LET j+=y
  410   IF b(i,j)
  420    WHILE b(k,l) AND b(k,l)<>b(i,j)
  430     LET k+=x: LET l+=y
  440    END WHILE
  450    IF NOT b(k,l)
  460    LET b(k,l)=b(i,j)
  470    LET b(i,j)=0
  480    LET action=1
  490    ELSE IF b(k,l)=b(i,j) AND (i<>k OR j<>l)
  500     LET b(k,l)+=1
  510     LET k+=x: LET l+=y
  520     LET b(i,j)=0
  530     LET action=1
  540    END IF
  550   END IF
  560  NEXT 
  570 END PROC

  580 @board()
  590  FOR i=1 TO 4: FOR j=1 TO 4
  600    PROC tile(i,j,b(i,j))
  610  NEXT : NEXT 
  620 END PROC

  630 @tile(x,y,k)
  640  LOCAL k$=k?("",STR$ (2^k))
  650  LET k$=" "*(4-LEN k$)+k$
  660  PRINT AT x*5-2,y*5-4;k$
  670 END PROC
