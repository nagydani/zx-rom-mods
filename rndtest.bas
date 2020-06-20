   10 LET seed=1
   20 RANDOMIZE seed
   30 LET seed=seed*75+75
   40 LET seed=seed-65537*INT (seed/65537)-1
   50 IF seed<>1 AND seed=RND*65536 THEN GO TO 30
   60 IF seed=1 THEN PRINT "Passed."
