0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 

: store a! 15 for pop dup push - 17 + !+ next ;
: print a! 15 for @+ unext .s ;
(addr = address of beginning of 16-entry storage)
(addr store)
(addr print -> 1 2 ... 16)

: iter 15 for pop dup push - 17 + next ;
(.s -> 1 2 ... 16)

11 12 mult .s
(-> 11 0 132)

-1 1 mult .s
(-> ???)

: ftchp1 @p { a } .s 400 b! !b 400 p! ;
: ftchp2 @p { a } ;
: ftchp3 .s 400 b! !b 400 p! ;
: ftchp4 ftchp2 ftchp3 ;
(fchp1 and fchp2 should have the same behavior)
