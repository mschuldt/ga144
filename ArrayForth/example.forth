0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 

: store a! 15 for pop dup push - 17 + !+ next ;
: print a! 15 for @+ unext .s ;
(addr = address of beginning of 16-entry storage)
(addr store)
(addr print -> 1 2 ... 16)

: 1iter 15 push begin pop dup push - 17 + next ;
: 2iter 15 for pop dup push - 17 + next ;
(before | >)
(after  | 1 2 .. 16 >)

11 12 mult .s
(-> 11 0 132)

-1 1 mult .s
(-> ???)

(assume that addr 400 is empty)
: 1ftchp @p { a } 400 b! !b 123 a! 400 p! ;
: 2ftchp @p { a } ;
: 3ftchp 400 b! !b 123 a! 400 p! ;
: 4ftchp ftchp2 ftchp3 ;
1ftchp .s
(| 123 >)
4ftchp .s
(| 123 >)

: lita { a! !+ } ;
: litfor { 15 for 1 unext } ;
(we don't support for inside {})
