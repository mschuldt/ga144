: 1iter 9 push begin pop dup push - 10 + next ;
: 2iter 9 for pop dup push - 10 + next ;
(both are the same)
(before | >)
(after  | 0 1 .. 9 >)

: store a! 15 for pop dup push - 17 + !+ next ;
: print a! 15 for @+ unext .s ;
(# 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , )
(addr = address of beginning of 16-entry storage)
(# addr store)
(# addr print)
(| 1 2 ... 16 >)
