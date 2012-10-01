: 1branch if 1 ; then ] 2 ;
0 1branch .s
( | 0 2 > )
c

1 1branch .s
( | 1 1 > )
c

: 2branch -if 1 ; then ] 2 ;
1 2branch .s
( | 1 2 > )
c

-1 2branch .s
( | -1 1 > )
c

: 1iter 9 push begin pop dup push - 10 + next ;
: 2iter 9 for pop dup push - 10 + next ;
( both are the same )
( before | > )
( after  | 0 1 .. 9 > )

: store a! 15 for pop dup push - 17 + !+ next ;
: print a! 15 for @+ unext .s ;
( # 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , )
( addr = address of beginning of 16-entry storage )
( # addr store )
( # addr print )
( | 1 2 ... 16 > )
