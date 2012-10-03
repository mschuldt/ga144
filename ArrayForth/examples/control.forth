: 1branch if 1 ; then ] 2 ;
0 1branch .s c
1 1branch .s c
: 2branch -if 1 ; then ] 2 ;
1 2branch .s c
-1 2branch .s c
: 1iter 9 push begin pop dup push - 10 + next ;
: 2iter 9 for pop dup push - 10 + next ;
1iter .s c
2iter .s c
: store a! 15 for pop dup push - 17 + !+ next ;
: print a! 15 for @+ unext .s ;
500 store
500 print
