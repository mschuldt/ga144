: ;  postpone exit reveal postpone [ ; immediate
: 1+  1 + ;
: 1-  1 - ;
: 2+  2 + ;
: 2-  2 - ;
: 2*  2 * ;
: 2/  2 / ;
: negate  0 swap - ;
: dnegate 0, 2swap d- ;
: page  ." sorry, can't clear screen" ;
: ['] ' postpone literal ; immediate

0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 

: store 
a! 15 for pop dup push - 17 + !+ next ;

: iter 15 for pop dup push - 17 + next ;

1 2 3 .s
: 1+  1 + ;
1+ .s
: iter 15 for pop dup push - 17 + next ;
iter
.s
