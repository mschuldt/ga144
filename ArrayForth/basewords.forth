: ;  postpone exit reveal postpone [ ; immediate
: 1+  1 + ;
: 1-  1 - ;
: 2+  2 + ;
: 2-  2 - ;
: 2*  2 * ;
: 2/  2 / ;
: negate  0 swap - ;
: abs dup 0 < if negate then ;
: dnegate 0, 2swap d- ;
: page  ." sorry, can't clear screen" ;
: ['] ' postpone literal ; immediate
