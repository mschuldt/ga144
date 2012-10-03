( assume that addr 400 is empty )
: 1ftchp b! @p { @+ } !b 406 a! 111 ! ;
: 2ftchp b! @p { @+ } ;
: 3ftchp !b 456 a! 407 a! 222 ! ;
: 4ftchp 2ftchp 3ftchp ;
: print b! @p { .s c } !b ;
400 1ftchp 401 print 402 4ftchp 403 print
: 1lit @p { 1 2 3 .s } ;
: 2lit @p { 4 5 .s } ;
404 a! 1lit !+ 2lit !+ 400 p!

