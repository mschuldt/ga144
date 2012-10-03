: lita { a! !+ } ;
: litfor { 15 for 1 unext } ;
( we don't support for inside {} )

: 1lit @p { 1 2 3 .s } ;
: 2lit @p { 4 5 .s } ;
400 a! 1lit !+ 2lit !+ 400 p!
