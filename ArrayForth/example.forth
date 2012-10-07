{block 42}
: lita { a! !+ } ; | cr
: litfor { 15 for 1 unext } ; | cr
( we don't support for inside {} )

: 1lit @p { 1 2 3 .s } ; | cr
: 2lit @p { 4 5 .s } ; | cr
400 a! 1lit !+ 2lit !+ 400 p!
