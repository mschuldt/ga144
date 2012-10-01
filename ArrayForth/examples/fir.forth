variable input 31 cells allot
variable output 31 cells allot

input
$00000 , $0c3ef , $16a09 , $1d906 , $1ffff , $1d906 , $16a09 , $0c3ef , 
$00000 , $33c11 , $295f7 , $226fa , $20001 , $226fa , $295f7 , $33c11 , 

output
0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 
0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 

$30 org
: store output $30 - -if $10 output! then !output+ ;

: fir 0 @input+ 15 42 push taps
[ $3fd3e , 0 , $3fb3c , 0 , $00aeb , 0 , $016dd , 0 ,
$3d48d , 0 , $3b05a , 0 , $09d41 , 0 , $1ffff , 0 ,
$1ffff , 0 , $09d41 , 0 , $3b05a , 0 , $3d48d , 0 ,
$016dd , 0 , $00aeb , 0 , $3fb3c , 0 , $3fd3e , 0 ,
] drop store ;

: go io b! $30000 !b
: rep 0 input! 15 for fir next | cr

( toggle LED if counter is 0) | cr
@ -1 . + dup ! if rep ; ] then | cr
$03fff ! io b! @b -if $20000 !b rep ; ] then | cr
$30000 !b rep ;

go
