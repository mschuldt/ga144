(assume that addr 400 is empty)
: 1ftchp @p { a } 400 b! !b 123 a! 400 p! ;
: 2ftchp @p { a } ;
: 3ftchp 400 b! !b 123 a! 400 p! ;
: 4ftchp ftchp2 ftchp3 ;
1ftchp .s
(| 123 >)
c
4ftchp .s
(| 123 >)

