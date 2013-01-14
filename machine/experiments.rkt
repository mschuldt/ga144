#lang racket

(require "cegis.rkt" "state.rkt")

(pretty-display "swap only at m (x' y') (complete version)")
(optimize "a! over over nop or a and nop over or push nop over or a nop and or dup nop pop nop nop nop" #:name "roundup" #:constraint (constraint s t) #:num-bits 4 #:inst-pool `no-mem-no-p)


;;; Population count:
(optimize "@p and nop nop #xFFFF @p a! dup nop 1 a and push nop 2/ dup a nop and pop nop + push 2/ dup nop a and pop nop + push 2/ nop dup a and nop pop nop + nop push 2/ dup nop a and pop nop + push 2/ nop dup a and nop pop nop + nop push 2/ dup nop a and pop nop + push 2/ nop dup a and nop pop nop + nop push 2/ dup nop a and pop nop + push 2/ nop dup a and nop pop nop + nop push 2/ dup nop a and pop nop + push 2/ nop dup a and nop pop nop + nop push 2/ dup nop a and pop nop + push 2/ nop dup a and nop pop nop + nop push 2/ a nop and pop nop +" #:name "foo" #:slots "#xFFFF and nop nop dup @p rshift @p and - @p nop + nop + dup @p and push @p rshift @p and nop pop nop + dup @p rshift nop + @p and  dup @p @p rshift nop + dup @p rshift nop + and nop nop" #:constraint (constraint t) #:time-limit 1000)

;;; Add 32-bit number
(optimize "
dup dup or nop
a! @+ @+ nop 
+ dup @p nop 4
b! !b nop nop
2/ 2/ 2/ nop 
2/ 2/ 2/ nop 
2/ 2/ 2/ nop 
2/ 2/ 2/ nop 
2/ 2/ 2/ nop 
2/ @+ @+ nop 
+ nop + @p 5 
b! !b nop nop" #:name "add32bit" 
#:slots "
dup dup or nop
a! @+ @+ nop 
+ dup 4 nop
b! !b nop nop
2/ 2/ 2/ nop 
2/ 2/ 2/ nop 
2/ 2/ 2/ nop 
2/ 2/ 2/ nop 
2/ 2/ 2/ nop 
2/ @+ @+ nop 
+ nop + 5 
b! !b nop nop" #:mem 6 #:constraint (constraint memory) #:time-limit 1000)

;; ;;; Get the most significant bit of a number by shifting it right 17 times.
;; (pretty-display "shift right")
;; (fastest-program "2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ nop nop"
;;                    #:slots 19 #:mem 4 #:start 5 #:constraint (constraint t))
;; This one still hasn't finished yet...

;;; Averaging four numbers:
;;; (fastest-program "nop + nop + nop + 2/ nop 2/ nop nop nop"  #:slots 8 #:constraint (constraint t))
;;; This one finished in a rather uninteresting '("+ nop + nop + 2/ 2/ nop" . 21)
;;; It took 4970 seconds (about 80 minutes). Might be a useful data
;;; point, but certainly not a useful example!

;;; This is how to use sketch with init and repeat
(fastest-program "or and 2* nop and 2* nop nop" #:init "or" #:slots "_ 2* _" #:repeat 2 #:constraint (constraint t) #:time-limit 1000)
(fastest-program "or and 2* nop and 2* nop nop" #:init 1 #:slots 3 #:repeat 2 #:constraint (constraint t) #:time-limit 1000)

;; moving average: original PASS
;; (fastest-program "@p a! @p nop 0    277    b! @b !+ nop a push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 325    b! !b pop nop a! @p b! nop 277    @b !+ a nop push @p a! @p 0    0    @+ nop + nop @+ nop + nop @+ nop + nop @+ nop + nop 2/ 2/ @p nop 325    b! !b pop nop a! nop nop nop" 
;;    #:init "0 a!"
;;    #:slots "277 nop b! @b !+ nop a push 0 nop a! 0 @+ nop + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ 325 nop b! !b pop nop a! nop"
;;    #:repeat 2 #:constraint (constraint r) #:inst-pool `no-fake #:num-bits 9
;;    #:mem 4 #:comm 2 #:time-limit 1000)

;; moving average: optimized PASS
;; (fastest-program "@p a! @p nop 0    12    b! @b !+ nop a push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! @p b! nop 12    @b !+ a nop push @p a! @p 0    0    @+ nop + nop @+ nop + nop @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! nop nop nop" 
;;    #:init "dup or dup dup a! @+ nop + @+ nop + nop @+ nop + nop @+ nop + nop over a! nop nop"
;;    #:slots "@ - 1 nop + nop + 12 b! @b dup nop !+ a 3 nop and a! nop + dup 2/ 2/ 14 b! !b nop nop"
;;    #:repeat 2 #:constraint (constraint r) #:inst-pool `no-fake #:num-bits 4 
;;    #:mem 4 #:comm 2 #:time-limit 1000
;;    #:name "check")

(pretty-display "fastest3--no sketch")
(fastest-program3 "@p a! @p nop 0    12    b! @b !+ nop a push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! @p b! nop 12    @b !+ a nop push @p a! @p 0    0    @+ nop + nop @+ nop + nop @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! nop nop nop" 
   #:init "dup or dup nop a! @+ nop + @+ nop + nop @+ nop + nop @+ nop + _ _ _ _ _ _"
   #:slots 36 #:repeat 2 #:constraint (constraint r) #:inst-pool `no-fake #:num-bits 4 
   #:mem 4 #:comm 2 #:time-limit 1000
   #:name "no-sketch")

(pretty-display "fastest1--with sketch")
(fastest-program "@p a! @p nop 0    12    b! @b !+ nop a push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! @p b! nop 12    @b !+ a nop push @p a! @p 0    0    @+ nop + nop @+ nop + nop @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! nop nop nop" 
   #:init "dup or dup dup a! @+ nop + @+ nop + nop @+ nop + nop @+ nop + _ _ _ _ _"
   #:slots "_ _ _ _
_ _ _ 12 
b! @b _ _ 
_ _ _ _ 
_ _ _ _ 
dup 2/ 2/ 14 
b! !b _ _"
   #:repeat 2 #:constraint constraint-none #:inst-pool `no-fake #:num-bits 4 
   #:mem 4 #:comm 2 #:time-limit 1000
   #:name "with-sketch")

(pretty-display "fastest1--more sketch")
(fastest-program "@p a! @p nop 0    12    b! @b !+ nop a push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! @p b! nop 12    @b !+ a nop push @p a! @p 0    0    @+ nop + nop @+ nop + nop @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! nop nop nop" 
   #:init "dup or dup dup a! @+ nop + @+ nop + nop @+ nop + nop @+ nop + nop over a! nop nop"
   #:slots "_ _ _ _ 
_ _ _ 12 
b! @b dup _ 
_ _ _ _ 
_ _ _ _ 
dup 2/ 2/ _ 
14 b! !b nop"
   #:repeat 2 #:constraint constraint-none #:inst-pool `no-fake #:num-bits 4 
   #:mem 4 #:comm 2 #:time-limit 1000
   #:name "more-sketch")

(pretty-display "fastest1--unroll 5 times")
(fastest-program "@p a! nop nop 0
@p b! @b nop 12    !+ a @p nop 3    and push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop  a! nop nop nop
@p b! @b nop 12    !+ a @p nop 3    and push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop  a! nop nop nop
@p b! @b nop 12    !+ a @p nop 3    and push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop  a! nop nop nop
@p b! @b nop 12    !+ a @p nop 3    and push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop  a! nop nop nop
@p b! @b nop 12    !+ a @p nop 3    and push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop  a! nop nop nop
" 
   #:init "dup or dup dup a! @+ nop + @+ nop + nop @+ nop + nop @+ nop + nop over a! nop nop"
   #:slots "@ _ _ _ 
_ _ _ 12 
b! @b dup nop 
!+ _ _ _ 
_ _ _ _ 
dup 2/ 2/ nop 
14 b! !b nop"
   #:repeat 5 #:constraint constraint-none #:inst-pool `no-fake #:num-bits 4 
   #:mem 4 #:comm 5 #:time-limit 1000
   #:name "unroll5")
