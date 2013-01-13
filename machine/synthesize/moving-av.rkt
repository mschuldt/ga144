#lang racket

(require "cegis.rkt" "state.rkt")

;; (fastest-program "@p a! @p nop 0    12    b! @b !+ nop a push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! @p b! nop 12    @b !+ a nop push @p a! @p 0    0    @+ nop + nop @+ nop + nop @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! nop nop nop" 
;;    #:init "dup or dup dup a! @+ nop + @+ nop + nop @+ nop + nop @+ nop + nop over a! nop nop"
;;    #:slots "@ - 1 nop + nop + 12 b! @b dup nop !+ a 3 nop and a! nop + dup 2/ 2/ 14 b! !b nop nop"
;;    #:repeat 2 #:constraint (constraint r) #:inst-pool `no-fake #:num-bits 4 
;;    #:mem 4 #:comm 2 #:time-limit 1000
;;    #:name "check")

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
   #:name "2exp")


;;    #:init "dup or dup dup a! @+ nop + @+ nop + nop @+ nop + nop @+ nop + nop over a! nop nop"
;;    #:slots "@ - 1 nop 
;; + nop + 12 
;; b! @b dup nop 
;; !+ a 3 nop 
;; and a! nop + 
;; dup 2/ 2/ 14 
;; b! !b nop nop"
