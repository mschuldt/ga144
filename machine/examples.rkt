#lang racket

(require "cegis.rkt" "state.rkt")

;;; x - (x & y)
;(fastest-program "over and - @p 1 nop + nop +" #:slots 8 #:constraint (constraint t))
;("- and" . 6)

;;; ~ (x - y)
;; (pretty-display "~ (x - y)")
;; (fastest-program "- @p nop + 1 nop + - nop"  #:slots 8 #:constraint (constraint t))
;; ("over - nop +" . 12)

;; ;;; x | y
;; (pretty-display "x | y")
;; (fastest-program "over over or nop a! and a nop or nop nop nop" #:slots 8 #:constraint (constraint t))
;; ("over - and nop +" . 15)

;;; (x | y) - (x & y)
;; (pretty-display "(x | y) - (x & y)")
;; (fastest-program "over over and nop - @p nop + 1 push over over nop or a! and nop a or pop nop + nop nop nop" #:slots 8 #:constraint (constraint t) #:num-bits 4)
;; ("or" . 3)

;;; swap only at m is 1 xym -> - y'
(pretty-display "swap only at m (y')")
(fastest-program "a! over over nop a - and nop push a and nop pop 
over over nop or push and nop pop nop nop nop or nop nop nop" #:slots 12 #:constraint (constraint t) #:num-bits 4)

;;; swap only at m is 1 xym -> - x' y'
(pretty-display "swap only at m (x' y')")
(fastest-program "a! over over nop a - and nop push a and nop pop 
over over nop or push and nop pop nop nop nop or 
push 
a nop and push a nop - and pop nop 
over over or nop push and pop nop or 
pop nop nop" #:slots 16 #:constraint (constraint s t) #:num-bits 4)

;;; round x up to the nearest multiple of 8:
(pretty-display "nearest multiple of 8 (4 bit)")
(fastest-program "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:slots 14 #:constraint (constraint t) #:num-bits 4)
;; (fastest-program "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:slots 14 #:constraint constraint-all)
;; '("7 nop + 262136 a a! and nop nop nop nop nop nop nop" . 35)

;;; f' in MD5
(pretty-display "f'")
(fastest-program "push over - nop push and pop nop pop and over @p 15 or and or nop" #:slots 16 #:constraint (constraint t) #:num-bits 6)
;; (fastest-program "push over - nop push and pop nop pop and over @p 65535 or and or nop" #:slots 16 #:constraint (constraint t))

;;; f' in MD5 (with mask)
(pretty-display "f' (with mask)")
(fastest-program "push over - nop push and pop nop pop and over @p 15 or and or @p 15 and nop nop nop" #:slots 16 #:constraint (constraint t) #:num-bits 6)
;; (fastest-program "push over - nop push and pop nop pop and over @p 65535 or and or @p 65535 and nop nop nop" #:slots 16 #:constraint (constraint t))

;;; g' in MD5
(pretty-display "g'")
(fastest-program "a! push a nop and pop a nop - and over @p 15 or and or nop" #:slots 16 #:constraint (constraint t) #:num-bits 6)
;; (fastest-program "a! push a nop and pop a nop - and over @p 65535 or and or nop" #:slots 16 #:constraint (constraint t))

;;; i' in MD5
(pretty-display "i'")
(fastest-program "a! push a nop - over @p nop 15 or and or nop pop or nop nop" #:slots 16 #:constraint (constraint t) #:num-bits 6)
;; (fastest-program "a! push a nop - over @p nop 65535 or and or nop pop or nop nop" #:slots 16 #:constraint (constraint t))

;;; Get the most significant bit of a number by shifting it right 17 times.
(pretty-display "shift right")
(fastest-program "2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ nop nop"
                   #:slots 19 #:mem 4 #:start 5 #:constraint (constraint t))
;; This one still hasn't finished yet...

;;; Averaging four numbers:
;;; (fastest-program "nop + nop + nop + 2/ nop 2/ nop nop nop"  #:slots 8 #:constraint (constraint t))
;;; This one finished in a rather uninteresting '("+ nop + nop + 2/ 2/ nop" . 21)
;;; It took 4970 seconds (about 80 minutes). Might be a useful data
;;; point, but certainly not a useful example!
