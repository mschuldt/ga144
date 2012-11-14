#lang racket

(require "cegis.rkt" "state.rkt")

;;; x - (x & y)
(fastest-program "over and - @p 1 nop + nop +" #:slots 8 #:constraint (constraint t))
;("- and" . 6) 81.95

;;; ~ (x - y)
(fastest-program "- @p nop + 1 nop + - nop"  #:slots 8 #:constraint (constraint t))
;; ("over - nop +" . 12) 64.18

;; ;;; x | y
(fastest-program "over over or nop a! and a nop or nop nop nop" #:slots 8 #:constraint (constraint t))
;; ("over - and nop +" . 15) 191.03

;;; (x | y) - (x & y)
(fastest-program "over over and nop - @p nop + 1 push over over nop or a! and nop a or pop nop + nop nop nop" #:slots 8 #:constraint (constraint t) #:num-bits 4)
;; ("or" . 3) 63.37

;;; swap only at m is 1 xym -> - y' (first part)
(fastest-program "a! over over nop a - and nop push a and nop pop over over nop or push and nop pop or nop nop" #:name "swap" #:slots 22 #:num-bits 4 #:inst-pool `no-mem-no-p)
;("a! over a dup push and over dup pop or and dup dup or or nop + nop nop nop nop nop" . 51) 516

(fastest-program "a! over over nop a - and nop push a and nop pop over over nop or push and nop pop or push nop" #:name "swap1" #:constraint (constraint a r s t) #:slots 20 #:num-bits 4 #:inst-pool `no-mem-no-p)
;("a! over over nop or a and nop over or push nop nop nop nop nop nop nop nop nop" . 33) 32639
(fastest-program3 "a! over over nop a - and nop push a and nop pop over over nop or push and nop pop or push nop" #:name "swap1" #:constraint (constraint a r s t) #:num-bits 4 #:inst-pool `no-mem-no-p)
;("a! over over nop or a and nop over or push nop nop nop nop" . 33) 2278

;;; swap only at m is 1 xym -> - x' (second part)
(pretty-display "swap only at m (x' y')")
(fastest-program "push nop a nop and push a nop - and nop nop pop over over nop or push and nop pop or pop nop" #:name "swap2" #:slots 23 #:num-bits 4 #:inst-pool `no-mem-no-p)
;'("push a and nop push a - nop and pop pop dup and push or nop pop nop nop nop nop nop nop" . 51) 693
(fastest-program "push nop a nop and push a nop - and nop nop pop over over nop or push and nop pop or pop nop" #:name "swap2" #:constraint (constraint s t) #:slots 16 #:num-bits 4 #:inst-pool `no-mem-no-p)
;("push over or nop a and or dup pop nop nop nop nop nop nop nop" . 27) 8989
(fastest-program3 "push nop a nop and push a nop - and nop nop pop over over nop or push and nop pop or pop nop" #:name "swap2" #:constraint (constraint s t) #:num-bits 4 #:inst-pool `no-mem-no-p)
;("push over or nop a and or dup pop nop nop nop" . 27) 2852

;;; f' in MD5
(fastest-program "push over - nop push and pop nop pop and over @p 15 or and or nop" #:slots 16 #:num-bits 4 #:inst-pool `no-mem)
;("push over - nop push and pop nop pop and 15 dup and and or nop" . 52) 277 same runtime

;;; g'
(fastest-program "a! push a nop and pop a nop - and over @p 15 or and or nop" #:slots 16 #:constraint constraint-all #:num-bits 6 #:inst-pool `no-mem)
;("a! a - nop and push a nop and pop dup 15 and and or nop" . 52) 135

;;; i'
(fastest-program "a! push a nop - over @p nop 15 or and or nop pop or nop nop" #:slots 16 #:constraint constraint-all #:num-bits 6 #:inst-pool `no-mem)
;("nop a! push nop a - over 15 or and pop nop or or nop nop" . 49) 440

;; (pretty-display "nearest multiple of 8 (4 bit)")
(fastest-program "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:name "roundup" #:slots 8 #:constraint (constraint t) #:num-bits 18)
;("7 nop + 262136 and" . 29) 3587?
(fastest-program3 "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:name "roundup" #:constraint (constraint t) #:num-bits 4 #:inst-pool `no-mem)
;("7 nop + dup 2* 2/ or" . 28) 312
(fastest-program3 "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:name "roundup" #:constraint (constraint t) #:num-bits 8 #:inst-pool `no-mem)
;("7 nop + 248 and nop nop nop" . 29) 219