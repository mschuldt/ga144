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

;;; swap only at m is 1 xym -> - y'
(fastest-program "a! over over nop a - and nop push a and nop pop over over nop or push and nop pop or nop nop" #:name "swap" #:slots 22 #:num-bits 4 #:inst-pool `no-mem-no-p)
;("a! over a dup push and over dup pop or and dup dup or or nop + nop nop nop nop nop" . 51) 516

;;; f' in MD5
(fastest-program "push over - nop push and pop nop pop and over @p 15 or and or nop" #:slots 16 #:num-bits 4 #:inst-pool `no-mem)
;("push over - nop push and pop nop pop and 15 dup and and or nop" . 52) 277 same runtime

;;; g'
(fastest-program "a! push a nop and pop a nop - and over @p 15 or and or nop" #:slots 16 #:constraint constraint-all #:num-bits 6 #:inst-pool `no-mem)
;("a! a - nop and push a nop and pop dup 15 and and or nop" . 52) 135

;;; i'
(fastest-program "a! push a nop - over @p nop 15 or and or nop pop or nop nop" #:slots 16 #:constraint constraint-all #:num-bits 6 #:inst-pool `no-mem)
;("nop a! push nop a - over 15 or and pop nop or or nop nop" . 49) 440

