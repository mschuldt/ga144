#lang racket

(require "interpreter.rkt" "stack.rkt" "state.rkt" "greensyn.rkt")

;; (greensyn-reset 1 1 #:num-bits 18)
;; (greensyn-spec "a! over over nop a - and nop push a and nop pop over over nop or push and nop pop or push nop a and push nop a - and nop pop over over nop or push and nop pop or pop")
;; (greensyn-verify "ver.smt2" "a! over over nop or dup push nop a and or dup pop or over")

;; (reset! 4)
;; (define my-state (random-state 16))
;; (load-state! my-state)
;; ;(display-data)
;; (load-program "@p a! @p nop 0    12    b! @b !+ nop a push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! @p b! nop 12    @b !+ a nop push @p a! @p 0    0    @+ nop + nop @+ nop + nop @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! nop nop nop")
;; ;(display-state)
;; (step-program!*)
;; ;(display-state)
;; (display-comm)
;; (pretty-display (current-commstate))

;; (newline)
;; (reset! 18)
;; (load-state! my-state)
;; ;(display-data)
;; (load-program "a! over over nop or a and nop over or push nop over or a nop and or dup nop pop nop nop nop")
;; (step-program!*)
;; (display-state)