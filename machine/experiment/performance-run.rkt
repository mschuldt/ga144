#lang racket

(require "performance.rkt" "state.rkt")

(define prog1 "over over or nop a! and a nop or nop nop nop")
(define prog2 "over and - @p 1 nop + nop +")
(define prog3 "@p nop + @p 7 8 - @p nop + 1 and nop nop nop")

;; 1st RUN
(pretty-display "RUN 2 - (bits) with (constraint t)")
(bits prog1 #:constraint (constraint t))
(bits prog2 #:constraint (constraint t))

;; 2nd RUN
(pretty-display "RUN 2.5 - (bits) with constraint-all")
(bits prog1)
(bits prog2)
(bits prog3)

;; 3rd RUN
(pretty-display "RUN 3 - (instr-pool) without no-p")
(instr-pools prog1)
(instr-pools prog2 #:pool '(all no-fake no-mem))
(instr-pools prog3 #:pool '(all no-fake no-mem))

;; 5th RUN
(pretty-display "RUN 5 - (opt)")
(optimizer prog1)
(optimizer prog2)
(optimizer prog3)

;; 4th RUN
(pretty-display "RUN 4 - (constraint) with 4 bits")
(constraints prog1)
(constraints prog2)
(constraints prog3 #:num-bits 4 #:inst-pool `no-mem)
