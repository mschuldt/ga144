#lang racket

(require "performance.rkt")

(define (test prog)
  (pretty-display prog)
  (bits prog)
  (instr-pools prog)
  (constraints prog)
  (optimizer prog)
)

(test "over over or nop a! and a nop or nop nop nop")
(test "over and - @p 1 nop + nop +")
(test "@p nop + @p 7 8 - @p nop + 1 and nop nop nop")
