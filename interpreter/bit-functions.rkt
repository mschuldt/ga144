#lang racket
(provide (all-defined-out))

(define & bitwise-and)
(define ^ bitwise-xor)
(define << arithmetic-shift)
(define (>> x n) (arithmetic-shift x (- n)))
(define ior bitwise-ior)

