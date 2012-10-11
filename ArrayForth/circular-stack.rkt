#lang racket

(provide stack make-stack push! pop! peek)

;;; Push an element onto the given stack:
(define (push! old-stack element)
  (let-values ([(new-registers next)
                (vector-split-at-right (stack-registers old-stack) 1)])
    (set-stack-registers! old-stack (vector-append `#(,element) new-registers))
    (set-stack-sp! old-stack (modulo (add1 (stack-sp old-stack)) 8))
    (vector-set! (stack-rest old-stack) (stack-sp old-stack)
                 (vector-ref next 0))))

;;; Pops an element off the stack and returns it.
(define (pop! old-stack)
  (let-values ([(ret new-registers)
                (vector-split-at (stack-registers old-stack) 1)])
    (let ([element (vector-ref (stack-rest old-stack) (stack-sp old-stack))])
      (set-stack-registers! old-stack (vector-append new-registers `#(,element))))
    (set-stack-sp! old-stack (modulo (sub1 (stack-sp old-stack)) 8))
    (vector-ref ret 0)))

;;; Returns the top value of the stack without taking it off the stack.
(define (peek old-stack)
  (vector-ref (stack-registers old-stack) 0))

;;; Represents a stack with its top registers and the rest of its
;;; elements as vectors.
(struct stack ([registers #:mutable] [sp #:mutable] rest))

;;; Creates an empty stack with the specified number of top registers
;;; (2 by default).
(define (make-stack [registers 2] [stack-body-size 8])
  (stack (make-vector registers) 0 (make-vector stack-body-size)))
