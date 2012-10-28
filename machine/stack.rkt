;;; Some utilities for working with 8-word circular stacks
#lang racket

(provide (all-defined-out))

(struct stack ([sp #:mutable] body) #:transparent)

;;; Copies the given stack. This keeps mutable vectors from being
;;; shared between different stacks.
(define (copy-stack s)
  (stack (stack-sp s) (vector-copy (stack-body s))))

;;; Print a circular stack:
(define (display-stack stack)
  (for [(i (in-range 0 8))]
       (display (format " ~x" (vector-ref (stack-body stack)
                                          (modulo (- (stack-sp stack) i) 8))))))

;;; Pushes a value to the given stack's body.
(define (push-stack! stack value)
  (set-stack-sp! stack (modulo (add1 (stack-sp stack)) 8))
  (vector-set! (stack-body stack) (stack-sp stack) value))

;;; Pops from the given stack's body.
(define (pop-stack! stack)
  (let ([ret-val (vector-ref (stack-body stack) (stack-sp stack))])
    (set-stack-sp! stack (modulo (sub1 (stack-sp stack)) 8))
    ret-val))
