#lang racket

(require racket/list "stack.rkt")

(provide (all-defined-out))

(define MEM-SIZE 1024)

(struct state (a b p i r s t dstack rstack memory) #:mutable #:transparent)
;;(struct commstate (data type recv p) #:mutable #:transparent)

 ;;; The blank state that the interpreter usually starts in.
(define start-state (state 0 0 0 0 0 0 0
                               (stack 0 (make-vector 8))
                               (stack 0 (make-vector 8))
                               (make-vector MEM-SIZE)))
