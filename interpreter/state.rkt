#lang racket

(require racket/list "stack.rkt")

(provide (all-defined-out))

(define MEM-SIZE 1024)

(struct progstate (a b p i r s t data return memory) #:mutable #:transparent)
;;(struct commstate (data type recv p) #:mutable #:transparent)

 ;;; The blank state that the interpreter usually starts in.
(define start-state (progstate 0 0 0 0 0 0 0
                               (stack 0 (make-vector 8))
                               (stack 0 (make-vector 8))
                               (make-vector MEM-SIZE)))
