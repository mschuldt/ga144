#lang racket

(require "interpreter.rkt" "stack.rkt" "greensyn.rkt")

(define (synthesize)
  (define mem (make-vector 1))
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (reset-greensyn 1 1 1)
  
  ;; input
  (set-input (stack-body data) (stack-body return) mem t s r a b (stack-sp data) (stack-sp return))
  
  ;; run the interpreter
  (load-program "- 2/ dup dup dup + a! dup")
  (step-program!)
  (step-program!)
  (display-data)
  
  ;; output (no communication in this example)
  (set-output (stack-body data) (stack-body return) mem t s r a b (stack-sp data) (stack-sp return))
  (set-comm comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0)
  
  ;; commit to add input-output pair
  (commit-inout)
  
  ;; generate file for Z3
  ;; (check-sat <filename> <#holes>
  (check-sat #:file "test1.smt2" 8)

  ;; add another pair
  ;; (set-input (stack-body data) (stack-body return) mem t s r a b (stack-sp data) (stack-sp return))
  ;; (reset-p!)
  ;; (step-program!)
  ;; (step-program!)
  ;; (display-data)
  ;; (set-output (stack-body data) (stack-body return) mem t s r a b (stack-sp data) (stack-sp return))
  ;; (set-comm comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0)
  ;; (commit-inout)
  ;; (check-sat #:file "test2.smt2" 7)
  )

(define (compare)
  (reset-greensyn 1 1 1)
  (set-spec "- 2* 2/")
  (verify "verify.smt2" "- 2* 2/"))

;(synthesize)
(compare)
