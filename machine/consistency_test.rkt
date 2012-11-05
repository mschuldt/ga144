#lang racket

(require "state.rkt" "stack.rkt" "interpreter.rkt" "greensyn.rkt")

;;; +* (even case)
(define (test01)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1)
  (reset!)
  (set-state! data return 10 b p i r 10 0 memory)
  (load-program "+* nop nop nop")
  (greensyn-spec "+*")
  
  ;; input
  (greensyn-input (clone-state))
  
  ;; run the interpreter
  (step-program!*)
  
  ;; output (no communication in this example)
  (greensyn-output (clone-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (greensyn-gen-formula"test01.smt2" #t)
 )


;;; +* (odd case)
(define (test02)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1)
  (reset!)
  (set-state! data return 11 b p i r 10 0 memory)
  (load-program "+* nop nop nop")
  (greensyn-spec "+*")
  
  ;; input
  (greensyn-input (clone-state))
  
  ;; run the interpreter
  (step-program!*)
  
  ;; output (no communication in this example)
  (greensyn-output (clone-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (greensyn-gen-formula "test02.smt2" #t)
 )

;;; +* (T17 is kept the same)
(define (test03)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1)
  (reset!)
  (set-state! data return 1 b p i r 262143 0 memory)
  (load-program "+* nop nop nop")
  (greensyn-spec "+*")
  
  ;; input
  (greensyn-input (clone-state))
  
  ;; run the interpreter
  (step-program!*)
  
  ;; output (no communication in this example)
  (greensyn-output (clone-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (greensyn-gen-formula "test03.smt2" #t)
 )

(test01)
(test02)
(test03)
