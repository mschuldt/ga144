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

;;; interp in block 1384
(define (test04)
  (define comm (make-vector 1))
  (define mem (make-vector 64))
  (vector-set! mem 0 0)
  (vector-set! mem 1 450)
  (vector-set! mem 2 900)
  (vector-set! mem 3 1350)
  (vector-set! mem 4 1800)
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 6 1)
  (reset!)
  (set-state! data return a b p i r s t mem)
  (display-data)
  (load-program "@p @p nop nop 128 63 over 2/ 2/ nop 2/ 2/ 2/ nop 2/ a! and nop push @+ dup nop @+ - nop + - pop a! dup dup or +* +* +* +* +* +* push drop pop nop + nop nop nop" 16)
  (reset-p! 16)
  (greensyn-spec "128 63 over 2/ 2/ 2/ 2/ 2/ 2/ a! and push @+ dup @+ - + - pop a! dup dup or +* +* +* +* +* +* push drop pop +")
  
  ;; input
  (greensyn-input (clone-state))
  
  ;; run the interpreter
  (step-program!*)
  (display-data)
  (display-state)
  
  ;; output (no communication in this example)
  (greensyn-output (clone-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (greensyn-gen-formula "interp-syn.smt2" #t)
 )

;; (test01)
;; (test02)
;; (test03)
(test04)
