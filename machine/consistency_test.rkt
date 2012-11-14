#lang racket

(require "state.rkt" "stack.rkt" "interpreter.rkt" "greensyn.rkt")

;;; literal
(define (test00)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1)
  (reset!)
  (load-program "@p @p nop + 1 2")
  (greensyn-spec "1 2 nop +")
  
  ;; input
  (greensyn-input (current-state))
  
  ;; run the interpreter
  (step-program!*)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (greensyn-gen-formula "test00.smt2" #t)
 )

;;; add can't be right after insturctions that change s or t
;;; unsat
(define (test01)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1)
  (reset!)
  (load-program "@p @p + nop 1 2")
  (greensyn-spec "1 2 +")
  
  ;; input
  (greensyn-input (current-state))
  
  ;; run the interpreter
  (step-program!*)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (greensyn-gen-formula "test01.smt2" #t)
 )

;;; +* (even case)
(define (test02)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1)
  (reset!)
  (set-state! 10 b p i r 10 0 data return memory)
  (load-program "+* nop nop nop")
  (greensyn-spec "+*")
  
  ;; input
  (greensyn-input (current-state))
  
  ;; run the interpreter
  (step-program!*)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (greensyn-gen-formula"test02.smt2" #t)
 )


;;; +* (odd case)
(define (test03)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1)
  (reset!)
  (set-state! 11 b p i r 10 0 data return memory)
  (load-program "+* nop nop nop")
  (greensyn-spec "+*")
  
  ;; input
  (greensyn-input (current-state))
  
  ;; run the interpreter
  (step-program!*)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (greensyn-gen-formula "test03.smt2" #t)
 )

;;; +* (T17 is kept the same)
(define (test04)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1)
  (reset!)
  (set-state! 1 b p i r 262143 0 data return memory)
  (load-program "+* nop nop nop")
  (greensyn-spec "+*")
  
  ;; input
  (greensyn-input (current-state))
  
  ;; run the interpreter
  (step-program!*)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (greensyn-gen-formula "test04.smt2" #t)
 )

;;; interp in block 1384
(define (test05)
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
  (set-state! a b p i r s t data return mem)
  (display-data)
  
  (load-program "@p @p nop nop 128 63 over 2/ 2/ nop 2/ 2/ 2/ nop 2/ a! and nop push @+ dup nop @+ - nop + - pop a! dup dup or +* +* +* +* +* +* push drop pop nop + nop nop nop" 16)
  (reset-p! 16)
  (greensyn-spec "128 63 nop nop over 2/ 2/ nop 2/ 2/ 2/ nop 2/ a! and nop push @+ dup nop @+ - nop + - pop a! dup dup or +* +* +* +* +* +* push drop pop nop + nop nop nop")
  ;; input
  (greensyn-input (current-state))
  
  ;; run the interpreter
  (step-program!*)
  (display-data)
  (display-state)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (greensyn-gen-formula "test05.smt2" #t)
 )

(define (test06)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1)
  (reset!)
  (load-program "@p @p @p @p 1 2 3 4")
  (greensyn-spec "1 2 3 4")
  
  ;; input
  (greensyn-input (current-state))
  
  ;; run the interpreter
  (step-program!*)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (greensyn-gen-formula "test06.smt2" #t)
 )


(define (test07)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1 #:num-bits 4)
  (reset!)
  (load-program "@p nop + @p 7 8 - @p nop + 1 and nop nop nop")
  (greensyn-spec "7 nop + 8 - 1 nop + and")
  
  ;; input
  (greensyn-input (current-state))
  
  ;; run the interpreter
  (step-program!*)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (greensyn-gen-formula "test07.smt2" #t)
 )

;; (test00)
;; (test01)
;; (test02)
;; (test03)
;; (test04)
;; (test05)
(test07)