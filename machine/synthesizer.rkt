#lang racket

(require "interpreter.rkt" "stack.rkt" "greensyn.rkt")

;;; synthesize

(define (syn-example)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1 1)
  
  ;; input
  (greensyn-input (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  
  ;; run the interpreter
  (reset!)
  (load-program "- 2/ dup dup dup + a! dup")
  (step-program!)
  (step-program!)
  (display-data)
  
  ;; output (no communication in this example)
  (greensyn-output (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  (greensyn-send-recv comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0)
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  ;; generate file for Z3 (check-sat <filename> <#holes>
  (greensyn-check-sat #:file "example.smt2" 8)
 )

(define (syn-mem)
  (define comm (make-vector 1))
  ;; set up the program
  (reset!)
  (load-program "dup or a! @p 123 !+ @p ! nop 456" 16)
  (reset-p! 16)
  (step-program!*)

  (greensyn-reset 3 1 1)

  (load-program "dup or a! nop @+ 2* @+ nop 2/ + ! nop" 20)
  (reset-p! 20)
  
  ;; set 1st pair
  (define my-state (clone-state!))
  (greensyn-input (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  
  (step-program!*)

  (greensyn-output (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  (greensyn-send-recv comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0)
  (greensyn-commit)
  
  ;; set 2nd pair
  (load-state! my-state)
  (set-state! data return 1 277 p i r 2048 t memory)
  (greensyn-input (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  
  (step-program!*)
  
  (greensyn-output (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  (greensyn-send-recv comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0)
  (greensyn-commit)
  
  ;; set 3nd pair
  (load-state! my-state)
  (set-state! data return 0 0 p i r 0 7 memory)
  (greensyn-input (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  
  (step-program!*)
  
  (greensyn-output (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  (greensyn-send-recv comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0)
  (greensyn-commit)
  
  (greensyn-check-sat #:file "mem.smt2" 9))

;;; verify
(define (ver-example) ; unsat
  (greensyn-reset 1 1 1)
  (greensyn-spec "- 2* 2/")
  (greensyn-verify "verify.smt2" "- 2* 2/"))

(define (ver-mem) ; sat
  (greensyn-reset 3 1 1)
  (greensyn-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (greensyn-verify "ver-mem.smt2" "@+ 2/ or @b nop 2* + !"))

(define (ver-mem-2) ; sat
  (greensyn-reset 3 1 1)
  (greensyn-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (greensyn-verify "ver-mem.smt2" "a! @+ 2* @+ nop 2/ + !"))

(define (ver-mem-3) ; sat
  (greensyn-reset 4 1 1)
  (greensyn-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (greensyn-verify "ver-mem.smt2" "a! @+ 2* @+ nop 2/ + !"))

(define (ver-mem-4) ; sat
  (greensyn-reset 4 1 1)
  (greensyn-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (greensyn-verify "ver-mem.smt2" "2/ a! @+ 2* @+ 2/ + !"))

(define (ver-mem-5) ; unsat
  (greensyn-reset 4 1 1)
  (greensyn-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (greensyn-verify "ver-mem.smt2" "dup or a! @+ 2* @+ 2/ + !"))

;; (syn-example)
;; (ver-example)
(syn-mem)
(ver-mem-5)
