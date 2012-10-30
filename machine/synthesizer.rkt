#lang racket

(require "interpreter.rkt" "stack.rkt" "greensyn.rkt")

;; synthesize

(define (syn-example)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (reset-greensyn 1 1 1)
  
  ;; input
  (set-input (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  
  ;; run the interpreter
  (reset!)
  (load-program "- 2/ dup dup dup + a! dup")
  (step-program!)
  (step-program!)
  (display-data)
  
  ;; output (no communication in this example)
  (set-output (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  (set-comm comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0)
  
  ;; commit to add input-output pair
  (commit-inout)
  
  ;; generate file for Z3 (check-sat <filename> <#holes>
  (check-sat #:file "example.smt2" 8)
 )

(define (syn-mem)
  (define comm (make-vector 1))
  ;; set up the program
  (reset!)
  (load-program "dup or a! @p 123 !+ @p ! nop 456" 16)
  (reset-p! 16)
  (step-program!*)

  (reset-greensyn 3 1 1)

  (load-program "dup or a! nop @+ 2* @+ nop 2/ + ! nop" 20)
  (reset-p! 20)
  
  ;; set 1st pair
  (define my-state (clone-state!))
  (set-input (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  
  (step-program!*)

  (set-output (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  (set-comm comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0)
  (commit-inout)
  
  ;; set 2nd pair
  (load-state! my-state)
  (set-state! data return 1 277 p i r 2048 t memory)
  (set-input (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  
  (step-program!*)
  
  (set-output (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  (set-comm comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0)
  (commit-inout)
  
  ;; set 3nd pair
  (load-state! my-state)
  (set-state! data return 0 0 p i r 0 7 memory)
  (set-input (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  
  (step-program!*)
  
  (set-output (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  (set-comm comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0 comm 0)
  (commit-inout)
  
  (check-sat #:file "mem.smt2" 9))

;; verify
(define (ver-example)
  (reset-greensyn 1 1 1)
  (set-spec "- 2* 2/")
  (verify "verify.smt2" "- 2* 2/"))

(define (ver-mem)
  (reset-greensyn 3 1 1)
  (set-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (verify "ver-mem.smt2" "@+ 2/ or @b nop 2* + !"))

(define (ver-mem-2)
  (reset-greensyn 3 1 1)
  (set-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (verify "ver-mem.smt2" "a! @+ 2* @+ nop 2/ + !"))

(define (ver-mem-3)
  (reset-greensyn 4 1 1)
  (set-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (verify "ver-mem.smt2" "a! @+ 2* @+ nop 2/ + !"))

(define (ver-mem-4)
  (reset-greensyn 4 1 1)
  (set-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (verify "ver-mem.smt2" "2/ a! @+ 2* @+ 2/ + !"))

(define (ver-mem-5)
  (reset-greensyn 4 1 1)
  (set-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (verify "ver-mem.smt2" "dup or a! @+ 2* @+ 2/ + !"))

;; (syn-example)
;; (ver-example)
(syn-mem)
(ver-mem-5)
