#lang racket

(require "interpreter.rkt" "stack.rkt" "state.rkt" "greensyn.rkt")

;;; synthesize

(define (syn-example)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1 1)
  
  ;; input
  (greensyn-input (clone-state))
  
  ;; run the interpreter
  (reset!)
  (load-program "- 2/ dup dup dup + a! dup")
  (step-program!)
  (step-program!)
  (display-data)
  
  ;; output (no communication in this example)
  (greensyn-output (clone-state))
  (greensyn-send-recv (default-commstate))
  
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

  (greensyn-reset 3 1)

  (load-program "dup or a! nop @+ 2* @+ nop 2/ + ! nop" 20)
  (reset-p! 20)
  
  ;; set 1st pair
  (define my-state (clone-state))
  (greensyn-input my-state)
  
  (step-program!*)

  (greensyn-output (clone-state))
  ;(greensyn-scope (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)
  
  ;; set 2nd pair
  (load-state! my-state)
  (set-state! data return 1 277 p i r 2048 t memory)
  (greensyn-input (clone-state))
  (display-return)
  (step-program!*)
  (display-return)
  
  (greensyn-output (clone-state))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)
  
  ;; set 3nd pair
  (load-state! my-state)
  (set-state! data return 0 0 p i r 0 7 memory)
  (greensyn-input (clone-state))
  
  (step-program!*)
  
  (greensyn-output (clone-state))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)
  
  (greensyn-check-sat #:file "mem.smt2" 9))

;;; verify
(define (ver-example) ; unsat
  (greensyn-reset 1 1)
  (greensyn-spec "- 2* 2/")
  (greensyn-verify "verify.smt2" "- 2* 2/"))

(define (ver-mem) ; sat
  (greensyn-reset 3 1)
  (greensyn-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (greensyn-verify "ver-mem.smt2" "@+ 2/ or @b nop 2* + !"))

(define (ver-mem-2) ; sat
  (greensyn-reset 3 1)
  (greensyn-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (greensyn-verify "ver-mem.smt2" "a! @+ 2* @+ nop 2/ + !"))

(define (ver-mem-3) ; sat
  (greensyn-reset 4 1)
  (greensyn-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (greensyn-verify "ver-mem.smt2" "a! @+ 2* @+ nop 2/ + !"))

(define (ver-mem-4) ; sat
  (greensyn-reset 4 1)
  (greensyn-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (greensyn-verify "ver-mem.smt2" "2/ a! @+ 2* @+ 2/ + !"))

(define (ver-mem-5) ; unsat
  (greensyn-reset 4 1)
  (greensyn-spec "dup or a! nop @+ 2* @+ nop 2/ + ! nop")
  (greensyn-verify "ver-mem.smt2" "dup or a! @+ 2* @+ 2/ + !"))

(define (ver-mem-6) ; sat (but we should allow this by relaxing constraint)
  (greensyn-reset 4 1)
  (greensyn-spec "0 a! @ 2* 1 a! @+ 2/ + !")
  (greensyn-verify "ver-mem.smt2" "dup or a! @+ 2* @+ 2/ + !"))

(define (ver-mem-7) ; unsat
  (greensyn-reset 4 1)
  (greensyn-spec "0 a! @ 2* 1 a! @+ 2/ + !")
  (greensyn-verify "ver-mem.smt2" "dup dup or a! @+ 2* @+ 2/ + !"))

; pop a! push begin 
; @+ @ push a push *.17 pop a! 
; push !+ pop . + pop next @ a!

; *.17: a! 16 push dup dup or
; begin +* unext - +* a -if
; drop - 2* ; then drop 2* - ;
(define (syn-taps)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1 1)
  
  ;; input
  (greensyn-input (clone-state))
  
  ;; run the interpreter
  (reset!)
  (load-program "- 2/ dup dup dup + a! dup")
  (step-program!)
  (step-program!)
  (display-data)
  
  ;; output (no communication in this example)
  (greensyn-output (clone-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  ;; generate file for Z3 (check-sat <filename> <#holes>
  (greensyn-check-sat #:file "example.smt2" 8)
 )

;; (define (syn-debug)
;;   (define comm (make-vector 1))
;;   (define dst (make-vector 8))
;;   (vector-set! dst 0 1)
;;   (vector-set! dst 1 2)
;;   (vector-set! dst 2 3)
;;   (vector-set! dst 3 4)
;;   (vector-set! dst 4 5)
;;   (vector-set! dst 5 6)
;;   (vector-set! dst 6 7)
;;   (vector-set! dst 7 8)

;;   ;; set up the program
;;   (reset!)
;;   (set-state! (stack 0 dst) return a b p i r 100 200 memory)
;;   (greensyn-reset 3 1)
;;   (greensyn-input (clone-state))

;;   (display-data)
;;   ;(load-program "@p a! @ nop 0 2* @p a! nop 1 @+ 2/ + nop ! nop nop nop")
;;   (load-program "dup dup or nop a! @+ 2* nop @+ 2/ + nop ! nop nop nop")
;;   (step-program!)
;;   (display-data)
;;   (step-program!)
;;   (display-data)
;;   (step-program!)
;;   (display-data)

;;   (step-program!*)
;;   (display-data)

;;   (greensyn-output (clone-state))
;;   ;(greensyn-scope (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
;;   (greensyn-send-recv (default-commstate))
;;   (greensyn-commit)
;;   (greensyn-check-sat #:file "debug2.smt2" 9))

;; (syn-example)
;; (ver-example)
;; (syn-mem)
;; (ver-mem-7)
