#lang racket

(require "interpreter.rkt" "stack.rkt" "state.rkt" "greensyn.rkt")

;;; synthesize

(define (syn-example)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1)
  (reset!)
  (load-program "- 2/ dup dup dup + a! dup")
  
  ;; input
  (greensyn-input (current-state))
  
  ;; run the interpreter
  (step-program!)
  (step-program!)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  ;; generate file for Z3 (check-sat <filename> <#holes>
  (greensyn-check-sat #:file "example.smt2" 8)
 )

;(syn-example)


(define (syn-literal)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1)
  (reset!)
  (load-program "@p @p + nop 1 2")
  
  ;; input
  (greensyn-input (current-state))
  
  ;; run the interpreter
  (step-program!*)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  ;; generate file for Z3 (check-sat <filename> <#holes>
  (greensyn-check-sat #:file "literal.smt2" 3)
 )

(syn-literal)

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
  (define my-state (current-state))
  (greensyn-input my-state)
  
  (step-program!*)

  (greensyn-output (current-state))
  ;(greensyn-scope (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  (greensyn-send-recv (default-commstate))
  ;(greensyn-commit)
  
  ;; set 2nd pair
  (load-state! my-state)
  (set-state! 1 277 p i r 2048 t data return memory)
  (greensyn-input (current-state))
  (display-return)
  (step-program!*)
  (display-return)
  
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  ;(greensyn-commit)
  
  ;; set 3nd pair
  (load-state! my-state)
  (set-state! 0 0 p i r 0 7 data return memory)
  (greensyn-input (current-state))
  
  (step-program!*)
  
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  ;(greensyn-commit)
  
  (greensyn-check-sat #:file "mem.smt2" 9 #:time-limit 47))

;(syn-mem)

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

(define (ver-mem-8) ; sat
  (greensyn-reset 4 1)
  (greensyn-spec "0 a! @ 2* 1 a! @+ 2/ + !")
  (greensyn-verify "ver-mem.smt2" "a! 0 a! @+ 2* @+ 2/ + !"))

(define (syn-interp)
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
  (load-program "@p @p nop nop 128 63 over 2/ 2/ nop 2/ 2/ 2/ nop 2/ a! and nop push @+ dup nop @+ - nop + - pop a! dup dup or +* +* +* +* +* +* push drop pop nop + nop nop nop" 16)
  (reset-p! 16)
  
  ;; 1
  (greensyn-input (current-state))
  (step-program!*)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; 2 ; dst 262143 ... 128
  ;; (vector-set! mem 0 0)
  ;; (vector-set! mem 1 0)
  ;; (vector-set! mem 2 0)
  ;; (vector-set! mem 3 0)
  ;; (vector-set! mem 4 0)
  (reset-p! 16)
  (set-state! 203893 48523 p i 0 262015 262015 data return memory)
  (greensyn-input (current-state))
  (step-program!*)
  (display-data)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)
  
  (greensyn-check-sat #:file "interp-syn.smt2" 33)
 )

;(syn-interp)

(define (ver-interp) ; sat
  (greensyn-reset 4 1)
  (greensyn-spec "128 63 over 2/ 2/ 2/ 2/ 2/ 2/ a! and push @+ dup @+ - + - pop a! dup dup or +* +* +* +* +* +* push drop pop +")
  (greensyn-verify "ver-interp.smt2" "@+ @ @b @b b! push right a! b! b! a! +* a! right drop a! push 128 63 +* a! b! + 2* over +* pop pop push a! b! drop"))

;(ver-interp)

; pop a! push begin 
; @+ @ push a push *.17 pop a! 
; push !+ pop . + pop next @ a!

; *.17: a! 16 push dup dup or
; begin +* unext - +* a -if
; drop - 2* ; then drop 2* - ;
;(define (syn-taps)

(define (ver-add)
  (greensyn-reset 1 1)
  (greensyn-spec "+ nop nop nop")
  (greensyn-verify "ver-add.smt2" "- -"))

;(ver-add)


(reset!)
(display-data)
(load-program "- 2/ dup dup dup + a! dup")
(step-program!*)
(display-data)

(newline)
(reset!)
(display-data)
(load-program "- 2/ 2/ dup over nop 2* nop dup a! nop nop")
(step-program!*)
(display-data)

