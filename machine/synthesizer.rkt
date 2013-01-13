#lang racket

(require "interpreter.rkt" "stack.rkt" "state.rkt" "greensyn.rkt")

;;; synthesize

(define (syn-example)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1 constraint-all)
  (reset!)
  (load-program "- 2/ dup dup dup + a! dup")
  
  ;; input
  (greensyn-input (current-state))
  
  ;; run the interpreter
  (step-program!*)
  (step-program!)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  ;; generate file for Z3 (check-sat <filename> <#holes>
  (greensyn-check-sat #:file "example.smt2" 8 #:time-limit 50)
 )


(define (syn-literal)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1)
  (reset!)
  (set-state! a b p i r 7 6 data return memory)
  (load-program "@p @p nop + 1 2")
  
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
  (greensyn-check-sat #:file "literal.smt2" 4)
 )

;(syn-literal)

(define (syn-mem)
  (define comm (make-vector 1))
  ;; set up the program
  (reset!)
  (load-program "dup or a! @p 123 !+ @p ! nop 456" 16)
  (reset-p! 16)
  (step-program!*)

  (greensyn-reset 3 1)

  (load-program "dup or a! nop @+ 2* @+ nop 2/ nop + nop ! nop nop nop" 20)
  (reset-p! 20)
  
  ;; set 1st pair
  (define my-state (current-state))
  (greensyn-input my-state)
  
  (step-program!*)

  (greensyn-output (current-state))
  ;(greensyn-scope (stack-body data) (stack-body return) memory t s r a b (stack-sp data) (stack-sp return))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)
  
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
  (define mem (make-vector 64))
  (vector-set! mem 0 0)
  (vector-set! mem 1 450)
  (vector-set! mem 2 0)
  (load-state! my-state)
  (set-state! 0 0 p i r 0 7 data return mem)
  (load-program "dup or a! nop @+ 2* @+ nop 2/ nop + nop ! nop nop nop" 20)
  (reset-p! 20)
  (greensyn-input (current-state))
  
  (step-program!*)
  
  (greensyn-output (current-state))
  (greensyn-send-recv (default-commstate))
  ;(greensyn-commit)
  
  (greensyn-check-sat #:file "mem.smt2" 14))
;(syn-mem)

;;; verify
(define (ver-example) ; unsat
  (greensyn-reset 1 1)
  (greensyn-spec "- 2* 2/")
  (greensyn-verify "verify.smt2" "- 2* 2/"))

;(ver-example)

(define (ver-mem) ; sat
  (greensyn-reset 3 1)
  (greensyn-spec "dup or a! nop @+ 2* @+ nop 2/ nop + nop ! nop nop nop")
  (greensyn-verify "ver-mem.smt2" "@+ 2/ or nop @b nop 2* nop + ! nop nop"))

;(ver-mem)

(define (ver-mem-4) ; unsat
  (greensyn-reset 4 1)
  (greensyn-spec "dup or a! nop @+ 2* @+ nop 2/ nop + nop ! nop nop nop")
  (greensyn-verify "ver-mem-4.smt2" "dup or a! nop @+ 2* @+ nop 2/ nop + nop ! nop nop nop"))
;(ver-mem-4)

(define (ver-mem-5) ; unsat
  (greensyn-reset 4 1 (constraint memory))
  (greensyn-spec "0 a! @ nop 2* 1 a! nop @+ 2/ nop + nop ! nop nop nop")
  (greensyn-verify "ver-mem-5.smt2" "dup or a! nop @+ 2* @+ nop 2/ nop + nop ! nop nop nop"))
;(ver-mem-5)

(define (ver-mem-6) ; sat (but we should allow this by relaxing constraint)
  (greensyn-reset 4 1)
  (greensyn-spec "0 a! @ nop 2* 1 a! nop @+ 2/ nop + nop ! nop nop nop")
  (greensyn-verify "ver-mem-6.smt2" "dup or a! nop @+ 2* @+ nop 2/ nop + nop ! nop nop nop"))
;(ver-mem-6)

(define (ver-mem-7) ; unsat
  (greensyn-reset 4 1)
  (greensyn-spec "0 a! @ nop 2* 1 a! nop @+ 2/ nop + nop ! nop nop nop")
  (greensyn-verify "ver-mem-7.smt2" "dup dup or nop a! @+ 2* nop @+ 2/ nop + ! nop nop nop"))
;(ver-mem-7)

(define (ver-mem-8) ; sat
  (greensyn-reset 4 1)
  (greensyn-spec "0 a! @ nop 2* 1 a! nop @+ 2/ nop + nop ! nop nop nop")
  (greensyn-verify "ver-mem-8.smt2" "a! 0 a! nop @+ 2* @+ nop 2/ nop + nop ! nop nop nop"))
;(ver-mem-8)


(define (syn-comm)
  (define vec (make-vector 1))
  (define comm (default-commstate))
  
  (greensyn-reset 1 1 (constraint t))
  
  ;; 1
  (vector-set! vec 0 (random 1000))
  (set-commstate-recv-d! comm vec)
  (set-commstate-recvp-d! comm 1)
  (set-commstate-send-u! comm vec)
  (set-commstate-sendp-u! comm 1)

  (greensyn-input (current-state))
  (greensyn-output (current-state))
  (greensyn-send-recv comm)
  (greensyn-commit)

  ;; 2
  (vector-set! vec 0 (random 1000))
  (set-commstate-recv-d! comm vec)
  (set-commstate-recvp-d! comm 1)
  (set-commstate-send-u! comm vec)
  (set-commstate-sendp-u! comm 1)

  (greensyn-input (current-state))
  (greensyn-output (current-state))
  (greensyn-send-recv comm)
  (greensyn-commit)
  
  (greensyn-check-sat #:file "comm-syn.smt2" 8)
 )

(define (ver-comm-1) ; unsat
  (greensyn-reset 1 1 (constraint t))
  (greensyn-spec "277 b! @b 325 b! !")
  (greensyn-verify "comm-ver.smt2" "277 b! @b 325 b! !"))

(define (ver-comm-2) ; unsat
  (greensyn-reset 1 1 (constraint t))
  (greensyn-spec "277 b! @b 325 b! !b")
  (greensyn-verify "comm-ver.smt2" "277 b! @b 325 b! !b"))

(define (ver-comm-3) ; unsat
  (greensyn-reset 1 1 (constraint t))
  (greensyn-spec "!")
  (greensyn-verify "comm-ver.smt2" "!"))

(syn-comm)
;; (ver-comm-3)

(define (syn-repeat)
  (define comm (make-vector 1))
  
  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset 1 1 constraint-all)
  (reset!)
  (load-program "or and 2* nop and 2* nop nop")
  
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
  (greensyn-check-sat #:file "repeat.smt2" 2 2 2 #:time-limit 50)
 )

(define (ver-add)
  (greensyn-reset 1 1)
  (greensyn-spec "+ nop nop nop")
  (greensyn-verify "ver-add.smt2" "- -"))

;; (greensyn-reset 1 1 #:num-bits 18)
;; (greensyn-spec "a! over over nop a - and nop push a and nop pop over over nop or push and nop pop or push nop a and push nop a - and nop pop over over nop or push and nop pop or pop")
;; (greensyn-verify "ver.smt2" "a! over over nop or dup push nop a and or dup pop or over")

;; (reset! 4)
;; (define my-state (random-state 16))
;; (load-state! my-state)
;; ;(display-data)
;; (load-program "@p a! @p nop 0    12    b! @b !+ nop a push @p nop 0    a! @p @+ nop 0    + @+ nop + @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! @p b! nop 12    @b !+ a nop push @p a! @p 0    0    @+ nop + nop @+ nop + nop @+ nop + nop @+ nop + nop 2/ 2/ @p nop 14    b! !b pop nop a! nop nop nop")
;; ;(display-state)
;; (step-program!*)
;; ;(display-state)
;; (display-comm)
;; (pretty-display (current-commstate))

;; (newline)
;; (reset! 18)
;; (load-state! my-state)
;; ;(display-data)
;; (load-program "a! over over nop or a and nop over or push nop over or a nop and or dup nop pop nop nop nop")
;; (step-program!*)
;; (display-state)


(define (test name prog mem comm #:load-prog-at [start-p 0])

  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset mem comm)
  (reset!)
  (load-state! (random-state))
  (load-program prog start-p)
  (reset-p! start-p)
  (greensyn-spec (fix-@p prog))

  ;; input
  (greensyn-input (current-state))

  (display-state)
  (display-memory mem)
  
  ;; run the interpreter
  (step-program!*)

  (display-state)
  (display-memory mem)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (current-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (define temp-file "test.smt2")
  (greensyn-gen-formula temp-file #t)
  (define res (read-sexps (z3 temp-file)))

  (set! all (add1 all))
  (if (member 'sat res)
      (begin (pretty-display (format "~a \tPASSED" name))
	     (set! pass (add1 pass)))
      (pretty-display (format "~a \tFAILED" name))))


(test "!b" "@p b! !b nop 2" 4 1 #:load-prog-at 8)