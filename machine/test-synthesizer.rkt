#lang racket

(require "state.rkt" "stack.rkt" "interpreter.rkt" "greensyn.rkt" "programs.rkt" "cegis.rkt")

(set-uplr #x145 #x115 #x175 #x1d5)

(define all 0)
(define pass 0)

(define (test-syn name prog #:mem [mem 1] #:comm [comm 1] #:load-prog-at [start-p 0] #:expect [expect 'sat])

  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset mem comm)
  (reset!)
  (load-state! (random-state))
  (load-program prog start-p)
  (reset-p! start-p)

  ;; input
  (greensyn-input (current-state))
  
  ;; run the interpreter
  (step-program!*)
  
  ;; output (no communication in this example)
  (greensyn-output (current-state))
  (greensyn-send-recv (current-commstate))
  
  ;; commit to add input-output pair
  (greensyn-commit)
  
  (define temp-file "test.smt2")
  (greensyn-check-sat #:file temp-file (program-length-abs prog) #:time-limit 1000)
  (define res (read-sexps (z3 temp-file)))

  (set! all (add1 all))
  (if (member expect res)
      (begin (pretty-display (format "~a \tPASSED" name))
	     (set! pass (add1 pass)))
      (pretty-display (format "~a \tFAILED" name))))

(define (test-ver name spec prog #:mem [mem 1] #:comm [comm 1] expect #:constraint [constraint constraint-all])
  (greensyn-reset mem comm constraint)
  (greensyn-spec (fix-@p spec))

  (define temp-file "test.smt2")
  (greensyn-verify temp-file (fix-@p prog))
  (define res (read-sexps (z3 temp-file)))

  (set! all (add1 all))
  (if (member expect res)
      (begin (pretty-display (format "~a \tPASSED" name))
	     (set! pass (add1 pass)))
      (pretty-display (format "~a \tFAILED" name))))

(pretty-display "============== SYNTHESIZER TEST ==============")

(test-syn "random" "- 2/ dup dup dup + a! dup")
(test-syn "literal" "@p @p @p @p 1 2 3 4")
(test-syn "add-basic" "+ nop nop nop")
(test-syn "add-sat" "@p @p nop + 1 2")
(test-syn "add-unsat" "@p @p @p + 1 2 3" #:expect 'unsat)
(test-syn "memory" "dup or a! @p 123 !+ @p ! nop 456" #:mem 2)

(test-ver "same" "- 2* 2/" "- 2* 2/" 'unsat)
(test-ver "differnt" 
	  "dup or a! nop @+ 2* @+ nop 2/ nop + nop ! nop nop nop"
	  "@+ 2/ or nop @b nop 2* nop + ! nop nop"
	  #:mem 3 'sat)
(test-ver "constraint-all" 
	  "dup or a! nop @+ 2* @+ nop 2/ nop + nop ! nop nop nop"
	  "0 a! @ nop 2* 1 a! nop @+ 2/ nop + nop ! nop nop nop"
	  #:mem 3 'sat)
(test-ver "constraint-mem" 
	  "dup or a! nop @+ 2* @+ nop 2/ nop + nop ! nop nop nop"
	  "0 a! @ nop 2* 1 a! nop @+ 2/ nop + nop ! nop nop nop"
	  #:mem 3 'unsat #:constraint (constraint memory))
(test-ver "constraint-eqv" 
	  "0 a! @ nop 2* 1 a! nop @+ 2/ nop + nop ! nop nop nop"
	  "dup dup or nop a! @+ 2* nop @+ 2/ nop + ! nop nop nop"
	  #:mem 3 'unsat)
(test-ver "comm-1" 
	  "277 b! @b 325 b! !b"
	  "277 b! @b 325 b! !b"
	  'unsat)
(test-ver "comm-2" 
	  "277 a! @ nop @ 325 a! nop ! !"
	  "277 a! 325 nop b! @ !b nop @ !b"
	  #:comm 2 'sat)
(test-ver "comm-3" 
	  "277 a! @ nop @ 325 a! nop ! !"
	  "277 a! 325 nop b! @ @ nop !b !b"
	  #:comm 2 'unsat #:constraint constraint-none)

(pretty-display (format "\nSummary: pass ~a/~a" pass all))