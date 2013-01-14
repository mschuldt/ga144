#lang racket

(require racket/system "state.rkt" "stack.rkt" "interpreter.rkt" "greensyn.rkt" "programs.rkt" "cegis.rkt")

(set-udlr #x145 #x115 #x175 #x1d5)

(define all 0)
(define pass 0)

(define (test name prog #:mem [mem 1] #:comm [comm 1] #:load-prog-at [start-p 0])

  ;; reset the solver (reset <mem_entries> <comm_entries> <comm_bit>)
  (greensyn-reset mem comm)
  (reset!)
  (load-state! (random-state))
  (load-program prog start-p)
  (reset-p! start-p)
  (greensyn-spec (fix-@p prog))

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
  (greensyn-gen-formula temp-file #t)
  (define res (read-sexps (z3 temp-file)))

  (set! all (add1 all))
  (if (member 'sat res)
      (begin (pretty-display (format "~a \tPASSED" name))
	     (set! pass (add1 pass)))
      (pretty-display (format "~a \tFAILED" name))))

(pretty-display "============== FORMULA GENERATOR TEST ==============")

(test "+* (even)" "@p a! @p @p 10 10 0 +* nop nop nop")
(test "+* (odd)" "@p a! @p @p 11 10 0 +* nop nop nop")
(test "+* (bound)" "@p a! @p @p 1 262143 0 +* nop nop nop")
(test "+* (random1)" "+* nop nop nop")
(test "+* (random2)" "+* nop nop nop")
(test "+* (random3)" "+* nop nop nop")
(test "2*" "2* nop nop nop")
(test "2/" "2/ nop nop nop")
(test "-" "- nop nop nop")
(test "+" "@p @p nop + 1 2")
(test "xor" "or nop nop nop")
(test "drop" "drop nop nop nop")
(test "pop" "pop nop nop nop")
(test "over" "over nop nop nop")
(test "a" "a nop nop nop")
(test "." "nop nop nop nop")
(test "push" "push nop nop nop")
(test "b!" "b! nop nop nop")
(test "a!" "a! nop nop nop")

(test "@p" "@p nop nop nop 1")
(test "@+" "@p a! @+ nop 0")
(test "@b" "@p b! @b nop 0")
(test "@b-com" "@p b! @b nop 325")
(test "@" "@p a! @ nop 0")
(test "@-com" "@p a! @ nop 277")

(test "!+" "@p a! !+ nop 1" #:mem 4 #:load-prog-at 8)
(test "!b" "@p b! !b nop 2" #:mem 4 #:load-prog-at 8)
(test "!b-com" "@p b! !b nop 373")
(test "!" "@p a! ! nop 3" #:mem 4 #:load-prog-at 8)
(test "!-com" "@p a! ! nop 469")

(test "interp-basic" "@p a! nop nop 0
@p !+ nop nop 0
@p !+ nop nop 450
@p !+ nop nop 900
@p !+ nop nop 1350
@p !+ nop nop 1800
@p @p nop nop 128 63 over 2/ 2/ nop 2/ 2/ 2/ nop 2/ a! and nop push @+ dup nop @+ - nop + - pop a! dup dup or +* +* +* +* +* +* push drop pop nop + nop nop nop" #:mem 5 #:load-prog-at 16)

(test "interp-comm" "
@p a! @p nop 0    325    
b! @b !+ nop 
a push @p nop 0    
a! @p @+ nop 0    
+ @+ nop + 
@+ nop + nop 
@+ nop + nop 
2/ 2/ @p nop 277    
b! !b pop nop 
a! @p b! nop 325    
@b !+ a nop 
push @p a! @p 0    0    
@+ nop + nop 
@+ nop + nop 
@+ nop + nop 
@+ nop + nop 
2/ 2/ @p nop 277    
b! !b pop nop 
a! nop nop nop" #:mem 4 #:comm 2)

(pretty-display (format "\nSummary: pass ~a/~a" pass all))
(system "rm test.smt2")