#lang racket

(require "../interpreter.rkt" "../stack.rkt" "../state.rkt" "../greensyn.rkt")

(define (syn-divide3)
  (define comm (make-vector 1))
  ;; set up the program
  (greensyn-reset 4 1 (constraint t) #:num-bits 36)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 3 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 1 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 127 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 42 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 130945 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 43648 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 9392 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 3130 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;(greensyn-check-sat #:file "syn.smt2" "@p a! 131071 nop and 0 +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* a @p rshift nop" #:time-limit 1000))
  (greensyn-check-sat #:file "syn.smt2" "@p a! 131071 nop
and 0 +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* a @p rshift" #:time-limit 1000))
;< 2**7 86 8
;< 2**17 43691 17

(define (syn-divide5)
  (define comm (make-vector 1))
  ;; set up the program
  (greensyn-reset 4 1 (constraint t) #:num-bits 36)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 5 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 1 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 127 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 25 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 64 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 12 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 60441 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 12088 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 51279 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 10255 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 99329 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 19865 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;(greensyn-check-sat #:file "syn.smt2" "@p a! 127 nop and 0 +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* a @p rshift nop" #:time-limit 1000)
  (greensyn-check-sat #:file "syn.smt2" "@p a! 131071 nop
and 0 +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* a @p rshift" #:time-limit 1000))
;< 2**7 103 9
;< 2**17 209716 20

;(syn-divide5)

(define (syn-divide6)
  (define comm (make-vector 1))
  ;; set up the program
  (greensyn-reset 4 1 (constraint t) #:num-bits 36)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 5 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 0 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 127 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 21 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 60441 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 10073 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 51279 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 8546 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 99329 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 16554 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 114367 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 19061 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;(greensyn-check-sat #:file "syn.smt2" "@p a! 127 nop and 0 +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* a @p rshift nop" #:time-limit 1000)
  (greensyn-check-sat #:file "syn.smt2" "@p a! 131071 nop
and 0 +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* a @p rshift" #:time-limit 1000))

;(syn-divide6)

(define (syn-divide7)
  (define comm (make-vector 1))
  ;; set up the program
  (greensyn-reset 4 1 (constraint t) #:num-bits 36)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 5 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 0 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 7 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 1 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s 129590 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 18512 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;(greensyn-check-sat #:file "syn.smt2" "@p a! 127 nop and 0 +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* a @p rshift nop" #:time-limit 1000)
  (greensyn-check-sat #:file "syn.smt2" "@p a! 131071 nop
and 0 +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* a @p rshift" #:time-limit 1000))

(syn-divide7)

(define (verify)
  (greensyn-reset 4 1 (constraint t) #:num-bits 36)
  (greensyn-spec "131071 and 5 nop /")
  ;(greensyn-verify "verify.smt2" "683 a! 255 nop and 0 +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* a 11 rshift nop"))
  (greensyn-verify "verify.smt2" 
"104859 a! 131071 nop
and 0 +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* +* +* 
+* +* a 19 rshift"))

(verify)