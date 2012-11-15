#lang racket

(require "../interpreter.rkt" "../stack.rkt" "../state.rkt" "../greensyn.rkt")

(define (syn-divide3)
  (define comm (make-vector 1))
  ;; set up the program
  (greensyn-reset 4 1 (constraint t))

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


  (greensyn-check-sat #:file "syn.smt2" "_ a! 127 nop and 0 +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* a 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop" #:time-limit 1000))

(define (syn-divide5)
  (define comm (make-vector 1))
  ;; set up the program
  (greensyn-reset 4 1 (constraint t))

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

  (greensyn-check-sat #:file "syn.smt2" "_ a! 127 nop and 0 +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* +* a 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ nop nop nop" #:time-limit 1000))
(syn-divide5)

;; (define (verify)
;;   (greensyn-reset 4 1 (constraint t))
;;   (greensyn-spec "dup - 1 nop + and")
;;   (greensyn-verify "verify.smt2" "1 over - nop + and"))
;;   ;(greensyn-verify "verify.smt2" "dup - 180065 nop + and"))

;; (verify)