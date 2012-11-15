#lang racket

(require "../interpreter.rkt" "../stack.rkt" "../state.rkt" "../greensyn.rkt")

(define (syn-shift)
  (define comm (make-vector 1))
  ;; set up the program
  (greensyn-reset 4 1 (constraint t))

  ;; set pair 1
  (greensyn-input (progstate a b p i r s #b1010 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s #b1010000000 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 2
  (greensyn-input (progstate a b p i r s #b1101111 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s #b1101111000000 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  (greensyn-check-sat #:file "syn.smt2" 4 #:time-limit 25))

(syn-shift)

(define (verify)
  (greensyn-reset 4 1 (constraint t))
  (greensyn-spec "2* 2* 2* nop 2* 2* 2* nop")
  (greensyn-verify "verify.smt2" "6 lshift nop nop"))
  ;(greensyn-verify "verify.smt2" "dup - 180065 nop + and"))

(verify)
