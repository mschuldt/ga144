#lang racket

(require "../interpreter.rkt" "../stack.rkt" "../state.rkt" "../greensyn.rkt")

(define (syn-least-bit)
  (define comm (make-vector 1))
  ;; set up the program
  (greensyn-reset 4 1 constraint-only-t)

  ;; set pair
  (greensyn-input (progstate a b p i r s #b10000001010000 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s #b10000 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair	
  (greensyn-input (progstate a b p i r s #b10000001010001 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s #b1 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair	
  (greensyn-input (progstate 0 0 p i 3 8192 1 (stack 3 2) (stack 0 0) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 1 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair	
  (greensyn-input (progstate 0 0 p i 0 65792 12290 (stack 7 #xa860002808874b02819020381c8fffffffff) (stack 0 0) 0))
  (greensyn-output (progstate a b p i r s 2 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair	
  (greensyn-input (progstate 0 0 p i 262143 0 0 (stack 7 #xffffffffffffffffffffffffffffffffffff) (stack 0 0) 0))
  (greensyn-output (progstate a b p i r s 0 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  ;(greensyn-commit)


  (greensyn-check-sat #:file "syn.smt2" 6))

(syn-least-bit)

(define (verify)
  (greensyn-reset 4 1 constraint-only-t)
  (greensyn-spec "dup - 1 nop + and")
  (greensyn-verify "verify.smt2" "147535 !b @+ +* - and"))

(verify)