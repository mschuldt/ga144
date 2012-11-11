#lang racket

(require "../interpreter.rkt" "../stack.rkt" "../state.rkt" "../greensyn.rkt")

(define (syn-least-bit)
  (define comm (make-vector 1))
  ;; set up the program
  (greensyn-reset 4 1)

  ;; set 1st pair
  (greensyn-input (progstate a b p i r s #b10000001010000 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate #f #f #f #f #f #f #b10000 #f #f #f))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set 2nd pair	
  (greensyn-input (progstate a b p i r s #b10000001010001 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate #f #f #f #f #f #f #b1 #f #f #f))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  (greensyn-check-sat #:file "syn-least-bit.smt2" 6))

(syn-least-bit)

;; (define (verify)
;;   (greensyn-spec "dup - 1 + and")
;;   (greensyn-verify "verify.smt2" "182415 41602 !+ +* -"))