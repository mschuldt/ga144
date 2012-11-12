#lang racket

(require "../interpreter.rkt" "../stack.rkt" "../state.rkt" "../greensyn.rkt")

(define (syn-least-bit)
  (define comm (make-vector 1))
  ;; set up the program
  (greensyn-reset 4 1 constraint-only-t)

  ;; set pair 1
  (greensyn-input (progstate a b p i r s #b10000001010000 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s #b10000 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 2
  (greensyn-input (progstate a b p i r s #b10000001010001 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s #b1 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 3
  (greensyn-input (progstate 0 0 p i 3 8192 1 (stack 3 2) (stack 0 0) (vector-copy memory 0 64)))
  (greensyn-output (progstate a b p i r s 1 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 4	
  (greensyn-input (progstate 0 0 p i 0 65792 12290 (stack 7 #xa860002808874b02819020381c8fffffffff) (stack 0 0) 0))
  (greensyn-output (progstate a b p i r s 2 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 5	
  (greensyn-input (progstate 1 325 p i 0 16574 44355 (stack 7 #x000020201008060200006020080fffffffff) (stack 0 0) #x00000000054c140000))
  (greensyn-output (progstate a b p i r s 1 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 6
  (greensyn-input (progstate 0 0 p i 0 114816 78367 (stack 7 #x0400e1001d030ac36010f4300c0fffffffff) (stack 0 0) #x00000000054c140000))
  (greensyn-output (progstate a b p i r s 1 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)

  ;; set pair 7
  (greensyn-input (progstate 0 0 p i 0 320 7168 (stack 3 #x0100f4102fffffffff70f4241c88007e7fdf) (stack 0 0) 0))
  (greensyn-output (progstate a b p i r s 1024 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)


  ;; set pair 8
  (greensyn-input (progstate 0 0 p i 0 240 16384 (stack 0 #x1f2fe3cbf0fafebf6fcff3ffffffffffffff) (stack 0 0) 0))
  (greensyn-output (progstate a b p i r s 16384 (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))
  (greensyn-send-recv (default-commstate))
  (greensyn-commit)
  (greensyn-check-sat #:file "syn.smt2" 8 #:time-limit 25))

(syn-least-bit)

(define (verify)
  (greensyn-reset 4 1 constraint-only-t)
  (greensyn-spec "dup - 1 nop + and")
  (greensyn-verify "verify.smt2" "1 over - nop + and"))
  ;(greensyn-verify "verify.smt2" "dup - 180065 nop + and"))

(verify)