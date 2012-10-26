#lang racket

;;; Some very simple tests of the interpreter.

(require "interpreter.rkt" rackunit)

(define tests '())

;;; Defines a new test running the given arrayForth program followed
;;; by the body code. The interpreter state is reset automatically
;;; before the test is run.
(define-syntax define-test
  (syntax-rules ()
    ((_ program statement ...)
     (set! tests (cons (lambda () (reset!)
                               (load-program program)
                               (step-program!*)
                               statement ...)
                       tests)))))

(define-test "@p @p + nop 2 3"
  (check-equal? p 3)
  (check-equal? t 5))

(define-test "@p - nop nop 0"
  (check-equal? p 2)
  (check-equal? t (18bit (bitwise-not 0))))

;;; This should write 42 to slot 4 in memory then put it on top of the
;;; stack.
(define-test "@p b! @p nop 4 42 !b @p nop nop"
  (check-equal? p 5)
  (check-equal? t 42)
  (check-equal? (vector-ref memory 4) 42))

;;; Run all the currently defined tests.
(define (run-tests) (for ([test tests]) (test)))
