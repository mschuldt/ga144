#lang racket

;;; Some very simple tests of the interpreter.

(require "interpreter.rkt" "stack.rkt" rackunit)

(define tests '())

;;; Creates an arrayForth stack, turning the 8 given numbers into
;;; 18bits. If too few or too many numbers are passed in, this raises
;;; an error.
(define forth-stack
  (lambda contents
    (unless (= (length contents) 8) (raise "Wrong stack size."))
    (list->vector (map 18bit contents))))

(define old-state (make-parameter 1))

;;; Defines a new test running the given arrayForth program followed
;;; by the body code. The interpreter state is reset automatically
;;; before the test is run.
(define-syntax define-test
  (syntax-rules ()
    ((_ program statement ...)
     (set! tests (cons (lambda ()
                         (reset!)
                         (load-program program)
                         (step-program!*)
                         statement ...) tests)))))

;;; Helper macro for `check-unchanged?'  Checks if the given id was not
;;; changed after running the program. This currently assumes you
;;; started with a call to (reset!).
(define-syntax (check-unchanged-1 stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax
         ([get (datum->syntax
                stx (string->symbol
                     (format "state-~a" (symbol->string (syntax->datum #'id)))))])
       (syntax (check-equal? id (get start-state))))]))

;;; Checks whether the given variables (registers/stacks/memory) were
;;; not changed from the start state.
(define-syntax check-unchanged?
  (syntax-rules ()
    ((_ id ...)
     (begin (check-unchanged-1 id) ...))))

(define-test "@p @p + nop 2 3"
  (check-equal? p 3)
  (check-equal? t 5)
  (check-unchanged? a b r s return))

(define-test "@p - nop nop 0"
  (check-equal? p 2)
  (check-equal? t (18bit -1))
  (check-unchanged? a b r s return))

;;; This should write 42 to slot 4 in memory then put it on top of the
;;; stack.
(define-test "@p b! @p nop 4 42 !b @p nop nop"
  (check-equal? p 5)
  (check-equal? t 42)
  (check-equal? b 4)
  (check-equal? (vector-ref memory 4) 42)
  (check-unchanged? a r s return))

;;; An example of checking the whole data stack as well as the pc.
(define-test "- dup dup dup dup dup dup dup"
  (check-equal? p 2)
  (check-equal? t (18bit -1))
  (check-equal? s (18bit -1))
  (check-equal? data (stack 7 (forth-stack 0 0 -1 -1 -1 -1 -1 -1)))
  (check-unchanged? a b r return))

;;; Testing the basic instructions:
(define-test "call 10"   ; call
  (check-equal? r 1)
  (check-equal? p 10)
  (check-unchanged? a b s t data))

;;; Run all the currently defined tests.
(define (run-tests) (for ([test tests]) (test)))
