#lang racket

(require "interpreter.rkt" "state.rkt" "stack.rkt" )

(define tests '())
(define tests-passed 0)
(define tests-failed 0)
(define-syntax-rule (define-test program checks ...)
  (set! tests (cons (lambda ()
                      (let ([tests (list checks ...)]
                            [result #f]
                            [failed '()])
                        (reset!)
                        (load-program program)
                        (step-program!*)
                        (for ([x tests])
                          (set! result (x))
                          (when result
                            (set! failed (cons result failed))))
                        (if (null? failed)
                            (set! tests-passed (add1 tests-passed))
                            (begin (display (format "FAILED: '~a'\n" program))
                                   (for ([f failed])
                                     (display f))
                                   (newline)
                                   (set! tests-failed (add1 tests-failed))
                                   ))))
                    tests)))

(define-syntax-rule (check-mem mem ...)
  (lambda () (let* ([m (list mem ...)]
                    [s (length m)])
               (if (same-subset-v? m memory)
                   #f
                   (format "    Memory does not match
        Expected: ~a...
        Got:      ~a...\n"
                           m
                           (take (vector->list memory) s))))))

(define-syntax-rule (check-var var val)
  (lambda ()  (if (eq? var val)
                  #f
                  (format"    Failed assertion:
        Expected: (eq ~a ~a)
        Got:      (eq ~a ~a)\n"
                         (quote var) val
                         (quote var) var))))

(define-syntax-rule (check-dat mem ...)
  (lambda () (let* ([m (list mem ...)]
                    [s (length m)])
               (if (same-subset? m (data-stack->list))
                   #f
                   (format "    Data stack does not match
        Expected: ~a...
        Got:      ~a...\n"
                           m
                           (take (data-stack->list) s))))))

(define (same-subset-v? list vector)
  (define (same? i list)
    (or (null? list)
        (and (eq? (vector-ref vector i)
                  (car list))
             (same? (add1 i) (cdr list)))))
  (same? 0 list))

(define (same-subset? l1 l2)
  (or (or (null? l1)
          (null? l2))
      (and (eq? (car l1) (car l2))
           (same-subset? (cdr l1) (cdr l2)))))

(define-test "yellow 1 node green 1 2 +"
  (check-var t 3)
  (check-dat 3 0)
  (check-mem 67813 1 2 0))


(define (run-tests)
  (set! tests-failed 0)
  (set! tests-passed 0)
  (for ([test tests])
    (test))
  (display (format "passed: ~a\n" tests-passed))
  (display (format "failed: ~a\n" tests-failed)))

(run-tests)
