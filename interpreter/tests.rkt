#lang racket

(require compatibility/defmacro
         "interpreter.rkt"
         "state.rkt"
         "stack.rkt" )

(define tests '())
(define tests-passed 0)
(define tests-failed 0)
(define-syntax-rule (define-test program checks ...)
  (set! tests (cons (lambda ()
                      (let ([tests (list checks ...)]
                            [result #f]
                            [failed '()])
                        (reset!)
                        (compile-and-load program #t)
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

(define-syntax-rule (check-mem coord mem ...)
  (lambda () (let* ((m (list mem ...))
                    (memory (node:get-memory (coord->node coord)))
                    (s (length m)))
               (if (same-subset-v? m memory)
                   #f
                   (format "    Memory does not match (node ~a)
        Expected: ~a...
        Got:      ~a...\n"
                           coord
                           m
                           (take (vector->list memory) s))))))


(defmacro check-reg (coord var val)
  #`(lambda ()  (let* ((node (coord->node #,coord))
                       (state (node:current-state node))
                       (val (#,(string->symbol
                                (string-append "state-"
                                               (symbol->string var)))
                             state)))
                  (if (eq? #,val val)
                      #f
                      (format"    Failed assertion (node ~a):
        Expected: (eq ~a ~a)
        Got:      (eq ~a ~a)\n"
                             #,coord
                             (quote #,var) #,val
                             (quote #,var) val)))))

(define (data-stack->list coord)
  (let* ((state (node:current-state (coord->node coord)))
         (data (state-dstack state))
         (t (state-t state))
         (s (state-s state)))
    (cons t (cons s (stack->list data)))))

(define-syntax-rule (check-dat coord mem ...)
  (lambda () (let* ([m (list mem ...)]
                    [dstack (data-stack->list coord)]
                    [s (length m)])
               (if (same-subset? m dstack)
                   #f
                   (format "    Data stack does not match (node ~a)
        Expected: ~a...
        Got:      ~a...\n"
                           coord
                           m
                           (take dstack s))))))

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

(define-test "node 1 1 2 + node 717 1 2 3 4 +"
  (check-reg 1 t 3)
  (check-dat 1 3 0)
  (check-mem 1 67813 1 2 'end 0)

  (check-reg 717 t 7)
  (check-dat 717 7 2 1 0)
  )

(define (run-tests)
  (set! tests-failed 0)
  (set! tests-passed 0)
  (for ([test tests])
    (test))
  (display (format "passed: ~a\n" tests-passed))
  (display (format "failed: ~a\n" tests-failed)))

(run-tests)
