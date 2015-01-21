#lang racket

(require compatibility/defmacro
         "interpreter.rkt"
         "state.rkt"
         "stack.rkt")

(define (18bit n)
  (if (number? n)
      (bitwise-and n #x3ffff)
      n))

(define tests '())
(define tests-passed 0)
(define tests-failed 0)

(define-syntax-rule (define-test name program checks ...)
  (set! tests (cons (lambda ()
                      (let ([tests (list checks ...)]
                            [result #f]
                            [failed '()]
                            [compiled-file
                             (format "test-out/~a-compiled.rkt" name)]
                            [assembled-file
                             (format "test-out/~a-assembled.rkt" name)])
                        (reset!)
                        (compile-and-load program
                                          #t
                                          #:compiled-file compiled-file
                                          #:assembled-file assembled-file)
                        (step-program!*)
                        (for ([x tests])
                          (set! result (x))
                          (when result
                            (set! failed (cons result failed))))
                        (if (null? failed)
                            (set! tests-passed (add1 tests-passed))
                            (begin (display (format "FAILED: ~a: '~a'\n"
                                                    name
                                                    program))
                                   (for ([f failed])
                                     (display f))
                                   (newline)
                                   (set! tests-failed (add1 tests-failed))
                                   ))))
                    tests)))

(define-syntax-rule (check-mem coord mem ...)
  (lambda () (let* ((m (map 18bit (list mem ...)))
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
                       (val (#,(begin (unless (member var '(a b p i r s t))
                                        (raise "check-reg: invalid register"))
                                      (string->symbol
                                       (string-append "state-"
                                                      (symbol->string var))))
                             state))
                       (expect (18bit #,val)))
                  (if (eq? expect val)
                      #f
                      (format"    Failed assertion (node ~a):
        Expected: (eq ~a ~a)
        Got:      (eq ~a ~a)\n"
                             #,coord
                             (quote #,var) expect
                             (quote #,var) val)))))

(define (data-stack->list coord)
  (let* ((state (node:current-state (coord->node coord)))
         (data (state-dstack state))
         (t (state-t state))
         (s (state-s state)))
    (cons t (cons s (stack->list data)))))

(define (return-stack->list coord)
  (let* ((state (node:current-state (coord->node coord)))
         (rstack (state-rstack state))
         (r (state-r state)))
    (cons r (stack->list rstack))))

(define-syntax-rule (check-dat coord mem ...)
  (lambda () (let* ([m (map 18bit (list mem ...))]
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

(define-syntax-rule (check-ret coord mem ...)
  (lambda () (let* ([m (map 18bit (list mem ...))]
                    [rstack (return-stack->list coord)]
                    [s (length m)])
               (if (same-subset? m rstack)
                   #f
                   (format "    Return stack does not match (node ~a)
        Expected: ~a...
        Got:      ~a...\n"
                           coord
                           m
                           (take rstack s))))))

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

(define-test "basic1"
  "node 1 1 2 + node 717 1 2 3 4 +"
  (check-reg 1 t 3)
  (check-dat 1 3 0)
  (check-mem 1 67813 1 2 'end 0)

  (check-reg 717 t 7)
  (check-dat 717 7 2 1 0))

(define-test "basic2"
  "node 2 3 3 + 1"
  (check-reg 2 t 1)
  (check-reg 2 s 6)
  (check-dat 2 1 6 0))

(define-test "negate"
  "node 505 3 - 1 + dup 5 +"
  (check-reg 505 t 2)
  (check-reg 505 s -3))

(define-test "over"
  "node 500 1 2 3 over node 2 2 3 over over"
  (check-reg 500 t 2)
  (check-reg 500 s 3)
  (check-dat 500 2 3 2 1 0)
  ;;(check-dat 2 2 3 2 3)
  )

(define-test "if"
  "node 100 1 if 3 + then 1 +
   node 200 0 if 3 + then 1 +
   node 300 1 if 1 if 4 + then 3 + then 1 +
   node 301 0 if 0 if 4 + then 3 + then 1 +
   node 302 1 if 0 if 4 + then 3 + then 2 +
   node 303 0 if 1 if 4 + then 3 + then 3 +"
  (check-reg 100 t 5)
  (check-reg 100 s 0)
  (check-reg 200 t 1)
  (check-reg 200 s 0)
  (check-reg 300 t 9)
  (check-reg 300 s 1)
  (check-reg 301 t 1)
  (check-reg 301 s 0)
  (check-reg 302 t 5)
  (check-reg 302 s 1)
  (check-reg 303 t 3)
  (check-reg 303 s 0))

(define-test "-if"
  "node 100 2 -if 3 + then 5 +
   node 200 0 -if 3 + then 5 +
   node 300 2 - 1 +  -if 3 + then 5 +"
  (check-reg 100 t 7)
  (check-reg 100 s 0)
  (check-reg 200 t 5)
  (check-reg 200 s 0)
  (check-reg 300 t 6)
  (check-reg 300 s 0))

(define-test "and"
  "node 1 1 1 and
          0 0 and
          5 6 and
         -4 8 and
   node 5 2 dup dup -1 . + and
          4 dup dup -1 . + and
          7 dup dup -1 . + and"
  (check-dat 1 8 4 0 1 0)
  (check-dat 5 6 7 0 4 0 2 0))

(define-test "push&pop"
  "node 1 2 4 push 3 push 5 pop"
  (check-dat 1 3 5 2 0)
  (check-ret 1 4 0))

(define (run-tests)
  (set! tests-failed 0)
  (set! tests-passed 0)
  (for ([test tests])
    (test))
  (display (format "passed: ~a\n" tests-passed))
  (display (format "failed: ~a\n" tests-failed)))

(run-tests)
