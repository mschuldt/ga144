#lang racket

(require racket/list "stack.rkt")

(provide (all-defined-out))

;;; this is consistent with arrayForth
(define UP #x145) ;325
(define DOWN #x115) ;277
(define LEFT #x175)
(define RIGHT #x1d5)
(define IO #x15d)

;;; Number of entries for the default communication vectors.
(define ENTRIES 4)

(struct progstate (a b p i r s t data return memory) #:mutable #:transparent)
(struct commstate (send-u send-d send-l send-r
                   recv-u recv-d recv-l recv-r
                   sendp-u sendp-d sendp-l sendp-r
                   recvp-u recvp-d recvp-l recvp-r) #:transparent #:mutable)

;;; The blank state that the interpreter usually starts in.
(define start-state (progstate 0 0 0 0 0 0 0
                      (stack 0 (make-vector 8))
                      (stack 0 (make-vector 8))
                      (make-vector 64)))

;;; Generates a state with a randomized data stack and everything else
;;; empty.
(define (random-state [max-size #x40000])
  (struct-copy progstate start-state
               [a      (random max-size)]
               [b      (random max-size)]
               [r      (random max-size)]
               [s      (random max-size)]
               [t      (random max-size)]
               [data   (random-stack max-size)]
               [return (stack 0 (make-vector 8))]))

(define (default-commstate
	 #:send-u [send-u (make-vector ENTRIES 0)]
	 #:send-d [send-d (make-vector ENTRIES 0)]
	 #:send-l [send-l (make-vector ENTRIES 0)]
	 #:send-r [send-r (make-vector ENTRIES 0)]
	 #:recv-u [recv-u (make-vector ENTRIES 0)]
	 #:recv-d [recv-d (make-vector ENTRIES 0)]
	 #:recv-l [recv-l (make-vector ENTRIES 0)]
	 #:recv-r [recv-r (make-vector ENTRIES 0)])
  (commstate send-u send-d send-l send-r recv-u recv-d recv-l recv-r 0 0 0 0 0 0 0 0))

;;; The empty constraint. Pretty useless.
(define constraint-none (progstate #f #f #f #f #f #f #f #f #f #f))

;;; Constrain everything. We must have perfection!
(define constraint-all (progstate #t #t #t #t #t #t #t #t #t #t))

;;; Defines a constraint for some fields. For example, `(constraint
;;; t)' is the same as constraint-only-t and evaluates to `(progstate
;;; #f #f #f #f #f #f #t #f #f #f)'. If you want to constrain
;;; everything *except* the given fields, start with the except
;;; keyword: `(constrain except t)' constrains everything but t. 
(define-syntax constraint
  (syntax-rules (except)
    ((constraint except var ...) (struct-copy progstate constraint-all  [var #f] ...))
    ((constraint var ...)        (struct-copy progstate constraint-none [var #t] ...))))
