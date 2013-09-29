#lang racket

(require racket/list "stack.rkt" "programs.rkt")

(provide (all-defined-out))

;;; Number of entries for the default communication vectors.
(define ENTRIES 4)

(struct progstate (a b p i r s t data return memory) #:mutable #:transparent)
(struct commstate (data type recv p) #:mutable #:transparent)

;;; The blank state that the interpreter usually starts in.
(define start-state (progstate 0 0 0 0 0 0 0
                      (stack 0 (make-vector 8))
                      (stack 0 (make-vector 8))
                      (make-vector MEM-SIZE)))

(define (default-commstate
	 #:data [data (make-vector ENTRIES 0)]
	 #:type [type (make-vector ENTRIES 0)]
         #:recv [recv (make-vector ENTRIES 0)])
  (commstate data type recv 0))

;;; The empty constraint. Pretty useless.
(define constraint-none (progstate #f #f #f #f #f #f #f #f #f #f))

;;; Constrain everything. We must have perfection!
(define constraint-all (progstate #t #t #t #t #t #t #t 8 #t #t))

;;; Defines a constraint for some fields. For example, `(constraint
;;; t)' is the same as constraint-only-t and evaluates to `(progstate
;;; #f #f #f #f #f #f #t #f #f #f)'. If you want to constrain
;;; everything *except* the given fields, start with the except
;;; keyword: `(constrain except t)' constrains everything but t. 
(define-syntax constraint
  (syntax-rules (except)
    ((constraint except var ...) (struct-copy progstate constraint-all  [var #f] ...))
    ((constraint var ...)        (struct-copy progstate constraint-none [var #t] ...))
    ((constraint var ...)        (struct-copy progstate constraint-none [var #t] ...))
    ((constraint (key val) ...)  (struct-copy progstate constraint-none [key val] ...))
    ))

(define-syntax constraint-data
  (syntax-rules ()
    ((constraint-data x var ...) (struct-copy progstate constraint-none [data x] [var #t] ...))))

(define-syntax default-state
  (syntax-rules ()
    ((default-state) 
     (struct-copy progstate start-state))
    ((default-state (key val) ...)
     (struct-copy progstate start-state [key val] ...))))