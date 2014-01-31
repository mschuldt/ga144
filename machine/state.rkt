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
(define constraint-all (progstate #t #t #t #t #t #t #t 8 8 #t))

;;; Defines a constraint for some fields. For example, `(constraint
;;; t)' is the same as constraint-only-t and evaluates to `(progstate
;;; #f #f #f #f #f #f #t #f #f #f)'. If you want to constrain
;;; everything *except* the given fields, start with the except
;;; keyword: `(constrain except t)' constrains everything but t. 
(define-syntax constraint
  (syntax-rules (except data return)

    ((constraint (return val1) (data val2) var ...)  
     (struct-copy progstate constraint-none [return val1] [data val2] [var #t] ...))
    ((constraint (data val2) (return val1) var ...)  
     (struct-copy progstate constraint-none [return val1] [data val2] [var #t] ...))

    ((constraint (return val1) (data val2))
     (struct-copy progstate constraint-none [return val1] [data val2]))
    ((constraint (data val2) (return val1))
     (struct-copy progstate constraint-none [return val1] [data val2]))

    ((constraint (data val) var ...)  
     (struct-copy progstate constraint-none [data val] [var #t] ...))
    ((constraint (data val))  
     (struct-copy progstate constraint-none [data val]))
    ((constraint (return val) var ...)  
     (struct-copy progstate constraint-none [return val] [var #t] ...))
    ((constraint (return val))  
     (struct-copy progstate constraint-none [return val]))

    ((constraint except var ...) (struct-copy progstate constraint-all  [var #f] ...))
    ((constraint var ...)        (struct-copy progstate constraint-none [var #t] ...))
    ))



(define-syntax copy-state
  (syntax-rules (data-pair)
    ((copy-state x [data-pair (i i-val) ...] [key val] ...)
     (let ([body (make-vector 8)]
           [pairs (list (cons i i-val) ...)])
       (for ([p pairs])
            (vector-set! body (modulo (- (car p)) 8) (cdr p)))
       (struct-copy progstate x [data (stack 0 body)] [key val] ...)))

    ((copy-state x [key val] ...)
     (struct-copy progstate x [key val] ...))))

(define-syntax default-state
  (syntax-rules (data-pair)
    ((default-state) 
     (struct-copy progstate start-state))

    ((default-state [data-pair (i i-val) ...] [key val] ...)
     (let ([body (make-vector 8)]
           [pairs (list (cons i i-val) ...)])
       (for ([p pairs])
            (vector-set! body (modulo (- (car p)) 8) (cdr p)))
       (struct-copy progstate start-state [data (stack 0 body)] [key val] ...)))

    ((default-state [key val] ...)
     (struct-copy progstate start-state [key val] ...))))
   

(define (constrain-stack state precond)
  (when (list? precond)
    (for ([assume precond]
          [i (reverse (range (length precond)))])
      (cond
       [(= i 0) (set-progstate-t! state assume)]
       [(= i 1) (set-progstate-s! state assume)]
       [else
        (let ([body (stack-body (progstate-data state))])
          (vector-set! body (modulo (- 2 i) 8) assume))])))
  state)