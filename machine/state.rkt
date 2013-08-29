#lang racket

(require racket/list "stack.rkt" "programs.rkt")

(provide (all-defined-out))

;;; Number of entries for the default communication vectors.
(define ENTRIES 4)

(struct progstate (a b p i r s t data return memory) #:mutable #:transparent)
;; (struct commstate (send-u send-d send-l send-r send-io
;;                    recv-u recv-d recv-l recv-r recv-io
;;                    sendp-u sendp-d sendp-l sendp-r sendp-io
;;                    recvp-u recvp-d recvp-l recvp-r recvp-io
;;                    order-u order-d order-l order-r order-io) #:transparent #:mutable)
(struct commstate (data type recv p) #:mutable #:transparent)

;;; The blank state that the interpreter usually starts in.
(define start-state (progstate 0 0 0 0 0 0 0
                      (stack 0 (make-vector 8))
                      (stack 0 (make-vector 8))
                      (make-vector MEM-SIZE)))

;;; Generates a state with a randomized data stack and everything else
;;; empty.
(define (random-state [max-size #x40000])
  (struct-copy progstate start-state
               [a      0]
               [b      0]
               [r      0]
               [s 0]
               [t 0]
               [data (stack 0 (make-vector 8))]
               [return (stack 0 (make-vector 8))]))
               ;[s      (random max-size)]
               ;[t      (random max-size)]
               ;[data   (random-stack max-size)]
               ;[return (stack 0 (make-vector 8))]))

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
    ((constraint var ...)        (struct-copy progstate constraint-none [var #t] ...))))

(define-syntax constraint-data
  (syntax-rules ()
    ((constraint-data x var ...) (struct-copy progstate constraint-none [data x] [var #t] ...))))
