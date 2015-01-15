#lang racket

(provide (all-defined-out))

;;; Represents a stack with its top registers and the rest of its
;;; elements as vectors.
(struct stack ([registers #:mutable] [sp #:mutable] rest numregs numrest) #:transparent)

;;; Creates an empty stack with the specified number of top registers
;;; (2 by default).
(define (make-stack registers stack-body-size default)
  (stack (make-vector registers default) 0 (make-vector stack-body-size default) registers stack-body-size))

(struct infinite-stack (lst) #:mutable)

(define (make-infinite-stack)
  (infinite-stack '()))

(define (stack-length st)
  (if (infinite-stack? st)
      (length (infinite-stack-lst st))
      (+ (stack-numregs st)
	 (stack-numrest st))))

;;; Push an element onto the given stack:
(define (push! old-stack element)
  (if (infinite-stack? old-stack)
      (set-infinite-stack-lst! old-stack (cons element (infinite-stack-lst old-stack)))
      (let-values ([(new-registers next)
		    (vector-split-at-right (stack-registers old-stack) 1)])
	(set-stack-registers! old-stack (vector-append `#(,element) new-registers))
	(set-stack-sp! old-stack (modulo (add1 (stack-sp old-stack)) (stack-numrest old-stack)))
	(vector-set! (stack-rest old-stack) (stack-sp old-stack)
		     (vector-ref next 0)))))

;;; Pops an element off the stack and returns it.
(define (pop! old-stack)
  (if (infinite-stack? old-stack)
      (begin0 (car (infinite-stack-lst old-stack))
        (set-infinite-stack-lst! old-stack (cdr (infinite-stack-lst old-stack))))
      (let-values ([(ret new-registers)
		    (vector-split-at (stack-registers old-stack) 1)])
	(let ([element (vector-ref (stack-rest old-stack) (stack-sp old-stack))])
	  (set-stack-registers! old-stack (vector-append new-registers `#(,element))))
	(set-stack-sp! old-stack (modulo (sub1 (stack-sp old-stack)) (stack-numrest old-stack)))
	(vector-ref ret 0))))

;;; Returns the top value of the stack without taking it off the stack.
(define (peek old-stack [pos 0])
  (if (infinite-stack? old-stack)
      (list-ref (infinite-stack-lst old-stack) pos)
      (let ((modpos (modulo pos (stack-length old-stack))))
	(if (< modpos (stack-numregs old-stack))
	    (vector-ref (stack-registers old-stack) modpos)
	    (vector-ref (stack-rest old-stack)
			(modulo (- (stack-sp old-stack)
				   (- modpos (stack-numregs old-stack)))
				(stack-numrest old-stack)))))))
