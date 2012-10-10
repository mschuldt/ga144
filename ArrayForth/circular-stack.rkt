#lang racket

(provide stack make-stack)

;;; How big the circular stack bodies are.
(define stack-body-size 8)

;;; Pushes the specified element onto the given vector.
(define (push-vector vector value)
  (list->vector (cons value (drop-right (vector->list vector)))))

;;; Pops an element off the vector, copying it to the end. Returns
;;; both the element and the new vector.
(define (pop-vector vector value)
  (let ([ls (vector->list vector)])
    `(,(cons ls) ,(list->vector (append (cdr ls) (list (cons ls)))))))

;;; Represents a stack with its top registers and the rest of its
;;; elements as vectors.
(struct stack (top rest))

;;; Creates an empty stack with the specified number of top registers
;;; (2 by default).
(define (make-stack [registers 2])
  (stack (make-vector registers) (make-vector stack-body-size)))
