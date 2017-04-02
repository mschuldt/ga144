#lang racket ;; -*- lexical-binding: t -*-
;;; Some utilities for working with 8-word circular stacks

(require "el.rkt")

(provide (all-defined-out))

(struct stack (sp body) #:mutable #:transparent)

(define (make-stack len (init 0))
  (stack 0 (make-vector len init)))

;;; Copies the given stack. This keeps mutable vectors from being
;;; shared between different stacks.
(define (copy-stack s)
  (stack (stack-sp s) (vector-copy (stack-body s))))

;;; Print a circular stack:
(define (display-stack stack)
  (for ((i (in-range 0 8)))
    (printf " ~x" (vector-ref (stack-body stack)
                              (modulo (- (stack-sp stack) i) 8)))))

;;; Pushes a value to the given stack's body.
(define (push-stack! stack value)
  (set-stack-sp! stack (modulo (add1 (stack-sp stack)) 8))
  (vector-set! (stack-body stack) (stack-sp stack) value))

;;; Pops from the given stack's body.
(define (pop-stack! stack)
  (let ((ret-val (vector-ref (stack-body stack) (stack-sp stack))))
    (set-stack-sp! stack (modulo (sub1 (stack-sp stack)) 8))
    ret-val))

;;; Returns a stack with randomized entries, each less than max-size.
(define (random-stack (max-size #x40000))
  (stack 0 (vector-map! (lambda (_) (random max-size)) (make-vector 8))))

(define (stack->list stack)
  (let* ((len (vector-length (stack-body stack)))
         (stack-v (stack-body stack))
         (sp (stack-sp stack)))
    (for/list ((i len))
      (vector-ref stack-v (modulo (- sp i) 8)))))

(define (stack->vector stack)
  (stack->vector (vector->list stack)))
