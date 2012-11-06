#lang racket

(require "forth_state.rkt" "rvector.rkt")
(provide (all-defined-out))

(define (displaynl arg)
  (display arg)
  (newline))

(define (displayspace arg)
  (display arg)
  (display " "))

(add-primitive-word! #f "send"
                     (lambda ()
                       (let* [(arg1 (pop-int! dstack #t))
                              (arg2 (pop-int! dstack #t))]
			 (if (rvector-ref send-recv-table arg1)
			     (set! pc (sub1 pc))
			     (rvector-set! send-recv-table arg1 arg2)))))

(add-primitive-word! #f "recv"
                     (lambda ()
                       (let* [(arg1 (pop-int! dstack #t))
			      (val (rvector-ref send-recv-table arg1))]
			 (if val
			     (push-int! dstack val)
			     (begin (push-int! dstack arg1) (set! pc (sub1 pc)))))))

; Debugging
(add-primitive-word! #f ".s" (lambda () (print-stack dstack)))
(add-primitive-word! #f ".ns" (lambda () (print state-index) (display ": ") (print-stack dstack) (newline)))
(add-primitive-word! #f ".r" (lambda () (print-stack rstack)))
(add-primitive-word! #f ".nr" (lambda () (print state-index) (display ": ") (print-stack rstack) (newline)))
