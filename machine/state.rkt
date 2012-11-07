#lang racket

(require "stack.rkt")

(provide (all-defined-out))

(struct progstate (a b p i r s t data return memory) #:mutable #:transparent)
(struct commstate (send-u send-d send-l send-r recv-u recv-d recv-l recv-r sendp-u sendp-d sendp-l sendp-r recvp-u recvp-d recvp-l recvp-r))

;;; The blank state that the interpreter usually starts in.
(define start-state (progstate 0 0 0 0 0 0 0
                      (stack 0 (make-vector 8))
                      (stack 0 (make-vector 8))
                      (make-vector 64)))

;; ;;; Generates a state with a randomized data stack and everything else
;; ;;; empty.
;; (define (random-state)
;;   ())

(define ENTRIES 4)
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
