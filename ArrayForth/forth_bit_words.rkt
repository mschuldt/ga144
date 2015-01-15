#lang racket

(require "classes.rkt")
(provide add-bit-words!)

;; Booleans

(define true -1)
(define false 0)

(define (add-bit-words!)
  (add-instruction!
   "and" ; bitwise and
   (lambda (i)
     (let* [(dstack (send i get 'dstack))
	    (arg1 (pop-int! dstack #t))
	    (arg2 (pop-int! dstack #t))]
       (push-int! dstack (bitwise-and arg1 arg2)))))

  (add-instruction!
   "or" ; bitwise exclusive or
   (lambda (i)
     (let* [(dstack (send i get 'dstack))
	    (arg1 (pop-int! dstack #t))
	    (arg2 (pop-int! dstack #t))]
       (push-int! dstack (bitwise-xor arg1 arg2)))))

  (add-instruction!
   "-" ; invert (bitwise negation)
   (lambda (i)
     (let [(dstack (send i get 'dstack))]
       (push-int! dstack (bitwise-not (pop-int! dstack #t))))))


                                        ; Math

                                        ; Addition - adds 2 ints, pushes it back onto the stack.  Treated as signed, but works for unsigned as well.
  (add-instruction!
   "+"
   (lambda (i)
     (let* [(dstack (send i get 'dstack))
	    (arg1 (pop-int! dstack #t))
	    (arg2 (pop-int! dstack #t))]
       (push-int! dstack (+ arg1 arg2)))))

  ;; Multiply step.
  ;; Signed Multiplicand is in S, unsigned multiplier in A, T=0 at start of a step sequence.
  ;; Uses T:A as a 36-bit shift register with multiplier in A. Does the following:

  ;; 1. If bit A0 is one, S and T are added as though they had both been extended to be
  ;; 19 bit signed numbers, and the 37-bit concatenation of this sum and A is shifted
  ;; right one bit to replace T:A. Overflow may occur if S and T are both nonzero and
  ;; their signs differ; this can only occur through improper initialization of T.
  ;; 2. If bit A0 is zero, shifts the 36-bit register T:A right one bit arithmetically
  ;; (T17 is not changed and is copied into T16. T0 is copied to A17 and A0 is discarded.)
  (define (multiply-step-proc i)
    (let* [(a (send i get 'rega))
	   (dstack (send i get 'dstack))
	   (arg1 (pop-int! dstack #f))]
      (when (= (bitwise-and a 1) 1)
        (push-int! dstack (+ arg1 (get-int dstack #f))))
      (let [(t (pop-int! dstack #f))]
	(if (= (bitwise-and t 1) 1)
	    (send i set 'rega (+ 131072 (quotient a 2)))
	    (send i set 'rega (quotient a 2)))
	(push-int! dstack (+ (bitwise-and t 131072)
			     (quotient (bitwise-and t 262143) 2))))))

  (add-instruction! "+*" multiply-step-proc)

  (add-instruction!
   "2*"
   (lambda (i)
     (let [(dstack (send i get 'dstack))]
       (push-int! dstack (* (pop-int! dstack #t) 2)))))

  (add-instruction!
   "2/"
   (lambda (i)
     (let [(dstack (send i get 'dstack))]
       (push-int! dstack (/ (pop-int! dstack #t) 2)))))

  (add-instruction! "." (lambda (i) (void)))
  (make-instruction-synonym "." "nop"))
