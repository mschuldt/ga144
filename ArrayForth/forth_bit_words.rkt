#lang racket

(require "forth_state.rkt")
(provide (all-defined-out))

; Booleans

(define true -1)
(define false 0)

;; TODO: there is no ior but i couldn't find where they define ior. i think it should be somewhere
(add-primitive-word! #f "ior"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (bitwise-ior arg1 arg2))))) ; ior - inclusive or

;; TODO: this is how they define invert : invert begin - ; ???
(add-primitive-word! #f "invert" (lambda () (push-int! (bitwise-not (pop-int! #t)))))

(add-primitive-word! #f "and"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (bitwise-and arg1 arg2)))))

(add-primitive-word! #f "or"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (bitwise-xor arg1 arg2))))) ; xor - exclusive or


; Math

; Addition - adds 2 ints, pushes it back onto the stack.  Treated as signed, but works for unsigned as well.
(add-primitive-word! #f "+"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (+ arg1 arg2)))))

; Invert
(add-primitive-word! #f "-"(lambda () (push-int! (bitwise-not (pop-int! #t)))))

; Multiply step. 
; Signed Multiplicand is in S, unsigned multiplier in A, T=0 at start of a step sequence.
; Uses T:A as a 36-bit shift register with multiplier in A. Does the following:

; 1. If bit A0 is one, S and T are added as though they had both been extended to be
; 19 bit signed numbers, and the 37-bit concatenation of this sum and A is shifted
; right one bit to replace T:A. Overflow may occur if S and T are both nonzero and
; their signs differ; this can only occur through improper initialization of T.
; 2. If bit A0 is zero, shifts the 36-bit register T:A right one bit arithmetically 
; (T17 is not changed and is copied into T16. T0 is copied to A17 and A0 is discarded.)
(define (multiply-step-zero a t)
  (if (= (bitwise-and t 1) 1)
      ; if T0 = 1 then A17 = 1 (+ 2^17 = 131072)
      (set! rega (+ 131072 (quotient a 2)))
      (set! rega (quotient a 2)))
  (push-int! (+ (bitwise-and t 131072) (quotient t 2))))

(define (multiply-step-proc)
  (let* [(a (rega))]
    (if (= (bitwise-and a 1) 1)
        (push-int! (+ (pop-int! #f) (get-int #f)))
        (void))
    (let* [(t (pop-int! #f))]
      (if (= (bitwise-and t 1) 1)
          (set! rega (+ 131072 (quotient a 2)))
          (set! rega (quotient a 2)))
      (push-int! (+ (bitwise-and t 131072) (quotient (bitwise-and t 262143) 2))))))

(add-primitive-word! #f "+*" multiply-step-proc)

(add-primitive-word! #f "2*" (lambda () (push-int! (* (pop-int! #t) 2))))
(add-primitive-word! #f "2/" (lambda () (push-int! (/ (pop-int! #t) 2))))

(add-primitive-word! #f "*"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (* arg1 arg2)))))

(add-primitive-word! #f "/"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (quotient arg2 arg1)))))

(add-primitive-word! #f "mod"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (remainder arg2 arg1)))))

(add-primitive-word! #f "/mod"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (remainder arg2 arg1))
                         (push-int! (quotient arg2 arg1)))))

(add-primitive-word! #f "*/"
                     (lambda ()
                       (let* [(n3 (pop-int! #t))
                              (n2 (pop-int! #t))
                              (n1 (pop-int! #t))
                              (intermediate (* n1 n2))]
                         (push-int! (quotient intermediate n3)))))

(add-primitive-word! #f "min"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (min arg2 arg1)))))

(add-primitive-word! #f "max"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (max arg2 arg1)))))

; NOP
(add-primitive-word! #f "." (lambda () (void)))

; Word alignment
(add-primitive-word! #f ".." (lambda () (void)))

; Comments
(define (comment)
  (if (equal? (read-char) #\))
      (void)
      (comment)))
(add-primitive-word! #t "(" comment)
