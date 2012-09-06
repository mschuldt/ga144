#lang racket

; The resizable vector data type.  Must be put into a list to support mutation.

(provide make-rvector rvector-ref rvector-length rvector-set!)

(define (make-rvector size)
  (mcons (make-vector size) size))

(define (rvector-ref vlist index)
  (vector-ref (mcar vlist) index))

(define rvector-length mcdr)

(define (rvector-set! vlist index value)
  (if (>= index (rvector-length vlist))
      (let [(old-vec (mcar vlist))]
        (set-mcdr! vlist (max (+ index 1) (* 2 (rvector-length vlist))))
        (set-mcar! vlist
                  (make-vector (mcdr vlist)))
        (vector-copy! (mcar vlist) 0 old-vec))
      0) ; Used as an else statement.  Nothing really needs to be done.
  (vector-set! (mcar vlist) index value))
      