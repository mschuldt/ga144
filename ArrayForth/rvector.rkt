#lang racket

; The resizable vector data type.  Must be put into a mutable pair to support mutation.
; The cdr holds the highest index set so far + 1.  (This is a free space where we can store something.)

(provide (all-defined-out))

(define (make-rvector size [default 0])
  (mcons (make-vector size default) (mcons default 0)))

(define (plain-vector rvec)
  (let ((result (make-vector (rvector-length rvec))))
    (vector-copy! result 0 (mcar rvec) 0 (rvector-length rvec))
    result))
(define default-value (compose mcar mcdr))
(define rvector-length (compose mcdr mcdr))

(define (rvector-ref vlist index)
  (vector-ref (mcar vlist) index))

(define (rvector-set! vlist index value)
  (set-mcdr! (mcdr vlist) (max (+ index 1) (mcdr (mcdr vlist))))
  (when (>= index (vector-length (mcar vlist)))
	(let [(old-vec (mcar vlist))]
	  (set-mcar! vlist
		     (make-vector (mcdr (mcdr vlist)) (mcar (mcdr vlist))))
	  (vector-copy! (mcar vlist) 0 old-vec)))
  (vector-set! (mcar vlist) index value))

(define (add-element! vlist elmt)
  (rvector-set! vlist (rvector-length vlist) elmt))

; This uses the same signature as vector-copy!
(define (rvector-copy! dest dest-start src [src-start 0] [src-end (vector-length (mcar src))])
  ;; First force the dest to be at least as long as necessary
  (rvector-set! dest (+ dest-start (sub1 (- src-end src-start))) 0)
  (vector-copy! (mcar dest) dest-start (mcar src) src-start src-end))

