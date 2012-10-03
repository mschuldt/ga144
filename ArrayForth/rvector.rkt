#lang racket

; The resizable vector data type.  Must be put into a list to support mutation.

(provide make-rvector rvector-ref rvector-length rvector-set! proc-ref proc-add! proc-replace!)

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

(define procedures (make-rvector 1000))
(define proc-index 1) ; start from 1 because codespace is init to 0

(define (proc-ref vlist index)
  (rvector-ref procedures (rvector-ref vlist index)))

(define (proc-add! vlist index value)
  ;(printf "add-proc ~e\n" proc-index)
  (rvector-set! vlist index proc-index)
  (rvector-set! procedures proc-index value)
  (set! proc-index (add1 proc-index)))

(define (proc-replace! vlist index value)
  (let [(old-index (rvector-ref vlist index))]
    (rvector-set! procedures old-index value)))
      
;(define (codespace-set! vlist index value)
;  (if (|| (>= index (rvector-length vlist)) (= (rvector-ref vlist index) 0))
;      (codespace-add! vlist index value)
;      (codespace-replace! vlist index value)))