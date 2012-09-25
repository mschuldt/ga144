#lang racket

(provide cell_bytes single_cell_limit double_cell_limit
         int->bytes double->bytes string->bytes)

(define decimal_digits (string->list "0123456789"))
(define (digit? char)
  (if (member char decimal_digits) #t #f))

(define punctuation (string->list "+,-./:"))
(define (punct? char)
  (if (member char punctuation) #t #f))

(define cell_bytes 4) ; Must be 2 or 4, due to limitations of integer->integer-bytes
(define single_cell_limit (expt 2 (* cell_bytes 8)))
(define double_cell_limit (expt 2 (* (* cell_bytes 2) 8)))

(define (int->bytes num)
  (integer->integer-bytes
         (modulo num single_cell_limit) cell_bytes #f #t))
(define (double->bytes num)
  (integer->integer-bytes
         (modulo num double_cell_limit) (* 2 cell_bytes) #f #t))

; Converts the string representation of a number to its bytes.
; Assumes (> (string-length str) 0)
(define (string->bytes str)
  (define (create-bytes num punct)
    (if punct
        (double->bytes num)
        (int->bytes num)))
  (define (get-num pos result punct negate)
    (if (= pos (string-length str))
        (create-bytes (if negate (- result) result) punct)
        (let [(new_digit (string-ref str pos))]
          (cond [(digit? new_digit)
                 (get-num (+ pos 1)
                          (+ (* result 10)
                             (- (char->integer new_digit) (char->integer #\0)))
                          punct
                          negate)]
                [(punct? new_digit)
                 (get-num (+ pos 1) result #t negate)]
                [else #f]))))
  (cond [(and (= (string-length str) 1) (not (digit? (string-ref str 0)))) #f]
        [(eq? #\- (string-ref str 0))
         (get-num 1 0 #f #t)]
        [else (get-num 0 0 #f #f)]))