;;; This module provides functions for dealing with 18-bit numbers.
#lang racket

(provide word->bytes bytes->word)

;;; Given an int, returns a bytestring of the int as an 18-bit number.
(define (word->bytes word)
  (integer->integer-bytes
   (if (< word 0)
       (add1 (bitwise-and (bitwise-not (- word)) #x3ffff))
       (bitwise-and word #x3ffff)) 4 #f))

;;; Given a bytestring, returns an int read from the first 18 bits.
(define (bytes->word bstr [signed? #f])
  (let ([number (integer-bytes->integer bstr 4 #f)])
    (if (and signed? (> (bitwise-and #x20000 number) 0))
        (- (add1 (bitwise-and (bitwise-not number) #x3ffff)))
        number)))
