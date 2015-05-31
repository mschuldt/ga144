#lang racket
(provide assemble)

(define names '#(";" "ex" "jump" "call" "unext" "next" "if" "-if" "@p" "@+" "@b"
                 "@ !p" "!+" "!b" "!" "+*" "2*" "2/" "-" "+" "and" "or" "drop"
                 "dup" "pop" "over" "a" "." "push" "b!" "a!"))

(define (extract word slot shift)
  (let ((x (vector-ref word slot)))
    (if (string? x)
        (arithmetic-shift (floor (/ (vector-member x names)
                                    (if (= slot 3) 4 1)))
                          shift)
        (or x 0))))

(define (assemble-word word)
  (if (number? word)
      word
      (let* ((d (extract word 3 0))
             (c (extract word 2 (if d 3 0)))
             (b (extract word 1 (if c 8 0)))
             (a (extract word 0 (if b 13 0))))
        (bitwise-ior (or a 0) (or b 0) (or c 0) (or d 0)))))

(define (assemble nodes)
  ;;NODES is a list with forms (NODE-NUMBER . MEMORY-VECTOR)
  ;;mutates the MEMORY-VECTORs in place
  (unless (null? nodes)
    (for ([i 64])
      (vector-set! (cdar nodes) i (assemble-word (vector-ref (cdar nodes) i))))
    (assemble (cdr nodes))))
