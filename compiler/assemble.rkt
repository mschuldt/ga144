#lang racket
(provide assemble-word
         assemble)

(define names '#(";" "ex" "jump" "call" "unext" "next" "if" "-if" "@p" "@+" "@b"
                 "@" "!p" "!+" "!b" "!" "+*" "2*" "2/" "-" "+" "and" "or" "drop"
                 "dup" "pop" "over" "a" "." "push" "b!" "a!"))

(define (assemble-inst word slot shift)
  (let ((inst (vector-ref word slot)))
    (if (string? inst)
        (arithmetic-shift (floor (/ (vector-member inst names)
                                    (if (= slot 3) 4 1)))
                          shift)
        (or inst 0))))

(define (assemble-word word)
  (cond ((number? word) word)
        ((or (equal? word (vector #f #f #f #f))
             (not word)) #f)
        (else (let* ((d (assemble-inst word 3 0))
                     (c (assemble-inst word 2 3))
                     (b (assemble-inst word 1 8))
                     (a (assemble-inst word 0 13)))
                (bitwise-ior a b c d)))))

(define (assemble nodes)
  ;;NODES is a list with forms (NODE-NUMBER . MEMORY-VECTOR)
  ;;mutates the MEMORY-VECTORs in place
  (unless (null? nodes)
    (for ([i 64])
      (vector-set! (cdar nodes) i (assemble-word (vector-ref (cdar nodes) i))))
    (assemble (cdr nodes))))
