#lang racket

(provide disassemble
         display-disassemble
         disassemble-word)

(define names '#(";" "ex" "jump" "call" "unext" "next" "if" "-if" "@p" "@+" "@b"
                 "@" "!p" "!+" "!b" "!" "+*" "2*" "2/" "-" "+" "and" "or" "drop"
                 "dup" "pop" "over" "a" "." "push" "b!" "a!"))

(define address-ops '("jump" "call" "next" "if" "-if"))
(define ops-that-end-word '("unext" ";"))

(define (disassemble-inst from from^ to index start end jump)
  ;;FROM is the integer we are dissembling
  ;;FROM^ = FROM ^ 0x15555
  ;;TO is a vector we disassemble into
  ;;START, END mark the bit positions of the instruction in FROM
  ;;JUMP is the size of the jump field in FROM, #f if none
  (let ((inst (vector-ref names (* (bitwise-bit-field from^ start end)
                                   (if jump 1 4)))))
    (vector-set! to index inst)
    (if (and (member inst address-ops)
             (< index 3))
        (begin (vector-set! to (add1 index) (bitwise-bit-field from 0 jump))
               #f)
        (not (member inst ops-that-end-word)))))

(define (disassemble-word word)
  (let ((to (make-vector 4 #f))
        (word^ (and word (bitwise-xor word #x15555))))
    (and word
         (disassemble-inst word word^ to 0 13 18 10)
         (disassemble-inst word word^ to 1 8 13 8)
         (disassemble-inst word word^ to 2 3 8 3)
         (disassemble-inst word word^ to 3 0 3 #f))
    to))

(define (disassemble nodes)
  ;;NODES is a list with forms (NODE-NUMBER . MEMORY-VECTOR)
  ;;mutates the MEMORY-VECTORs in place
  (unless (null? nodes)
    (for ([i 64])
      (vector-set! (cdar nodes) i (disassemble-word (vector-ref (cdar nodes) i))))
    (disassemble (cdr nodes))))

(define (display-disassemble nodes [all? #f])
  ;;like `disassemble' but also prints out the disassemble and the original words
  (define (display-word word [n 0])
    (when (< n 4)
      (let ((inst (vector-ref word n)))
        (when inst
          (display (format "~a " inst))
          (display-word word (add1 n))))))

  (define (dis-mem mem [i 0])
    (display (format "~a    " i))
    (let* ((word (vector-ref mem i))
           (dis (disassemble-word word)))
      (display (~a word #:min-width 6 #:align 'right #:left-pad-string " "))
      (display "    ")
      (display-word dis)
      (newline)
      (vector-set! (cdar nodes) i dis))
    (when (and (< i 62)
               (or all?
                   (vector-ref mem (add1 i))))
      (dis-mem mem (add1 i))))

  (define (dis nodes)
    (unless (null? nodes)
      (display (format "\nnode ~a\n" (caar nodes)))
      (dis-mem (cdar nodes))
      (dis (cdr nodes))))
  (dis nodes))
