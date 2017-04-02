#lang racket ;; -*- lexical-binding: t -*-

(require "common.rkt"
         "el.rkt")

(when elisp? (_def '(disassemble-nodes display-disassemble disassemble-word)))

(provide disassemble-nodes
         display-disassemble
         disassemble-word)

(define (disassemble-inst from from^ to index start end jump)
  ;;FROM is the integer we are disassembling
  ;;FROM^ = FROM ^ 0x15555
  ;;TO is a vector we disassemble into
  ;;START, END mark the bit positions of the instruction in FROM
  ;;JUMP is the size of the jump field in FROM, false if none
  (let ((inst (vector-ref opcodes (* (bitwise-bit-field from^ start end)
                                     (if jump 1 4)))))
    (vector-set! to index inst)
    (if (and (member inst address-required)
             (< index 3))
        (begin (vector-set! to (add1 index) (bitwise-bit-field from 0 jump))
               false)
        (not (member inst instructions-using-rest-of-word)))))

(define (disassemble-word word)
  (let ((to (make-vector 4 false))
        (word^ (and word (bitwise-xor word #x15555))))
    (and word
         (disassemble-inst word word^ to 0 13 18 10)
         (disassemble-inst word word^ to 1 8 13 8)
         (disassemble-inst word word^ to 2 3 8 3)
         (disassemble-inst word word^ to 3 0 3 false))
    to))

(define (disassemble-nodes nodes)
  ;;NODES is a list of 'node' structs
  ;;mutates the structs 'mem' field in place
  (for ((node nodes))
    (define mem (node-mem node))
    (for ((i (node-len node)))
      (vector-set! mem i (disassemble-word (vector-ref mem i))))))

(define (display-disassemble compiled (all? false))
  ;;like `disassemble' but also prints out the disassemble and the original words
  (assert (not elisp?))
  (define nodes (compiled-nodes compiled))

  (define (display-word word (n 0))
    (let ((inst false))
      (for ((n 4))
        (set! inst (vector-ref word n))
        (when inst
          (printf (rkt-format "~a " inst))))))

  (define (dis-mem mem)
    (define i 0)
    (while (and (< i 64)
                (or all?
                    (vector-ref mem i)))
      (begin
        (printf (rkt-format "~a    " i))
        (define word (vector-ref mem i))
        (define dis (disassemble-word word))
        (printf (~a word #:min-width 6 #:align 'right #:left-pad-string " "))
        (printf "    ")
        (display-word dis)
        (newline)
        (set! i (add1 i)))))

  (for ((node nodes))
    (printf (rkt-format "\nnode ~a\n" (node-coord node)))
    (dis-mem (node-mem node))))
