#lang racket ;; -*- lexical-binding: t -*-

(require  "common.rkt"
          "el.rkt")

(when elisp? (_def '(assemble-word assemble)))

(provide assemble-word assemble)

(defconst const-masks (vector #x3ffff #x3ff #xff #x7))
(defconst xor-bits (vector #b1010 #b10101 #b1010 #b101))
(define (xor-inst inst slot) (bitwise-xor inst (vector-ref xor-bits slot)))

(define (assemble-inst word slot shift)
  ;;Assemble the instruction from WORD in SLOT, SHIFTed to its proper location
  ;;If the given slot contains an address, it is returned unchanged
  ;;If the slot contains false, as unused slots do, return 0
  (let ((inst (vector-ref word slot)))
    (if (string? inst)
        (begin (unless (vector-member inst opcodes)
                 (printf "attempt to assemble invalid opcode: '~a'\n" inst)
                 (exit 1))
               (arithmetic-shift (xor-inst (floor (/ (vector-member inst opcodes)
                                                     (if (= slot 3) 4 1)))
                                           slot)
                                 shift))
        ;;slot contains an address, number, or is unused
        (or (and inst (& (vector-ref const-masks slot) inst)) 0))))

(define (assemble-word word)
  (cond ((number? word)
         (& word #x3ffff))
        ;;in the assembled memory vector, false represents unused words
        ((or (equal? word (vector false false false false))
             (not word))
         #x134a9) ;; call warm
        (else (let* ((d (assemble-inst word 3 0))
                     (c (assemble-inst word 2 3))
                     (b (assemble-inst word 1 8))
                     (a (assemble-inst word 0 13)))
                (bitwise-ior a b c d)))))

(define (assemble compiled)
  ;;COMPILED is a struct of type 'compiled'
  ;;This function mutates the node structs, assembling the words in place
  (define nodes (compiled-nodes compiled))
  (define node false)
  (define mem false)
  (define mem-length false)
  (define used-words false)
  (while (not (null? nodes))
    (begin
      (set! node (car nodes))
      (set! nodes (cdr nodes))
      (set! mem (node-mem node))
      (set! mem-length (vector-length mem))
      (set! used-words (get-compiled-size mem))
      (for ((i used-words))
        (vector-set! mem i (assemble-word (vector-ref mem i))))
      (while (< used-words mem-length)
        (begin (vector-set! mem used-words false)
               (set! used-words (add1 used-words))))
      ))
  compiled)

(define (get-compiled-size mem)
  (let ((meml (reverse (vector->list mem)))
        (i 0))
    (while (and (not (null meml))
                (or (not (car meml))
                    (equal? (car meml) (vector false false false false))))
      (begin (set! i (add1 i))
             (set! meml (cdr meml))))
    (- (vector-length mem) i)))
