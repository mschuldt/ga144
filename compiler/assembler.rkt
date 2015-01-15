#lang racket

(provide read-program)

;; ; and . are ret and nop respectively so I can reuse the Racket lexer.
(define names '#(ret ex jump call unext next if -if @p @+ @b @ !p !+ !b ! +*
                     2* 2/ - + and or drop dup pop over a nop push b! a!))

;; Returns the op-code corresponding to the given name. #<eof> is
;; read as a nop. If the name is not valid, raises an error.
(define (to-opcode name)
  (if (eof-object? name) (to-opcode 'nop)
      (or (vector-member name names)
          (raise (format "~s is not a valid name!" name)))))

;; The list of instructions that can go into slot 3.
(define slot-3 '(ret +* unext + @p dup !p nop))

;; Instructions taking an address:
(define jumps '(jump call next if -if))

;; Reads a whole 18-bit word's worth of instructions. This can take
;; up to four tokens from the list above. It can also take a single
;; numeric constant. Instructions expecting an address--like jump and
;; if--have to be followed by a numeric constant which finished off
;; the word.
(define (read-18bit-word in)
  (define (pack size instr . rest)
    (if (null? rest)
        (begin
          (when (not (number? instr))
            (raise (format "~s is not a valid jump address!" instr)))
          (bitwise-bit-field instr 0 (+ size 5)))
        (bitwise-ior (arithmetic-shift (to-opcode instr) size)
                     (apply pack (cons (- size 5) rest)))))
  (let/cc end
          (define a (read in))
          (cond [(eof-object? a)  (end #f)]
                [(number? a)      (end a)]
                [(member a jumps) (end (pack 13 a (read in)))])
          (define b (read in))
          (when (member b jumps) (end (pack 13 a b (read in))))
          (define c (read in))
          (when (member c jumps) (end (pack 13 a b c (read in))))
          (define d (read in))
          (when (not (member d slot-3))
            (raise (format "~s cannot go in the last slot!" d)))
          (end (pack 13 a b c (bitwise-bit-field (to-opcode d) 2 5)))))

;; Read a whole program in from the given port or string, stopping at
;; eof. Returns a list of 18-bit words.
(define (read-program in)
  (when (string? in) (set! in (open-input-string in)))
  (define (go)
    (let ([next (read-18bit-word in)])
      (if next (cons next (go)) '())))
  (go))
