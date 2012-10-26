#lang racket

(provide read-program)

;;; ; and . are ret and nop respectively so I can reuse the Racket lexer.
(define names '#(ret ex jump call unext next if -if @p @+ @b @ !p !+ !b ! +*
                    2* 2/ - + and or drop dup pop over a nop push b! a!))

;;; Returns the op-code corresponding to the given name. #<eof> is
;;; read as a nop. If the name is not valid, raises an error.
(define (to-opcode name)
  (if (eof-object? name) (to-opcode 'nop)
      (or (vector-member name names) (raise (format "~s is not a valid name!" name)))))

;;; The list of instructions that can go into slot 3.
(define slot-3 (map to-opcode '(ret +* unext + @p dup !p nop)))

;;; Reads a whole 18-bit word's worth of instructions. This can take
;;; up to four tokens from the list above. It can also take a single
;;; numeric constant. This constant can use any syntax Racket recognizes.
(define (read-18bit-word in)
  (define a (read in))
  (if (number? a) a
      (let ([b (read in)]
            [c (read in)]
            [d (read in)])
        (and
         (not (eof-object? a))
         (when (not (member (to-opcode d) slot-3)) (raise (format "~s cannot go in the last slot!" d)))
         (bitwise-ior (arithmetic-shift (to-opcode a) 13)
                      (arithmetic-shift (to-opcode b) 8)
                      (arithmetic-shift (to-opcode c) 3)
                      (bitwise-bit-field (to-opcode d) 2 5))))))
;;; Read a whole program in from the given port or string, stopping at
;;; eof. Returns a list of 18-bit words.
(define (read-program in)
  (when (string? in) (set! in (open-input-string in)))
  (define (go)
    (let ([next (read-18bit-word in)])
      (if next (cons next (go)) '())))
  (go))
