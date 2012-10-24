#lang racket

(provide read-18bit-word)

;;; ; and . are ret and nop respectively so I can reuse the Racket lexer.
(define names '#(ret ex jump call unext next if -if @p @+ @b @ !p !+ !b ! +*
                    2* 2/ - + and or drop dup pop over a nop push b! a!))

;;; The list of instructions that can go into slot 3.
(define slot-3 (map to-opcode '(ret +* unext + @p dup !p nop)))

;;; Returns the op-code corresponding to the given name. If #<eof> is
;;; passed in, returns 0. If the name is not valid, raises an error.
(define (to-opcode name)
  (if (eof-object? name) 0
      (or (vector-member name names) (raise (format "~s is not a valid name!" name)))))

;;; Reads a whole 18-bit word's worth of instructions.
(define (read-18bit-word in)
  (let ([a (read in)]
        [b (read in)]
        [c (read in)]
        [d (read in)])
    (when (not (member (to-opcode d) slot-3)) (raise (format "~s cannot go in the last slot!" d)))
    (bitwise-ior (arithmetic-shift (to-opcode a) 13)
                 (arithmetic-shift (to-opcode b) 8)
                 (arithmetic-shift (to-opcode c) 3)
                 (bitwise-and #x7 (to-opcode d)))))
