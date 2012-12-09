#lang racket

(require "cegis.rkt" "state.rkt")
(provide bits instr-pools constraints optimizer)

(define-syntax-rule (time actions ...)
  (apply average
         (for/list ([i (in-range 0 5)])
                   (let ([start (current-seconds)])
                     actions ...
                     (- (current-seconds) start)))))

(define (average . args)
  (/ (apply + args) (length args)))

;;; Test how the number of bits affects synthesis time.
(define (bits program)
  (newline) (pretty-display ">>> BIT TEST >>>") (newline)
  (perf-mode)
  (for/list ([bits (in-range 2 19)])
            (pretty-display bits)
            (cons bits
                  (time (fastest-program program #:num-bits bits)))))

(define (instr-pools program)
  (newline) (pretty-display ">>> INSTR-POOL TEST >>>") (newline)
  (perf-mode)
  (for/list ([instrs '(all no-fake no-mem no-mem-no-p no-fake-no-p)])
            (pretty-display instrs)
            (cons instrs
                  (time (fastest-program program #:inst-pool instrs #:num-bits 4)))))

(define (constraints program)
  (newline) (pretty-display ">>> CONSTRAINT TEST >>>") (newline)
  (perf-mode)
  (for/list ([constraints (list constraint-all (constraint t) (constraint s) (constraint s t))])
            (pretty-display constraints)
            (cons constraints
                  (time (fastest-program program #:constraint constraints #:num-bits 4)))))

(define (optimizer program)
  (newline) (pretty-display ">>> OPTIMIZER TEST >>>") (newline)
  (perf-mode)
  (for/list ([opt (list fastest-program fastest-program3)])
            (pretty-display opt)
            (cons opt
                  (time (opt program #:num-bits 4)))))
