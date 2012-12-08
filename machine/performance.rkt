#lang racket

(require "cegis.rkt" "state.rkt")

(define-syntax-rule (time actions ...)
  (apply average
         (for/list ([i (in-range 1 10)])
                   (let ([start (current-seconds)])
                     actions ...
                     (- (current-seconds) start)))))

(define (average . args)
  (/ (apply + args) (length args)))

;;; Test how the number of bits affects synthesis time.
(define (bits)
  (for/list ([bits (in-range 2 19)])
            (pretty-display bits)
            (cons bits
                  (time (fastest-program "over over or nop a! and a nop or nop nop nop"
                                         #:slots 8 #:constraint (constraint t)
                                         #:num-bits bits #:inst-pool `no-fake)))))

(define (instr-pools)
  (for/list ([instrs '(all no-fake no-mem no-mem-no-p no-fake-no-p)])
            (pretty-display instrs)
            (cons instrs
                  (time (fastest-program "over over or nop a! and a nop or nop nop nop"
                                         #:slots 8 #:constraint (constraint t)
                                         #:inst-pool instrs)))))

(define (constraints)
  (for/list ([constraints (list constraint-all (constraint t) (constraint s) (constraint s t))])
            (pretty-display constraints)
            (cons constraints
                  (time (fastest-program "over over or nop a! and a nop or nop nop nop"
                                         #:slots 8 #:constraint constraints
                                         #:inst-pool `no-fake)))))
