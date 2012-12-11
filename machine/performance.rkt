#lang racket

(require "cegis.rkt" "state.rkt" "programs.rkt" "interpreter.rkt")
(provide bits instr-pools constraints optimizer)

(define state (random-state (expt 2 BIT)))

(define-syntax-rule (time actions ...)
  (apply average
         (for/list ([i (in-range 0 5)])
                   (let ([start (current-seconds)])
                     actions ...
                     (- (current-seconds) start)))))

(define (average . args)
  (exact->inexact (/ (apply + args) (length args))))

;;; Test how the number of bits affects synthesis time.
(define (bits program #:constraint [constraint constraint-all] 
	      #:inst-pool [inst-pool `no-fake] 
	      #:slots [slots (program-length-abs program)])
  (pretty-display ">>> BIT TEST >>>") 
  (pretty-display program)
  (newline)
  (perf-mode)
  (for/list ([bits (in-range 2 19)])
	    (define t (time (fastest-program program #:num-bits bits 
					     #:constraint constraint 
					     #:inst-pool inst-pool 
					     #:slots slots
					     #:start-state state)))
            (pretty-display (format "~a\t~a" bits t))
            (cons bits t)))

(define (instr-pools program #:pool [pool '(all no-fake no-mem no-mem-no-p no-fake-no-p)]  
		     #:slots [slots (program-length-abs program)])
  (pretty-display ">>> INSTR-POOL TEST >>>")
  (pretty-display program)
  (newline)
  (perf-mode)
  (for/list ([instrs pool])
	    (define t (time (fastest-program program #:inst-pool instrs #:slots slots
					     #:start-state state)))
            (pretty-display (format "~a\t~a" instrs t))
            (cons instrs t)))

(define (constraints program #:num-bits [num-bits 18] #:inst-pool [inst-pool `no-fake] 
		     #:slots [slots (program-length-abs program)])
  (pretty-display ">>> CONSTRAINT TEST >>>")
  (pretty-display program)
  (newline)
  (perf-mode)
  (for/list ([constraints (list constraint-all (constraint t) (constraint s) (constraint s t))])
	    (define t (time (fastest-program program #:constraint constraints 
					     #:num-bits num-bits 
					     #:inst-pool inst-pool
					     #:slots slots
					     #:start-state state)))
            (pretty-display (format "~a\t~a" constraints t))
            (cons constraints t)))

(define (optimizer program)
  (pretty-display ">>> OPTIMIZER TEST >>>")
  (pretty-display program)
  (newline)
  (perf-mode)
  (for/list ([opt (list fastest-program3 fastest-program)])
	    (define t (time (opt program #:start-state state)))
            (pretty-display (format "~a\t~a" opt t))
            (cons opt t)))
