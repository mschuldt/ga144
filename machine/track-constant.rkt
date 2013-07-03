;;; A bit-level arrayForth interpreter.
#lang racket

(require "assembler.rkt" "stack.rkt" "state.rkt" "programs.rkt")

(provide (all-defined-out))

;;; stacks:
(define data   (stack 0 (make-vector 8)))
(define return (stack 0 (make-vector 8)))

;;; registers:
(define a 0)
(define b 0)
(define p 0)
(define i 0)
(define r 0)
(define s 0)
(define t 0)

(define memory 0)

(define instructions (make-vector 35))

(define BIT 18)

;;; Reset the interpreter to the given state.
(define (load-state! state)
  (set! a (progstate-a state))
  (set! b (progstate-b state))
  (set! p (progstate-p state))
  (set! i (progstate-i state))
  (set! r (progstate-r state))
  (set! s (progstate-s state))
  (set! t (progstate-t state))

  (set! data   (copy-stack (progstate-data state)))
  (set! return (copy-stack (progstate-return state)))

  (set! memory (progstate-memory state))

;;; Sets the current state to the given values for the registers,
;;; stacks and memory.
(define set-state! (lambda args (load-state! (apply progstate args))))

;;; Returns a snapshot of the current state.
(define (current-state)
  (progstate a b p i r s t (copy-stack data) (copy-stack return) memory))

;;; Print the data stack:
(define (display-data [state (current-state)])
  (display (format "|d> ~x ~x" (progstate-t state) (progstate-s state)))
  (display-stack (progstate-data state))
  (newline))

;;; Print the return stack:
(define (display-return)
  (display (format "|r> ~x" r))
  (display-stack return)
  (newline))

;;; Displays some state, useful for debugging. Currently this just
;;; shows the pc and data stack.
(define (display-state [state (current-state)])
  (pretty-display (format "p:~a a:~a b:~a r:~a"
                          (progstate-p state) (progstate-a state)
                          (progstate-b state) (progstate-r state)))
  (display-data state))

(define (display-vector vec n name)
  (when (> n 0)
	(display name)
	(for ([i (in-range 0 n)])
	     (display (format "~x " (vector-ref vec i))))
	(newline)))

;;; Pushes to the data stack.
(define (push! value)
  (push-stack! data s)
  (set! s t)
  (set! t (value)))

;;; Pushes to the return stack.
(define (r-push! value)
  (push-stack! return r)
  (set! r value))

;;; Pops from the data stack.
(define (pop!)
  (let ([ret-val t])
    (set! t s)
    (set! s (pop-stack! data))
    ret-val))

;;; Pops from the return stack.
(define (r-pop!)
  (let ([ret-val r])
    (set! r (pop-stack! return))
    ret-val))

;;; Return the value of p or a incremented as appropriately. If the
;;; register points to an IO region, does nothing. Otherwise increment
;;; the register circularly within the current memory region (RAM or
;;; ROM).
(define (incr curr)
  (cond [(< curr #x07F) (add1 curr)]
        [(= curr #x07F) #x000]
        [(< curr #x0FF) (add1 curr)]
        [(= curr #x0FF) #x080]
        [else curr]))

(define index-set (set))

;;; Read from the given memory address or communication port. If it
;;; gets a communication port, it just returns a random number (for
;;; now).
(define (read-memory addr)
  (when (set? addr)
	(set! index-set (set-union index-set addr)))
  memory)
      

;;; Read from the given memory address or communication port. If it
;;; gets a communication port, it just returns a random number (for
;;; now).
(define (read-memory-@p addr)
  (vector-ref memory addr))

;;; Write to the given memeory address or communication
;;; port. Everything written to any communication port is simply
;;; aggregated into a list.
(define (set-memory! addr value)
  (when (set? addr)
	(set! index-set (set-union index-set addr)))

  (when (set? value)
	(set! memory (if (set? memory)
			 (set-union memory value)
			 value))))


(define (track-constant program)
  (for ([inst (string-split program)])
       (cond
	[(or (equal? inst "@+")
	     (equal? inst "@"))
	 (push! (read-memory a))]

	[(or (equal? inst "!+")
	     (equal? inst "!"))
	 (set-memory! a (pop!))])))
       
