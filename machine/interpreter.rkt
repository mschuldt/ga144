;;; A bit-level arrayForth interpreter.
#lang racket

(require "assembler.rkt" "stack.rkt")

(provide (all-defined-out))

;;; A snapshot of the interpreter's state.
(struct state (a b p i r s t data return memory) #:transparent)

;;; Returns a snapshot of the current state.
(define (current-state)
  (state a b p i r s t data return memory))

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

;;; Reset the interpreter to the given state.
(define (load-state! state)
  (set! data   (copy-stack (state-data state)))
  (set! return (copy-stack (state-return state)))

  (set! a (state-a state))
  (set! b (state-b state))
  (set! p (state-p state))
  (set! i (state-i state))
  (set! r (state-r state))
  (set! s (state-s state))
  (set! t (state-t state))

  (set! memory (vector-copy (state-memory state))))

(define (set-state! new-data new-return new-a new-b new-p new-i new-r new-s new-t new-memory)
  (set! data   new-data)
  (set! return new-return)

  (set! a new-a)
  (set! b new-b)
  (set! p new-p)
  (set! i new-i)
  (set! r new-r)
  (set! s new-s)
  (set! t new-t)

  (set! memory new-memory))

(define start-state (state 0 0 0 0 0 0 0
                      (stack 0 (make-vector 8))
                      (stack 0 (make-vector 8))
                      (make-vector 64)))

(define (clone-state!)
  (state a b p i r s t (copy-stack data) (copy-stack return) (vector-copy memory 0 64)))

;;; Resets the state of the interpreter:
(define (reset!)
  (load-state! start-state))

;;; Resets only p
(define (reset-p! [start 0])
  (set! p start))

;;; Print the data stack:
(define (display-data)
  (display (format "|d> ~x ~x" t s))
  (display-stack data)
  (newline))

;;; Print the return stack:
(define (display-return)
  (display (format "|r> ~x" r))
  (display-stack return)
  (newline))

;;; Print the memory:
(define (display-memory n)
  (for ([i (in-range 0 n)])
    (display (format "~x " (vector-ref memory i))))
  (newline))

;;; Displays some state, useful for debugging. Currently this just
;;; shows the pc and data stack.
(define (display-state)
  (display (format "p:~a " p))
  (display-data))

;;; Loads the given program into memory at the given start
;;; address. The program is run through the assembler before being
;;; loaded.
(define (load-program in [start 0])
  (foldl (lambda (word pos) (vector-set! memory pos word) (add1 pos))
         start (read-program in)))

;;; Extracts the bottom 18 bits of n:
(define (18bit n)
  (bitwise-bit-field n 0 18))

;;; Pushes to the data stack.
(define (push! value)
  (push-stack! data s)
  (set! s t)
  (set! t (18bit value)))

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

(define memory (make-vector 64))

(define instructions (make-vector 32))

;;; Executes a single integer, treating it as an 18-bit word.
(define (execute-word!)
  (define (execute! opcode [jump-addr-pos 0])
    (let ([jump-addr (bitwise-bit-field i 0 jump-addr-pos)])
      ((vector-ref instructions opcode) jump-addr)))
  (and (execute! (bitwise-bit-field i 13 18) 10)
       (execute! (bitwise-bit-field i 8 13)  8)
       (execute! (bitwise-bit-field i 3 8)   3)
       (execute! (arithmetic-shift (bitwise-bit-field i 0 3) 2))))

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

;;; Executes one step of the program by fetching a word, incrementing
;;; p and executing the word.
(define (step-program! [debug? #f])
  (set! i (vector-ref memory p))
  (set! p (incr p))
  (execute-word!)
  (when debug? (display-state)))

;;; Steps the program n times.
(define (step-program-n! n [debug? #f])
  (for ([i (in-range 0 n)]) (step-program! debug?)))

;;; Steps the program until it hits an instructions made up only of
;;; nops or only of 0s. This should be useful for debugging small
;;; programs.
(define (step-program!* [debug? #f])
  (let ([next (vector-ref memory p)])
    (unless (or (= next #x39ce7) (= next 0))
      (step-program! debug?) (step-program!* debug?))))

;;; Defines a new instruction. This implicitly sets the instructions'
;;; opcodes based on the order they're defined in. An instruction can
;;; abort the rest of the current word by returning #f.
(define define-instruction!
  (let ([current-opcode 0])
    (lambda (body)
      (vector-set! instructions current-opcode body)
      (set! current-opcode (add1 current-opcode)))))

(define-instruction! (lambda (_) (set! p r) (r-pop!) #f))                            ; return (;)
(define-instruction! (lambda (_) (define temp p) (set! p r) (set! r temp) #f))       ; execute (ex)
(define-instruction! (lambda (a) (set! p a) #f))                                     ; jump (name ;)
(define-instruction! (lambda (a) (r-push! p) (set! p a) #f))                         ; call (name)
(define-instruction! (lambda (_) (if (= r 0) (r-pop!)                                ; micronext (unext) -- hacky!
                           (begin (set! r (sub1 r)) (set! p (sub1 p)) #f))))
(define-instruction! (lambda (a) (if (= r 0) (begin (r-pop!) #f)                     ; next (next)
                           (begin (set! r (sub1 r)) (set! p a) #f))))
(define-instruction! (lambda (a) (and (not (= t 0)) (set! p a) #f)))                 ; if (if)
(define-instruction! (lambda (a) (and (not (bitwise-bit-set? t 17)) (set! p a) #f))) ; minus if (-if)
(define-instruction! (lambda (_) (push! (vector-ref memory p)) (set! p (incr p))))   ; fetch-p (@p)
(define-instruction! (lambda (_) (push! (vector-ref memory a)) (set! a (incr a))))   ; fetch-plus (@+)
(define-instruction! (lambda (_) (push! (vector-ref memory b))))                     ; fetch-b (@b)
(define-instruction! (lambda (_) (push! (vector-ref memory a))))                     ; fetch (@)
(define-instruction! (lambda (_) (vector-set! memory p (pop!)) (set! p (incr p))))   ; store-p (!p)
(define-instruction! (lambda (_) (vector-set! memory a (pop!)) (set! a (incr a))))   ; store-plus (!+)
(define-instruction! (lambda (_) (vector-set! memory b (pop!))))                     ; store-b (!b)
(define-instruction! (lambda (_) (vector-set! memory a (pop!))))                     ; store (!)
(define-instruction! (lambda (_) (void)))                                            ; TODO: multiply-step
(define-instruction! (lambda (_) (set! t (18bit (arithmetic-shift t 1)))))           ; 2*
(define-instruction! (lambda (_) (set! t (arithmetic-shift t -1))))                  ; 2/
(define-instruction! (lambda (_) (set! t (18bit (bitwise-not t)))))                  ; not (-)
(define-instruction! (lambda (_) (push! (+ (pop!) (pop!)))))                         ; TODO: extended arithmetic mode
(define-instruction! (lambda (_) (push! (bitwise-and (pop!) (pop!)))))               ; and
(define-instruction! (lambda (_) (push! (bitwise-xor (pop!) (pop!)))))               ; or 
(define-instruction! (lambda (_) (pop!)))                                            ; drop 
(define-instruction! (lambda (_) (push! t)))                                         ; dup
(define-instruction! (lambda (_) (push! (r-pop!))))                                  ; pop
(define-instruction! (lambda (_) (push! s)))                                         ; over
(define-instruction! (lambda (_) (push! a)))                                         ; read a (a)
(define-instruction! (lambda (_) (void)))                                            ; nop (.)
(define-instruction! (lambda (_) (r-push! (pop!))))                                  ; push
(define-instruction! (lambda (_) (set! b (pop!))))                                   ; store into b (b!) 
(define-instruction! (lambda (_) (set! a (pop!))))                                   ; store into a (a!)
