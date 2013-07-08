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

(define memory (make-vector MEM-SIZE))

(define send-u '())
(define send-d '())
(define send-l '())
(define send-r '())
(define send-io '())

(define recv-u '())
(define recv-d '())
(define recv-l '())
(define recv-r '())
(define recv-io '())

(define order-u '())
(define order-d '())
(define order-l '())
(define order-r '())
(define order-io '())

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

  (set! memory (vector-copy (progstate-memory state))))

;;; Sets the current state to the given values for the registers,
;;; stacks and memory.
(define set-state! (lambda args (load-state! (apply progstate args))))

;;; Returns a snapshot of the current state.
(define (current-state)
  (progstate a b p i r s t (copy-stack data) (copy-stack return) (vector-copy memory 0 MEM-SIZE)))

;;; Returns the current commstate.
(define (current-commstate)
  (let* ([messages (list send-u send-d send-l send-r send-io
                         recv-u recv-d recv-l recv-r recv-io)]
         [vectors (map (compose list->vector reverse (curry cons 0)) messages)]
         [lengths (map length messages)]
	 [orders  (list order-u order-d order-l order-r order-io)]
	 [order-vecs (map (compose list->vector reverse (curry cons 0)) orders)])
    (apply commstate (append vectors lengths order-vecs))))

;;; Resets the state of the interpreter:
(define (reset! [bit 18])
  (set! BIT bit)

  (set! send-u '())
  (set! send-d '())
  (set! send-l '())
  (set! send-r '())
  (set! send-io '())
  (set! recv-u '())
  (set! recv-d '())
  (set! recv-l '())
  (set! recv-r '())
  (set! recv-io '())
  (set! order-u '())
  (set! order-d '())
  (set! order-l '())
  (set! order-r '())
  (set! order-io '())

  (load-state! start-state))

;;; Resets only p
(define (reset-p! [start 0])
  (set! p start))

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

;;; Print the memory:
(define (display-memory n)
  (for ([i (in-range 0 n)])
    (display (format "~x " (vector-ref memory i))))
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

(define (display-comm)
  (define comm (current-commstate))
  (display-vector (commstate-send-u comm) (commstate-sendp-u comm) "send-u: ")
  (display-vector (commstate-send-d comm) (commstate-sendp-d comm) "send-d: ")
  (display-vector (commstate-send-l comm) (commstate-sendp-l comm) "send-l: ")
  (display-vector (commstate-send-r comm) (commstate-sendp-r comm) "send-r: ")
  (display-vector (commstate-recv-u comm) (commstate-recvp-u comm) "recv-u: ")
  (display-vector (commstate-recv-d comm) (commstate-recvp-d comm) "recv-d: ")
  (display-vector (commstate-recv-l comm) (commstate-recvp-l comm) "recv-l: ")
  (display-vector (commstate-recv-r comm) (commstate-recvp-r comm) "recv-r: "))

;;; Loads the given program into memory at the given start
;;; address. The program is run through the assembler before being
;;; loaded.
(define (load-program in [start 0])
  (foldl (lambda (word pos) (vector-set! memory pos word) (add1 pos))
         start (read-program in)))

;;; Extracts the bottom 18 bits of n:
(define (18bit n)
  (bitwise-bit-field n 0 BIT))

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
  (when debug? (display-state))
  (execute-word!)
  )

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

;;; Read from the given memory address or communication port. If it
;;; gets a communication port, it just returns a random number (for
;;; now).
(define (read-memory addr)
  (if (member addr (list UP DOWN LEFT RIGHT IO))
      (let ([value 12]);(random (arithmetic-shift 1 BIT))])
        (cond [(= addr UP)    (set! recv-u (cons value recv-u))
	                      (set! order-u (cons 0 order-u))]
              [(= addr DOWN)  (set! recv-d (cons value recv-d))
	                      (set! order-d (cons 0 order-d))]
              [(= addr LEFT)  (set! recv-l (cons value recv-l))
	                      (set! order-l (cons 0 order-l))]
              [(= addr RIGHT) (set! recv-r (cons value recv-r))
	                      (set! order-r (cons 0 order-r))]
              [(= addr IO)    (set! recv-io (cons value recv-io))
	                      (set! order-io (cons 0 order-io))])
        value)
      (vector-ref memory addr)))

;;; Read from the given memory address or communication port. If it
;;; gets a communication port, it just returns a random number (for
;;; now).
(define (read-memory-@p addr)
  (vector-ref memory addr))

;;; Write to the given memeory address or communication
;;; port. Everything written to any communication port is simply
;;; aggregated into a list.
(define (set-memory! addr value)
  (cond [(= addr UP)    (set! send-u (cons value send-u))
	                (set! order-u (cons 1 order-u))]
        [(= addr DOWN)  (set! send-d (cons value send-d))
	                (set! order-d (cons 1 order-d))]
        [(= addr LEFT)  (set! send-l (cons value send-l))
	                (set! order-l (cons 1 order-l))]
        [(= addr RIGHT) (set! send-r (cons value send-r))
	                (set! order-r (cons 1 order-r))]
        [(= addr IO)    (set! send-io (cons value send-io))
	                (set! order-io (cons 1 order-io))]
        [else           (vector-set! memory addr value)]))

(define-instruction! (lambda (_) (set! p r) (r-pop!) #f))                            ; return (;)
(define-instruction! (lambda (_) (define temp p) (set! p r) (set! r temp) #f))       ; execute (ex)
(define-instruction! (lambda (a) (set! p a) #f))                                     ; jump (name ;)
(define-instruction! (lambda (a) (r-push! p) (set! p a) #f))                         ; call (name)
(define-instruction! (lambda (_) (if (= r 0) (r-pop!)                                ; micronext (unext) -- hacky!
                           (begin (set! r (sub1 r)) (set! p (sub1 p)) #f))))
(define-instruction! (lambda (a) (if (= r 0) (begin (r-pop!) #f)                     ; next (next)
                           (begin (set! r (sub1 r)) (set! p a) #f))))
(define-instruction! (lambda (a) (and (not (= t 0)) (set! p a) #f)))                 ; if (if)
(define-instruction! (lambda (a) (and (not (bitwise-bit-set? t (sub1 BIT))) (set! p a) #f))) ; minus if (-if)
(define-instruction! (lambda (_) (push! (read-memory-@p p)) (set! p (incr p))))   ; fetch-p (@p) TODO: this is a HACK!!!
(define-instruction! (lambda (_) (push! (read-memory a)) (set! a (incr a))))   ; fetch-plus (@+)
(define-instruction! (lambda (_) (push! (read-memory b))))                     ; fetch-b (@b)
(define-instruction! (lambda (_) (push! (read-memory a))))                     ; fetch (@)
(define-instruction! (lambda (_) (set-memory! p (pop!)) (set! p (incr p))))   ; store-p (!p)
(define-instruction! (lambda (_) (set-memory! a (pop!)) (set! a (incr a))))   ; store-plus (!+)
(define-instruction! (lambda (_) (set-memory! b (pop!))))                     ; store-b (!b)
(define-instruction! (lambda (_) (set-memory! a (pop!))))                     ; store (!)
(define-instruction! (lambda (_) (if (even? a)                                       ; multiply-step (+*)
                                     (multiply-step-even!)
                                     (multiply-step-odd!)))) 
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

;;; Fake instructions


;;; Treats T:A as a single 36 bit register and shifts it right by one
;;; bit. The most signficicant bit (T17) is kept the same.
(define (multiply-step-even!)
  (let ([t17 (bitwise-and t #x20000)]
        [t0  (bitwise-and t #x1)])
    (set! t (bitwise-ior t17 (arithmetic-shift t -1)))
    (set! a (bitwise-ior (arithmetic-shift t0 (sub1 BIT)) (arithmetic-shift a -1)))))

;;; Sums T and S and concatenates the result with A, shifting
;;; everything to the right by one bit.
(define (multiply-step-odd!)
  (let* ([sum (+ t s)]
	 [sum17 (bitwise-and sum #x20000)]
         [result (bitwise-ior (arithmetic-shift sum (sub1 BIT)) (arithmetic-shift a -1))])
    (set! a (bitwise-bit-field result 0 BIT))
    (set! t (bitwise-ior sum17 (bitwise-bit-field result BIT (* 2 BIT))))))
