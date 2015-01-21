;; A bit-level arrayForth interpreter.
#lang racket

(require compatibility/defmacro
         "../compiler/assembler.rkt"
         "../compiler/compile.rkt"
         "state.rkt"
         "stack.rkt")

(provide (all-defined-out))

(define UP #x145) ;325
(define DOWN #x115) ;277
(define LEFT #x175) ;373
(define RIGHT #x1d5) ;469
(define IO #x15d)

(define opcodes (vector "ret" "ex" "jump" "call" "unext" "next" "if"
                        "-if" "@p" "@+" "@b" "@" "!p" "!+" "!b" "!" "+*"
                        "2*" "2/" "-" "+" "and" "or" "drop" "dup" "pop"
                        "over" "a" "nop" "push" "b!" "a!"))

(define time 0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 8x18 node matrix

(define nodes (make-vector 144))
(define active-nodes '())

;;builds matrix of 144 f18 nodes
(define (build-node-matrix)
  (for ([i 144])
    (let* ([node (make-node i)]
           [coord (node:get-coord node)])
      (vector-set! nodes i node)
      (node:set-ludr-ports node (list (get-create-port (port-id coord 'left))
                                      (get-create-port (port-id coord 'up))
                                      (get-create-port (port-id coord 'down))
                                      (get-create-port (port-id coord 'right))))
      )))

(define (coord->index n)
  (+ (* (quotient n 100) 18) (remainder n 100)))

(define (index->coord n)
  (+ (* (quotient n 18) 100) (remainder n 18)))

(define (coord->node coord)
  (vector-ref nodes (coord->index coord)))

(define (index->node index)
  (vector-ref nodes index))

(define (coord->row coord)
  (quotient coord 100))

(define (coord->col coord)
  (remainder coord 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ludr communication ports
(define ports (make-hash))

(define (connecting-port-id coord1 coord2)
  (+ (* (min coord1 coord2) 1000) (max coord1 coord2)))

(define (port-id coord direction)
  (cond [(eq? direction 'up)    (connecting-port-id coord (+ coord 100))]
        [(eq? direction 'down)  (connecting-port-id coord (- coord 100))]
        [(eq? direction 'left)  (connecting-port-id coord (- coord 1))]
        [(eq? direction 'right) (connecting-port-id coord (+ coord 1))]
        [else (raise "Err: invalid direction")]))

(define (get-create-port port-id)
  (if (hash-has-key? ports port-id)
      (hash-ref ports port-id)
      (let ([port (make-port)])
        (hash-set! ports port-id port)
        port)))

(define (make-port)
  (vector #f))

(define (port-read port)
  (lambda () (vector-ref port 0)))

(define (port-write port value)
  (vector-set! port 0 value)
  (lambda () (vector-ref port 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; program loading

;;compiles, assembles, and loads code into nodes
(define (compile-and-load in [include-end-token? #f])
  (let ([n 0]
        [code 0]
        [node 0])
    (for ([code (assemble-all (compile-string in))])
      (set! node (coord->node (car code)))
      (node:load-code node (cdr code) include-end-token?)
      (set! active-nodes (cons node active-nodes)))))

(define (load-file file)
  (call-with-input-file file compile-and-load))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node object

(define (make-node index)
  (define self (make-vector N-METHODS))

  (define coord (index->coord index))

  ;; stacks:
  (define dstack (make-stack 8))
  (define rstack (make-stack 8))

  ;; registers:
  (define A 0)
  (define B 0)
  (define P 0)
  (define I -1)
  (define R 0)
  (define S 0)
  (define T 0)

  (define memory-size MEM-SIZE)
  (define memory (make-vector MEM-SIZE))
  (define memory-wrap #f)

  (define port-left #f)
  (define port-up #f)
  (define port-down #f)
  (define port-right #f)

  (define blocking-read #f)
  (define blocking-write #f)
  (define blocking #f)

  (define instructions (make-vector 35))
  (define BIT 18)

  ;; Extracts the bottom 18 bits of n:
  (define (18bit n)
    (bitwise-bit-field n 0 BIT))

  ;; Pushes to the data stack.
  (define (d-push! value)
    (push-stack! dstack S)
    (set! S T)
    (set! T (18bit value)))

  ;; Pushes to the rstack stack.
  (define (r-push! value)
    (push-stack! rstack R)
    (set! R value))

  ;; Pops from the data stack.
  (define (pop!)
    (let ([ret-val T])
      (set! T S)
      (set! S (pop-stack! dstack))
      ret-val))

  ;; Pops from the rstack stack.
  (define (r-pop!)
    (let ([ret-val R])
      (set! R (pop-stack! rstack))
      ret-val))

  ;; Executes a single integer, treating it as an 18-bit word.
  (define (execute-word!)
    (define (execute! opcode [jump-addr-pos 0])
      (if (< opcode 8)
          ((vector-ref instructions opcode) (bitwise-bit-field I 0 jump-addr-pos))
          ((vector-ref instructions opcode))))
    (and (execute! (bitwise-bit-field I 13 18) 10)
         (execute! (bitwise-bit-field I 8 13)  8)
         (execute! (bitwise-bit-field I 3 8)   3)
         (execute! (arithmetic-shift (bitwise-bit-field I 0 3) 2))))

  ;; Return the value of p or a incremented as appropriately. If the
  ;; register points to an IO region, does nothing. Otherwise increment
  ;; the register circularly within the current memory region (RAM or
  ;; ROM).
  (define (incr curr)
    (cond [(< curr #x07F) (add1 curr)]
          [(= curr #x07F) #x000]
          [(< curr #x0FF) (add1 curr)]
          [(= curr #x0FF) #x080]
          [else curr]))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; memory accesses

  ;; Read from the given memory address or communication port. If it
  ;; gets a communication port, it just returns a random number (for
  ;; now).
  (define (read-memory addr)
    (if (member addr (list UP DOWN LEFT RIGHT IO))
        (let* ((fn (cond [(= addr LEFT)  (port-read port-left)]
                         [(= addr UP)    (port-read port-up)]
                         [(= addr DOWN)  (port-read port-down)]
                         [(= addr RIGHT) (port-read port-right)]
                         ;;TODO:
                         [(= addr IO) (raise "unimplemented: reading from IO")]))
               (value (fn)))
          (if value
              value
              (begin (set! blocking-read fn)
                     (set! blocking #t))))
        (vector-ref memory (if memory-wrap
                               (modulo addr memory-size)
                               addr))))

  ;; Read from the given memory address or communication port. If it
  ;; gets a communication port, it just returns a random number (for
  ;; now).
  (define (read-memory-@p addr)
    (vector-ref memory addr))

  ;; Write to the given memeory address or communication
  ;; port. Everything written to any communication port is simply
  ;; aggregated into a list.
  (define (set-memory! addr value)
    (if (member addr (list UP DOWN LEFT RIGHT IO))
        (begin (set! blocking-write
                     (cond [(= addr LEFT)  (port-write port-left value)]
                           [(= addr UP)    (port-write port-up value)]
                           [(= addr DOWN)  (port-write port-down value)]
                           [(= addr RIGHT) (port-write port-right value)]
                           ;;TODO:
                           [(= addr IO) (raise "unimplemented: writing to IO")]))
               (when blocking-write
                 (set! blocking #t)))
        (vector-set! memory
                     (if memory-wrap
                         (modulo addr memory-size)
                         addr)
                     value)))

  ;;attempts to complete a read, returns #t if read is completed
  ;;and #f if still blocking
  (define (complete-read)
    (if blocking-read
        (let ((val (blocking-read)))
          (if val
              (begin (d-push! val)
                     (set! blocking-read #f)
                     (set! blocking #f)
                     #t)
              #f))
        #t))

  ;;attempts to complete a write, returns #t if read is completed
  ;;and #f if still blocking
  (define (complete-write)
    (if blocking-write
        (if (blocking-write)
            #f
            (begin (set! blocking-write #f)
                   (set! blocking #f)
                   #t))
        #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; instructions

  ;; Define a new instruction. An instruction can
  ;; abort the rest of the current word by returning #f.
  (define-syntax-rule (define-instruction! opcode args body ...)
    (let ((n (or (vector-member opcode opcodes)
                 (raise (format "Err: invalid opcode: '~s'" opcode)))))
      (vector-set! instructions n (lambda args body ...))))

  (define-instruction! "ret" (_)
    (set! P R)
    (r-pop!)
    #f)

  (define-instruction! "ex" (_)
    (define temp P)
    (set! P R)
    (set! R temp)
    #f)

  (define-instruction! "jump" (addr)
    (set! P addr)
    #f)

  (define-instruction! "call" (addr)
    (r-push! P)
    (set! P addr)
    #f)

  (define-instruction! "unext" (_) ;; -- hacky!
    (if (= R 0)
        (r-pop!)
        (begin (set! R (sub1 R))
               (set! P (sub1 P))
               #f)))

  (define-instruction! "next" (addr)
    (if (= R 0)
        (begin (r-pop!)
               #f)
        (begin (set! R (sub1 R))
               (set! P addr)
               #f)))

  (define-instruction! "if" (addr)
    (and (not (= T 0))
         (set! P addr)
         #f))

  (define-instruction! "-if" (addr)
    (and (not (bitwise-bit-set? T (sub1 BIT)))
         (set! P addr)
         #f))

  (define-instruction! "@p" () ;;TODO: this is a HACK!!!
    (d-push! (read-memory-@p P))
    (set! P (incr P)))

  (define-instruction! "@+" () ; fetch-plus
    (d-push! (read-memory A))
    (set! A (incr A)))

  (define-instruction! "@b" () ;fetch-b
    (d-push! (read-memory B)))

  (define-instruction! "@" (); fetch a
    (d-push! (read-memory A)))

  (define-instruction! "!p" () ; store p
    (set-memory! P (pop!))
    (set! P (incr P)))

  (define-instruction! "!+" () ;store plus
    (set-memory! A (pop!))
    (set! A (incr A)))

  (define-instruction! "!b" (); store-b
    (set-memory! B (pop!)))

  (define-instruction! "!" (); store
    (set-memory! A (pop!)))

  (define-instruction! "+*" () ; multiply-step
    (if (even? A)
        (multiply-step-even!)
        (multiply-step-odd!)))

  (define-instruction! "2*" ()
    (set! T (18bit (arithmetic-shift T 1))))

  (define-instruction! "2/" ()
    (set! T (arithmetic-shift T -1)))

  (define-instruction! "-" () ;not
    (set! T (18bit (bitwise-not T))))

  (define-instruction! "+" () ;;TODO: extended arithmetic mode
    (d-push! (+ (pop!) (pop!))))

  (define-instruction! "and" ()
    (d-push! (bitwise-and (pop!) (pop!))))

  (define-instruction! "or" ()
    (d-push! (bitwise-xor (pop!) (pop!))))

  (define-instruction! "drop" ()
    (pop!))

  (define-instruction! "dup" ()
    (d-push! T))

  (define-instruction! "pop" ()
    (d-push! (r-pop!)))

  (define-instruction! "over" ()
    (d-push! S))

  (define-instruction! "a" ()  ; read a
    (d-push! A));;??

  (define-instruction! "nop" () ;; .
    (void))

  (define-instruction! "push" ()
    (r-push! (pop!)))

  (define-instruction! "b!" () ;; store into b
    (set! B (pop!)))

  (define-instruction! "a!" () ;store into a
    (set! A (pop!)))

  ;; Treats T:A as a single 36 bit register and shifts it right by one
  ;; bit. The most signficicant bit (T17) is kept the same.
  (define (multiply-step-even!)
    (let ([t17 (bitwise-and T #x20000)]
          [t0  (bitwise-and T #x1)])
      (set! T (bitwise-ior t17 (arithmetic-shift T -1)))
      (set! A (bitwise-ior (arithmetic-shift t0 (sub1 BIT))
                           (arithmetic-shift A -1)))))

  ;; Sums T and S and concatenates the result with A, shifting
  ;; everything to the right by one bit.
  (define (multiply-step-odd!)
    (let* ([sum (+ T S)]
           [sum17 (bitwise-and sum #x20000)]
           [result (bitwise-ior (arithmetic-shift sum (sub1 BIT))
                                (arithmetic-shift A -1))])
      (set! A (bitwise-bit-field result 0 BIT))
      (set! T (bitwise-ior sum17 (bitwise-bit-field result BIT (* 2 BIT))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; public methods

  (defmacro declare-public (name)
    #`(vector-set! self
                   #,(string->symbol (string-append (symbol->string name) "-i"))
                   #,name))

  ;; (define (get-coord) coord)
  ;; (declare-public get-coord)
  (vector-set! self get-coord-i coord)

  (define (get-memory) memory)
  (declare-public get-memory)

  (define (get-rstack) rstack)
  (declare-public get-rstack)

  (define (get-dstack) dstack)
  (declare-public get-dstack)

  (define (get-registers) (list A B P I R S T))
  (declare-public get-registers)

  (define (load-code code [include-end-token? #f])
    (define (load code index)
      (unless (null? code)
        (vector-set! memory index (car code))
        (load (cdr code) (add1 index))))
    (if include-end-token?
        (begin (load (append code '(end)) 0)
               (set! step0 step0*))
        (begin (load code 0)
               (set! step0 step0-))))
  (declare-public load-code)

  ;; Returns a snapshot of the current state.
  (define (current-state)
    (state A B P I R S T (copy-stack dstack) (copy-stack rstack) (vector-copy memory 0 MEM-SIZE)))
  (declare-public current-state)

  (define (reset! [bit 18])
    (set! BIT bit)
    (set! A 0)
    (set! B 0)
    (set! P 0)
    (set! I 0)
    (set! R 0)
    (set! S 0)
    (set! T 0)
    (set! memory (make-vector MEM-SIZE))
    (set! dstack (make-stack 8))
    (set! rstack (make-stack 8))
    (set! blocking-read #f)
    (set! blocking-write #f)
    (set! blocking #f))
  (declare-public reset!)

  ;; Resets only p
  (define (reset-p! [start 0])
    (set! P start))
  (declare-public reset-p!)

  ;;when not #f, value is the next field to to execute
  (define next-field #f)

  ;;Q: when waiting to complete a read or write, is the node active?
  (define (make-non-active)
    (set! active-nodes (remove self active-nodes)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; single instruction stepping

  (define (execute! opcode [jump-addr-pos 0])
    (if (< opcode 8)
        ((vector-ref instructions opcode) (bitwise-bit-field I 0 jump-addr-pos))
        ((vector-ref instructions opcode))))

  (define (block-maybe next-step-fn)
    ;;cond does not work here. Error: "else: not allowed as an expression"
    (if blocking
        (if blocking-read
            (lambda () (when (complete-read)
                         (next-step-fn)))
            (lambda () (when (complete-write)
                         (next-step-fn))))
        next-step-fn))

  (define (step0-)
    (set! I (vector-ref memory P))
    (set! P (incr P))
    (set! step-fn (block-maybe (if (execute! (bitwise-bit-field I 13 18) 10)
                                   step1
                                   step0-))))
  (define (step0*)
    (set! I (vector-ref memory P))
    (set! P (incr P))
    (if (eq? I 'end)
        (make-non-active)
        (set! step-fn (block-maybe (if (execute! (bitwise-bit-field I 13 18) 10)
                                       step1
                                       step0*)))))
  (define step0 step0-)

  (define (step1)
    (set! step-fn (block-maybe (if (execute! (bitwise-bit-field I 8 13) 8)
                                   step2
                                   step0))))

  (define (step2)
    (set! step-fn (block-maybe (if (execute! (bitwise-bit-field I 3 8) 3)
                                   step3
                                   step0))))

  (define (step3)
    (execute! (arithmetic-shift (bitwise-bit-field I 0 3) 2))
    (set! step-fn (block-maybe step0)))

  (define step-fn step0)

  ;; Executes one step of the program by fetching a word, incrementing
  ;; p and executing the word.
  ;; returns #f when P = 0, else #t
  (define (step-program! [debug? #f])
    (when debug? (display-state (list self)))
    (step-fn))
  (declare-public step-program!)

  ;; Steps the program n times.
  (define (step-program-n! n [debug? #f])
    (for ([i (in-range 0 n)]) (step-program! debug?)))
  (declare-public step-program-n!)


  (define (set-ludr-ports ports)
    (set! port-left (car ports))
    (set! port-up (cadr ports))
    (set! port-down (caddr ports))
    (set! port-right (cadddr ports)))
  (declare-public set-ludr-ports)

  self
  );;end make-node

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execution control

(define (step-program! [debug? #f])
  (set! time (add1 time))
  (for ([node active-nodes])
    (node:step-program! node debug?)))

(define (step-program-n! n [debug? #f])
  (when (> n 0)
    (step-program! debug?)
    (step-program-n! (sub1 n) debug?)))

;;step program until all nodes are non-active
(define (step-program!* [debug? #f])
  (unless (null? active-nodes)
    (step-program! debug?)
    (step-program!* debug?)))

(define (reset!)
  (set! time 0)
  (vector-map node:reset! nodes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; state display functions

(define (display-dstack t s dstack)
  (printf "|d> ~x ~x" t s)
  (display-stack dstack)
  (newline))

(define (display-rstack r rstack)
  (printf "|r> ~x" r)
  (display-stack rstack)
  (newline))

(define (display-state [nodes #f])
  (let ((nodes (if nodes
                   (map coord->node nodes)
                   active-nodes)))
    (for ([node nodes])
      (printf "_____Node ~a state_____\n" (node:get-coord node))
      (let ((state (node:current-state node)))
        (printf "p:~a a:~a b:~a r:~a\n"
                (state-p state)
                (state-a state)
                (state-b state)
                (state-r state))
        (display-dstack (state-t state)
                        (state-s state)
                        (state-dstack state))
        (display-rstack (state-r state)
                        (state-rstack state))))))

(define (display-dstacks [nodes #f])
  (let ((nodes (if nodes
                   (map coord->node nodes)
                   active-nodes)))
    (for ([node nodes])
      (let ((state (node:current-state node)))
        (display (format "(~a)|d> ~x ~x"
                         (node:get-coord node)
                         (state-t state)
                         (state-s state)))
        (display-stack (state-dstack state))
        (newline)))))

(define (display-memory coord [n MEM-SIZE])
  (let* ((node (coord->node coord))
         (mem (state-memory (node:current-state node)))
         (n (sub1 n)))
    (define (print i)
      (let ((v (vector-ref mem i)))
        (printf "~a " v)
        (unless (or (eq? v 'end)
                    (>= i n))
          (print (add1 i)))))
    (printf "node ~a memory: " coord)
    (print 0)
    (newline)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; methods

;; (defmacro method (name)
;;   #`(define #,(string->symbol (string-append (symbol->string name) "-i"))
;;       (begin (set! method-count (add1 method-count))
;;              method-count))
;;   )

(define N-METHODS 0)
(define (i) (let ([index N-METHODS])
              (set! N-METHODS (add1 N-METHODS))
              index))

(define get-coord-i (i))
(define (node:get-coord node) (vector-ref node get-coord-i))

(define get-memory-i (i))
(define (node:get-memory node) ((vector-ref node get-memory-i)))

(define get-rstack-i (i))
(define (node:get-rstack node) ((vector-ref node get-rstack-i)))

(define get-dstack-i (i))
(define (node:get-dstack node) ((vector-ref node get-dstack-i)))

(define get-registers-i (i))
(define (get-registers node) ((vector-ref node get-registers-i)))

(define load-code-i (i))
(define (node:load-code node code [end-token? #f])
  ((vector-ref node load-code-i) code end-token?))

(define current-state-i (i))
(define (node:current-state node) ((vector-ref node current-state-i)))

(define reset!-i (i))
(define (node:reset! node) ((vector-ref node reset!-i)))

(define reset-p!-i (i))
(define (node:reset-p! node) ((vector-ref node reset-p!-i)))

(define step-program!-i (i))
(define (node:step-program! node [debug? #f])
  ((vector-ref node step-program!-i) debug?))

(define step-program-n!-i (i))
(define (node:step-program-n! node n [debug? #f])
  ((vector-ref node step-program-n!-i) n debug?))

(define set-ludr-ports-i (i))
(define (node:set-ludr-ports node ports)
  ((vector-ref node set-ludr-ports-i) ports))

(build-node-matrix)
