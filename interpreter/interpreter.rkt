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
      ;; (set-ludr-ports node (list (get-create-port coord 'left)
      ;;                            (get-create-port coord 'up)
      ;;                            (get-create-port coord 'down)
      ;;                            (get-create-port coord 'right)))
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
  null ;;TODO
  )
(define (set-ludr-ports ports)
  null;;TODO
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; program loading

;;compiles, assembles, and loads code into nodes
(define (load-program in)
  (let ([n 0]
        [code 0]
        [node 0])
    (for ([code (assemble-all (compile-string in))])
      (set! node (coord->node (car code)))
      (node:load-code node (cdr code))
      (set! active-nodes (cons node active-nodes)))))

(define (load-file file)
  (call-with-input-file file load-program))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; node object

(define (make-node index)
  (define coord (index->coord index))

  ;; stacks:
  (define dstack (make-stack 8))
  (define rstack (make-stack 8))

  ;; registers:
  (define A 0)
  (define B 0)
  (define P 0)
  (define I 0)
  (define R 0)
  (define S 0)
  (define T 0)

  (define memory-size MEM-SIZE)
  (define memory (make-vector MEM-SIZE))
  (define memory-wrap #f)

  ;; (define comm-data '())
  ;; (define comm-type '())
  ;; (define comm-recv '())

  (define instructions (make-vector 35))
  (define BIT 18)

  (define (load-program in [start 0])
    (foldl (lambda (word pos) (vector-set! memory pos word) (add1 pos))
           start in))

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
      (let ([jump-addr (bitwise-bit-field I 0 jump-addr-pos)])
        ((vector-ref instructions opcode) jump-addr)))
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
        (let ([value 12]);(random (arithmetic-shift 1 BIT))])
          ;; (cond [(= addr UP)    (set! comm-data (cons value comm-data))
          ;;        (set! comm-recv (cons value comm-recv))
          ;;        (set! comm-type (cons 0 comm-type))]
          ;;       [(= addr DOWN)  (set! comm-data (cons value comm-data))
          ;;        (set! comm-recv (cons value comm-recv))
          ;;        (set! comm-type (cons 1 comm-type))]
          ;;       [(= addr LEFT)  (set! comm-data (cons value comm-data))
          ;;        (set! comm-recv (cons value comm-recv))
          ;;        (set! comm-type (cons 2 comm-type))]
          ;;       [(= addr RIGHT) (set! comm-data (cons value comm-data))
          ;;        (set! comm-recv (cons value comm-recv))
          ;;        (set! comm-type (cons 3 comm-type))]
          ;;       [(= addr IO)    (set! comm-data (cons value comm-data))
          ;;        (set! comm-recv (cons value comm-recv))
          ;;        (set! comm-type (cons 4 comm-type))])
          value)
        (vector-ref memory (if memory-wrap (modulo addr memory-size) addr))))

  ;; Read from the given memory address or communication port. If it
  ;; gets a communication port, it just returns a random number (for
  ;; now).
  (define (read-memory-@p addr)
    (vector-ref memory addr))

  ;; Write to the given memeory address or communication
  ;; port. Everything written to any communication port is simply
  ;; aggregated into a list.
  (define (set-memory! addr value)
    (cond ;; [(= addr UP)    (set! comm-data (cons value comm-data))
     ;;  (set! comm-type (cons 5 comm-type))]
     ;; [(= addr DOWN)  (set! comm-data (cons value comm-data))
     ;;  (set! comm-type (cons 6 comm-type))]
     ;; [(= addr LEFT)  (set! comm-data (cons value comm-data))
     ;;  (set! comm-type (cons 7 comm-type))]
     ;; [(= addr RIGHT) (set! comm-data (cons value comm-data))
     ;;  (set! comm-type (cons 8 comm-type))]
     ;; [(= addr IO)    (set! comm-data (cons value comm-data))
     ;;  (set! comm-type (cons 9 comm-type))]
     [else           (vector-set! memory (if memory-wrap (modulo addr memory-size) addr) value)]))

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

  (define-instruction! "jump" (a)
    (set! P a)
    #f)

  (define-instruction! "call" (a)
    (r-push! P)
    (set! P a)
    #f)

  (define-instruction! "unext" (_) ;; -- hacky!
    (if (= R 0)
        (r-pop!)
        (begin (set! R (sub1 R))
               (set! P (sub1 P))
               #f)))

  (define-instruction! "next" (a)
    (if (= R 0)
        (begin (r-pop!)
               #f)
        (begin (set! R (sub1 R))
               (set! P a)
               #f)))

  (define-instruction! "if" (a)
    (and (not (= T 0))
         (set! P a)
         #f))

  (define-instruction! "-if" (a)
    (and (not (bitwise-bit-set? T (sub1 BIT)))
         (set! P a)
         #f))

  (define-instruction! "@p" (_) ;;TODO: this is a HACK!!!
    (d-push! (read-memory-@p P))
    (set! P (incr P)))

  (define-instruction! "@+" (_) ; fetch-plus
    (d-push! (read-memory A))
    (set! A (incr A)))

  (define-instruction! "@b" (_) ;fetch-b
    (d-push! (read-memory B)))

  (define-instruction! "@" (_); fetch a
    (d-push! (read-memory A)))

  (define-instruction! "!p" (_) ; store p
    (set-memory! P (pop!))
    (set! P (incr P)))

  (define-instruction! "!+" (_) ;store plus
    (set-memory! A (pop!))
    (set! A (incr A)))

  (define-instruction! "!b" (_); store-b
    (set-memory! B (pop!)))

  (define-instruction! "!" (_); store
    (set-memory! A (pop!)))

  (define-instruction! "+*" (_) ; multiply-step
    (if (even? A)
        (multiply-step-even!)
        (multiply-step-odd!)))

  (define-instruction! "2*" (_)
    (set! T (18bit (arithmetic-shift T 1))))

  (define-instruction! "2/" (_)
    (set! T (arithmetic-shift T -1)))

  (define-instruction! "-" (_) ;not
    (set! T (18bit (bitwise-not T))))

  (define-instruction! "+" (_) ;;TODO: extended arithmetic mode
    (d-push! (+ (pop!) (pop!))))

  (define-instruction! "and" (_)
    (d-push! (bitwise-and (pop!) (pop!))))

  (define-instruction! "or" (_)
    (d-push! (bitwise-xor (pop!) (pop!))))

  (define-instruction! "drop" (_)
    (pop!))

  (define-instruction! "dup" (_)
    (d-push! T))

  (define-instruction! "pop" (_)
    (d-push! (r-pop!)))

  (define-instruction! "over" (_)
    (d-push! S))

  (define-instruction! "a" (_)  ; read a
    (d-push! A));;??

  (define-instruction! "nop" (_) ;; .
    (void))

  (define-instruction! "push" (_)
    (r-push! (pop!)))

  (define-instruction! "b!" (_) ;; store into b
    (set! B (pop!)))

  (define-instruction! "a!" (_) ;store into a
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

  (define method-vector (make-vector N-METHODS))

  (defmacro declare-public (name)
    #`(vector-set! method-vector
                   #,(string->symbol (string-append (symbol->string name) "-i"))
                   #,name))

  ;; (define (get-coord) coord)
  ;; (declare-public get-coord)
  (vector-set! method-vector get-coord-i coord)

  (define (get-memory) memory)
  (declare-public get-memory)

  (define (get-rstack) rstack)
  (declare-public get-rstack)

  (define (get-dstack) dstack)
  (declare-public get-dstack)

  (define (get-registers) (list A B P I R S T))
  (declare-public get-registers)

  (define (load-code code)
    (define (load code index)
      (unless (null? code)
        (vector-set! memory index (car code))
        (load (cdr code) (add1 index))))
    (load code 0))
  (declare-public load-code)

  ;; Returns a snapshot of the current state.
  (define (current-state)
    (state A B P I R S T (copy-stack dstack) (copy-stack rstack) (vector-copy memory 0 MEM-SIZE)))
  (declare-public current-state)

  ;; Resets the state of the interpreter:
  (define (reset! [bit 18])
    (set! BIT bit)
    (set! A 0)
    (set! B 0)
    (set! P 0)
    (set! I 0)
    (set! R 0)
    (set! S 0)
    (set! T 0)
    (set! memory (make-vector MEM-SIZE)))
  (declare-public reset!)

  ;; Resets only p
  (define (reset-p! [start 0])
    (set! P start))
  (declare-public reset-p!)

  ;; Executes one step of the program by fetching a word, incrementing
  ;; p and executing the word.
  (define (step-program! [debug? #f])
    (set! I (vector-ref memory P))
    (set! P (incr P))
    (when debug? (display-state))
    (execute-word!))
  (declare-public step-program!)

  ;; Steps the program n times.
  (define (step-program-n! n [debug? #f])
    (for ([i (in-range 0 n)]) (step-program! debug?)))
  (declare-public step-program-n!)

  ;; Steps the program until it hits an instructions made up only of
  ;; nops or only of 0s. This should be useful for debugging small programs.
  (define (step-program!* [debug? #f])
    (let ([next (vector-ref memory P)])
      (unless (or (= next #x39ce7) (= next 0))
        (step-program! debug?) (step-program!* debug?))))
  (declare-public step-program!*)

  method-vector
  );;end make-node

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execution control

(define (step-program! [debug? #f])
  (for ([node active-nodes])
    (node:step-program! node debug?)))

(define (step-program-n! n [debug? #f])
  (if (> n 0)
      (step-program! debug?)
      (step-program-n! (sub1 n) debug?)))

(define (step-program!* [debug? #f])
  ;;TODO: fix
  (for ([node active-nodes])
    (node:step-program!* node debug?)))

(define (reset!)
  (map node:reset! active-nodes))

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
         (mem (state-memory (node:current-state node))))
    (printf "node ~a memory:\n" coord)
    (for ([i (in-range 0 n)])
      (printf "~x " (vector-ref mem i)))
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
(define (node:load-code node code) ((vector-ref node load-code-i) code))

(define current-state-i (i))
(define (node:current-state node) ((vector-ref node current-state-i)))

(define reset!-i (i))
(define (node:reset! node) ((vector-ref node reset!-i)))

(define reset-p!-i (i))
(define (node:reset-p! node) ((vector-ref node reset-p!-i)))

(define step-program!-i (i))
(define (node:step-program! node [debug? #f])
  ((vector-ref node step-program!-i) [debug? #f]))

(define step-program-n!-i (i))
(define (node:step-program-n! node n [debug? #f])
  ((vector-ref node step-program-n!-i) n debug?))

(define step-program!*-i (i))
(define (node:step-program!* node [debug? #f])
  ((vector-ref node step-program!*-i) debug?))


(build-node-matrix)
