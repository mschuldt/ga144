(require "../compiler/circular-stack.rkt"
	 "../compiler/forth_num_convert.rkt"
	 "../compiler/forth_read.rkt"
	 "../compiler/rvector.rkt")

(define nodes (make-vector 144))
(define used-nodes '())
;;the current node
(define node #f)
(define coord #f)

;;node array indexes
(define node-memory-i 0)
(define node-location-counter-i 1)
(define node-i-register-i 2)
(define node-coord-i 3)
(define node-used-i 4)
(define n-node-fields 5)

(define (make-node coord)
  (let ((a (make-vector n-node-fields)))
    (vector-set! a node-memory-i (make-rvector 100 '()))
    (vector-set! a node-location-counter-i 1)
    (vector-set! a node-i-register-i 0)
    (vector-set! a node-coord-i coord)
    (vector-set! a node-used-i #f)
    a))

(define (init-nodes)
  (for ([i 144])
       (vector-set! nodes i (make-node (index->coord i))))))

(define (index->coord n)
  (+ (* (quotient n 18) 100) (remainder n 18)))

(define (coord->index n)
  (+ (* (quotient n 100) 18) (remainder n 100)))

;; compiler variables
(define stack 0)
(define location-counter 1)
(define i-register 0)
(define dict (make-hash)) ;;word dict

;; Compiler directives
(define directives (make-hash))
(define (is-directive? name)
  (hash-has-key? directives name))
(define (add-directive! name code)
  (hash-set! directives name code))
(define (get-directive-proc name)
  (and (is-directive? name)
       (hash-ref directives name)))


(define (add-word! name code)
  (hash-set! dict name code))

(define (get-word-address name)
  (and (hash-has-key? dict name)
       (hash-ref dict name)))

(define (get-slot addr)
  (if (< addr 0)
      #f
      (let [(lst (rvector-ref memory (quotient addr 4)))
	    (index (remainder addr 4))]
	(if (list? lst)
	    (if (< index (length lst))
		(list-ref lst index)
		#f)
	    lst))))

(define (max-address-size word)
  ;;returns the max address that can fit in the remaining slots of WORD
  (let ((len (- 4 (length word))))
    (if (= len 1)
	8
	(expt 2 (+ 3 (* 5 (sub1 len)))))))

(define (add-to-slot! elmt addr)
  (let* [(index (quotient addr 4))
	 (slot (remainder addr 4))
	 (existing (rvector-ref memory index))]
    (unless (= (length existing) slot)
	    (raise "not enough or too many existing slots"))
    (rvector-set! memory index (append existing (list elmt)))))


(define (add-compiled-data! data)
  (add-to-next-word! data)
  (fill-rest-with-nops))

(define (add-to-next-empty-word! data)
  (let* [(location-counter-1 (sub1 location-counter))]
    (if (= (remainder i-register 4) 0)
	(begin
	  (unless (null? (rvector-ref memory location-counter-1))
		  (raise "Expected empty memory cell"))
	  (rvector-set! memory location-counter-1 data)
	  (go-to-next-word))
	(rvector-set! memory location-counter data))
    (set! location-counter (add1 location-counter))))

(define (add-to-next-word! data)
  (rvector-set! memory location-counter data)
  (set! location-counter (add1 location-counter)))

(define (go-to-next-word)
  (set! i-register (* 4 location-counter))
  (set! location-counter (add1 location-counter)))

(define (add-compiled-code! elmt)
  ;; Standard compilation - Put the thing into the slot given by i-register.
  ;; Increment i-register if there are still remaining slots in the word.
  ;; Otherwise, set i-register to address represented by location-counter, and
  ;; increment location-counter.
  (define (standard-compile! elmt)
    (add-to-slot! elmt i-register)
    (if (= (remainder i-register 4) 3)
	(go-to-next-word)
	(set! i-register (add1 i-register))))

  (cond [(not elmt)
	 (raise "Got a value of #f in standard-compile!")]

	[(bytes? elmt)
	 ;; Number constant.  Need to compile @p and the number.
	 (add-to-next-word! elmt)
	 (add-compiled-code! "@p")]

	[(string? elmt)
	 ;; Standard instruction compilation.  Deals with inserting
	 ;; nops where necessary.
	 ;; Addresses (for address-required instructions) must be
	 ;; supplied separately.
	 (when (or (and (member elmt instructions-preceded-by-nops)
			(not (equal? (get-slot (sub1 i-register)) ".")))
		   (and (= (remainder i-register 4) 3)
			(not (member elmt last-slot-instructions))))
	       (add-compiled-code! "."))
	 (standard-compile! elmt)]

	[(number? elmt)
	 (let ((word (rvector-ref memory (quotient i-register 4))))
	   (when (> elmt (max-address-size word))
		 (let* ((word (reverse word))
			(op (car word))
			(word (reverse (cdr word))))
		   (rvector-set! memory (quotient i-register 4) word)
		   (set! i-register (sub1 i-register))
		   (fill-rest-with-nops)
		   (add-compiled-code! op))))
	 (when (not (member (get-slot (sub1 i-register)) address-required))
	       (raise "Tried to compile a number that was not an address "))
	 (add-to-slot! elmt i-register)
	 (set! i-register (* 4 location-counter))
	 (set! location-counter (add1 location-counter))]

	[else (raise "Unknown thing to compile")]))


(define (compile-address! addr)
  (add-compiled-code! addr))

(define (compile-address-to-slot! addr slot)
  (when (not (member (get-slot (sub1 slot)) address-required))
        (raise "Tried to compile a number that was not an address"))
  (add-to-slot! addr slot))

(define (compile-constant! const)
  (add-compiled-code! const))

(define (fill-rest-with-nops)
  (unless (= (remainder i-register 4) 0)
	  (add-compiled-code! ".")
	  (fill-rest-with-nops)))

(define (port->number str)
  (cond
   [(equal? str "up")    (int->bytes 325)] ;;?????
   [(equal? str "down")  (int->bytes 277)]
   [(equal? str "left")  (int->bytes 373)]
   [(equal? str "right") (int->bytes 469)]
   [(equal? str "io")    (int->bytes 349)]
   [else #f]))


(define (compile-loop)
  (let [(token (forth_read))]
    (if (eof-object? token)
	(fill-rest-with-nops)
	(begin (unless (eq? token #\newline)
		       (compile-token token))
	       (compile-loop)))))

(define (compile-token token)
  (let [(directive (get-directive-proc token))
	(instruction (get-instruction-proc token))
	(address (get-word-address token))]
    (cond [directive
	   (directive)]
	  [instruction
	   (add-compiled-code! token)
	   (when (member token instructions-using-entire-word)
                 (fill-rest-with-nops))]
	  [address
	   (let [(nxt (forth_read))]
	     ;; TODO: Check if address can fit.  For now, don't put
	     ;; jump/call in last slot.
	     ;; This is already taken care of by add-compiled-code!
	     (if (equal? nxt ";")
		 (add-compiled-code! "jump")
		 (begin (forth_read 'put-back nxt)
			(add-compiled-code! "call")))
	     ;; Compile the address.  Automatically compiles #f into the
	     ;; rest of the word.
	     (compile-address! address))]
	  [else
	   (let [(num (or (port->number token)
			  (string->bytes token)))]
	     (if num
		 (compile-constant! num)
		 (raise (string-append token " ?"))))])))


(define (print-state)
  (printf "  i-register = ~a\n" i-register)
  (printf "  location-counter = ~a\n" location-counter)
  (printf "  memory = ")
  (print-rvector memory))


;; Stacks
(define push-cells! push!)
(define (push-int! stack num)
  (push-cells! stack (int->bytes num)))

(define pop-cells! pop!)
(define (pop-int! stack signed?)
  (integer-bytes->integer (pop-cells! stack) signed? #t))

(define get-cells peek)
(define (get-int stack signed? [pos 0])
  (integer-bytes->integer (get-cells stack pos) signed? #t))

(define (print-stack stack)
  (define (loop pos)
    (print (get-int stack #t pos))
    (display " ")
    (unless (= pos 0) (loop (sub1 pos))))
  (display "| ")
  (loop (sub1 (stack-length stack)))
  (display ">"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;compiler directives


(define (valid-node? n)
  ;;TODO: fix
  (and (integer? n)
       (>= n 0)))

(define (coord->index n)
  (+ (* (quotient n 100) 18) (remainder n 100)))

(add-directive!
 "node"
 (lambda ()
   (let* ((token (forth_read))
	  (n (string->number token)))
     (unless (valid-node? n)
	     (raise (format "Err: invalid node number: ~a" token)))
     (let* ((index (coord->index n))
	    (node (vector-ref nodes index)))
       ;;save in used list
       (unless (vector-ref node node-used-i)
	       (set! used-nodes (cons node used-nodes))
	       (vector-set! node node-used-i #t))
       ;;save current node state
       (when node
	     (vector-set! node node-memory-i memory)
	     (vector-set! node node-location-counter-i location-counter)
	     (vector-set! node node-i-register-i i-register))
       ;;set globals for new node
       (set! memory (vector-ref node node-memory-i))
       (set! location-counter (vector-ref node node-location-counter-i))
       (set! i-register (vector-ref node node-i-register-i))
       (set! coord (vector-ref node node-coord-i))))))

(add-directive! ;;TODO: test
 "org"
 (lambda ()
   (let* ([token (forth_read)]
	  [n (string->number token)])
     (unless (and (integer? n)
		  (>= n 0))
	     ;;TODO: upper bound?
	     (raise (format "Err: invalid argument to 'org' directive: '~a'" n)))
     (set! location-counter (add1 n))
     (set! i-register (* 4 n)))))

(add-directive!
 ":"
 (lambda ()
   (fill-rest-with-nops)
   (add-word! (forth_read_no_eof) (quotient i-register 4))))

(add-directive!
 ".."
 (lambda () (fill-rest-with-nops)))

;; Comments
(define (comment)
  (unless (equal? (read-char) #\))
	  (comment)))
(add-directive! "(" comment)

;; ,
(add-directive! ;; page 23 of arrayforth users manual DB004
 ","
 (lambda ()
   (let* ([token (forth_read)]
	  [data (string->number token)])
     ;;(send compiler add-compiled-data! data)
     (add-to-next-empty-word! data))))

;; begin
(define (begin-proc)
  (fill-rest-with-nops)
  (push-int! stack (quotient i-register 4)))

(add-directive! "begin" begin-proc)

(add-directive!
 "for"
 (lambda ()
   (add-compiled-code! "push")
   (begin-proc)))

;; next, when seen in the compiler
(add-directive!
 "next"
 (lambda ()
   (let [(addr (pop-int! stack #f))]
     (if (= addr (quotient i-register 4))
	 (add-compiled-code! "unext")
	 (begin (add-compiled-code! "next")
		(compile-address! addr))))))

(define (make-if-directive instr)
  (lambda ()
    (add-compiled-code! instr)
    ;; Since it cannot be in the last spot, there must be space for the address
    (push-int! stack i-register)
    (go-to-next-word)))

;; if, when seen in the compiler
;; Compile the "if" instruction
;; Put the current address on the stack so that "then" can compile the address to jump to
(add-directive!
 "if"
 (make-if-directive "if"))

(add-directive!
 "-if"
 (make-if-directive "-if"))

(add-directive!
 "then"
 (lambda ()
   (fill-rest-with-nops)
   (let [(old-ireg (pop-int! stack #f))
	 (curr-ireg i-register)]
     (when (not (= (remainder curr-ireg 4) 0))
	   (raise "i-register should be at the start of a word, but is not"))
     (compile-address-to-slot! (/ curr-ireg 4) old-ireg))))

(define (swap)
  (let* ((T (pop-cells! stack))
	 (S (pop-cells! stack)))
    (push-cells! stack T)
    (push-cells! stack S)))

(add-directive!
 "#swap"
 swap)
