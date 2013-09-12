#lang racket

(require (only-in racket [equal? equal-case-sensitive?]))
(require "circular-stack.rkt" "forth_num_convert.rkt" "forth_read.rkt"
	 "rvector.rkt")

(provide (all-defined-out))

(define (equal? a b)
  (if (and (string? a) (string? b))
      (string-ci=? a b)
      (equal-case-sensitive? a b)))

(define address-required-on-dstack '("next" "if" "-if"))
(define address-required (append '("jump" "call") address-required-on-dstack))
(define last-slot-instructions
  '(";" "ret" "unext" "@p" "!p" "+*" "+" "dup" "." "nop"))
(define instructions-preceded-by-nops '("+" "+*"))

; Some instructions end execution of the current word.
; So, the rest of the word should be filled with nops.
(define instructions-using-entire-word '(";" "ret" "ex" "unext"))

(define core%
  (class object%
	 (super-new)
	 (field (dstack (make-stack 2 8 (integer->integer-bytes -1 4 #t)))
		(rstack (make-stack 1 8 (integer->integer-bytes -1 4 #t)))
		(pc 0)
		(next-word 1)
		(rega 0)
		(regb 0)
		(memory (make-rvector 100 '()))
		(blocked-instructions #f))))

(define interpreter%
  (class object%
	 (super-new)

	 (field (num-cores 144)
		(used-cores '())
		(cores (make-vector num-cores))
		(state-index 0)
		(send-recv-table (make-rvector 100 -1)))

	 (for [(i (in-range 0 num-cores))]
	      (vector-set! cores i (new core%)))

	 (for ([i (in-range 100)])
	      (rvector-set! send-recv-table i #f))

	 (define/public (get name)
	   (with-handlers
	    ([exn:fail:object?
	      (lambda (e)
		(dynamic-get-field name
				   (vector-ref cores state-index)))])
	    (dynamic-get-field name this)))

	 (define/public (set name value)
	   (with-handlers
	    ([exn:fail:object?
	      (lambda (e)
		(dynamic-set-field! name
				    (vector-ref cores state-index)
				    value))])
	    (dynamic-set-field! name this value)))

	 (define/public (increment-pc!)
	   (set 'pc (get 'next-word))
	   (set 'next-word (add1 (get 'next-word))))

	 (define/public (read-and-increment-pc!)
	   (let [(old-pc (get 'pc))]
	     (increment-pc!)
	     (rvector-ref (get 'memory) old-pc)))

	 (define/public (set-pc! addr)
	   (set 'pc addr)
	   (set 'next-word (add1 addr)))

	 (define/public (execute-instructions instructions)
	   (define (loop lst)
	     (unless (null? lst)
		     (let* [(name (if (string? (car lst))
				      (car lst)
				      (raise "Not a string")))
			    (proc (get-instruction-proc name))
			    (result (if (member name address-required)
					(proc this (cadr lst))
					(proc this)))]
		       (cond [(equal? result 'restart)
			      (execute-instructions instructions)]
			     [(equal? result 'block)
			      (set 'blocked-instructions lst)]
			     [(member name address-required)
			      (loop (cddr lst))]
			     [else
			      (loop (cdr lst))]))))
	   (loop instructions))

	 ; Continues any instructions that were forced to block previously.
	 ; If there are no more instructions to fetch, removes this core from
	 ; the list of active cores.
	 ; Otherwise, reads the next instruction word and executes it.
	 (define/public (single-step core)
	   (set 'state-index core)
	   (let [(pc (get 'pc))
		 (blocked (get 'blocked-instructions))]
	     (cond [blocked
		    (set 'blocked-instructions #f)
		    (execute-instructions blocked)]
		   [(or (< pc 0) (>= pc (rvector-length (get 'memory))))
		    (set 'used-cores (remove core (get 'used-cores)))]
		   [else (execute-instructions (read-and-increment-pc!))])))

	 (define/public (step)
	   (for [(core (get 'used-cores))]
		(single-step core)))

	 (define/public (interpret)
	   (step)
	   (unless (null? (get 'used-cores))
		   (interpret)))))

(define instructions (make-hash))
(define (is-instruction? name)
  (hash-has-key? instructions name))
(define (add-instruction! name code)
  (hash-set! instructions name code))
(define (get-instruction-proc name)
  (and (is-instruction? name)
       (hash-ref instructions name)))

; Compiler directive - something that is executed at compile time
(define directives (make-hash))
(define (is-directive? name)
  (hash-has-key? directives name))
(define (add-directive! name code)
  (hash-set! directives name code))
(define (get-directive-proc name)
  (and (is-directive? name)
       (hash-ref directives name)))

(define (make-instruction-synonym a b)
  (let [(a-present (is-instruction? a))
	(b-present (is-instruction? b))]
    ; If both are defined, or neither is defined, error.
    (cond [(or (and a-present b-present) (not (or a-present b-present)))
	   (raise "Cannot make synonym")]
	  [a-present
	   (add-instruction! b (hash-ref instructions a))]
	  [else
	   (add-instruction! a (hash-ref instructions b))])))

(define compiler%
  (class object%
	 (super-new)

	 (field (location-counter 1)
		(i-register 0)
		(dict (make-hash))
		(execute? #f)
		(dstack (make-infinite-stack))
		(interpreter (new interpreter%)))

	 ; Note: It is important that we look in this object before the
	 ; interpreter, because both of them have a dstack.
	 (define/public (get name)
	   (with-handlers
	    ([exn:fail:object?
	      (lambda (e)
		(send interpreter get name))])
	    (dynamic-get-field name this)))

	 (define/public (set name value)
	   (with-handlers
	    ([exn:fail:object?
	      (lambda (e)
		(send interpreter set name value))])
	    (dynamic-set-field! name this value)))

	 (define/public (set-pc! value)
	   (send interpreter set-pc! value))

	 (define/public (increment-pc!)
	   (send interpreter increment-pc!))

	 (define/public (add-word! name code)
	   (hash-set! dict name code))

	 (define/public (get-word-address name)
	   (and (hash-has-key? dict name)
		(hash-ref dict name)))

	 (define/public (get-slot addr)
	   (let [(lst (rvector-ref (get 'memory) (quotient addr 4)))]
	     (if (list? lst)
		 (list-ref lst (remainder addr 4))
		 lst)))

	 (define/public (add-to-slot! elmt addr)
	   (let* [(memory (get 'memory))
		  (index (quotient addr 4))
		  (slot (remainder addr 4))
		  (existing (rvector-ref memory index))]
	     (unless (= (length existing) slot)
		     (raise "Incorrect compilation - not enough or too many existing slots"))
	     (rvector-set! memory index (append existing (list elmt)))))

	 (define/public (add-compiled-data! data)
	   (add-to-next-word! data)
	   (fill-rest-with-nops))
	   
	 (define/public (add-to-next-word! data)
	   (let [(memory (get 'memory))
		 (location-counter (get 'location-counter))]
	     (rvector-set! memory location-counter data)
	     (set 'location-counter (add1 location-counter))))

; Compiles a single instruction or constant.
; In the case of a constant, it implicitly adds @p.
; Deals with instructions-preceded-by-nops
; Deals with last-slot-instructions
; Invariant:  At the beginning and the end of each invocation:
; a. i-register is the index of the next slot (not word) in (the variable) memory to compile into
; b. location-counter is the F18A address which is the next available word.  The memory address is 4*location-counter.
	 (define/public (add-compiled-code! elmt)
	   (let [(memory (get 'memory))]

; Standard compilation - Put the thing into the slot given by i-register.
; Increment i-register if there are still remaining slots in the word.
; Otherwise, set i-register to address represented by location-counter, and
; increment location-counter.
	     (define (standard-compile! elmt)
	       (add-to-slot! elmt i-register)
	       (if (= (remainder i-register 4) 3)
		   (begin (set 'i-register (* 4 (get 'location-counter)))
			  (set 'location-counter
			       (add1 (get 'location-counter))))
		   (set 'i-register (add1 i-register))))

	     (cond [(not elmt)
		    (raise "Got a value of #f in standard-compile!")]

		   [(bytes? elmt)
; Number constant.  Need to compile @p and the number.
		    (add-to-next-word! elmt)
		    (add-compiled-code! "@p")]

		   [(string? elmt)
; Standard instruction compilation.  Deals with inserting nops where necessary.
; Addresses (for address-required instructions) must be supplied separately.
		    (when (or (and (member elmt instructions-preceded-by-nops)
				   (not (equal? (get-slot (sub1 i-register)) ".")))
			      (and (= (remainder i-register 4) 3)
				   (not (member elmt last-slot-instructions))))
			  (add-compiled-code! "."))
		    (standard-compile! elmt)]

		   [(number? elmt)
; Compilation of an address.
; TODO: Check if the address can fit.
		    (when (not (member (get-slot (sub1 i-register)) address-required))
			  (raise "Tried to compile a number that was not an address --- add-compiled-code!"))
		    (add-to-slot! elmt i-register)
		    (set 'i-register (* 4 location-counter))
		    (set 'location-counter (add1 location-counter))]
	
		   [else (raise "Unknown thing to compile --- add-compiled-code!")])))

	 (define/public (compile-address! addr)
	   (add-compiled-code! addr))

	 (define/public (compile-constant! const)
	   (add-compiled-code! const))

	 (define/public (fill-rest-with-nops)
	   (unless (= (remainder i-register 4) 0)
		   (add-compiled-code! ".")
		   (fill-rest-with-nops)))

	 (define/public (port->number str)
	   (cond
	    [(equal? str "up")    (int->bytes 325)]
	    [(equal? str "down")  (int->bytes 277)]
	    [(equal? str "left")  (int->bytes 373)]
	    [(equal? str "right") (int->bytes 469)]
	    [(equal? str "io")    (int->bytes 349)]
	    [else #f]))

	 (define/public (compile-loop)
	   (let [(token (forth_read))]
	     (if (eof-object? token)
		 (fill-rest-with-nops)
		 (begin (unless (eq? token #\newline)
				(compile-token token))
			(compile-loop)))))

	 (define/public (compile-token token)
	   (let [(directive (get-directive-proc token))
		 (instruction (get-instruction-proc token))
		 (address (get-word-address token))]
	     (cond [directive
		    (directive this)]
		   [(and instruction execute?)
; Assume that it is not an instruction that requires an address as an argument
		    (instruction (get 'interpreter))]
		   [instruction
		    (add-compiled-code! token)
		    (when (member token instructions-using-entire-word)
			  (fill-rest-with-nops))]
		   [address
		    (let [(nxt (forth_read))]
; TODO: Check if address can fit.  For now, don't put jump/call in last slot.
; This is already taken care of by add-compiled-code!
		      (if (equal? nxt ";")
			  (add-compiled-code! "jump")
			  (begin (forth_read 'put-back nxt)
				 (add-compiled-code! "call")))
; Compile the address.  Automatically compiles #f into the rest of the word.
		      (compile-address! address))]
		   [else
		    (let [(num (or (port->number token) 
				   (string->bytes token)))]
		      (if num
			  (if execute?
			      (push-cells! (get 'dstack) num)
			      (compile-constant! num))
			  (raise (string-append token " ?"))))])))))

; Stacks
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
