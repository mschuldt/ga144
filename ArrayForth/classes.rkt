#lang racket

(require (only-in racket [equal? equal-case-sensitive?]))
(require "arithmetic.rkt" "circular-stack.rkt" "forth_num_convert.rkt"
	 "forth_read.rkt" "rvector.rkt")

(provide (all-defined-out))

(define (equal? a b)
  (if (and (string? a) (string? b))
      (string-ci=? a b)
      (equal-case-sensitive? a b)))

(define address-required-on-cstack '("next" "if" "-if"))
(define address-required (append '("jump" "call") address-required-on-cstack))
(define last-slot-instructions
  '(";" "ret" "unext" "@p" "!p" "+*" "+" "dup" "." "nop"))
(define instructions-preceded-by-nops '("+" "+*"))

; Some instructions end execution of the current word.
; So, the rest of the word should be filled with nops.
(define instructions-using-entire-word '(";" "ret" "unext"))

(define core%
  (class object%
	 (super-new)
	 (field (dstack (make-stack 2 8 (integer->integer-bytes -1 4 #t)))
		(rstack (make-stack 1 8 (integer->integer-bytes -1 4 #t)))
		(pc 0)
		(next-word 1)
		(rega 0)
		(regb 0)
		(memory (make-rvector 100 ".")))))

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
	   (if (= (remainder (get 'pc) 4) 3)
	       (begin (set 'pc (* 4 (get 'next-word)))
		      (set 'next-word (add1 (get 'next-word))))
	       (set 'pc (add1 (get 'pc)))))

	 (define/public (set-pc! addr)
	   (set 'pc (* 4 addr))
	   (set 'next-word (add1 addr)))

	 (define/public (execute-code addr)
	   ; pc will be the address of the next instruction to execute
	   (push-int! (get 'rstack) (inexact->exact (ceiling (/ (get 'pc) 4))))
	   (set-pc! addr))

	 (define/public (execute entry)
	   (execute-code (entry-code entry)))

	 (define/public (code-loop)
	   (let* [(ended-list
		   (map
		    (lambda (num)
		      (set 'state-index num)
		      (let [(memory (get 'memory))]
			(if (or (< (get 'pc) 0) (>= (get 'pc) (rvector-length memory)))
			    #t
			    (let [(code (rvector-ref memory (get 'pc)))]
			      (increment-pc!)
			      (cond [(and (string? code) (member code address-required))
				     (let [(addr (rvector-ref memory (get 'pc)))]
				       (increment-pc!)
				       ((rvector-ref codespace (entry-code (find-entry dict code))) this addr))]
				    [(string? code)
				     ((rvector-ref codespace (entry-code (find-entry dict code))) this)]
				    [else
				     (raise "Instruction is not a string -- code-loop")])
			      #f))))
		    used-cores))]
	     (unless (for/and ([bool ended-list]) bool)
		     (code-loop))))))


(define codespace (make-rvector 100 -1))
(define dict (make-rvector 500 -1))
(define compiler-directives (make-rvector 100 -1))

(define (add-entry! prim name code)
  (let [(new (entry prim name code))]
    (add-element! dict new)
    new))

(define (add-primitive-code! elmt)
  (add-element! codespace elmt))

(define (add-word! prim name)
  (add-entry! prim name (rvector-length codespace)))

(define (add-primitive-word! name code)
  (add-word! #t name)
  (add-primitive-code! code))

; Adds a new compiler directive - something that is executed
(define (add-compiler-directive! name code)
  (add-element! compiler-directives
		(entry #t name (rvector-length codespace)))
  (add-primitive-code! code))

(define (make-synonym a b)
  (let [(a-dir (find-entry dict a))
	(b-dir (find-entry dict b))]
    ; If both are defined, or neither is defined, error.
    (cond [(or (and a-dir b-dir) (not (or a-dir b-dir)))
	   (raise "Cannot make synonym")]
	  [a-dir (add-entry! (entry-primitive a-dir) b (entry-code a-dir))]
	  [else (add-entry! (entry-primitive b-dir) a (entry-code b-dir))])))

; Entry for the dictionary.
; Code must be mutable to allow procs which refer to the entry itself.
(struct entry (primitive name [code #:mutable]))

(define compiler%
  (class object%
	 (super-new)

	 (field (location-counter 1)
		(i-register 0)
		(execute? #f)
		(cstack (make-infinite-stack))
		(literal-mode 0)
		(litspace (make-rvector 100 -1))
		(lit-entry 0)
		(interpreter (new interpreter%)))

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
	     (define (standard-compile! thing)
	       (rvector-set! memory i-register elmt)
	       (if (= (remainder i-register 4) 3)
		   (begin (set 'i-register (* 4 (get 'location-counter)))
			  (set 'location-counter
			       (add1 (get 'location-counter))))
		   (set 'i-register (add1 i-register))))

	     (cond [(not elmt)
; This slot should be taken up by something else, but I don't bother.
; For example, numbers should take up all 4 slots, but they only take up 1.
; Rest are #f.
; Similar thing happens whenever an address is compiled.
		    (standard-compile! elmt)]

		   [(bytes? elmt)
; Number constant.  Need to compile @p and the number.
		    (rvector-set! memory (* 4 (get 'location-counter)) elmt)
		    (for [(i (in-range 1 4))]
			 (rvector-set! memory (+ (* 4 (get 'location-counter)) i) #f))
		    (set 'location-counter (add1 (get 'location-counter)))
		    (add-compiled-code! "@p")]

		   [(string? elmt)
; Standard instruction compilation.  Deals with inserting nops where necessary.
; Addresses (for address-required instructions) must be supplied separately.
		    (when (or (and (member elmt instructions-preceded-by-nops)
				   (not (equal? (rvector-ref memory (sub1 i-register)) ".")))
			      (and (= (remainder i-register 4) 3)
				   (not (member elmt last-slot-instructions))))
			  (add-compiled-code! "."))
		    (standard-compile! elmt)]

		   [(number? elmt)
; Compilation of an address.
; TODO: Check if the address can fit.
		    (when (not (member (rvector-ref memory (sub1 i-register)) address-required))
			  (raise "Tried to compile a number that was not an address --- add-compiled-code!"))
		    (standard-compile! elmt)
		    (fill-rest-with-false)]
	
		   [else (raise "Unknown thing to compile --- add-compiled-code!")])))

	 (define/public (compile-address! addr)
	   (add-compiled-code! addr))
	 (define/public (compile-constant! const)
	   (add-compiled-code! const))

	 (define/public (fill-rest-with-nops)
	   (unless (= (remainder i-register 4) 0)
		   (add-compiled-code! ".")
		   (fill-rest-with-nops)))

	 (define/public (fill-rest-with-false)
	   (unless (= (remainder i-register 4) 0)
		   (add-compiled-code! #f)
		   (fill-rest-with-false)))

	 (define/public (port->number str)
	   (cond
	    [(equal? str "up")    (int->bytes 325)]
	    [(equal? str "down")  (int->bytes 277)]
	    [(equal? str "left")  (int->bytes 373)]
	    [(equal? str "right") (int->bytes 469)]
	    [(equal? str "io")    (int->bytes 349)]
	    [else #f]))

	 (define/public (compile-loop)
	   (let [(to-compile (forth_read))]
	     (unless (eof-object? to-compile)
		     (unless (eq? to-compile #\newline)
			     (let [(directive (find-entry compiler-directives to-compile))]
			       (if directive
				   ((rvector-ref codespace (entry-code directive)) this)
				   (let [(entry (find-entry dict to-compile))]
				     (cond [(not entry)
					    (let [(num (or (port->number to-compile) 
							   (string->bytes to-compile)))]
					      (if num
						  (if execute?
						      (push-cells! (get 'dstack) num)
						      (compile-constant! num))
						  (raise (string-append to-compile " ?"))))]
					   [execute?
; Assume that it is not an instruction that requires an address as an argument
					    ((rvector-ref codespace (entry-code entry)) (get 'interpreter))]
					   [(member to-compile address-required-on-cstack)
					    (when (= (remainder i-register 4) 3)
						  (fill-rest-with-nops))
					    (add-compiled-code! to-compile)
					    (compile-address! (pop-int! cstack #f))
					    (fill-rest-with-nops)]
					   [(entry-primitive entry)
					    (add-compiled-code! to-compile)
					    (when (member to-compile instructions-using-entire-word)
						  (fill-rest-with-nops))]
					   [else
					    (let [(nxt (forth_read))]
; TODO: Check if address can fit.  For now, don't put jump/call in last slot.
; This is already taken care of by add-compiled-code!
					      (if (equal? nxt ";")
						  (add-compiled-code! "jump")
						  (begin (forth_read 'put-back nxt)
							 (add-compiled-code! "call")))
; Compile the address.  Automatically compiles #f into the rest of the word.
					      (compile-address! (entry-code entry)))])))))
	      (compile-loop))))))


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

; Debugging

(define (print-stack stack)
  (define (loop pos)
    (print (get-int stack #t pos))
    (display " ")
    (unless (= pos 0) (loop (sub1 pos))))
  (display "| ")
  (loop (sub1 (stack-length stack)))
  (display ">"))

(define (find-address d name)
  (define (loop address)
    (let [(word (rvector-ref d address))]
      (cond [(equal? name (entry-name word)) address]
            [(= address 0) #f]
            [else (loop (sub1 address))])))
  (let ((len (rvector-length d)))
    (if (= len 0)
	#f
	(loop (sub1 len)))))

(define (find-entry d name)
  (let [(address (find-address d name))]
    (if address
        (rvector-ref d address)
        #f)))

(define (lookup key records)
  (cond ((null? records) #f)
	((equal? key (caar records)) (cdar records))
	(else (lookup key (cdr records)))))
