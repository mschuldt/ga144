#lang racket

(require (only-in racket [equal? equal-case-sensitive?]))
(require "arithmetic.rkt" "rvector.rkt" "forth_read.rkt"
	 "forth_num_convert.rkt" "forth_state.rkt" "forth_state_words.rkt"
	 "forth_bit_words.rkt" "forth_io_words.rkt" "forth_control_words.rkt")

(provide (all-defined-out))

; Interpreter and associated procedures

(define (equal? a b)
  (if (and (string? a) (string? b))
      (string-ci=? a b)
      (equal-case-sensitive? a b)))

(define address-required-on-cstack '("next" "if" "-if"))
(define address-required (append '("jump" "call") address-required-on-cstack))
; This code is ugly, because I don't know how to use Racket well.
(define (code-loop)
  (let [(ended-list
	 (map
	  (lambda (num)
	    (set! state-index num)
	    (if (or (< pc 0) (>= pc (rvector-length memory)))
		#t
		(let [(code (rvector-ref memory pc))]
		  (begin
		    (increment-pc!)
		    (cond [(and (string? code) (member code address-required))
			   (let [(addr (rvector-ref memory pc))]
			     (increment-pc!)
			     ((rvector-ref codespace (entry-code (find-entry dict code))) addr))]
			  [(string? code)
			   ((rvector-ref codespace (entry-code (find-entry dict code))))]
			  [(procedure? code)
			   (code)]
			  [else
			   (raise "Unknown type in memory -- code-loop")])
		    #f))))
	  used-cores))]
    (unless (for/and ([bool ended-list]) bool)
	    (code-loop))))

(define (execute-code addr)
  (push-int! rstack (inexact->exact (ceiling (/ pc 4)))) ; pc will be the address of the next instruction to execute
  (set-pc! addr))

(define execute (compose execute-code entry-code))

; Compiler directives

(define used-cores '())

(add-compiler-directive! "node"
		     (lambda ()
		       (set! state-index (pop-int! dstack #f))
		       (unless (member state-index used-cores)
			       (set! used-cores (cons state-index used-cores)))
		       (set! memory (make-rvector 100 "."))
		       (set! location-counter 1)
		       (set! i-register 0)))

(add-compiler-directive! "org"
		     (lambda ()
		       (set! location-counter (add1 (pop-int! dstack #f)))
		       (set! i-register (* 4 (sub1 location-counter)))))

(add-compiler-directive! "yellow"
		     (lambda () (set! execute? #t)))

(add-compiler-directive! "green"
		     (lambda () (set! execute? #f)))

(add-compiler-directive! ":"
		     (lambda ()
		       (fill-rest-with-nops)
		       (add-entry! #f (forth_read_no_eof) (quotient i-register 4))))

(add-compiler-directive! ".." (lambda () (fill-rest-with-nops)))

; Custom addition to make it easy to specify where to start programs.
(add-compiler-directive! "start"
		     (lambda ()
		       (set-pc! (sub1 location-counter))))

; Comments
(define (comment)
  (unless (equal? (read-char) #\))
	  (comment)))
(add-compiler-directive! "(" comment)

#|
(define (start-literal)
  (set! literal-mode 1)
  (set! lit-entry 0)
  (for* ([i (in-range 0 4)])
    (rvector-set! litspace i (lambda () (void)))))
(add-compiler-directive! "{" start-literal)

(define (stop-literal)
  (set! literal-mode 0)
  (let [(code0 (rvector-ref litspace 0)) 
        (code1 (rvector-ref litspace 1)) 
        (code2 (rvector-ref litspace 2)) 
        (code3 (rvector-ref litspace 3))]
    (add-primitive-code! (lambda () 
                        (code0) 
                        (code1)
                        (code2)
                        (code3)
                        ))))
(add-compiler-directive! "}" stop-literal)
|#

(define last-slot-instructions '(";" "unext" "@p" "!p" "+*" "+" "dup" "."))

(define instructions-preceded-by-nops '("+" "+*"))

; Compiler

; Compiles a single instruction or constant.
; In the case of a constant, it implicitly adds @p.
; Deals with instructions-preceded-by-nops
; Deals with last-slot-instructions
; Invariant:  At the beginning and the end of each invocation:
; a. i-register is the index of the next slot (not word) in (the variable) memory to compile into
; b. location-counter is the F18A address which is the next available word.  The memory address is 4*location-counter.
(define (add-compiled-code! elmt)

  ; Standard compilation - Put the thing into the slot given by i-register.
  ; Increment i-register if there are still remaining slots in the word.
  ; Otherwise, set i-register to address represented by location-counter, and increment location-counter.
  (define (standard-compile! thing)
    (rvector-set! memory i-register elmt)
    (if (= (remainder i-register 4) 3)
	(begin (set! i-register (* 4 location-counter))
	       (set! location-counter (add1 location-counter)))
	(set! i-register (add1 i-register))))

  (cond [(not elmt)
	 ; This slot should be taken up by something else, but I don't bother.
	 ; For example, numbers should take up all 4 slots, but they only take up 1.  Rest are #f.
	 ; Similar thing happens whenever an address is compiled (for address-required instructions)
	 (standard-compile! elmt)]
	
	[(bytes? elmt)
	 ; Number constant.  Need to compile @p and the number.
	 (rvector-set! memory (* 4 location-counter) elmt)
	 (for [(i (in-range 1 4))]
	      (rvector-set! memory (+ (* 4 location-counter) i) #f))
	 (set! location-counter (add1 location-counter))
	 (add-compiled-code! "@p")]
	
	[(string? elmt)
	 ; Standard instruction compilation.  Deals with inserting nops where necessary.
	 ; Addresses (for address-required instructions) must be supplied separately.
	 (when (or (and (member elmt instructions-preceded-by-nops)
                        (or (= i-register 0)
                            (not (equal? (rvector-ref memory (sub1 i-register)) "."))))
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
	
	[else (raise "Unknown thing to compile --- add-compiled-code!")]))

(define compile-address! add-compiled-code!)
(define compile-constant! add-compiled-code!)

(define (fill-rest-with-nops)
  (unless (= (remainder i-register 4) 0)
	  (add-compiled-code! ".")
	  (fill-rest-with-nops)))

(define (fill-rest-with-false)
  (unless (= (remainder i-register 4) 0)
	  (add-compiled-code! #f)
	  (fill-rest-with-false)))

; ,
(add-compiler-directive! ","
                     (lambda ()
                       (let [(num (pop-int! dstack #t))]
                         (compile-constant! num))))

; begin
(define (begin-proc)
  (fill-rest-with-nops)
  (push-int! cstack (quotient i-register 4)))
(add-compiler-directive! "begin" begin-proc)

(add-compiler-directive! "for"
			 (lambda ()
			   (add-compiled-code! "push")
			   (begin-proc)))

; next, when seen in the compiler
(add-compiler-directive! "next"
			 (lambda ()
			   (let [(addr (pop-int! cstack #f))]
			     (if (= addr (quotient i-register 4))
				 (add-compiled-code! "unext")
				 (begin (add-compiled-code! "next")
					(compile-address! addr))))))

;; Input:  Code in the form of an input port
;; If a word is an immediate word, it is executed instead.
;; To compile an immediate word, you need to postpone it.
;; Compiler directives are added in this section.
(define (compile code-port)
  (when (string? code-port)
	(set! code-port (open-input-string code-port)))
  
  (define (lookup key records)
    (cond ((null? records) #f)
	  ((equal? key (caar records)) (cdar records))
	  (else (lookup key (cdr records)))))
  
  (define (port->number str)
    (cond
      [(equal? str "up")    (int->bytes 325)]
      [(equal? str "down")  (int->bytes 277)]
      [(equal? str "left")  (int->bytes 373)]
      [(equal? str "right") (int->bytes 469)]
      [(equal? str "io")    (int->bytes 349)]
      [else #f]))
      
  (define (compile-loop)
    (let [(to-compile (forth_read))]
      (unless (eof-object? to-compile)
	      (unless (eq? to-compile #\newline)
		      (let [(directive (find-entry compiler-directives to-compile))]
			(if directive
			    ((rvector-ref codespace (entry-code directive)))
			    (let [(entry (find-entry dict to-compile))]
			      (cond [(not entry)
				     (let [(num (or (port->number to-compile) 
                                                    (string->bytes to-compile)))]
				       (if num
					   (if execute?
					       (push-cells! dstack num)
					       (compile-constant! num))
					   (raise (string-append to-compile " ?"))))]
				    [execute?
					; Assume that it is not an instruction that requires an address as an argument
				     ((rvector-ref codespace (entry-code entry)))]
				    [(member to-compile address-required-on-cstack)
				     (when (= (remainder i-register 4) 3)
					   (fill-rest-with-nops))
				     (add-compiled-code! to-compile)
				     (compile-address! (pop-int! cstack #f))
				     (fill-rest-with-nops)]
				    [(entry-primitive entry)
				     (add-compiled-code! to-compile)]
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
	      (compile-loop))))
  (set! used-cores '())
  (let [(old (current-input-port))]
    (forth_read 'clear)
    (current-input-port code-port)
    (set! execute? #f)
    (compile-loop)
    (fill-rest-with-nops)
    (current-input-port old)))

(define reset! (set-as-defaults!))
