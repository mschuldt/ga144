#lang racket

(require (only-in racket [equal? equal-case-sensitive?]))
(require "arithmetic.rkt" "rvector.rkt" "forth_read.rkt" "forth_num_convert.rkt")
(require "forth_state.rkt" "forth_state_words.rkt" "forth_bit_words.rkt" "forth_io_words.rkt" "forth_control_words.rkt")
(provide compile-and-run compile-to-vector compile-to-string)

; Interpreter and associated procedures

(define (equal? a b)
  (if (and (string? a) (string? b))
      (string-ci=? a b)
      (equal-case-sensitive? a b)))

(define address-required-on-cstack '("next" "if" "-if"))
(define address-required '("jump" "call" "next" "if" "-if"))
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

(add-primitive-word! #f "ex" (lambda () (execute (rvector-ref dict (pop-int! dstack #f)))))

(add-primitive-word! #f "call"
		     (lambda ()
		       (push-int! rstack (inexact->exact (ceiling (/ pc 4))))
		       (set-pc! (entry-code (find-entry dict (rvector-ref memory pc))))))

(define used-cores '())

;;;;;;;;;; NEW COMPILER FOR ARRAYFORTH ;;;;;;;;;;;;;;

(define last-slot-instructions '(";" "unext" "@p" "!p" "+*" "+" "dup" "."))
(define instructions-preceded-by-nops '("+" "+*"))

; Compiles an instruction or constant.
; In the case of a constant, it implicitly adds @p.
; Deals with instructions-preceded-by-nops
; Deals with last-slot-instructions
; Invariant:  At the beginning and the end of each invocation:
; a. i-register is the index of the next slot (not word) in (the variable) memory to compile into
; b. location-counter is the F18A address which is the next available word.  The memory address is 4*location-counter.
(define (add-compiled-code! elmt)

  (define (standard-compile! thing)
    (rvector-set! memory i-register elmt)
    (if (= (remainder i-register 4) 3)
	(begin (set! i-register (* 4 location-counter))
	       (set! location-counter (add1 location-counter)))
	(set! i-register (add1 i-register))))

  (cond [(bytes? elmt)
	 (rvector-set! memory (* 4 location-counter) elmt)
	 (for [(i (in-range 1 4))]
	      (rvector-set! memory (+ (* 4 location-counter) i) #f))
	 (set! location-counter (add1 location-counter))
	 (add-compiled-code! "@p")]
	[(string? elmt)
	 (when (or (and (member elmt instructions-preceded-by-nops)
			(not (equal? (rvector-ref memory (sub1 i-register)) ".")))
		   (and (= (remainder i-register 4) 3)
			(not (member elmt last-slot-instructions))))
	       (add-compiled-code! "."))
	 (standard-compile! elmt)]
	[(number? elmt)
	 (when (not (member (rvector-ref memory (sub1 i-register)) address-required))
	       (raise "Tried to compile a number that was not an address --- add-compiled-code!"))
	 (standard-compile! elmt)]
	[else (raise "Unknown thing to compile --- add-compiled-code!")]))

(define named-numbers (map (lambda (x) 
			     (cons (car x) 
				   (integer->integer-bytes (cdr x) 4 #f)))
			   '(("up" . #x145) ("down" . #x115) ("left" . #x175) ("right" . #x1d5))))

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
  (define (compile-loop)
    (let [(to-compile (forth_read))]
      (unless (eof-object? to-compile)
	      (unless (eq? to-compile #\newline)
		      (let [(entry (find-entry dict to-compile))]
			(cond [(not entry)
			       (let [(directive (find-entry compiler-directives to-compile))]
				 (if directive
				     ((rvector-ref codespace (entry-code directive)))
				     (let [(num (or (lookup to-compile named-numbers) (string->bytes to-compile)))]
				       (if num
					   (if execute?
					       (push-cells! dstack num)
					       (add-compiled-code! num))
					   (raise (string-append to-compile " ?"))))))]
			      [execute?
			       ; Assume that it is not an instruction that requires an address as an argument
			       ((rvector-ref codespace (entry-code entry)))]
			      [(member to-compile address-required-on-cstack)
			       (when (= (remainder i-register 4) 3)
				     (fill-rest-with-nops))
			       (add-compiled-code! to-compile)
			       (add-compiled-code! (pop-int! cstack #f))
			       (fill-rest-with-nops)]
			      [(entry-primitive entry)
			       (add-compiled-code! to-compile)]
			      [else
			       (let [(nxt (forth_read))]
				 (if (equal? nxt ";")
				     (add-compiled-code! "jump")
				     (begin (forth_read 'put-back nxt)
					    (add-compiled-code! "call")))
				 (add-compiled-code! (entry-code entry))
				 (fill-rest-with-nops))] )))
	      (compile-loop))))
  (set! used-cores '())
  (let [(old (current-input-port))]
    (forth_read 'clear)
    (current-input-port code-port)
    (set! execute? #f)
    (compile-loop)
    (current-input-port old)))

; Compiles ArrayForth and runs it using this interpreter (not fully functional yet).
; Output is typically generated by .ns and .nr in the code
(define (compile-and-run code-port)
  (reset!)
  (compile code-port)
  (code-loop))

; Compiles ArrayForth into F18 code.  Outputs a vector of strings and bytes/numbers.
; If #:use-bytes? is set to #f (the default), the output will contain numbers, otherwise, output will contain numbers.
; If #:use-nop-and-ret? is set to #f, output will contain ";" and ".", else output will contain "nop" and "ret".
(define (compile-to-vector code-port #:bytes? [use-bytes? #f] #:use-nop-and-ret? [no-punct? #t])
  (reset!)

  ; Use node 0 for compilation
  (push-int! dstack 0)
  ((rvector-ref codespace (entry-code (find-entry compiler-directives "node"))))
  ((rvector-ref codespace (entry-code (find-entry compiler-directives "green"))))
  (compile code-port)

  ; Compilation stores everything into memory.
  (let ((result (plain-vector memory)))
    ; Convert bytes to integers if necessary
    (unless use-bytes?
	    (vector-map! (lambda (code)
			   (if (bytes? code)
			       (integer-bytes->integer code #t #t)
			       code))
			 result))

    ; Convert . and ; to nop and ret respectively, if necessary
    (let [(replace-nop (if no-punct? "nop" "."))
	  (find-nop (if no-punct? "." "nop"))
	  (replace-ret (if no-punct? "ret" ";"))
	  (find-ret (if no-punct? ";" "ret"))]
      (vector-map! (lambda (code)
		     (cond [(equal? code find-nop) replace-nop]
			   [(equal? code find-ret) replace-ret]
			   [else code]))
		   result))
    result))

(define (compile-to-string code-port #:use-nop-and-ret? [no-punct? #t])
  (define (convert s)
    (cond [(string? s) s]
	  [(number? s) (number->string s)]
	  [(not s) ""]
	  [else (raise "Unknown memory element")]))
  (foldr (lambda (x y) (string-append (convert x) " " y)) ""
	 (vector->list (compile-to-vector code-port #:bytes? #f #:use-nop-and-ret? no-punct?))))

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

(define (fill-rest-with-nops)
  (unless (= (remainder i-register 4) 0)
	  (add-compiled-code! ".")
	  (fill-rest-with-nops)))

; TODO: Does this have to be on a word boundary?  If not, then use i-register.
(add-compiler-directive! ":"
		     (lambda ()
		       (fill-rest-with-nops)
		       (add-entry! #f (forth_read_no_eof) location-counter)))

; Comments
(define (comment)
  (unless (equal? (read-char) #\))
	  (comment)))
(add-compiler-directive! "(" comment)


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

; ,
(add-compiler-directive! ","
                     (lambda ()
                       (let [(num (pop-int! dstack #t))]
                         (add-primitive-code! num))))

(define reset! (set-as-defaults!))

