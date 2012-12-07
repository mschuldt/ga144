#lang racket

(require "arithmetic.rkt" "rvector.rkt" "forth_read.rkt" "forth_num_convert.rkt")
(require "forth_state.rkt" "forth_state_words.rkt" "forth_bit_words.rkt" "forth_io_words.rkt" "forth_control_words.rkt")
(provide compile-and-run compile-to-vector compile-to-string)

; Interpreter and associated procedures

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
		    (cond ((string? code) ((rvector-ref codespace (entry-code (find-entry dict code)))))
			  ((procedure? code) (code))
			  (else (raise "Unknown type in memory -- code-loop")))
		    #f))))
	  used-cores))]
    (unless (for/and ([bool ended-list]) bool)
	    (code-loop))))

(define (execute-code addr)
  (push-int! rstack (inexact->exact (ceiling (/ pc 4)))) ; pc will be the address of the next instruction to execute
  (set-pc! addr))

(define execute (compose execute-code entry-code))

(add-primitive-word! #f ";" (lambda () (set-pc! (pop-int! rstack #f))))

(add-primitive-word! #f "ex" (lambda () (execute (rvector-ref dict (pop-int! dstack #f)))))

(add-primitive-word! #f "call"
		     (lambda ()
		       (push-int! rstack (inexact->exact (ceiling (/ pc 4))))
		       (set-pc! (entry-code (find-entry dict (rvector-ref memory pc))))))

(define used-cores '())

;;;;;;;;;; NEW COMPILER FOR ARRAYFORTH ;;;;;;;;;;;;;;

(define last-slot-instructions '(";" "unext" "@p" "!p" "+*" "+" "dup" "."))
(define instructions-preceded-by-nops '("+" "+*"))

(define (add-compiled-code! elmt)
  (cond [(bytes? elmt)
	 (rvector-set! memory (* 4 location-counter) elmt)
	 (for [(i (in-range 1 4))]
	      (rvector-set! memory (+ (* 4 location-counter) i) #f))
	 (set! location-counter (add1 location-counter))
	 (add-compiled-code! "@p")]
	[(string? elmt)
	 (when (or (and (memq elmt instructions-preceded-by-nops)
			(not (equal? (rvector-ref memory (sub1 i-register)) ".")))
		   (and (= (remainder i-register 4) 3)
			(not (memq elmt last-slot-instructions))))
	       (add-compiled-code! "."))
	 (rvector-set! memory i-register elmt)
	 (if (= (remainder i-register 4) 3)
	     (begin (set! i-register (* 4 location-counter))
		    (set! location-counter (add1 location-counter)))
	     (set! i-register (add1 i-register)))]
	[else (raise "Unknown thing to compile --- add-compiled-code!")]))

;; Input:  Code in the form of an input port
;; If a word is an immediate word, it is executed instead.
;; To compile an immediate word, you need to postpone it.
;; Compiler directives are added in this section.
(define (compile code-port)
  (define (compile-loop)
    (let [(to-compile (forth_read))]
      (unless (eof-object? to-compile)
	      (unless (eq? to-compile #\newline)
		      (let [(entry (find-entry dict to-compile))]
			(cond [(not entry)
			       (let [(directive (find-entry compiler-directives to-compile))]
				 (if directive
				     ((rvector-ref codespace (entry-code directive)))
				     (let [(num (string->bytes to-compile))]
				       (if num
					   (if execute?
					       (push-cells! dstack num)
					       (add-compiled-code! num))
					   (raise (string-append to-compile " ?"))))))]
			      [execute?
			       ((rvector-ref codespace (entry-code entry)))]
			      [(entry-primitive entry)
			       (add-compiled-code! to-compile)]
			      [else
			       (add-compiled-code! "call")
			       (add-compiled-code! to-compile)] )))
	      (compile-loop))))
  (set! used-cores '())
  (for ([i (in-range num-cores)])
       (set! state-index i))
  (let [(old (current-input-port))]
    (current-input-port code-port)
    (set! execute? #f)
    (compile-loop)
    (current-input-port old)))

(define (compile-and-run code-port)
  (reset!)
  (compile code-port)
  (code-loop))

(define (compile-to-vector code-port #:str? [str? #t] #:bytes? [use-bytes? #f])
  (reset!)
  (push-int! dstack 0)
  ((rvector-ref codespace (entry-code (find-entry compiler-directives "node"))))
  ((rvector-ref codespace (entry-code (find-entry compiler-directives "green"))))
  (compile code-port)
  (let ((result (plain-vector memory)))
    (unless use-bytes?
	    (vector-map! (lambda (code)
			   (if (bytes? code)
			       (integer-bytes->integer code #t #t)
			       code))
			 result))
    result))

(define (compile-to-string code-port #:str? [str? #t])
  (define (convert s)
    (cond [(string? s) s]
	  [(number? s) (number->string s)]
	  [(not s) ""]
	  [else (raise "Unknown memory element")]))
  (foldr (lambda (x y) (string-append (convert x) " " y)) ""
	 (vector->list (compile-to-vector code-port #:str? str? #:bytes? #f))))

(add-compiler-directive! "node"
		     (lambda ()
		       (set! state-index (pop-int! dstack #f))
		       (unless (member state-index used-cores)
			       (set! used-cores (cons state-index used-cores)))
		       (set! memory (make-rvector 100 -1))
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
  (define (loop)
    (if (= (remainder i-register 4) 0)
	(begin (set! i-register (* 4 location-counter))
	       (set! location-counter (add1 location-counter)))
	(begin (rvector-set! memory i-register ".")
	       (set! i-register (add1 i-register))
	       (loop))))
  (unless (= (remainder i-register 4) 0)
	  (loop)))

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

