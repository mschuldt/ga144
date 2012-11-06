#lang racket

(require "arithmetic.rkt" "rvector.rkt" "forth_read.rkt" "forth_num_convert.rkt")
(require "forth_state.rkt" "forth_state_words.rkt" "forth_bit_words.rkt" "forth_io_words.rkt" "forth_control_words.rkt")
(provide compile-and-run)

; Interpreter and associated procedures

; This code is ugly, because I don't know how to use Racket well.
(define (code-loop)
  (let [(ended-list
	 (map
	  (lambda (num)
	    (set! state-index num)
	    (let [(code (rvector-ref ram pc))]
	      (if (equal? code -1)
		  #t
		  (begin
		    (set! pc (add1 pc))
		    (if (number? code)
			(execute-code code)
			(code))
		    #f))))
	  used-cores))]
    (unless (for/and ([bool ended-list]) bool)
	    (code-loop))))

(define (execute-code addr)
  (push-int! rstack pc) ; pc will be the address of the next instruction to execute
  (set! pc addr))

(define execute (compose execute-code entry-code))

(add-primitive-word! #f "ex" (lambda () (execute (rvector-ref dict (pop-int! dstack #f)))))

(define (reset-states!)
  (for [(i (in-range 0 num-cores))]
       (set-state-dstack! (vector-ref cores i) (make-dstack))
       (set-state-rstack! (vector-ref cores i) (make-rstack))
       (set-state-pc! (vector-ref cores i) 0)))

(define used-cores '())

;;;;;;;;;; NEW COMPILER FOR ARRAYFORTH ;;;;;;;;;;;;;;

(define (add-compiled-code! flag code)
  (if flag
      (add-primitive-code! code)
      (begin (rvector-set! ram location-counter code)
	     (set! location-counter (add1 location-counter)))))

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
					;		      (when entry (display (entry-name entry)) (display " ") (display (entry-precedence entry)) (newline))
			(cond [(not entry)
			       (let [(directive (find-entry compiler-directives to-compile))]
				 ;(display to-compile) (display ": ") (display directive) (newline)
				 (if directive
				     ((rvector-ref codespace (entry-code directive)))
				     (let [(num (string->bytes to-compile))]
				       (if num
					   (if execute?
					       (push-cells! dstack num)
					       (add-compiled-code! defining? (lambda () (push-cells! dstack num))))
					;				   (begin
					;				     (add-compiled-code! defining? (rvector-ref codespace (entry-code (find-entry "@p"))))
					;				     (add-compiled-code! defining? num))
					   (raise (string-append to-compile " ?"))))))]
			      [execute?
			       ((rvector-ref codespace (entry-code entry)))]
			      [(entry-primitive entry)
			       (add-compiled-code! defining? (rvector-ref codespace (entry-code entry)))]
			      [else
			       ;(unless defining? (print to-compile) (print (entry-code entry)) (newline))
			       (add-compiled-code! defining? (entry-code entry))] )))
	      (compile-loop))))
  (set! used-cores '())
  (for ([i (in-range num-cores)])
       (set! state-index i)
       (rvector-copy! rom 0 dict))
  (let [(old (current-input-port))]
    (current-input-port code-port)
    (set! execute? #f)
    (compile-loop)
    (current-input-port old)))

(define (compile-and-run code-port)
  (reset-states!)
  (compile code-port)
  #|(for [(i used-cores)]
       (set! state-index i)
       (print (mcar ram)) (newline))|#
  (code-loop))

;; TODO:  Clear node's codespace
(add-compiler-directive! "node"
		     (lambda ()
		       (set! state-index (pop-int! dstack #f))
		       (unless (member state-index used-cores)
			       (set! used-cores (cons state-index used-cores)))
		       (set! location-counter 0)
		       (rvector-copy! rom 0 dict)))

(add-compiler-directive! "org"
		     (lambda ()
		       (set! location-counter (pop-int! dstack #f))))

(add-compiler-directive! "yellow"
		     (lambda () (set! execute? #t)))

(add-compiler-directive! "green"
		     (lambda () (set! execute? #f)))

(add-compiler-directive! ":"
		     (lambda ()
		       (add-entry! #f (forth_read_no_eof) location-counter)
		       (set! defining? #t)))

(add-compiler-directive! ";"
		     (lambda ()
		       (add-compiled-code! #t exit-addr)
		       (set! defining? #f)))
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

