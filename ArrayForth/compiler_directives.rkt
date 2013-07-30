#lang racket

(require "classes.rkt" "forth_read.rkt" "rvector.rkt")

(provide add-compiler-directives!)

; Compiler directives

(define (add-compiler-directives!)
  (add-compiler-directive!
   "node"
   (lambda (compiler)
     (send compiler set 'state-index
	   (pop-int! (send compiler get 'dstack) #f))
     (unless (member (send compiler get 'state-index)
		     (send compiler get 'used-cores))
	     (send compiler set 'used-cores
		   (cons (send compiler get 'state-index)
			 (send compiler get 'used-cores))))
     (send compiler set 'memory (make-rvector 100 "."))
     (send compiler set 'location-counter 1)
     (send compiler set 'i-register 0)))

  (add-compiler-directive!
   "org"
   (lambda (compiler)
     (send compiler set 'location-counter
	   (add1 (pop-int! (send compiler get 'dstack) #f)))
     (send compiler set 'i-register
	   (* 4 (sub1 (send compiler get 'location-counter))))))

  (add-compiler-directive!
   "yellow"
   (lambda (compiler) (send compiler set 'execute? #t)))

  (add-compiler-directive!
   "green"
   (lambda (compiler) (send compiler set 'execute? #f)))

  (add-compiler-directive!
   ":"
   (lambda (compiler)
     (send compiler fill-rest-with-nops)
     (add-entry! #f (forth_read_no_eof)
		 (quotient (send compiler get 'i-register) 4))))

  (add-compiler-directive!
   ".."
   (lambda (compiler) (send compiler fill-rest-with-nops)))

  ; Custom addition to make it easy to specify where to start programs.
  (add-compiler-directive!
   "start"
   (lambda (compiler)
     (send compiler set-pc! (sub1 (send compiler get 'location-counter)))
     (let [(used-cores (send compiler get 'used-cores))
	   (state-index (send compiler get 'state-index))]
       (unless (member state-index used-cores)
	       (send compiler set 'used-cores (cons state-index used-cores))))))

  ; Comments
  (define (comment compiler)
    (unless (equal? (read-char) #\))
	    (comment compiler)))
  (add-compiler-directive! "(" comment)

  ; ,
  (add-compiler-directive!
   ","
   (lambda (compiler)
     (let [(data (pop-cells! (send compiler get 'dstack)))]
       (send compiler add-compiled-data! data))))

  ; begin
  (define (begin-proc compiler)
    (send compiler fill-rest-with-nops)
    (push-int! (send compiler get 'cstack)
	       (quotient (send compiler get 'i-register) 4)))
  (add-compiler-directive! "begin" begin-proc)
  
  (add-compiler-directive!
   "for"
   (lambda (compiler)
     (send compiler add-compiled-code! "push")
     (begin-proc compiler)))

  ; next, when seen in the compiler
  (add-compiler-directive!
   "next"
   (lambda (compiler)
     (let [(addr (pop-int! (send compiler get 'cstack) #f))]
       (if (= addr (quotient (send compiler get 'i-register) 4))
	   (send compiler add-compiled-code! "unext")
	   (begin (send compiler add-compiled-code! "next")
		  (send compiler compile-address! addr)))))))
  
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
