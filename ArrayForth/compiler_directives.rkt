#lang racket

(require "classes.rkt" "forth_read.rkt" "rvector.rkt")

(provide add-directives!)

; Compiler directives

(define (add-directives!)
  (add-directive!
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

  (add-directive!
   "org"
   (lambda (compiler)
     (send compiler set 'location-counter
	   (add1 (pop-int! (send compiler get 'dstack) #f)))
     (send compiler set 'i-register
	   (* 4 (sub1 (send compiler get 'location-counter))))))

  (add-directive!
   "yellow"
   (lambda (compiler) (send compiler set 'execute? #t)))

  (add-directive!
   "green"
   (lambda (compiler) (send compiler set 'execute? #f)))

  (add-directive!
   ":"
   (lambda (compiler)
     (send compiler fill-rest-with-nops)
     (send compiler add-word!
	   (forth_read_no_eof)
	   (quotient (send compiler get 'i-register) 4))))

  (add-directive!
   ".."
   (lambda (compiler) (send compiler fill-rest-with-nops)))

  ; Custom addition to make it easy to specify where to start programs.
  (add-directive!
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
  (add-directive! "(" comment)

  ; ,
  (add-directive!
   ","
   (lambda (compiler)
     (let [(data (pop-cells! (send compiler get 'dstack)))]
       (send compiler add-compiled-data! data))))

  ; begin
  (define (begin-proc compiler)
    (send compiler fill-rest-with-nops)
    (push-int! (send compiler get 'dstack)
	       (quotient (send compiler get 'i-register) 4)))
  (add-directive! "begin" begin-proc)
  
  (add-directive!
   "for"
   (lambda (compiler)
     (send compiler add-compiled-code! "push")
     (begin-proc compiler)))

  ; next, when seen in the compiler
  (add-directive!
   "next"
   (lambda (compiler)
     (let [(addr (pop-int! (send compiler get 'dstack) #f))]
       (if (= addr (quotient (send compiler get 'i-register) 4))
	   (send compiler add-compiled-code! "unext")
	   (begin (send compiler add-compiled-code! "next")
		  (send compiler compile-address! addr)))))))
