#lang racket

(require "classes.rkt" "forth_read.rkt" "rvector.rkt")

(provide add-directives!)

;; Compiler directives

(define (valid-node? n)
  ;;TODO: fix
  (and (integer? n)
       (>= n 0)))


(define (coord->index n)
  (+ (* (quotient n 100) 18) (remainder n 100)))

(define (add-directives!)

  (add-directive!
   "node"
   (lambda (compiler)
     (let* ([token (forth_read)]
            [n (string->number token)])
       (unless (valid-node? n)
	       (raise (format "Err: invalid node number: ~a" token)))
       (send compiler set 'state-index (coord->index n))
       (unless (member (send compiler get 'state-index)
                       (send compiler get 'used-cores))
	       (send compiler set 'used-cores
		     (cons (send compiler get 'state-index)
			   (send compiler get 'used-cores))))
       (send compiler set 'memory (make-rvector 100 '()))
       (send compiler set 'location-counter 1)
       (send compiler set 'i-register 0))))

  (add-directive! ;;TODO: test
   "org"
   (lambda (compiler)
     (let* ([token (forth_read)]
            [n (string->number token)])
       (unless (and (integer? n)
                    (>= n 0))
	       ;;TODO: upper bound?
	       (raise (format "Err: invalid argument to 'org' directive: '~a'" n)))
       (send compiler set 'location-counter (add1 n))
       (send compiler set 'i-register (* 4 n)))))

  (add-directive!
   "yellow"
   (lambda (compiler)
                                        ;(send compiler set 'execute? #t)
     (raise "'yellow' directive is unsupported")))

  (add-directive!
   "green"
   (lambda (compiler)
     ;;(send compiler set 'execute? #f)
     (raise "'green' directive is unsupported")))

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

  ;; Custom addition to make it easy to specify where to start programs.
  (add-directive!
   "start"
   (lambda (compiler)
     (let [(i-register (send compiler get 'i-register))
	   (used-cores (send compiler get 'used-cores))
	   (state-index (send compiler get 'state-index))]
       (unless (= (remainder i-register 4) 0)
	       (raise "start directive is not at the beginning of a word - consider using .."))
       (send compiler set-pc! (quotient i-register 4))
       (unless (member state-index used-cores)
	       (send compiler set 'used-cores (cons state-index used-cores))))))

  ;; Comments
  (define (comment compiler)
    (unless (equal? (read-char) #\))
	    (comment compiler)))
  (add-directive! "(" comment)

  ;; ,
  (add-directive!
   ","
   (lambda (compiler)
     (let* ;; [(data (pop-cells! (send compiler get 'dstack)))]
	 ([token (forth_read)]
	  [data (string->number token)])
       (send compiler add-compiled-data! data))))

  ;; begin
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

  ;; next, when seen in the compiler
  (add-directive!
   "next"
   (lambda (compiler)
     (let [(addr (pop-int! (send compiler get 'dstack) #f))]
       (if (= addr (quotient (send compiler get 'i-register) 4))
	   (send compiler add-compiled-code! "unext")
	   (begin (send compiler add-compiled-code! "next")
		  (send compiler compile-address! addr))))))

  (define (make-if-directive instr)
    (lambda (compiler)
      (send compiler add-compiled-code! instr)
      ;; Since it cannot be in the last spot, there must be space for the address
      (push-int! (send compiler get 'dstack)
                 (send compiler get 'i-register))
      (send compiler go-to-next-word)))

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
   (lambda (compiler)
     (send compiler fill-rest-with-nops)
     (let [(old-ireg (pop-int! (send compiler get 'dstack) #f))
           (curr-ireg (send compiler get 'i-register))]
       (when (not (= (remainder curr-ireg 4) 0))
	     (raise "Internal error:  i-register should be at the start of a word, but is not"))
       (send compiler compile-address-to-slot! (/ curr-ireg 4) old-ireg))))

  )
