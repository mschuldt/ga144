#lang racket

(require "arrayforth.rkt")

(define (displayln arg)
  (display arg)
  (newline))

(define (exec-file file output)
  (call-with-input-file file
    (lambda (in)
      (call-with-output-file output
	#:exists 'truncate
	(lambda (out)
	  (let ((old (current-output-port)))
	    (current-output-port out)
	    (compile-and-run in)
	    (current-output-port old)))))))

(define (compile-file file #:str? [str? #t])
  (call-with-input-file file
    (lambda (in)
      (if str?
	  (compile-to-string in)
	  (compile-to-vector in)))))

(define (run-test name)
  (let [(file (string-append "examples/" (string-append name ".aforth")))
        (tmp (string-append "examples/out/" (string-append name ".tmp")))
        (output (string-append "examples/out/" (string-append name ".out")))]
    (displayln (string-append ">>> " name))
    (exec-file file tmp)
    (system (string-append "diff -b " output " " tmp))
    (displayln "Tests checked")
    (newline)))

(define (run-compiler-test name)
  (let [(file (string-append "examples/" (string-append name ".ctest")))
        (tmp (string-append "examples/out/" (string-append name ".tmp")))
        (output (string-append "examples/out/" (string-append name ".out")))]
    (call-with-output-file tmp
      #:exists 'truncate
      (lambda (out)
	(parameterize ([current-output-port out])
		      (pretty-display (compile-file file)))))
    (displayln (string-append ">>> " name))
    (system (string-append "diff -b " output " " tmp))
    (displayln "Tests checked")
    (newline)))

(for [(i (in-range 1 4))]
     (run-test (string-append "basic" (number->string i))))

(for [(i (in-range 1 4))]
     (run-compiler-test (string-append "small" (number->string i))))

(for [(i (in-range 1 3))]
     (run-compiler-test (string-append "large" (number->string i))))

#|
(define test-str "yellow 2 node
10 org
green
: sum 1 0 b! !b 2 1 b! !b 0 a! @+ @+ ;
.. start sum .ns 0 0 .mem")|#

(define test-str "
yellow 0 node
6 org green
: sum 
  0 a! !+ !+ !+ !+ 
  3 a! @ 2 a! @ . + 1 a! @ . + 0 a! @ . + ; 
: main 
  4 a! @ 5 a! @ right a! @ right a! @ right a! @ 
  5 a! @ 4 a! @ sum sum 
  dup down a! ! ; 
.. start main .ns 0 6 .mem

yellow 1 node
2 org green
: main 
  0 a! @ right a! ! 1 a! @ right a! ! 
  0 a! @ right a! ! ; 
.. start main .ns 0 2 .mem

yellow 100 node
1 org green
: main 
  down a! @ 0 a! ! ; 
.. start main .ns 0 1 .mem")

(compile-to-string test-str)
(compile-and-run test-str)

;(exec-file "examples/test.forth" "examples/test.out")
