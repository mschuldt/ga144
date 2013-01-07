#lang racket

(require "compiler.rkt")

(define (displaynl arg)
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


(define (remove-return str)
  (if (equal? (string-ref str (sub1 (string-length str))) #\return)
      (substring str 0 (sub1 (string-length str)))
      str))

(define (check-equal-files a b [line 1])
  (let [(aline (read-line a))
        (bline (read-line b))]
    (cond [(and (eof-object? aline) (eof-object? bline))
           (displaynl "Test checked")]
          [(or (eof-object? aline) (eof-object? bline))
           (displaynl (string-append "Line " (number->string line)
                                   ": Encountered end of file for only 1 file"))]
          [(equal? (remove-return aline) (remove-return bline)) ; Returns appear only on Unix, so remove them.
           (check-equal-files a b (+ line 1))]
          [else
           (displaynl (string-append "Line " (number->string line)
                                   ": Did not match"))
           (check-equal-files a b (+ line 1))])))
  

(define (run-test name)
  (let [(file (string-append "examples/" (string-append name ".aforth")))
        (tmp (string-append "examples/out/" (string-append name ".tmp")))
        (output (string-append "examples/out/" (string-append name ".out")))]
    (exec-file file tmp)
    (displaynl (string-append ">>> " name))
    (system (string-append "diff -b " output " " tmp))
    (displaynl "Tests checked")
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
    (displaynl (string-append ">>> " name))
    (system (string-append "diff -b " output " " tmp))
    (displaynl "Tests checked")
    (newline)))

(for [(i (in-range 1 4))]
     (run-test (string-append "basic" (number->string i))))

(for [(i (in-range 1 4))]
     (run-compiler-test (string-append "small" (number->string i))))

(for [(i (in-range 1 3))]
     (run-compiler-test (string-append "large" (number->string i))))

(compile-to-string "
277 b! @b !+ a 3 and
push 0 a!
0 @+ + @+ + @+ + @+ +
2/ 2/ 325 b! !b pop a!")
