#lang racket

(require "forth_base.rkt")

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
    (check-equal-files (open-input-file output) (open-input-file tmp))))

(run-test "basic1")
(run-test "basic2")
(run-test "basic3")
(pretty-display (compile-file "examples/basic3.aforth")) (newline)
(pretty-display (compile-to-string (open-input-string "yellow 3 node green 1 2 3 4 + +"))) (newline)
(pretty-display (compile-to-string (open-input-string "
0 a! 
277 b! @b !+
a push 0 a!
0 @+ + @+ + @+ + @+ +
2/ 2/ 325 b! !b pop a!
277 b! @b !+
a push 0 a!
0 @+ + @+ + @+ + @+ +
2/ 2/ 325 b! !b pop a!"))) (newline)
(pretty-display (compile-to-string (open-input-string "
dup or dup a! @+ +  @+ + @+ + @+ +"))) (newline)
(pretty-display (compile-to-string (open-input-string "
277 b! @b !+ a 1 and a! 
a push 0 a!
0 @+ + @+ +
2/ 325 b! !b pop a!
277 b! @b !+ a 1 and a! 
a push 0 a!
0 @+ + @+ +
2/ 325 b! !b pop a!
277 b! @b !+ a 1 and a! 
a push 0 a!
0 @+ + @+ +
2/ 325 b! !b pop a!
277 b! @b !+ a 1 and a! 
a push 0 a!
0 @+ + @+ +
2/ 325 b! !b pop a!")))

(pretty-display (compile-to-string (open-input-string "
. ; nop ret UP left")))
