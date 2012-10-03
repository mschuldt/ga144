#lang racket

(require "forth_base.rkt")

(define (displaynl arg)
  (display arg)
  (newline))

(define (exec-file file output)
  (let [(old_in (current-input-port))
        (old_out (current-output-port))]
    (current-input-port (open-input-file file))
    (current-output-port (open-output-file output #:exists 'truncate))
    (interpret)
    (close-input-port (current-input-port))
    (close-output-port (current-output-port))
    (current-input-port old_in)
    (current-output-port old_out)))

(define (check-equal-files a b [line 1])
  (let [(aline (read-line a))
        (bline (read-line b))]
    (cond [(and (eof-object? aline) (eof-object? bline))
           (displaynl "Tests checked")]
          [(or (eof-object? aline) (eof-object? bline))
           (displaynl (string-append "Line " (number->string line)
                                   ": Encountered end of file for only 1 file"))]
          [(equal? aline bline)
           (check-equal-files a b (+ line 1))]
          [else
           (displaynl (string-append "Line " (number->string line)
                                   ": Did not match"))
           (check-equal-files a b (+ line 1))])))
  
(define (run-test name)
  (let [(file (string-append "examples/" (string-append name ".forth")))
        (tmp (string-append "examples/out/" (string-append name ".tmp")))
        (output (string-append "examples/out/" (string-append name ".out")))]
    (exec-file file tmp)
    (displaynl (string-append ">>> " name))
    (check-equal-files (open-input-file output) (open-input-file tmp))))

(run-test "control")
(run-test "multiply")
(run-test "literal")

;(let [(out (open-input-file "forth.out"))
;      (tmp (open-input-file "forth.tmp"))]
;  (check-equal-files out tmp))