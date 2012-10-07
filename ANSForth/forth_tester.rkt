#lang racket

(require "forth_base.rkt")

(define (displaynl arg)
  (display arg)
  (newline))

(call-with-output-file "forth.tmp" #:exists 'truncate
  (lambda (out) (run-file "forth.test" out)))
(displaynl "Tests executed")

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

(let [(out (open-input-file "forth.out"))
      (tmp (open-input-file "forth.tmp"))]
  (check-equal-files out tmp))