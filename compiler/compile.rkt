#lang racket

(require "arrayforth.rkt")
(require "assembler.rkt")
(require racket/cmdline)

(require profile)

(define file "test.aforth")

(define (run-file file)
  (call-with-input-file file compile-and-run))

(define (to-list file [as-array #f])
  (call-with-input-file file compile-all-to-list))

;;TODO: this will all need to be fixed after the incorrect node numbering
;;      in the compiler is fixed

(define (compile-to-list file)
  "compile FILE to f18 assembly
return format: ((node-number . code-array) ...)"
  (let [(cores (to-list file))
        (i 0)
        (empty (vector))
        (ret '())]
    (for [(core cores)]
      (unless (equal? core empty)
        (set! ret (cons (cons i core) ret)))
      (set! i (add1 i)))
    ret))

(define (print-mem-list cores)
  (let [(i 0)
        (empty (vector))]
    (for [(core cores)]
      (unless (equal? core empty)
        (newline)
        (display (format "{node ~a}" i))
        (newline)
        (print-vector core))
      (set! i (add1 i)))))

(define (print-vector v)
  (define (print-vector-1 v i len)
    (unless (>= i len)
      (begin (print (vector-ref v i))
             (newline)
             (print-vector-1 v (add1 i) len))))
  (print-vector-1 v 0 (vector-length v)))





;;(print (compile-to-vector file))
;;(print-mem-list (to-list file))
;;(profile-thunk (thunk (print-mem-list (to-list file))))


;;(print (call-with-input-file file compile-to-string)) (newline)

(print (compile-to-list file)) (newline)
;(newline)
;(print "-----------------------------------------")
