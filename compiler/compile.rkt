#lang racket

(require "arrayforth.rkt")
(require "assembler.rkt")
(require racket/cmdline)
(require profile)

(provide compile-string compile-file assemble-all)


(define file "test.aforth")

(define (run-file file)
  (call-with-input-file file compile-and-run))


;;TODO: this will all need to be fixed after the incorrect node numbering
;;      in the compiler is fixed

(define (index->coord n)
  (+ (* (quotient n 18) 100) (remainder n 18)))

(define (compile-string str)
  "compile STR to f18 assembly
return format: ((node-number . code-array) ...)"
  (let ((cores (compile-all-to-list str))
        (i 0)
        (empty (vector))
        (ret '()))

    (for [(core cores)]
      (unless (equal? core empty)
        (set! ret (cons (cons (index->coord i) core) ret)))
      (set! i (add1 i)))
    ret))

(define (compile-file file)
  (call-with-input-file file compile-string))


(define (assemble-all code-list)
  (let ([ret '()])
    (for [(core code-list)]
      (set! ret (cons (cons (car core)
                            (assemble (vector->list (cdr core))))
                      ret))
      (set code-list (cdr code-list)))
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

;;(define x (compile-to-list file))
;;(print x) (newline)
;;(define xx (assemble-all x))
;;(print "__________assembled____________")(newline)
;;(print xx) (newline)


;(newline)
;(print "-----------------------------------------")
