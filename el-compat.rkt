#lang racket

(require compatibility/defmacro)

(provide (all-defined-out))

(defmacro assert (x)
  `(unless ,x
     (error ,(format "Assertion failed: ~a" x))))

(defmacro el-require (feature)
  #f
  )

(defmacro defvar (symbol initval)
  `(define ,symbol ,initval)
  )

(defmacro defconst (symbol initval)
  `(define ,symbol ,initval)
  )


(defmacro setq (var val)
  `(let [(__v__ ,val)]
     (set! ,var __v__) __v__))

(defmacro while (condition code)
  `(letrec ((fn (lambda ()
                  (when ,condition
                    ,code
                    (fn)))))
     (fn)))

(defmacro push (list item)
  `(set! ,list (cons ,item ,list)))

(defmacro pop (list)
  `(if (equal? ,list '())
       (pretty-display "ERROR: pop -- list is empty")
       (begin0 (car ,list) (set! ,list (cdr ,list)))))

(defmacro test-require (x)
  x)


;; (define rkt-require require) ;;ERROR: not at module level or top level
;; (defmacro rkt-provide (files) ;;TODO: syntax for many args
;;   `(provide ,@
;;
(define t #t)
(define nil #f)

(define (fn x) x)

(define elisp? #f)
(define racket? #t)
