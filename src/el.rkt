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

(defmacro push (item list)
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

(define _char-hash #\#)
(define _char-0 #\0)
(define _char-x #\x)
(define _char-b #\b)
(define _char-close-paren #\))
(define _char-newline  #\newline)
(define _char-& #\&)
(define _char-space #\ )

(define (_def syms) 0)

(define make-set set)

;;;;;;;;;;;
;;; need to define these to something for the compiler. these functions are not called when code is executed as racket
(define aforth-token-value true)
(define aforth-token-start true)
(define aforth-token-end true)
(define aforth-token-type true)
(define subseq true)
(define (funcall fn) (fn))
(define (funcall1 fn a) (fn a))
(define (funcall2 fn a b) (fn a b))
(define require true)
(define with-temp-buffer true)
(define insert true)
(define write-file true)
(define message printf)
(define concat true)
(define unread-last-tok true)
(define aforth-print-error-data true)
(define error-data-p true)
(define throw true)
(define aforth-error-message true)
(define most-positive-fixnum false)
(define princ false)

(define rkt-format format)
(define token-start false)
(define token-end false)
(define intern false)
(define mapc false)
(define gethash false)

