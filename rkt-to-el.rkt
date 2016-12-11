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
