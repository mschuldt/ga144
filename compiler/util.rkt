#lang racket

(require compatibility/defmacro)

(provide (all-defined-out))

(defmacro setq (var val)
  `(let [(__v__ ,val)]
     (set! ,var __v__) __v__))

(defmacro enum (syms)
  (let ((i 0)
        (code '())
        (sym #f))
    (for ([sym syms])
      (set! code (cons (list 'define sym i) code))
      (set! i (add1 i)))
    (cons 'begin code)))

(defmacro push (list item)
  `(set! ,list (cons ,item ,list)))

(defmacro pop (list)
  `(if (equal? ,list '())
       (pretty-display "ERROR: pop -- list is empty")
       (begin0 (car ,list) (set! ,list (cdr ,list)))))

(defmacro swap (list)
  `(set! ,list (cons (cadr ,list) (cons (car ,list) (cddr ,list)))))

(define (coord->index n)
  (+ (* (quotient n 100) 18) (remainder n 100)))

(define (index->coord n)
  (+ (* (quotient n 18) 100) (remainder n 18)))

(define (convert-direction coord dir)
  ;;converts DIR={North, East, South, West} Into Left, Up, Down, or Right
  ;;depending on the nodes coordinate COORD
  (let ([x (remainder coord 100)]
        [y (quotient coord 100)])
    (cond
     [(equal? dir "north")
      (if (= (modulo y 2) 0) "down" "up")]
     [(equal? dir "south")
      (if (= (modulo y 2) 0) "up" "down")]
     [(equal? dir "east")
      (if (= (modulo x 2) 0) "right" "left")]
     [(equal? dir "west")
      (if (= (modulo x 2) 0) "left" "right")]
     [else (raise "convert-direction: invalid direction")])))
