#lang racket ;; -*- lexical-binding: t -*-

;; Generates code and a bootstream that dumps ROM from every node.

(require "compile.rkt"
         "bootstream.rkt"
         "assemble.rkt"
         "common.rkt"
         "el.rkt")

(define start (bootstream-start async-bootstream))
(define path (bootstream-path async-bootstream))
;;(define path (take (bootstream-path async-bootstream) 2))

(define prev-dir false)

(define ports '())
(define coords '())
(define coord-changes (vector 100 1 -100 -1)) ;; N, E, S, W coordinate changes
(define coord (+ start (vector-ref coord-changes (car path))))
(set! ports '(469))

(for ((dir (cdr path)))
  (when dir
    (set! ports (cons (get-direction coord dir) ports))
    (set! coords (cons coord coords))
    (set! coord (+ coord (vector-ref coord-changes dir)))))

(set! coords (cons coord coords))

(define code (string-join (for/list ((to ports)
                                     (coord coords)
                                     (from (cons false ports))
                                     (i (range 144)))
                            (string-join
                             (list (rkt-format "node ~a" coord)
                                   (rkt-format "~a b! " (port-name to))
                                   ;; pump rom from other nodes through this one
                                   (if from
                                       (rkt-format "~a a!\n ~a for @ !b unext"
                                                   (port-name from) (sub1 (* i 65)))
                                       "")
                                   ;; send this nodes ID and its ROM
                                   "0x80 a!"
                                   (rkt-format "~a !b" coord)
                                   "63 for @+ !b unext warm") "\n"))
                          "\n"))

(set! code (string-append code
                          "\nnode 708
: emit1 ( n )
    1 and 3 or !b
    865 for unext ;
: emit8 ( n - n )
    0 emit1
    7 for dup emit1 2/ next
    1 emit1 ;
: emit18 ( n - n )
    0xa5 emit8 drop
    emit8 emit8 emit8 ;
: main
    io b! right a!
    9294 for @ emit18 drop next
    708 emit18
    0x80 a!
    63 for @+ emit18 next
    2375 emit18
    warm
"))

;;(printf code)

(define c (aforth-compile code))
(define assembled (assemble c))

(define (pad-print thing [pad 20])
  (let* ((s (rkt-format "~a" thing))
         (len (string-length s))
         (str (string-append s (make-string (- pad len) #\ ))))
    (printf str)))

(printf "[~a]\n" (comma-join (sget-convert (make-async-bootstream assembled))))
