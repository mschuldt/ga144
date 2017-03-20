#lang racket  ;; -*- lexical-binding: t -*-
;; convert output of dump-rom.py

(require "rkt-to-el.rkt")

(define file "raw-rom-dump.txt")

(define lines (file->lines file))
(define (next)
  (let ((line (car lines)))
    (set! lines (cdr lines))
    line))

(with-output-to-file "rom-dump.rkt"
  (lambda ()
    (printf "#lang racket ;; -*- lexical-binding: t -*-\n(provide (all-defined-out))\n")
    (printf "(define ROM-DUMP (list\n")
    (for/list ((node (range 144)))
      (print (for/list ((word (range 65))) (string->number (next))))
      (printf "\n"))
    (printf "))")))
