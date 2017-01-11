#lang racket ;; -*- lexical-binding: t -*-

(require compatibility/defmacro
         "../interpreter/interpreter.rkt"
	 "../compiler/compile.rkt"
         "../compiler/assemble.rkt"
         "../compiler/bootstream.rkt"
         "../interpreter/stack.rkt")

(define chip (new-ga144 "host"))


(define loop-count 10)
(define code (format "node 705
, 444
, 555
, 666

: main

io b!
0x3ffff !b
1 3 add
4 double
~a fn
0 if
: add + ;
: double dup + ;
: inc 1 + ; 
: fn for inc next ;
then
" loop-count))

(define compiled (compile code))
(define assembled (assemble compiled))

(define node (get-node chip 705))

(send chip load assembled)
(send node set-breakpoint "add")
(send node set-breakpoint "double")
(define inc-counter 0)
(send node set-word-hook-fn
      "inc"
      (lambda () (set! inc-counter (add1 inc-counter))))

(enter-cli-on-breakpoint t)
(step*) ;;step to add
(step*) ;;step to double
(step*) ;;finish program

(unless (equal? inc-counter (add1 loop-count))
  (raise (format "check failed: ~a == ~a" inc-counter (add1 loop-count))))


