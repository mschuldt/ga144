#lang racket

(require compatibility/defmacro
         "../interpreter/interpreter.rkt"
	 "../compiler/compile.rkt"
         "../compiler/assemble.rkt"
         "../compiler/bootstream.rkt"
         "../interpreter/stack.rkt")

(define chip (new-ga144 "host"))

;; compile and assemble code
(define compiled (compile (file->string "hmm_pinning_real-noopt3.aforth")))
(define assembled (assemble compiled))

;; load assembled code into chip
(send chip load assembled)

;; set breakpoint at word 'two' in node 305
(define node_305 (get-node chip 305))
                                        ;(send node_305 set-breakpoint "one")
(send node_305 set-breakpoint "two")

;; entering the cli is optional and must be enabled
(enter-cli-on-breakpoint #t)

;; prints a representation of every nodes io state at each change
;(send chip show-io-changes #t)

;; step* runs the interpreter until it exits or a breakpoint is reached
(step*)
(step*)
(step*)
(step*)
(step*)
(step*)

;(enter-cli)