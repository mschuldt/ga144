#lang racket ;; -*- lexical-binding: t -*-

(require compatibility/defmacro
         "../src/interpreter.rkt"
	 "../src/compile.rkt"
         "../src/assemble.rkt"
         "../src/bootstream.rkt"
         "../src/stack.rkt"
         "../src/el.rkt")

(define chip (new-ga144 "host"))

;; compile and assemble code
(define compiled (aforth-compile (file->string "hmm_pinning_real-noopt3.aforth")))
(define assembled (assemble compiled))

;; load assembled code into chip
(send chip load assembled)

;; set breakpoint at word 'two' in node 305
(define node_305 (get-node chip 305))
                                        ;(send node_305 set-breakpoint "one")
(send node_305 set-breakpoint "two")

;; entering the cli is optional and must be enabled
(enter-cli-on-breakpoint t)

;; prints a representation of every nodes io state at each change
;(send chip show-io-changes t)

;; step* runs the interpreter until it exits or a breakpoint is reached
(step*)
(step*)
(step*)
(step*)
(step*)
(step*)

;(enter-cli)
