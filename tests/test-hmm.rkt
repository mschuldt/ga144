#lang racket

(require compatibility/defmacro
         "../interpreter/interpreter.rkt"
	 "../compiler/compile.rkt"
         "../compiler/assemble.rkt"
         "../compiler/bootstream.rkt"
         "../interpreter/stack.rkt")

(define chip (new-ga144 "host"))

(define code (file->string "hmm_pinning_real-noopt3.aforth"))

(define compiled (compile code))
(define assembled (assemble compiled))

(send chip load assembled)

;(define node_205 (get-node chip 205))
;(send node_205 set-breakpoint "derive_group")
;(send node_205 set-breakpoint "x")


(define node_305 (get-node chip 305))
;(send node_305 set-breakpoint "one")
(send node_305 set-breakpoint "two")


(enter-cli-on-breakpoint #t)
;;(send chip show-io-changes #t)


(step*)
(step*)
(step*)
(step*)
(step*)
(step*)

;(enter-cli)

