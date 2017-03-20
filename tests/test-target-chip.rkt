#lang racket ;; -*- lexical-binding: t -*-

;; tests loading a program with into the target chip through node 300
;; using the 2-wire connection.
;; [[ currently requires changes in function load-bootstream in f18a.rkt to load
;; bootstream from 708 instead of 300 ]]
;; The normal async bootstream is used to load code into the host chip that
;; carries teh bootstream from node 708 to 300 where it is sent
;; The normal async bootstream is loaded into the host chip, that loads code
;; that carries the sync bootstream from node 708 to 300 where it is sent
;; over 2-wire to the target chip, loading code for the target chip.

(require compatibility/defmacro
         "../src/common.rkt"
         "../src/interpreter.rkt"
	 "../src/compile.rkt"
         "../src/assemble.rkt"
         "../src/bootstream.rkt"
         "../src/stack.rkt"
         "../src/el.rkt")

(define host (new-ga144 "host"))
(define target (new-ga144 "target"))

(define code "
node 708
11 22 +
")

(define assembled (assemble (aforth-compile code)))
;;(define bootstream (make-sync-bootstream (compiled-nodes assembled)))
(define bs (make-bootstream assembled "async-target"))
(connect-pins (get-node host 300) 0
              (get-node target 300) 0)
(connect-pins (get-node host 300) 1
              (get-node target 300) 1)

;;(enter-cli)
(step*)
(send host load-bootstream bs)
;;(enter-cli)

(step*)

(define node708 (send target coord->node 708))
(define memory (send node708 get-memory))
(assert (= (vector-ref memory 1) 11))
(assert (= (vector-ref memory 2) 22))

(define dstack (send node708 get-dstack-as-list))
(assert (= (car dstack) 33))
(assert (= (cadr dstack) #x15555))
