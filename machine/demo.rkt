#lang racket

(require "cegis.rkt" "state.rkt")


;;; swap only at m is 1 xym -> - y' (first part)
(fastest-program3 "a! over over nop a - and nop push a and nop pop over over nop or push and nop pop or nop nop" #:name "swap" #:num-bits 4 #:inst-pool `no-mem-no-p)

;;; x - (x & y)
;; (fastest-program3 "over and - @p 1 nop + nop +" #:slots 8 #:constraint (constraint t))

;;; x | y
;; (fastest-program3 "over over or nop a! and a nop or nop nop nop" #:constraint (constraint t) #:num-bits 4)
;; ("over - and nop +" . 15)

;;; round up to multiple of 8 (8-bit)
;(fastest-program3 "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:name "roundup" #:constraint (constraint t) #:num-bits 8 #:inst-pool `no-mem)

;;; some sketch examples here

