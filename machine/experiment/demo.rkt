#lang racket

(require "../cegis.rkt" "../state.rkt")

;; >>> 0x3ffff = 262143
;; >>> 0x1ffff = 131071
;; >>> 0xffff = 65535
;; >>> 0x7fff = 32767
;; >>> 0x3fff = 16383
;; >>> 0x1fff = 8191

;;; swap only at m is 1 xym -> - y' (first part)
;; (fastest-program3 "a! over over nop a - and nop push a and nop pop over over nop or push and nop pop or nop nop" #:name "swap" #:num-bits 4 #:inst-pool `no-mem-no-p)

;;; x - (x & y)
;(optimize "over and - @p 1 . + . +" #:slots 8 #:constraint (constraint t))

;;; x | y
;(optimize "over over or nop a! and a nop or nop nop nop" #:constraint (constraint t) #:num-bits 4)
; ("over - and nop +" . 15)

;;; round up to multiple of 8 (8-bit)
;(optimize "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:name "roundup" #:constraint (constraint t) #:num-bits 8 #:inst-pool `no-mem)

;; communi;cation
;;(optimize "@p b! !b . 325 @p b! !b . 325" 
;;          #:constraint constraint-none #:num-bits 9 #:name "comm")

; <= 10
;(optimize "@p a! @ @p 469    40    a! @ @p nop 0    nop + nop nop"
;          #:constraint (constraint memory s t) #:num-bits 9 #:name "try" #:mem 41)
; <= 8
;(optimize "@p a! @ @p 469    40    a! @ @p nop 0"
;          #:constraint (constraint memory s t data) #:num-bits 9 #:name "try" #:mem 41)

; timeout: 1200
(optimize "left a! ! 2 a! @ 65535 and 2 a! ! 2 a! @"
          #:constraint (constraint memory t) #:num-bits 17 #:name "debug"
          #:mem 6 #:f18a #f)

;(optimize "left a! ! 2 a! @ 7 and 2 a! ! 2 a! @"
;          #:constraint (constraint memory t) #:num-bits 4 #:name "debug"
;          #:mem 6 #:f18a #f)

#|
(optimize 
 "@p a! @ @p 349    
0    
a! ! @p nop 349    
a! @ @p nop 1    
a! ! @p nop 349    
a! @ @p nop 2    
a! ! @p nop 2    
a! @ @p nop 325    
a! ! nop nop"
 #:slots 
 "349 b! @b 0 
a! ! @b 1 
a! !+ @b .
! @ 325 .
a! !"
 #:mem 3
 #:constraint (constraint memory) #:num-bits 9 #:name "hi1")|#


