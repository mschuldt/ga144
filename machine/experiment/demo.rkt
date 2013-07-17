#lang racket

(require "../cegis.rkt" "../state.rkt" "../greensyn.rkt" "../programs.rkt")

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

;; communication
;;(optimize "@p b! !b . 325 @p b! !b . 325" 
;;          #:constraint constraint-none #:num-bits 9 #:name "comm")

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

(define (verify spec candidate mem comm-length constraint num-bits)
  (greensyn-reset mem comm-length constraint #:num-bits num-bits)
  (greensyn-spec spec)
  (greensyn-verify "me.smt2" candidate)
  )

#|(verify "6 a! @ 2   - 1 nop +   nop + 4 nop   + a! @ 6    a! @"
        "6 a! @ 2   - 1 nop +   nop + 4 nop   + a! @ 6    a! @"
        32 1 (constraint memory s t data) 18)|#

(validate (insert-nops "2 6 a! ! 1")
          (insert-nops "6 a! 1 2 !")
          "me" 7 (constraint memory s t) 18)

