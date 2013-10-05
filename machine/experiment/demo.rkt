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
;(optimize "over and - @p 1 . + . +" #:slots 4 #:constraint (constraint t))

;;; x | y
;(optimize "over over or nop a! and a nop or nop nop nop" #:constraint (constraint s t) #:num-bits 4)
;("over - and nop +" . 15)

;;; round up to multiple of 8 (8-bit)
;(optimize "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:name "roundup" 
;          #:constraint (constraint t) 
;          #:num-bits 8 #:inst-pool `no-mem)

;; communication
;(optimize "@p b! !b . 325 @p b! !b . 325" 
;          #:constraint constraint-none #:num-bits 9 #:name "comm")
;(fastest-program "@p b! !b @p 325 325 b! !b nop nop"
;                 ;; #:slots "325 b! !b nop !b"
;                 #:constraint constraint-none #:num-bits 18 #:name "comm")

;(optimize "1 2 3 4 5" #:constraint (constraint-data 1 s t) #:num-bits 4
;          #:f18a #f)
          

; i want: 65535 dup push . and 4 a! . !+ @ pop
#|(fastest-program "65535 and 4 b! !b 5 b! @b 65535" 
                 #:slots 11
                 ;;"65535 dup push nop and 4 a! nop !+ @ pop"
                 #:length-limit 20
                 #:constraint (constraint-data 1 s t memory) #:f18a #f
                 #:mem 6 #:num-bits 18)|#


(optimize "pop a!" 
          #:constraint (constraint (return 1) r s t a memory)  #:f18a #f
          #:mem 3 #:num-bits 18)

#|
original program	: "pop a! nop + @p b! !b nop 0    "
memory			: 5
constraint		: #(struct:progstate #t #f #f #f #t #t #t #f 1 #t)
length			: 7
approx. runtime		: 35
length with literal	: 11
|#
