#lang racket

(require "../cegis.rkt" "../state.rkt" "../stack.rkt" "../greensyn.rkt" "../programs.rkt"
         "../../ArrayForth/arrayforth.rkt")

;; >>> 0x3ffff = 262143
;; >>> 0x1ffff = 131071
;; >>> 0xffff = 65535
;; >>> 0x7fff = 32767
;; >>> 0x3fff = 16383
;; >>> 0x1fff = 8191

;;; swap only at m is 1 xym -> - y' (first part)
;(optimize "a! over over nop a - and nop push a and nop pop over over nop or push and nop pop or nop nop" 
;          #:name "swap" #:num-bits 4 #:inst-pool `no-mem-no-p #:f18a #t)

;;; x - (x & y)
;(optimize "over and - 1 + +" #:constraint (constraint t))

;;; x | y
;(optimize "over over or a! and a or" #:constraint (constraint s t) #:bin-search `time)
;("over - and nop +" . 15)

;(cegis (compile-to-string "over over or a! and a or") 
;       #:constraint (constraint s t) #:slots 5 #:length-limit 100)

;;; round up to multiple of 8 (8-bit)
;(optimize "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:name "roundup" 
;          #:constraint (constraint t) 
;          #:num-bits 8 #:inst-pool `no-mem)

;; communication
;(optimize "@p b! !b . 325 @p b! !b . 325" 
;          #:constraint constraint-none #:num-bits 9 #:name "comm")
;; (optimize-linear "@p b! !b @p 325 325 b! !b nop nop"
;;                  #:slots "325 b! !b nop !b"
;;                  #:constraint constraint-none #:num-bits 18 #:name "comm")

;(optimize "1 2 3 4 5" #:constraint (constraint-data 1 s t) #:num-bits 4
;          #:f18a #f)
          

; i want: 65535 dup push . and 4 a! . !+ @ pop
#|(optimize-linear "65535 and 4 b! !b 5 b! @b 65535" 
                 #:slots 11
                 ;;"65535 dup push nop and 4 a! nop !+ @ pop"
                 #:length-limit 20
                 #:constraint (constraint (data 1) s t memory) #:f18a #f
                 #:mem 6 #:num-bits 18)|#


#|(optimize "nop nop b! !b down a!"
          #:constraint (constraint (return 1) r s t memory b a)  #:f18a #f
          #:start-state (default-state (a 469))
          #:mem 5 #:num-bits 18)|#

(optimize "0 a! !+ !+ push pop dup 1 b! @b and over 65535 or 0 b! @b and over - and + push drop pop" 
          #:constraint (constraint (return 2) (data 1) r s t)  #:f18a #f
          #:mem 1 #:num-bits 18
          #:start-state (constrain-stack (default-state) '((<= . 65535) (<= . 65535) (<= . 65535))))

#|
(program-diff? "0 a! !+ !+ push pop dup 1 b! @b and over 65535 or 0 b! @b and over - and + push drop pop" 
               ;"push over - push and pop pop and over 65535 or and or"
               "dup push or and pop or"
               5 
               (constraint [return 2] [data 1] r s t) 18
               #:start-state (constrain-stack (default-state) '((<= . 65535) (<= . 65535) (<= . 65535))))|#


;(program-diff? "1 1 1 1" "nop" 1 (constraint (data 2) s t) 18 
;               #:start-state (constrain-stack (default-state) '((= . 1) (= . 1) (= . 1) (= . 1))))
