#lang racket

(require racket/system "cegis.rkt" "state.rkt")

;; Hello World example :)
(optimize "1 . . +" #:bin-search `time)

;; Inclusive or, with constraint on everything. Optimize for length as default.
(optimize "over over or a! and a or")

;; Inclusive or with constraint on register t only
(optimize "over over or a! and a or" #:constraint (constraint t))

;; Inclusive or with no binary search on length of the program
(optimize "over over or a! and a or" #:bin-search #f)

;; Inclusive or using 4-bit number
(optimize "over over or a! and a or" #:num-bits 4)

;; Round up to multiple of 8.
;; Constraint on t only because we only care about the output at t.
;; Use 8-bit number to speed up the optimization process.
;; Consider instruction in `no-mem pool because we don't want the optimized program
;; to access memory.
(optimize "7 + 8 - 1 + and" #:constraint (constraint t) #:num-bits 8 #:inst-pool `no-mem)

;; This is how to use sketch with init and repeat
(optimize "2* dup or . . and 2* dup . and 2* dup" 
          #:init "_ _ or _" 
          #:slots "_ 2* _ _" #:repeat 2 #:constraint (constraint t))

;; This is how to optimize program with reads and writes to NSEW.
(optimize "up b! @b down b! !b left b! @b right b! !b" 
          #:constraint constraint-none)
