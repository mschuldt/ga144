#lang racket

(require racket/system "cegis.rkt" "state.rkt")

;; Inclusive or with constraint on everything
(optimize "over over or nop a! and a nop or nop nop nop")

;; Inclusive or with constraint on register t only
(optimize "over over or nop a! and a nop or nop nop nop" #:constraint (constraint t))

;; Inclusive or with no binary search on length of the program
(optimize "over over or nop a! and a nop or nop nop nop" #:bin-search #f)

;; Inclusive or using 4-bit number
(optimize "over over or nop a! and a nop or nop nop nop" #:num-bits 4)

;; Round up to multiple of 8.
;; Constraint on t only because we only care about the output at t.
;; Use 8-bit number to speed up the optimization process.
;; Consider instruction in `no-mem pool because we don't want the optimized program to access memory.
(optimize "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:constraint (constraint t) #:num-bits 8 #:inst-pool `no-mem)

;; This is how to use sketch with init and repeat
(optimize "2* dup or . . and 2* dup . and 2* dup" #:init "_ _ or _" #:slots "_ 2* _ _" #:repeat 2 #:constraint (constraint t))

;; This is how to optimize program with reads and writes to NSEW.
(optimize "@p b! @b . UP @p b! !b . DOWN @p b! @b . LEFT @p b! !b . RIGHT" #:constraint constraint-none)

;; This is how to optimize program with reads and writes to NSEW. It also works
;; with smaller number of bit. 
;; Run this and see the note at the end about NSWE constants.
(optimize "@p b! @b . UP @p b! !b . DOWN @p b! @b . LEFT @p b! !b . RIGHT" #:constraint constraint-none #:num-bits 4)
