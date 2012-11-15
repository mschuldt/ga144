#lang racket

(require racket/system "cegis.rkt" "state.rkt")

;; (set! start-time (current-seconds))
;; (pretty-display "swap only at m (x' y') (complete version)")
;; (fastest-program3 "a! over over nop or a and nop over or push nop over or a nop and or dup nop pop nop nop nop" #:name "roundup" #:constraint (constraint s t) #:num-bits 4 #:inst-pool `no-mem-no-p)
;; (pretty-display (format "Time: ~a seconds." (- (current-seconds) start-time)))

;(cegis "+ - 2* nop" #:slots "+ _ _" #:time-limit 30)

(fastest-program3 "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:name "roundup" #:constraint (constraint t) #:num-bits 8 #:inst-pool `no-mem)