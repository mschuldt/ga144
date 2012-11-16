#lang racket

(require racket/system "cegis.rkt" "state.rkt")

;; (set! start-time (current-seconds))
;; (pretty-display "swap only at m (x' y') (complete version)")
;; (fastest-program3 "a! over over nop or a and nop over or push nop over or a nop and or dup nop pop nop nop nop" #:name "roundup" #:constraint (constraint s t) #:num-bits 4 #:inst-pool `no-mem-no-p)
;; (pretty-display (format "Time: ~a seconds." (- (current-seconds) start-time)))

;(cegis "+ - 2* nop" #:slots "+ _ _" #:time-limit 30)

;(fastest-program3 "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:name "roundup" #:constraint (constraint t) #:num-bits 8 #:inst-pool `no-mem)

;;; Population count:
;(fastest-program "@p and nop nop #xFFFF @p a! dup nop 1 a and push nop 2/ dup a nop and pop nop + push 2/ dup nop a and pop nop + push 2/ nop dup a and nop pop nop + nop push 2/ dup nop a and pop nop + push 2/ nop dup a and nop pop nop + nop push 2/ dup nop a and pop nop + push 2/ nop dup a and nop pop nop + nop push 2/ dup nop a and pop nop + push 2/ nop dup a and nop pop nop + nop push 2/ dup nop a and pop nop + push 2/ nop dup a and nop pop nop + nop push 2/ dup nop a and pop nop + push 2/ nop dup a and nop pop nop + nop push 2/ a nop and pop nop +" #:name "foo" #:slots "#xFFFF and nop nop dup _ rshift _ and - _ nop + nop + dup _ and push _ rshift _ and nop pop nop + dup _ rshift nop + _ and  dup _ _ rshift nop + dup _ rshift nop + and nop nop" #:constraint (constraint t))

(fastest-program
"@p a! ! @p 0 2 a! ! nop + push @p a! nop 1 ! @p a! nop 3 ! nop + nop pop dup 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop push a! pop nop + a over nop"
#:name "32bitadd"
#:slots "0 a! ! 2 a! ! _ _ _ 1 a! _ ! 3 a! _ ! _ _ _ _ _ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop _ _ _ _ _ _ _ _" 
#:mem 4)