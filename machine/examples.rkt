#lang racket

(require racket/system "cegis.rkt" "state.rkt")

;; ;;; Get the most significant bit of a number by shifting it right 17 times.
;; (pretty-display "shift right")
;; (fastest-program "2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ 2/ nop 2/ 2/ nop nop"
;;                    #:slots 19 #:mem 4 #:start 5 #:constraint (constraint t))
;; This one still hasn't finished yet...

;;; Averaging four numbers:
;;; (fastest-program "nop + nop + nop + 2/ nop 2/ nop nop nop"  #:slots 8 #:constraint (constraint t))
;;; This one finished in a rather uninteresting '("+ nop + nop + 2/ 2/ nop" . 21)
;;; It took 4970 seconds (about 80 minutes). Might be a useful data
;;; point, but certainly not a useful example!

;; (set! start-time (current-seconds))
;; (pretty-display "swap only at m (x' y') (second part)")
;; (fastest-program3 "push nop a nop and push a nop - and nop nop pop over over nop or push and nop pop or pop nop" #:name "swap2" #:constraint (constraint s t) #:num-bits 4 #:inst-pool `no-mem-no-p)
;; (pretty-display (format "Time: ~a seconds." (- (current-seconds) start-time)))

;; (set! start-time (current-seconds))
;; (pretty-display "swap only at m (x' y') (first part)")
;; (fastest-program3 "a! over over nop a - and nop push a and nop pop over over nop or push and nop pop or push nop" #:name "swap1" #:constraint (constraint a r s t) #:num-bits 4 #:inst-pool `no-mem-no-p)
;; (pretty-display (format "Time: ~a seconds." (- (current-seconds) start-time)))

;; (set! start-time (current-seconds))
;; (pretty-display "nearest multiple of 8 (4-bit)")
;; (fastest-program3 "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:name "roundup" #:constraint (constraint t) #:num-bits 4 #:inst-pool `no-mem)
;; (pretty-display (format "Time: ~a seconds." (- (current-seconds) start-time)))

;; (set! start-time (current-seconds))
;; (pretty-display "nearest multiple of 8 (8-bit)")
;; (fastest-program3 "@p nop + @p 7 8 - @p nop + 1 and nop nop nop" #:name "roundup" #:constraint (constraint t) #:num-bits 8 #:inst-pool `no-mem)
;; (pretty-display (format "Time: ~a seconds." (- (current-seconds) start-time)))

(set! start-time (current-seconds))
(pretty-display "swap only at m (x' y') (complete version)")
(fastest-program3 "a! over over nop or a and nop over or push nop over or a nop and or dup nop pop nop nop nop" #:name "roundup" #:constraint (constraint s t) #:num-bits 4 #:inst-pool `no-mem-no-p)
(pretty-display (format "Time: ~a seconds." (- (current-seconds) start-time)))