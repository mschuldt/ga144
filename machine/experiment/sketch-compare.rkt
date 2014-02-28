#lang racket

(require "../cegis.rkt" "../state.rkt" "../../ArrayForth/arrayforth.rkt")

(define (test program n mem constraint [assume (default-state)])
  (define start (current-seconds))
  (cegis (compile-to-string program) 
         #:mem mem #:constraint constraint #:slots n #:length-limit 100
         #:start-state assume
         )
  (pretty-display `(cegis-time ,(- (current-seconds) start))))
 
;(test "325 b! !b 277 b! !b 373 b! !b 469 b! !b" 14 1 (constraint memory r s t))
;(test "2 b! @b 277 b! !b 1 b! @b 277 b! !b" 9 3 (constraint memory s t))
;(test "5 b! !b 373 b! @b 5 b! @b 277 b! !b" 10 6 (constraint memory s t))
;(test "up b! @b push drop pop up b! @b 0 b! !b dup 0 b! @b or over 0 b! @b and or" 
;      15 1 (constraint r s t))
(test "0 a! !+ push !+ pop dup 1 b! @b 0 b! @b 65535 or over - and + or push drop pop" 
      10 2 (constraint s t)
      (constrain-stack (default-state) '((<= . 65535) (<= . 65535) (<= . 65535))))