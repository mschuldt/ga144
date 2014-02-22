#lang racket

(require "../cegis.rkt" "../state.rkt" "../../ArrayForth/arrayforth.rkt")

(define (test program n mem constraint)
  (define start (current-seconds))
  (cegis (compile-to-string program) 
         #:mem mem #:constraint constraint #:slots n #:length-limit 100)
  (pretty-display `(cegis-time ,(- (current-seconds) start))))
 
;(test "325 b! !b 277 b! !b 373 b! !b 469 b! !b" 14 1 (constraint memory r s t))
(test "2 b! @b 277 b! !b 1 b! @b 277 b! !b" 9 3 (constraint memory s t))
;(test "5 b! !b 373 b! @b 5 b! @b 277 b! !b" 10 6 (constraint memory s t))