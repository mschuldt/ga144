#lang racket

(require "../cegis.rkt" "../state.rkt" "../../ArrayForth/arrayforth.rkt")

(define (test program n)
  (define start (current-seconds))
  (cegis (compile-to-string program) 
         #:constraint (constraint s t) #:slots n #:length-limit 100)
  (pretty-display `(cegis-time ,(- (current-seconds) start))))
 
;;; x | y
(test "over over or a! and a or" 5)