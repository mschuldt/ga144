#lang racket

(require "rvector.rkt" "forth_state.rkt")
(provide (all-defined-out))

; Control
(define (dummy-proc) (void))

; IF - 
; 1. Puts a procedure which jumps over one slot if TRUE is on the stack.
; 2. Puts HERE on the stack, and then fills the slot with a dummy procedure.
; This will later be replaced by an unconditional branch by ELSE or THEN.
(define (if-proc)
  (add-primitive-code!
   (lambda () (if (= (get-int dstack #f) 0)
                  (void)
                  (set! pc (add1 pc)))))
  (push-int! dstack location-counter)
  (add-primitive-code! dummy-proc))
(add-primitive-word! #t "if" if-proc)

; -IF
; 1. Puts a procedure which jumps over one slot if the top of stack is negative.
; 2. Puts HERE on the stack, and then fills the slot with a dummy procedure.
; This will later be replaced by an unconditional branch by ELSE or THEN.
(define (nif-proc)
  (add-primitive-code!
   (lambda () (if (>= (get-int dstack #t) 0)
                  (void)
                  (set! pc (add1 pc)))))
  (push-int! dstack location-counter)
  (add-primitive-code! dummy-proc))
(add-primitive-word! #t "-if" nif-proc)

; THEN
; Put an unconditional branch to HERE.
; This will patch up the dummy procedure left by IF or ELSE.
(define (then-proc)
  (let [(here-addr location-counter)]
    (rvector-set! codespace (pop-int! dstack #f) (lambda () (set! pc here-addr)))))
(add-primitive-word! #t "then" then-proc)


; Loops
;; Removed LEAVE

; LOOP
(define (loop-proc)
  (let [(addr (pop-int! dstack #f))]
    (add-primitive-code!
     (lambda ()
       (if (= (add1 (get-int rstack #t)) (get-int rstack #t 1))
           (begin (pop-int! rstack #t)
		  (pop-int! rstack #t))
           (begin (push-int! rstack (add1 (pop-int! rstack #t)))
                  (set! pc addr)))))))
(add-primitive-word! #t "loop" loop-proc)

; FOR
; 1. Add to compiled code the PUSH command.
; 2. Put HERE on the stack, to be used by NEXT.
; TODO: Is push-proc supposed to be called?  If yes, then rewrite push-proc in terms of the lambda (push-proc is not available in this file).
#|
(define (for-proc)
  (add-primitive-code!(push-proc))
  (push-int! dstack location-counter))
(add-primitive-word! #t "for" for-proc)
|#
  
; NEXT
; 1. Pop counter from the top of rstack.
; 2. If counter is 0, continue.
; 3. If counter is not 0, push counter-1 back to rstack and jump to label pushed by FOR
(define (next-loop-proc counter addr)
  (push-int! rstack (- counter 1))
  (set! pc addr))
  
(define (next-proc)
  (let [(addr (pop-int! dstack #f))]
    (add-primitive-code! (lambda ()
                        (let [(counter (pop-int! rstack #t))]
                          (if (= counter 0)
                              (void)
                              (next-loop-proc counter addr)))))))

(add-primitive-word! #t "next" next-proc)

; MICRO NEXT. Loop to the beginning of I, the current instruction word, instead of FOR.
;; TODO: need assertion that instructions between FOR and UNEXT 3 slots and start at slot 0.
(add-primitive-word! #t "unext" next-proc)

; BEGIN
; Put HERE on the stack, to be used by UNTIL or REPEAT.
(add-primitive-word! #t "begin" (lambda () (push-int! dstack location-counter)))

; UNTIL
; Jumps back to the address left by BEGIN if it sees a false flag.
(define (until-proc)
  (let [(addr (pop-int! dstack #f))]
    (add-primitive-code! (lambda ()
                          (if (= (pop-int! dstack #t) 0)
                              (set! pc addr)
                              (void))))))
(add-primitive-word! #t "until" until-proc)

; WHILE
; Does the same thing as IF.
; BEGIN - WHILE - REPEAT is like BEGIN - IF - LOOP THEN
(add-primitive-word! #t "while" if-proc)

(add-primitive-word! #f "?dup" (lambda () (if (= 0 (get-int dstack #f))
                                              (void)
                                              (push-cells! (get-cells dstack)))))

(add-primitive-word! #t "abort\""
                     (lambda () (let [(str (read-string))]
                                  (add-primitive-code!
                                   (lambda () (if (= (pop-int! dstack #t) false)
                                                  (void)
                                                  (raise str)))))))
