#lang racket

(require "classes.rkt" "rvector.rkt")
(provide add-control-words!)

; Control
(define dummy-proc void)

(define (add-control-words!)
; Return
; Moves R into P, popping the return stack.
; As a result, it skips any remaining slots and fetches next instruction word.
  (add-primitive-word!
   ";" 
   (lambda (i)
     (send i set-pc! (pop-int! (send i get 'rstack) #f))))
  (make-synonym ";" "ret")

; Execute
; Exchanges R and P.
; As a result, it skips any remaining slots and fetches next instruction word.
  (add-primitive-word!
   "ex"
   (lambda (i)
     (let [(temp (send i get 'pc))
	   (rstack (send i get 'rstack))]
       (send i set-pc! (pop-int! rstack #f))
       (push-int! rstack temp))))

; Jump
; Sets P to destination address
; As a result, it fetches next instruction word.
  (add-primitive-word!
   "jump"
   (lambda (i addr) (send i set-pc! addr)))

; call
; Moves P into R, pushing an item onto the return stack,
; Sets P to destination address 
; As a result, it fetches next instruction word.
  (add-primitive-word!
   "call"
   (lambda (i addr)
     (let [(pc (send i get 'pc))
	   (rstack (send i get 'rstack))]
       (push-int! rstack (add1 (quotient pc 4)))
       (send i set-pc! addr))))

; unext
; If R is zero, pops the return stack and continues with the next opcode.
; If R is nonzero, decrements R by 1 and causes execution to continue with slot 0 of the current instruction word
; This is done without re-fetching the word (irrelevant here).
  (add-primitive-word!
   "unext"
   (lambda (i)
     (let* [(rstack (send i get 'rstack))
	    (r (pop-int! rstack #f))
	    (pc (send i get 'pc))]
       (unless (= r 0)
	       (push-int! rstack (- r 1))
	       (send i set-pc! (quotient (- pc 1) 4))))))

; next
; If R is zero, pops the return stack and continues with the next instruction word addressed by P.
; If R is nonzero, decrements R by 1 and jumps to the given address.
  (add-primitive-word!
   "next"
   (lambda (i addr)
     (let* [(rstack (send i get 'rstack))
	    (r (pop-int! rstack #f))
	    (pc (send i get 'pc))]
       (if (= r 0)
	   (send i set-pc! (+ 1 (quotient (- pc 1) 4)))
	   (begin (push-int! rstack (- r 1))
		  (send i set-pc! addr)))))))

#|
; IF - 
; 1. Puts a procedure which jumps over one slot if TRUE is on the stack.
; 2. Puts HERE on the stack, and then fills the slot with a dummy procedure.
; This will later be replaced by an unconditional branch by ELSE or THEN.
; TODO:  Just use location-counter, or also use i-register?
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
; TODO:  Just use location-counter, or also use i-register?
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
; TODO:  Just use location-counter, or also use i-register?
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

; BEGIN
; Put HERE on the stack, to be used by UNTIL or REPEAT.
; TODO:  Just use location-counter, or also use i-register?
(define (begin-proc)
  (fill-rest-with-nops)
  (push-int! cstack location-counter))
(add-compiler-directive! "begin" begin-proc)

; FOR
; 1. Add to compiled code the PUSH command.
; 2. Put HERE on the stack, to be used by NEXT.
; TODO: Is push-proc supposed to be called?  If yes, then rewrite push-proc in terms of the lambda (push-proc is not available in this file).
; TODO:  Just use location-counter, or also use i-register?

(define (for-proc)
  (add-primitive-code!(push-proc))
  (push-int! dstack location-counter))
(add-primitive-word! #t "for" for-proc)


(add-compiler-directive! "for"
			 (lambda ()
			   (add-compiled-code! "push")
			   (begin-proc)))


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
|#
