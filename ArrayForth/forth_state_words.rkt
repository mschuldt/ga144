#lang racket

(require "rvector.rkt" "forth_state.rkt")

(provide (all-defined-out))

; Stack manipulation words

(define (dup)
  (push-cells! dstack (get-cells dstack))) ; Get the first cell and push it back on
(add-primitive-word! #f "dup" dup)

(define (over)
  (push-cells! dstack (get-cells dstack 1)))
(add-primitive-word! #f "over" over)

(define (drop)
  (pop-cells! dstack))
(add-primitive-word! #f "drop" drop)

(add-primitive-word! #f "c" (lambda () (set-state-dstack! (vector-ref cores state-index) (make-bytes 0))))

(define (swap)
  (let* [(arg1 (pop-cells! dstack))
         (arg2 (pop-cells! dstack))]
    (push-cells! dstack arg1)
    (push-cells! dstack arg2)))
(add-primitive-word! #f "swap" swap)

(define (rot)
  (let* [(arg1 (pop-cells! dstack))
         (arg2 (pop-cells! dstack))
         (arg3 (pop-cells! dstack))]
    (push-cells! dstack arg2)
    (push-cells! dstack arg1)
    (push-cells! dstack arg3)))
(add-primitive-word! #f "rot" rot)

; rstack manipulation words

(add-primitive-word! #f "push" (lambda () (push-cells! rstack (pop-cells! dstack))))
(add-primitive-word! #f "pop" (lambda () (push-cells! dstack (pop-cells! rstack))))

; register manipulation

; fetch via register
(add-primitive-word! #f "@+"
                     (lambda ()
                         (push-int! dstack (rvector-ref memory rega))
                         (set! rega (add1 rega))))
(add-primitive-word! #f "@" (lambda () (push-int! dstack (rvector-ref memory rega))))
(add-primitive-word! #f "@b" (lambda () (push-int! dstack (rvector-ref memory regb))))

; @p only works when it is immediately followed by { .. }
; Annoyingly, needs to take into account the pc increment done by code-loop
(define (fetch-p-proc)
  (push-int! dstack location-counter)
  (add-primitive-code! (lambda () (void))))
(add-primitive-word! #f "@p" 
                     (lambda ()
		       (if (= (remainder pc 4) 0)
			   (begin (push-cells! dstack (rvector-ref memory (* 4 (sub1 next-word))))
				  (set! pc (* 4 next-word)))
			   (push-cells! dstack (rvector-ref memory (* 4 next-word))))
		       (set! next-word (add1 next-word))))

; store via register

;; TODO: !p doesn't work if write to memory
(add-primitive-word! #f "!p" 
                     (lambda () 
                       (rvector-set! memory pc (pop-cells! dstack))
                       (increment-pc!)))
(add-primitive-word! #f "!+" 
                     (lambda () 
                       (rvector-set! memory rega (pop-int! dstack #t))
                       (set! rega (add1 rega))))
(add-primitive-word! #f "!" (lambda () (rvector-set! memory rega (pop-int! dstack #t))))
(add-primitive-word! #f "!b" (lambda () (rvector-set! memory regb (pop-int! dstack #t))))

; fetch from register
(add-primitive-word! #f "a" (lambda () (push-int! dstack rega)))

; store to register
(add-primitive-word! #f "a!" (lambda () (set! rega (pop-int! dstack #f))))
(add-primitive-word! #f "b!" (lambda () (set! regb (pop-int! dstack #f))))

;;;;;;;;;;;;;;;;;;;; (testing only) : store to pc ;;;;;;;;;;;;;;;;;;;
(add-primitive-word! #f "p!" (lambda () (set! pc (pop-int! dstack #f))))
