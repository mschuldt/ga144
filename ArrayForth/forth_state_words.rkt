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


;;;;;;;;;;;;;;;;;;;;; END - not sure if in arrayForth ;;;;;;;;;;;;;;;;;;;;;
#|
(define (2swap)
  (let* [(arg1 (pop-2cells! dstack))
         (arg2 (pop-2cells! dstack))]
    (push-cells! dstack arg1)
    (push-cells! dstack arg2)))
(add-primitive-word! #f "2swap" 2swap)

(define (2dup)
  (push-cells! dstack (get-cells dstack 0 2))) ; Get the first cell and push it back on
(add-primitive-word! #f "2dup" 2dup)

(define (2over)
  (push-cells! dstack (get-cells dstack 2 4)))
(add-primitive-word! #f "2over" 2over)

(define (2rot)
  (push-cells! dstack (pop-cells! dstack 4 6)))
(add-primitive-word! #f "2rot" 2rot)

(define (2drop)
  (pop-2cells! dstack))
(add-primitive-word! #f "2drop" 2drop)
|#
;;;;;;;;;;;;;;;;;;;;; END - not sure in arrayForth ;;;;;;;;;;;;;;;;;;;;;

; rstack manipulation words

(define (push-proc) 
  (lambda () (push-cells! rstack (pop-cells! dstack))))
(define (pop-proc)
  (lambda () (push-cells! dstack (pop-cells! rstack))))

(add-primitive-word! #f "push" (push-proc))
(add-primitive-word! #f "pop" (pop-proc))

;; TODO: check if it means the same thing in arrayforth
(add-primitive-word! #f "i" (lambda () (push-cells! dstack (get-cells rstack))))

; register manipulation

; fetch via register
(add-primitive-word! #f "@+"
                     (lambda ()
                         (push-int! dstack (rvector-ref codespace rega))
                         (set! rega (add1 rega))))
(add-primitive-word! #f "@" (lambda () (push-int! dstack (rvector-ref codespace rega))))
(add-primitive-word! #f "@b" (lambda () (push-int! dstack (rvector-ref codespace regb))))

; @p only works when it is immediately followed by { .. }
(define (fetch-p-proc)
  (push-int! dstack (entry-data here-entry))
  (add-primitive-code! (lambda () (void))))
(add-primitive-word! #f "@p" 
                     (lambda ()
                         (push-int! dstack (rvector-ref codespace pc))
                         (set! pc (add1 pc))))

; store via register

;; TODO: !p doesn't work if write to memory
(add-primitive-word! #f "!p" 
                     (lambda () 
                       (rvector-set! codespace pc (pop-cells! dstack))
                       (set! pc (add1 pc))))
(add-primitive-word! #f "!+" 
                     (lambda () 
                       (rvector-set! codespace rega (pop-int! dstack #t))
                       (set! rega (add1 rega))))
(add-primitive-word! #f "!" (lambda () (rvector-set! codespace rega (pop-int! dstack #t))))
(add-primitive-word! #f "!b" (lambda () (rvector-set! codespace regb (pop-int! dstack #t))))

; fetch from register
(add-primitive-word! #f "a" (lambda () (push-int! dstack rega)))

; store to register
(add-primitive-word! #f "a!" (lambda () (set! rega (pop-int! dstack #f))))
(add-primitive-word! #f "b!" (lambda () (set! regb (pop-int! dstack #f))))

;;;;;;;;;;;;;;;;;;;; (testing only) : store to pc ;;;;;;;;;;;;;;;;;;;
(add-primitive-word! #f "p!" (lambda () (set! pc (pop-int! dstack #f))))
