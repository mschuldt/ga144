#lang racket

(require "rvector.rkt" "forth_state.rkt")

(provide (all-defined-out))

; Stack manipulation words

(define (dup)
  (push-cells! (get-cells))) ; Get the first cell and push it back on
(add-primitive-word! #f "dup" dup)

(define (over)
  (push-cells! (get-cells 1 2)))
(add-primitive-word! #f "over" over)

(define (drop)
  (pop-cells!))
(add-primitive-word! #f "drop" drop)

(add-primitive-word! #f "c" (lambda () (set-state-stack! (vector-ref cores state-index) (make-bytes 0))))

(define (swap)
  (let* [(arg1 (pop-cells!))
         (arg2 (pop-cells!))]
    (push-cells! arg1)
    (push-cells! arg2)))
(add-primitive-word! #f "swap" swap)

(define (rot)
  (push-cells! (pop-cells! 2 3)))

(add-primitive-word! #f "rot" rot)


;;;;;;;;;;;;;;;;;;;;; END - not sure if in arrayForth ;;;;;;;;;;;;;;;;;;;;;
(define (2swap)
  (let* [(arg1 (pop-2cells!))
         (arg2 (pop-2cells!))]
    (push-cells! arg1)
    (push-cells! arg2)))
(add-primitive-word! #f "2swap" 2swap)

(define (2dup)
  (push-cells! (get-cells 0 2))) ; Get the first cell and push it back on
(add-primitive-word! #f "2dup" 2dup)

(define (2over)
  (push-cells! (get-cells 2 4)))
(add-primitive-word! #f "2over" 2over)

(define (2rot)
  (push-cells! (pop-cells! 4 6)))
(add-primitive-word! #f "2rot" 2rot)

(define (2drop)
  (pop-2cells!))
(add-primitive-word! #f "2drop" 2drop)
;;;;;;;;;;;;;;;;;;;;; END - not sure in arrayForth ;;;;;;;;;;;;;;;;;;;;;

; rstack manipulation words

(define (push-proc) 
  (lambda () (push-cells! #:getter state-rstack #:setter set-state-rstack! (pop-cells!))))
(define (pop-proc)
  (lambda () (push-cells! (pop-cells! #:getter state-rstack #:setter set-state-rstack!))))

(add-primitive-word! #f "push" (push-proc))
(add-primitive-word! #f "pop" (pop-proc))

;; TODO: check if it means the same thing in arrayforth
(add-primitive-word! #f "i" (lambda () (push-cells! (get-cells #:stack (state-rstack (vector-ref cores state-index))))))

; register manipulation

; fetch via register
(add-primitive-word! #f "@+"
                     (lambda ()
                         (push-int! (rvector-ref codespace rega))
                         (set! rega (add1 rega))))
(add-primitive-word! #f "@" (lambda () (push-int! (rvector-ref codespace rega))))
(add-primitive-word! #f "@b" (lambda () (push-int! (rvector-ref codespace regb))))

; @p only works when it is immediately followed by { .. }
(define (fetch-p-proc)
  (push-int! (entry-data here-entry))
  (add-primitive-code! (lambda () (void))))
(add-primitive-word! #f "@p" 
                     (lambda ()
                         (push-int! (rvector-ref codespace pc))
                         (set! pc (add1 pc))))

; store via register

;; TODO: !p doesn't work if write to memory
(add-primitive-word! #f "!p" 
                     (lambda () 
                       (rvector-set! codespace pc (pop-cells!))
                       (set! pc (add1 pc))))
(add-primitive-word! #f "!+" 
                     (lambda () 
                       (rvector-set! codespace rega (pop-int! #t))
                       (set! rega (add1 rega))))
(add-primitive-word! #f "!" (lambda () (rvector-set! codespace rega (pop-int! #t))))
(add-primitive-word! #f "!b" (lambda () (rvector-set! codespace regb (pop-int! #t))))

; fetch from register
(add-primitive-word! #f "a" (lambda () (push-int! rega)))

; store to register
(add-primitive-word! #f "a!" (lambda () (set! rega (pop-int! #f))))
(add-primitive-word! #f "b!" (lambda () (set! regb (pop-int! #f))))

;;;;;;;;;;;;;;;;;;;; (testing only) : store to pc ;;;;;;;;;;;;;;;;;;;
(add-primitive-word! #f "p!" (lambda () (set! pc (pop-int! #f))))
