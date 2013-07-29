#lang racket

(require "classes.rkt" "rvector.rkt")

(provide add-state-words!)

(define (add-state-words!)

  ; Stack manipulation words

  ; Get the first cell and push it back on
  (add-primitive-word!
   "dup"
   (lambda (i)
     (let [(dstack (send i get 'dstack))]
       (push-cells! dstack (get-cells dstack)))))

  ; Get the second cell and push it back on
  (add-primitive-word!
   "over"
   (lambda (i)
     (let [(dstack (send i get 'dstack))]
       (push-cells! dstack (get-cells dstack 1)))))

  (add-primitive-word!
   "drop"
   (lambda (i)
     (pop-cells! (send i get 'dstack))))

#|
  (add-primitive-word!
   "swap"
   (lambda (i)
     (let* [(dstack (send i get 'dstack))
	    (arg1 (pop-cells! dstack))
	    (arg2 (pop-cells! dstack))]
       (push-cells! dstack arg1)
       (push-cells! dstack arg2))))

  (add-primitive-word!
   "rot"
   (lambda (i)
     (let* [(dstack (send i get 'dstack))
	    (arg1 (pop-cells! dstack))
	    (arg2 (pop-cells! dstack))
	    (arg3 (pop-cells! dstack))]
       (push-cells! dstack arg2)
       (push-cells! dstack arg1)
       (push-cells! dstack arg3))))
|#

  ; rstack manipulation words

  (add-primitive-word!
   "push"
   (lambda (i)
     (push-cells! (send i get 'rstack)
		  (pop-cells! (send i get 'dstack)))))

  (add-primitive-word!
   "pop"
   (lambda (i)
     (push-cells! (send i get 'dstack)
		  (pop-cells! (send i get 'rstack)))))

  ; register manipulation

  ; fetch via register

  (add-primitive-word!
   "@+"
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (rega (send i get 'rega))]
       (push-int! dstack (rvector-ref memory rega))
       (send i set 'rega (add1 rega)))))

  (add-primitive-word!
   "@"
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (rega (send i get 'rega))]
       (push-int! dstack (rvector-ref memory rega)))))

  (add-primitive-word!
   "@b"
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (regb (send i get 'regb))]
     (push-int! dstack (rvector-ref memory regb)))))

  ; @p only works when it is immediately followed by { .. }
  ; Annoyingly, needs to take into account the pc increment done by code-loop
  (add-primitive-word!
   "@p" 
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (pc (send i get 'pc))
	   (next-word (send i get 'next-word))]
       (if (= (remainder pc 4) 0)
	   (begin (push-cells! dstack
			       (rvector-ref memory (* 4 (sub1 next-word))))
		  (send i set-pc! next-word))
	   (begin (push-cells! dstack
			       (rvector-ref memory (* 4 next-word)))
		  (send i set 'next-word (add1 next-word)))))))

  ; store via register

  ; TODO: !p doesn't work if write to memory
  (add-primitive-word!
   "!p" 
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (pc (send i get 'pc))]
       (rvector-set! memory pc (pop-cells! dstack))
       (send i increment-pc!))))

  (add-primitive-word!
   "!+" 
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (rega (send i get 'rega))]
       (rvector-set! memory rega (pop-int! dstack #t))
       (send i set 'rega (add1 rega)))))

  (add-primitive-word!
   "!"
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (rega (send i get 'rega))]
       (rvector-set! memory rega (pop-int! dstack #t)))))

  (add-primitive-word!
   "!b"
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (regb (send i get 'regb))]
     (rvector-set! memory regb (pop-int! dstack #t)))))

  ; fetch from register
  (add-primitive-word!
   "a"
   (lambda (i)
     (push-int! (send i get 'dstack) (send i get 'rega))))

  ; store to register
  (add-primitive-word!
   "a!"
   (lambda (i)
     (send i set 'rega (pop-int! (send i get 'dstack) #f))))

  (add-primitive-word!
   "b!"
   (lambda (i)
     (send i set 'regb (pop-int! (send i get 'dstack) #f))))

  ;;;;;;;;;;;;;;;;;;;; (testing only) : store to pc ;;;;;;;;;;;;;;;;;;;
  (add-primitive-word!
   "p!"
   (lambda (i)
     (send i set 'pc (pop-int! (send i get 'dstack) #f)))))
