#lang racket

(require "classes.rkt" "rvector.rkt")

(provide add-state-words!)

(define (add-state-words!)

  ; Stack manipulation words

  ; Get the first cell and push it back on
  (add-instruction!
   "dup"
   (lambda (i)
     (let [(dstack (send i get 'dstack))]
       (push-cells! dstack (get-cells dstack)))))

  ; Get the second cell and push it back on
  (add-instruction!
   "over"
   (lambda (i)
     (let [(dstack (send i get 'dstack))]
       (push-cells! dstack (get-cells dstack 1)))))

  (add-instruction!
   "drop"
   (lambda (i)
     (pop-cells! (send i get 'dstack))))

#|
  (add-instruction!
   "swap"
   (lambda (i)
     (let* [(dstack (send i get 'dstack))
	    (arg1 (pop-cells! dstack))
	    (arg2 (pop-cells! dstack))]
       (push-cells! dstack arg1)
       (push-cells! dstack arg2))))

  (add-instruction!
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

  (add-instruction!
   "push"
   (lambda (i)
     (push-cells! (send i get 'rstack)
		  (pop-cells! (send i get 'dstack)))))

  (add-instruction!
   "pop"
   (lambda (i)
     (push-cells! (send i get 'dstack)
		  (pop-cells! (send i get 'rstack)))))

  ; register manipulation

  ; fetch via register

  (add-instruction!
   "@+"
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (rega (send i get 'rega))]
       (push-int! dstack (rvector-ref memory rega))
       (send i set 'rega (add1 rega)))))

  (add-instruction!
   "@"
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (rega (send i get 'rega))]
       (push-int! dstack (rvector-ref memory rega)))))

  (add-instruction!
   "@b"
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (regb (send i get 'regb))]
     (push-int! dstack (rvector-ref memory regb)))))

  (add-instruction!
   "@p" 
   (lambda (i)
     (push-cells! (send i get 'dstack) (send i read-and-increment-pc!))))

  ; store via register

  (add-instruction!
   "!p" 
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (pc (send i get 'pc))]
       (rvector-set! memory pc (pop-cells! dstack))
       (send i increment-pc!))))

  (add-instruction!
   "!+" 
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (rega (send i get 'rega))]
       (rvector-set! memory rega (pop-int! dstack #t))
       (send i set 'rega (add1 rega)))))

  (add-instruction!
   "!"
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (rega (send i get 'rega))]
       (rvector-set! memory rega (pop-int! dstack #t)))))

  (add-instruction!
   "!b"
   (lambda (i)
     (let [(dstack (send i get 'dstack))
	   (memory (send i get 'memory))
	   (regb (send i get 'regb))]
     (rvector-set! memory regb (pop-int! dstack #t)))))

  ; fetch from register
  (add-instruction!
   "a"
   (lambda (i)
     (push-int! (send i get 'dstack) (send i get 'rega))))

  ; store to register
  (add-instruction!
   "a!"
   (lambda (i)
     (send i set 'rega (pop-int! (send i get 'dstack) #f))))

  (add-instruction!
   "b!"
   (lambda (i)
     (send i set 'regb (pop-int! (send i get 'dstack) #f))))

  ;;;;;;;;;;;;;;;;;;;; (testing only) : store to pc ;;;;;;;;;;;;;;;;;;;
  (add-instruction!
   "p!"
   (lambda (i)
     (send i set 'pc (pop-int! (send i get 'dstack) #f)))))
