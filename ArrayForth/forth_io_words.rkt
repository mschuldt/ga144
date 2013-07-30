#lang racket

(require "classes.rkt" "rvector.rkt")
(provide add-io-words!)

(define (add-io-words!)

  (add-primitive-word!
   "send"
   (lambda (i)
     (let* [(dstack (send i get 'dstack))
	    (pc (send i get 'pc))
	    (table (send i get 'send-recv-table))
	    (arg1 (pop-int! dstack #t))
	    (arg2 (pop-int! dstack #t))]
       (if (rvector-ref table arg1)
	   (send i set 'pc (sub1 pc))
	   (rvector-set! table arg1 arg2)))))

  (add-primitive-word!
   "recv"
   (lambda (i)
     (let* [(dstack (send i get 'dstack))
	    (pc (send i get 'pc))
	    (table (send i get 'send-recv-table))
	    (arg1 (pop-int! dstack #t))
	    (val (rvector-ref table arg1))]
       (if val
	   (begin  (push-int! dstack val)
		   (rvector-set! table arg1 #f))
	   (begin (push-int! dstack arg1)
		  (send i set 'pc (sub1 pc)))))))

  ; Debugging
  (add-primitive-word!
   ".s"
   (lambda (i)
     (print-stack (send i get 'dstack))))

  (add-primitive-word!
   ".ns"
   (lambda (i)
     (print (send i get 'state-index))
     (display ": ")
     (print-stack (send i get 'dstack))
     (newline)))

  (add-primitive-word!
   ".r"
   (lambda (i)
     (print-stack (send i get 'rstack))))

  (add-primitive-word!
   ".nr"
   (lambda (i)
     (print (send i get 'state-index))
     (display ": ")
     (print-stack (send i get 'rstack))
     (newline)))

  (define (print-memory memory start end)
    (if (>= start end)
	(display " |")
	(begin (display " |")
	       (for [(i (in-range 0 4))]
		    (let [(elmt (rvector-ref memory (+ start i)))]
		      (cond [(bytes? elmt)
			     (display " ")
			     (display (integer-bytes->integer elmt #t #t))]
			    [elmt
			     (display " ")
			     (display elmt)])))
	       (print-memory memory (+ 4 start) end))))

  (add-primitive-word!
   ".mem"
   (lambda (i)
     (let* [(dstack (send i get 'dstack))
	    (memory (send i get 'memory))
	    (end (pop-int! dstack #f))
	    (start (pop-int! dstack #f))]
       (printf "Printing memory from ~a to ~a:" start end)
       (print-memory memory (* 4 start) (* 4 end))
       (newline)))))
