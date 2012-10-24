#lang racket

(require "arithmetic.rkt" "rvector.rkt" "forth_read.rkt" "forth_num_convert.rkt")
(require "forth_state.rkt" "forth_state_words.rkt" "forth_bit_words.rkt" "forth_io_words.rkt" "forth_control_words.rkt")
(provide interpret interpret-cores run-tests)

; Interpreter and associated procedures

; This code is ugly, because I don't know how to use Racket well.
(define (code-loop)
  (let [(ended-list
	 (map
	  (lambda (num)
	    (set! state-index num)
	    (let [(code (rvector-ref ram pc))]
	      (if (equal? code 0)
		  #t
		  (begin
		    (set! pc (add1 pc))
		    (with-handlers ([string? abort])
				   (if (number? code)
				       (execute-code code)
				       (code)))
		    #f))))
	  used-cores))]
    (unless (for/and ([bool ended-list]) bool)
	    (code-loop))))

(define (execute-code addr)
  (push-int! rstack pc) ; pc will be the address of the next instruction to execute
  (set! pc addr))

(define execute (compose execute-code entry-code))

(add-primitive-word! #f "ex" (lambda () (execute (rvector-ref dict (pop-int! dstack #f)))))

(define (abort msg)
  (displaynl msg)
  (set! dstack (make-dstack))
  (quit))

(define (quit)
  (read-line) ; The rest of the line should not be used as input
  (set! rstack (make-rstack))
  (set! pc interpreter-addr))
(add-primitive-word! #f "quit" quit)

(define (tick)
  (push-int! dstack (find-address (forth_read_no_eof))))
(add-primitive-word! #f "'" tick)

(define (interpret-proc)
  (push-int! rstack (sub1 pc)) ; When interpret exits, go back to interpret itself.
  (let [(name (forth_read))]
    (if (eof-object? name)
        (set! pc -1)
        (if (eq? name #\newline)
            (displaynl " ok")
            (let [(entry (find-entry name))]
              (if entry
                  (execute entry)
                  (let [(num (string->bytes name))]
                    (if num
                        (push-cells! dstack num)
                        (raise (string-append name " ?"))))))))))
(add-primitive-word! #f "interpret" interpret-proc)

(define (reset-states!)
  (for [(i (in-range 0 num-cores))]
       (set-state-dstack! (vector-ref cores i) (make-dstack))
       (set-state-rstack! (vector-ref cores i) (make-rstack))
       (set-state-pc! (vector-ref cores i) 0)))

(define (interpret)
  (set! used-cores '(0))
  (set! state-index 0)
  (set! input (current-input-port))
  (reset-states!)
  (code-loop))

(define used-cores '())

(define (interpret-cores code-info)
  (define (setup-loop code)
    (set! state-index (get-core-num (car code)))
    (set! used-cores (cons state-index used-cores))
    (set! input (get-core-port (car code)))
    (set! pc interpreter-addr)
    (if (null? (cdr code))
	(void)
	(setup-loop (cdr code))))
  (setup-loop code-info)
  (set! state-index (get-core-num (car code-info)))
  (code-loop))

; Colon compiler
(define (colon-compiler)
  (push-int! rstack (sub1 pc)) ; When ] exits, go back to ] itself.
  (let [(to-compile (forth_read_no_eof))]
    (if (not (eq? to-compile #\newline))
        (let [(entry (find-entry to-compile))]
          (cond [(not entry)
                 (let [(num (string->bytes to-compile))]
                   (if num
                       (add-primitive-code! (lambda () (push-cells! dstack num)))
                       (raise (string-append to-compile " ?"))))]
                [(entry-precedence entry)
                 (execute entry)]
                [(entry-primitive entry)
                 (add-primitive-code! (proc-ref codespace (entry-code entry)))]
                [else
                 (add-primitive-code! (entry-code entry))] ))
        (void))))
(add-primitive-word! #f "]" colon-compiler)

(define interpreter-addr (entry-code (find-entry "interpret")))
(define compiler-addr (entry-code (find-entry "]")))

; Colon definition - Uses the colon compiler
(void (add-word! #f #t ":")) ; Don't want Racket saying #<entry> when this is loaded
; Can't be primitive because it has more than one entry in the codespace (not counting EXIT)
(add-primitive-code! (lambda () (add-entry! #f #f (forth_read_no_eof) (entry-data here-entry))))
(add-primitive-code! (entry-code (find-entry "]")))
(add-primitive-code! exit-addr)

(define (stop-compilation)
  (define (loop pos)
    (cond [(<= pos 0) (void)]
          [(= (get-int rstack #f pos) compiler-addr)
           (pop-int! rstack #f pos)
           (pop-int! rstack #f pos)
           (loop (- pos 2))] ; Pop off the place to go back to (the exit), as well as the link for colon-compiler
          [else (loop (sub1 pos))]))
  (loop (sub1 (/ (bytes-length (state-rstack (vector-ref cores state-index))) 4))))
(add-primitive-word! #t "[" stop-compilation)


;;;;;;;;;; NEW COMPILER FOR ARRAYFORTH ;;;;;;;;;;;;;;

;; Input:  Code, which by default we assume to be compiled.
;; If a word is an immediate word, it is executed instead.
;; To compile an immediate word, you need to postpone it.
;; Compiler directives are added in this section.

(define (add-compiled-code! code)
  (rvector-set! ram location-counter code)
  (set! location-counter (add1 location-counter)))

(define (compile code-port)
  (set! used-cores '())
  (rvector-copy! dict 0 primitive-dict)
  (for ([i (in-range num-cores)])
       (set! state-index i)
       (rvector-copy! rom 0 primitive-dict))
  (let [(old (current-input-port))]
    (current-input-port code-port)
    (set! execute? #f)
    (compile-loop)
    (current-input-port old)))

(define (compile-loop)
  (let [(to-compile (forth_read))]
    (unless (eof-object? to-compile)
	    (unless (eq? to-compile #\newline)
		    (let [(entry (find-entry to-compile))]
;		      (when entry (display (entry-name entry)) (display " ") (display (entry-precedence entry)) (newline))
		      (cond [(not entry)
			     (let [(num (string->bytes to-compile))]
			       (if num
				   (if execute?
				       (push-cells! dstack num)
				       (add-compiled-code! (lambda () (push-cells! dstack num))))
					;				   (begin
					;				     (add-compiled-code! (proc-ref codespace (entry-code (find-entry "@p"))))
					;				     (add-compiled-code! num))
				   (raise (string-append to-compile " ?"))))]
			    [(or execute? (entry-precedence entry))
			     ((proc-ref codespace (entry-code entry)))]
			    [(entry-primitive entry)
			     (add-compiled-code! (proc-ref codespace (entry-code entry)))]
			    [else
			     (add-compiled-code! (entry-code entry))] )))
	    (compile-loop))))

;; TODO:  Clear node's codespace
(add-primitive-word! #t "node"
		     (lambda ()
		       (set! state-index (pop-int! dstack #f))
		       (unless (member state-index used-cores)
			       (set! used-cores (cons state-index used-cores)))
		       (set! location-counter 0)
		       (rvector-copy! rom 0 primitive-dict)))

(add-primitive-word! #t "org"
		     (lambda ()
		       (set! location-counter (pop-int! dstack #f))))

(add-primitive-word! #t "yellow"
		     (lambda () (set! execute? #t)))

(add-primitive-word! #t "green"
		     (lambda () (set! execute? #f)))

;;;;;;;;;; CHANGES END HERE (mostly) ;;;;;;;;;;;;;;;;



(define (start-literal)
  (set! literal-mode 1)
  (set! lit-entry 0)
  (for* ([i (in-range 0 4)])
    (rvector-set! litspace i (lambda () (void)))))
(add-primitive-word! #t "{" start-literal)

(define (stop-literal)
  (set! literal-mode 0)
  (let [(code0 (rvector-ref litspace 0)) 
        (code1 (rvector-ref litspace 1)) 
        (code2 (rvector-ref litspace 2)) 
        (code3 (rvector-ref litspace 3))]
    (add-primitive-code! (lambda () 
                        (code0) 
                        (code1)
                        (code2)
                        (code3)
                        ))))

(add-primitive-word! #t "}" stop-literal)

(void (add-word! #f #t ";")) ; Can't be primitive since it uses multiple spaces in codespace
(add-primitive-code! (lambda () (add-primitive-code! exit-addr)))
(add-primitive-code! reveal-entry!)
(add-primitive-code! stop-compilation)
(add-primitive-code! exit-addr)


(define (immediate)
  (set-entry-precedence! (rvector-ref dict visible-address) #t))
(add-primitive-word! #f "immediate" immediate)


(define (postpone)
  (let* [(name (forth_read_no_eof))
         (entry (find-entry name))]
    (cond [(not entry)
           (raise (string-append name " ?"))]
          [(entry-precedence entry)
           (add-primitive-code! (entry-code entry))]
          [else
           (add-primitive-code! (lambda () (add-primitive-code! (entry-code entry))))])))
(add-primitive-word! #t "postpone" postpone)

(add-primitive-word! #f "reveal" reveal-entry!)

(add-primitive-word! #t "literal"
                     (lambda ()
                       (let [(num (pop-int! dstack #t))]
                         (add-primitive-code! (lambda () (push-int! dstack num))))))

; ,
(add-primitive-word! #t ","
                     (lambda ()
                       (let [(num (pop-int! dstack #t))]
                         (add-primitive-code! num))))

; Dictionary manipulation words

(define (forget name)
  (set! next-address (find-address name))
  (set! visible-address (sub1 next-address)))
(add-primitive-word! #f "forget" (lambda () (forget (forth_read_no_eof))))

(define (marker name)
  (let [(addr next-address)]
    (add-primitive-word! #f name (lambda () (set! next-address addr) (set! visible-address (sub1 next-address))))))
(add-primitive-word! #f "marker" (lambda () (marker (forth_read_no_eof))))

(define (load-primitive-file name)
  (call-with-input-file name
    (lambda (in)
      (let [(old-in (current-input-port))
            (old-out (current-output-port))]
        (current-input-port in)
        (current-output-port (open-output-string))
        (interpret)
        (close-output-port (current-output-port))
        (current-input-port old-in)
        (current-output-port old-out))))
  (void))

(define (run-file-with-cores name)
  (call-with-input-file name
    (lambda (in)
      (let* [(core-info (get-core-info-and-code in))]
        (interpret-cores core-info))))
  (void))

(define (run-file file-string)
  (let [(old-in (current-input-port))
	(old-out (current-output-port))]
    (current-input-port (open-input-file file-string))
    (current-output-port (open-output-string)) ; Discard the output
    (interpret)
    (close-input-port (current-input-port))
    (close-output-port (current-output-port))
    (current-input-port old-in)
    (current-output-port old-out)))

(define (run-file-with-output file-string)
  (let [(old-in (current-input-port))]
    (current-input-port (open-input-file file-string))
    (interpret)
    (close-input-port (current-input-port))
    (current-input-port old-in)))

(define make-core-info cons)
(define get-core-num car)
(define get-core-port cdr)

;(run-file "basewords.forth")
;(run-file-with-output "example.forth")
; Stub for get-core-info-and-code
(define (get-core-info-and-code input) (void))

(define (run-tests)
  (display "Start of test 1:") (newline)
  (interpret-cores
   (list
    (make-core-info 5 (open-input-string "1 2 .ns + .ns"))
    (make-core-info 40 (open-input-string "7 .ns .ns"))))
  (display "Start of test 2:") (newline)
  (interpret-cores
   (list
    (make-core-info 22 (open-input-string "9 dup if .ns ; then 1 .ns"))
    (make-core-info 143 (open-input-string "7 .ns .ns"))))
  (display "Start of test 3:") (newline)
  (interpret-cores
   (list
    (make-core-info 0 (open-input-string "1 2 + 1 .ns send .ns 2 recv .ns"))
    (make-core-info 1 (open-input-string ".ns 1 recv .ns - 2 send")))))

;(run-tests)

(define (run-compiler-tests)
  (let ((test-port (open-input-string "yellow 2 node
green 1 2 + 1 .ns send 2 recv .ns
yellow 44 node
green 1 recv .ns - .ns 2 send .ns")))
    (compile test-port)
    (code-loop))
  (reset-states!)
  (let ((test-port (open-input-string "yellow 2 node
green 1 2 3 4 5 6 7 8 .ns 9 .ns 10 .ns 11 .ns drop .ns drop .ns")))
    (compile test-port)
    (code-loop)))

(run-compiler-tests)
