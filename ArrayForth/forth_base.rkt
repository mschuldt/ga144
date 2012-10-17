#lang racket

(require "arithmetic.rkt" "forth_read.rkt" "forth_num_convert.rkt" "rvector.rkt" "forth_state.rkt" "forth_state_words.rkt" "forth_bit_words.rkt" "forth_io_words.rkt")
(provide interpret interpret-cores run-tests)

; Interpreter and associated procedures

; This code is ugly, because I don't know how to use Racket well.
(define (code-loop)
  (let [(ended-list
	 (map
	  (lambda (num)
	    (set! state-index num)
	    (if (= pc 0)
		#t
		(let [(code (proc-ref codespace pc))]
		  (current-input-port input)
		  (set! pc (add1 pc))
		  (with-handlers ([string? abort])
				 (if (number? code)
				     (execute-code code)
				     (code)))
		  #f)))
	  used-cores))]
    (unless (for/and ([bool ended-list]) bool)
	    (code-loop))))

(define (execute-code addr)
  (push-int! #:getter state-rstack #:setter set-state-rstack! pc) ; pc will be the address of the next instruction to execute
  (set! pc addr))

(define execute (compose execute-code entry-code))

(add-primitive-word! #f "ex" (lambda () (execute (rvector-ref dict (pop-int! #f)))))

(define (abort msg)
  (displaynl msg)
  (set-state-stack! (vector-ref cores state-index) (make-bytes 0))
  (quit))

(define (quit)
  (read-line) ; The rest of the line should not be used as input
  (set-state-rstack! (vector-ref cores state-index) (make-bytes 0))
  (set! pc interpreter-addr))
(add-primitive-word! #f "quit" quit)

(define (tick)
  (push-int! (find-address (forth_read_no_eof))))
(add-primitive-word! #f "'" tick)

(define (interpret-proc)
  (push-int! #:getter state-rstack #:setter set-state-rstack! (sub1 pc)) ; When interpret exits, go back to interpret itself.
  (let [(name (forth_read))]
    (if (eof-object? name)
        (set! pc 0)
        (if (eq? name #\newline)
            (displaynl " ok")
            (let [(entry (find-entry name))]
              (if entry
                  (execute entry)
                  (let [(num (string->bytes name))]
                    (if num
                        (push-cells! num)
                        (raise (string-append name " ?"))))))))))
(add-primitive-word! #f "interpret" interpret-proc)

(define (reset-states!)
  (for [(i (in-range 0 num-cores))]
       (set-state-stack! (vector-ref cores i) (make-bytes 0))
       (set-state-rstack! (vector-ref cores i) (make-bytes 0))
       (set-state-pc! (vector-ref cores i) interpreter-addr)))

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
  (push-int! #:getter state-rstack #:setter set-state-rstack! (sub1 pc)) ; When ] exits, go back to ] itself.
  (let [(to-compile (forth_read_no_eof))]
    (if (not (eq? to-compile #\newline))
        (let [(entry (find-entry to-compile))]
          (cond [(not entry)
                 (let [(num (string->bytes to-compile))]
                   (if num
                       (add-compiled-code! (lambda () (push-cells! num)))
                       (raise (string-append to-compile " ?"))))]
                [(entry-precedence entry)
                 (execute entry)]
                [(entry-primitive entry)
                 (add-compiled-code! (proc-ref codespace (entry-code entry)))]
                [else
                 (add-compiled-code! (entry-code entry))] ))
        (void))))
(add-primitive-word! #f "]" colon-compiler)

(define interpreter-addr (entry-code (find-entry "interpret")))
(define compiler-addr (entry-code (find-entry "]")))

; Colon definition - Uses the colon compiler
(void (add-word! #f #t ":")) ; Don't want Racket saying #<entry> when this is loaded
; Can't be primitive because it has more than one entry in the codespace (not counting EXIT)
(add-compiled-code! (lambda () (add-entry! #f #f (forth_read_no_eof) (entry-data here-entry))))
(add-compiled-code! (entry-code (find-entry "]")))
(add-compiled-code! exit-addr)

(define (stop-compilation)
  (define (loop pos)
    (cond [(<= pos 0) (void)]
          [(= (get-int #:stack (state-rstack (vector-ref cores state-index)) #f pos) compiler-addr)
           (pop-double! #:getter state-rstack #:setter set-state-rstack! #f pos)
           (loop (- pos 2))] ; Pop off the place to go back to (the exit), as well as the link for colon-compiler
          [else (loop (sub1 pos))]))
  (loop (sub1 (/ (bytes-length (state-rstack (vector-ref cores state-index))) 4))))
(add-primitive-word! #t "[" stop-compilation)

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
    (add-compiled-code! (lambda () 
                        (code0) 
                        (code1)
                        (code2)
                        (code3)
                        ))))

(add-primitive-word! #t "}" stop-literal)

(void (add-word! #f #t ";")) ; Can't be primitive since it uses multiple spaces in codespace
(add-compiled-code! (lambda () (add-compiled-code! exit-addr)))
(add-compiled-code! reveal-entry!)
(add-compiled-code! stop-compilation)
(add-compiled-code! exit-addr)


(define (immediate)
  (set-entry-precedence! (rvector-ref dict visible-address) #t))
(add-primitive-word! #f "immediate" immediate)


(define (postpone)
  (let* [(name (forth_read_no_eof))
         (entry (find-entry name))]
    (cond [(not entry)
           (raise (string-append name " ?"))]
          [(entry-precedence entry)
           (add-compiled-code! (entry-code entry))]
          [else
           (add-compiled-code! (lambda () (add-compiled-code! (entry-code entry))))])))
(add-primitive-word! #t "postpone" postpone)

(add-primitive-word! #f "reveal" reveal-entry!)

(add-primitive-word! #t "literal"
                     (lambda ()
                       (let [(num (pop-int! #t))]
                         (add-compiled-code! (lambda () (push-int! num))))))

; ,
(add-primitive-word! #t ","
                     (lambda ()
                       (let [(num (pop-int! #t))]
                         (add-compiled-code! num))))

; Dictionary manipulation words

(define (forget name)
  (set! next-address (find-address name))
  (set! visible-address (sub1 next-address)))
(add-primitive-word! #f "forget" (lambda () (forget (forth_read_no_eof))))

(define (marker name)
  (let [(addr next-address)]
    (add-primitive-word! #f name (lambda () (set! next-address addr) (set! visible-address (sub1 next-address))))))
(add-primitive-word! #f "marker" (lambda () (marker (forth_read_no_eof))))

; Control
(define (dummy-proc) (void))

; IF - 
; 1. Puts a procedure which jumps over one slot if TRUE is on the stack.
; 2. Puts HERE on the stack, and then fills the slot with a dummy procedure.
; This will later be replaced by an unconditional branch by ELSE or THEN.
(define (if-proc)
  (add-compiled-code!
   (lambda () (if (= (get-int #f) 0)
                  (void)
                  (set! pc (add1 pc)))))
  (push-int! (entry-data here-entry))
  (add-compiled-code! dummy-proc))
(add-primitive-word! #t "if" if-proc)

; -IF
; 1. Puts a procedure which jumps over one slot if the top of stack is negative.
; 2. Puts HERE on the stack, and then fills the slot with a dummy procedure.
; This will later be replaced by an unconditional branch by ELSE or THEN.
(define (nif-proc)
  (add-compiled-code!
   (lambda () (if (>= (get-int #t) 0)
                  (void)
                  (set! pc (add1 pc)))))
  (push-int! (entry-data here-entry))
  (add-compiled-code! dummy-proc))
(add-primitive-word! #t "-if" nif-proc)

; ELSE
; 1. Put HERE as the second item on the stack.  Fill it with a dummy procedure.
; This will be replaced with an unconditional branch by THEN.
; 2. Replace the dummy procedure put by IF with a conditional branch to HERE.
(define (else-proc)
  (push-int! (entry-data here-entry) 1)
  (add-compiled-code! dummy-proc)
  (let [(here-addr (entry-data here-entry))]
    (proc-replace! codespace (pop-int! #f) (lambda () (set! pc here-addr)))))
(add-primitive-word! #t "else" else-proc)

; THEN
; Put an unconditional branch to HERE.
; This will patch up the dummy procedure left by IF or ELSE.
(define (then-proc)
  (let [(here-addr (entry-data here-entry))]
    (proc-replace! codespace (pop-int! #f) (lambda () (set! pc here-addr)))))
(add-primitive-word! #t "then" then-proc)


; Loops
;; Removed LEAVE

; LOOP
(define (loop-proc)
  (let [(addr (pop-int! #f))]
    (add-compiled-code!
     (lambda ()
       (if (= (add1 (get-int #:stack (state-rstack (vector-ref cores state-index)) #t)) (get-int #:stack (state-rstack (vector-ref cores state-index)) #t 1))
           (pop-double! #:getter state-rstack #:setter set-state-rstack! #t)
           (begin (push-int! #:getter state-rstack #:setter set-state-rstack! (add1 (pop-int! #:getter state-rstack #:setter set-state-rstack! #t)))
                  (set! pc addr)))))))
(add-primitive-word! #t "loop" loop-proc)

; FOR
; 1. Add to compiled code the PUSH command.
; 2. Put HERE on the stack, to be used by NEXT.
(define (for-proc)
  (add-compiled-code!(push-proc))
  (push-int! (entry-data here-entry)))
(add-primitive-word! #t "for" for-proc)

  
; NEXT
; 1. Pop counter from the top of rstack.
; 2. If counter is 0, continue.
; 3. If counter is not 0, push counter-1 back to rstack and jump to label pushed by FOR
(define (next-loop-proc counter addr)
  (push-int! #:getter state-rstack #:setter set-state-rstack! (- counter 1))
  (set! pc addr))
  
(define (next-proc)
  (let [(addr (pop-int! #f))]
    (add-compiled-code! (lambda ()
                        (let [(counter (pop-int! #:getter state-rstack #:setter set-state-rstack! #t))]
                          (if (= counter 0)
                              (void)
                              (next-loop-proc counter addr)))))))

(add-primitive-word! #t "next" next-proc)

; MICRO NEXT. Loop to the beginning of I, the current instruction word, instead of FOR.
;; TODO: need assertion that instructions between FOR and UNEXT 3 slots and start at slot 0.
(add-primitive-word! #t "unext" next-proc)

; BEGIN
; Put HERE on the stack, to be used by UNTIL or REPEAT.
(add-primitive-word! #t "begin" (lambda () (push-int! (entry-data here-entry))))

; UNTIL
; Jumps back to the address left by BEGIN if it sees a false flag.
(define (until-proc)
  (let [(addr (pop-int! #f))]
    (add-compiled-code! (lambda ()
                          (if (= (pop-int! #t) 0)
                              (set! pc addr)
                              (void))))))
(add-primitive-word! #t "until" until-proc)

; WHILE
; Does the same thing as IF.
; BEGIN - WHILE - REPEAT is like BEGIN - IF - LOOP THEN
(add-primitive-word! #t "while" if-proc)

(add-primitive-word! #f "?dup" (lambda () (if (= 0 (get-int #f))
                                              (void)
                                              (push-cells! (get-cells)))))

(add-primitive-word! #t "abort\""
                     (lambda () (let [(str (read-string))]
                                  (add-compiled-code!
                                   (lambda () (if (= (pop-int! #t) false)
                                                  (void)
                                                  (raise str)))))))

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

;; (run-tests)
