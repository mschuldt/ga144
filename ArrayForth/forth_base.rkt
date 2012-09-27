#lang racket

(require "forth_read.rkt" "forth_num_convert.rkt" "rvector.rkt")
(provide interpret)

(define (displaynl arg)
  (display arg)
  (newline))

(define (displayspace arg)
  (display arg)
  (display " "))

; State for a core
; Currently, stacks and dict are infinite.
; Once a size is determined, a dict can be just a vector, not an rvector.
; Need to add ports for each core.
; Use Racket ports?  Probably not, since we want to only have 1 word at a time.

(struct state (stack rstack codespace dict next-address visible-address pc rega regb) #:mutable)

; Codespace - somewhat like assembly instructions
;; TODO: make-core, which puts garbage in the stacks and dict
;; TODO: regb is initialized to io

(define (make-zeroed-core)
  (state (make-bytes 0) (make-bytes 0) (make-rvector 500) (make-rvector 100) 1 0 0 0 0))

(define cores (make-vector 144))
(for ((i (in-range 0 144)))
     (vector-set! cores i (make-zeroed-core)))

(define state-index 0)
(define current-state (vector-ref cores state-index))

(define-syntax-rule (generate-macro name getter setter)
  (define-syntax name
    (syntax-id-rules (set!)
      [(set! name x) (setter current-state x)]
      [name (getter current-state)])))

(generate-macro codespace state-codespace set-state-codespace!)
(generate-macro dict state-dict set-state-dict!)
(generate-macro next-address state-next-address set-state-next-address!)
(generate-macro visible-address state-visible-address set-state-visible-address!)
(generate-macro pc state-pc set-state-pc!)
(generate-macro rega state-rega set-state-rega!)
(generate-macro regb state-regb set-state-regb!)

; Stacks
(define (push-cells! #:getter [getter state-stack] #:setter [setter set-state-stack!] bstr [pos 0])
  (setter current-state (bytes-append (subbytes (getter current-state) 0 (* pos 4))
			       bstr
			       (subbytes (getter current-state) (* pos 4)))))
(define (push-int! #:getter [getter state-stack] #:setter [setter set-state-stack!] num [pos 0])
  (push-cells! #:getter getter #:setter setter (int->bytes num) pos))
(define (push-double! #:getter [getter state-stack] #:setter [setter set-state-stack!] num [pos 0])
  (push-cells! #:getter getter #:setter setter (double->bytes num) pos))

(define (get-cells #:stack [stack (state-stack current-state)] [start 0] [end 1])
  (if (< (bytes-length stack) (* end 4))
      (raise "Stack underflow")
      (subbytes stack (* start 4) (* end 4))))
(define (get-2cells #:stack [stack (state-stack current-state)] [pos 0])
  (get-cells #:stack stack pos (+ pos 2)))
(define (get-int #:stack [stack (state-stack current-state)] signed? [pos 0])
  (integer-bytes->integer (get-cells #:stack stack pos (+ pos 1)) signed? #t))
(define (get-double #:stack [stack (state-stack current-state)] signed? [pos 0])
  (integer-bytes->integer (get-2cells #:stack stack pos) signed? #t))

(define (pop-cells! #:getter [getter state-stack] #:setter [setter set-state-stack!] [start 0] [end 1])
  (if (< (bytes-length (getter current-state)) (* end 4))
      (raise "Stack underflow")
      (let [(res (subbytes (getter current-state) (* start 4) (* end 4)))]
	(setter current-state (bytes-append (subbytes (getter current-state) 0 (* start 4))
				     (subbytes (getter current-state) (* end 4))))
	res)))
(define (pop-2cells! #:getter [getter state-stack] #:setter [setter set-state-stack!] [pos 0])
  (pop-cells! #:getter getter #:setter setter pos (+ pos 2)))
(define (pop-int! #:getter [getter state-stack] #:setter [setter set-state-stack!] signed? [pos 0])
  (integer-bytes->integer (pop-cells! #:getter getter #:setter setter pos (+ pos 1)) signed? #t))
(define (pop-double! #:getter [getter state-stack] #:setter [setter set-state-stack!] signed? [pos 0])
  (integer-bytes->integer (pop-2cells! #:getter getter #:setter setter pos) signed? #t))
; Debugging

(define (print-stack stack)
  (define (loop pos)
    (if (>= pos 0)
        (begin (print (get-int #:stack stack #t pos))
               (display " ")
               (loop (sub1 pos)))
        (void)))
  (display "| ")
  (loop (sub1 (/ (bytes-length stack) 4)))
  (display ">"))


; Entry for the dictionary.  Code must be mutable to allow procs which refer to the entry itself.
(struct entry (primitive [precedence #:mutable] name [code #:mutable] [data #:mutable]))

; Dictionary
(define (add-entry! prim prec name code [data '()])
  (let [(new (entry prim prec name code data))]
    (rvector-set! dict next-address new)
    (set! next-address (add1 next-address))
    new))

; Create the HERE variable first, so that it can be used by other
; procedures that manipulate the dictionary and the codespace.
(define here-entry
  (let [(addr next-address)]
    (rvector-set! codespace 1 (lambda () (push-int! addr)))
    (add-entry! #t #f "here" 1 2))) ; a primitive variable named "here" whose code starts at address 1 and whose value is 2.

(define (add-compiled-code! proc-or-addr)
  ;(printf "add-compile ~e\n" (entry-data here-entry))
  (rvector-set! codespace (entry-data here-entry) proc-or-addr)
  (set-entry-data! here-entry (add1 (entry-data here-entry)))
  )

(define exit-addr 3) ; Kind of hacky, but not too bad.
; It's obvious that it will be at address 3.

(define (exit)
  (pop-int! #:getter state-rstack #:setter set-state-rstack! #f) ; Don't return to wherever exit came from
  (set! pc (pop-int! #:getter state-rstack #:setter set-state-rstack! #f)))
(add-compiled-code! exit-addr) ; The word HERE also has to have an EXIT

(define (reveal-entry!)
  (set! visible-address (sub1 next-address)))

(define (add-and-reveal-entry! prim prec name code data)
  (let [(entry (add-entry! prim prec name code data))]
    (reveal-entry!)
    entry))

(define (add-word! prim prec name [data '()])
  (add-and-reveal-entry! prim prec name (entry-data here-entry) '()))

(define (add-primitive-word! prec name code [data '()])
  (add-word! #t prec name data)
  (add-compiled-code! code)
  (add-compiled-code! exit-addr)) ; To prevent Racket from spewing a bunch of #<entry> when the file is loaded.
(void (add-word! #f #f "exit"))
(add-compiled-code! exit)

(define (find-address name)
  (define (loop address)
    (let [(word (rvector-ref dict address))]
      (cond [(string-ci=? name (entry-name word)) address]
            [(= address 1) #f]
            [else (loop (sub1 address))])))
  (loop visible-address))

(define (find-entry name)
  (let [(address (find-address name))]
    (if address
        (rvector-ref dict address)
        #f)))

; Interpreter and associated procedures

(define (code-loop)
  (if (= pc 0)
      'Exiting
      (let [(code (rvector-ref codespace pc))]
        (set! pc (add1 pc))
        (with-handlers ([string? abort])
          (if (number? code)
              (execute-code code)
              (code)))
        (code-loop))))

(define (execute-code addr)
  (push-int! #:getter state-rstack #:setter set-state-rstack! pc) ; pc will be the address of the next instruction to execute
  (set! pc addr))

(define execute (compose execute-code entry-code))

(add-primitive-word! #f "ex" (lambda () (execute (rvector-ref dict (pop-int! #f)))))

(define (abort msg)
  (displaynl msg)
  (set-state-stack! current-state (make-bytes 0))
  (quit))

(define (quit)
  (read-line) ; The rest of the line should not be used as input
  (set-state-rstack! current-state (make-bytes 0))
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

(define (interpret)
  (set-state-stack! current-state (make-bytes 0))
  (set-state-rstack! current-state (make-bytes 0))
  (set! pc interpreter-addr)
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
                 (add-compiled-code! (rvector-ref codespace (entry-code entry)))]
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
          [(= (get-int #:stack (state-rstack current-state) #f pos) compiler-addr)
           (pop-double! #:getter state-rstack #:setter set-state-rstack! #f pos)
           (loop (- pos 2))] ; Pop off the place to go back to (the exit), as well as the link for colon-compiler
          [else (loop (sub1 pos))]))
  (loop (sub1 (/ (bytes-length (state-rstack current-state)) 4))))
(add-primitive-word! #t "[" stop-compilation)

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
   (lambda () (if (= (pop-int! #f) 0)
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
   (lambda () (if (>= (pop-int! #t) 0)
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
    (rvector-set! codespace (pop-int! #f) (lambda () (set! pc here-addr)))))
(add-primitive-word! #t "else" else-proc)

; THEN
; Put an unconditional branch to HERE.
; This will patch up the dummy procedure left by IF or ELSE.
(define (then-proc)
  (let [(here-addr (entry-data here-entry))]
    (rvector-set! codespace (pop-int! #f) (lambda () (set! pc here-addr)))))
(add-primitive-word! #t "then" then-proc)


; Loops
;; Removed LEAVE

; DO
(define (do-proc)
  (add-compiled-code! (lambda () (push-cells! #:getter state-rstack #:setter set-state-rstack! (pop-cells! 0 2))))
  (push-int! (entry-data here-entry)))
(add-primitive-word! #t "do" do-proc)

; LOOP
(define (loop-proc)
  (let [(addr (pop-int! #f))]
    (add-compiled-code!
     (lambda ()
       (if (= (add1 (get-int #:stack (state-rstack current-state) #t)) (get-int #:stack (state-rstack current-state) #t 1))
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

; +LOOP
(define (plusloop-proc)
  (let [(addr (pop-int! #f))]
    (add-compiled-code!
     (lambda ()
       (let [(n (pop-int! #t))
             (old (pop-int! #:getter state-rstack #:setter set-state-rstack! #t))
             (limit (get-int #:stack (state-rstack current-state) #t))]
         (let [(new (+ n old))]
           (if (and (< (min old new) limit) (>= (max old new) limit))
               (pop-int! #:getter state-rstack #:setter set-state-rstack! #t) ; Remove the limit (index already removed)
               (begin (push-int! #:getter state-rstack #:setter set-state-rstack! new)
                      (set! pc addr)))))))))
(add-primitive-word! #t "+loop" plusloop-proc)

; UNLOOP
(add-primitive-word! #f "unloop" (lambda () (pop-double! #:getter state-rstack #:setter set-state-rstack! #t)))

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

; REPEAT
(define (repeat-proc)
  (let [(addr (pop-int! #f 1))] ; Get the second address (the one left by BEGIN)
    (add-compiled-code! (lambda () (set! pc addr)))
    (then-proc)))
(add-primitive-word! #t "repeat" repeat-proc)

; Comments
(define (comment)
  (if (equal? (read-char) #\))
      (void)
      (comment)))
(add-primitive-word! #t "(" comment)


; Constants
(define get-constant-value entry-data)
(define (constant)
  (let* [(name (forth_read_no_eof))
         (data (pop-cells!))]
    (add-primitive-word! #f name
                         (lambda () (push-cells! data))
                         data)))
(add-primitive-word! #f "constant" constant)
(define (2constant)
  (let* [(name (forth_read_no_eof))
         (data (pop-2cells!))]
    (add-primitive-word! #f name
                           (lambda () (push-cells! data))
                           data)))
(add-primitive-word! #f "2constant" 2constant)


; Stack manipulation words
(define (swap)
  (let* [(arg1 (pop-cells!))
         (arg2 (pop-cells!))]
    (push-cells! arg1)
    (push-cells! arg2)))
(add-primitive-word! #f "swap" swap)

(define (dup)
  (push-cells! (get-cells))) ; Get the first cell and push it back on
(add-primitive-word! #f "dup" dup)

(define (over)
  (push-cells! (get-cells 1 2)))
(add-primitive-word! #f "over" over)

(define (rot)
  (push-cells! (pop-cells! 2 3)))

(add-primitive-word! #f "rot" rot)

(define (drop)
  (pop-cells!))
(add-primitive-word! #f "drop" drop)

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


; rstack manipulation words

(define (push-proc) 
  (lambda () (push-cells! #:getter state-rstack #:setter set-state-rstack! (pop-cells!))))
(define (pop-proc)
  (lambda () (push-cells! (pop-cells! #:getter state-rstack #:setter set-state-rstack!))))

(add-primitive-word! #f ">r" (push-proc))
(add-primitive-word! #f "push" (push-proc))
(add-primitive-word! #f "r>" (pop-proc))
(add-primitive-word! #f "pop" (pop-proc))
(add-primitive-word! #f "r@" (lambda () (push-cells! (get-cells #:stack (state-rstack current-state)))))

(add-primitive-word! #f "i" (lambda () (push-cells! (get-cells #:stack (state-rstack current-state)))))
(add-primitive-word! #f "j" (lambda () (push-cells! (get-cells #:stack (state-rstack current-state) 2 3))))

(add-primitive-word! #f "2>r" (lambda () (push-cells! #:getter state-rstack #:setter set-state-rstack! (pop-cells! 0 2))))
(add-primitive-word! #f "2r>" (lambda () (push-cells! (pop-cells! #:getter state-rstack #:setter set-state-rstack! 0 2))))
(add-primitive-word! #f "2r@" (lambda () (push-cells! (get-cells #:stack (state-rstack current-state) 0 2))))

; register manipulation

; fetch via register
(add-primitive-word! #f "@+"
                     (lambda ()
                         (push-int! (rvector-ref codespace (state-rega current-state)))
                         (set-state-rega! current-state (+ (state-rega current-state) 1))))
(add-primitive-word! #f "@" (lambda () (push-int! (rvector-ref codespace (state-rega current-state)))))
(add-primitive-word! #f "@b" (lambda () (push-int! (rvector-ref codespace (state-regb current-state)))))

; store via register

;; TODO: check if !p does the rigth thing.
(add-primitive-word! #f "!p" 
                     (lambda () 
                       (rvector-set! codespace (state-pc current-state) (pop-cells!))
                       (set-state-pc! current-state (+ (state-pc current-state) 1))))
(add-primitive-word! #f "!+" 
                     (lambda () 
                       (rvector-set! codespace (state-rega current-state) (pop-int! #t))
                       (set-state-rega! current-state (+ (state-rega current-state) 1))))
(add-primitive-word! #f "!" (lambda () (rvector-set! codespace (state-rega current-state) (pop-int! #t))))
(add-primitive-word! #f "!b" (lambda () (rvector-set! codespace (state-regb current-state) (pop-int! #t))))

; fetch from register
(add-primitive-word! #f "a" (lambda () (push-int! (state-rega current-state))))

; store to register
(add-primitive-word! #f "a!" (lambda () (set-state-rega! current-state (pop-int! #f))))
(add-primitive-word! #f "b!" (lambda () (set-state-regb! current-state (pop-int! #f))))

; Math

; Addition - adds 2 ints, pushes it back onto the stack.  Treated as signed, but works for unsigned as well.
(add-primitive-word! #f "+"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (+ arg1 arg2)))))

; Normal minus
(add-primitive-word! #f ".-"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (- arg2 arg1)))))

; Invert
(add-primitive-word! #f "-"
                     (lambda ()
                       (let* [(arg (pop-int! #t))]
                         (push-int! (- -1 arg)))))

; Multiply step. Add S to T if A0 = 1 then shift T and A right.
;(add-primitive-word! #f "+*"
;                     (lambda ()
;                       (let* [(a (state-rega current-state))]
;                         (if (= (or a 1) 1)
;                             (push-int! (+ (pop-int!) (get-int)))
;                             (void))
;                         (let* [(t (pop-int!))]
;                           (if (= (or t 1) 1)
;                             (set-state-rega! current-state (+ (<< 1 17) (>> a 1)))
;                             (set-state-rega! current-state (>> a 1)))
;                           (push-int! (>> t 1))))))

(add-primitive-word! #f "*"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (* arg1 arg2)))))

(add-primitive-word! #f "/"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (quotient arg2 arg1)))))

(add-primitive-word! #f "mod"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (remainder arg2 arg1)))))

(add-primitive-word! #f "/mod"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (remainder arg2 arg1))
                         (push-int! (quotient arg2 arg1)))))

(add-primitive-word! #f "*/"
                     (lambda ()
                       (let* [(n3 (pop-int! #t))
                              (n2 (pop-int! #t))
                              (n1 (pop-int! #t))
                              (intermediate (* n1 n2))]
                         (push-int! (quotient intermediate n3)))))

(add-primitive-word! #f "*/mod"
                     (lambda ()
                       (let* [(n3 (pop-int! #t))
                              (n2 (pop-int! #t))
                              (n1 (pop-int! #t))
                              (intermediate (* n1 n2))]
                         (push-int! (remainder intermediate n3))
                         (push-int! (quotient intermediate n3)))))

(add-primitive-word! #f "min"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (min arg2 arg1)))))

(add-primitive-word! #f "max"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (max arg2 arg1)))))

(add-primitive-word! #f "um*"
                     (lambda ()
                       (let* [(arg1 (pop-int! #f))
                              (arg2 (pop-int! #f))]
                         (push-double! (* arg2 arg1)))))

(add-primitive-word! #f "um/mod"
                     (lambda ()
                       (let* [(arg1 (pop-int! #f))
                              (arg2 (pop-double! #f))]
                         (push-int! (remainder arg2 arg1))
                         (push-int! (quotient arg2 arg1)))))

(add-primitive-word! #f "d+"
                     (lambda ()
                       (let* [(arg1 (pop-double! #t))
                              (arg2 (pop-double! #t))]
                         (push-double! (+ arg1 arg2)))))

(add-primitive-word! #f "d-"
                     (lambda ()
                       (let* [(arg1 (pop-double! #t))
                              (arg2 (pop-double! #t))]
                         (push-double! (- arg2 arg1)))))

; Output

; Displays an int, interpreted as a signed number
(add-primitive-word! #f "." (lambda () (displayspace (pop-int! #t))))
; Displays an int, interpreted as an unsigned number
(add-primitive-word! #f "u." (lambda () (displayspace (pop-int! #f))))
; Displays a double, interpreted as a signed number
(add-primitive-word! #f "d." (lambda () (displayspace (pop-double! #t))))
; Displays a double, interpreted as an unsigned number
(add-primitive-word! #f "du." (lambda () (displayspace (pop-double! #f))))

; Compiles a string and displays it upon execution.
; Note: Can only be used in the colon compiler.
(define (read-string)
  (define (iter lst)
    (let [(new-char (read-char))]
      (if (eq? new-char #\")
          (list->string lst)
          (iter (append lst (list new-char))))))
  (iter '()))

(define (dot-quote)
  (let [(str (read-string))]
    (add-compiled-code! (lambda () (display str)))))
(add-primitive-word! #t ".\"" dot-quote)

(add-primitive-word! #f "cr" newline)
(add-primitive-word! #f "space" (lambda () (display " ")))

(define (spaces)
  (define (loop num)
    (if (= num 0)
        'done
        (begin (display " ") (loop (sub1 num)))))
  (loop (pop-int! #f)))
(add-primitive-word! #f "spaces" spaces)

(add-primitive-word! #f "emit"
                     (lambda () (display (integer->char (pop-int! #f)))))


; Booleans

(define true -1)
(define false 0)

(add-primitive-word! #f "true" (lambda () (push-int! true)))
(add-primitive-word! #f "false" (lambda () (push-int! false)))

(add-primitive-word! #f ">"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (if (> arg2 arg1) true false)))))

(add-primitive-word! #f "<"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (if (< arg2 arg1) true false)))))

(add-primitive-word! #f "u<"
                     (lambda ()
                       (let* [(arg1 (pop-int! #f))
                              (arg2 (pop-int! #f))]
                         (push-int! (if (< arg2 arg1) true false)))))

(add-primitive-word! #f "="
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (if (= arg2 arg1) true false)))))

(add-primitive-word! #f "0=" (lambda () (push-int! (if (= (pop-int! #t) 0) true false))))
(add-primitive-word! #f "0<" (lambda () (push-int! (if (< (pop-int! #t) 0) true false))))
(add-primitive-word! #f "0>" (lambda () (push-int! (if (> (pop-int! #t) 0) true false))))

(add-primitive-word! #f "and"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (bitwise-and arg1 arg2)))))

(add-primitive-word! #f "or"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
                         (push-int! (bitwise-ior arg1 arg2))))) ; ior - inclusive or

(add-primitive-word! #f "invert" (lambda () (push-int! (bitwise-not (pop-int! #t)))))

; ?stack is supposed to check for stack underflow.  However, here Racket's own
; error checking will notice the stack underflow, so it has to be checked on every
; access.  As a result, whenever you manually check the stack, it will be fine.
(add-primitive-word! #f "?stack" (lambda () false))

(add-primitive-word! #f "?dup" (lambda () (if (= 0 (get-int #f))
                                              (void)
                                              (push-cells! (get-cells)))))

(add-primitive-word! #t "abort\""
                     (lambda () (let [(str (read-string))]
                                  (add-compiled-code!
                                   (lambda () (if (= (pop-int! #t) false)
                                                  (void)
                                                  (raise str)))))))

(add-primitive-word! #f ".s" (lambda () (print-stack (state-stack current-state))))

(add-primitive-word! #f ".r" (lambda () (print-stack (state-rstack current-state))))

(let [(old-in (current-input-port))
      (old-out (current-output-port))]
  (current-input-port (open-input-file "example.forth"))
  (current-output-port (open-output-string)) ; Discard the output
  (interpret)
  (close-input-port (current-input-port))
  (close-output-port (current-output-port))
  (current-input-port old-in)
  (current-output-port old-out))
