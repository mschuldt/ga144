#lang racket

(require "forth_read.rkt" "forth_num_convert.rkt" "rvector.rkt")

(provide (all-defined-out))

; TODO: Make an rvector with default always #f (right now, when expanded, will have default 0).

; Hack for @p { .. }
;(define literal-mode 0)
;(define litspace (make-rvector 100))
;(define lit-entry 0)

(define num-cores 144)
;(define cores (make-vector num-cores))

; State for a core
; Currently, stacks and dict are infinite.
; Once a size is determined, a dict can be just a vector, not an rvector.
; Need to add ports for each core.
; Use Racket ports?  Probably not, since we want to only have 1 word at a time.

(struct state (stack rstack pc rega regb input rom ram) #:mutable)
; Codespace - somewhat like assembly instructions
;; TODO: make-core, which puts garbage in the stacks and dict
;; TODO: regb is initialized to io

(define (make-zeroed-core)
  (state (make-bytes 0) (make-bytes 0) 0 0 0 '() (make-rvector 100) (make-rvector 100)))

(struct interpreter-struct (primitive-dict dict codespace next-address visible-address cores state-index literal-mode litspace lit-entry send-recv-table cstack) #:mutable)

(define (make-interpreter)
  (let ((core-list (make-vector num-cores))
	(srtable (make-rvector 100)))
    (for [(i (in-range 0 num-cores))]
	 (vector-set! core-list i (make-zeroed-core)))
    (for ([i (in-range 100)])
	 (rvector-set! srtable i #f))
    (interpreter-struct (make-rvector 100) (make-rvector 100) (make-rvector 500) 1 0 core-list 0 0 (make-rvector 100) 0 srtable (make-bytes 0))))

(define interpreter (make-interpreter))

(struct compiler-struct (location-counter execute?) #:mutable)

(define compiler (compiler-struct 0 #f))

(define-syntax-rule (generate-compiler-macro name getter setter)
  (define-syntax name
    (syntax-id-rules (set!)
      [(set! name x) (setter compiler x)]
      [name (getter compiler)])))

(generate-compiler-macro location-counter compiler-struct-location-counter set-compiler-struct-location-counter!)
(generate-compiler-macro execute? compiler-struct-execute? set-compiler-struct-execute?!)

(define-syntax-rule (generate-interpreter-macro name getter setter)
  (define-syntax name
    (syntax-id-rules (set!)
      [(set! name x) (setter interpreter x)]
      [name (getter interpreter)])))

#| Doesn't work, it appears that the lexical scoping means the the generated macro is not visible elsewhere.
(define-syntax gen-i-macro
  (lambda (stx)
    (syntax-case stx ()
      [(gen-i-macro name)
       (let* ((sym (syntax->datum #'name))
	      (getter (string->symbol
		       (string-append "intepreter-struct-"
				      (symbol->string sym))))
	      (setter (string->symbol
		       (string-append "set-intepreter-struct-"
				      (symbol->string sym) "!"))))
	 #`(define-syntax #,sym
	     (syntax-id-rules (set!)
			      [(set! #,sym x) (#,setter interpreter x)]
			      [#,sym (#,getter interpreter)])))])))
(gen-i-macro codespace)
|#

(generate-interpreter-macro primitive-dict interpreter-struct-primitive-dict set-interpreter-struct-primitive-dict!)
(generate-interpreter-macro dict interpreter-struct-dict set-interpreter-struct-dict!)
(generate-interpreter-macro codespace interpreter-struct-codespace set-interpreter-struct-codespace!)
(generate-interpreter-macro next-address interpreter-struct-next-address set-interpreter-struct-next-address!)
(generate-interpreter-macro visible-address interpreter-struct-visible-address set-interpreter-struct-visible-address!)
(generate-interpreter-macro cores interpreter-struct-cores set-interpreter-struct-cores!)
(generate-interpreter-macro state-index interpreter-struct-state-index set-interpreter-struct-state-index!)
(generate-interpreter-macro literal-mode interpreter-struct-literal-mode set-interpreter-struct-literal-mode!)
(generate-interpreter-macro litspace interpreter-struct-litspace set-interpreter-struct-litspace!)
(generate-interpreter-macro lit-entry interpreter-struct-lit-entry set-interpreter-struct-lit-entry!)
(generate-interpreter-macro send-recv-table interpreter-struct-send-recv-table set-interpreter-struct-send-recv-table!)

(define-syntax-rule (generate-core-macro name getter setter)
  (define-syntax name
    (syntax-id-rules (set!)
      [(set! name x) (setter (vector-ref cores state-index) x)]
      [name (getter (vector-ref cores state-index))])))

(generate-core-macro pc state-pc set-state-pc!)
(generate-core-macro rega state-rega set-state-rega!)
(generate-core-macro regb state-regb set-state-regb!)
(generate-core-macro input state-input set-state-input!)
;(generate-core-macro stk state-stack set-state-stack!)
;(generate-core-macro rstk state-rstack set-state-rstack!)
(generate-core-macro rom state-rom set-state-rom!)
(generate-core-macro ram state-ram set-state-ram!)

; Stacks
(define (push-cells! #:getter [getter state-stack] #:setter [setter set-state-stack!] bstr [pos 0])
  (setter (vector-ref cores state-index) (bytes-append (subbytes (getter (vector-ref cores state-index)) 0 (* pos 4))
			       bstr
			       (subbytes (getter (vector-ref cores state-index)) (* pos 4)))))
(define (push-int! #:getter [getter state-stack] #:setter [setter set-state-stack!] num [pos 0])
  (push-cells! #:getter getter #:setter setter (int->bytes num) pos))
(define (push-double! #:getter [getter state-stack] #:setter [setter set-state-stack!] num [pos 0])
  (push-cells! #:getter getter #:setter setter (double->bytes num) pos))

(define (get-cells #:stack [stack (state-stack (vector-ref cores state-index))] [start 0] [end 1])
  (if (< (bytes-length stack) (* end 4))
      (raise "Stack underflow")
      (subbytes stack (* start 4) (* end 4))))
(define (get-2cells #:stack [stack (state-stack (vector-ref cores state-index))] [pos 0])
  (get-cells #:stack stack pos (+ pos 2)))
(define (get-int #:stack [stack (state-stack (vector-ref cores state-index))] signed? [pos 0])
  (integer-bytes->integer (get-cells #:stack stack pos (+ pos 1)) signed? #t))
(define (get-double #:stack [stack (state-stack (vector-ref cores state-index))] signed? [pos 0])
  (integer-bytes->integer (get-2cells #:stack stack pos) signed? #t))

(define (pop-cells! #:getter [getter state-stack] #:setter [setter set-state-stack!] [start 0] [end 1])
  (if (< (bytes-length (getter (vector-ref cores state-index))) (* end 4))
      (raise "Stack underflow")
      (let [(res (subbytes (getter (vector-ref cores state-index)) (* start 4) (* end 4)))]
	(setter (vector-ref cores state-index) (bytes-append (subbytes (getter (vector-ref cores state-index)) 0 (* start 4))
				     (subbytes (getter (vector-ref cores state-index)) (* end 4))))
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
    (rvector-set! primitive-dict next-address new)
    (rvector-set! dict next-address new)
    (set! next-address (add1 next-address))
    new))

; Create the HERE variable first, so that it can be used by other
; procedures that manipulate the dictionary and the codespace.
;(define here-entry
;  (let [(addr next-address)]
;    (rvector-set! codespace 1 (lambda () (push-int! addr))) ;; TODO(codespace)
;    (add-entry! #t #f "here" 1 2))) ; a primitive variable named "here" whose code starts at address 1 and whose value is 2.

(define here-entry (add-entry! #t #f "here" 1 1)) ; a primitive variable named "here" whose code starts at address 1, value is 1.
; Code to deal with "here" will be inserted later.

(define (add-to-codespace proc-or-addr)
  ;(printf "add-compile ~e\n" (entry-data here-entry))
  (proc-add! codespace (entry-data here-entry) proc-or-addr)
  (set-entry-data! here-entry (add1 (entry-data here-entry))))
  
(define (add-to-litspace proc-or-addr)
  (rvector-set! litspace lit-entry proc-or-addr) 
  (set! lit-entry (add1 lit-entry)))

(define (add-primitive-code! proc-or-addr)
  (if (= literal-mode 0)
      (add-to-codespace proc-or-addr)
      (add-to-litspace proc-or-addr)))

(define exit-addr 3) ; Kind of hacky, but not too bad.
; It's obvious that it will be at address 3.

(define (exit)
  (pop-int! #:getter state-rstack #:setter set-state-rstack! #f) ; Don't return to wherever exit came from
  (set! pc (pop-int! #:getter state-rstack #:setter set-state-rstack! #f)))

(define (reveal-entry!)
  (set! visible-address (sub1 next-address)))

; Now we can add the code for HERE.
(add-primitive-code! (lambda () (push-int! next-address))) ; Adds this code to codespace at address 1 (value of HERE)
(add-primitive-code! exit-addr) ; Adding the EXIT for HERE.
(reveal-entry!)

(define (add-and-reveal-entry! prim prec name code data)
  (let [(entry (add-entry! prim prec name code data))]
    (reveal-entry!)
    entry))

(define (add-word! prim prec name [data '()])
  (add-and-reveal-entry! prim prec name (entry-data here-entry) data))
(void (add-word! #f #f "exit"))
(add-primitive-code! exit)

(define (add-primitive-word! prec name code [data '()])
  (add-word! #t prec name data)
  (add-primitive-code! code)
  (add-primitive-code! exit-addr)) ; To prevent Racket from spewing a bunch of #<entry> when the file is loaded.

(define (add-compiled-word! prec name)
  (let [(new (entry #f prec name (entry-data here-entry)))]
    (rvector-set! dict next-address new)
    (set! next-address (add1 next-address))))

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
