#lang racket

(require "forth_read.rkt" "forth_num_convert.rkt" "rvector.rkt" "circular-stack.rkt")

(provide (all-defined-out))

(define num-cores 144)

; State for a core
; Currently, stacks and dict are infinite.
; Once a size is determined, a dict can be just a vector, not an rvector.
; Need to add ports for each core.
; Use Racket ports?  Probably not, since we want to only have 1 word at a time.

(struct state (dstack rstack pc rega regb memory) #:mutable)

(define (make-dstack) (make-stack 2 8 (integer->integer-bytes -1 4 #t)))
(define (make-rstack) (make-stack 1 8 (integer->integer-bytes -1 4 #t)))

;; TODO: regb is initialized to io
(define (make-new-core)
  (state (make-dstack) (make-rstack) 0 0 0 (make-rvector 100 -1)))

(struct interpreter-struct 
	(codespace cores state-index send-recv-table)
	#:mutable)

(define (make-interpreter)
  (let ((core-list (make-vector num-cores))
	(srtable (make-rvector 100 -1)))
    (for [(i (in-range 0 num-cores))]
	 (vector-set! core-list i (make-new-core)))
    (for ([i (in-range 100)])
	 (rvector-set! srtable i #f))
    (interpreter-struct (make-rvector 500 -1) core-list 0 srtable)))

(struct compiler-struct (compiler-directives dict location-counter execute? cstack literal-mode litspace lit-entry) #:mutable)

(define-syntax-rule (generate-compiler-macro name getter setter)
  (define-syntax name
    (syntax-id-rules (set!)
      [(set! name x) (setter compiler x)]
      [name (getter compiler)])))

(generate-compiler-macro compiler-directives compiler-struct-compiler-directives set-compiler-struct-compiler-directives!)
(generate-compiler-macro dict compiler-struct-dict set-compiler-struct-dict!)
(generate-compiler-macro location-counter compiler-struct-location-counter set-compiler-struct-location-counter!)
(generate-compiler-macro execute? compiler-struct-execute? set-compiler-struct-execute?!)
(generate-compiler-macro cstack compiler-struct-cstack set-compiler-struct-cstack!)
(generate-compiler-macro literal-mode compiler-struct-literal-mode set-compiler-struct-literal-mode!)
(generate-compiler-macro litspace compiler-struct-litspace set-compiler-struct-litspace!)
(generate-compiler-macro lit-entry compiler-struct-lit-entry set-compiler-struct-lit-entry!)

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

(generate-interpreter-macro codespace interpreter-struct-codespace set-interpreter-struct-codespace!)
(generate-interpreter-macro cores interpreter-struct-cores set-interpreter-struct-cores!)
(generate-interpreter-macro state-index interpreter-struct-state-index set-interpreter-struct-state-index!)
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
(generate-core-macro dstack state-dstack set-state-dstack!)
(generate-core-macro rstack state-rstack set-state-rstack!)
(generate-core-macro memory state-memory set-state-memory!)

(define interpreter (make-interpreter))
(define compiler (compiler-struct (make-rvector 100 -1) (make-rvector 100 -1) 0 #f (make-infinite-stack) 0 (make-rvector 100 -1) 0))

(define (set-as-defaults!)
  (let ((default-codespace (make-rvector 1))
	(default-dict (make-rvector 1))
	(default-directives (make-rvector 1)))
    (rvector-copy! default-codespace 0 codespace 0 (rvector-length codespace))
    (rvector-copy! default-dict 0 dict 0 (rvector-length dict))
    (rvector-copy! default-directives 0 compiler-directives 0 (rvector-length compiler-directives))
    (lambda ()
      (set! interpreter (make-interpreter))
      (set! compiler (compiler-struct (make-rvector 1 -1) (make-rvector 1 -1) 0 #f (make-infinite-stack) 0 (make-rvector 100 -1) 0))
      (for [(i (in-range 0 num-cores))]
	   (vector-set! cores i (make-new-core)))
      (rvector-copy! codespace 0 default-codespace 0 (rvector-length default-codespace))
      (rvector-copy! dict 0 default-dict 0 (rvector-length default-dict))
      (rvector-copy! compiler-directives 0 default-directives 0 (rvector-length default-directives)))))

; Stacks
(define push-cells! push!)
(define (push-int! stack num)
  (push-cells! stack (int->bytes num)))

(define pop-cells! pop!)
(define (pop-int! stack signed?)
  (integer-bytes->integer (pop-cells! stack) signed? #t))

(define get-cells peek)
(define (get-int stack signed? [pos 0])
  (integer-bytes->integer (get-cells stack pos) signed? #t))

; Debugging

(define (print-stack stack)
  (define (loop pos)
    (print (get-int dstack #t pos))
    (display " ")
    (unless (= pos 0) (loop (sub1 pos))))
  (display "| ")
  (loop (sub1 (stack-length stack)))
  (display ">"))

; Entry for the dictionary.  Code must be mutable to allow procs which refer to the entry itself.
(struct entry (primitive name [code #:mutable]))

; Dictionary

(define (add-entry! prim name code)
  (let [(new (entry prim name code))]
    (add-element! dict new)
    new))

(define (add-to-codespace proc-or-addr)
  ;(printf "add-compile ~e\n" location-counter)
  ;(unless (= location-counter (rvector-length codespace)) (display location-counter) (display " ") (display (rvector-length codespace)) (newline))
  (add-element! codespace proc-or-addr))
  
(define (add-to-litspace proc-or-addr)
  (add-element! litspace proc-or-addr))

(define (add-primitive-code! proc-or-addr)
  (if (= literal-mode 0)
      (add-to-codespace proc-or-addr)
      (add-to-litspace proc-or-addr)))

(define exit-addr 0) ; Kind of hacky, but not too bad.
; It's obvious that it will be at address 0.

(define (exit)
  (pop-int! rstack #f) ; Don't return to wherever exit came from
  (let ((val (pop-int! rstack #f)))
    (set! pc (if (= val 0) -1 val))))

(define (add-word! prim name)
  (add-entry! prim name (rvector-length codespace)))
(void (add-word! #f "exit"))
(add-primitive-code! exit)

(define (add-primitive-word! prec name code)
  (add-word! #t name)
  (add-primitive-code! code)
  (add-primitive-code! exit-addr)) ; To prevent Racket from spewing a bunch of #<entry> when the file is loaded.

(define (add-compiler-directive! name code)
  (add-element! compiler-directives (entry #t name (rvector-length codespace)))
  (add-primitive-code! code))

(define (find-address d name)
  (define (loop address)
    (let [(word (rvector-ref d address))]
      (cond [(string-ci=? name (entry-name word)) address]
            [(= address 0) #f]
            [else (loop (sub1 address))])))
  (loop (sub1 (rvector-length d))))

(define (find-entry d name)
  (let [(address (find-address d name))]
    (if address
        (rvector-ref d address)
        #f)))
