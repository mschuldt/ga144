#lang racket

(require racket/system "stack.rkt" "state.rkt" "interpreter.rkt" "greensyn.rkt")

(provide cegis)

(define debug #t)
(define debug-n 0)

;;; Generates a random temporary file name with an .smt2 extension.
(define (temp-file-name)
  (format "~x.smt2" (random 100000000)))

;;; Run z3 on the given file, returning all output as a string.
(define (z3 file)
  (with-output-to-string (lambda () (system (format "z3 ~a" file)))))

;;; Creates an alist of variable names and values from a z3 model.
(define (extract-model model)
  (define (fun->pair fun) ; given a (define-fun ....), gives you a pair.
    `(,(list-ref fun 1) ,(list-ref fun 4)))
  (sort (map fun->pair (cdr model)) string<? #:key (compose (curry format "~a") car)))

;;; Given a model, interprets the holes as instructions.
(define (model->program model)
  (define instrs '#(@p @+ @b @ !+ !b ! +* 2* 2/ - + and or drop dup pop over a nop push b! a!))
  (define (is-hole var) (equal? (substring (format "~a" var) 0 2) "h_"))
  (define (process-instr res)
    (if (= 0 (cadr res))
        (format "~a" (cadr
                      (assoc
                       (string->symbol
                        (regexp-replace "h" (format "~a" (car res)) "hlit")) model)))
        (format "~a" (vector-ref instrs (cadr res)))))
  (string-join (map process-instr (filter (compose is-hole car) model)) " "))

;;; Given a model, extract the input/output pair it corresponds
;;; to. This lets you get new pairs after running the validator.
(define (model->pair model #:mem [mem 6] prog-length)
  (define (extract-state n)
    (define (var v) (cadr (assoc (string->symbol (format "~a_~a_v0" v n)) model)))
    (progstate (var 'a) (var 'b) 0 0 (var 'r) (var 's) (var 't)
               (stack (var 'sp) (bytes->vector (var 'dst) 8))
               (stack (var 'rp) (bytes->vector (var 'rst) 8))
               (bytes->vector (var 'mem) mem)))
  `(,(extract-state 0) ,(extract-state prog-length)))

;;; Parses the given bitvector into a vector of 18bit numbers.
(define (bytes->vector input size)
  (define (go bytes curr-size)
    (if (= curr-size 0)
        '()
        (cons (bitwise-bit-field bytes 0 18)
              (go (arithmetic-shift bytes -18) (sub1 curr-size)))))
  (list->vector (go input size)))

;;; read all the sexps from the given string or port.
(define (read-sexps in)
  (when (string? in) (set! in (open-input-string in)))
  (define (go)
    (let ([next (read in)])
      (if (eof-object? next) '() (cons next (go)))))
  (go))

;;; If the z3 output is sat, reads in the model. If it isn't, returns
;;; #f.
(define (read-model in)
  (define input (read-sexps in))
  (and (not (member 'unsat input))
       (let ([res (filter
                   (lambda (x)
                     (or (equal? x 'sat) (equal? (car x) 'model))) input)])
         (and (member 'sat res) (extract-model (cadr res))))))

;;; Returns a random input/output pair for the given F18A program.
(define (random-pair program)
  (define in (random-state))
  (load-state! in)
  (load-program program)
  (set! in (current-state))
  (step-program!*)
  `(,in ,(current-state)))

;;; Add an input/output pair to greensyn.
(define (greensyn-add-pair pair [comm #f])
  (greensyn-input (car pair))
  (greensyn-output (cadr pair))
  (greensyn-send-recv (or comm (default-commstate)))
  (greensyn-commit))

;;; Generate a candidate using the specified input/output pairs. If no
;;; pairs are specified, seed the process with a randomly generated
;;; pair. The returned model is an assoc list of variable name symbols
;;; and their numerical values.
(define (generate-candidate program [previous-pairs '()]
                            #:mem [mem 6] #:comm [comm 1] #:slots [slots 30])
  (when (null? previous-pairs) (display "Generating random seed pair.") (newline)
                               (set! previous-pairs (list (random-pair program))))
  (define temp-file (format "debug-syn-~a.smt2" debug-n))
  (greensyn-reset mem comm)
  (map greensyn-add-pair previous-pairs)

  (greensyn-check-sat #:file temp-file slots)

  (when debug
    (copy-file temp-file (format "debug-generate-model~a" debug-n) #t))

  (define z3-res (z3 temp-file))
  (define result (read-model z3-res))
  ;; (delete-file temp-file)

  (when debug
    (define out (open-output-file #:exists 'truncate (format "debug-generate-~a" debug-n)))
    (display z3-res out)
    (close-output-port out)
    (call-with-output-file #:exists 'truncate (format "debug-pair-~a" debug-n)
      (lambda (file)
        (display (last previous-pairs) file)
        (newline file)))
    (call-with-output-file #:exists 'truncate (format "debug-program-~a" debug-n)
      (lambda (file)
        (display (model->program result) file))))

  (or (and result (model->program result))
      (error "Program cannot be written: synthesis not sat!")))

;;; Generate a counter-example or #f if the program is valid.
(define (validate spec candidate #:mem [mem 6] #:comm [comm 1] prog-length)
  (define temp-file (format "debug-verify-~a.smt2" debug-n))
  (greensyn-reset mem comm)
  (greensyn-spec spec)
  ;(pretty-display "spec")
  ;(pretty-display spec)
  ;(pretty-display "candidate")
  ;(pretty-display candidate)
  (greensyn-verify temp-file candidate)
  (define result (read-model (z3 temp-file)))
  
  (when debug
    (set! debug-n (add1 debug-n))
    (define out (open-output-file #:exists 'truncate (format "debug-verifier-~a" debug-n)))
    (and result (map (lambda (p) (display p out) (newline out)) result))
    (close-output-port out))
  
  (delete-file temp-file)
  (and result (model->pair result #:mem mem prog-length)))

;;; This function runs the whole CEGIS loop. It stops when validate
;;; returns #f and returns the valid synthesized program.
(define (cegis program #:mem [mem 6] #:comm [comm 1] #:slots [slots 30])
  (define prog-length (length (regexp-split " +" program)))
  (define (go pairs)
    (let* ([candidate (generate-candidate program pairs
                                          #:mem mem #:comm comm #:slots slots)]
           [new-pair (validate program candidate #:mem mem #:comm comm prog-length)])
      (if new-pair
          (go (cons new-pair pairs))
          candidate)))
  (go '()))

;; (cegis "+ nop nop nop" #:mem 1 #:slots 2)

