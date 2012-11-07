#lang racket

(require racket/system "state.rkt" "interpreter.rkt" "greensyn.rkt")

(provide cegis)

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
  (sort (map fun->pair (cdr model)) string<? #:key (compose symbol->string car)))

;;; Given a model, interprets the holes as instructions.
(define (model->program model)
  (define instrs '#(2* 2/ - + and or drop dup @+ @ @b !+ ! !b a! b! a +* pop push over up down left right nop))
  (define (is-hole var) (equal? (string-ref (symbol->string var) 0) #\h))
  (string-join (map (lambda (x) (symbol->string (vector-ref instrs (cadr x))))
                    (filter (compose is-hole car) model)) " "))

;;; Given a model, extract the input/output pair it corresponds
;;; to. This lets you get new pairs after running the validator.
(define (model->pair model)
  (define max (car (regexp-match "[0-9]+" (symbol->string (car (last model))))))
  (define (extract-state n)
    (define (var v) (string->symbol (format "~a_~a_v1" v n)))
    (progstate (var 'a) (var 'b) (var 'p) (var 'i) (var 'r) (var 's) (var 't)
                (var 'data) (var 'return) (var 'memory)))
  `(,(extract-state 0) ,(extract-state max)))

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
  (define res
    (filter (lambda (x) (or (equal? x 'sat) (equal? (car x) 'model))) (read-sexps in)))
  (and (member 'sat res) (extract-model (cadr res))))

;;; Returns a random input/output pair for the given F18A program.
(define (random-pair program)
  (define in (random-state))
  (load-state! in)
  (load-program program)
  (step-program!*)
  `(,in ,(current-state)))

;;; Add an input/output pair to greensyn.
(define (greensyn-add-pair pair [comm #f])
  (greensyn-input (car pair))
  (greensyn-output (cadr pair))
  (greensyn-send-recv (or comm (default-commstate))))

;;; Generate a candidate using the specified input/output pairs. If no
;;; pairs are specified, seed the process with a randomly generated
;;; pair. The returned model is an assoc list of variable name symbols
;;; and their numerical values.
(define (generate-candidate program [previous-pairs '()]
                            #:mem [mem 6] #:comm [comm 1] #:slots [slots 30])
  (define temp-file (temp-file-name))
  (greensyn-reset mem comm)
  (when (null? previous-pairs) (set! previous-pairs `(,(random-pair program))))
  (map greensyn-add-pair previous-pairs)
  (greensyn-commit)
  (greensyn-check-sat #:file temp-file slots)
  (define result (read-model (z3 temp-file)))
  (delete-file temp-file)
  (or (and result (model->program result))
      (error "Program cannot be written: synthesis not sat!")))

;;; Generate a counter-example or #f if the program is valid.
(define (validate spec candidate #:mem [mem 6] #:slots [slots 30])
  (define temp-file (temp-file-name))
  (greensyn-reset mem slots)
  (greensyn-spec spec)
  (greensyn-verify temp-file candidate)
  (define result (read-model (z3 temp-file)))
  (delete-file temp-file)
  (and result (model->pair result)))

;;; This function runs the whole CEGIS loop. It stops when validate
;;; returns #f and returns the valid synthesized program.
(define (cegis program #:mem [mem 6] #:comm [comm 1] #:slots [slots 30])
  (define (go pairs)
    (let* ([candidate (generate-candidate program pairs
                                          #:mem mem #:comm comm #:slots slots)]
           [new-pair (validate program candidate #:mem mem #:slots slots)])
      (if new-pair
          (go (cons new-pair pairs))
          candidate)))
  (go '()))
