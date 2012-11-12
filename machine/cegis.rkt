#lang racket

(require racket/system "stack.rkt" "state.rkt" "interpreter.rkt" "greensyn.rkt")

(provide cegis)

(define debug #t)
(define current-step 0) ; the number of the current cegis step
(define current-run 0)  ; the number of the current call to cegis.

;;; Returns a file name with the given prefix, containing the current
;;; step and run. You can also optionally specify a suffix like
;;; `.smt2'. Note that the `.' in `.smt2' is not added automatically.
(define (temp-file-name prefix [suffix ""])
  (format "debug-~a-~a-~a~a" prefix current-run current-step suffix))

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
  (define (is-hole var) (equal? (substring (format "~a" var) 0 2) "h_"))
  (define (process-instr res)
    (if (= 0 (cadr res))
        (format "~a" (cadr
                      (assoc
                       (string->symbol
                        (regexp-replace "h" (format "~a" (car res)) "hlit")) model)))
        (format "~a" (vector-ref choice-id (cadr res)))))
  (string-join (map process-instr (filter (compose is-hole car) model)) " "))

;;; Given a model, extract the input/output pair it corresponds
;;; to. This lets you get new pairs after running the validator.
(define (model->pair model #:mem [mem 6] prog-length)
  (define (extract-state n)
    (define (var v)
      (let* ([name (string->symbol (format "~a_~a_v0" v n))]
             [result (assoc name model)])
        (if result
            (cadr result)
            (error (format "~a not found in model!" name model)))))
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
(define (random-pair program [memory-start 0])
  (define in (random-state))
  (load-state! in)
  (load-program program memory-start)
  (set! in (current-state))
  (reset-p! memory-start)
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
                            #:mem [mem 6] #:comm [comm 1] #:slots [slots 30] #:constraint [constraint constraint-all])
  (when (null? previous-pairs) (error "No input/output pairs given!"))
  (define temp-file (temp-file-name "syn" ".smt2"))
  (greensyn-reset mem comm constraint)
  (map greensyn-add-pair previous-pairs)

  (pretty-display (estimate-time program))
  (greensyn-check-sat #:file temp-file slots #:time-limit (estimate-time program))
  
  (define z3-res (z3 temp-file))
  (define result (read-model z3-res))

  (unless debug (delete-file temp-file))

  (when debug
    (call-with-output-file #:exists 'truncate (temp-file-name "syn-model")
                           (curry display z3-res))
    (call-with-output-file #:exists 'truncate (temp-file-name "syn-result")
                           (lambda (out)
                             (and result
                                  (map (lambda (p)
                                         (display p out) (newline out)) result))))
    (call-with-output-file #:exists 'truncate (temp-file-name "pair")
                           (curry display (first previous-pairs)))
    (call-with-output-file #:exists 'truncate (temp-file-name "program")
                           (lambda (file)
                             (and result (display (model->program result) file)))))
  (or (and result (model->program result))
      (error "Program cannot be written: synthesis not sat!")))

;;; Generate a counter-example or #f if the program is valid.
(define (validate spec candidate #:mem [mem 6] #:comm [comm 1] prog-length  #:constraint [constraint constraint-all])
  (set! current-step (add1 current-step))

  (define temp-file (temp-file-name "verify" ".smt2"))
  (greensyn-reset mem comm constraint)
  (greensyn-spec spec)
  ;(pretty-display "spec")
  ;(pretty-display spec)
  ;(pretty-display "candidate")
  ;(pretty-display candidate)
  (greensyn-verify temp-file candidate)
  (define result (read-model (z3 temp-file)))
  
  (when debug
    (call-with-output-file
        #:exists 'truncate (temp-file-name "verifier")
        (lambda (out) (and result (map (lambda (p) (display p out) (newline out)) result)))))
  
  (unless debug (delete-file temp-file))

  (and result (model->pair result #:mem mem prog-length)))

;;; This function runs the whole CEGIS loop. It stops when validate
;;; returns #f and returns the valid synthesized program.
(define (cegis program program-for-ver #:mem [mem 6] #:comm [comm 1] #:slots [slots 30] #:start [start 0] #:constraint [constraint constraint-all])
  (set! current-run (add1 current-run))
  (set! current-step 0)

  (define prog-length (length (regexp-split " +" program-for-ver)))
  (define (go pairs)
    (let* ([candidate (generate-candidate program pairs
                                          #:mem mem #:comm comm #:slots slots #:constraint constraint)]
           [new-pair (validate program-for-ver candidate #:mem mem #:comm comm prog-length #:constraint constraint)])
      (if new-pair
          (go (cons new-pair pairs))
          candidate)))

  (go (list (random-pair program start))))

;; (cegis "@p nop nop nop 1" "1 nop nop nop" #:mem 1 #:slots 4 #:constraint constraint-only-t)
;; (cegis "@p nop nop nop 1" "1 nop nop nop" #:mem 1 #:slots 4)
;; (cegis "- 2/ dup dup dup + a! dup" #:mem 4 #:slots 10)
