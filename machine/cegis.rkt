#lang racket

(require racket/system openssl/sha1 "programs.rkt" "stack.rkt" "state.rkt" "interpreter.rkt" "greensyn.rkt")

(provide cegis fastest-program fastest-program2 fastest-program3)
(provide estimate-time program-length perf-mode)

(define debug #t)
(define demo #t)
(define current-step 0) ; the number of the current cegis step
(define current-run 0)  ; the number of the current call to cegis.

(define (perf-mode)
  (set! debug #f)
  (set! demo #f))

;;; Returns a file name with the given prefix, containing the current
;;; step and run. You can also optionally specify a suffix like
;;; `.smt2'. Note that the `.' in `.smt2' is not added automatically.
(define (temp-file-name name prefix [suffix ""])
  (format "debug/~a-~a-~a-~a~a" name prefix current-run current-step 
	  ;(substring (sha1 (open-input-string (format "~a" (current-inexact-milliseconds)))) 0 10)
	  suffix))

;;; Run z3 on the given file, returning all output as a string.
(define (z3 file)
  (with-output-to-string (lambda () (system (format "z3 ~a" file)))))

;;; Return 'lt if x1 < x2, 'eq if x1 = x2 and 'gt if x1 > x2. Compares
;;; numbers as numbers; otherwise compares as strings.
(define (compare x1 x2)
  (let ([x1-num (string->number x1)]
        [x2-num (string->number x2)])
    (if (and x1-num x2-num)
        (cond [(< x1-num x2-num) 'lt]
              [(= x1-num x2-num) 'eq]
              [(> x1-num x2-num) 'gt])
        (cond [(string<? x1 x2) 'lt]
              [(string=? x1 x2) 'eq]
              [(string>? x1 x2) 'gt]))))

;;; Orders variables by name and then number.
(define (var-name<? v1 v2)
  (let* ([v1-parts (regexp-split "_" v1)]
         [v2-parts (regexp-split "_" v2)]
         [len (min (length v1-parts) (length v2-parts))]
         [res
          (foldl (lambda (part res)
                   (if (equal? res 'eq)
                       (compare (car part) (cdr part))
                       res)) 'eq (map cons (take v1-parts len) (take v2-parts len)))])
    (if (equal? res 'eq)
        (< (length v1-parts) (length v2-parts))
        (equal? res 'lt))))

;;; Creates an alist of variable names and values from a z3 model.
(define (extract-model model)
  (define (fun->pair fun) ; given a (define-fun ....), gives you a pair.
    `(,(list-ref fun 1) ,(list-ref fun 4)))
  (sort (map fun->pair (cdr model)) var-name<? #:key (compose (curry format "~a") car)))

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
  (define (time)
      (let* ([name `total_time]
             [result (assoc name model)])
        (if result
            (cadr result)
            (error (format "~a not found in model!" name)))))
  ;(pretty-display (time))
  (cons (string-join (map process-instr (filter (compose is-hole car) model)) " ") (time)))

;;; Given a model, extract the input/output pair it corresponds
;;; to. This lets you get new pairs after running the validator.
(define (model->pair model #:mem [mem 6] prog-length)
  (define (extract-state n)
    (define (var v)
      (let* ([name (string->symbol (format "~a_~a_v0" v n))]
             [result (assoc name model)])
        (if result
            (cadr result)
            (error (format "~a not found in model!" name)))))
    (progstate (var 'a) (var 'b) 0 0 (var 'r) (var 's) (var 't)
               (stack (var 'sp) (var 'dst))
               (stack (var 'rp) (var 'rst))
               (var 'mem)))
  `(,(extract-state 0) ,(extract-state prog-length)))

;;; Extract the commstate from the model.
(define (model->commstate model prog-length)
  (define (send x) (car (cdr (assoc (string->symbol (format "send~s_v0" x)) model))))
  (define (recv x) (car (cdr (assoc (string->symbol (format "recv~s_v0" x)) model))))
  (define (sendp x) (car (cdr (assoc (string->symbol (format "sendp~s_~s_v0" x prog-length)) model))))
  (define (recvp x) (car (cdr (assoc (string->symbol (format "recvp~s_~s_v0" x prog-length)) model))))
  (commstate (send 0) (send 1) (send 2) (send 3)
             (recv 0) (recv 1) (recv 2) (recv 3)
             (sendp 0) (sendp 1) (sendp 2) (sendp 3)
             (recvp 0) (recvp 1) (recvp 2) (recvp 3)))

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
(define (random-pair program [memory-start 0]
		     #:start-state [start-state (random-state (expt 2 BIT))])
  (load-state! start-state)
  (load-program program memory-start)
  (set! start-state (current-state))
  (reset-p! memory-start)
  (step-program!*)
  (cons `(,start-state ,(current-state)) (current-commstate)))

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
(define (generate-candidate program previous-pairs name mem comm slots init repeat constraint time-limit num-bits inst-pool)
  (when demo (pretty-display "     + add pair"))
  (when (null? previous-pairs) (error "No input/output pairs given!"))
  (define temp-file (temp-file-name name "syn" ".smt2"))
  (greensyn-reset mem comm constraint #:num-bits num-bits #:inst-pool inst-pool)
  (map greensyn-add-pair (map car previous-pairs) (map cdr previous-pairs))
  
  (greensyn-check-sat #:file temp-file slots init repeat #:time-limit time-limit)
  
  (define z3-res (z3 temp-file))
  (define result (read-model z3-res))

  (unless debug (delete-file temp-file))

  (when debug
    (call-with-output-file #:exists 'truncate (temp-file-name name "syn-model")
                           (curry display z3-res))
    (call-with-output-file #:exists 'truncate (temp-file-name name "syn-result")
                           (lambda (out)
                             (and result
                                  (map (lambda (p)
                                         (display p out) (newline out)) result))))
    (call-with-output-file #:exists 'truncate (temp-file-name name "pair")
                           (curry display (first previous-pairs)))
    (call-with-output-file #:exists 'truncate (temp-file-name name "program")
                           (lambda (file)
                             (and result (display (car (model->program result)) file)))))
  ;(when result (pretty-display "\t>> Found a candidate."))
  ;; (if result
  ;;     (model->program result)
  ;;     null))
  (and result (model->program result)))

;;; Generate a counter-example or #f if the program is valid.
(define (validate spec candidate name mem comm prog-length constraint num-bits inst-pool)
  (set! current-step (add1 current-step))

  (define temp-file (temp-file-name name "verify" ".smt2"))
  (greensyn-reset mem comm constraint #:num-bits num-bits #:inst-pool inst-pool)
  (greensyn-spec spec)
  (greensyn-verify temp-file candidate)
  (define result (read-model (z3 temp-file)))
  
  (when debug
    (call-with-output-file
        #:exists 'truncate (temp-file-name name "verifier")
      (lambda (out) (and result (map (lambda (p) (display p out) (newline out)) result)))))
  
  (unless debug (delete-file temp-file))
  ;(when result (pretty-display "\t>> Add counterexample."))
  (and result (cons (model->pair result #:mem mem prog-length)
                    (model->commstate result prog-length))))

;;; This function runs the whole CEGIS loop. It stops when validate
;;; returns #f and returns the valid synthesized program. 
(define (cegis program 
	       #:name [name "prog"]
	       #:mem [mem 1] 
	       #:comm [comm 1]
	       #:slots [slots 30] 
	       #:init [init 0] 
	       #:repeat [repeat 1] 
	       #:start [start 0] 
               #:constraint [constraint constraint-all] 
	       #:time-limit [time-limit (estimate-time program)]
	       #:num-bits [num-bits 18]
	       #:inst-pool [inst-pool `no-fake]
	       #:start-state [start-state (random-state (expt 2 BIT))])
  (reset! num-bits)
  (unless (nop-before-plus? program) (error "+ has to follow a nop unless it's the first instruction!"))
  (define program-for-ver (fix-@p program))
  (when demo
	(if (number? slots)
	    (pretty-display (format ">> Synthesizing a program with <= ~a instructions, whose approx runtime < ~a ns." slots (* time-limit 0.5)))
	    (pretty-display (format ">> Synthesizing a program from ~e.\n   Approx runtime < ~a ns." slots (* time-limit 0.5)))))
  (set! current-run (add1 current-run))
  (set! current-step 0)

  (define (go pairs)
    (let ([candidate (generate-candidate program pairs name mem comm slots init repeat constraint time-limit num-bits inst-pool)])
      (and candidate
          (let ([new-pair (validate program-for-ver (car candidate) name mem comm (program-length program-for-ver) constraint num-bits inst-pool)])
            (if new-pair
                (go (cons new-pair pairs))
                (begin 
		  (when demo
			(pretty-display (format "\tFound ~e.\n\tApprox runtime = ~e ns." (car candidate) (* (cdr candidate) 0.5))))
		  candidate))))))
  (go (list (random-pair program start #:start-state start-state))))

(define (fastest-program program [best-so-far #f]
			 #:name       [name "prog"]
                         #:mem        [mem 1]
                         #:comm       [comm 1]
			 #:init       [init 0]
			 #:slots      [slots (program-length-abs program)]
			 #:repeat     [repeat 1]
                         #:start      [start mem] 
                         #:constraint [constraint constraint-all]
                         #:time-limit [time-limit (add1 (estimate-time program))]
			 #:num-bits  [num-bits 18]
			 #:inst-pool [inst-pool `no-fake]
			 #:start-state [start-state (random-state (expt 2 BIT))])
  (define start-time (current-seconds))
  (define program-for-ver (fix-@p program))
  (define candidate (cegis program #:name name
			   #:mem mem #:comm comm 
                           #:slots slots #:init init #:repeat repeat
                           #:start start 
			   #:constraint constraint #:time-limit time-limit
			   #:num-bits num-bits #:inst-pool inst-pool #:start-state start-state))
  (define result (if candidate
                     (fastest-program program candidate #:name name
				      #:mem mem #:comm comm 
                                      #:slots slots #:init init #:repeat repeat
                                      #:start start 
                                      #:constraint constraint #:time-limit (cdr candidate)
				      #:num-bits num-bits #:inst-pool inst-pool
				      #:start-state start-state)
		     best-so-far))
  (when demo (when debug (pretty-display (format "Time: ~a seconds." (- (current-seconds) start-time)))))
  result)

(define (fastest-program2 program [best-so-far #f]
			 #:name       [name "prog"]
                         #:mem        [mem 1]
                         #:comm       [comm 1]
                         #:slots      [slots (+ (program-length-abs program) 2)]
                         #:start      [start mem] 
                         #:constraint [constraint constraint-all]
                         #:time-limit [time-limit (estimate-time program)]
			 #:num-bits  [num-bits 18]
			 #:inst-pool [inst-pool `no-fake])
  (define candidate (cegis program #:name name
			   #:mem mem #:comm comm #:slots slots 
			   #:start start 
			   #:constraint constraint #:time-limit time-limit
			   #:num-bits num-bits #:inst-pool inst-pool))
  (define result (if candidate
                     (fastest-program2 program candidate 
				       #:name name #:mem mem #:comm comm 
				       #:slots (min (+ (program-length-abs (car candidate)) 2) slots)
				       #:start start 
				       #:constraint constraint #:time-limit (cdr candidate)
				       #:num-bits num-bits #:inst-pool inst-pool)
		     best-so-far))
  result)

(define (binary-search slot-min slot-max init repeat program name mem comm start constraint time-limit num-bits inst-pool start-state [best-so-far #f])
  (if (> slot-min slot-max)
      best-so-far
      (let* ([slot-mid (quotient (+ slot-min slot-max) 2)]
	     [candidate (cegis program 
			       #:name name
			       #:mem mem #:comm comm #:slots slot-mid #:init init #:repeat repeat
			       #:start start 
			       #:constraint constraint #:time-limit time-limit
			       #:num-bits num-bits #:inst-pool inst-pool)])
	(if candidate
	    (binary-search slot-min (sub1 slot-mid) init repeat program name mem comm start constraint (cdr candidate) num-bits inst-pool start-state candidate)
	    (binary-search (add1 slot-mid) slot-max init repeat program name mem comm start constraint (if best-so-far (cdr best-so-far) time-limit) num-bits inst-pool start-state best-so-far)))))

(define (fastest-program3 program [best-so-far #f]
			  #:name       [name "prog"]
			  #:mem        [mem 1]
			  #:comm       [comm 1]
			  #:init       [init 0]
			  #:slots      [slots (program-length-abs program)]
			  #:repeat     [repeat 1]
			  #:start      [start mem] 
			  #:constraint [constraint constraint-all]
			  #:time-limit [time-limit (estimate-time program)]
			  #:num-bits  [num-bits 18]
			  #:inst-pool [inst-pool `no-fake]
			  #:start-state [start-state (random-state (expt 2 BIT))])
  (when demo
  (pretty-display (format "original program\t: ~e" program))
  (pretty-display (format "length\t\t\t: ~a" (program-length-abs program)))
  (pretty-display (format "approx. runtime\t\t: ~a" (* time-limit 0.5))))

  (define start-time (current-seconds))

  ;; (newline)
  ;; (pretty-display "PHASE 1: finding appropriate program length who runtime is less than the original.")
  (define candidate (binary-search 1 slots init repeat program name 
				   mem comm start constraint 
				   time-limit num-bits inst-pool start-state))

  (when demo (newline))
  (define result candidate)
  ;; (pretty-display "PHASE 2: optimizing for runtime.")
  ;; (define result 
  ;;   (if candidate
  ;; 	(fastest-program2 program candidate 
  ;; 			  #:name name #:mem mem #:comm comm 
  ;; 			  #:slots (min (+ (program-length-abs (car candidate)) 2) slots)
  ;; 			  #:start start 
  ;; 			  #:constraint constraint #:time-limit (cdr candidate)
  ;; 			  #:num-bits num-bits #:inst-pool inst-pool)
  ;; 	#f))
  (when demo
  (newline)
  (when result
	(pretty-display (format "output program\t\t: ~e" (car result)))
	(pretty-display (format "length\t\t\t: ~a" (program-length-abs (car result))))
	(pretty-display (format "approx. runtime\t\t: ~a" (* (cdr result) 0.5))))
  (pretty-display (format "Time to synthesize: ~a seconds." (- (current-seconds) start-time)))))

  
		 
