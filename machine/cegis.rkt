#lang racket

(require racket/system openssl/sha1 
         "programs.rkt" "stack.rkt" "state.rkt" "interpreter.rkt" "greensyn.rkt"
         "cache.rkt"
         "../ArrayForth/compiler.rkt")

(provide optimize program-diff? fastest-program)
(provide estimate-time program-length perf-mode)
(provide z3 read-sexps)

(define debug #f)
(define demo #t)
(define current-step 0) ; the number of the current cegis step
(define current-run 0)  ; the number of the current call to cegis.

(define comm-length 1)
(define all-pairs '())
(define timeout 3000) ;3000

(define (initialize)
  (system "mkdir debug")
  (set! comm-length 1)
  (set! all-pairs '()))

(define (finalize)
  ;(system "rm debug/*"))
  void)

(define (set-comm-length comm)
  (pretty-display (format "comm-length: ~a" (vector-length (commstate-data comm))))
  (set! comm-length (max 1 (vector-length (commstate-data comm))))
)

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

(define (z3 file break temp)
  (define out-port (open-output-file temp #:exists 'truncate))
  (define t0 (current-seconds))
  (define-values (sp o i e) (subprocess out-port 
                                        #f #f 
                                        (find-executable-path "z3") file))

  (define (close-ports)
    (close-output-port i)
    (close-input-port e)
    (close-output-port out-port))
    

  (define (inner)
    (sync/timeout timeout sp)
    (define t1 (current-seconds))
    (pretty-display (format "z3 time = ~a s." (- t1 t0)))
    
    (if (and break (equal? (subprocess-status sp) 'running))
        (begin
          (pretty-display "\t[timeout]")
          (subprocess-kill sp #t)
          (close-ports)
          #f)
        (begin
          (when (equal? (subprocess-status sp) 'running)
                (subprocess-wait sp))
          (close-ports)
          (open-input-file temp))))

  (define (cleanup e)
    (subprocess-kill sp #t)
    (close-ports)
    (raise e)
    )

  (with-handlers* ([exn:break? cleanup])
                  (inner)))
                  

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
        (and result (cadr result))))
  (define (length)
      (let* ([name `total_length]
             [result (assoc name model)])
        (and result (cadr result))))
  (cons (string-join (map process-instr (filter (compose is-hole car) model)) " ") (or (length) (time))))

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
  (define commdata (car (cdr (assoc (string->symbol "commdata_v0") model))))
  (define commtype (car (cdr (assoc (string->symbol "commtype_v0") model))))
  (define recvdata (car (cdr (assoc (string->symbol "recvdata_v0") model))))
  (define commp (car (cdr (assoc (string->symbol (format "commp_~s_v0" prog-length)) model))))
  (commstate commdata commtype recvdata commp))

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
  (define (inner in)
    (and in
         (let ([input (read-sexps in)])
           (close-input-port in)
           (and (not (member 'unsat input))
                (member 'sat input)
                (let ([res (filter
                            (lambda (x)
                              (or (equal? x 'sat) (equal? (car x) 'model))) input)])
                  (and (member 'sat res) (extract-model (cadr res))))))))
  (let ([result (inner in)])
    (close-input-port in)
    result))


(define (trim-start program)
  (cond
   [(empty? program) (list)]
   [(member (car program) (list "." "nop")) (trim-start (cdr program))]
   [else program]))

;; Return the corresponding number of the first not-nop @p.
;; Return #f if the first not-nop instruction is not @p.
(define (first-lit insts)
  (cond
   [(string->number (car insts)) (string->number (car insts))]
   [else (first-lit (cdr insts))]))


;;; Returns a random input/output pair for the given F18A program.
(define (random-pair program memory-start start-state)
  ;; Set up legel initial state.
  (define insts (trim-start (string-split program)))
  (define first-inst (car insts))
  (cond
   [(equal? first-inst "@p")
    (define lit (first-lit (cdr insts)))
    (when (negative? lit)
	  (set! start-state (struct-copy progstate start-state [t (- lit)])))]

   [(member first-inst (list "a!" "b!"))
    (when (= (progstate-t start-state) 0)
	  (set! start-state (struct-copy progstate start-state [t UP])))]

   [(member first-inst (list "!" "@"))
    (when (= (progstate-a start-state) 0)
	  (set! start-state (struct-copy progstate start-state [a UP])))]

   [(member first-inst (list "!b" "@b"))
    (when (= (progstate-b start-state) 0)
	  (set! start-state (struct-copy progstate start-state [b UP])))])
  
  (load-state! start-state)
  (load-program program memory-start)
  (set! start-state (current-state))
  (reset-p! memory-start)
  (step-program!*)

  (define comm (current-commstate))
  (set-comm-length comm)

  (cons `(,start-state ,(current-state)) comm))

;;; Add an input/output pair to greensyn.
(define (greensyn-add-pair pair comm)
  (greensyn-input (car pair))
  (greensyn-output (cadr pair))
  (greensyn-send-recv comm)
  (greensyn-commit))

;;; Generate a candidate using the specified input/output pairs. If no
;;; pairs are specified, seed the process with a randomly generated
;;; pair. The returned model is an assoc list of variable name symbols
;;; and their numerical values.
(define (generate-candidate program previous-pairs name mem slots init repeat constraint time-limit length-limit num-bits inst-pool)
  (when demo (pretty-display "     + add pair"))
  (when (null? previous-pairs) (error "No input/output pairs given!"))
  (define temp-file (temp-file-name name "syn" ".smt2"))
  (greensyn-reset mem comm-length constraint #:num-bits num-bits #:inst-pool inst-pool)
  (map greensyn-add-pair (map car previous-pairs) (map cdr previous-pairs))
  
  (greensyn-check-sat #:file temp-file slots init repeat #:time-limit time-limit #:length-limit length-limit)
  
  (pretty-display `(temp-file ,temp-file))
  (define output-temp (format "output~a.tmp" (current-milliseconds)))
  (define z3-res (z3 temp-file #t output-temp))
  (delete-file output-temp)

  (if z3-res
      ;; not timeout
      (let ([result (read-model z3-res)])
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
        ;;(when result (pretty-display "\t>> Found a candidate."))
        (and result (model->program result)))
      'timeout))

(define (program-diff? spec candidate mem-size constraint num-bits [inst-pool `no-fake])
  (pretty-display `(program-diff? mem-size ,mem-size))
  (define formatted-spec (insert-nops spec))
  (define formatted-cand (insert-nops candidate))
  (if (equal? formatted-spec formatted-cand)
      #f
      (validate formatted-spec formatted-cand 
                (format "eqtest~a" (current-milliseconds))
                (if (> mem-size 0) mem-size 1) constraint num-bits (default-state)
                inst-pool)))

;;; Generate a counter-example or #f if the program is valid.
(define (validate spec candidate name mem constraint num-bits start-state 
                  [inst-pool `no-fake])
  (set! current-step (add1 current-step))

  (define temp-file (temp-file-name name "verify" ".smt2"))
  (greensyn-reset mem comm-length constraint 
                  #:num-bits num-bits #:inst-pool inst-pool)
  (greensyn-spec spec)
  (greensyn-verify temp-file candidate start-state)
  (pretty-display `(ver-file ,temp-file))
  (define output-temp (format "output~a.tmp" (current-milliseconds)))
  (define result (read-model (z3 temp-file #f output-temp)))
  (delete-file output-temp)
  
  (when debug
    (call-with-output-file
        #:exists 'truncate (temp-file-name name "verifier")
      (lambda (out) (and result (map (lambda (p) (display p out) (newline out)) result)))))
  
  (unless debug (delete-file temp-file))
  ;(when result (pretty-display "\t>> Add counterexample."))
  (define prog-length (program-length spec))
  (and result (cons (model->pair result #:mem mem prog-length)
                    (model->commstate result prog-length))))

;;; This function runs the whole CEGIS loop. It stops when validate
;;; returns #f and returns the valid synthesized program. 
(define (cegis program 
	       #:name [name "prog"]
	       #:mem [mem 1] 
	       #:slots [slots 30] 
	       #:init [init 0] 
	       #:repeat [repeat 1] 
	       #:start [start 0] 
               #:constraint [constraint constraint-all] 
	       #:time-limit [time-limit #f]
	       #:length-limit [length-limit #f]
	       #:num-bits [num-bits 18]
	       #:inst-pool [inst-pool `no-fake]
	       #:start-state [start-state (default-state)]
	       #:print-time [print-time #f])
  (define cegis-start (current-seconds))
  (reset! num-bits)
  (unless (nop-before-plus? program) (error "+ has to follow a nop unless it's the first instruction!"))
  (define program-for-ver (fix-@p program))
  (when demo
    (if length-limit
        (if (number? slots)
	    (pretty-display (format ">> Synthesizing a program with <= ~a instructions, whose actual length < ~a." 
                                    slots length-limit))
	    (pretty-display (format ">> Synthesizing a program from ~e.\n   Approx runtime < ~a." 
                                    (regexp-replace* #rx"\n" slots " ") length-limit)))
	(if (number? slots)
	    (pretty-display (format ">> Synthesizing a program with <= ~a instructions, whose approx runtime < ~a ns." 
                                    slots (* time-limit 0.5)))
	    (pretty-display (format ">> Synthesizing a program from ~e.\n   Approx runtime < ~a ns." 
                                    (regexp-replace* #rx"\n" slots " ") (* time-limit 0.5))))))
  (set! current-run (add1 current-run))
  (set! current-step 0)

  (define (go)
    (let ([candidate (generate-candidate program all-pairs name mem slots init repeat constraint time-limit length-limit num-bits inst-pool)])
      (if (equal? candidate 'timeout)
          'timeout
          (and candidate
               (let ([new-pair (validate program-for-ver (car candidate) name mem constraint num-bits start-state inst-pool)])
                 (if new-pair
                     (begin
                       (set! all-pairs (cons new-pair all-pairs))
                       (go))
                     (begin 
                       (when demo
                         (if length-limit
                             (pretty-display (format "\tFound ~e.\n\tActual length = ~e." 
                                                     (car candidate) (cdr candidate)))
                             (pretty-display (format "\tFound ~e.\n\tApprox runtime = ~e ns." 
                                                     (car candidate) (* (cdr candidate) 0.5)))))
                       candidate)))))))
  
  (when (empty? all-pairs)
  	(set! all-pairs (list (random-pair program start start-state))))
  
  (define result (go))
  
  (when print-time (newline) 
	(pretty-display (format "Time to synthesize: ~a seconds." (- (current-seconds) cegis-start))))
  result)

(define (fastest-program program [best-so-far #f]
			 #:name       [name "prog"]
                         #:mem        [mem 1]
			 #:init       [init 0]
			 #:slots      [slots (program-length-abs program)]
			 #:repeat     [repeat 1]
                         #:start      [start mem] 
                         #:constraint [constraint constraint-all]
                         #:time-limit [time-limit #f]
                         #:length-limit [length-limit #f]
			 #:num-bits  [num-bits 18]
			 #:inst-pool [inst-pool `no-fake]
			 #:start-state [start-state (default-state)]
                         #:f18a [f18a #t])
  (unless f18a
          (set! program (compile-to-string program)))
  (unless (or time-limit length-limit)
          (set! length-limit (length-with-literal program)))
          
  (define start-time (current-seconds))
  (define program-for-ver (fix-@p program))
  (define candidate (cegis program #:name name
			   #:mem mem 
                           #:slots slots #:init init #:repeat repeat
                           #:start start 
			   #:constraint constraint 
                           #:time-limit time-limit #:length-limit length-limit
			   #:num-bits num-bits #:inst-pool inst-pool 
                           #:start-state start-state))
  (define result (if (and candidate (not (equal? candidate 'timeout)))
                     (fastest-program program candidate #:name name
				      #:mem mem
                                      #:slots slots #:init init #:repeat repeat
                                      #:start start 
                                      #:constraint constraint 
                                      #:time-limit (and time-limit (cdr candidate))
                                      #:length-limit (and length-limit (cdr candidate))
				      #:num-bits num-bits #:inst-pool inst-pool
				      #:start-state start-state)
		     best-so-far))
  (when demo (when debug (pretty-display (format "Time: ~a seconds." (- (current-seconds) start-time)))))
  result)

(define (binary-search slot-min slot-max init repeat program name mem start 
                       constraint limit length-search num-bits inst-pool 
                       start-state [best-so-far #f])
  (if (> slot-min slot-max)
      best-so-far
      (let* ([slot-mid (quotient (+ slot-min slot-max) 2)]
	     [candidate (cegis program 
			       #:name name
			       #:mem mem #:slots slot-mid #:init init #:repeat repeat
			       #:start start 
			       #:constraint constraint 
                               #:start-state start-state
                               #:time-limit (and (not length-search) limit) 
                               #:length-limit (and length-search limit)
			       #:num-bits num-bits #:inst-pool inst-pool)])
        (cond
          [(equal? candidate 'timeout) (or best-so-far 'timeout)]
          [(and (not candidate) (= slot-min slot-max)) best-so-far]
          [(not candidate)
           (binary-search (add1 slot-mid) slot-max init repeat program name mem start constraint 
                           (if best-so-far (cdr best-so-far) limit) length-search num-bits inst-pool start-state best-so-far)]
          [else
           (binary-search slot-min (sub1 slot-mid) init repeat program name mem start constraint 
                          (cdr candidate) length-search num-bits inst-pool start-state candidate)]))))

(define (fastest-program3 program [best-so-far #f]
			  #:name       [name "prog"]
			  #:mem        [mem 1]
			  #:init       [init 0]
			  #:slots      [slots (program-length-abs program)]
			  #:repeat     [repeat 1]
			  #:start      [start mem] 
			  #:constraint [constraint constraint-all]
			  #:time-limit [time-limit #f]
                          #:length-limit [length-limit #f]
			  #:num-bits  [num-bits 18]
			  #:inst-pool [inst-pool `no-fake]
			  #:start-state [start-state (default-state)])
  (when demo
        (pretty-display (format "original program\t: ~e" program))
        (pretty-display (format "memory\t\t\t: ~a" mem))
        (pretty-display (format "constraint\t\t: ~a" constraint))
        (pretty-display (format "length\t\t\t: ~a" (program-length-abs program)))
        (pretty-display (format "approx. runtime\t\t: ~a" (estimate-time program)))
        (pretty-display (format "length with literal\t: ~a" (length-with-literal program))))

  (define start-time (current-seconds))

  ;; (pretty-display "PHASE 1: binary search on program length to find a program whose runtime is less than the original.")
  (define candidate (binary-search 1 slots init repeat program name 
                                   mem start constraint 
                                   (or length-limit time-limit) length-limit
                                   num-bits inst-pool start-state))

  ;; (pretty-display "PHASE 1: finding a slightly larger program length whose runtime is less than the original.")
  (if (equal? candidate 'timeout)
      'timeout
      (let ([n-slots (if candidate (program-length-abs (car candidate)) slots)]
            [limit-time (and time-limit (if candidate (cdr candidate) time-limit))]
            [limit-length (and length-limit (if candidate (cdr candidate) length-limit))])
        (or (fastest-program program candidate #:name name #:mem mem #:init init
                         #:slots (+ n-slots 3) #:repeat repeat #:start start 
                         #:constraint constraint
                         #:time-limit limit-time
                         #:length-limit limit-length
                         #:num-bits num-bits #:inst-pool inst-pool
                         #:start-state start-state)
            candidate))))

  ;candidate)

;; Optimize for the fastest running program. The runtime is estimated by summing runtime of 
;; all instructions in the given program without considering instruction fetching time.
;;
;; Output (on display):
;; The fasted F18A program that is equivalent to the given input program. 
;; Programs that we can synthesize do not contain instructions that change 
;; the control flow of the program, which are ; ex jump call next if -if.
;; It also cannot synthesize !p instruction.
;;
;; Required arguments:
;; orig-program :: F18A program to be optimized. Literals have to be written in form of @p as in F18A, 
;;                not arrayForth (e.g. @p @p @p @p 1 2 3 4). up down left right have to be written as 
;;                UP DOWN LEFT RIGHT (with capitalized letters) and written as they are literal 
;;                (e.g. @p @p @p @p UP DOWN LEFT RIGHT). Multiport read and write are not supported.
;; 
;; Optional arguments:
;; name  :: description of the program.
;; mem   :: number of entries of memory. The more it is the longer the synthesizer takes. 
;;          Therefore, provide just enough for the program. Note that we only support storing data
;;          from memory 0th entry until mem-1'th entry and the program itself is stored starting at
;;          mem'th entry. 
;;          DEFAULT = 1
;; slots :: maximum length of the synthesized program.
;;          slots can be string when user want to provide a sketch.
;;          For example, "_ . + _" means the synthesized program contains 4 instructions.
;;          The 1st and 4st instructions can be anything. The 2nd instruction is nop, 
;;          and the 3rd instruction is plus. 
;;          DEFAULT = original program's length
;; repeat :: When slots is a sketch in form of string. repeat can be used to indicate how many time 
;;           the sketch is unrolled. For example, #:slots "dup _ _ ." #:repeat 3 means that
;;           the actual sketch is "dup _ _ . dup _ _ . dup _ _ ."
;;           DEFAULT = 1
;; init   :: Init is the additional header sketch that comes before slots.
;;           For example, #:init "over push - 2*" #:slots "dup _ _ ." #:repeat 3 means that
;;           the actual sketch is "over push - 2* dup _ _ . dup _ _ . dup _ _ ."
;;           DEFAULT = ""
;; start  :: The entry of the memory where the program is loaded to (starting from that entry).
;;           DEFAULT = mem
;; constraint :: The registers and/or stacks that contain the output you are looking for.
;;               For example, if you want to synthesize x y --> x+y, you might only care that you want
;;               register t (the top of th stack) to be equal to x+y and don't care that if other registers 
;;               and stacks are changed or not. The synthesizer always constraints reads and writes to NSWE.
;;               Use "#:constraint constraint-all" to constraint on everything 
;;               (a b r s t data-stack return-stack memory).
;;               Use "#:constraint constraint-none" to constraint on nothing except reads and writes 
;;               to NSEW.
;;               Use "#:constraint (constraint <reg> ...) to constraint on <reg>. 
;;               For example, to constraint on a and t, use "#:constraint (constraint a t)"
;;               DEFAULT = constraint-all
;; time-limit :: The maximum runtime in ns that of the synthesized program.
;;               DEFAULT = the original runtime
;; num-bits ::   number of bits of a word.
;;               DEFAULT = 18
;; inst-pool ::  Instructions available to compose the synthesized program. 
;;               #:inst-pool `no-fake = {@p @+ @b @ !+ !b ! +* 2* 2/ - + and or drop dup pop over a nop push b! a!}
;;               #:inst-pool `no-fake-no-p = `no-fake - {@p}
;;               #:inst-pool `no-mem = `no-fake - {@+ @b @ !+ !b !}
;;               #:inst-pool `no-mem = `no-mem - {@p}
;;               DEFAULT = `no-fake
;; bin-search :: When slots is a number. We perform binary search the length of the synthesized program.
;;               For example, if slots is 8, we will start searching for a program whose length is 4.
;;               If we find an equivalent program, we will search on length 2. 
;;               If not, we will search on length 6. The process keeps going like normal binary search.
;;               If bin-search is set to false, we will always search program whose length is equal
;;               to slots.
;;               DEFAULT = true

(define (optimize-internal orig-program 
                  #:f18a       [f18a #t]
		  #:name       [name "prog"]
		  #:mem        [mem 1]
		  #:init       [init 0]
		  #:slots      [raw-slots 0]
		  #:repeat     [repeat 1]
		  #:start      [start mem] 
		  #:constraint [constraint constraint-all]
		  #:num-bits   [num-bits 18]
		  #:inst-pool  [inst-pool `no-fake]
                  #:start-state [start-state (default-state)]
		  #:time-limit [time-limit #f]
                  #:length-limit [length-limit #f]
		  #:bin-search [bin-search `length])
  (when (> mem 512) (begin (pretty-display (format "~a words of member is too big!") mem) (exit)))
  (when (< mem 1) (set! mem 1))
  
  (initialize)
  (set-udlr-from-constraints mem num-bits)
  (define program (preprocess (if f18a orig-program (compile-to-string orig-program))))
  (define slots raw-slots)
  (when (and (number? slots) (= slots 0))
	(set! slots (program-length-abs program)))
  (when (string? slots)
	(set! slots (fix-@p (preprocess slots))))
  (unless time-limit
	(set! time-limit (estimate-time program)))
  (unless length-limit
        (set! length-limit (length-with-literal program)))

  (define start-time (current-seconds))

  (define result 
    (if (and (number? slots) bin-search)
      (fastest-program3 program 
			#:name       name
			#:mem        mem
			#:init       init
			#:slots      slots
			#:repeat     repeat
			#:start      start
			#:constraint constraint
			#:time-limit (and (equal? bin-search `time) time-limit)
                        #:length-limit (and (equal? bin-search `length) length-limit)
			#:num-bits   num-bits
			#:inst-pool  inst-pool
			#:start-state start-state)
      (fastest-program program 
			#:name       name
			#:mem        mem
			#:init       init
			#:slots      slots
			#:repeat     repeat
			#:start      start
			#:constraint constraint
			#:time-limit (and (equal? bin-search `time) time-limit)
                        #:length-limit (and (equal? bin-search `length) length-limit)
			#:num-bits   num-bits
			#:inst-pool  inst-pool
			#:start-state start-state)))
  (when demo
    (newline)
    (cond
     [(equal? result 'timeout)
      (pretty-display "Timeout.")]
     
     [result
      (pretty-display (format "output program\t\t: ~e" (postprocess (car result))))
      (pretty-display (format "length\t\t\t: ~a" (program-length-abs (car result))))
      (if (equal? bin-search `time)
          (pretty-display (format "approx. runtime\t\t: ~a" (* (cdr result) 0.5)))
          (pretty-display (format "length with literal\t: ~a" (cdr result))))
      (newline)
      (pretty-display "Constants for neighbor ports:")
      (pretty-display (format "UP = ~a, DOWN = ~a, LEFT = ~a, RIGHT = ~a" UP DOWN LEFT RIGHT))
      (newline)]
     
     [else
      (pretty-display "No better implementation found.")])
    
    (pretty-display (format "Time to synthesize: ~a seconds." (- (current-seconds) start-time))))

  (when (not debug)
  	(finalize))

  (cond
   [(equal? result 'timeout) 'timeout]
   [result (car result)]
   [else orig-program])
)
  
(define cache (make-hash))
(define (optimize orig-program 
                  #:f18a       [f18a #t]
		  #:name       [name "prog"]
		  #:mem        [mem 1]
		  #:init       [init 0]
		  #:slots      [raw-slots 0]
		  #:repeat     [repeat 1]
		  #:start      [start mem] 
		  #:constraint [constraint constraint-all]
		  #:num-bits   [num-bits 18]
		  #:inst-pool  [inst-pool `no-fake]
                  #:start-state [start-state (default-state)]
		  #:time-limit [time-limit #f]
                  #:length-limit [length-limit #f]
		  #:bin-search [bin-search `length])
  ;; (unless f18a
  ;;         (set! orig-program (compile-to-string orig-program)))
  ;; (set! orig-program (preprocess orig-program))
  (load-cache cache)
  ;; (let ([insts (string-split orig-program)])
  ;;   (when (and (not f18a) 
  ;;              (not (empty? insts)) 
  ;;              (string->number (car insts))
  ;;              (negative? (string->number (car insts))))
  ;;         (set! start-state (struct-copy progstate start-state 
  ;;                                        [t (- (string->number (car insts)))]))
  ;;         ))
  (define key (cache-get-key orig-program num-bits mem time-limit length-limit
                             constraint start-state))

  (if (hash-has-key? cache key)
      (hash-ref cache key)
      (let ([result (optimize-internal orig-program
				      #:f18a       f18a
				      #:name       name
				      #:mem        mem
				      #:init       init
				      #:slots      raw-slots
				      #:repeat     repeat
				      #:start      start
				      #:constraint constraint
				      #:num-bits   num-bits
				      #:inst-pool  inst-pool
                                      #:start-state start-state
				      #:time-limit time-limit
				      #:length-limit length-limit
				      #:bin-search bin-search)])
	(cache-put cache key result)
	result)))
