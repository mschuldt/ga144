#lang racket
(require compatibility/defmacro
         scheme/mpair
         "read.rkt")

(provide compile-file compile-string)

(define instructions (list->set '(";" "ret" "ex" "jump" "call" "unext" "next" "if"
                                  "-if" "@p" "@+" "@b" "@" "!p" "!+" "!b" "!" "+*"
                                  "2*" "2/" "-" "+" "and" "or" "drop" "dup" "pop"
                                  "over" "a" "." "nop" "push" "b!" "a!")))

(define address-required '("jump" "call" "next" "if" "-if"))

(define last-slot-instructions
  '(";" "ret" "unext" "@p" "!p" "+*" "+" "dup" "." "nop"))

(define instructions-preceded-by-nops '("+" "+*"))

(define instructions-using-rest-of-word '(";" "ret" "ex" "unext"))

(define num-nodes 144)
(define num-words 64)
(define nodes (make-vector num-nodes #f)) ;;node# -> memory vector

(define last-inst #f)

(define stack '())

(defmacro push (list item)
  `(set! ,list (cons ,item ,list)))

(defmacro pop (list)
  `(if (equal? ,list '())
       (pretty-display "ERROR: pop -- list is empty")
       (begin0 (car ,list) (set! ,list (cdr ,list)))))

(defmacro swap (list)
  `(set! ,list (cons (cadr ,list) (cons (car ,list) (cddr ,list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-file file)
  (call-with-input-file file (lambda (code-port)
                               (current-input-port code-port)
                               (compile-loop))))

(define (compile-string str)
  #f;;TODO
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;info about the current target core
(define memory #f) ;;vector of words
(define current-addr #f);;index of current word in memory
(define current-word #f);; tail of current word list
(define next-word #f) ;;index of next word in memory
(define current-slot #f);;index of the next slot in current-word

(define words (make-hash)) ;;word definitions -> addresses
;;TODO: need to have a seporate mapping for each core
;;TOOD: create a struct for each core. memory, current/next-word/slot, words
(define (add-word! name code)
  (hash-set! words name code))

(define waiting (make-hash));;word -> list of cells waiting for the word's address
(define (add-to-waiting word addr-cell)
  (unless (hash-has-key? waiting word)
    (hash-set! waiting word (list)))
  (hash-set! waiting word (cons addr-cell (hash-ref waiting word))))
(define (get-waiting-list word)
  (and (hash-has-key? waiting word)
       (hash-ref waiting word)))
(define (waiting-clear word)
  (hash-set! words word #f))


;;TODO: initial scan should resolve tail calls and collect word names
(define (get-word-address name)
  (and (hash-has-key? words name)
      (hash-ref words name)))

(define (instruction? token)
  (set-member? instructions token))

;; compiler directive - words executed at compile time
(define directives (make-hash));;directive names -> functions
(define (add-directive! name code)
  (hash-set! directives name code))
(define (get-directive name)
  (and (hash-has-key? directives name)
       (hash-ref directives name)))

;;

;;successfully parses a token as a number, or returns false
(define (parse-num tok)
  (string->number tok))

(defmacro setq (var val)
  `(let [(__v__ ,val)]
     (set! ,var __v__) __v__))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-loop)
  ;;(pretty-display "compile-loop")
  (unless (eof-object? (compile-token (forth-read)))
    (compile-loop)))

(define (compile-token tok)
  ;;(pretty-display (format "compile-token(~a)" tok))
  (let [(x #f)]
    (cond [(setq x (get-directive tok)) (x)]
          [(instruction? tok) (compile-instruction! tok)]
          [(setq x (parse-num tok)) (compile-constant! x)]
          [else (compile-call! tok)])
    tok))

(define (add-to-next-slot inst)
  ;;this assumes that we are not going to be overwriting code
  ;;(pretty-display (format "   add-to-next-slot(~a)" inst))
  ;;(pretty-display (format "     current-slot = ~a" current-slot))
  (if (= current-slot 4)
      (let [(cw (mlist inst))]
        ;;(pretty-display "        starting new word")
        (set! current-slot 1)
        (set! current-word cw)
        (vector-set! memory next-word cw)
        (set! next-word (add1 next-word))
        (set! current-addr (add1 current-addr)))

      (begin (set-mcdr! current-word (mlist inst))
             (set! current-word (mcdr current-word))
             (set! current-slot (add1 current-slot))))
  (set! last-inst inst))

(define (compile-instruction! inst)
  (when (and (member inst instructions-preceded-by-nops)
             (not (equal? last-inst ".")))
    (add-to-next-slot "."))
  (when (and (equal? current-slot 3)
             (not (member inst last-slot-instructions)))
    (add-to-next-slot "."))
  (add-to-next-slot inst)
  (when (member inst instructions-using-rest-of-word)
    (fill-rest-with-nops)))

(define (compile-constant! const)
  (add-to-next-slot "@p")
  (vector-set! memory next-word const)
  (set! next-word (add1 next-word)))

(define (compile-call! word)
  (let ([addr (get-word-address word)]);;TODO: ROM words
    (if addr
        (begin
          (when (> addr (max-address-size (add1 current-slot)))
            (fill-rest-with-nops))
          (add-to-next-slot "call")
          (add-to-next-slot addr))
        ;;else
        (begin
          (add-to-waiting word current-word)
          (skip-rest-of-word)))))

(define (fill-rest-with-nops)
  (unless (= current-slot 4)
    (add-to-next-slot ".")
    (fill-rest-with-nops)))

(define (set-next-empty-word! word)
  #f;;TODO
  )


(define (skip-rest-of-word)
  (set! current-slot 4))

(define (max-address-size slot)
  ;;returns the max address that can fit from SLOT(inclusive)to the end of the word
  (let ((len (- 4 slot)))
    (cond ((= len 1) 8);;last slot only has 3 bits
          ((< len 0) 0)
          (else (expt 2 (+ 3 (* 5 (sub1 len))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(add-directive!
 eof
 (lambda ()
   (fill-rest-with-nops)))

(define (comment)
  (unless (equal? (read-char) #\))
    (comment)))
(add-directive! "(" comment)

(define (insert-call cell addr)
  #f;;insert a call to ADDR in word CELL
  ;;TODO
  )

(add-directive!
 ":"
 (lambda ()
   (fill-rest-with-nops)
   (let* ([word (forth-read)]
          [waiting-list (get-waiting-list word)])
     (if waiting-list
         (begin (for [(cell waiting-list)]
                  (insert-call cell current-addr))
                (waiting-clear word))
         (begin
           (when (hash-has-key? words word)
             (pretty-display (format "WARNING: redefinition of word '~a'" word)))
           (hash-set! words word current-addr)))
     )
   ))

(add-directive!
 ".."
 (lambda () (fill-rest-with-nops)))

(add-directive! ;; page 23 of arrayforth users manual DB004
 ","
 (lambda ()
   (let* ([token (forth-read)]
          [data (parse-num token)])
     (if (not data)
         (raise (format "invalid token: ~a" token))
         (set-next-empty-word! data)))))

(add-directive!
 "node"
 (lambda ()
   (let* ([token (forth-read)]
          [node (parse-num token)])
     ;;TODO: validate 'node'
     (set! memory (vector-ref nodes node))
     (unless memory
       (set! memory (make-vector 64 0));;TODO: proper default?
       (vector-set! nodes node memory))
     (set! current-addr -1)
     (set! current-word #f)
     (set! next-word 0)
     (set! current-slot 4)
     )))

(define (here)
  (fill-rest-with-nops)
  (push stack next-word))

;;forces word alignment and pushes current aligned location onto compiler stack
(add-directive! "here" here)
(add-directive! "begin" here)

(add-directive!
 "for"
 (lambda ()
   (add-to-next-slot "push")
   (here)))

(add-directive!
 "next"
 (lambda ()
   (let ((addr (pop stack)))
     (when (> addr (max-address-size (add1 current-slot)))
       (fill-rest-with-nops))
     (add-to-next-slot "next")
     (add-to-next-slot addr))))

(define (compile-if-instruction inst)
  ;;cannot be in last word.
  (when (and (equal? current-slot 3)
             (not (member inst last-slot-instructions)))
    (add-to-next-slot "."))
  (add-to-next-slot inst)
  (pretty-display (format "[if]: current-addr = ~a" current-addr))
  (push stack current-addr)
  (set! current-slot 4));;force move to next word

;;If T is nonzero, program flow continues; otherwise jumps to matching 'then'
(define (if-directive)
  (compile-if-instruction "if"))
(add-directive! "if" if-directive)

;;If T is negative, program flow continues; otherwise jumps to matching 'then'
(define (-if-directive)
  (compile-if-instruction "-if"))
(add-directive! "-if" -if-directive)

;;jumps to matching 'then'
(add-directive!
 "ahead"
 (lambda ()
   (compile-if-instruction "jump")))

;;compiles a call to matching 'then'
(add-directive!
 "leap"
 (lambda ()
   (compile-if-instruction "call")))

(define (add-to-slot slot thing)
  (pretty-display (format "add-to-slot(~a,  ~a)" slot thing))
  (define (find-tail word)
    (if (equal? (mcdr word) '())
        word
        (find-tail (mcdr word))))
  (let ((tail (find-tail (vector-ref memory slot))))
    (if tail
        (set-mcdr! tail (mlist thing))
        (pretty-display "ERROR: add-to-slot -- invalid slot"))))

(add-directive!
 "then"
 (lambda ()
   (fill-rest-with-nops)
   (add-to-slot (pop stack) next-word)))

;;sets the compiler's location counter to a given address at
;;which following code will be compiled into
(add-directive!
 "org"
 (lambda ()
   (let ([n (parse-num (forth-read))])
     (set! current-addr (sub1 n))
     (set! next-word n)
     (set! current-slot 4))))

;;equivalent to 'if swap'. Typically used as a conditional exit from within a loop
(add-directive!
 "while"
 (lambda ()
   (if-directive)
   (swap stack)))

;;equivalent to '-if swap'. Typically used as a conditional exit from within a loop
(add-directive!
 "-while"
 (lambda ()
   (-if-directive)
   (swap stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-memory)
  (define (display-word word)
    (if (number? word)
        (display word)
        (unless (equal? word '())
          (display (format "~a " (mcar word)))
          (display-word (mcdr word)))))

  (define (display-mem mem [index 0])
    (let ([word (vector-ref mem index)])
      (unless (or (null? word)
                  (equal? word 0))
        (display (format "~a    " index))
        (display-word word)
        (newline)
        (when (< index 64)
          (display-mem mem (add1 index))))))

  (define node #f)
  (for ([i num-nodes])
    (when (setq node (vector-ref nodes i))
      (pretty-display (format "\nnode ~a" i))
      (display-mem node))))

(define file "test.aforth")
(compile-file file)
(display-memory)
