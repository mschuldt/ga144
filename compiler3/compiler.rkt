#lang racket
(require compatibility/defmacro
         scheme/mpair
         "read.rkt")
(provide compile-file compile-string)

(define instructions (vector ";" "ret" "ex" "jump" "call" "unext" "next" "if"
                             "-if" "@p" "@+" "@b" "@" "!p" "!+" "!b" "!" "+*"
                             "2*" "2/" "-" "+" "and" "or" "drop" "dup" "pop"
                             "over" "a" "." "nop" "push" "b!" "a!"))


(define address-required '("jump" "call" "next" "if" "-if"))

(define last-slot-instructions
  '(";" "ret" "unext" "@p" "!p" "+*" "+" "dup" "." "nop"))

(define instructions-preceded-by-nops '("+" "+*"))

(define instructions-using-entire-word '(";" "ret" "ex" "unext"))

(define num-cores 144)
(define num-words 64)
(define cores (make-vector num-cores #f)) ;;core# -> memory vector

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-file file)
  (call-with-input-file file compile-and-run)
  (current-input-port code-port)
  (compile-loop))

(define (compile-string str)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;info about the current target core
(define memory #f) ;;vector of words
(define current-word #f);; tail of current word list
(define next-word #f) ;;index of next word in memory
(define current-slot #f);;index into current-word


(define words (make-hash)) ;;word definitions -> addresses
;;TODO: need to have a seporate mapping for each core
;;TOOD: create a struct for each core. memory, current/next-word/slot, words
(define (add-word! name code)
  (hash-set! words name code))

(define (get-word-address name)
  (and (hash-has-key? words name)
       (hash-ref words name)))

(define (instruction? token)
  ;;TODO
  )

;; compiler directive - words executed at compile time
(define directives (make-hash));;directive names -> functions
(define (add-directive! name code)
  (hash-set! directives name code))
(define (get-directive name)
  (and (hash-has-key? directives name)
       (hash-ref directives name)))

;;successfully parses a token as a number, or returns false
(define (parse-num tok)
  ;;TODO
  )

(defmacro setq (var val)
  `(let [(__v__ ,val)]
     (set! ,var __v__) __v__))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-loop)
  (unless (eof-object? (compile-token (forth_read)))
    (compile-loop)))

(define (compile-token tok)
  (let [(x #f)]
    (cond [(setq x (get-directive tok)) (x tok)]
          [(instruction? tok) (compile-instruction! tok)]
          [(setq x (get-word-address tok)) (compile-call! x)];;TODO: ROM words
          [(setq x (parse-num tok)) (compile-constant! tok)]
          [else (raise (string-append "unknown token: " tok))])
    tok))

(define (add-to-next-slot inst)
  ;;this assumes that we are not going to be overwriting code
  (if (= current-slot 4)
      (let [(cw (mlist inst))]
        (set! current-slot 0)
        (set! current-word cw)
        (vector-set! memory next-word cw)
        (set! next-word (add1 next-word)))

      (begin (set-mcdr! current-word (mlist inst))
             (set! current-slot (add1 current-slot)))))

(define (compile-instruction! inst)
  (add-to-next-slot inst))

(define (compile-constant! const)
  (add-to-next-slot "@p")
  (vector-set! memory next-word const)
  (set! next-word (add1 next-word)))

(define (compile-call! addr)
  (when (> addr (max-address-size (add1 current-slot)))
    (fill-rest-with-nops))
  (when (> addr (max-address-size (add1 current-slot)))
    (pretty-display (format "WARNING: address '~a' is to large" addr)))
  (add-to-next-slot "call")
  (add-to-next-slot addr))

(define (fill-rest-with-nops)
  (unless (= current-slot 4)
    (add-to-next-slot ".")
    (fill-rest-with-nops)))

(define (max-address-size nslots)
  ;;returns the max address that can fit in NSLOTS number of slots
  (let ((len (- 4 nslots)))
    (if (= len 1)
        8;;last slot only has 3 bits
        (expt 2 (+ 3 (* 5 (sub1 len)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-directive!
 eof
 (lambda ()
   (fill-rest-with-nops)))

(define (comment compiler)
  (unless (equal? (read-char) #\))
    (comment compiler)))
(add-directive! "(" comment)
