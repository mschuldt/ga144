#lang racket
(require compatibility/defmacro
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
(define cores (make-vector num-cores #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-file file)
  (call-with-input-file file compile-and-run)
  (current-input-port code-port)
  (compile-loop))

(define (compile-string str)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define words (make-hash)) ;;word definitions -> addresses
(define (add-word! name code)
  (hash-set! dict name code))

(define (get-word-address name)
  (and (hash-has-key? dict name)
       (hash-ref dict name)))

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
  `(begin (set! ,var ,val) ,val))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-loop)
  (unless (eof-object? (compile-token (forth_read)))
    (compile-loop)))

(define (compile-token tok)
  (let [(x #f)]
    (cond [(setq x (get-directive tok)) (x tok)]
          [(instruction? tok) (compile-instruction! tok)]
          [(setq x (get-word-address tok)) (compile-call! x)]
          [(setq x (parse-num tok)) (compile-constant! tok)]
          [else (raise (string-append "unknown token: " tok))])
    tok))

(define (compile-instruction! inst)
  ;;TODO
  )

(define (compile-constant! const)
  ;;TODO
  )

(define (compile-call! addr)
  ;;TODO
  )

(define (fill-rest-with-nops)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-directive!
 eof
 (lambda ()
   (fill-rest-with-nops)))

(define (comment compiler)
  (unless (equal? (read-char) #\))
    (comment compiler)))
(add-directive! "(" comment)
