#lang racket

;;references:
;; - colorforth blocks 1404-1412
;; - DB004 arrayForth User's Manual, section 5

(require compatibility/defmacro
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
(define used-nodes '())

(define last-inst #f)

(define stack '())

(define extended-arith 0);;0x200 if extended arithmetic is enabled, else 0

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
                               (compile-loop)
                               (when memory ;;make sure last instruction is full
                                 (fill-rest-with-nops))
                               used-nodes)))

(define (compile-string str)
  #f;;TODO
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;info about the current target core
(define memory #f) ;;vector of words
(define current-addr #f);;index of current word in memory
(define current-word #f);; tail of current word list
(define next-addr #f) ;;index of next word in memory
(define current-slot #f);;index of the next slot in current-word

(define words (make-hash)) ;;word definitions -> addresses
;;TODO: need to have a seporate mapping for each core
;;TOOD: create a struct for each core. memory, current/next-addr/slot, words
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

(define (org n)
  (set! current-addr n)
  (set! next-addr (add1 n))
  (set! current-word (vector-ref memory n))
  (set! current-slot 0))

(define (goto-next-word) (org next-addr))

(define (add-to-next-slot inst)
  ;;this assumes that we are not going to be overwriting code
  (pretty-display (format "   add-to-next-slot(~a)" inst))
  (pretty-display (format "     current-slot = ~a" current-slot))

  (vector-set! current-word current-slot inst)
  (set! current-slot (add1 current-slot))
  (when (= current-slot 4)
    (goto-next-word))
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
  (vector-set! memory next-addr const)
  (set! next-addr (add1 next-addr)))

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
          (goto-next-word)))))

(define (fill-rest-with-nops)
  (pretty-display (format "[fill-rest-with-nops]current-word = ~a" current-word))
  (unless (= current-slot 0)
    (add-to-next-slot ".")
    (fill-rest-with-nops)))

(define (set-next-empty-word! word)
  #f;;TODO
  )

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
           (hash-set! words word (make-addr current-addr)))))))

;;forces word alignment
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

;;node (nn)
;;starts compilation for the given node with number in yyxx notation
(add-directive!
 "node"
 (lambda ()
   (when memory ;;make sure last instruction is full
     (fill-rest-with-nops))
   (let* ([token (forth-read)]
          [node (parse-num token)])
     ;;TODO: validate 'node'
     (set! memory (vector-ref nodes node))
     (unless memory
       (set! memory (list->vector (for/list ([_ num-words]) (make-vector 4 #f))))
       (vector-set! nodes node memory))
     (set! used-nodes (cons (cons node memory) used-nodes))
     (org 0))))

(define (make-addr addr)
  (bitwise-ior addr extended-arith))

;;+cy
;;forces word alignment then turns P9 on in the location counter. Places in memory
;;subsequently defined will be run in Extended Arithmetic Mode if reached by
;;jump, call, execute or return to those places.
(add-directive!
 "+cy"
 (lambda ()
   (fill-rest-with-nops)
   (set! extended-arith #x200)))

;;-cy
;;forces word alignment then turns P9 off in the location counter
(add-directive!
 "-cy"
 (lambda ()
   (fill-rest-with-nops)
   (set! extended-arith 0)))

(define (here)
  (fill-rest-with-nops)
  (push stack (make-addr current-addr)))

;;here (-n)
;;forces word alignment and pushes current aligned location onto compiler stack
(add-directive! "here" here)
;;begin (-a)
;;forces word alignment and saves here to be used as a transfer destination.
(add-directive! "begin" here)

;;for (-a) (n)
;;pushes n onto the return stack, forces word alignment and saves 'here' to be
;;used as a transfer destination by the directive that ends the loop.
;;There are times when it is useful to decompose this directive's actions so
;;that the pushing of the loop count and the start of the loop itself may be
;;separated by such things as initialization code or a word definition. In this
;;case you may write "push <other things> begin".
(add-directive!
 "for"
 (lambda ()
   (add-to-next-slot "push")
   (here)))

(define (compile-next-type inst)
  (let ((addr (pop stack)))
    (when (> addr (max-address-size (add1 current-slot)))
      (fill-rest-with-nops))
    (add-to-next-slot inst)
    (add-to-next-slot addr)))

;;next (a)
;;ends a loop with conditional transfer to the address a. If R is zero when next
;;is executed, the return stack is popped and program flow continues. Otherwise
;;R is decremented by one and control is transferred to a.
(add-directive! "next" (lambda () (compile-next-type "next")))
;; end (a)
;; unconditionally jumps to a
(add-directive! "end" (lambda () (compile-next-type "jump")))
;;until (a)
;;If T is nonzero, program flow continues; otherwise jumps to a.
;;Typically used as a conditional exit at the end of a loop.
(add-directive! "until" (lambda () (compile-next-type "if")))
;;-until (a)
;;If T is negative, program flow continues; otherwise jumps to a. Used like 'until'
(add-directive! "-until" (lambda () (compile-next-type "-if")))

;;*next (ax-x)
;;equivalent to 'swap next'
(add-directive!
 "*next"
 (lambda ()
   (swap stack)
   (compile-next-type "next")))

(define (compile-if-instruction inst)
  ;;cannot be in last word.
  (when (and (equal? current-slot 3)
             (not (member inst last-slot-instructions)))
    (add-to-next-slot "."))
  (add-to-next-slot inst)
  (pretty-display (format "[if]: current-addr = ~a" current-addr))
  (push stack (make-addr current-addr))
  (goto-next-word))

;;If T is nonzero, program flow continues; otherwise jumps to matching 'then'
(define (if-directive)
  (compile-if-instruction "if"))
(add-directive! "if" if-directive)

(define (-if-directive)
  (compile-if-instruction "-if"))

;;if (-r)
;;If T is negative, program flow continues; otherwise jumps to matching 'then'
(add-directive! "-if" -if-directive)

;;zif (-r)
;;If R is zero, pops the return stack and program flow continues;
;;otherwise decrements R and jumps to matching 'then'
(add-directive!
 "zif"
 (lambda ()
   (compile-if-instruction "next")))

;;ahead (-r)
;;jumps to matching 'then'
(add-directive!
 "ahead"
 (lambda ()
   (compile-if-instruction "jump")))

;;leap (-r)
;;compiles a call to matching 'then'
(add-directive!
 "leap"
 (lambda ()
   (compile-if-instruction "call")))

(define (add-to-slot slot thing)
  (pretty-display (format "add-to-slot(~a,  ~a)" slot thing))
  (define (find-last-empty word [n 0])
    (pretty-display (format "find-last-empty(~a,  ~a)" word n))
    (if (< n 4)
        (if (vector-ref word n)
            (find-last-empty word (add1 n))
            n)
        #f))

  (let* ([word (vector-ref memory slot)]
         [last (and (vector? word) (find-last-empty word))])

    (if last
        (vector-set! word last thing)
        (pretty-display "ERROR: add-to-slot -- invalid slot"))))

;;then (r)
;;forces word alignment and resolves a forward transfer.
(add-directive!
 "then"
 (lambda ()
   (fill-rest-with-nops)
   (add-to-slot (pop stack) current-addr)))

;;org (n)
;;sets the compiler's location counter to a given address at
;;which following code will be compiled into
(add-directive!
 "org"
 (lambda ()
   (let ([n (parse-num (forth-read))])
     ;;TODO: validate n
     (org n))))

;;while (x-rx)
;;equivalent to 'if swap'. Typically used as a conditional exit from within a loop
(add-directive!
 "while"
 (lambda ()
   (if-directive)
   (swap stack)))

;;-while (x-rx)
;;equivalent to '-if swap'. Typically used as a conditional exit from within a loop
(add-directive!
 "-while"
 (lambda ()
   (-if-directive)
   (swap stack)))

;;' (-a)
;;(tick) places the address of an F18 red word on the compiler's stack.
(add-directive!
 "`"
 (lambda ()
   (let* ([word (forth-read)]
          [addr (get-word-address word)])
     (if addr
         (push stack addr)
         (pretty-display (format "ERROR: ` -- \"~a\" is not defined" word))))))

(define named-addresses '(("right" . #x1D5)
                          ("down" . #x115)
                          ("left" . #x175)
                          ("up" . #x145)
                          ("io" . #x15D)
                          ("ldata" . #x171)
                          ("data" . #x141)
                          ("warp" . #x157)
                          ("center" . #x1A5)
                          ("top" . #x1B5)
                          ("side" . #x185)
                          ("corner" . #x195)))

(for ([addr named-addresses])
  (add-directive!
   (car addr)
   ((lambda (a) (lambda () (compile-constant! a))) (cdr addr))))

(define io-places '(("---u" . #x145)
                    ("--l-" . #x175)
                    ("--lu" . #x165)
                    ("-d--" . #x115)
                    ("-d-u" . #x105)
                    ("-dl-" . #x135)
                    ("-dlu" . #x125)
                    ("r---" . #x1D5)
                    ("r--u" . #x1C5)
                    ("r-l-" . #x1F5)
                    ("r-lu" . #x1E5)
                    ("rd--" . #x195)
                    ("rd-u" . #x185)
                    ("rdl-" . #x1B5)
                    ("rdlu" . #x1A5)))

(for ([place io-places])
  (hash-set! words (car place) (make-addr (cdr place))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-memory nodes)
  ;;NODES is a list with member format: (nodeNum . memoryVector)
  (define (display-word word [n 0])
    (if (number? word)
        (display word)
        (when (< n 4)
          (display (format "~a " (vector-ref word n)))
          (display-word word (add1 n)))))

  (define (display-mem mem [index 0])
    (let ([word (vector-ref mem index)])
      (unless (or (equal? word (vector #f #f #f #f))
                  (equal? word 0))
        (display (format "~a    " index))
        (display-word word)
        (newline)
        (when (< index num-words)
          (display-mem mem (add1 index))))))

  (define (display-node nodes)
    (unless (null? nodes)
      (pretty-display (format "\nnode ~a" (caar nodes)))
      (display-mem (cdar nodes))
      (display-node (cdr nodes))))
  (display-node nodes))o

(define file "test.aforth")
(display-memory (compile-file file))


;;unext (a)
;;ends a micronext loop. Since the loop occurs entirely within a single
;;instruction word, the address is superfluous; it is present only so that the
;;form "<n> for ... unext" may be written. The micronext opcode may be compiled
;;into any of the four slots.
