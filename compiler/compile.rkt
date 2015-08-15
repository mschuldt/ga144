#lang racket

;;references:
;; - colorforth blocks 190-192,1404-1412
;; - DB001 F18A Technology Reference
;; - DB004 arrayForth User's Manual, section 5


(require "read.rkt"
         "assemble.rkt"
         "disassemble.rkt"
         "../common.rkt")

(provide compile compile-file display-compiled)

(define DEBUG? #f)

(define nodes #f) ;;node# -> code struct
(define used-nodes '())

(define last-inst #f)
;;coordinate of the current node we are compiling for
(define current-node-coord #f)
;;struct of the current node we are compiling for
(define current-node #f)

(define stack '())

(define extended-arith 0);;0x200 if extended arithmetic is enabled, else 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile port)
  (reset!)
  (when (string? port)
    (set! port (open-input-string port)))
  (current-input-port port)
  (compile-loop)
  (when memory
    (fill-rest-with-nops) ;;make sure last instruction is full
    (set-node-len! current-node (sub1 next-addr)))
  used-nodes)

(define (compile-file file)
  (call-with-input-file file compile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;info about the current target core
(define memory #f) ;;vector of words
(define current-addr #f);;index of current word in memory
(define current-word #f);; tail of current word list
(define next-addr #f) ;;index of next word in memory
(define current-slot #f);;index of the next slot in current-word
(define words #f) ;;word definitions -> addresses

(define (add-word! name addr)
  (set-node-symbols! current-node
                     (cons (symbol name addr)
                           (node-symbols current-node)))
  (hash-set! words name addr))

(define waiting #f);;word -> list of cells waiting for the word's address
(define (add-to-waiting word addr-cell)
  (unless (hash-has-key? waiting word)
    (hash-set! waiting word (list)))
  (hash-set! waiting word (cons addr-cell (hash-ref waiting word))))
(define (get-waiting-list word)
  (and (hash-has-key? waiting word)
       (hash-ref waiting word)))
(define (waiting-clear word)
  (hash-set! words word #f))

(define (make-addr addr)
  (bitwise-ior addr extended-arith))

(define io-places-hash (make-hash))
(for ([place io-places])
  (hash-set! io-places-hash (car place) (make-addr (cdr place))))

;;TODO: initial scan should resolve tail calls and collect word names
(define (get-word-address name)
  (or (and (hash-has-key? words name)
           (hash-ref words name))
      (and (hash-has-key? io-places-hash name)
           (hash-ref io-places-hash name))))

(define (instruction? token)
  (set-member? opcode-set token))

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

(define (reset!)
  (set! nodes (make-vector num-nodes #f))
  (for ([i num-nodes])
    (vector-set! nodes i (create-node (index->coord i)
                                      (list->vector (for/list ([_ num-words])
                                                      (make-vector 4 #f))))))
  (set! used-nodes '())
  (set! last-inst #f)
  (set! stack '())
  (set! memory #f)
  (set! current-word #f)
  (set! current-addr #f)
  (set! next-addr #f)
  (set! words (make-hash))
  (set! waiting (make-hash))
  (define-named-addresses!))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-loop)
  (unless (eof-object? (compile-token (forth-read)))
    (compile-loop)))

(define (compile-token tok)
  (when DEBUG? (printf "compile-token(~a) [~a  ~a  ~a]\n" tok current-addr current-slot next-addr))
  (let [(x #f)]
    (cond [(setq x (get-directive tok)) (x)]
          [(instruction? tok) (compile-instruction! tok)]
          [(setq x (parse-num tok)) (compile-constant! x)]
          [else (compile-call! tok)])
    tok))

(define (org n)
  (when DEBUG? (printf "        org(~a)\n" n))
  (set! current-addr n)
  (set! next-addr (add1 n))
  (set! current-word (vector-ref memory n))
  (set! current-slot 0))

(define (goto-next-word) (org next-addr))

(define (add-to-next-slot inst)
  ;;this assumes that we are not going to be overwriting code
  (when DEBUG? (printf "        add-to-next-slot(~a)\n" inst))
  (unless current-word (raise "Error. You probably forgot to use 'node' first"))
  (vector-set! current-word current-slot inst)
  (set! current-slot (add1 current-slot))
  (when (= current-slot 4)
    (goto-next-word))
  (set! last-inst inst))

(define (compile-instruction! inst)
  (when DEBUG? (printf "    compile-instruction!(~a)\n" inst))
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
  (when DEBUG? (printf "    compile-constant!(~a)\n" const))
  (add-to-next-slot "@p")
  (set-next-empty-word! const))

(define (compile-call! word)
  (when DEBUG? (printf "    compile-call!(~a)\n" word))
  (let ([addr (get-word-address word)]);;TODO: ROM words
    (if addr
        (begin
          (unless (address-fits? addr current-slot)
            (fill-rest-with-nops))
          (when DEBUG? (printf "       address = ~a\n" addr))
          (let ((next (forth-read)))
            (if (equal? next ";")
                (add-to-next-slot "jump")
                (begin (add-to-next-slot "call")
                       (forth-read next))))
          (add-to-next-slot addr)
          (unless (= current-slot 0)
            (goto-next-word)))
        ;;else
        (begin
          (when DEBUG? (printf "       waiting on address....\n"))
          (add-to-waiting word current-word)
          (goto-next-word)))))

(define (fill-rest-with-nops)
  (unless (= current-slot 0)
    (add-to-next-slot ".")
    (fill-rest-with-nops)))

(define (set-next-empty-word! word)
  (if (= current-slot 0)
      (begin (vector-set! memory current-addr word)
             (org next-addr))
      (begin (vector-set! memory next-addr word)
             (set! next-addr (add1 next-addr)))))

;; map jump instruction slots to bit masks for their address fields
(define address-masks (vector #x3ff #xff #x7))

(define (address-fits? destination-addr jump-slot)
  ;; returns #t if DESTINATION-ADDR is reachable from the current word
  ;; JUMP-SLOT is the slot of the jump/call instruction
  (and jump-slot
       (>= jump-slot 0)
       (< jump-slot 3)
       (let* ([mask (vector-ref address-masks jump-slot)]
              [~mask (& (~ mask) #x3ffff)]
              [min-dest (& ~mask next-addr)]
              [max-dest (ior (& ~mask next-addr) (& mask destination-addr))])
         (and (>= destination-addr min-dest)
              (<= destination-addr max-dest)))))

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
           (add-word! word (make-addr current-addr)))))))

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
     (fill-rest-with-nops)
     (set-node-len! current-node (sub1 next-addr)))
   (let* ([token (forth-read)]
          [coord (parse-num token)]
          [index (coord->index coord)])
     ;;TODO: validate 'node'
     ;;assert (node-coord node) == coord
     (set! current-node (vector-ref nodes index))
     (set! memory (node-mem current-node))
     (set! words (node-word-dict current-node))
     ;;TODO: should calling 'node' multiple times be ok?
     ;;      if so, don't add current-node to used-nodes again
     (set! used-nodes (cons current-node used-nodes))
     (set! current-node-coord coord)
     (org 0))))

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
    (unless (address-fits? addr current-slot)
      (fill-rest-with-nops))
    (add-to-next-slot inst)
    (add-to-next-slot addr)
    (unless (= current-slot 0)
      (goto-next-word))))

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
  (define (find-last-empty word [n 0])
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boot descriptors
;; DB004 section 5.5.1

(define (set-register-helper name set-fn)
  (let* ((n (forth-read))
         (addr (and n (string->number n))))
    (when (and (not addr)
               (not (setq addr (get-word-address n))))
      (raise (format "unknown address for compiler directive '~a': ~a" name n)))
    (unless current-node
      (raise (format "must select node before '~a'" name)))
    (set-fn current-node addr)))

;; /b (a)
;; Specifies an initial value for register B.
;; Default value is the address of the IO register, as at reset.
(add-directive!
 "/b"
 (lambda ()
   (set-register-helper "/b" set-node-b!)))
;; /a (n)
;; Specifies an initial value for register A.
;; Default value is unspecified, as at reset.
(add-directive!
 "/a"
 (lambda ()
   (set-register-helper "/a" set-node-a!)))

;;/io (n)
;;Specifies a value to be loaded into the IO register.
(add-directive!
 "/io"
 (lambda ()
   (set-register-helper "/io" set-node-io!)))

;;/p (a)
;;Specifies an initial value for register P. Default value is xA9 which is
;;the routine warm in every node's ROM.
(add-directive!
 "/p"
 (lambda ()
   (set-register-helper "/p" set-node-p!)))

;; /stack n <n values>
;; Specifies up to ten values to be pushed onto the data stack, with the
;; rightmost value on top. For example 30 20 10 3 /stack produces the same
;; effect as though a program had executed code 30 20 10
(add-directive!
 "/stack"
 (lambda ()
   (let* ((tok (forth-read))
          (len (and tok (string->number tok)))
          (stack '())
          (val #f))
     (when (or (not len)
               (< len 0)
               (> len 10))
       (raise (format "invalid number for /stack item count: '~a'" len)))

     (while (> len 0)
       (begin
         (set! tok (forth-read))
         (set! val (and tok (string->number tok)))
         (when (and (not val)
                    (not (setq val (get-word-address tok))))
           (raise (format "invalid stack value: ~a" tok)))
         (push stack val)
         (set! len (sub1 len))))
     (set-node-stack! current-node (reverse stack)))))

;;NOTE: +node, /ram, and /part are note supported

(for [(dir (list "north" "south" "east" "west"))]
  (add-directive!
   dir
   ((lambda (dir)
      (lambda () ((get-directive (convert-direction current-node-coord dir)))))
    dir)))


(define (define-named-addresses!)
  (for ([addr named-addresses])
    (add-directive!
     (car addr)
     ((lambda (a) (lambda () (compile-constant! a))) (cdr addr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-compiled nodes)
  ;;NODES is a list of node structs
  (define (display-word word [n 0])
    (if (number? word)
        (display word)
        (when (< n 4)
          (display (format "~a " (vector-ref word n)))
          (display-word word (add1 n)))))

  (define (display-mem mem [index 0])
    (let ([word (vector-ref mem index)])
      (unless (equal? word (vector #f #f #f #f))
        (display (format "~a    " index))
        (display-word word)
        (newline)
        (when (< index num-words)
          (display-mem mem (add1 index))))))

  (define (display-node nodes)
    (unless (null? nodes)
      (pretty-display (format "\nnode ~a" (node-coord (car nodes))))
      (display-mem (node-mem (car nodes)))
      (display-node (cdr nodes))))
  (display-node nodes))

;;unext (a)
;;ends a micronext loop. Since the loop occurs entirely within a single
;;instruction word, the address is superfluous; it is present only so that the
;;form "<n> for ... unext" may be written. The micronext opcode may be compiled
;;into any of the four slots.
