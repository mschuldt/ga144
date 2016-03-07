#lang racket

(require compatibility/defmacro
         "rom.rkt"
         "rom-dump.rkt")

(provide (all-defined-out))

(define num-words 64)
(define num-nodes 144)

(define opcodes (vector ";" "ex" "jump" "call" "unext" "next" "if"
                        "-if" "@p" "@+" "@b" "@" "!p" "!+" "!b" "!" "+*"
                        "2*" "2/" "-" "+" "and" "or" "drop" "dup" "pop"
                        "over" "a" "." "push" "b!" "a!"))

(define opcode-set (list->set (vector->list opcodes)))

(define address-required '("jump" "call" "next" "if" "-if"))

(define last-slot-instructions
  '(";" "unext" "@p" "!p" "+*" "+" "dup" "." ))

(define instructions-preceded-by-nops '("+" "+*"))

(define instructions-using-rest-of-word '(";" "ex"))

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
(define addresses->names (make-hash (for/list ((x named-addresses))
                                      (cons (cdr x) (car x)))))
(define names->addresses (make-hash named-addresses))

(define (port-name address)
  (hash-ref addresses->names address))

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

(define node-to-gpio-pins '((701 . 2)
                            (705 . 4)
                            (708 . 2)
                            (715 . 1)
                            (517 . 1)
                            (417 . 1)
                            (317 . 1)
                            (217 . 1)
                            (008 . 4)
                            (001 . 2)
                            (100 . 1)
                            (200 . 1)
                            (300 . 2)
                            (500 . 1)
                            (600 . 1)))

(define &UP #x145) ;325
(define &DOWN #x115) ;277
(define &LEFT #x175) ;373
(define &RIGHT #x1d5) ;469
(define &IO #x15d)
(define &DATA #x141)

(define &---U #x145)
(define &--L- #x175)
(define &--LU #x165)
(define &-D-- #x115)
(define &-D-U #x105)
(define &-DL- #x135)
(define &-DLU #x125)
(define &R--- #x1D5)
(define &R--U #x1C5)
(define &R-L- #x1F5)
(define &R-LU #x1E5)
(define &RD-- #x195)
(define &RD-U #x185)
(define &RDL- #x1B5)
(define &RDLU #x1A5)

(define MEM-SIZE #x301)

(define rom-ht (make-hash ROM-DUMP))

(define basic-rom-ht (make-hash basic-rom))
(define analog-rom-ht (make-hash analog-rom))
(define serdes-boot-rom-ht (make-hash serdes-boot-rom))
(define sync-boot-rom-ht (make-hash sync-boot-rom))
(define async-boot-rom-ht (make-hash async-boot-rom))
(define spi-boot-rom-ht (make-hash spi-boot-rom))
(define 1-wire-rom-ht (make-hash 1-wire-rom))

;; from section 2.3, DB002
(define analog-nodes '(709 713 717 617 117))
(define serdes-nodes '(1 701))
(define sync-boot-nodes '(300))
(define async-boot-nodes '(708))
(define spi-boot-nodes '(705))
(define 1-wire-nodes '(200))
(define SDRAM-addr-node 9)
(define SDRAM-control-node 8)
(define SDRAM-data-node 7)
(define eForth-Bitsy-node 105)
(define eForth-stack-node 106)
(define SDRAM-mux-node 107)
(define SDRAM-idle-node 108)

(define (get-node-rom node)
  (cond ((member node analog-nodes) analog-rom-ht)
        ((member node serdes-nodes) serdes-boot-rom-ht)
        ((member node sync-boot-nodes) sync-boot-rom-ht)
        ((member node async-boot-nodes) async-boot-rom-ht)
        ((member node spi-boot-nodes) spi-boot-rom-ht)
        ((member node 1-wire-nodes) 1-wire-rom-ht)
        ;;TODO: SDRAM-addr-node, SDRAM-control-node, SDRAM-data-node
        ;;       eForth-Bitsy-node,  eForth-stack-node,  SDRAM-mux-node
        ;;       SDRAM-idle-node
        ;;   => currently default to basic rom
        (else basic-rom-ht)))

;; This is the type that holds compiled code and other node info.
;; Compiling a program returns a list of these
(struct node (coord mem len [symbols #:auto] [word-dict #:auto]
                    [a #:auto] [b #:auto] [io #:auto] [stack #:auto] [p #:auto]
                    [address-cells #:auto] [consts #:auto])
        #:mutable #:transparent)
;; 'coord' is the node coordinate this code belongs to
;; 'mem' is the vector of compiled words
;; 'len' is how many instructions are used in mem. len <= length(mem)
;;       the remaining words in mem are all #f
;; 'symbols' a list of symbol structs

(struct compiled (nodes bootstream) #:mutable #:transparent)
;; struct to hold compiled code
;; 'nodes': list of 'node' structs
;; 'bootstream': type of bootstream to generate

(struct symbol (name address line col))

(struct token (tok line col))

(struct bootstream (name start path))

(define bootstream-types '("async" ;; load through node 708 serial
                           "2wire" ;; load through node 300 2wire
                           "async-target" ;; in host node 708 to target node 300
                           ))
(define default-bootstream-type "async")

(define (create-node coord [mem #f] [len 0])
  (let ((new (node coord mem len)))
    (set-node-symbols! new (list))
    (set-node-word-dict! new (make-hash))
    (set-node-address-cells! new (set))
    (set-node-consts! new (make-hash))
    new))

(define (coord->index n)
  (+ (* (quotient n 100) 18) (remainder n 100)))

(define (index->coord n)
  (+ (* (quotient n 18) 100) (remainder n 18)))

(define (coord->row coord)
  (quotient coord 100))

(define (coord->col coord)
  (remainder coord 100))

(define (convert-direction coord dir)
  ;;converts DIR={North, East, South, West} Into Left, Up, Down, or Right
  ;;depending on the nodes coordinate COORD
  (let ([x (remainder coord 100)]
        [y (quotient coord 100)])
    (cond
     [(equal? dir "north")
      (if (= (modulo y 2) 0) "down" "up")]
     [(equal? dir "south")
      (if (= (modulo y 2) 0) "up" "down")]
     [(equal? dir "east")
      (if (= (modulo x 2) 0) "right" "left")]
     [(equal? dir "west")
      (if (= (modulo x 2) 0) "left" "right")]
     [else (raise "convert-direction: invalid direction")])))

;;successfully parses a token as a number, or returns false
(define (parse-num tok)
  (when (and (> (string-length tok) 2)
             (eq? (string-ref tok 0) #\0)
             (or (eq? (string-ref tok 1) #\x)
                 (eq? (string-ref tok 1) #\b)))
    ;; convert format 0x... to #x...
    (set! tok (list->string (cons #\# (cdr (string->list tok))))))
  (string->number tok))

(define (get-address name [node #f])
  (cond ((hash-has-key? names->addresses name) ;;normal address names
         (hash-ref names->addresses name))
        ((and node  ;;relative names
              (member name '("north" "south" "east" "west")))
         (convert-direction node name))
        (else (parse-num name)))) ;; literals and word addresses

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;stack macros
(defmacro push (list item)
  `(set! ,list (cons ,item ,list)))

(defmacro pop (list)
  `(if (equal? ,list '())
       (pretty-display "ERROR: pop -- list is empty")
       (begin0 (car ,list) (set! ,list (cdr ,list)))))

(defmacro swap (list)
  `(set! ,list (cons (cadr ,list) (cons (car ,list) (cddr ,list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(defmacro setq (var val)
  `(let [(__v__ ,val)]
     (set! ,var __v__) __v__))

(defmacro while (condition code)
  `(letrec ((fn (lambda ()
                  (when ,condition
                    ,code
                    (fn)))))
     (fn)))

(defmacro enum (syms)
  (let ((i 0)
        (code '()))
    (for ([sym syms])
      (set! code (cons (list 'define sym i) code))
      (set! i (add1 i)))
    (cons 'begin code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define & bitwise-and)
(define ^ bitwise-xor)
(define << arithmetic-shift)
(define (>> x n) (arithmetic-shift x (- n)))
(define ior bitwise-ior)
(define ~ bitwise-not)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enum (LEFT UP DOWN RIGHT))
(define port-names (vector "LEFT" "UP" "DOWN" "RIGHT"))

(enum (N E S W))
(define (get-direction coord dir)
  ;;converts dir={N, E, S, W} into an address for node COORD
  (define dir-names (vector "north" "east" "south" "west"))
  (cdr (assoc (convert-direction coord
                                 (vector-ref dir-names dir))
              named-addresses)))



(define (comma-join things)
  (when (vector? things)
    (set! things (vector->list things)))
  (string-join (for/list ((thing things))
                 (cond ((string? thing)
                        thing)
                       ((number? thing)
                        (number->string thing))
                                        ;(else (raise "invalid thing"))
                       (else (format "~a" thing))
                       ))
               ","))

;;Determine if we are in RAM or ROM, return the index for that memory location
;;DB001 section 2.2  Top half of RAM and ROM is repeated
(define (region-index addr)
  (set! addr (& addr #xff)) ;; get rid of extended arithmetic bit
  (if (>= addr #x80);;ROM
      (if (> addr #xbf)
          (- addr #x40)
          addr)
      (if (> addr #x3F)
          (- addr #x40)
          addr)))

(defmacro assert (x)
  `(unless ,x
     (error ,(format "Assertion failed: ~a" x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler options
(define auto-nop-insertion #t)

(define compile-0-as-dup-dup-or #f)
(define reorder-words-with-fallthrough #f)
