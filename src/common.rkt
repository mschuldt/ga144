#lang racket ;; -*- lexical-binding: t -*-

(require compatibility/defmacro
         "rom.rkt"
         "rom-dump.rkt"
         "el.rkt")

(provide (all-defined-out))

(define num-words 100);;TODO: compile arbitrarily large programs per node but warn if > 64 words
(define num-nodes 144)

(define opcodes (vector ";" "ex" "jump" "call" "unext" "next" "if"
                        "-if" "@p" "@+" "@b" "@" "!p" "!+" "!b" "!" "+*"
                        "2*" "2/" "-" "+" "and" "or" "drop" "dup" "pop"
                        "over" "a" "." "push" "b!" "a!"))

;; from DB002. Typical time. pico seconds
(define opcode-time '((";" . 5200)
                      ("ex" . 5200)
                      ("jump" . 5200)
                      ("call" . 5200)
                      ("unext" . 2400)
                      ("next" . 5200)
                      ("if" . 5200)
                      ("-if" . 5200)
                      ("@p" . 5000)  ;; TODO @p time is slot dependent
                      ("@+" . 5000) ;; 3500 when reading from IO register
                      ("@b" . 5000)
                      ("@" . 5000)
                      ("!p" . 5000)
                      ("!+" . 5000)
                      ("!b" . 5000)
                      ("!" . 5000)
                      ("+*" . 1400)
                      ("2*" . 1400)
                      ("2/" . 1400)
                      ("-" . 1400)
                      ("+" . 1400)
                      ("and" . 1400)
                      ("or" . 1400)
                      ("drop" . 1400)
                      ("dup" . 1400)
                      ("pop" . 1400)
                      ("over" . 1400)
                      ("a" . 1400)
                      ("." . 1400)
                      ("push" . 1400)
                      ("b!" . 1400)
                      ("a!" . 1400)))

(define opcode-time-v (list->vector (map cdr opcode-time)))

(define opcode-set (list->set (vector->list opcodes)))

(defconst address-required '("jump" "call" "next" "if" "-if"))

(defconst last-slot-instructions
  '(";" "unext" "@p" "!p" "+*" "+" "dup" "." ))

(defconst instructions-preceded-by-nops '("+" "+*"))

(defconst instructions-using-rest-of-word '(";" "ex"))

(defconst named-addresses '(("right" . #x1D5)
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

(define address-names (make-hash (map (lambda (x) (cons (cdr x) (car x)))
                                      (append named-addresses io-places))))

(defconst &UP #x145) ;325
(defconst &DOWN #x115) ;277
(defconst &LEFT #x175) ;373
(defconst &RIGHT #x1d5) ;469
(defconst &IO #x15d)
(defconst &DATA #x141)

(defconst &---U #x145)
(defconst &--L- #x175)
(defconst &--LU #x165)
(defconst &-D-- #x115)
(defconst &-D-U #x105)
(defconst &-DL- #x135)
(defconst &-DLU #x125)
(defconst &R--- #x1D5)
(defconst &R--U #x1C5)
(defconst &R-L- #x1F5)
(defconst &R-LU #x1E5)
(defconst &RD-- #x195)
(defconst &RD-U #x185)
(defconst &RDL- #x1B5)
(defconst &RDLU #x1A5)

(defconst MEM-SIZE #x301)

(define rom-ht (make-hash ROM-DUMP))

(define basic-rom-ht (make-hash basic-rom))
(define analog-rom-ht (make-hash analog-rom))
(define serdes-boot-rom-ht (make-hash serdes-boot-rom))
(define sync-boot-rom-ht (make-hash sync-boot-rom))
(define async-boot-rom-ht (make-hash async-boot-rom))
(define spi-boot-rom-ht (make-hash spi-boot-rom))
(define 1-wire-rom-ht (make-hash 1-wire-rom))
(define SDRAM-addr-rom-ht (make-hash SDRAM-addr-rom))
(define SDRAM-control-rom-ht (make-hash SDRAM-control-rom))
(define SDRAM-data-rom-ht (make-hash SDRAM-data-rom))
(define eForth-bitsy-rom-ht (make-hash eForth-bitsy-rom))
(define eForth-stack-rom-ht (make-hash eForth-stack-rom))
(define SDRAM-mux-rom-ht (make-hash SDRAM-mux-rom))
(define SDRAM-idle-rom-ht (make-hash SDRAM-idle-rom))

;; from section 2.3, DB002
(defconst analog-nodes '(709 713 717 617 117))
(defconst serdes-nodes '(1 701))
(defconst sync-boot-nodes '(300))
(defconst async-boot-nodes '(708))
(defconst spi-boot-nodes '(705))
(defconst 1-wire-nodes '(200))
(defconst SDRAM-addr-node 9)
(defconst SDRAM-control-node 8)
(defconst SDRAM-data-node 7)
(defconst eForth-Bitsy-node 105)
(defconst eForth-stack-node 106)
(defconst SDRAM-mux-node 107)
(defconst SDRAM-idle-node 108)

(defconst node-rom-type `((300 . ,sync-boot-rom-ht)
                          (708 . ,async-boot-rom-ht)
                          (705 . ,spi-boot-rom-ht)
                          (200 . ,1-wire-rom-ht )
                          (9 . ,SDRAM-addr-rom-ht)
                          (8 . ,SDRAM-control-rom-ht)
                          (7 . ,SDRAM-data-rom-ht)
                          (105 . ,eForth-bitsy-rom-ht)
                          (106 . ,eForth-stack-rom-ht)
                          (107 . ,SDRAM-mux-rom-ht)
                          (108 . ,SDRAM-idle-rom-ht)))

(define (get-node-rom node)
  (let ((nodes false))
    (cond ((member node analog-nodes) analog-rom-ht)
          ((member node serdes-nodes) serdes-boot-rom-ht)
          ((setq nodes (assoc node node-rom-type))
           (cdr nodes))
          (else basic-rom-ht))))

;; This is the type that holds compiled code and other node info.
;; Compiling a program returns a list of these
(struct node (coord mem buffer-map len (symbols #:auto) (word-dict #:auto)
                    (a #:auto) (b #:auto) (io #:auto) (stack #:auto) (p #:auto)
                    (address-cells #:auto) (consts #:auto) (extern-funcs #:auto))
  #:mutable #:transparent)
;; 'coord' is the node coordinate this code belongs to
;; 'mem' is the vector of compiled words
;; 'len' is how many instructions are used in mem. len <= length(mem)
;;       the remaining words in mem are all #f
;; 'symbols' a list of symbol structs

(struct compiled (nodes (error-info #:auto) (node-locations #:auto) ) #:mutable #:transparent)
;; struct to hold compiled code
;; 'nodes': list of 'node' structs
;; 'error-info': struct error-info

(struct symbol (val address line col))

(struct token (tok line col))

(struct bootstream (name start path))

(struct address-cell (val next-addr name (type #:auto)) #:mutable #:transparent)

(defconst bootstream-types '("async" ;; load through node 708 serial
                             "2wire" ;; load through node 300 2wire
                             "async-target" ;; in host node 708 to target node 300
                             ))
(define default-bootstream-type "async")

(define (create-node coord (mem false) (buffer-map false) (len 0) (buffer false))
  (let ((new (node coord mem buffer-map len buffer)))
    (set-node-symbols! new (list))
    (set-node-word-dict! new (make-hash))
    (set-node-address-cells! new (make-set))
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

(define (valid-coord? coord)
  (and (>= coord 0)
       (< (modulo coord 100) 18)
       (< (/ coord 100) 8)))

(define (convert-direction coord dir)
  ;;converts DIR={North, East, South, West} Into Left, Up, Down, or Right
  ;;depending on the nodes coordinate COORD
  (let ((x (remainder coord 100))
        (y (quotient coord 100)))
    (cond
      ((equal? dir "north")
       (if (= (modulo y 2) 0) "down" "up"))
      ((equal? dir "south")
       (if (= (modulo y 2) 0) "up" "down"))
      ((equal? dir "east")
       (if (= (modulo x 2) 0) "right" "left"))
      ((equal? dir "west")
       (if (= (modulo x 2) 0) "left" "right"))
      (else (raise "convert-direction: invalid direction")))))

;;successfully parses a token as a number, or returns false
(define (parse-num tok)
  (define base nil)
  (cond ((number? tok)
         tok)
        ((string? tok)
         (if (and (> (string-length tok) 2)
                  (eq? (string-ref tok 0) _char-0)
                  (or (and (eq? (string-ref tok 1) _char-x)
                           (setq base 16))
                      (and (eq? (string-ref tok 1) _char-b)
                           (setq base 2))))
             (if elisp?
                 (string->number (subseq tok 2) base)
                 ;; convert format 0x... to #x...
                 (begin (set! tok (list->string (cons _char-hash (cdr (string->list tok)))))
                        (string->number tok)))
             (string->number tok)))
        (else (error (rkt-format "parse-num -- invalid arg type ~a" tok)))))

(define (get-address name (node false))
  (cond ((hash-has-key? names->addresses name) ;;normal address names
         (hash-ref names->addresses name))
        ((and node  ;;relative names
              (member name '("north" "south" "east" "west")))
         (cdr (assoc (convert-direction node name) named-addresses)))
        ((and node
              (assoc name named-addresses))
         (cdr (assoc name named-addresses)))
        (else (parse-num name)))) ;; literals and word addresses

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;stack macros

(defmacro swap (stack)
  `(if (> (length ,stack) 1)
       (set! ,stack (cons (cadr ,stack) (cons (car ,stack) (cddr ,stack))))
       (error "swap! requires stack depth of 2")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro enum (syms)
  (let ((i 0)
        (code '()))
    (for ((sym syms))
      (set! code (cons (list 'define sym i) code))
      (set! i (add1 i)))
    (cons 'begin code)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (& a b)
  (bitwise-and a b))
(define (^ a b)
  (bitwise-xor a b))
(define (<< a b)
  (arithmetic-shift a b))
(define (>> x n) (arithmetic-shift x (- n)))
(define (ior a b)
  (bitwise-ior a b))
(define (~ x)
  (bitwise-not x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(enum (LEFT UP DOWN RIGHT))
(defconst port-names (vector "LEFT" "UP" "DOWN" "RIGHT"))

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
                       (else (rkt-format "~a" thing))
                       ))
               ","))

(define (port-addr? addr)
  ;;True if ADDR is an IO port or register, else False
  (> (& addr #x100) 0))

;;Determine if we are in RAM or ROM, return the index for that memory location
;;DB001 section 2.2  Top half of RAM and ROM is repeated
(define (region-index addr)
  (if (port-addr? addr)
      addr
      (begin
        (set! addr (& addr #xff)) ;; get rid of extended arithmetic bit
        (if (>= addr #x80);;ROM
            (if (> addr #xbf)
                (- addr #x40)
                addr)
            (if (> addr #x3F)
                (- addr #x40)
                addr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler options
(define auto-nop-insertion false)

(define compile-0-as-dup-dup-or false)
(define reorder-words-with-fallthrough false)
