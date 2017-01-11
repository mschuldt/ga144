#lang racket ;; -*- lexical-binding: t -*-

(require compatibility/defmacro
         "rom.rkt"
         "rom-dump.rkt"
         "rkt-to-el.rkt")


(provide (all-defined-out))

(el-require 'cl)

(defvar num-words 100);;TODO: compile arbitrarily large programs per node but warn if > 64 words
(defvar num-nodes 144)

(defvar opcodes (vector ";" "ex" "jump" "call" "unext" "next" "if"
                        "-if" "@p" "@+" "@b" "@" "!p" "!+" "!b" "!" "+*"
                        "2*" "2/" "-" "+" "and" "or" "drop" "dup" "pop"
                        "over" "a" "." "push" "b!" "a!"))

(defvar opcode-set (list->set (vector->list opcodes)))

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

(defvar addresses->names (make-hash (for/list ((x named-addresses))
                                      (cons (cdr x) (car x)))))
(defvar names->addresses (make-hash named-addresses))

(define (port-name address)
  (hash-ref addresses->names address))

(defvar io-places '(("---u" . #x145)
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

(defvar node-to-gpio-pins '((701 . 2)
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

(defvar rom-ht (make-hash ROM-DUMP))

(defvar basic-rom-ht (make-hash basic-rom))
(defvar analog-rom-ht (make-hash analog-rom))
(defvar serdes-boot-rom-ht (make-hash serdes-boot-rom))
(defvar sync-boot-rom-ht (make-hash sync-boot-rom))
(defvar async-boot-rom-ht (make-hash async-boot-rom))
(defvar spi-boot-rom-ht (make-hash spi-boot-rom))
(defvar 1-wire-rom-ht (make-hash 1-wire-rom))

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

(struct compiled (nodes) #:mutable #:transparent)
;; struct to hold compiled code
;; 'nodes': list of 'node' structs

(struct symbol (name address line col))

(struct token (tok line col))

(struct bootstream (name start path))

(defconst bootstream-types '("async" ;; load through node 708 serial
                           "2wire" ;; load through node 300 2wire
                           "async-target" ;; in host node 708 to target node 300
                           ))
(defvar default-bootstream-type "async")

(define (create-node coord [mem false] [len 0])
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
  (when (and (> (string-length tok) 2)
             (eq? (string-ref tok 0) #\0)
             (or (eq? (string-ref tok 1) #\x)
                 (eq? (string-ref tok 1) #\b)))
    ;; convert format 0x... to #x...
    (set! tok (list->string (cons #\# (cdr (string->list tok))))))
  (string->number tok))

(define (get-address name [node false])
  (cond ((hash-has-key? names->addresses name) ;;normal address names
         (hash-ref names->addresses name))
        ((and node  ;;relative names
              (member name '("north" "south" "east" "west")))
         (convert-direction node name))
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

(define & bitwise-and)
(define ^ bitwise-xor)
(define << arithmetic-shift)
(define (>> x n) (arithmetic-shift x (- n)))
(define ior bitwise-ior)
(define ~ bitwise-not)


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler options
(defvar auto-nop-insertion false)

(defvar compile-0-as-dup-dup-or false)
(defvar reorder-words-with-fallthrough false)
