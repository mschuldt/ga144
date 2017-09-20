#lang racket ;; -*- lexical-binding: t -*-

(require "common.rkt"
         "compile.rkt"
         "assemble.rkt"
         "bootstream.rkt"
         "disassemble.rkt"
         "el.rkt")

(provide (all-defined-out))

(define (compiled->json compiled)
  (comma-join
   (for/list ((node (compiled-nodes compiled)))
     (rkt-format "'~a' : [~a]"
                 (node-coord node)
                 (comma-join (let ((mem (node-mem node)))
                               (for/list ((i (range (node-len node))))
                                 (let ((word (vector-ref mem i)))
                                   (if (vector? word)
                                       (rkt-format "[~a]"
                                                   (comma-join (for/list ((w word))
                                                                 (rkt-format "'~a'" (or  w "~")))))
                                       word)))))))))

(define (boot-descriptors->json compiled)
  (comma-join
   (for/list ((node (compiled-nodes compiled)))
     (rkt-format " '~a' : {~a}"
                 (node-coord node)
                 (comma-join (list (rkt-format "'a' : ~a" (or (node-a node) "None"))
                                   (rkt-format "'b' : ~a" (or (node-b node) "None"))
                                   (rkt-format "'io' : ~a"(or (node-io node) "None"))
                                   (rkt-format "'p' : ~a" (or (node-p node) "None"))
                                   (rkt-format "\n'stack' : ~a \n"
                                               (if (node-stack node)
                                                   (rkt-format "[~a]"
                                                               (comma-join (node-stack node)))
                                                   "None"))))))))
(define (symbols->json compiled)
  (let ((syms '())
        (symbols false))
    (for/list ((node (compiled-nodes compiled)))
      (set! symbols (node-symbols node))
      (unless (null? symbols)
        (push (rkt-format "'~a' : {~a}"
                          (node-coord node)
                          (comma-join
                           (for/list ((sym (node-symbols node)))
                             (rkt-format "'~a' : {'address' : ~a, 'line' : ~a, 'col' : ~a}"
                                         (symbol-val sym) (symbol-address sym)
                                         (symbol-line sym) (symbol-col sym)))))
              syms)))
    (comma-join syms)))

(define (assembled->json assembled)
  (comma-join (for/list ((node (compiled-nodes assembled)))
                (rkt-format "'~a' : [~a]"
                            (node-coord node)
                            (comma-join (let ((mem (node-mem node)))
                                          (for/list ((i (range (node-len node))))
                                            (vector-ref mem i))))))))

(define (elisp-maybe-print-and-exit compiled)
  ;; temporary method of dealing with the error types returned from the elisp version
  (when (and elisp?
             (compiled-error-info compiled))
    (aforth-print-error-data compiled)
    (exit)))

(define (print-json input-file (bootstream-type false) (symbols? false))
  (define compiled (aforth-compile-file input-file))
  (elisp-maybe-print-and-exit compiled)
  (define compiled-json (compiled->json compiled))
  (define boot-descriptors-json (boot-descriptors->json compiled))
  (define symbols-json (symbols->json compiled))
  (define assembled (assemble compiled))
  (define assembled-json (assembled->json assembled))

  (define bootstream (sget-convert (make-bootstream assembled (or (bootstream-type) default-bootstream-type))))

  (define x (list (rkt-format "'file' : '~a'\n" input-file)
                  (rkt-format "'compiled': {~a}\n" compiled-json)
                  (rkt-format "'boot-descriptors' : {~a}\n" boot-descriptors-json)
                  (rkt-format "'assembled': {~a}\n" assembled-json)))

  (when (symbols?)
    (set! x (append x (list (rkt-format "'symbols': {~a}\n" symbols-json)))))

  (when (bootstream-type)
    (set! x (append x (list (rkt-format "'bootstream' : [~a] "
                                        (comma-join bootstream))))))

  (printf "{~a}\n" (comma-join x)))

(define (ga-n val (hex? false) (pre false))
  (let ((n (abs val))
        (s (if (< val 0) "-" "")))
  (rkt-format (if hex? "~a~a~x" "~a~a~a") s (if (and hex? pre) "0x" "") n)))

(define (print-count input-file)
  (define compiled (aforth-compile-file input-file))
  (elisp-maybe-print-and-exit compiled)
  (define total 0)
  (define (percent a b)
    (exact->inexact (* (/ (* a 1.0) b) 100)))
  (for ((n (compiled-nodes compiled) hex?))
    (printf "~a  ~a~a ~a%\n"
            (node-coord n)
            (node-len n)
            (if (> (node-len n) 64) "*" " ")
            (percent (node-len n) 64))
    (set! total (+ total (node-len n))))
  (printf "Total: ~a nodes, ~a words, ~a%\n"
          (length (compiled-nodes compiled)) total (percent total (* 64 144))))

(define (ga-make-symbol-hash syms)
    (let ((ht (make-hash)))
      (for ((sym syms))
        (hash-set! ht (symbol-address sym) (symbol-val sym) ))
      ht))

(define (ga-print-get-name ht index)
    (if (hash-has-key? ht index)
        (hash-ref ht index)
        false))

(define (print-pretty input-file (hex? false))
  (define compiled (aforth-compile-file input-file))
  (elisp-maybe-print-and-exit compiled)
  (define compiled-hash (make-hash))
  (for ((node (compiled-nodes compiled)))
    (hash-set! compiled-hash (node-coord node)
               (vector-copy (node-mem node))))


  (define assembled (assemble compiled))

  (define i 0)
  (define name false)

  (define hex? (hex?))
  (define (pad-print thing (pad 20))
    (let* ((s (rkt-format "~a" thing))
           (len (string-length s))
           (str (string-append s (make-string (- pad len) _char-space))))
      (printf str)))

  (define (make-pretty thing)
    (if (vector? thing)
        (vector->list thing)
        thing))

  (for ((node (compiled-nodes assembled)))
    (define coord (node-coord node))
    (define symbols (ga-make-symbol-hash (node-symbols node)))
    (define comp (hash-ref compiled-hash coord)) ;;why not access directly???
    (define asm (node-mem node))
    (define word false)
    (printf "\n\n__________________ node ~a ____________________\n" coord)
    (printf "P = ~a\n" (ga-n (or (node-p node) 0) hex?))
    (printf "     Compiled            Assembled    Disassembled\n")
    (for ((i (node-len node)))
      (set! word (vector-ref comp i))
      (unless (equal? word (vector false false false false))
        (set! name (ga-print-get-name symbols i))
        (when name (printf "~a:\n" name))
        (printf "~a    " (ga-n i hex?))
        (pad-print (make-pretty word))
        (pad-print (rkt-format "~a" (ga-n (vector-ref asm i) hex?)) 13)
        (printf "~a\n" (make-pretty (disassemble-word (vector-ref asm i))))))))


(define ga-transfer-insts '("jump" "call" "next" "if" "-if"))

(define (ga-get-transfer-addr word)
  ;; return the destination address if word contains a transfer instruction, nil if none
  (when (vector? word)
    (let ((addr false)
          (instr false)
          (i 0))
      (while (< i (length word))
        (set! instr (vector-ref word i))
        (if (member instr ga-transfer-insts)
            (begin (set! addr (vector-ref word (1+ i)))
                   (set! i 5))
            (set! i (add1 i))))
      addr)))

(define (ga-collect-transfer-addrs mem len)
  (let ((a false)
        (h (make-hash)))
    (for ((i len))
      (set! a (ga-get-transfer-addr (vector-ref mem i)))
      (when a
        (if (hash-has-key? address-names a)
            (hash-set! h a (hash-ref address-names a))
            (hash-set! h a (format "_%s" i)))
        ))
    h))

(define (print-bowman-format input-file (hex? false) (full true) (aforth-sim false))
  (when full (printf "include(ga144.hdr)\n"))
  (define compiled (aforth-compile-file input-file))
  (elisp-maybe-print-and-exit compiled)
  (define (make-pretty thing)
    (if (vector? thing)
        (vector->list thing)
        thing))
  (define hex? (hex?))
  (define (get-addr* name)
    (or (get-address name)
        (cdr (assoc name io-places))))

  (define (print-boot-descriptor word val)
    (when val
      (printf (format "   %s\n   %s\n" word val))))

  (for ((node (compiled-nodes compiled)))
    (define coord (node-coord node))
    (define symbols (ga-make-symbol-hash (node-symbols node)))

    (define mem (node-mem node))
    (define len (node-len node))
    (define word false)
    (printf (format "\n\n---------------------------- %03d ----------------------------\n"
                    coord))
    (when (and full (not aforth-sim))
      (print-boot-descriptor "@p a!" (node-a node))
      (when (node-io node)
        (printf "   @p @p b!\n")
        (printf "   ~a\n" (node-io node))
        (printf "   0x15d\n") ;; io
        (printf "   !b\n"))
      (print-boot-descriptor "@p b!" (node-b node))
      (when (node-stack node)
        (for ((val (node-stack node)))
          (printf "   @p\n") ;:TODO: load multiple per word
          (printf "   ~a\n" val)))

      ;;initial jump currently defaults to 0 so don't insert an extra jump
      ;; => ga144tools version always inserts jump, GA144-watch requires jump
      (printf "   jump ~a\n" (or (node-p node) 0))

      (printf ": __start\n"))
    (when aforth-sim
      (when (and (node-p node)
                 (not (member "main" (hash-values symbols))))
        (printf "/p ~a\n"  (node-p node)))
      (when (node-io node)
        (printf "/io ~a\n"  (node-io node)))
      (when (node-b node)
        (printf "/b ~a\n"  (node-b node)))
      (when (node-a node)
        (printf "/a ~a\n"  (node-a node)))
      (when (node-stack node)
        (error "printing /stack is unimplemented")))

    (define addr-names (ga-collect-transfer-addrs mem len))
    (define ok false)
    (define a false)

    (define (get-name i)
      (or (ga-print-get-name symbols i)
          (hash-ref addr-names i)))

    (for ((i len))
      (set! word (vector-ref mem i))
      (if (or (not word)
              (equal? word (vector false false false false)))
          (printf ".\n");;correct?
          (begin
            (set! name (get-name i))
            (when name
              (printf ": ~a\n" name))

            (printf "    ")
            (set! ok true)
            (set! comment false)
            (for ((instr (if (number? word)
                             (list (ga-n word hex? true))
                             word))
                  (i 4))
              (when (and instr ok)
                (printf "~a " instr)
                (when (member instr ga-transfer-insts)
                  (set! a (vector-ref word (1+ i)))
                  (set! name (get-name a))
                  (if name
                      ;; convert names like ---l to their address
                      (begin (set! a (get-addr* name))
                             (set! comment (and a (format "  \\ %s" name)))
                             (set! name (or a name)))
                      (set! name a))
                  (printf "~a" name)
                  (when comment (printf comment))
                  (set! ok false))))
            (printf "\n")
            )))))
