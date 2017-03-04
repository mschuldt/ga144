#lang racket ;; -*- lexical-binding: t -*-

(require "common.rkt"
         "compiler/compile.rkt"
         "compiler/assemble.rkt"
         "compiler/bootstream.rkt"
         "compiler/disassemble.rkt"
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
                                                                 (rkt-format "'~a'" w))))
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

(define (print-count input-file) 
  (define compiled (aforth-compile-file input-file))
  (elisp-maybe-print-and-exit compiled)
  (define total 0)
  (define (percent a b)
    (exact->inexact (* (/ (* a 1.0) b) 100)))
  (for ((n (compiled-nodes compiled)))
    (printf "~a  ~a~a ~a%\n"
            (node-coord n)
            (node-len n)
            (if (> (node-len n) 64) "*" " ")
            (percent (node-len n) 64))
    (set! total (+ total (node-len n))))
  (printf "Total: ~a nodes, ~a words, ~a%\n"
          (length (compiled-nodes compiled)) total (percent total (* 64 144))))

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


  (define (pad-print thing (pad 20))
    (let* ((s (rkt-format "~a" thing))
           (len (string-length s))
           (str (string-append s (make-string (- pad len) _char-space))))
      (printf str)))

  (define (make-pretty thing)
    (if (vector? thing)
        (vector->list thing)
        thing))

  (define (make-symbol-hash syms)
    (let ((ht (make-hash)))
      (for ((sym syms))
        (hash-set! ht (symbol-address sym) (symbol-val sym) ))
      ht))

  (define (get-name ht index)
    (if (hash-has-key? ht index)
        (hash-ref ht index)
        false))

  (set! hex? (hex?))
  (define (n val)
    (rkt-format (if hex? "~x" "~a") val))

  (for ((node (compiled-nodes assembled)))
    (define coord (node-coord node))
    (define symbols (make-symbol-hash (node-symbols node)))
    (define comp (hash-ref compiled-hash coord))
    (define asm (node-mem node))
    (define word false)
    (printf "\n\n__________________ node ~a ____________________\n" coord)
    (printf "P = ~a\n" (n (or (node-p node) 0)))
    (printf "     Compiled            Assembled    Disassembled\n")
    (for ((i (node-len node)))
      (set! word (vector-ref comp i))
      (unless (equal? word (vector false false false false))
        (set! name (get-name symbols i))
        (when name (printf "~a:\n" name))
        (printf "~a    " (n i))
        (pad-print (make-pretty word))
        (pad-print (rkt-format "~a" (n (vector-ref asm i))) 13)
        (printf "~a\n" (make-pretty (disassemble-word (vector-ref asm i))))))))
