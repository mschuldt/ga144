#lang racket ;; -*- lexical-binding: t -*-

(require compatibility/defmacro
         "../common.rkt"
         "../compiler/compile.rkt"
         "../compiler/assemble.rkt"
         "../el.rkt")

(define compiler-tests
  '(("node 1 " (1 ))
    ("node 1 2" (1 ("@p" "." "." ".") 2))
    ("node 1 22 +" (1 ("@p" "+" "." ".") 22))
    ("node 1 @ @ @ @ @ @" (1 ("@" "@" "@" ".") ("@" "@" "@" ".")))
    ;;("node 1 " (1 ))
    ))

(define (fix-word word)
  (when  (vector? word)
    (set! word (vector->list word)))

  (if (list? word)
      (map (lambda (x) (if (equal? x false) "." x))
           word)
      word))

(define (trim-mem mem)
  (map fix-word
       (filter (lambda (x) (not (or (equal? x (vector false false false false))
                                    (equal? x false))))
               (vector->list mem))))


(define (run-compiler-tests)
  (define code false)
  (define compiled false)
  (define compiled-hash false)
  (define node false)
  (define expect false)
  (define mem false)
  (define ok true)
  (for ((test compiler-tests))
    (set! code (car test))
    (assert (string? code))
    (set! compiled (aforth-compile code))
    (set! compiled-hash (make-hash))
    (for ((node (compiled-nodes compiled)))
      (hash-set! compiled-hash (node-coord node)
                 (trim-mem (vector-copy (node-mem node)))))
    (for ((x (cdr test)))
      (set! node (car x))
      (set! expect (cdr x))
      (set! mem (hash-ref compiled-hash node))
      (when (not (equal? mem expect))
        (printf "failed: '~a'\n" code)
        (printf "     got: ~a\n" mem)
        (printf "expected: ~a\n" expect)
        (set! ok false))))
  ok)

(run-compiler-tests)
