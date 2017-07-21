#lang racket ;; -*- lexical-binding: t -*-

(require compatibility/defmacro
         "../src/common.rkt"
         "../src/compile.rkt"
         "../src/assemble.rkt"
         "../src/el.rkt")

(define compiler-tests
  '(("node 1 " (1 ))
    ("node 1 2" (1 ("@p" "." "." ".")
                   2))
    ("node 1 22 +" (1 ("@p" "+" "." ".")
                      22))
    ("node 1 @ @ @ @ @ @" (1 ("@" "@" "@" ".")
                             ("@" "@" "@" ".")))
    ("node 1 dup dup dup dup @p @p @p @p" (1 ("dup" "dup" "dup" "dup")
                                             ("@p" "@p" "@p" "@p")))
    ("node 1 0 if dup dup then over over" (1 ("@p" "if" 3 ".")
                                             0
                                             ("dup" "dup" "." ".")
                                             ("over" "over" "." ".")))
    ("node 1 up down left right" (1 ("@p" "@p" "@p" "@p") 325 277 373 469))
    ("node 2 up down left right" (2 ("@p" "@p" "@p" "@p") 325 277 373 469))
    ("node 101 up down left right" (101 ("@p" "@p" "@p" "@p") 325 277 373 469))
    ("node 1 north east south west" (1 ("@p" "@p" "@p" "@p")
                                       277 373 325 469))
    ("node 2 north east south west" (2 ("@p" "@p" "@p" "@p")
                                       277 469 325 373))
    ("node 101 north east south west" (101 ("@p" "@p" "@p" "@p")
                                           325 373 277 469))
    ("node 102 north east south west" (102 ("@p" "@p" "@p" "@p")
                                           325 469 277 373))
    ("node 1 : word dup ; : word2 + + + ; word word2" (1 ("dup" ";" "." ".")
                                                         ("+" "+" "+" ";")
                                                         ("call" 0 "." ".")
                                                         ("call" 1 "." ".")))
    ("node 1 ---u --l- --lu -d--" (1 ("call" 325 "." ".")
                                     ("call" 373 "." ".")
                                     ("call" 357 "." ".")
                                     ("call" 277 "." ".")))
    ("node 1 100 0x100 0b101" (1 ("@p" "@p" "@p" ".") 100 256 5))
    ("node 1 + ( comment ) (comment) + \\comment" (1 ("+" "+" "." ".")))
    ("node 1 + .. + .  + .. ." (1 ("+" "." "." ".")
                                  ("+" "." "+" ".")
                                  ("." "." "." ".")))
    ("node 1 1 , 2 3" (1 ("@p" "@p" "." ".")
                         1 2 3))
    ("node 1 0 2 + + here 5 next" (1 ("@p" "@p" "+" "+")
                                     0 2
                                     ("@p" "next" 3 ".") 5))
    ("node 1 3 for 2/ next + + 3 for dup dup dup dup dup next" (1 ("@p" "push" "." ".")
                                                                  3
                                                                  ("2/" "next" 2 ".")
                                                                  ("+" "+" "@p" ".")
                                                                  3
                                                                  ("push" "." "." ".")
                                                                  ("dup" "dup" "dup" "dup")
                                                                  ("dup" "next" 6 ".")))
    ("node 1 3 for 2* unext" (1 ("@p" "push" "." ".")
                                3
                                ("2*" "unext" "." ".")))
    ("node 1 0 if dup then +" (1 ("@p" "if" 3 ".")
                                 0
                                 ("dup" "." "." ".")
                                 ("+" "." "." ".")))
    ("node 1 0 -if dup then +" (1 ("@p" "-if" 3 ".")
                                  0
                                  ("dup" "." "." ".")
                                  ("+" "." "." ".")))
    ("node 1 :: aaa dup 1 + + lit ; :: bbb 5 aaa ; + bbb +" (1 ("+" "@p" "+" ".")
                                                               11))
    ("node 1 :: five  5 lit ; five five" (1 ("@p" "@p" "." ".")
                                            5 5))
    ("node 1 warm node 5 warm node 104 warm ; "
     (1 ("call" 169 "." "."))
     (5 ("call" 169 "." "."))
     (104 ("jump" 169 "." ".")))
    ("node 0 +" (0 ( "+" "." "." ".")))
    ("node 1 org 0 +" (1 ( "+" "." "." ".")))
    ("node 1 0 " (1 ( "@p" "." "." ".")
                    0))
    ("node 1 :: zero 0 lit ; zero " (1 ( "@p" "." "." ".")
                                       0))
    ("node 1 : min - over . + - -if + ; then drop ; : max - over . + - -if drop ; then + ;"
     (1 ("-" "over" "." "+")
        ("-" "-if" 3 ".")
        ("+" ";" "." ".")
        ("drop" ";" "." ".")
        ("-" "over" "." "+")
        ("-" "-if" 7 ".")
        ("drop" ";" "." ".")
        ("+" ";" "." ".")))

    ("node 1 : A + ; : B dup ; : C 2/ ; &A &B &C" (1 ("+" ";" "." ".")
                                                     ("dup" ";" "." ".")
                                                     ("2/" ";" "." ".")
                                                     ("@p" "@p" "@p" ".")
                                                     0 1 2))
    ;;("node 1 : A + ; B@100 node 100 B over ; A@1" (1 ) (100 ))
    ("node 1 dup : A + ; node 100 : B over ; A@1"
     (1 ("dup" "." "." ".")
        ("+" ";" "." "."))
     (100 ("over" ";" "." ".")
          ("call" 1 "." ".")))
    ("node 1 dup : A + ; node 100 : B over ; &A@1"
     (1 ("dup" "." "." ".")
        ("+" ";" "." "."))
     (100 ("over" ";" "." ".")
          ("@p" "." "." ".")
          1))

    ;; test instruction shifting
    ("node 500 . . if .. . : word1 1 ; word then : word word1 ;"
     (500 ("." "." "if" 5)
          ("." "." "." ".")
          ("@p" ";" "." ".")
          1
          ("call" 5 "." ".")
          ("jump" 2 "." ".")))
    ("node 500 . . if .. . .. . .. . .. . : word1 1 ; word then : word word1 ;"
     (500 ("." "." "." ".")
          ("if" 9 "." ".")
          ("." "." "." ".")
          ("." "." "." ".")
          ("." "." "." ".")
          ("." "." "." ".")
          ("@p" ";" "." ".")
          1
          ("call" 9 "." ".")
          ("jump" 6 "." ".")))

    ;; test if: and next:
    ("node 0 . : word1 ; if: word2 if: word1 : word2 ;"
     (0 ("." "." "." ".")
        (";" "." "." ".")
        ("if" 4 "." ".")
        ("if" 1 "." ".")
        (";" "." "." ".")))
    ("node 1 . : word1 ; next: word2 next: word1 : word2 ;"
     (1 ("." "." "." ".")
        (";" "." "." ".")
        ("next" 4 "." ".")
        ("next" 1 "." ".")
        (";" "." "." ".")))

    ("node 1 dup + + + include __test.aforth over "
     (1 ("dup" "+" "+" "+")
        ("@p" "." "." ".")
        ("+" ";" "." ".")
        ("jump" 2 "." ".")
        ("-" "over" "." ".")))


    ;; test double shifting
    ("node 205
: main

: update
    2*
    0x20003 or drop
     .. @p !
     ..  @+ !p unext ..
     .. 2/ push
     begin @ push @ pop next ..
     @p !
     ..  @p a! ; ..

    pop ! done

.. . ..

: focus
   .. . ..
   .. . ..
   .. . ..
   .. @p @p @p @p ..

     . + last
     ;
: done
    @p !
    .. ; ..
    ;
..  .
..  .
..  .

: last
."
     (205
      ("2*" "@p" "or" ".")
      131075
      ("drop" "." "." ".")
      ("@p" "!" "." ".")
      ("@+" "!p" "unext" ".")
      ("2/" "push" "." ".")
      ("@" "push" "@" ".")
      ("pop" "next" 6 ".")
      ("@p" "!" "." ".")
      ("@p" "a!" ";" ".")
      ("pop" "!" "." ".")
      ("call" 19 "." ".")
      ("." "." "." ".")
      ("." "." "." ".")
      ("." "." "." ".")
      ("." "." "." ".")
      ("@p" "@p" "@p" "@p")
      ("." "+" "." ".")
      ("jump" 25 "." ".")
      ("@p" "!" "." ".")
      (";" "." "." ".")
      (";" "." "." ".")
      ("." "." "." ".")
      ("." "." "." ".")
      ("." "." "." ".")
      ("." "." "." ".")))
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
    (printf "testing: ~a\n" (replace-regexp-in-string "\n" " " code))
    (assert (string? code))
    (set! compiled (aforth-compile code))
    (set! compiled-hash (make-hash))
    (if (not (compiled-nodes compiled))
        (begin (if elisp?
                   (aforth-print-error-data compiled)
                   (printf "compilation failure"))
               (set! ok false))
        (begin
          (for ((node (compiled-nodes compiled)))
            (hash-set! compiled-hash (node-coord node)
                       (trim-mem (vector-copy (node-mem node)))))
          (for ((x (cdr test)))
            (set! node (car x))
            (set! expect (cdr x))
            (if (hash-has-key? compiled-hash node)
                (set! mem (hash-ref compiled-hash node))
                (error (rkt-format "Expected node '~a' results from test code \"~a\"\n" node code)))
            (when (not (equal? mem expect))
              (printf "failed: '~a'\n" code)
              (printf "     got: ~a\n" mem)
              (printf "expected: ~a\n\n" expect)
              (set! ok false))))))
  ;;(printf "ran ~a tests\n" (length compiler-tests))
  ok)

(unless elisp?
  (printf "running compiler checks...\n")
  (printf "~a\n" (if (run-compiler-tests) "ok" "failed")))
