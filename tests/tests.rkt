#lang racket ;; -*- lexical-binding: t -*-

(require compatibility/defmacro
         "../src/interpreter.rkt"
         "../src/common.rkt"
         "../src/stack.rkt"
         "../src/el.rkt")

(define (18bit n)
  (if (number? n)
      (bitwise-and n #x3ffff)
      n))

(define test-chip (new-ga144))
(define tests '())
(define tests-passed 0)
(define tests-failed 0)

(define-syntax-rule (define-test name program checks ...)
  (set! tests (cons (lambda ()
		      (printf "running test: '~a'\n" name)
                      (let ((tests (list checks ...))
                            (result false)
                            (failed '())
                            (compiled-file
                             (rkt-format "test-out/~a-compiled.txt" name))
                            (assembled-file
                             (rkt-format "test-out/~a-assembled.txt" name)))
                        ;;(reset!);;TODO: fix reset
                        (delete-all-chips)
                        (set! test-chip (new-ga144))
                        (compile-and-load test-chip
                                          program
                                          t
                                          #:compiled-file compiled-file
                                          #:assembled-file assembled-file)
                        ;;(step*)
                        (send test-chip step-program!*);; faster then (step*)
                        (for ((x tests))
                          (set! result (x))
                          (when result
                            (set! failed (cons result failed))))
                        (if (null? failed)
                            (set! tests-passed (add1 tests-passed))
                            (begin (display (rkt-format "FAILED: ~a: '~a'\n"
                                                    name
                                                    program))
                                   (for ((f failed))
                                     (display f))
                                   (newline)
                                   (set! tests-failed (add1 tests-failed))
                                   ))))
                    tests)))

(define (coord->node coord)
  (send test-chip coord->node coord))

(define (check-mem coord . expect)
  (lambda () (let* ((expect (map 18bit expect))
                    (memory (send (coord->node coord) get-memory))
                    (s (length expect)))
               (if (same-subset-v? expect memory)
                   false
                   (rkt-format "    Memory does not match (node ~a)
        Expected: ~a...
        Got:      ~a...\n"
                           coord
                           expect
                           (take (vector->list memory) s))))))

;;NOTE: dependent on node::get-registers
(enum (a b p i r s t io))

(define (check-reg coord reg-num expect)
  (lambda () (let ((val (vector-ref (send (coord->node coord) get-registers)
                                    reg-num))
                   (var (vector-ref (vector "A" "B" "P" "I" "R" "S" "t") reg-num)))
               (if (eq? val (18bit expect))
                   false
                   (rkt-format "    Failed assertion (node ~a):
        Expected: (eq ~a ~a)
        Got:      (eq ~a ~a)\n"
                           coord
                           var expect
                           var val)))))

(define (check-dat coord . expect)
  (lambda () (let* ((m (map 18bit expect))
                    (dstack (send (coord->node coord) get-dstack-as-list))
                    (s (length m)))
               (if (same-subset? m dstack)
                   false
                   (rkt-format "    Data stack does not match (node ~a)
        Expected: ~a...
        Got:      ~a...\n"
                           coord
                           m
                           (take dstack s))))))

(define (check-ret coord . expect)
  (lambda () (let* ((m (map 18bit expect))
                    (rstack (send (coord->node coord) get-rstack-as-list))
                    (s (length m)))
               (if (same-subset? m rstack)
                   false
                   (rkt-format "    Return stack does not match (node ~a)
        Expected: ~a...
        Got:      ~a...\n"
                           coord
                           m
                           (take rstack s))))))

(define (same-subset-v? list vector)
  (define (same? i list)
    (or (null? list)
        (and (eq? (vector-ref vector i)
                  (car list))
             (same? (add1 i) (cdr list)))))
  (same? 0 list))

(define (same-subset? l1 l2)
  (or (or (null? l1)
          (null? l2))
      (and (eq? (car l1) (car l2))
           (same-subset? (cdr l1) (cdr l2)))))

(define default #x15555)

(define-test "basic1"
  "node 1 1 2 + node 717 1 2 3 4 +"
  (check-reg 1 t 3)
  (check-dat 1 3 default)
  ;;(check-mem 1 67813 1 2)

  (check-reg 717 t 7)
  (check-dat 717 7 2 1 default))

(define-test "basic2"
  "node 2 3 3 + 1"
  (check-reg 2 t 1)
  (check-reg 2 s 6)
  (check-dat 2 1 6 default))

(define-test "negate"
  "node 505 3 - 1 + dup 5 +"
  (check-reg 505 t 2)
  (check-reg 505 s -3))

(define-test "over"
  "node 500 1 2 3 over node 2 2 3 over over"
  (check-reg 500 t 2)
  (check-reg 500 s 3)
  (check-dat 500 2 3 2 1 default)
  ;;(check-dat 2 2 3 2 3)
  )

(define-test "if"
  "node 100 1 if 3 + then 1 +
   node 200 0 if 3 + then 1 +
   node 300 1 if 1 if 4 + then 3 + then 1 +
   node 301 0 if 0 if 4 + then 3 + then 1 +
   node 302 1 if 0 if 4 + then 3 + then 2 +
   node 303 0 if 1 if 4 + then 3 + then 3 +"
  (check-reg 100 t 5)
  (check-reg 100 s default)
  (check-reg 200 t 1)
  (check-reg 200 s default)
  (check-reg 300 t 9)
  (check-reg 300 s 1)
  (check-reg 301 t 1)
  (check-reg 301 s default)
  (check-reg 302 t 5)
  (check-reg 302 s 1)
  (check-reg 303 t 3)
  (check-reg 303 s default))

(define-test "-if"
  "node 100 2 -if 3 + then 5 +
   node 200 0 -if 3 + then 5 +
   node 300 2 - 1 +  -if 3 + then 5 +"
  (check-reg 100 t 7)
  (check-reg 100 s default)
  (check-reg 200 t 5)
  (check-reg 200 s default)
  (check-reg 300 t 6)
  (check-reg 300 s default))

(define-test "and"
  "node 1 1 1 and
          0 0 and
          5 6 and
         -4 8 and
   node 5 2 dup dup -1 . + and
          4 dup dup -1 . + and
          7 dup dup -1 . + and"
  (check-dat 1 8 4 0 1 default)
  (check-dat 5 6 7 0 4 0 2 default))

(define-test "push&pop"
  "node 1 2 4 push 3 push 5 pop warm ; ( use 'warm ;' to preserve r stack ) "
  (check-dat 1 3 5 2 default)
  (check-ret 1 4 default))

(define-test "b"
  "node 0 east b! @b @b @b +
   node 1 4 west b! 123 !b 2 !b 3 !b"
  (check-reg 0 t 5)
  (check-reg 0 s 123)
  (check-reg 1 t 4))

(define-test "a"
  "node 0 east a! @ @ @ +
   node 1 4 west a! 123 ! 2 ! 3 !
   node 2 12 a! 2 a"
  (check-reg 0 t 5)
  (check-reg 0 s 123)
  (check-reg 1 t 4)
  (check-reg 2 t 12)
  (check-reg 2 s 2))

(define-test "ludr-ports"
  "node 000 north    a! east b! 1 !b 2 !b 3 !b @ @ @
   node 001 west  a! east b! @ !b @ !b @ !b
   node 002 west  a! north    b! @ !b @ !b @ !b
   node 102 south  a! north    b! @ !b @ !b @ !b
   node 202 south  a! west  b! @ !b @ !b @ !b
   node 201 east a! west  b! @ !b @ !b @ !b
   node 200 east a! south  b! @ !b @ !b @ !b
   node 100 north    a! south  b! @ 1 + !b @ 1 + !b @ 1 + !b "
  (check-dat 0 4 3 2 default)
  (check-dat 1 default default default)
  (check-dat 102 default default default)
  (check-dat 202 default default default)
  (check-dat 201 default default default)
  (check-dat 200 default default default)
  (check-dat 100 default default default))

(define-test "blocking-read-right-port1"
  "node 0 east a! @
   node 1 28 west a! . . . . . . 234 !"
  (check-reg 0 t 234)
  (check-reg 1 t 28))
(define-test "blocking-read-right-port2"
  "node 1 west a! @
   node 0 28 east a! . . . . . . 234 !"
  (check-reg 1 t 234)
  (check-reg 0 t 28))

(define-test "blocking-read-left-port1"
  "node 1 east a! @
   node 2 28 west a! . . . . . . 234 !"
  (check-reg 1 t 234)
  (check-reg 2 t 28))
(define-test "blocking-read-left-port2"
  "node 2 west a! @
   node 1 28 east a! . . . . . . 234 !"
  (check-reg 2 t 234)
  (check-reg 1 t 28))

(define-test "blocking-read-down-port1"
  "node 0 north a! @
   node 100 28 south a! . . . . . . 234 !"
  (check-reg 0 t 234)
  (check-reg 100 t 28))
(define-test "blocking-read-down-port2"
  "node 100 south a! @
   node 0 28 north a! . . . . . . 234 !"
  (check-reg 100 t 234)
  (check-reg 0 t 28))

(define-test "blocking-read-up-port1"
   "node 100 north a! @
   node 200 28 south a! . . . . . . 234 !"
   (check-reg 100 t 234)
   (check-reg 200 t 28))
(define-test "blocking-read-up-port2"
  "node 200 south a! @
   node 100 28 north a! . . . . . . 234 !"
  (check-reg 200 t 234)
  (check-reg 100 t 28))

(define-test "blocking-write-up-port1"
  "node 202 south a! . . . . . . . . . @
   node 102 north a! 234 ! 28"
  (check-reg 202 t 234)
  (check-reg 102 t 28))
(define-test "blocking-write-up-port2"
  "node 102 north a! . . . . . . . . . @
   node 202 south a! 234 ! 28"
  (check-reg 102 t 234)
  (check-reg 202 t 28))

(define-test "blocking-write-down-port1"
  "node 101 south a! . . . . . . . . . @
   node 1 north a! 234 ! 28"
  (check-reg 101 t 234)
  (check-reg 1 t 28))
(define-test "blocking-write-down-port2"
  "node 1 north a! . . . . . . . . . @
   node 101 south a! 234 ! 28"
  (check-reg 1 t 234)
  (check-reg 101 t 28))

(define-test "blocking-write-left-port1"
  "node 2 west a! . . . . . . . . . @
   node 1 east a! 234 ! 28"
  (check-reg 2 t 234)
  (check-reg 1 t 28))
(define-test "blocking-write-left-port2"
  "node 1 east a! . . . . . . . . . @
   node 2 west a! 234 ! 28"
  (check-reg 1 t 234)
  (check-reg 2 t 28))


(define-test "blocking-write-right-port1"
  "node 0 east a! . . . . . . . . . @
   node 1 west a! 234 ! 28"
  (check-reg 0 t 234)
  (check-reg 1 t 28))
(define-test "blocking-write-right-port2"
  "node 1 west a! . . . . . . . . . @
   node 0 east a! 234 ! 28"
  (check-reg 1 t 234)
  (check-reg 0 t 28))

(define-test "shift"
  "node 4 4 2/ 2 2* 8 2* 1 2* 2* 2/ 2/ 0 2/ 0 2*
   node 3 4 dup 2* . +  ( multiply by 3 )
   node 5 8 dup 2* 2* . +  ( multiply by 5 )
   node 6 3 2* dup 2* . +  ( multiply by 6 )
   node 7 3 dup 2* dup 2* . + . + ( multiply by 7)"
  (check-dat 4 0 0 1 16 4 2)
  (check-reg 3 t 12)
  (check-reg 5 t 40)
  (check-reg 6 t 18)
  (check-reg 7 t 21))

(define-test "or"
  "node 7   23 45 or
   node 707 1 2 over push over or or pop ( swap)"
  (check-reg 707 t 1)
  (check-reg 707 s 2)
  (check-reg 7 t 58))

(define-test "drop"
  "node 1 2 4 drop 3
   node 2 1 2 3 push drop pop ( nip)"
  (check-dat 1 3 2 default)
  (check-dat 2 3 1 default))

(define-test "a-fetch-inc"
  "node 1 5 a! @+ @+ @+ warm 2 3 4 5 10"
  (check-dat 1 4 3 2 default))

(define-test "a-store-inc"
  "node 1 1 1 1 10 20 30 0 a! !+ !+ !+"
  (check-mem 1 30 20 10 1))

(define-test "call+ret"
  "node 1 0 if
            : double dup + ;
            : x4 double double ;
          then
          2 double
          4 x4"
  (check-reg 1 t 16)
  (check-reg 1 s 4))

(define-test "computed-goto"
  "node 1 0 if
              : case pop + push ;
              : add10 10 + ;
              : add20 20 + ;
              : add30 30 + ;
              : 10x case add10 ; add20 ; add30 ;
            then
           1 1 10x
           5 0 10x
           2 2 10x"
  (check-dat 1 32 15 21))

(define-test "max_min"
  "node 1 0 if
           : min - over . + - -if + ; then drop ;
           : max - over . + - -if drop ; then + ;
 (         : min2 - over . + - -if begin + ;      )
 (         : max2 - over . + - -until then drop ; )
         then
      1 2 max
      1 2 min
      5 6 max
      5 6 min
      1 1 max
 (    5 6 max2 )
 (    5 6 min2 )
"
  (check-dat 1 1 5 6 1 2 0))

(define-test "variables"
  "node 2
   0 if
    : x! @p drop !p ;
    : x 0 ;
   then
    8 x! x push 5 x! pop x"
  (check-reg 2 t 5)
  (check-reg 2 s 8))

(define-test "unext"
  "node 1
    1 2 for 2* unext
    1 3 for 2* unext
    1 4 for 2* unext
      4 for 2/ unext
    "
  (check-dat 1 1 16 8 default))

(define-test ","
  "node 1
    @p @p @p
    , 1 , 22 , 333  "
  (check-dat 1 333 22 1 default default))

(define-test "fib"
  "node 1
0 if
 : fib dup -2 + -if 1 ; then drop push 1 dup pop -3 + for over over + next ;
then
3 fib
4 fib
5 fib
6 fib
"
  (check-dat 1 8 5 3 2 ))

(define-test "comma"
  "node 0 5 @p .. , 1 +"
  (check-dat 0 6))

(define-test "port-execution1"
  "node 1
east b!
.. @p !b ..
dup + dup .

node 2
3
west push ;
"
  (check-dat 1 default)
  (check-dat 2 6 6))

(define-test "multiport-execution"
  "node 101
east b!
.. @p !b ..
dup + dup .
node 102
3 rdlu ;"
  (check-dat 101 default)
  (check-dat 102 6 6))

(define-test "port-execution2"
  "node 1
0 if
: set @p ! ! ; .. @p . a! ..
: !next @p ! ! ; .. @p . !+ ..
then
east a!
0 set
9 !next
3 !next
5 !next
"
  (check-dat 1 0)
  (check-mem 2 9 3 5))

;; ;;http://www.greenarraychips.com/home/documents/greg/AB004-141021-PORTEX.pdf
(define-test "port-execution3" ;;TODO: fix errors when node = 1
  "node 2
0 if
: set @p ! ! ; .. @p a! ..
: @next @p ! @ ; .. @+ !p ..
: !next @p ! ! ; .. @p !+ ..
: fetch set @next ;
: store set !next ;
then
east a!
..
@p ! .. ( focusing call)
 r---
 0 set
 6 !next 3 !next
  0 set
 @next dup @next
"
  (check-dat 2 3 6 6)
  (check-mem 3 6 3))

(define-test "word-references"
  "node 400
0 if
: A + ;
: B dup ;
: C 2/ ;
then
&A &B &C
"
  (check-dat 400 4 3 2 0 default))

(define-test "-until"
  "node 509
0 10
begin
-1 + over 2 + over
-until
"
  (check-dat 509 -1 22))

(define-test "hex+bin"
  "node 100
0x1 0x2 +
0x1 0x0 +
0x12abd 0xd +
0b1010 0b101 +
"
  (check-dat 100 15 76490 1 3 default))


(define-test "--u-slash-mod"
  "node 500
0 200 10 - 1 . + --u/mod
node 1
0 5 3 - 1 . + --u/mod"
  (check-dat 500 20 0 default)
  (check-dat 1 1 2 default))

(define-test "const"
  "node 600
const x 3
const xx or 5 1
const xxx + 4 5
x xx xxx
x xx +"
  (check-dat 600 7 9 4 3 default))

(define-test "remote-word-call"
  "node 2
.. ;
.. ;
: fn2  10 + dup !p ;
: fn  5 + dup ;
: main west push ;

node 1
east b!

.. @p !b .. @p ..  6 !b ( send arg )
.. @p !b .. fn@2 .. ( send call)
.. @p !b .. !p .. @b ( read result )"

  (check-dat 1 11 default)
  (check-dat 2 11 default)
  )

(define-test "wrap-around"
  "node 100
org 62
: main 1 2 3 4 + + +
"
  (check-dat 100 10 default)
  (check-mem 100 2 3 4)
  )

(define (run-tests)
  (set! tests-failed 0)
  (set! tests-passed 0)
  (for ((test tests))
  ;;(for ((test (list (car tests))))
    (test))
  (display (rkt-format "passed: ~a\n" tests-passed))
  (display (rkt-format "failed: ~a\n" tests-failed)))

(unless (directory-exists? "test-out")
  (make-directory "test-out"))

(run-tests)
