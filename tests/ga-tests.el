;; -*- lexical-binding: t -*-

(require 'ga144-sim)

(ga144-clear-all)

(defun 18bit (n)
  (cond ((number? n)
         (bitwise-and n #x3ffff))
        ((eq n 'default)
         default)
        (t n)))

(setq tests nil)
(setq test-chip false)

(defun define-test (name program &rest checks)
  (push (lambda () (_run-test name program checks)) tests)
  )

(defun define-test-fn (name fn)
  (push (lambda ()
          (ga144-clear-all)
          (message "running test: '%s'" name)
          (funcall fn))
        tests))

(defun _run-test (name program checks)
  (let ((result false)
        (failed '())
        ;;(compiled-file (rkt-format "test-out/~a-compiled.txt" name))
        ;;(assembled-file (rkt-format "test-out/~a-assembled.txt" name))
        assembled
        )
    (printf "running test: '~a'\n" name)
    ;;(reset!);;TODO: fix reset
    ;;(delete-all-chips)
    (set! test-chip (make-ga144 "testchip" nil))
    (set! assembled (assemble (aforth-compile program)))
    (send test-chip load assembled)
    (send test-chip step-program!*)

    (dolist (check checks)
      (set! result (apply (car check) (cdr check)))
      (when result
        (set! failed (cons result failed))))
    (when (not (null? failed))
      (printf "FAILED: ~a: '~a'\n" name program
              (dolist (f failed)
                (printf "~a\n" f))
              (newline)
              ))
    (null failed)))


(define (coord->node coord)
  (send test-chip coord->node coord))

(defun check-mem (coord &rest expect)
  (let* ((expect (map 18bit expect))
         (memory (send (coord->node coord) get-memory))
         (s (length expect)))
    (if (same-subset-v? expect memory)
        false
      (rkt-format "    Memory does not match (node ~a)
        Expected: ~a...
        Got:      ~a...\n"
                  coord
                  expect
                  (take (vector->list memory) s)))))
(setq check-mem 'check-mem)

;;note: dependent on node::get-registers
(setq registers_ (for/list ((reg '(A B P I R S T IO))
                            (i (range 10)))
                           (cons reg i)))


(defun check-reg (coord reg expect)
  (set! reg (cdr (assoc reg registers_)))
  (let ((val (vector-ref (send (coord->node coord) get-registers)
                         reg))
        (var (vector-ref (vector "a" "b" "p" "i" "r" "s" "t") reg)))
    (if (eq? val (18bit expect))
        false
      (rkt-format "    failed assertion (node ~a):
        expected: (eq ~a ~a)
        got:      (eq ~a ~a)\n"
                  coord
                  var expect
                  var val))))

(defun check-dat (coord &rest expect)
  (let* ((m (map 18bit expect))
         (dstack (send (coord->node coord) get-dstack-as-list))
         (s (length m)))
    (if (same-subset? m dstack)
        false
      (rkt-format "    data stack does not match (node ~a)
        expected: ~a...
        got:      ~a...\n"
                  coord
                  m
                  (take dstack s)))))

(defun check-ret (coord &rest expect)
  (let* ((m (map 18bit expect))
         (rstack (send (coord->node coord) get-rstack-as-list))
         (s (length m)))
    (if (same-subset? m rstack)
        false
      (rkt-format "    return stack does not match (node ~a)
        expected: ~a...
        got:      ~a...\n"
                  coord
                  m
                  (take rstack s)))))

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
  '(check-reg 1 T 3)
  '(check-dat 1 3 default)
  ;;(check-mem 1 67813 1 2)

  '(check-reg 717 T 7)
  '(check-dat 717 7 2 1 default))

(define-test "basic2"
  "node 2 3 3 + 1"
  '(check-reg 2 T 1)
  '(check-reg 2 S 6)
  '(check-dat 2 1 6 default))

(define-test "negate"
  "node 505 3 - 1 + dup 5 +"
  '(check-reg 505 T 2)
  '(check-reg 505 S -3))

(define-test "over"
  "node 500 1 2 3 over node 2 2 3 over over"
  '(check-reg 500 T 2)
  '(check-reg 500 S 3)
  '(check-dat 500 2 3 2 1 default)
  ;;(check-dat 2 2 3 2 3)
  )

(define-test "if"
  "node 100 1 if 3 + then 1 +
   node 200 0 if 3 + then 1 +
   node 300 1 if 1 if 4 + then 3 + then 1 +
   node 301 0 if 0 if 4 + then 3 + then 1 +
   node 302 1 if 0 if 4 + then 3 + then 2 +
   node 303 0 if 1 if 4 + then 3 + then 3 +"
  '(check-reg 100 T 5)
  '(check-reg 100 S default)
  '(check-reg 200 T 1)
  '(check-reg 200 S default)
  '(check-reg 300 T 9)
  '(check-reg 300 S 1)
  '(check-reg 301 T 1)
  '(check-reg 301 S default)
  '(check-reg 302 T 5)
  '(check-reg 302 S 1)
  '(check-reg 303 T 3)
  '(check-reg 303 S default))

(define-test "-if"
  "node 100 2 -if 3 + then 5 +
   node 200 0 -if 3 + then 5 +
   node 300 2 - 1 +  -if 3 + then 5 +"
  '(check-reg 100 T 7)
  '(check-reg 100 S default)
  '(check-reg 200 T 5)
  '(check-reg 200 S default)
  '(check-reg 300 T 6)
  '(check-reg 300 S default))

(define-test "and"
  "node 1 1 1 and
          0 0 and
          5 6 and
         -4 8 and
   node 5 2 dup dup -1 . + and
          4 dup dup -1 . + and
          7 dup dup -1 . + and"
  '(check-dat 1 8 4 0 1 default)
  '(check-dat 5 6 7 0 4 0 2 default))

(define-test "push&pop"
  "node 1 2 4 push 3 push 5 pop warm ; ( use 'warm ;' to preserve r stack ) "
  '(check-dat 1 3 5 2 default)
  '(check-ret 1 4 default))

(define-test "b"
  "node 0 east b! @b @b @b +
   node 1 4 west b! 123 !b 2 !b 3 !b"
  '(check-reg 0 T 5)
  '(check-reg 0 S 123)
  '(check-reg 1 T 4))

(define-test "a"
  "node 0 east a! @ @ @ +
   node 1 4 west a! 123 ! 2 ! 3 !
   node 2 12 a! 2 a"
  '(check-reg 0 T 5)
  '(check-reg 0 S 123)
  '(check-reg 1 T 4)
  '(check-reg 2 T 12)
  '(check-reg 2 S 2))

(define-test "ludr-ports"
  "node 000 north    a! east b! 1 !b 2 !b 3 !b @ @ @
   node 001 west  a! east b! @ !b @ !b @ !b
   node 002 west  a! north    b! @ !b @ !b @ !b
   node 102 south  a! north    b! @ !b @ !b @ !b
   node 202 south  a! west  b! @ !b @ !b @ !b
   node 201 east a! west  b! @ !b @ !b @ !b
   node 200 east a! south  b! @ !b @ !b @ !b
   node 100 north    a! south  b! @ 1 + !b @ 1 + !b @ 1 + !b "
  '(check-dat 0 4 3 2 default)
  '(check-dat 1 default default default)
  '(check-dat 102 default default default)
  '(check-dat 202 default default default)
  '(check-dat 201 default default default)
  '(check-dat 200 default default default)
  '(check-dat 100 default default default))

(define-test "blocking-read-right-port1"
  "node 0 east a! @
   node 1 28 west a! . . . . . . 234 !"
  '(check-reg 0 T 234)
  '(check-reg 1 T 28))

(define-test "blocking-read-right-port2"
  "node 1 west a! @
   node 0 28 east a! . . . . . . 234 !"
  '(check-reg 1 T 234)
  '(check-reg 0 T 28))

(define-test "blocking-read-left-port1"
  "node 1 east a! @
   node 2 28 west a! . . . . . . 234 !"
  '(check-reg 1 T 234)
  '(check-reg 2 T 28))

(define-test "blocking-read-left-port2"
  "node 2 west a! @
   node 1 28 east a! . . . . . . 234 !"
  '(check-reg 2 T 234)
  '(check-reg 1 T 28))

(define-test "blocking-read-down-port1"
  "node 0 north a! @
   node 100 28 south a! . . . . . . 234 !"
  '(check-reg 0 T 234)
  '(check-reg 100 T 28))

(define-test "blocking-read-down-port2"
  "node 100 south a! @
   node 0 28 north a! . . . . . . 234 !"
  '(check-reg 100 T 234)
  '(check-reg 0 T 28))

(define-test "blocking-read-up-port1"
  "node 100 north a! @
   node 200 28 south a! . . . . . . 234 !"
  '(check-reg 100 T 234)
  '(check-reg 200 T 28))

(define-test "blocking-read-up-port2"
  "node 200 south a! @
   node 100 28 north a! . . . . . . 234 !"
  '(check-reg 200 T 234)
  '(check-reg 100 T 28))

(define-test "blocking-write-up-port1"
  "node 202 south a! . . . . . . . . . @
   node 102 north a! 234 ! 28"
  '(check-reg 202 T 234)
  '(check-reg 102 T 28))

(define-test "blocking-write-up-port2"
  "node 102 north a! . . . . . . . . . @
   node 202 south a! 234 ! 28"
  '(check-reg 102 T 234)
  '(check-reg 202 T 28))

(define-test "blocking-write-down-port1"
  "node 101 south a! . . . . . . . . . @
   node 1 north a! 234 ! 28"
  '(check-reg 101 T 234)
  '(check-reg 1 T 28))

(define-test "blocking-write-down-port2"
  "node 1 north a! . . . . . . . . . @
   node 101 south a! 234 ! 28"
  '(check-reg 1 T 234)
  '(check-reg 101 T 28))

(define-test "blocking-write-left-port1"
  "node 2 west a! . . . . . . . . . @
   node 1 east a! 234 ! 28"
  '(check-reg 2 T 234)
  '(check-reg 1 T 28))

(define-test "blocking-write-left-port2"
  "node 1 east a! . . . . . . . . . @
   node 2 west a! 234 ! 28"
  '(check-reg 1 T 234)
  '(check-reg 2 T 28))

(define-test "blocking-write-right-port1"
  "node 0 east a! . . . . . . . . . @
   node 1 west a! 234 ! 28"
  '(check-reg 0 T 234)
  '(check-reg 1 T 28))

(define-test "blocking-write-right-port2"
  "node 1 west a! . . . . . . . . . @
   node 0 east a! 234 ! 28"
  '(check-reg 1 T 234)
  '(check-reg 0 T 28))

(define-test "shift"
  "node 4 4 2/ 2 2* 8 2* 1 2* 2* 2/ 2/ 0 2/ 0 2*
   node 3 4 dup 2* . +  ( multiply by 3 )
   node 5 8 dup 2* 2* . +  ( multiply by 5 )
   node 6 3 2* dup 2* . +  ( multiply by 6 )
   node 7 3 dup 2* dup 2* . + . + ( multiply by 7)"
  '(check-dat 4 0 0 1 16 4 2)
  '(check-reg 3 T 12)
  '(check-reg 5 T 40)
  '(check-reg 6 T 18)
  '(check-reg 7 T 21))

(define-test "or"
  "node 7   23 45 or
   node 707 1 2 over push over or or pop ( swap)"
  '(check-reg 707 T 1)
  '(check-reg 707 S 2)
  '(check-reg 7 T 58))

(define-test "drop"
  "node 1 2 4 drop 3
   node 2 1 2 3 push drop pop ( nip)"
  '(check-dat 1 3 2 default)
  '(check-dat 2 3 1 default))

(define-test "a-fetch-inc"
  "node 1 5 a! @+ @+ @+ warm 2 3 4 5 10"
  '(check-dat 1 4 3 2 default))

(define-test "a-store-inc"
  "node 1 1 1 1 10 20 30 0 a! !+ !+ !+"
  '(check-mem 1 30 20 10 1))

(define-test "call+ret"
  "node 1 0 if
            : double dup + ;
            : x4 double double ;
          then
          2 double
          4 x4"
  '(check-reg 1 T 16)
  '(check-reg 1 S 4))

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
  '(check-dat 1 32 15 21))

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
  '(check-dat 1 1 5 6 1 2 0))

(define-test "variables"
  "node 2
   0 if
    : x! @p drop !p ;
    : x 0 ;
   then
    8 x! x push 5 x! pop x"
  '(check-reg 2 T 5)
  '(check-reg 2 S 8))

(define-test "unext"
  "node 1
    1 2 for 2* unext
    1 3 for 2* unext
    1 4 for 2* unext
      4 for 2/ unext
    "
  '(check-dat 1 1 16 8 default))

(define-test ","
  "node 1
    @p @p @p
    , 1 , 22 , 333  "
  '(check-dat 1 333 22 1 default default))

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
  '(check-dat 1 8 5 3 2 ))

(define-test "comma"
  "node 0 5 @p .. , 1 +"
  '(check-dat 0 6))

(define-test "port-execution1"
  "node 1
east b!
.. @p !b ..
dup + dup .

node 2
3
west push ;
"
  '(check-dat 1 default)
  '(check-dat 2 6 6))

(define-test "multiport-execution"
  "node 101
east b!
.. @p !b ..
dup + dup .
node 102
3 rdlu ;"
  '(check-dat 101 default)
  '(check-dat 102 6 6))

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
  '(check-dat 1 0)
  '(check-mem 2 9 3 5))

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
  '(check-dat 2 3 6 6)
  '(check-mem 3 6 3))

(define-test "word-references"
  "node 400
0 if
: A + ;
: B dup ;
: C 2/ ;
then
&A &B &C
"
  '(check-dat 400 4 3 2 0 default))

(define-test "-until"
  "node 509
0 10
begin
-1 + over 2 + over
-until
"
  '(check-dat 509 -1 22))

(define-test "hex+bin"
  "node 100
0x1 0x2 +
0x1 0x0 +
0x12abd 0xd +
0b1010 0b101 +
"
  '(check-dat 100 15 76490 1 3 default))


(define-test "--u-slash-mod"
  "node 500
0 200 10 - 1 . + --u/mod
node 1
0 5 3 - 1 . + --u/mod"
  '(check-dat 500 20 0 default)
  '(check-dat 1 1 2 default))

(define-test "compiler-word"
  "node 600
:: x 3 lit ;
:: xx 5 1 or lit ;
:: xxx 4 5 + lit ;
x xx xxx
x xx +"
  '(check-dat 600 7 9 4 3 default))

(define-test "remote-word-call"
  "node 2
.. ;
.. ;
: fn2  10 + dup !p ;
: fn  5 + dup ;
: main west push ; ( ok )

node 1
east b!

.. @p !b .. @p ..  6 !b ( send arg )
.. @p !b .. fn@2 .. ( send call)
.. @p !b .. !p .. @b ( read result )"

  '(check-dat 1 11 default)
  '(check-dat 2 11 default)
  )

(define-test "multiport-write"
  ;; test that only currently reading nodes can read the value from a multi-port write
"node 408
:: 'rdlu  0x1A5 lit ;
: main
'rdlu b! 5 !b  warm

node 308
: main
north b! @b

node 508
: main
. . . . . . . .
south b! @b warm

node 407
. . . . . . . .
east b! @b warm

node 409
west b! @b warm"
'(check-dat 408 default)
'(check-dat 508 default)
'(check-dat 409 5 default)
'(check-dat 308 5 default)
'(check-dat 407 default)
)

(define-test "wrap-around"
  "node 100
org 62
: main 1 2 3 4 + + +
"
  '(check-dat 100 10 default)
  '(check-mem 100 2 3 4)
  )

(define-test "empty-node"
  "node 1
node 2
1 2 +
node 3
node 4
1 4 + "
  '(check-dat 1 default)
  '(check-dat 2 3 default)
  '(check-dat 3 default)
  '(check-dat 4 5 default))

(define-test "negative-literals"
  "node 1
5 -1 . + warm"
  '(check-dat 1 4 default))

(define-test "subtract"
  "node 1
6 11 - + -
"
  '(check-dat 1 5 default))

(define-test "main"
  "node 1
5
: main 6 warm ;
"
  '(check-dat 1 6 default))

(define-test "extern"
  "node 1
6 11 !!_test-inc
"
  '(check-dat 1 12 6 default))

(defun run-simulation-tests ()
  (let ((tests-failed 0)
        (tests-passed 0))
    (dolist (test (reverse tests))
      (if (funcall test)
          (setq tests-passed (add1 tests-passed))
        (setq tests-failed (add1 tests-failed))))

    (printf "passed: ~a\n" tests-passed)
    (printf "failed: ~a\n" tests-failed)))

(require 'ga-test-target-chip)
(require 'ga-test-pins)

(provide 'ga-tests)
