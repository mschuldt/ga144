#lang racket

;;; Some very simple tests of the interpreter.

(require "interpreter.rkt" "stack.rkt" rackunit)

(define tests '())

;;; Creates an arrayForth stack, turning the 8 given numbers into
;;; 18bits. If too few or too many numbers are passed in, this raises
;;; an error.
(define forth-stack
  (lambda contents
    (unless (= (length contents) 8) (raise "Wrong stack size."))
    (list->vector (map 18bit contents))))

(define old-state (make-parameter 1))

;;; Defines a new test running the given arrayForth program followed
;;; by the body code. The interpreter state is reset automatically
;;; before the test is run.
(define-syntax define-test
  (syntax-rules ()
    ((_ program statement ...)
     (set! tests (cons (lambda ()
                         (reset!)
                         (load-program program)
                         (step-program!*)
                         statement ...) tests)))))

;;; Helper macro for `check-unchanged?'  Checks if the given id was not
;;; changed after running the program. This currently assumes you
;;; started with a call to (reset!).
(define-syntax (check-unchanged-1 stx)
  (syntax-case stx ()
    [(_ id)
     (with-syntax
         ([get (datum->syntax
                stx (string->symbol
                     (format "state-~a" (symbol->string (syntax->datum #'id)))))])
       (syntax (check-equal? id (get start-state))))]))

;;; Checks whether the given variables (registers/stacks/memory) were
;;; not changed from the start state.
(define-syntax check-unchanged?
  (syntax-rules ()
    ((_ id ...)
     (begin (check-unchanged-1 id) ...))))

(define-test "@p @p + nop 2 3"
  (check-equal? p 3)
  (check-equal? t 5)
  (check-unchanged? a b r s return))

(define-test "@p - nop nop 0"
  (check-equal? p 2)
  (check-equal? t (18bit -1))
  (check-unchanged? a b r s return))

;;; This should write 42 to slot 4 in memory then put it on top of the
;;; stack.
(define-test "@p b! @p nop 4 42 !b @p nop nop"
  (check-equal? p 5)
  (check-equal? t 42)
  (check-equal? b 4)
  (check-equal? (vector-ref memory 4) 42)
  (check-unchanged? a r s return))

;;; An example of checking the whole data stack as well as the pc.
(define-test "- dup dup dup dup dup dup dup"
  (check-equal? p 2)
  (check-equal? t (18bit -1))
  (check-equal? s (18bit -1))
  (check-equal? data (stack 7 (forth-stack 0 0 -1 -1 -1 -1 -1 -1)))
  (check-unchanged? a b r return))

;;; Testing the basic instructions:
(define-test "call 2 nop nop nop nop ret nop nop nop" ; return
  (check-equal? p 1)
  (check-unchanged? a b r s t data return))

;; TODO: execute

(define-test "jump 42"                  ; jump
  (check-equal? p 42)
  (check-unchanged? a b r s t data return))

(define-test "call 10"                  ; call
  (check-equal? r 1)
  (check-equal? p 10)
  (check-unchanged? a b s t data))

(define-test "nop nop unext nop"        ; unext
  (check-equal? p 1)
  (check-unchanged? a b r s t data))

(define-test "@p push nop nop 41 @+ nop nop unext" ; unext
  (check-equal? p 3)
  (check-equal? a 42)
  (check-unchanged? b r s t))

;; TODO: next

(define-test "if 42"                    ; if
  (check-equal? p 1)
  (check-unchanged? a b r s t data return))

(define-test "@p if 42 10"              ; if
  (check-equal? p 42)
  (check-unchanged? a b r s return))

(define-test "-if 42"                   ; -if
  (check-equal? p 42)
  (check-unchanged? a b r s t return))

(define-test "- -if 42"                 ; -if
  (check-equal? p 1)
  (check-unchanged? a b r s return))

(define-test "@p nop nop nop 42"        ; @p
  (check-equal? p 2)
  (check-equal? t 42)
  (check-unchanged? a b r s return))

(define-test "@+ nop nop nop"           ; @+
  (check-equal? a 1)
  (check-equal? t (vector-ref memory 0))
  (check-unchanged? b r s return))

(define-test "@b nop nop nop"           ; @b
  (check-equal? t (vector-ref memory 0))
  (check-unchanged? b r s return))

(define-test "@ nop nop nop"            ; @
  (check-equal? t (vector-ref memory 0))
  (check-unchanged? b r s return))

(define-test "@p !p nop nop 42"         ; !p
  (check-equal? p 3)
  (check-equal? (vector-ref memory (sub1 p)) 42)
  (check-unchanged? a b r s return))

(define-test "@p !+ nop nop 42"         ; !+
  (check-equal? a 1)
  (check-equal? p 2)
  (check-equal? 42 (vector-ref memory 0))
  (check-unchanged? b r s return))

(define-test "@p @p a! nop 42 10 !+ nop nop nop" ; !+
  (check-equal? a 11)
  (check-equal? p 4)
  (check-equal? 42 (vector-ref memory 10))
  (check-unchanged? b r s return))

(define-test "@p !b nop nop 42"         ; !b
  (check-equal? p 2)
  (check-equal? 42 (vector-ref memory 0))
  (check-unchanged? a b r s return))

(define-test "@p @p b! nop 42 10 !b nop nop nop" ; !b
  (check-equal? p 4)
  (check-equal? 42 (vector-ref memory 10))
  (check-equal? b 10)
  (check-unchanged? a r s return))

(define-test "@p ! nop nop 42"         ; !
  (check-equal? p 2)
  (check-equal? 42 (vector-ref memory 0))
  (check-unchanged? a b r s return))

(define-test "@p @p a! nop 42 10 ! nop nop nop" ; !
  (check-equal? a 10)
  (check-equal? p 4)
  (check-equal? 42 (vector-ref memory 10))
  (check-unchanged? b r s return))

;; TODO: +*

(define-test "@p 2* nop nop 2"          ; 2*
  (check-equal? t 4)
  (check-equal? p 2)
  (check-unchanged? a b r s return))

(define-test "@p 2/ nop nop 4"          ; 2/
  (check-equal? t 2)
  (check-equal? p 2)
  (check-unchanged? a b r s return))

(define-test "- nop nop nop"            ; -
  (check-equal? t (18bit -1))
  (check-equal? p 1)
  (check-unchanged? a b r s data return))

(define-test "@p - nop nop 42"          ; -
  (check-equal? t (18bit (bitwise-not 42)))
  (check-equal? p 2)
  (check-unchanged? a b r s return))

(define-test "@p @p + nop 12 30"        ; +
  (check-equal? t 42)
  (check-equal? p 3)
  (check-unchanged? a b r s return))

(define-test "@p @p and nop 12 30"      ; and
  (check-equal? t (18bit (bitwise-and 12 30)))
  (check-equal? p 3)
  (check-unchanged? a b r s return))

(define-test "@p @p or nop 12 30"      ; or
  (check-equal? t (18bit (bitwise-xor 12 30)))
  (check-equal? p 3)
  (check-unchanged? a b r s return))

(define-test "@p @p drop nop 1 2"       ; drop
  (check-equal? t 1)
  (check-equal? p 3)
  (check-unchanged? a b r s return))

(define-test "@p dup nop nop 42"        ; dup
  (check-equal? t 42)
  (check-equal? s 42)
  (check-equal? p 2)
  (check-unchanged? a b r return))

(define-test "@p dup or nop 42"         ; dup
  (check-equal? t 0)
  (check-equal? p 2)
  (check-unchanged? a b r s return))

(define-test "call 2 0 pop nop nop nop" ; pop
  (check-equal? t 1)
  (check-unchanged? a b r s return))

(define-test "@p @p over nop 1 2"       ; over
  (check-equal? t 1)
  (check-equal? s 2)
  (check-equal? (vector-ref (stack-body data) (stack-sp data)) 1)
  (check-equal? p 3)
  (check-unchanged? a b r return))

(define-test "@p a! a nop 42"           ; a
  (check-equal? a 42)
  (check-equal? t 42)
  (check-unchanged? b r s return))

(define-test "nop nop nop nop"          ; nop
  (step-program!)                       ; this is needed because step-program!* stops at nop nop nop nop.
  (check-equal? p 1)
  (check-unchanged? a b r s t data return))

(define-test "@p push nop nop 42"       ; push
  (check-equal? r 42)
  (check-equal? p 2)
  (check-unchanged? a b s t))

(define-test "@p b! nop nop 42"         ; b!
  (check-equal? b 42)
  (check-equal? p 2)
  (check-unchanged? a r s t return data))

(define-test "@p a! nop nop 42"         ; a!
  (check-equal? a 42)
  (check-equal? p 2)
  (check-unchanged? b r s t return data))

;;; Run all the currently defined tests.
(define (run-tests) (for ([test tests]) (test)))
