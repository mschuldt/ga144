#lang racket

(require "forth_state.rkt" "rvector.rkt")
(provide (all-defined-out))

(define (displaynl arg)
  (display arg)
  (newline))

(define (displayspace arg)
  (display arg)
  (display " "))

; Compiles a string and displays it upon execution.
; Note: Can only be used in the colon compiler.
(define (read-string)
  (define (iter lst)
    (let [(new-char (read-char))]
      (if (eq? new-char #\")
          (list->string lst)
          (iter (append lst (list new-char))))))
  (iter '()))

(define (dot-quote)
  (let [(str (read-string))]
    (add-primitive-code! (lambda () (display str)))))
(add-primitive-word! #t ".\"" dot-quote)

(add-primitive-word! #f "cr" newline)
(add-primitive-word! #f "space" (lambda () (display " ")))

(define (spaces)
  (define (loop num)
    (if (= num 0)
        'done
        (begin (display " ") (loop (sub1 num)))))
  (loop (pop-int! #f)))
(add-primitive-word! #f "spaces" spaces)

(add-primitive-word! #f "emit"
                     (lambda () (display (integer->char (pop-int! #f)))))


(add-primitive-word! #f "send"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
                              (arg2 (pop-int! #t))]
			 (if (rvector-ref send-recv-table arg1)
			     (set! pc (sub1 pc))
			     (rvector-set! send-recv-table arg1 arg2)))))

(add-primitive-word! #f "recv"
                     (lambda ()
                       (let* [(arg1 (pop-int! #t))
			      (val (rvector-ref send-recv-table arg1))]
			 (if val
			     (push-int! val)
			     (begin (push-int! arg1) (set! pc (sub1 pc)))))))

; Debugging
(add-primitive-word! #f ".s" (lambda () (print-stack (state-stack (vector-ref cores state-index)))))
(add-primitive-word! #f ".ns" (lambda () (print state-index) (display ": ") (print-stack (state-stack (vector-ref cores state-index))) (newline)))
(add-primitive-word! #f ".r" (lambda () (print-stack (state-rstack (vector-ref cores state-index)))))

; Output

; Displays an int, interpreted as a signed number
(add-primitive-word! #f "." (lambda () (displayspace (pop-int! #t))))

; Displays an int, interpreted as an unsigned number
(add-primitive-word! #f "u." (lambda () (displayspace (pop-int! #f))))

; Displays a double, interpreted as a signed number
(add-primitive-word! #f "d." (lambda () (displayspace (pop-double! #t))))

; Displays a double, interpreted as an unsigned number
(add-primitive-word! #f "du." (lambda () (displayspace (pop-double! #f))))
