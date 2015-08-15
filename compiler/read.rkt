#lang racket

(require "../common.rkt")
(provide read-token forth-read forth-read-no-eof)

(define line-number 1)
(define col-number 0)

(define (read-token in)
  (define (get-first-char-in-list)
    (let ((new-char (read-char in)))
      (cond [(eof-object? new-char) new-char]
            [(eq? new-char #\newline)
             (set! line-number (add1 line-number))
             (set! col-number 0)
             (get-first-char-in-list)]
            [(char-whitespace? new-char)
             (set! col-number (add1 col-number))
             (get-first-char-in-list)]
            [else (list new-char)])))

  (define (iter lst)
    (if (or (eof-object? (peek-char in))
            (eq? (peek-char in) #\newline))
        (begin (set! col-number (add1 col-number))
               lst)
        (let [(new-char (read-char in))]
          (set! col-number (add1 col-number))
          (if (char-whitespace? new-char)
              (begin (set! col-number (add1 col-number))
                     lst)
              (iter (cons new-char lst))))))

  (let [(first-char (get-first-char-in-list))
        (line-no line-number)
        (col-no col-number)]
    (if (list? first-char)
        (let ((tok (list->string (reverse (iter first-char)))))
          (token tok line-no col-no))
	(token first-char line-no col-no))))

(define (make-reader)
  (let ((stack '()))
    (lambda ([tok #f] [line -1] [col -1])
      (if tok
          (set! stack (cons (token tok line col) stack))
	  (if (null? stack)
	      (read-token (current-input-port))
	      (begin0 (car stack)
                (set! stack (cdr stack))))))))
(define forth-read (make-reader))

(define (forth-read-no-eof)
  (let [(res (forth-read))]
    (if (eof-object? res)
        (error "Unexpected EOF")
        res)))
