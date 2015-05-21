#lang racket

(provide read-token forth_read forth_read_no_eof)

(define (read-token in)
  (define (get-first-char-in-list)
    (let ((new-char (read-char in)))
      (cond [(eof-object? new-char) new-char]
            [(or (eq? new-char #\newline)
                 (char-whitespace? new-char))
             (get-first-char-in-list)]
            [else (list new-char)])))
  (define (iter lst)
    (if (or (eof-object? (peek-char in))
            (eq? (peek-char in) #\newline))
        lst
        (let [(new-char (read-char in))]
          (if (char-whitespace? new-char)
              lst
              (iter (cons new-char lst))))))

  (let [(first-char (get-first-char-in-list))]
    (if (list? first-char)
	(list->string (reverse (iter first-char)))
	first-char)))

(define (make-reader)
  (let ((stack '()))
    (lambda ([token #f])
      (if token
          (set! stack (cons token stack))
	  (if (null? stack)
	      (read-basic (current-input-port))
	      (begin0 (car stack)
                (set! stack (cdr stack))))))))
(define forth_read (make-reader))

(define (forth_read_no_eof)
  (let [(res (forth_read))]
    (if (eof-object? res)
        (error "Unexpected EOF")
        res)))
