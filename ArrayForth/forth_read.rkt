#lang racket

(provide forth_read_no_eof forth_read read-basic read-syntax read-to-list)

(define (read-basic in)
  (define (get_first_char_in_list)
    (let ((new_char (read-char in)))
      (cond [(eof-object? new_char)
             new_char]
            [(eq? new_char #\newline)
             new_char]
            [(char-whitespace? new_char)
             (get_first_char_in_list)]
            [else
             (list new_char)])))
  (define (iter lst)
    (if (or (eof-object? (peek-char in)) (eq? #\newline (peek-char in)))
        lst
        (let [(new_char (read-char in))]
          (if (char-whitespace? new_char)
              lst
              (iter (append lst (list new_char)))))))
  (let [(first_char (get_first_char_in_list))]
    (if (or (eof-object? first_char) (eq? first_char #\newline))
        first_char
        (let [(lst (iter first_char))]
          (list->string lst)))))

;;; Given a port, returns a list of forth tokens.
(define (read-to-list port)
  (let ([token (read-basic port)])
    (if (eof-object? token) '() (cons token (read-to-list port)))))

(define (read-syntax src in)
  (read-basic in))

(define (make-reader func)
  (let [(stack '())]
    (case-lambda
     [()
      (if (null? stack)
	  (func)
	  (begin0 (car stack)
		  (set! stack (cdr stack))))]
     [(msg)
      (when (not (equal? msg 'clear))
	    (raise (string-append "Unknown message to reader: " msg)))
      (set! stack '())]
     [(msg token)
      (when (not (equal? msg 'put-back))
	    (raise (string-append "Unknown message to reader: " msg)))
      (set! stack (cons token stack))] )))

(define forth_read
  (make-reader (lambda () (read-basic (current-input-port)))))

(define (forth_read_no_eof)
  (let [(res (forth_read))]
    (if (eof-object? res)
        (error "Unexpected EOF")
        res)))

