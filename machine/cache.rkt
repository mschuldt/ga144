#lang racket

(require "state.rkt" "programs.rkt")

(provide load-cache cache-put cache-get-key)

(define data-dir "/home/mangpo/work/forth-interpreter/machine/.db")
(define lock-file (format "~a/lock" data-dir))
(define db-file (format "~a/storage" data-dir))

(define (read-lock)
  (let* ([in (open-input-file lock-file)]
         [content (read in)])
    (close-input-port in)
    content))

(define (lock)
  (if (equal? (read-lock) "TRUE")
      (lock)
      (with-output-to-file lock-file #:exists 'truncate
        (lambda () (display "TRUE")))))

(define (unlock)
  (with-output-to-file lock-file #:exists 'truncate
    (lambda () (display "FALSE"))))

(define (unlock-exn e)
  (with-output-to-file lock-file #:exists 'truncate
    (lambda () (display "FALSE")))
  (raise e))

;; Load database from file
(define init-cache #f)
(define (load-cache cache)
  (define (load-cache-inner)
    (define in (open-input-file db-file))
    (define (loop)
      (let ([next (read-line in)])
        (unless (eof-object? next)
                (let ([content (string-split next ";")])
                  (hash-set! cache (first content) (second content)))
                (loop))))
    (loop)
    (close-input-port in))

  (when (and (not init-cache) (file-exists? db-file))
    (with-handlers* ([exn:break? unlock-exn])
      (lock)
      (load-cache-inner)
      (set! init-cache #t)
      (unlock)
      )))

(define-syntax-rule (string-list a ...)
  (list (format "~a" a) ...))

(define (cache-get-key program num-bits mem time-limit length-limit
                       constraint start-state)
  (define lst
    (string-list program num-bits mem time-limit length-limit constraint 
                 (struct-copy progstate start-state [memory #f])))
  (string-join lst ","))

(define (cache-put cache key value)
  (unless (hash-has-key? cache key)
    (with-handlers* ([exn:break? unlock-exn])
      (define orig-program (car (string-split key ",")))
      (lock)
      (hash-set! cache key value)
      (with-output-to-file db-file #:exists 'append
        (lambda () (pretty-display (format "~a;~a;~a;~a;~a;~a" key value
                                           (length-with-literal orig-program)
                                           (length-with-literal value #:f18a #f)
                                           (estimate-time orig-program)
                                           (estimate-time value #:f18a #f)))))
      (unlock))))