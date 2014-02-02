#lang racket

(require "state.rkt" "programs.rkt")

(provide load-cache cache-has-key? cache-ref cache-put cache-get-key)

;; (define data-dir ".db")
(define data-dir "/home/mangpo/work/forth-interpreter/machine/.db")
;; (define lock-file (format "~a/lock" data-dir))
(define db-file-length (format "~a/storage-length" data-dir))
(define db-file-time (format "~a/storage-time" data-dir))

;; (define (read-lock)
;;   (let* ([in (open-input-file lock-file)]
;;          [content (read in)])
;;     (close-input-port in)
;;     content))

;; (define (lock)
;;   (if (equal? (read-lock) "TRUE")
;;       (lock)
;;       (with-output-to-file lock-file #:exists 'truncate
;;         (lambda () (display "TRUE")))))

;; (define (unlock)
;;   (with-output-to-file lock-file #:exists 'truncate
;;     (lambda () (display "FALSE"))))

;; (define (unlock-exn e)
;;   (with-output-to-file lock-file #:exists 'truncate
;;     (lambda () (display "FALSE")))
;;   (raise e))

;; Load database from file
(define init-cache #f)
(define cache-length (make-hash))
(define cache-time   (make-hash))

(define (filter-key key)
  (define key-split (string-split key ","))
  ;; Exclude memory size
  (string-join (append (take key-split 2) (drop key-split 3)) ","))

(define (load-cache)
  (define (load-cache-inner)
    (define (loop in cache)
      (let ([next (read-line in)])
        (if (eof-object? next)
            (close-input-port in)
            (let* ([content (string-split next ";")]
                   [key (first content)]
                   [val (cdr content)])
              (hash-set! cache (filter-key key) (if (equal? val "timeout") 'timeout val))
              (loop in cache)))))
    
    (when (file-exists? db-file-length)
          (loop (open-input-file db-file-length) cache-length))
    (when (file-exists? db-file-time)
          (loop (open-input-file db-file-time)   cache-time)))

  (unless init-cache
    ;; (with-handlers* ([exn:break? unlock-exn])
    ;;   (lock)
    ;;   (system (format "echo FALSE > ~a/lock" data-dir))
      (system (format "mkdir ~a" data-dir))
      (load-cache-inner)
      (set! init-cache #t)
      ;; (unlock)
      ;; )
    ))

(define-syntax-rule (string-list a ...)
  (list (format "~a" a) ...))

(define (cache-get-key program num-bits mem time-limit length-limit 
                       constraint start-state)
  (define lst
    (string-list program num-bits mem time-limit length-limit constraint 
                 (struct-copy progstate start-state [memory #f])))
  (string-join lst ","))

(define (cache-has-key? type key)
  (if (equal? type `time)
      (hash-has-key? cache-time (filter-key key))
      (hash-has-key? cache-length (filter-key key))))

(define (cache-ref type key)
  (if (equal? type `time)
      (hash-ref cache-time (filter-key key))
      (hash-ref cache-length (filter-key key))))

(define (join lst delim)
  (string-join (map (lambda (x) 
                      (cond
                       [(number? x) (number->string x)]
                       [(symbol? x) (symbol->string x)]
                       [else x]))
                    lst)
	       delim))

(define (cache-put type key value)
  (define cache
    (if (equal? type `time)
        cache-time
        cache-length)) ;; If type = #f, default to cache-length.
  (define db-file
    (if (equal? type `time)
        db-file-time
        db-file-length)) ;; If type = #f, default to cache-length.
  (define key-mod (filter-key key))
  (unless (hash-has-key? cache key-mod)
    (define orig-program (car (string-split key ",")))
    (define orig-length (length-with-literal orig-program))
    (define orig-time (estimate-time orig-program))
    ;; (with-handlers* ([exn:break? unlock-exn])
    ;;   (lock)
      (define val (list value
			orig-length
			(if (equal? value 'timeout) 
			    orig-length
			    (length-with-literal value))
			(estimate-time orig-program)
			(if (equal? value 'timeout) 
			    orig-time
			    (estimate-time value))))
      (hash-set! cache key-mod val)
      (with-output-to-file db-file #:exists 'append
        (lambda () 
          (pretty-display (format "~a;~a" key (join val ";")))))
      ;; (unlock))
  ))
