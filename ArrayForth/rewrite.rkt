#lang racket

(provide rewrite-rule rewrites)

;;; Defines a simple rule for rewriting Forth programs.
;; TODO: Add more detailed description!
(define (rewrite-rule pattern result)
  (define (apply-rewrite matches)
    (if (list? result)
        (map (lambda (p) (cond [(pair? p) (list-ref (list-ref matches (car p)) (cadr p))]
                               [(number? p) (car (list-ref matches p))]
                               [else p]))
             result)
        (result matches)))
  (define (go tokens)
    (cond [(null? tokens) '()]
          [(> (length pattern) (length tokens)) tokens]
          [else (let-values ([(first rest) (split-at tokens (length pattern))])
                  (let ([matches (map regexp-match pattern first)])
                    (if (andmap identity matches)
                        (go (append (apply-rewrite matches) rest))
                        (cons (car tokens) (go (cdr tokens))))))]))
  go)

;;; Given the result of matching a hexadecimal number, returns a list
;;; containing that number in decimal.
(define (hex->decimal num)
  (list (number->string (string->number (string-append "#x" (cadr (car num)))))))

;;; A bunch of useful rewrite rules for Forth programs.
(define rules
  (list (rewrite-rule '("\\|" ".*") '())
        (rewrite-rule '("\\[" ".*" "\\]") '("{" 1 "}"))
        (rewrite-rule '("\\[" ".*" ".*" "\\]") '("{" 1 2 "}"))
        (rewrite-rule '("\\[" ".*" ".*" ".*" "\\]") '("{" 1 2 3 "}"))
        (rewrite-rule '("\\[" ".*" ".*" ".*" ".*" "\\]") '("{" 1 2 3 4 "}"))
        (rewrite-rule '("\\[" ".*" ".*" ".*" ".*" ".*")
                      (lambda args (error "Too many words between [ and ]!")))
        (rewrite-rule '("\\$([0-9A-Fa-f]+)") hex->decimal)))

;;; This procedure executes all the necessary rewrites on a list of
;;; Forth tokens.
(define rewrites (foldl compose identity rules))
