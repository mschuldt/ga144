#lang racket ;; -*- lexical-binding: t -*-

(require "common.rkt"
         "el.rkt")

(when elisp?
  (_def '(read-token
          forth-read
          forth-read-no-eof
          forth-read-char
          parse-code
          display-parsed-code)))

(provide read-token
         forth-read
         forth-read-no-eof
         forth-read-char
         parse-code
         display-parsed-code)

(define line-number 1)
(define col-number 0)

(define (syntax-error msg)
  (raise (rkt-format "[~a:~a]syntax error: ~a" line-number col-number msg )))

(define (read-token in)
  (assert (not elisp?))
  (define (get-first-char-in-list)
    (let ((new-char (read-char in)))
      (cond ((eof-object? new-char) new-char)
            ((eq? new-char _char-newline)
             (set! line-number (add1 line-number))
             (set! col-number 0)
             (get-first-char-in-list))
            ((char-whitespace? new-char)
             (set! col-number (add1 col-number))
             (get-first-char-in-list))
            (else (list new-char)))))

  (define (iter lst)
    (if (or (eof-object? (peek-char in))
            (eq? (peek-char in) _char-newline))
        (begin (set! col-number (add1 col-number))
               lst)
        (let ((new-char (read-char in)))
          (set! col-number (add1 col-number))
          (if (char-whitespace? new-char)
              (begin (set! col-number (add1 col-number))
                     lst)
              (iter (cons new-char lst))))))

  (let ((first-char (get-first-char-in-list))
        (line-no line-number)
        (col-no col-number))
    (if (list? first-char)
        (let ((tok (list->string (reverse (iter first-char)))))
          (token tok line-no col-no))
        (token first-char line-no col-no))))

(define (forth-read-char)
  (let ((char (read-char)))
    (when (eq? char _char-newline)
      (set! line-number (add1 line-number))
      (set! col-number 0))
    char))

(define (make-reader)
  (let ((stack '()))
    (lambda ((tok false) (line -1) (col -1))
      (if tok
          (set! stack (cons (token tok line col) stack))
          (if (null? stack)
              (read-token (current-input-port))
              (begin0 (car stack)
                (set! stack (cdr stack))))))))

(define forth-read (make-reader))

(define (comment)
  ;;TODO: update line/col numbers
  (unless (equal? (forth-read-char) _char-close-paren)
    (comment)))

(define (parse-code)
    (assert (not elisp?))
  ;;returns list of cons: ((NODE . CODELIST)...))
  ;;first item is list of code before the first node. boot descriptors etc
  ;;CODELIST is a list of type struct token.
  ;; the first two words are the colon and word name
  (define nodes '())
  (define current-node-num false)
  (define current-node-code false)

  (define (read-loop)
    (define tok-token (forth-read))
    (define tok (token-tok tok-token))
    (cond ((equal? tok "(")
           (comment)
           (read-loop))
          ((equal? tok "node")
           (let* ((node-token tok-token)
                  (num-token (forth-read))
                  (node-tok (token-tok node-token))
                  (num-tok (token-tok num-token)))
             (when (not (eof-object? node-tok))
               (when (or (eof-object? num-tok)
                         (not (equal? node-tok "node"))
                         (not (string->number num-tok)))
                 (syntax-error "malformed node syntax"))
               (set! nodes (cons (cons (string->number num-tok) (parse-words))
                                 nodes))
               (read-loop))))
          ((eof-object? tok)
           ;;return
           )
          ((equal? tok "bootstream")
           (forth-read) ;;TODO: ignoring for now
           (read-loop))
          (else (syntax-error (rkt-format "don't know what to do with '~a'" tok)))))

  (read-loop)
  (reverse nodes))


(define (parse-words)
  ;;returns a list of token lists, one for each word
  ;;if a word does not contain and ending ; then it is
  ;;merged with the next word
  (assert (not elisp?))
  (define words '())
  (define last false)
  (define current-word '())
  (define current-name false)
  (define done false)
  (define (end-word (name false))
    (set! current-word (reverse current-word))
    (when (= (length current-word) 0)
      (syntax-error "empty node body"))
    (set! words (cons current-word words))
    (set! current-word '()))

  (define (read-loop)
    (let* ((token (forth-read))
           (tok (and token (token-tok token))))
      (if (or (eof-object? tok)
              done)
          (begin
            (forth-read tok)
            (when (not (null? current-word))
              (when (= (length current-word) 0) (syntax-error "empty node body."))
              (set! words (cons (reverse current-word) words))))
          (begin
            (cond ((and (equal? words '())
                        (not (equal? current-word null))
                        (equal? tok ":"))
                   ;;code before the first word - create a pseudo word for it
                   (end-word "_first") ;;TODO: use false instead
                   (set! current-word (list token)))

                  ((and (and last (equal? (token-tok last) ";"))
                        (equal? tok ":"))
                   ;;start of a new word
                   (end-word)
                   (define name (forth-read))
                   (when (or (not name) (eof-object? (token-tok name)))
                     (syntax-error "expected word name following ':'"))
                   (set! current-word (list name token)))
                  ((equal? tok "(")
                   (comment))
                  ((equal? tok "node")
                   (forth-read tok)
                   (end-word)
                   (set! done t))
                  (else (set! current-word (cons token current-word))))
            (set! last token)
            (read-loop)))))
  (read-loop)
  (reverse words))

(define (forth-read-no-eof)
    (assert (not elisp?))
  (let ((res (forth-read)))
    (if (eof-object? res)
        (error "Unexpected EOF")
        res)))

(define (display-parsed-code parsed-nodes)
    (assert (not elisp?))
  ;; (display-parsed-code (parse-code))
  (for/list ((node parsed-nodes))
    (printf "_____________________ node ~a ___________________\n" (car node))
    ;;print boot descriptors
    (for/list ((inst (cadr node)))
      (printf " ~a " (token-tok inst)))
    (printf "\n")
    ;;print words
    (for/list ((word (cddr node)))
      ;; print ": <wordname>"
      (printf " ~a ~a\n"
              (token-tok (car word))
              (token-tok (cadr word)))
      ;; print word body
      (for/list ((inst (cddr word)))
        (printf "    ~a\n" (token-tok inst))))))
