#lang racket

(require racket/string)
(require racket/function)

;;; Represents a block with the given number and a pipe `code' for
;;; accessing the block's actual (unparsed) code.
(struct block (number code))

;;; Given a block and a line returns a block with the line appended to
;;; its code.
(define (add-line old-block line)
  (struct-copy block old-block [code (append (block-code old-block) (list line))]))

;;; Defines the syntax for a block annotation. Currently, a block is
;;; defined as: "{block <n>}" where n is some decimal number.
(define block-syntax "\\{block ([0-9]+)\\}")

;;; Returns whether the given string is a valid block declaration
;;; (based on block-syntax).
(define (is-block? block) (and (regexp-match block-syntax block) #t))

;;; Drops the initial elements of the list that satisfy some predicate.
(define (drop-while pred? list)
  (cond
   [(null? list) '()]
   [(pred? (car list)) (drop-while pred? (cdr list))]
   [else list]))

;;; Given a list of lines, turns them into blocks. Currently, I just
;;; lose any code before the first block number declaration.
(define (scan-blocks lines)
  (define (go line blocks)
    (let ([num (regexp-match block-syntax line)])
      (if num
          (cons (block (string->number (cadr num)) '()) blocks)
          (cons (add-line (car blocks) line) (cdr blocks)))))
  (map block-to-port (reverse (foldl go '() (drop-while (negate is-block?) lines)))))

;;; Maps a block with a list of strings to one with an output port instead.
(define (block-to-port old-block)
  (struct-copy block old-block
               [code (open-input-string (string-join (block-code old-block) "\n"))]))

;;; Reads a file until eof, returning a list of lines in the file.
(define (slurp file)
  (define (go line lines)
    (let ([c (read-char file)])
      (cond
       [(eof-object? c) lines]
       [(char=? c #\newline) (go "" (cons line lines))]
       [else (go (string-append line (string c)) lines)])))
 (reverse (go "" '())))
