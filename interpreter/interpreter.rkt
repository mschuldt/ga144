#lang racket

(require "../compiler/assemble.rkt"
         "../compiler/disassemble.rkt"
         "ga144.rkt"
         "../common.rkt"
         "../compiler/compile.rkt")

(provide (all-defined-out))

(define chips '());;list of ga144 chips
(define (new-ga144)
  (let ((chip (new ga144%)))
    (push chips chip)
    chip))

(define (compile-and-load chip
                          in
                          [include-end-token? #f]
                          #:compiled-file [compiled-file #f]
                          #:assembled-file [assembled-file #f])
  (let* ([n 0]
         [code 0]
         [node 0]
         [compiled (compile in)])
    (when compiled-file
      (with-output-to-file compiled-file
        (lambda () (display-compiled compiled))
        #:exists 'replace))
    (assemble compiled)
    (send chip load compiled)
    (when assembled-file
      ;;must do this last as disassembly mutates the nodes
      (with-output-to-file assembled-file
        (lambda () (display-disassemble compiled))
        #:exists 'replace))))

(define (load-file chip file)
  (call-with-input-file file (lambda (x) (compile-and-load chip x))))

(define (step* [chip #f])
  (if chip
      (send chip step-program!*)
      (for ((c chips))
        (send c step-program!*))))

(define (step interpreter [n 1] [chip #f])
  (if chip
      (send chip step-program-n! n)
      (for ((c chips))
        (send c step-program-n! n))))

(define (reset! [chip #f])
  (if chip
      (send chip reset!)
      (for ((c chips))
        (send c reset!))))
