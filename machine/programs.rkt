#lang racket
;;; Some useful functions for working with programs.

(provide (all-defined-out))

(define choice-id '#(@p @+ @b @ !p !+ !b ! +* 2* 2/ - + and or drop dup pop over a nop push b! a! lshift rshift /))
(define memory-op '#(@p @+ @b @ !p !+ !b !))

;;; Given a string containing a forth program, gives you an estimate
;;; of how long it would take to run.
(define (estimate-time program)
  (define (guess-speed instr)
    (if (member instr (vector->list memory-op)) 10 3))
  (apply + (map guess-speed (drop-trailing-nops
                             (filter (lambda (x) (vector-member x choice-id))
                                     (map string->symbol (program->instructions program)))))))

;;; Drop elements from the list while some predicate holds.
(define (drop-while pred? list)
  (cond
   [(null? list) '()]
   [(pred? (car list)) (drop-while pred? (cdr list))]
   [else list]))

;;; Given a list of instructions, drops all the nops at the end of the
;;; list.
(define drop-trailing-nops
  (compose reverse (curry drop-while (curry equal? 'nop)) reverse))

;;; Trim leading and trailing whitespace.
(define (trim str)
  (regexp-replace "^ +" (regexp-replace " +$" str "") ""))

;;; Splits a program string into a list of instruction strings.
(define program->instructions (compose (curry regexp-split #px"\\s+") trim))

;;; Fixes calls to @p for use in the synthesizer/verifier by replacing
;;; each @p instruction with the literal it puts on the stack. That
;;; is, `@p @p nop nop 1 2' would turn into `1 2 nop nop'. If you have
;;; an @p that is never followed by a number, this will break; you
;;; should only use @p with numeric constants (for now, anyhow)!
(define (fix-@p program)
  (define first-number (compose first (curry filter string->number)))
  (define (go instrs)
    (cond [(null? instrs) '()]
          [(equal? (first instrs) "@p")
           (let ([n (first-number instrs)])
             (cons n (go (remove n (rest instrs)))))]
          [else (cons (first instrs) (go (rest instrs)))]))
  (string-join (go (program->instructions program)) " "))

;;; Returns the length of the program.
(define program-length (compose length program->instructions))

;;; Gets the length of the program, ignoring trailing nops.
(define (program-length-abs prog) 
  (length (drop-trailing-nops (map string->symbol (program->instructions (fix-@p prog))))))

;;; Returns #t if every + except the first is precedeed by a nop.
(define (nop-before-plus? program)
  (define (go instrs)
    (cond
     [(or (null? instrs) (null? (rest instrs))) #t]
     [(and (equal? (second instrs) "+") (not (equal? (first instrs) "nop"))) #f]
     [else (go (rest instrs))]))
  (go (program->instructions (fix-@p program))))
