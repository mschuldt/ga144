#lang racket
;;; Some useful functions for working with programs.

(require rackunit
	 "../ArrayForth/compiler.rkt")

(provide (all-defined-out))

;;; this is consistent with arrayForth
(struct port (u d l r))
(define default-port (port #x145 #x115 #x175 #x1d5))

(define UP #x145) ;325
(define DOWN #x115) ;277
(define LEFT #x175) ;373
(define RIGHT #x1d5) ;469
(define IO #x15d)
(define MEM-SIZE 1024)

(define (set-udlr u d l r)
  (set! UP u)
  (set! DOWN d)
  (set! LEFT l)
  (set! RIGHT r))

(define (set-udlr-from-constraints mem num-bits)
  (set! UP   (port-u default-port))
  (set! DOWN (port-d default-port))
  (set! LEFT (port-l default-port))
  (set! RIGHT (port-r default-port))
  (define bound (arithmetic-shift 1 num-bits))
  (when (or (< DOWN mem) (>= RIGHT bound))
	(begin (when (< (- bound mem) 8)
		     (begin (pretty-display "num-bits is too small!") (exit)))
	       (set-udlr (- bound 8) (- bound 11) (- bound 5) (- bound 2))))
)

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

(define (length-with-literal program #:f18a [f18a #t])
  (unless f18a
	  (set! program (compile-to-string program)))
  (define inst-list (drop-trailing-nops (string-split program)))
  (define total (length inst-list))
  (define count-@p (count (lambda (x) (or (equal? x "@p") (equal? x `@p)))
                          inst-list))
  (+ total (* 3 count-@p))) ;; @p 1 -> count as 2 + 3

;;; Drop elements from the list while some predicate holds.
(define (drop-while pred? list)
  (cond
   [(null? list) '()]
   [(pred? (car list)) (drop-while pred? (cdr list))]
   [else list]))

;;; Given a list of instructions, drops all the nops at the end of the
;;; list.
;(define drop-trailing-nops
;  (compose reverse (curry drop-while (lambda (x) (or (equal? x 'nop) (equal? x "nop")))) reverse))

(define (drop-trailing-nops program)
  (define rm #t)
  (define (inner insts)
    (if (empty? insts)
        insts
        (let* ([rest (inner (cdr insts))]
               [ele (car insts)]
               [x (if (string? ele) (string->symbol ele) ele)])
          (cond
            [(and rm (equal? x 'nop))
             rest]
            [(vector-member x choice-id)
             (set! rm #f)
             (cons ele rest)]
            [else
             (cons ele rest)]))))
  (inner program))

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

(define (preprocess prog)
  (define out prog)
  (set! out (regexp-replace* "UP" out (format "~a" UP)))
  (set! out (regexp-replace* "DOWN" out (format "~a" DOWN)))
  (set! out (regexp-replace* "LEFT" out (format "~a" LEFT)))
  (set! out (regexp-replace* "RIGHT" out (format "~a" RIGHT)))
  (set! out (regexp-replace* #rx"[.]" out "nop"))
  (set! out (regexp-replace* #rx"[;]" out "ret"))
  out)

(define (postprocess prog)
  (define out prog)
  (set! out (regexp-replace* "nop" out "."))
  (set! out (regexp-replace* "ret" out ";"))
  out)

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

(define (insert-nops program)

  (define (nop-before-plus insts)
    (cond
     [(empty? insts) insts]
     [(and (or (equal? (first insts) "nop") (equal? (first insts) "."))
           (not (empty? (cdr insts)))
           (equal? (second insts) "+"))
      (append (list "nop" "+") (nop-before-plus (cddr insts)))]
     [(equal? (first insts) "+")
      (append (list "nop" "+" )(nop-before-plus (cdr insts)))]
     [else
      (cons (car insts) (nop-before-plus (cdr insts)))]))

  (define (nop-last-slot program count)
    (cond
     [(empty? program) program]
     [(= (modulo count 4) 3)
      (define id (vector-member (car program) choice-id))
      (if (or (not id) (= (modulo id 4) 0))
          (cons (car program) (nop-last-slot (cdr program) (add1 count)))
          (cons "nop" (nop-last-slot program (add1 count))))]
     [else
      (cons (car program) (nop-last-slot (cdr program) (add1 count)))]))

  (when (string? program)
        (set! program (string-split program)))
  (string-join (nop-last-slot (nop-before-plus program) 0)))
      
