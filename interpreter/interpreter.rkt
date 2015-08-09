#lang racket
;; simulates a collection of ga144 chips

(require compatibility/defmacro
         "../compiler/assemble.rkt"
         "../compiler/disassemble.rkt"
         "ga144.rkt"
         "../common.rkt"
         "../compiler/compile.rkt")

(provide (all-defined-out))

(define stdin (current-input-port))
(define _commands (make-hash))
(define _help (make-hash))
(define DEBUG #f)

(define chips '());;list of ga144 chips
(define num-chips 0)
(define name-to-chip (make-hash)) ;; maps chip names to chip objects
;; the currently selected chip. If not false, commands like
;; 'step' apply only to this chip. Otherwise they apply to all
;; chips in the 'chips' list
(define selected-chip #f)
(define selected-node #f)

(define _counter 1)
(define (new-ga144 [name #f])
  (unless name
    (set! name (format "chip~a" _counter))
    (set! _counter (add1 _counter)))
  (let ((chip (new ga144% [name name])))
    (push chips chip)
    (hash-set! name-to-chip name chip)
    (set! num-chips (add1 num-chips))
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

(define (step [n 1] [chip #f])
  (if chip
      (send chip step-program-n! n)
      (for ((c chips))
        ;;TODO: stop when everything is suspended
        (send c step-program-n! n))))

(define (reset! [chip #f])
  (if chip
      (send chip reset!)
      (for ((c chips))
        (send c reset!))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; command line interface

(defmacro def-command (name params doc-str body)
  (let* ((num-params (length params))
         (name-str (symbol->string name))
         (command-name (string-append name-str
                                      (number->string num-params))))

    `(let ((help (if (hash-has-key? _help ,name-str)
                     (hash-ref _help ,name-str)
                     '())))
       (hash-set! _commands ,command-name
                  ,(if (> num-params 0)
                       `(lambda (args)
                          (when DEBUG (printf ,(format "[[~a]]\n" command-name)))
                          (define (next-arg)
                            (let ((val (car args)))
                              (set! args (cdr args))
                              val))
                          (let (,@(for/list ((p params))
                                    (list p '(next-arg))))
                            ,body))
                       `(lambda (_) ,body)))

       (hash-set! _help ,name-str
                  (cons (string-join (cons ,name-str
                                           (append (for/list ((p ',params))
                                                     (symbol->string p))
                                                   (list  "   " ,doc-str))))
                        help)))))

(def-command help (command) "print help string for COMMAND"
  (pretty-display (get-help-string command)))

(def-command help () "print all commands"
  (for ((key (hash-keys _help)))
    (let* ((commands (hash-ref _help key))
           (len (length commands)))
      (cond ((> len 1)
             (printf "~a\n" key)
             (for ((x commands))
               (printf "   ~a\n" x)))
            ((= len 1)
             (printf "~a\n" (car commands)))
            (else (printf "??????\n"))))))

(def-command chips () "List all GA144 chips"
  (for ((chip chips))
    (pretty-display (get-field name chip))))

(def-command step () "Step 1 word"
  (step 1 selected-chip))

(def-command step (n) "Step N words"
  (step (string->number n) selected-chip))

(def-command break (n) "Set breakpoint at word N"
  (printf "TODO\n"))

(def-command unbreak (n) "Remove breakpoint from word N"
  (printf "TODO\n"))

(def-command continue () "Continue execution"
  (step* selected-chip))

(def-command reset () "Reset chip"
  (reset! selected-chip))

(def-command show () "Print a representation of the chip"
  (if selected-chip
      (begin
        (send selected-chip print-active))
      (for ((chip chips))
        (printf "\nchip: ~a\n" (get-field name chip))
        (send chip print-active))))

(def-command show (node) "Print a representation of NODE in selected chip"
  (if selected-chip
      (begin
        (send selected-chip print-node (string->number node)))
      (printf "[No selected chip]")))

(def-command only (chip/node) "Apply future commands only to CHIP or NODE"
  (if (hash-has-key? name-to-chip chip/node)
      (begin
        (set! selected-chip (hash-ref name-to-chip chip/node))
        (set! selected-node #f))
      (let ((coord (string->number chip/node)))
        (if coord
            (if selected-chip
                (set! selected-node (send selected-chip coord->node coord))
                (printf "must select a chip before selecting a node\n"))
            (printf "unknown chip name or node coordinate '~a'\n" chip/node)))))

(def-command all () "Apply future commands to all chips"
  (set! selected-chip #f))

(define (get-help-string command)
  (if (hash-has-key? _help command)
      (string-append (format "~a command usage:\n    " command)
                     (string-join (hash-ref _help command) "\n    "))
      (format "can't find '~a'" command)))

(define (test)
  (printf "test success!\n"))

(define (enter-cli)

  (define again #t)
  (def-command exit () "Exit this cli" (set! again #f))
  (define last-command #f)
  (define (loop)
    (printf ">>> ")
    (let* ((input (read-line stdin)))
      (when (or (eof-object? input)
                (= (string-length input) 0))
        (set! input last-command)
        (pretty-display (format "[~a]" input)))
      (when input
        (let* ((split (string-split (string-trim input)))
               (len (length split))
               (command (and (> len 0) (car split)))
               (args (and (> len 1) (cdr split))))
          (if (eq? (string-ref input 0) #\()
              ;;TODO: fix
              (pretty-display (eval (read (open-input-string input))))

              (let ((command-name (string-append command
                                                 (number->string (sub1 len)))))
                (if (hash-has-key? _commands command-name)
                    ((hash-ref _commands command-name) args)
                    (printf "unkown command '~a', arity ~a\n"
                            command (sub1 len))))))
        (set! last-command input)))
    (and again (loop)))
  (loop))
