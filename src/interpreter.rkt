#lang racket ;; -*- lexical-binding: t -*-
;; simulates a collection of ga144 chips

(require compatibility/defmacro
         "assemble.rkt"
         "disassemble.rkt"
         "ga144.rkt"
         "common.rkt"
         "compile.rkt"
         "el.rkt")

(provide (all-defined-out))

(define stdin (current-input-port))
(defvar _commands (make-hash))
(defvar _help (make-hash))
(defvar DEBUG false)
(setq DEBUG false)

(defvar chips '());;list of ga144 chips
(defvar num-chips 0)
(defvar name-to-chip (make-hash)) ;; maps chip names to chip objects
;; the currently selected chip. If not false, commands like
;; 'step' apply only to this chip. Otherwise they apply to all
;; chips in the 'chips' list
(defvar selected-chip false)
(defvar selected-node false)

(defvar _counter 1)

(defvar cli-active? false)

(defvar enter-cli-on-breakpoint? false)
(define (enter-cli-on-breakpoint x)
  (set! enter-cli-on-breakpoint? x))

(define (new-ga144 (name false))
  (unless name
    (set! name (rkt-format "chip~a" _counter))
    (set! _counter (add1 _counter)))
  (let ((chip (make-ga144 name t)))
    (push chip chips)
    (hash-set! name-to-chip name chip)
    (set! num-chips (add1 num-chips))
    chip))

(define (compile-and-load chip
                          in
                          (include-end-token? false)
                          #:compiled-file (compiled-file false)
                          #:assembled-file (assembled-file false))
  (let* ((n 0)
         (code 0)
         (node 0)
         (compiled (aforth-compile in)))
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

(define (step* (chip false))
  (define (step-all)
    (define again true)
    (define breakpoint? false)
    (while (and again
                (not breakpoint?))
      (begin
        (set! again false)
        (for ((c chips))
          (when (and (> (send c num-active-nodes) 0)
                     (not breakpoint?))
            (set! again t)
            (set! breakpoint? (send c step-program!))
            (when (and breakpoint?
                       (not cli-active?)
                       enter-cli-on-breakpoint?)
              (set! selected-chip c)
              (set! selected-node (send c get-breakpoint-node))
              (enter-cli)))))))
  (if chip
      (and (send chip step-program!*)
           (and (not cli-active?)
                enter-cli-on-breakpoint?)
           (enter-cli))
      (step-all)))

(define (step (n 1) (chip false))
  (if chip
      (and (send chip step-program-n! n)
           (and (not cli-active?)
                enter-cli-on-breakpoint?)
           (enter-cli))
      (for ((c chips))
        ;;TODO: stop when everything is suspended
        (and (send c step-program-n! n)
             (and (not cli-active?)
                  enter-cli-on-breakpoint?)
             (enter-cli)))))

(define (reset! (chip false))
  (if chip
      (send chip reset!)
      (for ((c chips))
        (send c reset!))))
(define (delete-all-chips)
  (set! chips '()))

(define (connect-pins from-node from-pin
                      to-node to-pin)
  (define (wire x)
    ;;    (printf "(WIRE ~a) ~a.~a<-->~a.~a\n" x
    ;;            (send from-node get-coord) from-pin
    ;;            (send to-node get-coord) to-pin)
    (send to-node set-pin! to-pin (= x 3)))
  (send from-node set-gpio-handler from-pin wire))

(define (get-node chip coord)
  (send chip coord->node coord))


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

(define (_step n chip)
  (if chip
      (send chip step-program-n! n)
      (for ((c chips))
        ;;TODO: stop when everything is suspended
        (send c step-program-n! n))))

(def-command step () "Step 1 word"
  (_step 1 selected-chip))

(def-command step (n) "Step N words"
  (_step (string->number n) selected-chip))

(def-command break (n) "Set breakpoint at word N"
  (printf "TODO\n"))

(def-command unbreak (n) "Remove breakpoint from word N"
  (printf "TODO\n"))

(define (_step* chip)
  (if chip
      (send chip step-program!*)
      (for ((c chips))
        (send c step-program!*))))

(def-command continue () "Continue execution"
  (if selected-chip
      (send selected-chip step-program!*)
      (for ((c chips))
        (send c step-program!*)))  )

(def-command reset () "Reset chip"
  (reset! selected-chip))

(def-command show () "Print a representation of the chip"
  (begin
    (if selected-chip
        (begin
          (send selected-chip print-active))
        (for ((chip chips))
          (printf "\nchip: ~a\n" (get-field name chip))
          (send chip print-active)))
    (when selected-node
      (send selected-node display-all))))

(def-command show (node) "Print a representation of NODE in selected chip"
  (if selected-chip
      (begin
        (send selected-chip print-node (string->number node)))
      (printf "[No selected chip]")))

(def-command only (chip/node) "Apply future commands only to CHIP or NODE"
  (if (hash-has-key? name-to-chip chip/node)
      (begin
        (set! selected-chip (hash-ref name-to-chip chip/node))
        (set! selected-node false))
      (let ((coord (string->number chip/node)))
        (if coord
            (if selected-chip
                (set! selected-node (send selected-chip coord->node coord))
                (printf "must select a chip before selecting a node\n"))
            (printf "unknown chip name or node coordinate '~a'\n" chip/node)))))

(def-command all () "Apply future commands to all chips"
  (begin (set! selected-chip false)
         (set! selected-node false)))

(define (is-yes-string str)
  (set! str (string-downcase str))
  (or (equal? str "yes")
      (equal? str "true")))

(def-command show-io-changes (on) "Print chip at every node suspension/wakeup"
  (if selected-chip
      (send selected-chip show-io-changes (is-yes-string on))
      (for ((c chips))
        (send c show-io-changes (is-yes-string on)))))

(def-command nactive () "print the number of active nodes"
  (if selected-chip
      (pretty-display (send selected-chip num-active-nodes))
      (for ((c chips))
        (printf "~a: ~a\n"
                (get-field name c)
                (send c num-active-nodes)))))

(define (break-wakeup chip coord)
  ;;break next time node coord wakes from suspended state
  ;;TODO: validate coord
  (let ((node (send chip coord->node coord)))
    (send node break-at-next-wakeup)))

(def-command break-wakeup (coord) "break next time node coord is awoken"
  (if selected-chip
      (break-wakeup selected-chip (string->number coord))
      (printf "Must select chip\n")))

(define (break-io-change chip coord)
  ;;break next time node coord wakes from suspended state
  ;;TODO: validate coord
  (let ((node (send chip coord->node coord)))
    (send node break-at-next-wakeup)))

(def-command break-io-change (chip coord)
  "break next time io changes in CHIP/COORD"
  (let ((node (send chip coord->node coord)))
    (send node break-at-next-io-change)))

(define (execute-instruction coord inst)
  (if selected-chip
      (if (set-member? opcode-set inst)
          (let ((opcode (vector-member inst opcodes)))
            (printf "opcode = ~a\n" opcode)
            (if (< opcode 8)
                (printf "~a is not supported\n" inst)
                (send (send selected-chip coord->node coord)
                      execute!
                      opcode)))
          (printf "TODO: push numbers onto node data stack"))
      (printf "Must select chip\n")))


(def-command ex (inst) "execute instruction in the selected node"
  (if selected-node
      (execute-instruction (send selected-node get-coord) inst)
      (printf "[Must select node]\n")))

(def-command ex (coord inst) "execute instruction in the COORD node"
  (execute-instruction coord inst))

(define (call-word coord word (args false))
  (if selected-chip
      (let ((node (send selected-chip coord->node coord)))
        (when args
          (for ((a (reverse args)))
            (send node d-push! a)))
        (send node call-word! word))
      (printf "Must select chip\n")))

(def-command : (name)  "call word NAME in the selected node"
  (if selected-node
      (call-word (send selected-node get-coord) name)
      (printf "[Must select node]\n")))

(def-command push (value)  "push VALUE onto the dstack of the selected node"
  (if selected-node
      (send selected-node d-push! value)
      (printf "[Must select node]\n")))


(def-command breakpoints () "print active breakpoints"
  (printf "TODO\n"))

(def-command dis (node) "disassemble all of NODE's memory"
  (if selected-chip
      (send selected-chip disassemble-memory (string->number node))
      (printf "Must select chip\n")))

(def-command dis-ram (node) "disassemble NODE's RAM"
  (if selected-chip
      (send selected-chip disassemble-memory (string->number node) 0 #x03F)
      (printf "Must select chip\n")))

(def-command dis-ram () "disassemble current-nodes RAM"
  (if selected-node
      (send selected-chip disassemble-memory (send selected-node get-coord) 0 #x03F)
      (printf "Must select node\n"))
  )

(def-command dis-rom (node) "disassemble NODE's ROM"
  (if selected-chip
      (send selected-chip disassemble-memory (string->number node) #x80 #xbf)
      (printf "Must select chip\n"))
  )

(def-command dis-rom () "disassemble current-nodes ROM"
  (if selected-node
      (send selected-chip disassemble-memory (send selected-node get-coord) #x80 #xbf)
      (printf "Must select node\n"))
  )

(def-command dis () "disassemble memory around P register in current-node"
  (if selected-node
      (send selected-chip disassemble-local (send selected-node get-coord))
      (printf "Must select node\n"))
  )

(def-command show-io (node) "display io register in detail"
  (if selected-chip
      (send (send selected-chip coord->node (string->number node))
            describe-io)
      (printf "Must select chip\n")))

(def-command show-io () "display io register of selected node in detail"
  (if selected-node
      (if selected-chip
          (send selected-node describe-io)
          (printf "Must select chip\n"))
      (printf "Must select node\n")))

(define (get-help-string command)
  (if (hash-has-key? _help command)
      (string-append (rkt-format "~a command usage:\n    " command)
                     (string-join (hash-ref _help command) "\n    "))
      (rkt-format "can't find '~a'" command)))

(define (test)
  (printf "test success!\n"))



(define (enter-cli)
  (set! cli-active? t)
  (when (and (= num-chips 1)
             (not selected-chip))
    (set! selected-chip (car chips)))
  ;;(set! again false)
  (define again t)
  (def-command exit () "Exit this cli" (set! again false))
  (define last-command false)

  (while again
    (begin
      (let* ((selected (if (and selected-chip
                                (> num-chips 1))
                           (list (get-field name selected-chip))
                           '()))
             (selected (if selected-node
                           (cons (rkt-format "~a" (send selected-node get-coord))
                                 selected)
                           false))
             (selected (if selected
                           (rkt-format "(~a)" (string-join (reverse selected) "."))
                           "")))
        (printf "~a>>> " selected))
      (let* ((input (read-line stdin)))
        (when (or (eof-object? input)
                  (= (string-length input) 0))
          (set! input last-command)
          (pretty-display (rkt-format "[~a]" input)))
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
          (set! last-command input)))))

  (set! cli-active? false))
