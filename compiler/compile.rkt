#lang racket

;;references:
;; - colorforth blocks 190-192,1404-1412
;; - DB001 F18A Technology Reference
;; - DB004 arrayForth User's Manual, section 5


(require "read.rkt"
         "assemble.rkt"
         "disassemble.rkt"
         "../common.rkt"
         "../rkt-to-el.rkt"
         )

(provide compile compile-file display-compiled)

(define DEBUG? false)

(define nodes false) ;;node# -> code struct
(define used-nodes '())

(define last-inst false)
;;coordinate of the current node we are compiling for
(define current-node-coord false)
(define current-node-consts false) ;; maps constant name to values
;;struct of the current node we are compiling for
(define current-node false)

(define stack '())

(define extended-arith 0);;0x200 if extended arithmetic is enabled, else 0

(define current-tok-line 0)
(define current-tok-col 0)
(define prev-current-tok-line 0)
(define prev-current-tok-col 0)

;;list of mconses containing word addresses and call/jump address
(define address-cells false)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; result of (parse-code), a list of nodes: (node-num wordlist1 wordlist2...)
(define parsed-nodes false)
(define parsed-words false)
;; the list of instructions we are currently compiling (body of the current word)
(define current-token-list false)

(define (compile in)
  ;;IN is a port, filename or list of parsed nodes in (parse-code) format
  (when DEBUG? (printf "DEBUG PRINT MODE\n"))
  (reset!)
  (define port false)
  (cond ((string? in)
         (set! port (open-input-string in)))
        ((list? in)
         (set! parsed-nodes in))
        (else (set! port in)))
  (when port
    (current-input-port port)
    (set! parsed-nodes (parse-code)))

  (when reorder-words-with-fallthrough
    (set! parsed-nodes (for/list ((node parsed-nodes))
                         (cons (car node) (reorder-with-fallthrough (cdr node)))))
    (when t
      (with-output-to-file "opt.aforth"
        (lambda ()
          (for ((node parsed-nodes))
            (printf "\nnode ~a\n" (car node))
            (for ((word (cdr node)))
              (printf "~a\n" (string-join (map token-tok word))))))
        #:exists 'replace)))


  (for ((node parsed-nodes))
    (start-new-node (car node))
    (set! parsed-words (cdr node))
    (for ((word parsed-words))
      (set! current-token-list word)
      (while (not (null? current-token-list))
        (compile-token (read-tok))
               )))
  (when memory
    (fill-rest-with-nops) ;;make sure last instruction is full
    (set-node-len! current-node (sub1 next-addr)))

  (when DEBUG? (display-compiled (compiled used-nodes)))

  ;; errors from this point on are not associated with line numbers
  (set! current-tok-line false)
  (set! current-tok-col false)

  (map check-for-undefined-words used-nodes)

  (compiled (map remove-address-cells used-nodes)))

(define (compile-file file)
  (call-with-input-file file compile))

(define (reorder-with-fallthrough words)
  (define fallthroughs (make-hash))
  (define name->word (make-hash))
  (define word false)
  (define setup-code false)
  (unless (equal? (token-tok (caar words)) ":")
    (set! setup-code (car words))
    (set! words (cdr words)))
  (define names (map (lambda (x) (token-tok (cadr x))) words))
  (define output '())
  (define tail-word false)
  ;;collect fall-through pairs
  (for ((word words))
    (hash-set! name->word (token-tok (cadr word)) word)
    (let* ((rword (reverse word))
           (ret (token-tok (car rword)))
           (name (token-tok (cadr rword))))
      (hash-set! fallthroughs (token-tok (cadr word))
                 (if (and (equal? ret ";")
                          (member name names)
                          ;; protect against recursive calls
                          (not (equal? name (token-tok (cadr word)))))
                     name false))))
  (let ((x (filter (lambda (x) (cdr x)) (hash->list fallthroughs))))
    (unless (or (null? x) t)
      (pretty-display "fallthroughs:")
      (pretty-display x)))
  (define fall-into-nodes (hash-values fallthroughs))
  (for ((node fall-into-nodes))
    (set! names (remove node names)))
  (set! names (append names (filter (lambda (x) x)
                                    (set->list (list->set fall-into-nodes)))))

  (define (pop)
    (define x (car names))
    (set! names (cdr names))
    x)

  (define (f [x false])
    (define name (or x (pop)))
    (define ft (hash-ref fallthroughs name))
    (define body (hash-ref name->word name))
    (when (and ft (member ft names))
      (set! body (reverse (cddr (reverse body))))) ;;remove tailcall
    (set! output (cons body output))
    (set! names (remove name names))
    (when (and ft (member ft names))
      (f ft)))

  (while (not (null? names))
    (f))

  (if setup-code
      (cons setup-code (reverse output))
      (reverse output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;info about the current target core
(define memory false) ;;vector of words
(define current-addr false);;index of current word in memory
(define current-word false);; tail of current word list
(define next-addr false) ;;index of next word in memory
(define current-slot false);;index of the next slot in current-word
(define words false) ;;word definitions -> addresses
(define rom false) ;; ROM word definitions -> addresses

(define (add-word! name addr)
  (define cell (make-new-address-cell addr name))
  (set-node-symbols! current-node
                     (cons (symbol name addr current-tok-line current-tok-col)
                           (node-symbols current-node)))
  (hash-set! words name cell))

(define waiting (make-hash));;word -> list of cells waiting for the word's address
(define (add-to-waiting word addr-cell)
  (unless (and (hash-has-key? waiting word)
               (hash-ref waiting word))
    (hash-set! waiting word (list)))
  (hash-set! waiting word (cons addr-cell (hash-ref waiting word))))
(define (get-waiting-list word)
  (and (hash-has-key? waiting word)
       (hash-ref waiting word)))
(define (waiting-clear word)
  (hash-set! waiting word false))

(define (make-addr addr)
  (bitwise-ior addr extended-arith))

(define io-places-hash (make-hash))
(for ([place io-places])
  (hash-set! io-places-hash (car place) (make-addr (cdr place))))

;;TODO: initial scan should resolve tail calls and collect word names
(define (get-word-address name)
  (and DEBUG? (printf "       get-word-address(~a)\n" name))
  (define addr (or (and (hash-has-key? words name)
                        (hash-ref words name))
                   (and (hash-has-key? io-places-hash name)
                        (hash-ref io-places-hash name))
                   (and (hash-has-key? rom name)
                        (hash-ref rom name))))
  (and DEBUG? (printf "          addr = ~a\n" addr))
  (when (not addr)
    ;; if name is not found locally, check if it is a remote call
    (let ((x (remote-call? name)))
      (and DEBUG? (printf "          x = ~a\n" addr))
      (set! addr (and x (get-remote-addr (car x) (cdr x))))
      (and DEBUG? (printf "          (global)addr = ~a\n" addr))))
  (if (mpair? addr) (mcar addr) addr))


(define (instruction? token)
  (set-member? opcode-set token))

(define (word-ref? token)
  (and (string? token)
       (> (string-length token) 1)
       (eq? (string-ref token 0) #\&)))

(define (remote-call? token)
  ;; return (NAME . NODE) for tokens with form "NAME@NODE"
  (define m (and (string? token)
                 (regexp-match #rx"^(.+)@([0-9]+)$" token)))
  (and m
       (= (length m) 3)
       (cons (cadr m) (string->number (caddr m)))))

;; compiler directive - words executed at compile time
(define directives (make-hash));;directive names -> functions
(define (add-directive! name code)
  (hash-set! directives name code))
(define (get-directive name)
  (and (hash-has-key? directives name)
       (hash-ref directives name)))

(define (reset!)
  (set! nodes (make-vector num-nodes false))
  (for ([i num-nodes])
    (vector-set! nodes i (create-node (index->coord i)
                                      (list->vector (for/list ([_ num-words])
                                                      (make-vector 4 false))))))
  (set! used-nodes '())
  (set! last-inst false)
  (set! stack '())
  (set! memory false)
  (set! current-word false)
  (set! current-addr false)
  (set! next-addr false)
  (set! words (make-hash))
  (set! rom false)
  (set! waiting (make-hash))
  (set! prev-current-tok-line 0)
  (set! prev-current-tok-col 0)
  (define-named-addresses!))

(define (read-tok)
  (if (null? current-token-list)
      false
      (let ((token (car current-token-list)))
        (set! current-token-list (cdr current-token-list))
        (when token
          (set! prev-current-tok-line current-tok-line)
          (set! prev-current-tok-col current-tok-col)
          (set! current-tok-line (token-line token))
          (set! current-tok-col (token-col token)))
        token)))

(define (unread-tok tok)
  (set! current-token-list (cons (token tok current-tok-line current-tok-col)
                                 current-token-list))
  (set! current-tok-line prev-current-tok-line)
  (set! current-tok-col prev-current-tok-col))

(define (read-tok-name)
  (define tok (read-tok))
  (and tok (token-tok tok)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define current-token false)

(define (compile-token token)
  (set! current-token (token-tok token))
  (when DEBUG? (printf "compile-token(~a) [~a  ~a  ~a]\n"
                       (token-tok token) current-addr current-slot next-addr))
  (let ((x false)
        (tok (if (token? token)
                 (token-tok token)
                 token)))
    (cond ((setq x (get-directive tok)) (x))
          ((instruction? tok) (compile-instruction! tok))
          ((setq x (parse-num tok)) (compile-constant! x))
          ((word-ref? tok) (compile-word-ref! (substring tok 1)))
          ((setq x (remote-call? tok)) (compile-remote-call! (car x) (cdr x)))
          ((node-const? tok) (compile-node-const tok))
          (else (compile-call! tok)))
    (set! current-token false)
    tok))

(define (org n)
  (when DEBUG? (printf "        org(~a)\n" n))
  (set! current-addr n)
  (set! next-addr (add1 n))
  (set! current-word (vector-ref memory n))
  (set! current-slot 0))

(define (goto-next-word) (org next-addr))

(define (add-to-next-slot inst)
  ;;this assumes that we are not going to be overwriting code
  (when DEBUG? (printf "        add-to-next-slot(~a)\n" inst))
  (unless current-word (error "You probably forgot to use 'node' first"))
  (vector-set! current-word current-slot inst)
  (set! current-slot (add1 current-slot))
  (when (= current-slot 4)
    (goto-next-word))
  (set! last-inst inst))

(define (compile-instruction! inst)
  (when DEBUG? (printf "    compile-instruction!(~a)\n" inst))
  (when (and auto-nop-insertion
             (member inst instructions-preceded-by-nops)
             (not (equal? last-inst ".")))
    (add-to-next-slot "."))
  (when (and (equal? current-slot 3)
             (not (member inst last-slot-instructions)))
    (add-to-next-slot "."))
  (add-to-next-slot inst)
  (when (member inst instructions-using-rest-of-word)
    (fill-rest-with-nops)))

(define (compile-constant! const)
  (when DEBUG? (printf "    compile-constant!(~a)\n" const))
  (if (and (= const 0)
           compile-0-as-dup-dup-or)
      (begin (compile-instruction! "dup")
             (compile-instruction! "dup")
             (compile-instruction! "or"))
      (begin
        (compile-instruction! "@p")
        (set-next-empty-word! const))))

(define (compile-call! word [address false])
  (when DEBUG? (printf "    compile-call!(~a)\n" word))
  (define (compile-call-or-jump)
    (let ((next (read-tok-name)))
      (when (equal? current-slot 3)
        (fill-rest-with-nops))
      (if (and next (equal? next ";"))
          (add-to-next-slot "jump")
          (begin (add-to-next-slot "call")
                 (and next (unread-tok next))))))
  (let* ((compiler-word-p (compiler-word? word))
         (addr (and (not compiler-word-p)
                    (or address (get-word-address word))))
         (cell false))
    (if compiler-word-p
        (exec-compiler-word word)
        (if addr
            (begin
              (unless (address-fits? (if (mpair? addr) (mcar addr) addr) current-slot)
                (fill-rest-with-nops))
              (when DEBUG? (printf "       address = ~a\n" addr))
              (compile-call-or-jump)
              (add-to-next-slot (if (mpair? addr)
                                    addr
                                    (make-new-address-cell addr word)))
              (unless (= current-slot 0)
                (goto-next-word)))
            ;;else
            (begin
              (when DEBUG? (printf "       waiting on address....\n"))
              (compile-call-or-jump)
              (set! cell (make-new-address-cell false word))
              (add-to-waiting word cell)
              (add-to-next-slot cell)
              (unless (= current-slot 0)
                (goto-next-word)))))))

;;TODO:
;; support for calling remote words in nodes that are defined later in the program

(define (get-remote-addr word coord)
  (define node (vector-ref nodes (coord->index coord))) ;;TODO: validate COORD
  (define words (node-word-dict node))
  (if words
      (if (hash-has-key? words word)
          (hash-ref words word)
          (error (format "remote word not found: ~a@~a (called from node ~a)"
                         word coord current-node-coord)))
      (error (format "can't find dictionary for node: ~a" coord))))

(define (compile-remote-call! word coord)
  (when DEBUG? (printf "     compile-remote-call!(~a, ~a)\n" word coord))
  (define addr (get-remote-addr word coord))
  (when DEBUG? (printf "        addr = ~a\n" addr))
  (compile-call! word addr))

(define (compile-word-ref! word)
  (let ((addr (get-word-address word)))
    (unless addr
      (raise (format "[TODO] reference to undefined word: ~a" word)))
    (compile-constant! addr)))

(define (fill-rest-with-nops)
  (unless (= current-slot 0)
    (add-to-next-slot ".")
    (fill-rest-with-nops)))

(define (set-next-empty-word! word)
  (if (= current-slot 0)
      (begin (vector-set! memory current-addr word)
             (org next-addr))
      (begin (vector-set! memory next-addr word)
             (set! next-addr (add1 next-addr)))))

(define (make-new-address-cell val [name false])
  ;; name is an optional tag, usually the name of the word, that discribes the address.)
  ;; it is use for debug only
  (define cell (mcons val (mcons next-addr (mcons name null))))
  (set! address-cells (set-add address-cells cell))
  cell)

(define (check-for-undefined-words node)
  (define addr-cells (node-address-cells node))
  (for ((word (node-mem node)))
    (when (vector? word)
      (when (vector? word)
        (for ((slot word))
          (when (and (mpair? slot)
                     (not (mcar slot))
                     (mcar (mcdr (mcdr slot))))
            (error (format "Undefined word: '~a' in node ~a" (mcar (mcdr (mcdr slot)))
                           (node-coord node)))))))))

(define (remove-address-cells node)
  ;; unwrap mcons address cells
  ;; shift down words to make room if address is to large
  (define mem (node-mem node))
  (define addr false)
  (define call-inst false)
  (define new-word-index false)
  (define addr-cells (node-address-cells node))
  (define (count word)
    (define n 0)
    (for ((inst word))
      (when (equal? inst "@p")
        (set! n (add1 n))))
    n)

  (for ((word mem)
        (word-index (vector-length mem)))
    (when (vector? word)
      (for ((slot word)
            (slot-index 4))
        (when (mpair? slot)
          (set! addr (mcar slot))
          (when (not addr)
            (error (format "remove-address-cells -- invalid address '~a' for '~a'" addr (mcar (mcdr (mcdr slot))))))

          (if (not (address-fits? addr (sub1 slot-index) (mcar (mcdr slot))))
              (begin
                (set! new-word-index (+ word-index 1 (count word)))
                (shift-words-down mem new-word-index)
                (increment-address (node-address-cells node) new-word-index)
                (set! call-inst (vector-ref word (sub1 slot-index)))
                (unless (member call-inst '("call" "jump" "next" "-if" "if"))
                  (error (format "invalid call instruction: '~a'" call-inst)))
                ;; remove call from old word
                (vector-set! word (sub1 slot-index) ".")
                (vector-set! word slot-index ".")
                ;; create and set call in new word
                (set! word (vector call-inst (add1 addr) "." "."))
                (vector-set! mem new-word-index word)
                )
              (vector-set! word slot-index addr))))))
  node)

(define (increment-address address-cells from)
  (for ((cell address-cells))
    (when (>= (mcar cell) from)
      (set-mcar! cell (add1 (mcar cell))))
    (when (>= (mcar (mcdr cell)) from)
      (set-mcar! (mcdr cell) (add1 (mcar (mcdr cell)))))))

(define (shift-words-down memory from)
  (raise "shift-words-down ~a" current-node-coord)
  ;;(printf "(shift-words-down ~a)\n" from)
  (set! current-addr (add1 current-addr))
  (set! next-addr (add1 next-addr))
  (set! current-word (vector-ref memory current-addr))
  (for ((i (reverse (range from next-addr))))
    (vector-set! memory (add1 i) (vector-ref memory i))))

;; map jump instruction slots to bit masks for their address fields
(define address-masks (vector #x3ff #xff #x7))

(define (address-fits? destination-addr jump-slot [P false])
  ;; returns t if DESTINATION-ADDR is reachable from the current word
  ;; JUMP-SLOT is the slot of the jump/call instruction
  (set! P (or P next-addr))
  (and jump-slot
       (>= jump-slot 0)
       (< jump-slot 3)
       (let* ((mask (vector-ref address-masks jump-slot))
              (~mask (& (~ mask) #x3ffff))
              (min-dest (& ~mask P))
              (max-dest (ior (& ~mask P) (& mask destination-addr))))
         (and (>= destination-addr min-dest)
              (<= destination-addr max-dest)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-directive!
 eof
 (lambda ()
   (fill-rest-with-nops)))

(define (comment)
  (unless (equal? (forth-read-char) #\))
    (comment)))
(add-directive! "(" comment)

(add-directive!
 ":"
 (lambda ()
   (fill-rest-with-nops)
   (let* ((word (read-tok-name))
          (address (make-addr current-addr))
          (waiting-list (get-waiting-list word)))
     (when waiting-list
       (for [(cell waiting-list)]
         (set-mcar! cell address))
       (waiting-clear word))

     (when (hash-has-key? words word)
       (error (format "redefinition of word '~a' in node ~a"
                      word current-node-coord)))
     (when (equal? word "main")
       (if (node-p current-node)
           (error (format "use of /p overrides 'main' in node ~a\n"
                          current-node-coord))
           (set-node-p! current-node (make-addr current-addr))))
     (add-word! word address))))


;;forces word alignment
(add-directive!
 ".."
 (lambda () (fill-rest-with-nops)))

(add-directive! ;; page 23 of arrayforth users manual DB004
 ","
 (lambda ()
   (let* ([token (read-tok-name)]
          [data (parse-num token)])
     (if (not data)
         (error (format "invalid token: ~a" token))
         (set-next-empty-word! data)))))

(define (start-new-node coord)
  (when memory ;;make sure last instruction is full
    (fill-rest-with-nops)
    (set-node-len! current-node (sub1 next-addr)))
  (define index (coord->index coord))
  ;;TODO: validate 'node'
  ;;assert (node-coord node) == coord
  (set! current-node (vector-ref nodes index))
  (set! memory (node-mem current-node))
  (set! words (node-word-dict current-node))
  (set! address-cells (node-address-cells current-node))
  (set! rom (get-node-rom coord))
  ;;TODO: should calling 'node' multiple times be ok?
  ;;      if so, don't add current-node to used-nodes again
  (set! used-nodes (cons current-node used-nodes))
  (set! current-node-coord coord)
  (set! current-node-consts (node-consts current-node))
  (org 0))

;;node (nn)
;;starts compilation for the given node with number in yyxx notation
(add-directive!
 "node"
 (lambda ()
   (define coord (read-tok-name))
   ;;TODO: validate coord
   (define x (parse-num coord))
   (if (not (null? x))
       (start-new-node )
       (error (format "invalid noe number: ~a" x)))))

;;+cy
;;forces word alignment then turns P9 on in the location counter. Places in memory
;;subsequently defined will be run in Extended Arithmetic Mode if reached by
;;jump, call, execute or return to those places.
(add-directive!
 "+cy"
 (lambda ()
   (fill-rest-with-nops)
   (set! extended-arith #x200)))

;;-cy
;;forces word alignment then turns P9 off in the location counter
(add-directive!
 "-cy"
 (lambda ()
   (fill-rest-with-nops)
   (set! extended-arith 0)))

(define (here)
  (fill-rest-with-nops)
  (push stack (make-addr current-addr)))

;;here (-n)
;;forces word alignment and pushes current aligned location onto compiler stack
(add-directive! "here" here)
;;begin (-a)
;;forces word alignment and saves here to be used as a transfer destination.
(add-directive! "begin" here)

;;for (-a) (n)
;;pushes n onto the return stack, forces word alignment and saves 'here' to be
;;used as a transfer destination by the directive that ends the loop.
;;There are times when it is useful to decompose this directive's actions so
;;that the pushing of the loop count and the start of the loop itself may be
;;separated by such things as initialization code or a word definition. In this
;;case you may write "push <other things> begin".
(add-directive!
 "for"
 (lambda ()
   (compile-instruction! "push")
   (here)))

(define (compile-next-type inst)
  (let ((addr (pop stack)))
    (unless (address-fits? addr current-slot)
      (fill-rest-with-nops))
    (add-to-next-slot inst)
    (add-to-next-slot (make-new-address-cell addr inst))
    (unless (= current-slot 0)
      (goto-next-word))))

;;next (a)
;;ends a loop with conditional transfer to the address a. If R is zero when next
;;is executed, the return stack is popped and program flow continues. Otherwise
;;R is decremented by one and control is transferred to a.
(add-directive! "next" (lambda () (compile-next-type "next")))
;; end (a)
;; unconditionally jumps to a
(add-directive! "end" (lambda () (compile-next-type "jump")))
;;until (a)
;;If T is nonzero, program flow continues; otherwise jumps to a.
;;Typically used as a conditional exit at the end of a loop.
(add-directive! "until" (lambda () (compile-next-type "if")))
;;-until (a)
;;If T is negative, program flow continues; otherwise jumps to a. Used like 'until'
(add-directive! "-until" (lambda () (compile-next-type "-if")))

;;unext (a)
;;ends a micronext loop. Since the loop occurs entirely within a single
;;instruction word, the address is superfluous; it is present only so that the
;;form "<n> for ... unext" may be written. The micronext opcode may be compiled
;;into any of the four slots.
(add-directive!
 "unext"
 (lambda ()
   (compile-instruction! "unext")
   (pop stack)))

;;*next (ax-x)
;;equivalent to 'swap next'
(add-directive!
 "*next"
 (lambda ()
   (swap stack)
   (compile-next-type "next")))

(define (compile-if-instruction inst)
  ;;cannot be in last word.
  (when (and (equal? current-slot 3)
             (not (member inst last-slot-instructions)))
    (add-to-next-slot "."))
  (add-to-next-slot inst)
  (push stack (make-addr current-addr))
  (goto-next-word))

;;If T is nonzero, program flow continues; otherwise jumps to matching 'then'
(define (if-directive)
  (compile-if-instruction "if"))
(add-directive! "if" if-directive)

(define (-if-directive)
  (compile-if-instruction "-if"))

;;if (-r)
;;If T is negative, program flow continues; otherwise jumps to matching 'then'
(add-directive! "-if" -if-directive)

;;zif (-r)
;;If R is zero, pops the return stack and program flow continues;
;;otherwise decrements R and jumps to matching 'then'
(add-directive!
 "zif"
 (lambda ()
   (compile-if-instruction "next")))

;;ahead (-r)
;;jumps to matching 'then'
(add-directive!
 "ahead"
 (lambda ()
   (compile-if-instruction "jump")))

;;leap (-r)
;;compiles a call to matching 'then'
(add-directive!
 "leap"
 (lambda ()
   (compile-if-instruction "call")))

(define (add-to-slot slot thing)
  (define (find-first-empty word [n 0])
    ;; find the first empty slot in WORD
    (if (< n 4)
        (if (vector-ref word n)
            (find-first-empty word (add1 n))
            n)
        false))
  (define max-slot-num (vector 262144 8192 256 8))
  (let* ((word (vector-ref memory slot))
         (last (and (vector? word) (find-first-empty word))))
    (if last
        (if (and (not (mpair? thing))
                 (> thing (vector-ref max-slot-num last)))
            ;; TODO: move instruction to next word in this case
            (error (format "'~a' cannot fit into slot ~a" thing last))
            (vector-set! word last thing))
        (error (format "add-to-slot -- slot ~a ~a: ~a"
                       slot
                       (if (vector? word)
                           "is not an instruction word"
                           "is full")
                       word)))))

;;then (r)
;;forces word alignment and resolves a forward transfer.
(add-directive!
 "then"
 (lambda ()
   (fill-rest-with-nops)
   (add-to-slot (pop stack) (make-new-address-cell current-addr "then"))))

;;org (n)
;;sets the compiler's location counter to a given address at
;;which following code will be compiled into
(add-directive!
 "org"
 (lambda ()
   (let ([n (parse-num (read-tok-name))])
     ;;TODO: validate n
     (unless n (error "invalid address for 'org'"))
     (org n))))

;;while (x-rx)
;;equivalent to 'if swap'. Typically used as a conditional exit from within a loop
(add-directive!
 "while"
 (lambda ()
   (if-directive)
   (swap stack)))

;;-while (x-rx)
;;equivalent to '-if swap'. Typically used as a conditional exit from within a loop
(add-directive!
 "-while"
 (lambda ()
   (-if-directive)
   (swap stack)))

;;' (-a)
;;(tick) places the address of an F18 red word on the compiler's stack.
(add-directive!
 "`"
 (lambda ()
   (let* ((word (read-tok-name))
          (addr (get-word-address word)))
     (if addr
         (push stack addr)
         (error (format "\"~a\" is not defined" word))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boot descriptors
;; DB004 section 5.5.1

(define (set-register-helper name set-fn)
  (unless current-node
    (raise (format "must select node before '~a'" name)))
  (let* ((n (read-tok-name))
         (addr (or (get-address n current-node-coord)
                   (get-word-address n))))
    (unless addr
      (raise (format "unknown address for compiler directive '~a': ~a" name n)))
    (set-fn current-node addr)))

;; /b (a)
;; Specifies an initial value for register B.
;; Default value is the address of the IO register, as at reset.
(add-directive!
 "/b"
 (lambda ()
   (set-register-helper "/b" set-node-b!)))
;; /a (n)
;; Specifies an initial value for register A.
;; Default value is unspecified, as at reset.
(add-directive!
 "/a"
 (lambda ()
   (set-register-helper "/a" set-node-a!)))

;;/io (n)
;;Specifies a value to be loaded into the IO register.
(add-directive!
 "/io"
 (lambda ()
   (set-register-helper "/io" set-node-io!)))

;;/p (a)
;;Specifies an initial value for register P. Default value is xA9 which is
;;the routine warm in every node's ROM.
(add-directive!
 "/p"
 (lambda ()
   (set-register-helper "/p" set-node-p!)))

;; /stack n <n values>
;; Specifies up to ten values to be pushed onto the data stack, with the
;; rightmost value on top. For example 30 20 10 3 /stack produces the same
;; effect as though a program had executed code 30 20 10
(add-directive!
 "/stack"
 (lambda ()
   (let* ((tok (read-tok-name))
          (len (and tok (string->number tok)))
          (stack '())
          (val false))
     (when (or (not len)
               (< len 0)
               (> len 10))
       (raise (format "invalid number for /stack item count: '~a'" len)))

     (while (> len 0)
       (begin
         (set! tok (read-tok-name))
         (set! val (and tok (string->number tok)))
         (when (and (not val)
                    (not (setq val (get-word-address tok))))
           (raise (format "invalid stack value: ~a" tok)))
         (push stack val)
         (set! len (sub1 len))))
     (set-node-stack! current-node (reverse stack)))))

(add-directive!
 "swap!"
 (lambda ()
   (swap stack)))

;;NOTE: +node, /ram, and /part are not supported

(define (define-const name val)
  (when (hash-has-key? current-node-consts name)
    (error (format "redefining node const '~a'" name)))
  (hash-set! current-node-consts name val))

(define (node-const? name)
  (hash-has-key? current-node-consts name))
(define (compile-node-const name)
  (unless (hash-has-key? current-node-consts name)
    (error (format "node const not found'~a'" name)))
  (compile-constant! (hash-ref current-node-consts name)))

(define const-ops (make-hash `(("+" . ,+)
                               ("or" . ,bitwise-xor)
                               )))

(define (lookup-const-value x)
  ;; parses x as a number or looks it up
  (or (parse-num x)
      (and (node-const? x)
           (hash-ref current-node-consts x))))

(add-directive!
 "const"
 (lambda ()
   (define (read-apply-op name op-name)
     (define op (hash-ref const-ops op-name))
     (define left-tok (read-tok-name))
     (define left (lookup-const-value left-tok))
     (unless left
       (error (format "invalid const param for op ~a: '~a'" op-name left-tok)))
     (define right-tok (read-tok-name))
     (define right (lookup-const-value right-tok))
     (unless right
       (error (format "invalid const param for op ~a: '~a'" op-name right-tok)))
     (define-const name (bitwise-and (apply op (list left right)) #x3ffff)))

   (define name (read-tok-name))
   (define op (read-tok-name))
   (define op-n (string->number op))

   (cond (op-n (define-const name op-n))
         ((hash-has-key? const-ops op)
          (read-apply-op name op))
         (else (error (format "invalid const op type: '~a'" op))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(for [(dir (list "north" "south" "east" "west"))]
  (add-directive!
   dir
   ((lambda (dir)
      (lambda () ((get-directive (convert-direction current-node-coord dir)))))
    dir)))


(define (define-named-addresses!)
  (for ([addr named-addresses])
    (add-directive!
     (car addr)
     ((lambda (a) (lambda () (compile-constant! a))) (cdr addr)))))


(define (error msg)
  (printf "ERROR")
  (when (and current-tok-line current-tok-col)
    (printf (format "[~a:~a]" current-tok-line current-tok-col)))
  (printf (format " ~a\n" msg))
  (when current-token
    (printf "  (while compiling token '~a')\n" current-token))
  (exit 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler words

;; list of words that are executed by the compiler instead of compiled for the f18a
(define compiler-words (make-hash))

(define compiler-ops (make-hash))

(define (add-compiler-word! name body)
  (pretty-display (format "adding compiler word: '~a' = " name))
  (pretty-display body)
  (hash-set! compiler-words name body))

(define (def-compiler-op! name fn)
  (hash-set! compiler-ops name fn))

(define (compiler-binop op)
  (push stack (op (pop stack) (pop stack))))

(define (compiler-unop op)
  (push stack (op (pop stack))))

(def-compiler-op! "+" (lambda () (compiler-binop +)))
(def-compiler-op! "sub" (lambda () (compiler-binop -)))
(def-compiler-op! "*" (lambda () (compiler-binop *)))
(def-compiler-op! "/" (lambda () (compiler-binop /)))
(def-compiler-op! "or" (lambda () (compiler-binop bitwise-xor)))
(def-compiler-op! "ior" (lambda () (compiler-binop bitwise-ior)))
(def-compiler-op! "-" (lambda () (compiler-unop bitwise-not)))

(def-compiler-op!
  "dup"
  (lambda ()
    (push stack (car stack))))

(def-compiler-op!
  "lit"
  (lambda ()
    (define n (pop stack))
    ;;assert number
    (compile-constant! n)))

(define (exec-compiler-word word)
  (pretty-display (format "Execing compiler word ~a" word))
  (let ((body (hash-ref compiler-words word)))
    (define i false)
    (define (exec-body body)
      (unless (null?  body)
        (define i (car body))
        (if (number? i)
            (push stack i)
            ((hash-ref compiler-ops i)))
        (exec-body (cdr body))))
    (exec-body body)))

(define (compiler-word? word)
  (hash-has-key? compiler-words word))

(add-directive!
 "::"
 (lambda ()
   (let* ((name (read-tok-name))
          (body '())
          (tok false)
          (x false))
     ;; Allow redefinition of compiler words
     ;;  (when (hash-has-key? compiler-words word)
     ;;    (error (format "redefinition of compiler word '~a' in node ~a"
     ;;                   word current-node-coord)))
     (define (read-body)
       (set! tok (read-tok-name))
       (unless (equal? tok ";")
         (cond ((setq x (parse-num tok))
                (set! body (cons x body)))
               ;;TODO:DOING: doe snot  recognize 'lit' is it defined? are hte string key comparison working?
               ((hash-has-key? compiler-ops tok)
                (set! body (cons tok body)))
               (else (error (format "Unsupported compiler word op: ~a" tok))))
         (read-body)))
     (read-body)
     (add-compiler-word! name (reverse body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-compiled compiled)
  ;;NODES is a list of node structs
  (define (display-word word [n 0])
    (if (number? word)
        (display word)
        (when (< n 4)
          (display (format "~a " (vector-ref word n)))
          (display-word word (add1 n)))))

  (define (display-mem mem [index 0])
    (let ([word (vector-ref mem index)])
      (unless (equal? word (vector false false false false))
        (display (format "~a    " index))
        (display-word word)
        (newline)
        (when (< index num-words)
          (display-mem mem (add1 index))))))

  (define (display-node nodes)
    (unless (null? nodes)
      (pretty-display (format "\nnode ~a" (node-coord (car nodes))))
      (display-mem (node-mem (car nodes)))
      (display-node (cdr nodes))))
  (display-node (compiled-nodes compiled)))
