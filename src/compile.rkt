#lang racket ;; -*- lexical-binding: t  mode: emacs-lisp -*-

;;references:
;; - colorforth blocks 190-192,1404-1412
;; - DB001 F18A Technology Reference
;; - DB004 arrayForth User's Manual, section 5

(require "read.rkt"
         "assemble.rkt"
         "disassemble.rkt"
         "common.rkt"
         "el.rkt"
         )

(when elisp? (_def '(aforth-compile aforth-compile-file display-compiled)))

(provide aforth-compile aforth-compile-file display-compiled)

(define 64-word-mem true)
(define ignore-overwrite false) ;;used to keep going when some nodes overflow
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

;; if true create a mapping from compiled instructions to buffer positions
(define save-buffer-mappings false)
(define buffer-mappings false)
(define current-word-buffer-mapping false)
(define current-token-buffer-position false)

;; word and slot where the last instruction to be compiled was placed
(define last-instruction-word 0)
(define last-instruction-slot 0)

(when elisp?
  (set! save-buffer-mappings true))

(define (aforth-compile in)
  ;;IN is a port, filename or list of parsed nodes in (parse-code) format
  (assert (not elisp?))
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
    (set-current-node-length))

  (when DEBUG? (display-compiled (compiled used-nodes)))

  ;; errors from this point on are not associated with line numbers
  (set! current-tok-line false)
  (set! current-tok-col false)

  (map check-for-undefined-words used-nodes)

  (compiled (map remove-address-cells used-nodes)))

(define (aforth-compile-file file)
  (call-with-input-file file aforth-compile)
  )

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
      (printf "fallthroughs: ~a\n" x)))
  (define fall-into-nodes (hash-values fallthroughs))
  (for ((node fall-into-nodes))
    (set! names (remove node names)))
  (set! names (append names (filter (lambda (x) x)
                                    (set->list (list->set fall-into-nodes)))))

  (define (pop)
    (define x (car names))
    (set! names (cdr names))
    x)

  (define (f (x false))
    (assert (not elisp?)) ;;TODO: elisp support refactoring
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
(define current-word-i 0);;index of current-word in memory
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
  (ior addr extended-arith))

(define io-places-hash (make-hash))
(for ((place io-places))
  (hash-set! io-places-hash (car place) (make-addr (cdr place))))

;;TODO: initial scan should resolve tail calls and collect word names
(define (get-word-address name (coord false))
  (and DEBUG? (printf "       get-word-address(~a)\n" name))
  (define addr false)
  (if coord
      (set! addr (get-remote-addr name coord))
      (begin
        (set! addr (or (and (hash-has-key? words name)
                            (hash-ref words name))
                       (and (hash-has-key? io-places-hash name)
                            (hash-ref io-places-hash name))
                       (and (hash-has-key? rom name)
                            (hash-ref rom name))))
        (and DEBUG? (printf "          addr = ~a\n" addr))
        (when (not addr)
          ;;this is only needed for racket version support
          (let ((x (remote-call? name)))
            (and DEBUG? (printf "          x = ~a\n" addr))
            (set! addr (and x (get-remote-addr (car x) (cdr x))))
            (and DEBUG? (printf "          (global)addr = ~a\n" addr))))))

  (when (and (null? addr) bowman-format)
    (if (number? name)
        (setq addr name)
      (setq addr (string->number name))))

  (when (and (null? addr) ;; bowman-format forms like "jump east"
             (member name '("north" "south" "east" "west")))
    (setq addr (hash-ref names->addresses (convert-direction current-node-coord name))))

  (if (address-cell? addr)
      (address-cell-val addr)
      addr))

(define (instruction? token)
  (set-member? opcode-set token))

(define (word-ref? token)
  (and (string? token)
       (> (string-length token) 1)
       (eq? (string-ref token 0) _char-&)))

(define (remote-call? token)
  ;; return (NAME . NODE) for tokens with form "NAME@NODE"
  ;; regexp-match is not defined in elisp and remote calls are parsed separately
  ;; so just return false as a simple workaround
  (unless elisp?
    (define m (and (string? token)
                   (regexp-match (regexp "^(.+)@([0-9]+)$") token)))
    (and m
         (= (length m) 3)
         (cons (cadr m) (string->number (caddr m))))))

;; compiler directive - words executed at compile time
(define directives (make-hash));;directive names -> functions

(define op-docs '())

(define (op-doc name doc)
  (set! op-docs (cons (cons name doc) op-docs)))

(define (add-directive! name doc code)
  (when doc
    (op-doc name doc))
  (hash-set! directives name code))

(define (get-directive name)
  (and (hash-has-key? directives name)
       (hash-ref directives name)))

(define (create-memory-array)
  (make-vector num-words false))

(define (get-node coord)
  ;; returns the node for COORDinate, creating if it does not exist
  (let* ((index (coord->index coord))
         (node (vector-ref nodes index))
         (mem false)
         (buf-map false))

    (if node
        node
        (begin
          (set! mem (create-memory-array))
          (when save-buffer-mappings
            (set! buf-map (create-memory-array)))
          (set! node (create-node coord mem buf-map))
          (vector-set! nodes index node)
          node))))

(define (reset!)
  (set! nodes (make-vector num-nodes false))
  ;;(for ((i num-nodes))
  ;;  (vector-set! nodes i (create-node (index->coord i)
  ;;                                    (list->vector (for/list ((_ num-words))
  ;;                                                    (make-vector 4 false))))))
  (set! used-nodes '())
  (set! last-inst false)
  (set! stack '())
  (set! memory false)
  (set! buffer-mappings false)
  (set! current-word-buffer-mapping false)
  (set! current-token-buffer-position false)
  (set! current-word false)
  (set! current-word-i 0)
  (set! current-addr false)
  (set! next-addr false)
  (set! words (make-hash))
  (set! compiler-words (make-hash))
  (set! rom false)
  (set! waiting (make-hash))
  (set! current-tok-line 0)
  (set! current-tok-col 0)
  (set! prev-current-tok-line 0)
  (set! prev-current-tok-col 0)
  (set! current-token-list false)
  (set! address-cells '())
  (set! extended-arith 0)
  (set! current-node false)
  (set! extern-funcalls false)
  (set! last-instruction-word 0)
  (set! last-instruction-slot 0)
  (define-named-addresses!))

(define (read-tok)
  (assert (not elisp?))
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
  (assert (not elisp?))
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
  (assert (not elisp?))
  (when DEBUG? (printf "compile-token(~a) [~a  ~a  ~a]\n"
                       (token-tok token) current-addr current-slot next-addr))
  (let ((x false)
        (tok (if (token? token)
                 (token-tok token)
                 token)))
    (cond ((setq x (fn (get-directive tok))) (x))
          ((instruction? tok) (compile-instruction! tok))
          ((setq x (parse-num tok)) (compile-constant! x))
          ((word-ref? tok) (compile-word-ref! (substring tok 1)))
          ((setq x (remote-call? tok)) (compile-remote-call! (car x) (cdr x)))
          ((node-const? tok) (compile-node-const tok))
          (else (compile-call! tok)))
    (set! current-token false)
    tok))

(define (get-word n (verify-unused false) (no-overwrite false))
  (when DEBUG? (printf "get-word(~a, ~a)\n" n verify-unused))
  (when (or (< n 0)
            (> n num-words))
    (err (format "invalid memory index: %s" n)))
  (if save-buffer-mappings
      ;; (and save-buffer-mappings
      ;;      (null? (vector-ref buffer-mappings n)))
      (vector-set! buffer-mappings n (make-vector 4 false)))
  ;; ignore overwrite by always fetching a new word
  ;; sometimees, like when resolving backreferences, no-overwrite is used to
  ;; signal that the word must not be overwritten
  (let ((word (if (and ignore-overwrite (not no-overwrite)) false (vector-ref memory n))))
    (if (null? word)
        (progn
          (setq word (make-vector 4 false))
          (vector-set! memory n word)
          word)
      (when (and verify-unused
                 (not (equal? word (vector false false false false))))
        (err (format "overwriting word %s at address %s (node overflow?)" word n)))
      word)))

(define (org n)
  (when DEBUG? (printf "        org(~a)\n" n))
  (when 64-word-mem
    (set! n (remainder n #x40)))
  (set! current-addr n)
  (set! next-addr (if 64-word-mem
                      (remainder (add1 n) #x40)
                    (add1 n)))
  (set! current-word (get-word n true))
  (set! current-word-i n)
  (when save-buffer-mappings
    (set! current-word-buffer-mapping (vector-ref buffer-mappings n)))
  (set! current-slot 0))

(define (goto-next-word) (org next-addr))

(define (add-to-next-slot inst (save-mapping true))
  ;;this assumes that we are not going to be overwriting code
  (when DEBUG? (printf "        add-to-next-slot(~a)\n" inst))
  (unless current-word (err "You probably forgot to use 'node' first"))
  (unless (vector? current-word)
    (err (format "'current-word' is not a vector: %s" current-word)))
  (vector-set! current-word current-slot inst)
  (when (instruction? inst)
    (set! last-instruction-word current-word-i)
    (set! last-instruction-slot current-slot))
  (when (and save-buffer-mappings save-mapping)
    (vector-set! current-word-buffer-mapping current-slot current-token-buffer-position))
  (set! current-slot (add1 current-slot))
  (when (= current-slot 4)
    (goto-next-word))
  (set! last-inst inst))

(define (compile-instruction! inst)
  (when DEBUG? (printf "    compile-instruction!(~a)\n" inst))
  (when (and auto-nop-insertion
             (member inst instructions-preceded-by-nops)
             (not (equal? last-inst ".")))
    (add-to-next-slot "." false))
  (when (and (equal? current-slot 3)
             (not (member inst last-slot-instructions)))
    (add-to-next-slot "." false))
  (add-to-next-slot inst)
  (when (member inst instructions-using-rest-of-word)
    (fill-rest-with-nops)))

(define (compile-constant! const)
  (when DEBUG? (printf "    compile-constant!(~a)\n" const))
  (if bowman-format
      (set-next-empty-word! const)
    (if (and (eq? const 0)
             compile-0-as-dup-dup-or)
        (begin (compile-instruction! "dup")
               (compile-instruction! "dup")
               (compile-instruction! "or"))
      (begin
       (compile-instruction! "@p")
       (set-next-empty-word! const)))))

(define (get-call-instruction)
  ;; returns 'jump' if next instruction is ';', else 'call'
  (let ((next (read-tok-name)))
    (if (and next (equal? next ";"))
        "jump"
      (begin (when next
               (if elisp? (unread-last-tok)
                 (unread-tok next)))
             "call"
             ))))

(define (compile-call! word (address false))
  (when DEBUG? (printf "    compile-call!(~a)\n" word))
  (if (compiler-word? word)
      (exec-compiler-word word)
    (compile-transfer-instruction (get-call-instruction) word address)))

(define extern-funcalls false)

(define (compile-funcall! name)
  ;; inserts the function NAME into the extern-funcalls array
  ;; at the location of the last compiled instruction
  (unless extern-funcalls
    (set! extern-funcalls (make-vector 64 false))
    (set-node-extern-funcs! current-node extern-funcalls))

  (let ((word (vector-ref extern-funcalls last-instruction-word)))
    (unless word
      (set! word (make-vector 4 false))
      (vector-set! extern-funcalls last-instruction-word word))
    (vector-set! word last-instruction-slot  (cons (intern name) (vector-ref word current-slot)))))

;;TODO:
;; support for calling remote words in nodes that are defined later in the program

(define (get-remote-addr word coord)
  (define node (get-node coord)) ;;TODO: validate COORD
  (define words (node-word-dict node))
  (if words
      (if (hash-has-key? words word)
          (hash-ref words word)
          (err (rkt-format "remote word not found: ~a@~a (called from node ~a)"
                           word coord current-node-coord)))
      (err (rkt-format "can't find dictionary for node: ~a" coord))))

(define (compile-remote-call! word coord)
  (when DEBUG? (printf "     compile-remote-call!(~a, ~a)\n" word coord))
  (define addr (get-remote-addr word coord))
  (when DEBUG? (printf "        addr = ~a\n" addr))
  (compile-call! word addr))

(define (compile-word-ref! word)
  (compile-constant! (get-word-address word)))

(define (compile-remote-word-ref! word coord)
  (let ((addr (get-word-address word coord))) ;;TODO: need to fix for undefined addresses
    (unless addr
      (err (rkt-format "reference to undefined word: ~a" word)))
    (compile-constant! addr)))

(define (fill-rest-with-nops)
  (when DEBUG? (printf "(fill-rest-with-nops)\n"))
  (unless (= current-slot 0)
    (add-to-next-slot "." false)
    (fill-rest-with-nops)))

(define (set-next-empty-word! word)
  (if (= current-slot 0)
      (begin (vector-set! memory current-addr word)
             (when save-buffer-mappings
               (vector-set! buffer-mappings current-addr
                            ;; store as [x x x x] to keep the format the same
                            (vector current-token-buffer-position
                                    current-token-buffer-position
                                    current-token-buffer-position
                                    current-token-buffer-position)))
             (org next-addr))
      (begin (vector-set! memory next-addr word)
             (when save-buffer-mappings
               (vector-set! buffer-mappings next-addr
                            (vector current-token-buffer-position
                                    current-token-buffer-position
                                    current-token-buffer-position
                                    current-token-buffer-position)))
             (set! next-addr (if 64-word-mem
                                 (remainder (add1 next-addr) #x40)
                                 (add1 next-addr))))))

(define (make-new-address-cell val (name false) (next false))
  ;; name is an optional tag, usually the name of the word, that discribes the address.
  ;; it is use for debug only
  (define cell (address-cell val (or next next-addr) name))
  (set! address-cells (cons cell address-cells))
  cell)

(define (check-for-undefined-words node)
  (for ((word (node-mem node)))
    (when (vector? word)
      (when (vector? word)
        (for ((slot word))
          (when (and (address-cell? slot)
                     (not (address-cell-val slot))
                     (address-cell-name slot))
            (err (rkt-format "Undefined word: '~a' in node ~a" (address-cell-name slot)
                             (node-coord node)))))))))

(define (get-address-cell-val cell)
  (let ((addr (address-cell-val cell)))
    (when (not addr)
      (err (rkt-format "remove-address-cells -- invalid address '~a' for '~a'"
                       addr (address-cell-name cell))))
    addr))

(define (remove-address-cells node)
  ;; unwrap mcons address cells
  ;; shift down words to make room if address is to large
  (define mem (node-mem node))
  (define addr false)
  (define call-inst false)
  (define new-word-index false)
  (define (count word)
    (define n 0)
    (for ((inst word))
         (when (equal? inst "@p")
           (set! n (add1 n))))
    n)
  (define (do-remove (finish false))
    (define again true)
    (while again
      (set! again false)
      (define word-index 0)
      (for ((word mem))
           (cond ((address-cell? word)
                  (vector-set! mem word-index (get-address-cell-val word)))
                 ((vector? word)
                  (for ((slot word)
                        (slot-index 4))
                       (when (address-cell? slot)
                         (set! addr (get-address-cell-val slot))
                         (when (consp addr)
                           (setq addr (car addr)))
                         (if (not (address-fits? addr (sub1 slot-index) (address-cell-next-addr slot)))
                             (begin
                              (assert (not finish))
                              (set! again true)
                              (define len (node-len node))
                              (when (= len 64)
                                (err (rkt-format "out of memory while shifting words down node ~a"
                                                 (node-coord node))))
                              (set! new-word-index (+ word-index 1 (count word)))
                              (shift-words-down mem new-word-index node len)
                              (set-node-len! node (add1 len))
                              (increment-address (node-address-cells node) new-word-index)
                              (set! call-inst (vector-ref word (sub1 slot-index)))
                              (unless (member call-inst '("call" "jump" "next" "-if" "if"))
                                (err (rkt-format "invalid call instruction: '~a'" call-inst)))
                              ;; remove call from old word
                              (vector-set! word (sub1 slot-index) ".")
                              (vector-set! word slot-index ".")
                              ;; create and set call in new word
                              (set-address-cell-val! slot (add1 addr))
                              (set! word (vector call-inst slot nil nil))
                              (vector-set! mem new-word-index word)
                              (setq word-index new-word-index)
                              )
                           (when finish
                             (vector-set! word slot-index addr)))))))
           (setq word-index (add1 word-index)))
      (when finish
        (setq again false))))
  (do-remove)
  (do-remove true)
  node)

(define (increment-address address-cells from)
  (when DEBUG? (printf "(increment-address from=~a)\n" from))
  (for ((cell address-cells))
       (when (>= (address-cell-val cell) from)
         (set-address-cell-val! cell (add1 (address-cell-val cell))))
       (when (>= (address-cell-next-addr cell) from)
         (set-address-cell-next-addr! cell (add1 (address-cell-next-addr cell))))))

(define (shift-words-down memory from node end)
  (when DEBUG? (printf "(shift-words-down memory from=~a node=~a)\n" from (node-coord node)))
  (for ((i (reverse (range from end))))
       (vector-set! memory (add1 i) (vector-ref memory i)))
  (when save-buffer-mappings
    (for ((i (reverse (range from end))))
         (vector-set! buffer-mappings (add1 i) (vector-ref buffer-mappings i))))
  ;;;; (let ((word-dict (node-word-dict node))
  ;;;;       (cell false)
  ;;;;       (a false))
  ;;;;   (for ((w (hash->list word-dict)))
  ;;;;        (set! cell (cdr w))
  ;;;;        (set! a (address-cell-val cell))
  ;;;;        (printf "w = ~a, cell = ~a, a = ~a\n" w cell a)
  ;;;;        (when (>= a from)
  ;;;;          (set-address-cell-val! cell (1+ a))
  ;;;;          (set-address-cell-next-addr! cell (1+ (address-cell-next-addr cell)))
  ;;;;          (hash-set! word-dict (car w) cell))))

  ;; shift symbol addresses down
  ;; otherwise the word annotations in simulation views will be wrong
  (let ((a false))
    (for ((sym (node-symbols node)))
         (set! a (symbol-address sym))
         (when (>= a from)
           (when (equal? (symbol-val sym) "main")
             (set-node-p! node (1+ a)))
           (set-symbol-address! sym (1+ a))))))

;; map jump instruction slots to bit masks for their address fields
(define address-masks (vector #x3ff #xff #x7))

(define (address-fits? destination-addr jump-slot (P false))
  ;; returns t if DESTINATION-ADDR is reachable from the current word
  ;; JUMP-SLOT is the slot of the jump/call instruction
  (assert (not (null destination-addr)))
  (if (consp destination-addr)
      (setq destination-addr (car destination-addr)))
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
 nil
 (lambda ()
   (fill-rest-with-nops)))

(add-directive!
 "("
 "Start comment"
 (lambda ()
   (while (not (equal? (forth-read-char) _char-close-paren))
     false)))

(op-doc ")" "End comment")

(define (start-word-def (word false))
  (fill-rest-with-nops)
  (let* ((word (or word (read-tok-name)))
         (address (make-addr current-addr))
         (waiting-list (get-waiting-list word)))
    (when waiting-list
      (for ((cell waiting-list))
           (set-address-cell-val! cell address))
      (waiting-clear word))
    (when (hash-has-key? words word)
      (err (rkt-format "redefinition of word '~a' in node ~a"
                       word current-node-coord)))
    (when (equal? word "main")
      (if (node-p current-node)
          (err (rkt-format "use of /p overrides 'main' in node ~a\n"
                           current-node-coord))
        (set-node-p! current-node (make-addr current-addr))))
    (add-word! word address)))

(when elisp?
  (setq start-word-def 'start-word-def))

(add-directive! ":" "Begin word definition" start-word-def)
(add-directive! 'word-def nil start-word-def)

(add-directive!
 ".."
 "forces word alignment"
 (lambda () (fill-rest-with-nops)))

(add-directive! ;; page 23 of arrayforth users manual DB004
 ","
 "compile word literal" ;;TODO
 (lambda ()
   (let* ((token (read-tok-name))
          (data (parse-num token)))
     (if (not data)
         (err (rkt-format "invalid token: ~a" token))
         (set-next-empty-word! data)))))

(define (set-current-node-length)
  (let ((i (- (vector-length memory) 1))
        (empty-word [nil nil nil nil]))
    (while (and (>= i 0)
                (or (not (vector-ref memory i))
                    (equal? (vector-ref memory i) empty-word)))
      (set! i (- i 1)))
    (set-node-len! current-node (add1 i))))

(define (start-new-node coord)
  (when DEBUG? (printf "new node: ~a\n" coord))
  (when current-node
    (set-node-address-cells! current-node address-cells))
  (when memory ;;make sure last instruction is full
    (fill-rest-with-nops)
    (set-current-node-length))

  (define index (coord->index coord))
  ;;TODO: validate 'node'
  ;;assert (node-coord node) == coord
  (when (vector-ref nodes index)
    (err "repeated node definition"))
  (set! current-node (get-node coord))
  (set! memory (node-mem current-node))
  (set! buffer-mappings (node-buffer-map current-node))
  (set! words (node-word-dict current-node))
  (set! address-cells '())
  (set! rom (get-node-rom coord))
  ;;TODO: should calling 'node' multiple times be ok?
  ;;      if so, don't add current-node to used-nodes again
  (set! used-nodes (cons current-node used-nodes))
  (set! current-node-coord coord)
  (set! current-node-consts (node-consts current-node))
  (set! extern-funcalls false)
  (org 0))

(add-directive!
 "node"
 "(nn) starts compilation for the given node with number in yyxx notation"
 (lambda ()
   (define coord (read-tok-name))
   ;;TODO: validate coord
   (define x (parse-num coord))
   (if (not (null? x))
       (start-new-node x)
       (err (rkt-format "invalid node number: ~a" x)))))

(add-directive!
 "+cy"
 "forces word alignment then turns P9 on in the location counter. Places in memory
subsequently defined will be run in Extended Arithmetic Mode if reached by
jump, call, execute or return to those places."
 (lambda ()
   (fill-rest-with-nops)
   (set! extended-arith #x200)))

(add-directive!
 "-cy"
 "forces word alignment then turns P9 off in the location counter"
 (lambda ()
   (fill-rest-with-nops)
   (set! extended-arith 0)))

(define (here)
  (fill-rest-with-nops)
  (push (cons (make-addr current-addr) next-addr) stack))
(when elisp?
  (setq here 'here))

(add-directive!
 "here"
 "(-n) forces word alignment and pushes current aligned location onto compiler stack"
 here)

(add-directive!
 "begin"
 "(-a) forces word alignment and saves here to be used as a transfer destination."
 here)

(add-directive!
 "for"
 "for (-a) (n)
pushes n onto the return stack, forces word alignment and saves 'here' to be
used as a transfer destination by the directive that ends the loop.
There are times when it is useful to decompose this directive's actions so
that the pushing of the loop count and the start of the loop itself may be
separated by such things as initialization code or a word definition. In this
case you may write 'push <other things> begin'"
 (lambda ()
   (compile-instruction! "push")
   (here)))

(define (compile-next-type inst)
  (let ((addr (pop stack))
        (next false))
    (when (null addr)
      (err "address from stack is nil"))
    (unless (consp addr)
      (err (rkt-format "expected type cons from stack, got ~a ~a"  (type-of addr) addr)))
    (set! next (cdr addr))
    (set! addr (car addr))

    ;;(add-to-slot addr (make-new-address-cell current-addr "then" next-addr))
    (unless (address-fits? addr current-slot)
      (fill-rest-with-nops))
    (add-to-next-slot inst)
    (add-to-next-slot (make-new-address-cell addr inst next))
    (unless (= current-slot 0)
      (goto-next-word))
    ))

(add-directive!
 "next"
 "(a) ends a loop with conditional transfer to the address a. If R is zero when next
is executed, the return stack is popped and program flow continues. Otherwise
R is decremented by one and control is transferred to a."
 (lambda () (if bowman-format
                (compile-transfer-instruction "next" (read-tok-name))
              (compile-next-type "next"))))

(add-directive!
 "next:"
 "todo"
 (lambda () (compile-transfer-instruction "next" (read-tok-name))))

(add-directive!
 "end"
 "(a) unconditionally jumps to a"
 (lambda () (compile-next-type "jump")))

(add-directive!
 "until"
 "(a) If T is nonzero, program flow continues; otherwise jumps to a.
Typically used as a conditional exit at the end of a loop."
 (lambda () (compile-next-type "if")))

(add-directive!
 "-until"
 "(a) If T is negative, program flow continues; otherwise jumps to a. Used like 'until'"
 (lambda () (compile-next-type "-if")))

(add-directive!
 "unext"
 "(a) ends a micronext loop. Since the loop occurs entirely within a single
instruction word, the address is superfluous; it is present only so that the
form \"<n> for ... unext\" may be written. The micronext opcode may be compiled
into any of the four slots."
 (lambda ()
   (compile-instruction! "unext")
   (pop stack)))

(add-directive!
 "*next"
 "(ax-x) equivalent to 'swap next'"
 (lambda ()
   (swap stack)
   (compile-next-type "next")))

(define (compile-transfer-instruction inst name (addr false))
  ;; inst - instruction type
  ;; name - name of word to jump to
  (set! addr (or addr (get-word-address name)))
  (if addr
      (begin
       (unless (address-fits? (if (address-cell? addr) (address-cell-val addr) addr) current-slot)
         (fill-rest-with-nops))
       (when (equal? current-slot 3)
         (fill-rest-with-nops))
       (add-to-next-slot inst)
       (add-to-next-slot (if (address-cell? addr)
                             addr
                           (make-new-address-cell addr name)) false)
       (unless (= current-slot 0)
         (goto-next-word)))
    ;;else
    (begin
     (when (equal? current-slot 3)
       (fill-rest-with-nops))
     (add-to-next-slot inst)
     (set! cell (make-new-address-cell false name))
     (add-to-waiting name cell)
     (add-to-next-slot cell false)
     (unless (= current-slot 0)
       (goto-next-word))
     )))

(define (compile-if-instruction inst (immediate false))
  ;;cannot be in last word.
  (setq immediate (or immediate bowman-format))
  (when (and (equal? current-slot 3)
             (not (member inst last-slot-instructions)))
    (add-to-next-slot "." false))

  (if immediate
      (compile-transfer-instruction inst (read-tok-name))
    (begin
     (add-to-next-slot inst)
     (push (cons (make-addr current-addr) next-addr) stack))
    (goto-next-word)))

;;If T is nonzero, program flow continues; otherwise jumps to matching 'then'
(define (if-directive)
  (compile-if-instruction "if"))
(when elisp? (setq if-directive 'if-directive))

(add-directive!
 "if"
 nil ;;TODO:
 if-directive)

;; TODO: the directive list used for parsing is in aforth-mode.el
;;       and needs to be maintained separately
;;       it should be generated from the directive map built here
(add-directive!
 "if:"
 nil
 (lambda () (compile-if-instruction "if" true)))

(add-directive!
 "-if:"
 nil
 (lambda () (compile-if-instruction "-if" true)))

(define (-if-directive)
  (compile-if-instruction "-if"))
(when elisp? (setq -if-directive '-if-directive))

(add-directive!
 "-if"
 "(-r) If T is negative, program flow continues; otherwise jumps to matching 'then'"
 -if-directive)

(add-directive!
 "zif"
 "(-r) If R is zero, pops the return stack and program flow continues;
otherwise decrements R and jumps to matching 'then'"
 (lambda ()
   (compile-if-instruction "next")))

(add-directive!
 "ahead"
 "(-r) jumps to matching 'then'"
 (lambda ()
   (compile-if-instruction "jump")))

(add-directive!
 "leap"
 "(-r) compiles a call to matching 'then'"
 (lambda ()
   (compile-if-instruction "call")))

(define (add-to-slot slot thing)
  (define (find-first-empty word (n 0))
    ;; find the first empty slot in WORD
    (if (< n 4)
        (if (vector-ref word n)
            (find-first-empty word (add1 n))
            n)
        false))
  (define max-slot-num (vector 262144 8192 256 8))
  (let* ((word (get-word slot false true))
         (buf-map (and save-buffer-mappings (vector-ref buffer-mappings slot)))
         (last (and (vector? word) (find-first-empty word))))
    (if last
        (if (and (not (address-cell? thing))
                 (> thing (vector-ref max-slot-num last)))
            ;; TODO: move instruction to next word in this case
            (err (rkt-format "'~a' cannot fit into slot ~a" thing last))
            (begin
              (vector-set! word last thing)
              (and buf-map (vector-set! buf-map last thing))))
        (err (rkt-format "add-to-slot -- slot ~a ~a: ~a"
                         slot
                         (if (vector? word)
                             "is not an instruction word"
                             "is full")
                         word)))))

(define (check-stack stack len)
  (unless (>= (length stack) len)
    (err "compiler stack underflow.")))

(add-directive!
 "jump"
 ""
 (lambda ()
   (unless bowman-format
     (err "jump instruction only supported in bowman mode. Use \"WORD ;\" to compile a jump to WORD" ))
   (compile-transfer-instruction "jump" (read-tok-name))))

(add-directive!
 "call"
 ""
 (lambda ()
   (assert bowman-format)
   (compile-transfer-instruction "call" (read-tok-name))))

;; back references for 'if' type instructions are different from word calls
;; because their address is inserted by a corresponding 'then'
;; this means that 'next-addr' as saved by 'make-new-address-cell is the
;; next-addr from the perspective of 'then' not the 'if' instruction.
;; To fix this issue the 'if' instructions push a cons cell with
;; the current-addr and the next-addr onto the compiler stack.
;; 'then' then uses that next-addr when calling 'make-new-address-cell'

(add-directive!
 "then"
 "(r) forces word alignment and resolves a forward transfer."
 (lambda ()
   (fill-rest-with-nops)
   (check-stack stack 1)

   (let* ((x false)
          (addr false)
          (next-addr false))

     (set! x (pop stack))
     (unless (consp x)
       (err (rkt-format "expected type cons from stack, got ~a ~a"  (type-of x) x)))
     (set! addr (car x))
     (set! next-addr (cdr x))
     (add-to-slot addr (make-new-address-cell current-addr "then" next-addr)))))

(define (org-directive (addr false))
  (let ((n (or addr (parse-num (read-tok-name)))))
    ;;TODO: validate n
    (unless n (err "invalid address for 'org'"))
    (org n)))

(when elisp? (setq org-directive 'org-directive))

(add-directive!
 "org"
 "(n) sets the compiler's location counter to a given address at
which following code will be compiled into"
 org-directive)

(add-directive!
 "while"
 "(x-rx) equivalent to 'if swap'. Typically used as a conditional exit from within a loop"
 (lambda ()
   (if-directive)
   (swap stack)))

(add-directive!
 "-while"
 "(x-rx) equivalent to '-if swap'. Typically used as a conditional exit from within a loop"
 (lambda ()
   (-if-directive)
   (swap stack)))


(add-directive!
 "`"  ;;TODO:
 "' (-a)
(tick) places the address of an F18 red word on the compiler's stack."
 (lambda ()
   (let* ((word (read-tok-name))
          (addr (get-word-address word)))
     (if addr
         (push (cons addr next-addr) stack)
         (err (rkt-format "\"~a\" is not defined" word))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boot descriptors
;; DB004 section 5.5.1

(define (set-register-helper name set-fn)
  (unless current-node
    (err (rkt-format "must select node before '~a'" name)))
  (let* ((n (read-tok-name))
         (addr (or (get-address n current-node-coord)
                   (get-word-address n))))
    (unless (and addr
                 (number? addr))
      (err (rkt-format "unknown address for compiler directive '~a': ~a" name n)))
    (if elisp?
        (funcall set-fn current-node addr)
        (set-fn current-node addr))))

(add-directive!
 "/b"
 "(a) Specifies an initial value for register B.
Default value is the address of the IO register, as at reset."
 (lambda ()
   (set-register-helper "/b" set-node-b!)))

(add-directive!
 "/a"
 "(n) Specifies an initial value for register A.
Default value is unspecified, as at reset."
 (lambda ()
   (set-register-helper "/a" set-node-a!)))

(add-directive!
 "/io"
 "(n) Specifies a value to be loaded into the IO register."
 (lambda ()
   (set-register-helper "/io" set-node-io!)))

(add-directive!
 "/p"
 "(a) Specifies an initial value for register P. Default value is xA9 which is
the routine warm in every node's ROM."
 (lambda ()
   (set-register-helper "/p" set-node-p!)))

(add-directive!
 "/stack"
 "/stack n <n values>
Specifies up to ten values to be pushed onto the data stack, with the
rightmost value on top. For example /stack 3 30 20 10 produces the same
effect as though a program had executed code 30 20 10"
 (lambda ()
   (let* ((len (read-tok-name))
          (stack '())
          (val false))
     (when (or (not len)
               (< len 0)
               (> len 10))
       (err (rkt-format "invalid number for /stack item count: '~a'" len)))

     (while (> len 0)
       (begin
        (set! val (read-tok-name))
        (when (and (not (number? val))
                   (not (setq val (get-word-address val))))
          (err (rkt-format "invalid stack value: ~a" val)))
        (push val stack)
        (set! len (sub1 len))))
     (set-node-stack! current-node (reverse stack)))))

(add-directive!
 "swap!"
 nil ;;TODO
 (lambda ()
   (swap stack)))

;;NOTE: +node, /ram, and /part are not supported

(define (define-const name val)
  (when (hash-has-key? current-node-consts name)
    (err (rkt-format "redefining node const '~a'" name)))
  (hash-set! current-node-consts name val))

(define (node-const? name)
  (hash-has-key? current-node-consts name))
(define (compile-node-const name)
  (unless (hash-has-key? current-node-consts name)
    (err (rkt-format "node const not found'~a'" name)))
  (compile-constant! (hash-ref current-node-consts name)))

(define const-ops (if elisp? (make-hash '(("+" . +)
                                          ("or" . bitwise-xor)
                                          ))
                      (make-hash `(("+" . ,+)
                                   ("or" . ,bitwise-xor)
                                   ))))

(define (lookup-const-value x)
  ;; parses x as a number or looks it up
  (or (parse-num x)
      (and (node-const? x)
           (hash-ref current-node-consts x))))

(add-directive!
 "const"
 nil
 (lambda ()
   ;; (err "'const' directive is deprecated") ;;TODO: remove tests
   (define (read-apply-op name op-name)
     (define op (hash-ref const-ops op-name))
     (define left-tok (read-tok-name))
     (define left (lookup-const-value left-tok))
     (unless left
       (err (rkt-format "invalid const param for op ~a: '~a'" op-name left-tok)))
     (define right-tok (read-tok-name))
     (define right (lookup-const-value right-tok))
     (unless right
       (err (rkt-format "invalid const param for op ~a: '~a'" op-name right-tok)))
     (define-const name (bitwise-and (apply op (list left right)) #x3ffff)))

   (define name (read-tok-name))
   (define op (read-tok-name))
   (define op-n (string->number op))

   (cond (op-n (define-const name op-n))
         ((hash-has-key? const-ops op)
          (read-apply-op name op))
         (else (err (rkt-format "invalid const op type: '~a'" op))))))

(define (get-word-address* word)
  (define addr (get-word-address word))
  (if addr
      addr
    (begin
     (define cell (make-new-address-cell false word))
     (add-to-waiting name cell)
     cell)))

(add-directive!
 "'"
 "' (-a) (tick) places the address of an F18 red word on the compiler's stack."
 (lambda ()
   (push (get-word-address* (read-tok-name)) stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(for ((dir (list "north" "south" "east" "west")))
  (add-directive!
   dir
   "address for the left/right/up/down directional port"
   ((lambda (dir)
      (lambda () (let ((d (get-directive (convert-direction current-node-coord dir))))
                   (assert d)
                   (if elisp?
                       (funcall d)
                       (d)))))
    dir)))

(define (define-named-addresses!)
  (for ((addr (if (or bowman-format true)
                  (append named-addresses io-places)
                  named-addresses)))
    (add-directive!
     (car addr)
     (format "Compile the address for the ~a port" (car addr))
     ((lambda (a) (lambda () (compile-constant! a))) (cdr addr)))))

(define (lit-directive)
  (compile-constant! (pop stack)))

(add-directive! "lit" "todo" lit-directive)

(define (err msg)
  (printf "ERROR")
  (when (and current-tok-line current-tok-col)
    (printf (rkt-format "[~a:~a]" current-tok-line current-tok-col)))
  (printf (rkt-format " ~a\n" msg))
  (when current-token
    (printf "  (while compiling token '~a')\n" current-token))
  (when elisp?
    (set 'aforth-error-message msg)
    ;;TODO: need to set current node and other info here
    ;;  currently that info is updated in the parse stage.
    ;;  if the error occurs during compilation then the node is always the last from the file
    (throw 'aforth-error nil))
  (exit 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compiler words

;; list of words that are executed by the compiler instead of compiled for the f18a
(define compiler-words (make-hash))

(define compiler-ops (make-hash))

(define (add-compiler-word! name body)
  (when DEBUG? (printf (rkt-format "adding compiler word: '~a' = \n body" name body)))
  (hash-set! compiler-words name body))

(define _no-compiler-op-def false)

(define (def-compiler-op! name fn)
  (assert (not _no-compiler-op-def))
  (hash-set! compiler-ops name fn))

(define (_compiler-binop op)
  (check-stack stack 2)
  (push (funcall op (pop stack) (pop stack)) stack))

(define (_compiler-unop op)
  (check-stack stack 1)
  (push (op (pop stack)) stack))

(def-compiler-op! "+" (lambda () (_compiler-binop '+)))
(def-compiler-op! "sub" (lambda () (_compiler-binop '-)))
(def-compiler-op! "*" (lambda () (_compiler-binop '*)))
(def-compiler-op! "/" (lambda () (_compiler-binop '/)))
(def-compiler-op! "or" (lambda () (_compiler-binop 'bitwise-xor)))
(def-compiler-op! "ior" (lambda () (_compiler-binop 'bitwise-ior)))
(def-compiler-op! "-" (lambda () (_compiler-unop 'bitwise-not)))

(def-compiler-op!
  "dup"
  (lambda ()
    (push (car stack) stack)))

(def-compiler-op!
  "lit"
  (lambda () (lit-directive)))

(define (exec-compiler-word word)
  (when DEBUG? (printf (rkt-format "Execing compiler word ~a\n" word)))
  (let ((body (hash-ref compiler-words word)))
    (define i false)

    (while (not (null? body))
      (begin (set! i (car body))
             (set! body (cdr body))
             (cond ((number? i)
                    (push i stack))
                   ((compiler-word? i)
                    (exec-compiler-word i))
                   (t (funcall (hash-ref compiler-ops i))))))
    ))

(define (compiler-word? word)
  (hash-has-key? compiler-words word))

(define (start-compiler-word-def (name false))
  (let* ((name (or name (read-tok-name)))
         (body '())
         (tok false)
         (x false))
    ;; Allow redefinition of compiler words
    ;;  (when (hash-has-key? compiler-words word)
    ;;    (err (rkt-format "redefinition of compiler word '~a' in node ~a"
    ;;                   word current-node-coord)))
    (while (and (setq tok (read-tok-name))
                (not (equal? tok ";")))
      (cond ((or (hash-has-key? compiler-ops tok)
                 (compiler-word? tok))
             (set! body (cons tok body)))
            ((setq x (parse-num tok))
             (set! body (cons x body)))
            (else (err (rkt-format "Unsupported compiler word op: ~a" tok)))))
    (add-compiler-word! name (reverse body))))
(when elisp? (setq start-compiler-word-def 'start-compiler-word-def))

(add-directive!
 "::"
 (rkt-format "Define a compiler directive. Words defined with :: with execute immediately during compilation
Only the following words are supported in the body of a compiler word:
~a" (string-join (hash-keys compiler-ops) ", "))
 start-compiler-word-def)

(add-directive! 'compile-def nil start-compiler-word-def)

(set! _no-compiler-op-def true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-compiled compiled)
  ;;NODES is a list of node structs
  (define (display-word word (n 0))
    (if (number? word)
        (printf "~a" word)
        (when (< n 4)
          (printf (rkt-format "~a " (vector-ref word n)))
          (display-word word (add1 n)))))

  (define (display-mem mem (index 0))
    (let ((word (vector-ref mem index)))
      (unless (equal? word (vector false false false false))
        (printf (rkt-format "~a    " index))
        (display-word word)
        (newline)
        (when (< index num-words)
          (display-mem mem (add1 index))))))

  (define (display-node nodes)
    (unless (null? nodes)
      (printf (rkt-format "\nnode ~a\n" (node-coord (car nodes))))
      (display-mem (node-mem (car nodes)))
      (display-node (cdr nodes))))
  (display-node (compiled-nodes compiled)))

(define (write-directive-docs filename)
  ;;TODO
  (assert elisp?)
  (with-temp-buffer
    (for ((doc op-docs))
      (insert (format "%s\n%s\n\n" (car doc) (cdr doc))))
    (write-file (concat filename ".txt"))

    )
  (with-temp-buffer
    (for ((doc op-docs))
      (insert (format "* %s\n%s\n" (car doc) (cdr doc))))
    (write-file (concat filename ".org"))
    )
  )
