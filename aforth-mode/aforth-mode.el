(require 'font-lock)

(defvar aforth-mode-hook nil)

(setq aforth-instructions-1 '( "ret" "nop""ex" "jump" "call" "unext" "next" "if"
			       "and" "or" "drop" "dup" "pop" "over" "a" "push" ))

(setq aforth-instructions-2 '("@+" "!+" "+*" "2*" "2/" "!p"  "!b" "!" "-if"
                              ";" "." "-" "+"  "@p" "@b" "@" "b!" "a!"))

(setq aforth-ports '("up" "left" "down" "right" "io"
                     "north" "east" "south" "west"))

(setq aforth-directives '( "start" "for" "begin" "then" "here"
                           "while"  "reclaim" "leap"))

(setq aforth-directives2 '(".." "#swap" "-while" "," "-until"
                           "---u" "--l-" "--lu" "-d--" "-d-u"
                           "-dl-" "-dlu" "r---" "r--u" "r-l-"
                           "r-lu" "rd--" "rd-u" "rdl-" "rdlu"))

;;directives that take an argument
(setq aforth-directives-3 '("node" "org"))

(setq boot-descriptors '("/b" "/a" "/io" "/p" "/stack"))

(defun aforth-make-regexp (strings &optional word)
  (let ((x (concat "\\("
		   (mapconcat 'regexp-quote strings "\\|")
		   "\\)")))
    (if word
	(concat "\\<" x "\\>")
      x)))

(setq aforth-number-regexp
      "\\<\\(\\(0x[0-9a-fA-F]+\\)\\|\\(0b[01]+\\)\\|[0-9]+\\)\\>")
(setq aforth-instruction-regexp-1 (aforth-make-regexp aforth-instructions-1 t))
(setq aforth-instruction-regexp-2 (aforth-make-regexp aforth-instructions-2))
(setq aforth-directive-regexp (aforth-make-regexp aforth-directives t))
(setq aforth-directive-regexp-2 (aforth-make-regexp aforth-directives2))
(setq aforth-ports-regexp (aforth-make-regexp aforth-ports t))
(setq aforth-boot-descriptor-regex (aforth-make-regexp boot-descriptors))
(setq aforth-directive-regexp-3
      (concat "\\(" (mapconcat (lambda (x)
                                 (format "%s[ ]+[a-zA-Z0-9]+" x))
                               aforth-directives-3
                               "\\|")
              "\\)"))
(aforth-make-regexp '("b!"))

(defface aforth-instruction-face '((((background light)) (:foreground "green"))
                                   (((background dark)) (:foreground "green")))
  "Default face for arrayforth instructions")

(defface aforth-directive-face '((((background light)) (:foreground "yellow"))
                                 (((background dark)) (:foreground "yellow")))
  "Default face for arrayforth compiler directives")

(defface aforth-number-face '((((background light)) (:foreground "LightGreen"))
                              (((background dark)) (:foreground "LightGreen")))
  "Default face for arrayforth numbers")

(defface aforth-word-face '((((background light)) (:foreground "red"))
                            (((background dark)) (:foreground "red")))
  "Default face for arrayforth word definitions")

(defface aforth-boot-descriptor-face '((((background light))
                                        (:foreground "maroon"))
                                       (((background dark))
                                        (:foreground "maroon")))
  "Default face for arrayforth boot descriptors")

(defface aforth-word-reference-face '((((background light))
                                       (:foreground "DeepPink"))
                                      (((background dark))
                                       (:foreground "DeepPink")))
  "Default face for arrayforth boot descriptors")

(setq aforth-instruction-face 'aforth-instruction-face)
(setq aforth-directive-face 'aforth-directive-face)
(setq aforth-word-face 'aforth-word-face)
(setq aforth-number-face 'aforth-number-face)
(setq aforth-boot-descriptor-face 'aforth-boot-descriptor-face)
(setq aforth-word-reference-face 'aforth-word-reference-face)

(setq aforth-indent-words '((("if" "begin" "for")    (0 . 2) (0 . 2))
			    (("then" "next" "unext") (-2 . 0) (0 . -2))
			    (("while" "-while")      (-2 . 4) (0 . 2))))

(setq aforth-font-lock-keywords
      `(;;word definitions
	("\\(:\\)[ \t\n]+\\([a-zA-Z0-9_+!@.*/\-]+\\)"
	 (1 aforth-word-face)
	 (2 aforth-word-face))
        ("&[a-zA-Z0-9]+" . aforth-word-reference-face)
	(,aforth-boot-descriptor-regex . aforth-boot-descriptor-face)
        (,aforth-directive-regexp-2 . aforth-directive-face)
	(,aforth-directive-regexp . aforth-directive-face)
        (,aforth-directive-regexp-3 . aforth-directive-face)
	(,aforth-instruction-regexp-2 . aforth-instruction-face)
	(,aforth-instruction-regexp-1 . aforth-instruction-face)
	(,aforth-ports-regexp . aforth-instruction-face)
        (,aforth-number-regexp . aforth-number-face)
	))

(defvar aforth-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "<1" table)
    (modify-syntax-entry ?\) ">4" table)
    table)
  "Syntax table in use in aforth buffers")

;; imenu support

(defvar aforth-defining-words
  '(":")
  "List of words, that define the following word.
Used for imenu index generation.")

(defvar aforth-defining-words-regexp nil
  "Regexp that's generated for matching `aforth-defining-words'")

(defun aforth-next-definition-starter ()
  (progn
    (let* ((pos (re-search-forward aforth-defining-words-regexp (point-max) t)))
      (if pos
	  (if (or (text-property-not-all (match-beginning 0) (match-end 0)
					 'aforth-parsed nil)
		  (text-property-not-all (match-beginning 0) (match-end 0)
					 'aforth-state nil)
                  nil)
	      (aforth-next-definition-starter)
	    t)
	nil))))

(defun aforth-create-index ()
  (let* ((aforth-defining-words-regexp
	  ;;(concat "\\<\\(" (regexp-opt aforth-defining-words) "\\)\\>")
          (concat "\\(" (regexp-opt aforth-defining-words) "\\)")
          )
	 (index nil))
    (goto-char (point-min))
    (while (aforth-next-definition-starter)
      (if (looking-at "[ \t]*\\([^ \t\n]+\\)")
	  (setq index (cons (cons (match-string 1) (point)) index))))
    index))

(define-derived-mode aforth-mode prog-mode "aforth"
  "Major mode for editing aforth files"
  (setq font-lock-defaults '((aforth-font-lock-keywords)))

  (set-syntax-table aforth-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "( ")
  (setq comment-end ")")
  (setq comment-start-skip "\\((\\*?\\|\\\\\\) *")

  (setq imenu-create-index-function 'aforth-create-index)
  (run-hooks 'aforth-mode-hook)
  )

(add-to-list 'auto-mode-alist '("\\.aforth\\'" . aforth-mode))

(provide 'aforth-mode)
