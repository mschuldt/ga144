(require 'font-lock)

(defvar aforth-mode-hook nil)

(setq aforth-instructions-1 '( ";" "ret"
			       "." "nop"
			       "ex" "jump" "call" "unext" "next" "if"
			      "-if" "@p" "@b" "@" "!p"  "!b" "!"
			      "-" "+" "and" "or" "drop" "dup" "pop"
			      "over" "a" "push" "b!" "a!"))

(setq aforth-instructions-2 '("@+" "!+" "+*" "2*" "2/"))

(defvar aforth-ports '("up" "left" "down" "right" "io"))

(defvar aforth-directives '("node" "org" ".." "start" "for" "begin"
			    "then" "#swap" "while" "-while" "reclaim"
			    ;;"," ":"
			    ))

(defun aforth-make-regexp (strings &optional word)
  (let ((x (concat "\\("
		   (mapconcat 'regexp-quote strings "\\|")
		   "\\)")))
    (if word
	(concat "\\<" x "\\>")
      x)))

(setq aforth-instruction-regexp-1 (aforth-make-regexp aforth-instructions t))
(setq aforth-instruction-regexp-2 (aforth-make-regexp aforth-instructions))
(setq aforth-directive-regexp (aforth-make-regexp aforth-directives t))
(setq aforth-ports-regexp (aforth-make-regexp aforth-ports t))
(aforth-make-regexp '("b!"))

(defface aforth-instruction-face2 '((((background light)) (:foreground "green"))
				 (((background dark)) (:foreground "green")))
  "Default face for arrayforth instructions")

(defface aforth-directive-face2 '((((background light)) (:foreground "yellow"))
				 (((background dark)) (:foreground "yellow")))
  "Default face for arrayforth compiler directives")

(defface aforth-number-face3 '((((background light)) (:foreground "LightGreen"))
			       (((background dark)) (:foreground "LightGreen")))
  "Default face for arrayforth numbers")

(defface aforth-word-face2 '((((background light)) (:foreground "red"))
				 (((background dark)) (:foreground "red")))
  "Default face for arrayforth word definitions")

(setq aforth-instruction-face 'aforth-instruction-face2)
(setq aforth-directive-face 'aforth-directive-face2)
(setq aforth-word-face 'aforth-word-face2)
(setq aforth-number-face 'aforth-number-face3)

(setq aforth-font-lock-keywords
      `(;;word definitions
	("\\(:\\)[ \t\n]+\\([a-zA-Z0-9_-]+\\)"
	 (1 aforth-word-face)
	 (2 aforth-word-face))
	;;numbers
	;;TODO: should not color numbers inside of words
	("\\(0x\\|0b\\)?[0-9+]" . aforth-number-face)
	(,aforth-directive-regexp . aforth-directive-face)
	(,aforth-instruction-regexp-1 . aforth-instruction-face)
	(,aforth-instruction-regexp-2 . aforth-instruction-face)
	(,aforth-ports-regexp . aforth-instruction-face)
	;;font-lock-comment-face
	))

(defvar aforth-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\( "<1" table)
    (modify-syntax-entry ?\) ">4" table)
    table)
  "Syntax table in use in aforth buffers")

(define-derived-mode aforth-mode prog-mode "aforth"
  "Major mode to edit aforth"
  (setq font-lock-defaults '((aforth-font-lock-keywords)))

  (set-syntax-table aforth-mode-syntax-table)
  (make-local-variable 'comment-start)
  (setq comment-start "( ")
  (setq comment-end ")")
  (setq comment-start-skip "\\((\\*?\\|\\\\\\) *")

  (run-hooks 'aforth-mode-hook)
  )

(add-to-list 'auto-mode-alist '("\\.aforth\\'" . aforth-mode))

(provide 'aforth-mode)
