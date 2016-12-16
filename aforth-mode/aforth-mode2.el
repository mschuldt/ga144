(require 'jit-lock)

(defvar aforth-mode-hook nil)

(setq aforth-instruction-list '("ret" "nop""ex" "jump" "call" "unext" "next" "if"
                                "and" "or" "drop" "dup" "pop" "over" "a" "push"
                                "@+" "!+" "+*" "2*" "2/" "!p"  "!b" "!" "-if"
                                ";" "." "-" "+"  "@p" "@b" "@" "b!" "a!"))

(setq aforth-port-list '("up" "left" "down" "right" "io"
                         "north" "east" "south" "west"))

(setq aforth-directive-list '("start" "for" "begin" "then" "here"
                              "while"  "reclaim" "leap"
                              ".." "#swap" "-while" "," "-until"
                              "---u" "--l-" "--lu" "-d--" "-d-u"
                              "-dl-" "-dlu" "r---" "r--u" "r-l-"
                              "r-lu" "rd--" "rd-u" "rdl-" "rdlu"
                              "node" "org" "::"))

(setq boot-descriptors-list '("/b" "/a" "/io" "/p" "/stack"))

(defun list-to-set (lst)
  (let ((ht (make-hash-table :test 'equal :size (* (length lst) 2))))
    (dolist (x lst)
      (puthash x t ht))
    ht))

(setq aforth-instruction-map (list-to-set aforth-instruction-list))
(setq aforth-port-map (list-to-set aforth-port-list))
(setq aforth-directive-map (list-to-set aforth-directive-list))
(setq boot-descriptors-map (list-to-set boot-descriptors-list))


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

(defface aforth-comment-face '((((background light))
                                (:foreground "grey"))
                               (((background dark))
                                (:foreground "grey")))
  "Default face for arrayforth comments")

(setq aforth-instruction-face 'aforth-instruction-face)
(setq aforth-directive-face 'aforth-directive-face)
(setq aforth-word-face 'aforth-word-face)
(setq aforth-number-face 'aforth-number-face)
(setq aforth-boot-descriptor-face 'aforth-boot-descriptor-face)

(setq aforth-buffer-words nil)
(setq aforth-next-id nil)
(setq aforth-current-str nil)
(setq aforth-current-pt nil)

(make-variable-buffer-local 'aforth-buffer-words)
(make-variable-buffer-local 'aforth-next-id)
(make-variable-buffer-local 'aforth-current-str)
(make-variable-buffer-local 'aforth-current-pt)

(defun aforth-get-token-face (token)
  (cond ((equal token ":") aforth-word-face)
        ((gethash token aforth-instruction-map) aforth-instruction-face)
        ((gethash token aforth-port-map) aforth-instruction-face)
        ((gethash token aforth-directive-map) aforth-directive-face)
        ((gethash token boot-descriptors-map) aforth-boot-descriptor-face)
        ((string-match "^\\(\\(0x[0-9a-fA-F]+\\)\\|\\(0b[01]+\\)\\|[0-9]+\\)$" token) aforth-number-face)
        ((string-match "&[a-zA-Z0-9]+" token)  aforth-word-reference-face)
        
        ))

(defun aforth-make-overlay (beg end)
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'aforth-id aforth-next-id)
    (setq aforth-next-id (1+ aforth-next-id))
    overlay))

(defun aforth-overlay-id (overlay)
  (overlay-get overlay 'aforth-id))

(defsubst aforth-token-delimiter-p (c)
  (or (eq c ?\[)
      (eq c ?\])
      (eq c ? )
      (eq c ?\t)
      (eq c ?\n)))


(defun aforth-update-overlays (beg end)
  (let ((str (string-to-list (buffer-substring-no-properties beg end)))
        (tok-beg 0)
        token tok-end first
        next-token-face
        next-token-def-p
        )

    (while str
      (while (and str (not first))
        (setq c (car str)
              str (cdr str)
              tok-beg (1+ tok-beg))
        (unless (aforth-token-delimiter-p c)
          (setq first c
                tok-end tok-beg
                tok-beg (1- tok-beg))))
      (when first
        (setq token (list first))
        (while (and str first)
          (setq c (car str)
                str (cdr str))
          (if (aforth-token-delimiter-p c)
              (setq first nil)
            (setq token (cons c token)
                  tok-end (1+ tok-end))))
        (when (> tok-end tok-beg)
          (setq token (concat (reverse token))
                face next-token-face
                next-token-face nil
                face (or face (aforth-get-token-face token)))
          (when face
            (progn (setq o (make-overlay (+ beg tok-beg) (+ beg tok-end)))
                   (overlay-put o 'face face)))
          (cond ((member token '("org" "node"))
                 (setq next-token-face aforth-directive-face))
                ((equal token ":")
                 (setq next-token-face aforth-word-face
                       next-token-def-p t))

                ((equal token "::")
                 (setq next-token-face aforth-directive-face)))
          ))
      (setq beg (+ beg tok-end 1)
            first nil
            token nil
            tok-beg 0
            tok-end 0))))


(defun aforth-remove-overlays (beg end)
  (dolist (o (overlays-in beg end))
    (delete-overlay o)
    ))

(defun aforth-update-region (beg end &optional _old-len)
  "After-change function updating overlays"
  (message "aforth-update-region")
  (let ((beg-line (save-excursion (goto-char beg) (line-beginning-position)))
	(end-line (save-excursion (goto-char end) (line-end-position))))
    ;;multiple token constructs may span multiple lines, but that style is ugly and not supported
    (aforth-remove-overlays beg-line end-line)
    (aforth-update-overlays beg-line end-line)))

(defun aforth-create-index ()
  (let* ((aforth-defining-words-regexp
          (concat "\\(" (regexp-opt aforth-defining-words) "\\)")
          )
	 (index nil))
    (goto-char (point-min))
    (while (aforth-next-definition-starter)
      (if (looking-at "[ \t]*\\([^ \t\n]+\\)")
	  (setq index (cons (cons (match-string 1) (point)) index))))
    index))


(define-derived-mode aforth-mode prog-mode "aforth2"
  "Major mode for editing aforth files"

  (widen)
  (progn (jit-lock-register 'aforth-update-region)
         (setq aforth-buffer-words (make-hash-table)
               aforth-next-id 1))
  (setq imenu-create-index-function 'aforth-create-index)
  (aforth-update-region (point-min) (point-max))
  (run-hooks 'aforth-mode-hook))

(add-to-list 'auto-mode-alist '("\\.aforth\\'" . aforth-mode))

(provide 'aforth-mode)
