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

(defun set-member? (set key)
  (gethash key set))

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

(defface aforth-remote-coord-face '((((background light))
                                     (:foreground "orange"))
                                    (((background dark))
                                     (:foreground "orange")))
  "Default face for arrayforth boot descriptors")


(setq aforth-instruction-face 'aforth-instruction-face)
(setq aforth-directive-face 'aforth-directive-face)
(setq aforth-word-face 'aforth-word-face)
(setq aforth-number-face 'aforth-number-face)
(setq aforth-boot-descriptor-face 'aforth-boot-descriptor-face)
(setq aforth-remote-coord-face 'aforth-remote-coord-face)
(setq aforth-comment-face 'aforth-comment-face)

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
        ;;TODO: <word>@<node> word=white @node=green

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

(defun aforth-comment-at-point-p (point)
  (save-excursion
    (goto-char point)
    (re-search-backward "[^ \t\n]" nil :no-error);;TODO: better re
    (if (looking-at ")")
        nil
      (let ((overlays (text-properties-at (point)))
            comment-p)
        (while overlays
          (if (eq (car overlays) 'aforth-comment)
              (setq comment-p t
                    overlays nil)
            (setq overlays (cddr overlays))))
        comment-p))))

;;TODO: make buffer local
(defstruct aforth-token type value args start end overlay subtoks)
(setq aforth-parse-data (make-hash-table)) ;;map lines to token listss

(defun aforth-current-node (&optional forward)
  (let ((found nil)
        (node nil)
        (re "node[ \t\n]+\\([0-9]+\\)")
        p)
    (save-excursion
      ;;(end-of-line)
      (while (not found)
        (if (if forward
                (re-search-forward re nil :noerror)
              (re-search-backward re nil :noerror))
            (progn (setq node (match-string 1))
                   (unless (aforth-comment-at-point-p (point))
                     (setq p (point)
                           found t)))
          (setq found t))))
    (and node (cons (string-to-number node) p))))

(defun aforth-back-to-node ()
  (interactive)
  (let ((point (aforth-current-node)))
    (when point
      (goto-char (cdr point)))))

(defun aforth-goto-next-node ()
  (interactive)
  (let ((point (aforth-current-node :forward)))
    (when point
      (goto-char (cdr point)))))

(defun test ()
  (interactive)
  (aforth-back-to-node))

(defun aforth-get-token-list (beg end)

  (let ((str (string-to-list (buffer-substring-no-properties beg end)))
        (tok-beg 0)
        (tokens '())
        token tok-end first
        next-token-face
        next-token-def-p)

    (while str
      ;;whitespace
      (while (and str
                  (not first))
        (setq c (car str)
              str (cdr str)
              tok-beg (1+ tok-beg))
        (unless (aforth-token-delimiter-p c)
          (setq first c
                tok-end tok-beg
                tok-beg (1- tok-beg))))
      ;;comment
      (when (eq first ?\()
        (setq c nil
              token (list first)
              tok-end (1+ tok-beg))
        (while (and str
                    (not (or (eq c ?\))
                             (eq c ?\n))))
          (setq c (car str)
                str (cdr str)
                token (cons c token)
                tok-end (1+ tok-end)))
        (push (make-aforth-token :type 'comment
                                 :value (concat (reverse token))
                                 :start (+ beg tok-beg)
                                 :end (+ beg tok-end))
              tokens)
        (setq tok-end (- tok-end 1))
        (setq first nil))

      ;;token
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
                next-token-face nil)
          (push (make-aforth-token :type 'op
                                   :value token
                                   :start (+ beg tok-beg)
                                   :end (+ beg tok-end))
                tokens)
          (let ((buf (buffer-substring (+ beg tok-beg ) (+ beg tok-end))))
            (unless (equal token buf)
              (assert (format "TOKEN '%s' DOES NOT MATCH BUFFER '%s'" token buf))))
          ))

      (setq beg (+ beg tok-end 1)
            first nil
            token nil
            tok-beg 0
            tok-end 0))
    (reverse tokens)))


(defsubst pop! (lst)
  (prog1 (car lst)
    (setcar lst (cadr lst))
    (setcdr lst (cddr lst))))

(defun aforth-parse-number (token)
  (let ((val (aforth-token-value token)))
    (when (or (string-match "^0x\\([0-9a-fA-F]+\\)$" val)
              (string-match "^0b\\([01]+\\)$" val)
              (string-match "^\\([0-9]+\\)$" val))
      (string-to-number (match-string 1 val)))))

(defun aforth-make-error (message token)
  )

(defun aforth-set-token (old type value &optional args start end)
  (if old
      (progn
        (setf (aforth-token-type old) type)
        (setf (aforth-token-value old) value)
        (setf (aforth-token-args old) args)
        (setf (aforth-token-start old) start)
        (setf (aforth-token-end old) end)
        old)
    (make-aforth-token :type type
                       :value value
                       :args args
                       :start start
                       :end end)))

(defun aforth-token->str (token)
  (format "aforth-token: type= %s, value=%s, args=%s, start= %s, end=%s"
          (aforth-token-type token)
          (aforth-token-value token)
          (aforth-token-args token)
          (aforth-token-start token)
          (aforth-token-end token)))

(defun aforth-error (msg)
  (message "aforth-error: %s" msg)
  (error msg))

(defun aforth-parse-region (beg end &optional tokens)
  ;; tokenize region BEG END-or use TOKENS from list. tokens are modified
  (let ((tokens (or tokens (aforth-get-token-list beg end)))
        next type out token)
    (while tokens
      (setq token (car tokens)
            tokens (cdr tokens)
            type (aforth-token-type token)
            val (aforth-token-value token)
            start (aforth-token-start token)
            end (aforth-token-end token))
      (cond ((not (stringp val))
             (error "expected string for :val field in token: %s" token))
            ((eq type 'comment)
             (push token out))
            ((member val '("org" "node"))
             (setq next (pop! tokens))
             (setq a (aforth-parse-number next))
             (if a
                 (push (aforth-set-token token 'directive val a start end)
                       out)
               (aforth-error (format "Expected number following token: %s, got: '%s'" val (aforth-token-value next)))))
            ((member val '(":" "::"))
             (setq next (pop! tokens))
             (setq name (aforth-token-value next))
             (if name
                 (push (aforth-set-token token
                                         (if (equal val ":") 'word-def 'compile-def)
                                         name nil start (aforth-token-end next))

                       out)
               (aforth-error (format "Expected definition name" val))))
            ((or (string-match "^0x\\([0-9a-fA-F]+\\)$" val)
                 (string-match "^0b\\([01]+\\)$" val)
                 (string-match "^\\([0-9]+\\)$" val))
             (push (aforth-set-token token 'number (string-to-number (match-string 1 val)) nil start end)
                   out))
            ((or (set-member? aforth-instruction-map val)
                 (set-member? aforth-port-map val))
             (push token out))

            ((set-member? aforth-directive-map val)
             (push (aforth-set-token token 'directive val nil start end)
                   out))
            ((set-member? boot-descriptors-map val)
             (push (aforth-set-token token 'boot-descriptor val nil start end)
                   out))

            ;;remote references
            ;; :args is the remote coord
            ;; :subToks is a decomposition of the tokens used for syntax highlighting only
            ;; if subToks is set then the fortification is done using that :overlay is
            ;; set from first member of toke
            ((string-match "^&?\\([^@\n ]+\\)@\\([0-9]+\\)$" val)

             (let* ((is-ref (eq (aref val 0) ?&))
                    (m1 (match-string 1 val))
                    (m2 (match-string 2 val))
                    (tstart (if is-ref (1+ start) start))
                    (m1-end (+ tstart (length m1)))
                    (m2-start (+ m1-end 1))
                    subtoks
                    )

               (setq token (aforth-set-token token (if is-ref 'r-reference 'r-call)
                                             m1
                                             (string-to-number m2)
                                             start end))

               (setq subtoks (list (cons tstart m1-end)
                                   (cons m1-end (+ m1-end 1))
                                   (cons m2-start (+ m2-start (length m2)))))
               (when is-ref
                 (setq subtoks (cons (cons start (1+ start)) subtoks)))

               (setf (aforth-token-subtoks token) subtoks)

               (push token out)))

            ((string-match "^&\\(.+\\)$" val)
             (push (aforth-set-token token 'reference (match-string 1 val)
                                     nil start end)
                   out))

            (t (push (aforth-set-token token 'call val
                                       nil start end)
                     out))))
    (nreverse out)))

(defun get-token-faces (type)
  (cond ((eq type 'r-reference) ;; & name @ node
         '(aforth-word-reference-face
           aforth-word-reference-face
           aforth-directive-face
           aforth-directive-face))
        ((eq type 'r-call)  ;; name @ node
         '(nil
           aforth-directive-face
           aforth-directive-face))
        (t (error "unknown sub token type: %s" type)))  )

(defun aforth-update-overlays (tokens)
  "list of TOKENS from `aforth-parse-region`"
  (let (start end face type o points faces point subtoks o)
    (dolist (token tokens)
      (setq type (aforth-token-type token)
            beg (aforth-token-start token)
            end (aforth-token-end token)
            subtoks (aforth-token-subtoks token)
            face (cond ((or (eq type 'directive)
                            (eq type 'compile-def))
                        aforth-directive-face)
                       ((eq type 'word-def)
                        aforth-word-face)
                       ((eq type 'number)
                        aforth-number-face)
                       ((eq type 'op)
                        aforth-instruction-face)
                       ((eq type 'comment)
                        aforth-comment-face)
                       ((eq type 'reference)
                        aforth-word-reference-face)
                       ((eq type 'boot-descriptor)
                        aforth-boot-descriptor-face)
                       ((or (eq type 'r-reference)
                            (eq type 'r-call)
                            (eq type 'call))
                        nil)
                       (t  (error "unknown token type: %s" type))))
      (when subtoks
        (dolist (face (get-token-faces type))
          (setq point (car subtoks)
                subtoks (cdr subtoks))
          (when face
            (setq o (make-overlay (car point) (cdr point)))
            (overlay-put o 'face face))))

      ;;for subtok tokens face is nil, create this overlay anyways for token position reference
      (assert (and beg end))
      (setq o (make-overlay beg end))
      (and face (overlay-put o 'face face))
      (setf (aforth-token-overlay token) o)
      )))

(defun aforth-parse-buffer ()
  (widen)
  (save-excursion
    (aforth-parse-region (point-min) (point-max))))

(defun aforth-remove-overlays (beg end)
  (dolist (o (overlays-in beg end))
    (delete-overlay o)
    ))

(defun aforth-update-region (beg end &optional _old-len)
  "After-change function updating overlays"
  (let ((start-time (current-time))
        (beg-line (save-excursion (goto-char beg) (line-beginning-position)))
        (end-line (save-excursion (goto-char end) (line-end-position)))
        tokens)
    ;;multiple token constructs may span multiple lines, but that style is ugly and not supported
    (aforth-remove-overlays beg-line end-line)
    (set-text-properties beg-line end-line nil)
    (setq tokens (aforth-parse-region beg-line end-line))
    (aforth-update-overlays tokens)
    (message "parsed region [%s %s] in %s seconds (%s tokens)" beg end (float-time (time-since start-time)) (length tokens)))
  )

(defun aforth-update-region (beg end &optional _old-len)
  (condition-case err
      (aforth-update-region3 beg end _old-len)
    (error (message "mbs error: %s" err))))

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

(setq aforth-mode-map
      (let ((map (make-sparse-keymap 'aforth-mode-map)))
        (define-key map (kbd "C-M-a") 'aforth-back-to-node)
        (define-key map (kbd "C-M-e") 'aforth-goto-next-node)
        map))

(define-derived-mode aforth-mode prog-mode "aforth2"
  "Major mode for editing aforth files"

  (setq imenu-create-index-function 'aforth-create-index)
  (widen)
  (use-local-map aforth-mode-map)
  (progn (jit-lock-register 'aforth-update-region)
         (setq aforth-buffer-words (make-hash-table)
               aforth-next-id 1))
  ;;(jit-lock-unregister 'aforth-update-region))

  (setq imenu-create-index-function 'aforth-create-index)

  (aforth-update-region (point-min) (point-max))
  (run-hooks 'aforth-mode-hook))

(add-to-list 'auto-mode-alist '("\\.aforth\\'" . aforth-mode))

(provide 'aforth-mode)
