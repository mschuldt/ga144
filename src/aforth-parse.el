;; -*- lexical-binding: t -*-

(require 'cl)
(require 'gv)

(defstruct aforth-token type value args start end overlay subtoks file)
(defstruct aforth-node coord code location)
(defstruct error-data message stage node line col input-type token)

(setq aforth-error-message nil)
(setq aforth-error-token nil)
(setq aforth-current-node nil)
(setq aforth-current-token nil)
(setq aforth-compile-stage nil)
(setq aforth-parse-current-file nil)
(setq aforth-parse-current-buffer nil)
(setq aforth-parse-currrent-point nil)
(setq aforth-compile-input-type nil)

(let ((base (file-name-directory (or buffer-file-name load-file-name))))
  (setq ga-lib-dirs (list (concat (file-name-directory (substring base 0 -1)) "lib/"))))

(defun ga-add-search-path (dir)
  (setq ga-lib-dirs (cons dir ga-lib-dirs)))

(defun aforth-set-token (old type value &optional args start end file)
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
                       :end end
                       :file file)))

(defun aforth-token->str (token)
  (format "aforth-token: type= %s, value=%s, args=%s, start= %s, end=%s file=%s"
          (aforth-token-type token)
          (aforth-token-value token)
          (aforth-token-args token)
          (aforth-token-start token)
          (aforth-token-end token)
          (aforth-token-file token)))

(defsubst aforth-token-delimiter-p (c)
  (or (eq c ? )
      (eq c ?\n)
      (eq c ?\[)
      (eq c ?\])
      (eq c ?\t)))

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

(defun aforth-tokenize-region (beg end)
  (setq aforth-compile-stage "tokenizing")
  (setq aforth-parse-currrent-point beg)
  (setq aforth-parse-current-file buffer-file-name)
  (let ((str (string-to-list (buffer-substring-no-properties beg end)))
        (tok-beg 0)
        (tokens '())
        token tok-end first
        next-token-face
        next-token-def-p
        aforth-reading-node)

    (while str
      ;;whitespace
      (while (and str
                  (not first))
        (setq c (car str)
              str (cdr str)
              tok-beg (1+ tok-beg))
        (if (eq c ?\n)
            (push (make-aforth-token :type 'newline
                                     :value ""
                                     :start nil
                                     :end nil
                                     :file aforth-parse-current-file)
                  tokens)
          (unless (aforth-token-delimiter-p c)
            (setq first c
                  tok-end tok-beg
                  tok-beg (1- tok-beg)))))
      ;;comment
      (when (or (eq first ?\()
                (eq first ?\\)) ;; bowman comment syntax
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
        (setq value (concat (reverse token)))

        (push (make-aforth-token :type 'comment
                                 :value value
                                 :start (+ beg tok-beg)
                                 :end (+ beg tok-end)
                                 :file aforth-parse-current-file)
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
                                   :end (+ beg tok-end)
                                   :file aforth-parse-current-file)
                tokens)
          (cond ((equal token "node")
                 (setq aforth-reading-node t))
                (aforth-reading-node
                 (setq aforth-current-node token
                       aforth-reading-node nil)))

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

(defun aforth-parse-number (tok)
  (let* ((str (if (stringp tok) tok
                (aforth-token-value tok)))
         (base 10)
         (neg (when (and (> (length str) 0)
                         (eq (aref str 0) ?-))
                (setq str (subseq str 1))
                t))
         (str (if (and (> (length str) 2)
                       (eq (aref str 0) ?0)
                       (or (and (eq (aref str 1) ?x)
                                (setq base 16))
                           (and (eq (aref str 1) ?b)
                                (setq base 2))))
                  (subseq str 2)
                str))
         n)
    (if (string-match "^0+$" str)
        0
      (setq n (string-to-number str base))
      (if (= n 0)
          nil
        (if neg (- n)
          n)))))

(defun aforth-find-lib-file (filename)
  (if (file-exists-p filename)
      filename
    (let ((dirs ga-lib-dirs)
          name found)
      (while dirs
        (setq name (concat (file-name-as-directory (car dirs)) filename)
              dirs (cdr dirs))
        (when (file-exists-p name)
          (setq dirs nil
                found t)))
      (and found name))))

(defun aforth-include-file (filename &optional no-comments)
  (let ((file (aforth-find-lib-file filename)))
    (if file
        (with-temp-buffer
          (let ((buffer-file-name file)
                (aforth-parse-current-buffer (current-buffer))
                (aforth-error-message nil)
                (aforth-current-node nil)
                (aforth-current-token nil)
                (aforth-compile-stage nil)
                (aforth-parse-buffer nil))
            (insert-file-contents-literally file)
            ;;TODO: check for parse error
            (aforth-parse-region (point-min) (point-max) nil no-comments)))
      (aforth-compile-error (format "included file does not exist: %s" filename)))))

(defun aforth-load-lisp-file (filename)
  (let ((file (aforth-find-lib-file filename)))
    (if file
        (load (expand-file-name file) nil t t)
      (aforth-compile-error (format "unknown file: %s" filename)))))

;; ignore-nops is used in bowman mode to ignore the trailing nops in a word
;; otherwise the nops in a line like " ; . . . " will get compiled into a seporate wod
;; because the compiler automatically word aligns after ';' or 'ex'
(setq ignore-nops nil)

(defun aforth-parse-region (beg end &optional tokens no-comments)
  (setq aforth-compile-stage "parsing")
  ;; tokenize region BEG END-or use TOKENS from list. tokens are modified
  (let ((tokens (or tokens (aforth-tokenize-region beg end)))
        next type out token val a)
    (while tokens
      (setq token (car tokens)
            aforth-current-token token
            tokens (cdr tokens)
            type (aforth-token-type token)
            val (aforth-token-value token)
            start (aforth-token-start token)
            end (aforth-token-end token))
      (cond ((not (stringp val)) ;;TODO: should not raise an error, only return error objects. otherwise region fontification gets messed up
             (aforth-compile-error (format "expected string for :val field in token: %s" token)))
            ((eq type 'comment)
             (unless no-comments
               (push token out)))
            ((eq type 'newline)
             (when bowman-format
               (setq ignore-nops nil)
               (push (aforth-set-token token 'directive "..")
                     out)))
            ((member val '("org" "node"))
             (setq next (pop! tokens))
             (setq a (aforth-parse-number next))
             (when (equal val "node")
               (setq aforth-current-node a))
             (if a
                 (push (aforth-set-token token 'directive val a start (aforth-token-end next))
                       out)
               (aforth-compile-error (format "Invalid %s argument '%s'" val (aforth-token-value next)))))
            ((member val '(":" "::"))
             (setq next (pop! tokens))
             (setq name (aforth-token-value next))
             (if name
                 (push (aforth-set-token token
                                         (if (equal val ":") 'word-def 'compile-def)
                                         name nil start (aforth-token-end next))

                       out)
               (aforth-compile-error (format "Expected %s definition name" val))))
            ((string= val "include")
             (setq next (pop! tokens))
             (setq name (aforth-token-value next))
             (if name
                 (if (string= (file-name-extension name) "el")
                     (aforth-load-lisp-file name)
                   (setq out (append (nreverse (setq _x (aforth-include-file name no-comments))) out)))
               (aforth-compile-error "Expected include filename")))

            ((or (string-match "^-?0x\\([0-9a-fA-F]+\\)$" val)
                 (string-match "^-?0b\\([01]+\\)$" val)
                 (string-match "^\\(-?[0-9]+\\)$" val))
             (push (aforth-set-token token 'number (aforth-parse-number val) nil start end)
                   out))

            ((or (set-member? aforth-directive-map val)
                 (set-member? aforth-port-map val)
                 (equal val "next")
                 (or (and bowman-format
                          (set-member? io-place-names-map val)))
                 )
             (push (aforth-set-token token 'directive val nil start end)
                   out))

            ((set-member? aforth-instruction-map val)
             (when (and bowman-format
                        (member val instructions-using-rest-of-word))
               (setq ignore-nops t))
             (unless (and ignore-nops
                          (equal val "."))
               (push token out)))

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
                                             (aforth-parse-number m2)
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

            ((string-match "^!!\\(.+\\)$" val)
             (push (aforth-set-token token 'funcall (match-string 1 val)  nil start end)
                   out))

            (t (push (aforth-set-token token 'call val
                                       nil start end)
                     out))))
    (nreverse out)))

(defun aforth-parse-nodes (beg end &optional tokens no-comments)
  ;; parse nodes and library words from region
  (aforth-begin-parse)
  (let ((tokens (or tokens (aforth-parse-region beg end nil no-comments)))
        nodes current-node current-code type)
    (setq aforth-compile-stage "parsing nodes")
    (dolist (token tokens)
      (setq type (aforth-token-type token))
      (cond ((and (or (eq type 'word-def)
                      (eq type 'compile-def))
                  (not current-node))
             (aforth-compile-error "TODO - word defined outside node")
             ;; word defined outside node
             ;;DOING: need some struct like aforth-node but for words
             )
            ((equal (aforth-token-value token) "node")
             (progn (when current-node
                      ;; (when (= (length current-code) 0)
                      ;;   (aforth-compile-error (format "node %s body is empty" (aforth-node-coord current-node))
                      ;;                         aforth-compile-error))
                      (setf (aforth-node-code current-node) (nreverse current-code))
                      (setq nodes (cons current-node nodes)))
                    (setq current-node (make-aforth-node :coord (aforth-token-args token)
                                                         :location (aforth-token-start token))

                          current-code nil)))
            (t (setq current-code (cons token current-code)))))
    (when current-node
      (setf (aforth-node-code current-node) (nreverse current-code))
      (setq nodes (cons current-node nodes)))
    (nreverse nodes)))

(defun aforth-parse-buffer ()
  (aforth-begin-parse)
  (save-excursion
    (save-restriction
      (widen)
      (aforth-parse-region (point-min) (point-max)))))

(defun aforth-parse-string (str)
  (with-temp-buffer
    (insert str)
    (aforth-parse-buffer)))

(defun aforth-begin-parse ()
  ;; clears old global parse state
  (setq aforth-parse-current-file buffer-file-name
        aforth-parse-current-buffer (current-buffer)
        aforth-error-message nil
        aforth-current-node nil
        aforth-current-token nil
        aforth-compile-stage nil
        aforth-parse-buffer nil))

(defun aforth-get-error-data ()
  (let ((point (cond (aforth-current-token
                      (aforth-token-start aforth-current-token))
                     (aforth-parse-currrent-point aforth-parse-currrent-point)
                     (t 0)))
        line col)
    (unless (numberp point) (setq point 0))
    (with-current-buffer aforth-parse-current-buffer
      (widen)
      (goto-char point)
      (setq col (current-column))
      ;;(message (buffer-substring (progn (beginning-of-line) (point))
      ;;                           (progn (end-of-line) (point))))
      ;;(message (concat (make-string (- point (progn (beginning-of-line) (point)) 1) ? )
      ;;                 "^"))
      (end-of-line)
      (setq line (count-lines 1 (point))))

    (make-error-data :message aforth-error-message
                     :node (cond ((numberp aforth-current-node)
                                  aforth-current-node)
                                 ((stringp aforth-current-node)
                                  (aforth-parse-number aforth-current-node)))
                     :stage (or aforth-compile-stage "")
                     :line line
                     :col col
                     :token aforth-current-token)))

(defun aforth-print-error-data (compiled)
  (let ((data (compiled-error-info compiled)))
    (message "Error: %s" (error-data-message data))
    (when (error-data-node data)
      (message "Node: %s" (error-data-node data)))
    (when (error-data-line data)
      (message "Line: %s" (error-data-line data)))
    ;;(when (error-data-stage data)
    ;;  (message "While %s" (error-data-stage data)))
    ))

(defun aforth-compile-error (msg &optional token)
  (setq aforth-error-message msg)
  ;; if the error is post compilation the token needs to be specified
  ;; or the error position will always be at the end of the buffer
  (setq aforth-error-token token)
  (throw 'aforth-error nil))


(provide 'aforth-parse)
;;(require 'aforth-mode)
