(defstruct aforth-token type value args start end overlay subtoks)

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

(defun aforth-toekenize-region (beg end)

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

(defun aforth-parse-number (token)
  (let ((val (aforth-token-value token)))
    (when (or (string-match "^0x\\([0-9a-fA-F]+\\)$" val)
              (string-match "^0b\\([01]+\\)$" val)
              (string-match "^\\([0-9]+\\)$" val))
      (string-to-number (match-string 1 val)))))


(defun aforth-parse-region (beg end &optional tokens)
  ;; tokenize region BEG END-or use TOKENS from list. tokens are modified
  (let ((tokens (or tokens (aforth-toekenize-region beg end)))
        next type out token)
    (while tokens
      (setq token (car tokens)
            tokens (cdr tokens)
            type (aforth-token-type token)
            val (aforth-token-value token)
            start (aforth-token-start token)
            end (aforth-token-end token))
      (cond ((not (stringp val)) ;;TODO: should not raise an error, only return error objects. otherwise region fontification gets messed up
             (error "expected string for :val field in token: %s" token))
            ((eq type 'comment)
             (push token out))
            ((member val '("org" "node"))
             (setq next (pop! tokens))
             (setq a (aforth-parse-number next))
             (if a
                 (push (aforth-set-token token 'directive val a start (aforth-token-end next))
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

(defun aforth-parse-buffer ()
  (widen)
  (save-excursion
    (aforth-parse-region (point-min) (point-max))))

(provide 'aforth-parse)
