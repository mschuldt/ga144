;; -*- lexical-binding: t -*-

;; elisp aforth-compiler entrance

(require 'rkt)
(require 'aforth-parse)

(rkt-require "compile.rkt")		;
(rkt-require "bootstream.rkt")

(setq bowman-format nil)

(defun aforth-compile-buffer (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    ;;(and buffer-file-name (save-buffer))
    (if (and buffer-file-name
             (string= (file-name-extension buffer-file-name) "ga"))
        (let ((expanded (shell-command-to-string (concat "m4 " buffer-file-name))))
          (with-temp-buffer
            (insert expanded)
            (convert-bowman-to-aforth)
            (aforth-compile-aforth-buffer)))
      (aforth-compile-aforth-buffer))))

(defun aforth-compile-aforth-buffer ()
  ;; compile the current buffer
  (when DEBUG? (printf "DEBUG PRINT MODE\n"))
  (save-excursion
    (let (parsed-nodes ret locations)
      (reset!)
      (catch 'aforth-error
        (setq parsed-nodes (aforth-parse-nodes (point-min) (point-max) nil 'no-comments))
        (setq aforth-compile-stage "compiling")
        (dolist (node parsed-nodes)
          (setq aforth-current-node (aforth-node-coord node))
          (push (cons aforth-current-node (aforth-node-location node))
                locations)
          (start-new-node aforth-current-node)
          (setq current-token-list (aforth-node-code node))

          (while current-token-list
            (setq aforth-current-token (read-tok))
            (compile-token aforth-current-token)))
        (when memory
          (fill-rest-with-nops) ;;make sure last instruction is full
          (set-current-node-length))
        (when current-node
          (set-node-address-cells! current-node address-cells))

        (when DEBUG? (display-compiled (compiled used-nodes nil)))

        ;; errors from this point on are not associated with line numbers
        (setq current-tok-line nil
              current-tok-col nil)

        (setq aforth-compile-stage "checking")
        (mapc 'check-for-undefined-words used-nodes)

        (setq aforth-compile-stage "finalizing")
        (setq used-nodes (mapcar 'remove-address-cells used-nodes))
        ;;(setq used-nodes (mapcar 'aforth-trim-memory used-nodes))
        (setq ret (compiled used-nodes nil locations)))

      (or ret
          (compiled nil (aforth-get-error-data))))))

(defun aforth-compile (code) ;;shadows racket version
  (setq aforth-compile-input-type 'string)
  (with-temp-buffer
    (insert code)
    (aforth-compile-buffer)))

(setq _last-read-tok nil)

(defun read-tok ()
  (when current-token-list
    (let ((token (car current-token-list))
          token-type)
      (set! current-token-list (cdr current-token-list))
      (when token
        (setq prev-current-tok-line current-tok-line
              prev-current-tok-col current-tok-col
              current-tok-line (aforth-token-start token) ;;TODO: line/col instead of buffer positions
              current-tok-col (aforth-token-end token)))
      (setq _last-read-tok token)
      token)))

(defun unread-last-tok ()
  (assert elisp?)
  (when _last-read-tok
    (setq current-token-list (cons _last-read-tok current-token-list))))

(defun read-tok-name ()
  (let ((tok (read-tok)))
    (and tok (aforth-token-value tok))))

;;(fset 'rkt-compile-token 'compile-token)

(defun compile-token (token)
  (let ((token-type (aforth-token-type token))
        (token-val (aforth-token-value token))
        (token-args (aforth-token-args token))
        func)
    (setq current-token-buffer-position (list (aforth-token-file token)
                                              (aforth-token-start token)
                                              (aforth-token-end token)))
    (setq current-token token)
    (cond ((or (eq token-type 'word-def)
               (eq token-type 'compile-def))
           (setq func (get-directive token-type))
           (unless func
             (err (format "Expected directive for: '%s, token=%s'\n" token-type token)))
           (funcall func token-val))

          ((or (eq token-type 'directive)
               (eq token-type 'boot-descriptor))
           (setq func (get-directive token-val))
           (unless func
             (err (format "Expected directive for: '%s, token=%s'\n" token-val token)))
	   (unless (listp token-args)
	     (setq token-args (list token-args)))
           (apply func token-args))

          ((or (setq func (get-directive token-type))
               (and (eq token-type 'op)
                    (setq func (get-directive token-val))))
           (funcall func))

          ((eq token-type 'op)
           (compile-instruction! token-val))

          ((eq token-type 'number)
           (compile-constant! token-val))

          ((eq token-type 'r-call)
           (compile-remote-call! token-val token-args))

          ((eq token-type 'r-reference)
           (compile-remote-word-ref! token-val token-args))

          ((eq token-type 'reference)
           (compile-word-ref! token-val))

          ((eq token-type 'call)
           (compile-call! token-val))

          ((eq token-type 'funcall)
           (compile-funcall! token-val))

          (t (error "unrecognized token type: %s" token)))
    (setq current-token-buffer-position nil)
    (setq current-token nil)))

(defun aforth-compile-file (filename)
  (setq aforth-compile-input-type 'file)
  (with-temp-buffer
    (if (string= (file-name-extension in-file) "ga")
        (open-file-from-bowman-format filename)
      (insert-file-contents-literally filename))
    (setq buffer-file-name filename)
    (aforth-compile-buffer)))

(defun open-file-from-bowman-format (filename)
  (setq bowman-format t)
  (setq filename (expand-file-name filename))
  (insert (shell-command-to-string (concat "m4 " filename)))
  (convert-bowman-to-aforth)
  ;;(write-file (concat filename ".aforth"))
  )

(defun convert-bowman-to-aforth ()
  (goto-char 1)
  (while (re-search-forward "^ *-+ \\([0-9]+\\) -+ *$" nil t)
    (replace-match (format "node %s" (match-string 1))))

  (dolist (d '("NORTH" "SOUTH" "EAST" "WEST"))
    (goto-char 1)
    (while (re-search-forward d nil t)
      (replace-match (downcase d) t))))

(defmacro compiler-binop(op)
  `(push (funcall ',op (pop stack) (pop stack)) stack))

(defun aforth-trim-memory (node)
  (let ((mem (vector->list (nreverse (node-mem node)))))
    (while (equal (car mem) [nil nil nil nil])
      (setq mem (cdr mem)))
    (set-node-mem! node (list->vector (nreverse mem)))
    node))

(defun compile-file-to-bootstream (file bootstream-type)
  (let* ((compiled (aforth-compile-file file))
         (assembled (assemble compiled))
         (bootstream (make-bootstream assembled bootstream-type)))
    (sget-convert bootstream)))

(defun aforth-parse-string (str &optional no-comments)
  (with-temp-buffer
    (insert str)
    (aforth-parse-nodes (point-min) (point-max) nil no-comments)))

(provide 'aforth-compile)
