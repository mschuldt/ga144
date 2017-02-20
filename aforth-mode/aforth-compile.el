;; -*- lexical-binding: t -*-

;; elisp aforth-compiler entrance

(add-to-list 'load-path "~/a/projects/ga144")
(require 'rkt)
(require 'aforth-parse)

(rkt-require "../compiler/compile.rkt")
(rkt-require "../compiler/bootstream.rkt")

(defun aforth-compile-buffer (&optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (reset!)
  (dolist (node (aforth-parse-nodes (point-min) (point-max) nil 'no-comments))
    (start-new-node (aforth-node-coord node))
    (setq current-token-list (aforth-node-code node))
    (while current-token-list
      (compile-token (read-tok))))

  (when memory
    (fill-rest-with-nops) ;;make sure last instruction is full
    (set-node-len! current-node (sub1 next-addr)))

  (when DEBUG? (display-compiled (compiled used-nodes)))

  ;; errors from this point on are not associated with line numbers
  (setq current-tok-line nil
        current-tok-col nil)

  (map check-for-undefined-words used-nodes)

  (compiled (map remove-address-cells used-nodes)))


(defun aforth-compile (code) ;;shadows racket version
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
    (setq current-token token)
    (when DEBUG? (printf "compile-token(~a) [~a  ~a  ~a]\n"
                         (token-tok token) current-addr current-slot next-addr))
    (cond ((or (eq token-type 'word-def)
               (eq token-type 'compile-def))
           (assert (setq func (get-directive token-type)))
           (funcall func token-val))

          ((or (eq token-type 'directive)
               (eq token-type 'boot-descriptor))
           (assert (setq func (get-directive token-val)))
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
           (error "TODO: compiling remote references")
           )
          ((eq token-type 'reference)
           (compile-remote-word-ref! token-val token-args))

          ((eq token-type 'call)
           (compile-call! token-val))

          (t (error "unrecognized token type: %s" token)))
    (setq current-token nil)))

(defun aforth-compile-file (filename)
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (aforth-compile-buffer)))

(provide 'aforth-compile)
