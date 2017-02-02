;; -*- lexical-binding: t -*-

;; racket comparability layer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'define' macro

(defun racket-gather-local-vars (form &optional exclude)
  (let (vars)
    (cond ((not (consp form)) nil)

          ((and (eq (car form) 'define)
                (symbolp (cadr form)))
           (unless (member (cadr form) exclude)
             (push (cadr form) vars))
           (setq vars (append vars (racket-gather-local-vars (caddr form) exclude))))

          ((and (eq (car form) 'define)
                (consp (cadr form)))
           (unless (member (cadr form) exclude)
             (push (caadr form) vars))
           (setq vars (append vars (racket-gather-local-vars (caddr form) exclude))))

          ((or (eq (car form) 'let)
               (eq (car form) 'let*))
           (setq vars (append vars (racket-gather-local-vars (caddr form)
                                                             (append exclude
                                                                     (mapcar (lambda (x)
                                                                               (if (consp x) (car x) x))
                                                                             (cadr form)))))))
          (t (dolist (x form)
               (setq vars (append vars (racket-gather-local-vars x exclude))))))
    (delete-dups vars)))


(defun racket-translate-define-body (form &optional local-functions)
  (cond ((not (consp form)) form)
        ((eq (car form) 'define)
         (if (consp (cadr form))
             `(setq ,(caadr form) (lambda ,@(racket-make-define-body (cadr form) (cddr form) (cons (caadr form) local-functions))))
           (cons 'setq (racket-translate-define-body (cdr form) local-functions))))

        ((member (car form) local-functions)
         (cons 'funcall (cons (car form) (racket-translate-define-body (cdr form) local-functions))))

        (t (let (body)
             (dolist (x form)
               (when (and (consp x)
                          (eq (car x) 'define)
                          (consp (cadr x)))
                 (setq local-functions (cons (caadr x) local-functions)))
               (push (racket-translate-define-body x local-functions) body))
             (reverse body)))))


(defun racket-make-define-body (form body &optional local-functions)
  (let ((args (cdr form))
        local-vars name positional optional defaults)
    (dolist (a args)
      (if (vectorp a)
          (progn
            (assert (= (length a) 2))
            (setq name (aref a 0))
            (assert (symbolp name))
            (push name optional)
            (push `(setq ,name (or ,name ,(aref a 1))) defaults))
        (assert (symbolp a))
        (push a positional)))
    (setq local-vars (racket-gather-local-vars body (append positional optional)))
    (setq body (racket-translate-define-body body local-functions))
    (when local-vars
      (setq body `((let ,local-vars ,@body))))

    `(,(append (reverse positional) (if optional '(&optional) nil) (reverse optional))
      ,@(reverse defaults)
      ,@body)))

(defmacro define (form &rest body)
  (if (consp form)
      (cons 'defun (cons (car form)  (racket-make-define-body form body)))
    (assert (symbolp form))
    `(defvar ,form ,@body)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flow control

(defmacro begin (&rest body)
  (cons 'progn body))

(defconst else t) ;;for 'cond'

(defun racket-for-loop-make-sequence (x)
  (cond ((listp x) x)
        ((stringp x) (cdr (butlast (split-string x ""))))
        ((vectorp x) (mapcar 'identity x))
        ((numberp x) (number-sequence 0 (1- x)))
        (t (error "unimplemented"))))

(defun racket-make-for-loop (spec body collect)
  (let* ((vars (mapcar 'car spec))
         (sequences (mapcar 'cadr spec))
         (seq-names (mapcar (lambda (v) (cons v (intern (concat (symbol-name v) "__rkt_for_loop_sequence")))) vars))
         (seqs (mapcar 'cdr seq-names))
         (setq-form  (let (v s forms)
                       (dolist (vs seq-names)
                         (setq v (car vs)
                               s (cdr vs))
                         (push `(cdr ,s) forms)
                         (push s forms)
                         (push `(car ,s) forms)
                         (push v forms))
                       (cons 'setq  forms)))
         (let-vars (append (mapcar* (lambda (name val)
                                      `(,(cdr name) (racket-for-loop-make-sequence ,val)))
                                    seq-names sequences)
                           vars))
         (condition (if (> (length seqs) 1) (cons 'and seqs) (car seqs))))

    (if collect
        `(let ,(append let-vars '(__rkt-list-ret__))
           (while ,condition
             ,setq-form
             (push (progn ,@body) __rkt-list-ret__))
           (reverse __rkt-list-ret__))
      `(let ,let-vars
         (while ,condition
           ,setq-form
           ,@body)))))

(defmacro for (spec &rest body)
  (racket-make-for-loop spec body nil))

(defmacro for/list (spec &rest body)
  (racket-make-for-loop spec body t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vectors
(defalias 'vector-set! 'aset)
(defalias 'vector-ref 'aref)
(defalias 'vector-length 'length)
(defalias 'vector? 'vectorp)
(defalias 'vector-map 'mapcar)
(defalias 'vector-append 'vconcat)
(defalias 'vector-copy 'copy-sequence)
(defalias 'vector-member 'position)
(defalias 'list->vector 'vconcat)

(defun vector->list (v)
  (mapcar 'identity v))

(defun vector-map! (fn vec)
  (dotimes (i (length vec))
    (aset vec i (funcall fn (aref vec i))))
  vec)


;; same: make-vector
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lists
(defalias 'list? 'listp)

(defun filter (func list)
  (let ((newlist ()))
    (dolist (e list)
      (if (funcall func e)
          (setq newlist (cons e newlist))))
    (reverse newlist)))

(defmacro map (fn lst)
  (if (symbolp fn)
      `(mapcar ',fn lst)
    `(mapcar fn lst)))

(defalias 'null? 'null)
(defalias 'cons? 'consp)

(defun list->set (lst)
  (let ((s (make-set)))
    (dolist (x lst)
      (puthash x t s))
    s))

(defun string-join (sequence separator)
  (mapconcat 'identity sequence separator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash tables
(defun make-hash (&optional alist)
  (let ((ht (make-hash-table)))
    (dolist (x alist)
      (puthash (car x) (cdr x) ht))
    ht))

(defsubst hash-ref (key table)
  (gethash table key))

(defsubst hash-set! (table key value)
  (puthash key value table))

(defun hash-has-key? (hash key)
  (let ((default (list 'default)))
    (not (eq (gethash key hash default) default))))

(defun hash->list (hash)
  (let (ret)
    (maphash (lambda (k v)
               (push (cons k v) ret))
             hash)))

(defalias 'hash-values 'hash-table-values)
(defalias 'hash-keys 'hash-table-keys)

;; raacket remove only removes the first item in the list, elisp version removes all matches

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sets

;; the racket function 'set' cannot be used in elisp, use make-set instead

(setq racket-magic-set-key '__racket_set_key__
      racket-magic-set-value '__racket_set_value__)

(defun make-set (&rest items)
  (let ((s (make-hash-table)))
    (puthash racket-magic-set-key racket-magic-set-value s)
    (dolist (x items)
      (puthash x t s))
    s))

(defun set? (s)
  (eq (gethash racket-magic-set-key s) racket-magic-set-value))

(defun set-member? (s item)
  (gethash item s))

(defun set->list (s)
  (let (lst)
    (maphash (lambda (k v)
               (unless (eq k racket-magic-set-key)
                 (push k lst)))
             s)
    lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strings
;;; same: make-string

(defalias 'string? 'stringp)
(defalias 'string-length 'length)
(defalias 'string->number 'string-to-number)
(defalias 'string-append 'concat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; numbers

(defalias 'sub1 '1+)
(defalias 'add1 '1-)
(defalias 'number? 'numberp)
(defun range (from to)
  (number-sequence from (1- to)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conditions
(defalias 'equal? 'equal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vectors

(defalias 'vector-set! 'aset)
(defalias 'vector-ref 'aref)
(defalias 'vector? 'vectorp)
(defalias 'vector-length 'length)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bool

(defconst false nil)
(defconst true t)
(defsubst void () nil)

(defsubst eof 'eof) ;;functions that use this should not be called from elisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mutable cons

(defalias 'mcar  'car)
(defalias 'set-mcar! 'setcar)
(defalias 'set-mcdr! 'setcdr)
(defalias 'mcdr 'cdr)
(defalias 'mcons 'cons)
(defalias 'mpair? 'consp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stucts

(defun _struct-get-args (fields)
  (let (positional optional is-opt arg)
    (dolist (field fields)
      (if (symbolp field)
          (if optional
              (error "positional arg following optional args is not allowed")
            (push field positional))
        ;;else
        (assert (and (consp field)
                     (> (length field) 1)))
        (setq arg (pop field)
              is-opt nil)

        (dolist (opt field)
          (unless (or (eq opt 'auto)
                      (eq opt 'mutable)) ;;ignoring mutable optional - everything is mutable
            (error "field option '%s' is not supported" opt))
          (when (eq opt 'auto)
            (setq is-opt t)))
        (when is-opt
          (push arg optional))
        ))
    (cons (nreverse positional) (nreverse optional))))

(defmacro struct (name fields &rest options)
  ;;field is either a symbol or [symbol <option>]
  ;; <option> is 'auto' or 'mutable'
  (let* ((struct-name (intern (symbol-name name)))
         (_args (_struct-get-args fields))
         (pos (car _args))
         (opt (cdr _args))
         (args (append pos opt))
         (n-pos (length pos))
         (n-opt (length opt))
         (max-args (+ n-pos n-opt))
         (test-name (intern (format "%s?" struct-name))))
    ;;ignoring OPTIONS
    `(progn

       (defun ,struct-name (&rest args)
         (let ((n-args (length args)))
           (when (< n-args ,n-pos)
             (error ,(format "expected at least %s args, got %%s" n-pos) n-args))
           (when (> n-args ,max-args)
             (error ,(format "expected at most %s args, got %%s" max-args) n-args))
           (vconcat ,(vector struct-name) args (make-vector (- ,max-args n-args) nil))))

       (defun ,test-name (thing)
         (and (vectorp thing)
              (= (length thing) ,(1+ max-args))
              (eq (aref thing 0) ',struct-name)))

       ,@(mapcar* (lambda (arg i)
                    `(defun ,(intern (concat "set-" (symbol-name struct-name) "-" (symbol-name arg) "!")) (s val)
                       (unless (,test-name s)
                         (error ,(format "expected struct type '%s'. got this instead: %%s" struct-name) s))
                       (aset s ,i val)))
                  args (number-sequence 1 max-args))

       ,@(mapcar* (lambda (arg i)
                    `(defun ,(intern (concat (symbol-name struct-name) "-" (symbol-name arg))) (s)
                       (unless (,test-name s)
                         (error ,(format "expected struct type '%s'. got this instead: %%s" struct-name) s))
                       (aref s ,i)))
                  args (number-sequence 1 max-args))
       )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; printing

(defun rkt-format (fmt &rest args)
  ;; incomplete but good enough!
  (dolist (x '(("~~" . "~")
               ("%" . "%%")
               ("~a" . "%s")))
    (setq fmt (replace-regexp-in-string (regexp-quote (car x)) (cdr x) fmt)))
  (apply 'format fmt args))

(defun printf (fmt &rest args)
  (let ((s (apply 'rkt-format fmt args)))
    (when (stringp s)
      (message "%s" s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file loading

(setq elisp? t
      compatibility/defmacro nil)

(defun all-defined-out () nil)

(defun rkt-load (file)
  (let ((lexical-binding t)
        (racket? nil)
        (elisp? t)
        (compatibility/defmacro nil)) ;; to prevent void-variable errors
    (with-temp-buffer
      (insert-file-contents-literally file)
      (setq buffer-file-name file)
      (goto-char (point-min))
      (forward-line) ;; skip  #lang ...
      ;; flet is _not_ semantically the same as cl-flet which does not rebind the symbol same so does not work when eval is called
      (flet ((require (&rest files) (rkt-require files))
             (provide (&rest syms) nil))
        (condition-case err
            (eval-region (point) (point-max))
          (error (message (format "rkt-load error. file='%s', error=%s" file err)))))
      (setq buffer-file-name nil)
      )))

(defun rkt-require (files)
  (when (stringp files)
    (setq files (list files)))
  (dolist (file files)
    (when (and (stringp file)
               (not (equal (car (last (split-string file "/"))) "rkt-to-el.rkt")))
      (rkt-load (concat (file-name-directory buffer-file-name) file)))))


(provide 'el-to-rkt)
