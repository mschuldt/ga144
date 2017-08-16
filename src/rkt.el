;; -*- lexical-binding: t -*-

;; racket compatibility layer

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 'define' macro

(setq print-receive-function-warnings nil)

(defun racket-gather-local-vars (form &optional exclude)
  "collects variables defined in FORM with `define' but excludes those bound with let macros"
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

(defsubst fn (thing)
  "declare THING to be a function"
  thing)

(setq maybe-undefined (make-hash-table))

(defun racket-translate-define-body (form &optional local-functions fn-names)
  "translates forms in the body of a Racket define macro FORM"
  (cond ((not (consp form)) form) ;;nothing to do
        ((eq (car form) 'define)
         ;;function or varable definition
         ;; replace 'define' with 'setq',
         (if (consp (cadr form))
             `(setq ,(caadr form) (lambda ,@(racket-make-define-body (cadr form) (cddr form)
                                                                     (cons (caadr form) local-functions)
                                                                     (cons (caadr form) fn-names))))
           (cons 'setq (racket-translate-define-body (cdr form) local-functions fn-names))))

        ((member (car form) local-functions)
         ;;translate call to local function -
         (when (and (member (car form) fn-names)
                    print-receive-function-warnings)
           (message "Warning: recursive call detected: %s (in file %s)" (car form) buffer-file-name))
         (cons 'funcall (cons (car form) (racket-translate-define-body (cdr form) local-functions fn-names))))
        (t (let (body)
             (dolist (x form)
               (if (and (consp x)
                        (>= (length x) 1)
                        (symbolp (car x))
                        (not (fboundp (car x))))
                   (puthash (car x) t maybe-undefined))

               (cond ((and (consp x)
                           (eq (car x) 'define)
                           (consp (cadr x)))
                      ;; (define (f ...) ...)
                      (setq local-functions (cons (caadr x) local-functions))
                      (push (racket-translate-define-body x local-functions fn-names) body))
                     ((and (consp x)
                           (= (length x) 3)
                           (or (eq (car x) 'define)
                               (eq (car x) 'set!)
                               (eq (car x) 'setq))
                           (symbolp (cadr x))
                           (consp (caddr x))
                           (or (eq (caaddr x) 'lambda)
                               (eq (caaddr x) 'fn)))
                      ;; (set! <var> <func>)
                      (setq local-functions (cons (cadr x) local-functions))
                      (push (list 'setq (cadr x) (racket-translate-define-body (caddr x)
                                                                               (cons (cadr x) local-functions)
                                                                               fn-names))
                            body))
                     ;; TODO: let bound functions
                     (t (push (racket-translate-define-body x local-functions fn-names) body))))
             (reverse body)))))

(defun racket-make-define-body (form body &optional local-functions fn-names)
  ;; (define (symbol args...) BODY...)
  ;; LOCAL-FUNCTIONS is a list of functions defined within this function
  (let ((args (cdr form))
        local-vars name positional optional defaults)
    (dolist (a args)
      (if (consp a)
          (progn
            (assert (= (length a) 2))
            (setq name (car a))
            (assert (symbolp name))
            (push name optional)
            (push `(setq ,name (or ,name ,(cadr a))) defaults))
        (assert (symbolp a))
        (push a positional)))
    (setq local-vars (racket-gather-local-vars body (append positional optional)))
    (setq body (racket-translate-define-body body local-functions fn-names))
    (when local-vars
      (setq body `((let ,local-vars ,@body))))

    `(,(append (reverse positional) (if optional '(&optional) nil) (reverse optional))
      ,@(reverse defaults)
      ,@body)))

(defmacro define (form &rest body)
  (if (consp form) ;;function definition
      (progn
        (when (and (fboundp (car form))
                   (not (get (car form) 'is-racket-fn)))
          (message "WARNING: 'define' attempting to overwrite function value of '%s' in %s"
                   (car form) buffer-file-name))
        ;; else: ok to define
        (put (car form) 'is-racket-fn t)
        `(progn ,(cons 'defun (cons (car form) (racket-make-define-body form body nil (list (car form)))))
                (setq ,(car form) ',(car form))))


    (assert (symbolp form))
    ;;else: variable definition

    (when (and (boundp form)
               (not (get form 'is-racket-var)))
      (message "WARNING: 'define' attempting to overwrite value of '%s' in %s"
               form buffer-file-name))
    ;;else: ok to define
    (put form 'is-racket-var t)
    `(progn (defvar ,form nil)
            (setq ,form ,@body))))

(def-edebug-spec define (sexp def-body))

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
        ((set? x) (set->list x))
        (t (error (format "unimplemented for loop sequence type: %s %s" (type-of x) x)))))

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
           (nreverse __rkt-list-ret__))
      `(let ,let-vars
         (while ,condition
           ,setq-form
           ,@body)))))

(defmacro for (spec &rest body)
  (racket-make-for-loop spec body nil))

(defmacro for/list (spec &rest body)
  (racket-make-for-loop spec body t))

(defun in-range (start &optional end step)
  (unless end
    (setq end start
          start 0))
  (number-sequence start (1- end) step))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vectors
(defalias 'vector-set! 'aset)
(defalias 'vector-ref 'aref)
(defalias 'vector-length 'length)
(defalias 'vector? 'vectorp)
(defalias 'vector-map 'mapcar)
(defalias 'vector-append 'vconcat)
(defalias 'vector-copy 'copy-sequence)
(defun vector-member (item vec)
  (cl-position item vec :test 'equal))
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
      `(mapcar ',fn ,lst)
    `(mapcar ,fn ,lst)))

(defalias 'null? 'null)
(defalias 'cons? 'consp)

(defun list->set (lst)
  (let ((s (make-set)))
    (dolist (x lst)
      (puthash x t s))
    s))

(defun string-join (sequence separator)
  (mapconcat 'identity sequence separator))

(defun take (list n)
  (let (ret)
    (while (and list (> n 0))
      (setq ret (cons (car list) ret)
            list (cdr list)
            n (1- n)))
    (nreverse ret)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash tables
(defun make-hash (&optional alist)
  (let ((ht (make-hash-table :test 'equal)))
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
             hash)
    ret))

(require 'subr-x)
(defalias 'hash-values 'hash-table-values)
(defalias 'hash-keys 'hash-table-keys)

;; raacket remove only removes the first item in the list, elisp version removes all matches

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sets

;; the racket function 'set' cannot be used in elisp, use make-set instead

(setq racket-magic-set-key '__racket_set_key__
      racket-magic-set-value '__racket_set_value__)

(defun list->set (lst)
  (let ((s (make-hash-table :test 'equal)))
    (puthash racket-magic-set-key racket-magic-set-value s)
    (dolist (x lst)
      (puthash x t s))
    s))

(defsubst make-set (&rest items)
  (list->set items))

(defsubst set? (s)
  (and (hash-table-p s)
       (eq (gethash racket-magic-set-key s) racket-magic-set-value)))

(defsubst set-member? (s item)
  (gethash item s))

(defun set->list (s)
  (let (lst)
    (maphash (lambda (k v)
               (unless (eq k racket-magic-set-key)
                 (push k lst)))
             s)
    lst))

(defsubst set-add (set item)
  (assert (set? set))
  (puthash item t set)
  set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strings
;;; same: make-string

(defalias 'string? 'stringp)
(defalias 'string-length 'length)
(defalias 'string-append 'concat)
(defalias 'string->list 'string-to-list)
(defalias 'string-ref 'aref)

(defun list->string (lst) (mapconcat 'identity (mapcar 'byte-to-string lst) ""))

(defun string->number (s)
  (let ((n (string-to-number s)))
    (if (= n 0)
        (if (string= (number-to-string n) s)
            0
          nil)
      n
      )))

(setq _char-hash ?#)
(setq _char-0 ?0)
(setq _char-x ?x)
(setq _char-b ?b)
(setq _char-close-paren ?\))
(setq _char-newline ?\n)
(setq _char-& ?&)
(setq _char-space ?\ )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; numbers

(defalias 'add1 '1+)
(defalias 'sub1 '1-)
(defalias 'number? 'numberp)
(defun range (from &optional to)
  (unless to
    (setq to from
          from 0))
  (number-sequence from (1- to)))

(defsubst quotient (n m) (floor (/ n m)))
(defalias 'remainder '%)
(defalias 'modulo 'mod)
(defalias 'exact->inexact 'identity)
(defalias 'number->string 'number-to-string)
(defsubst zero? (n) (= n 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conditions
(defalias 'equal? 'equal)
(defalias 'eq? 'eq)

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
(defsubst void (&rest x) t)
(setq void (function void))

(setq eof nil) ;;functions that use this should not be called from elisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bitwise functions

(defalias 'bitwise-and 'logand)
(defalias 'bitwise-xor 'logxor)
(defalias 'arithmetic-shift 'ash)
(defalias 'bitwise-ior 'logior)
(defalias 'bitwise-not 'lognot)

(defun bitwise-bit-field (n start end)
  (bitwise-and (sub1 (arithmetic-shift 1 (- end start)))
               (arithmetic-shift n (- start))))

(defun bitwise-bit-set? (n m)
    (not (zero? (bitwise-and n (arithmetic-shift 1 m)))))

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

(defun __fix (s)
  (intern (symbol-name s)))

(defmacro struct (name fields &rest options)
  ;;field is either a symbol or [symbol <option>]
  ;; <option> is 'auto' or 'mutable'
  (let* ((struct-name (intern (symbol-name name)))
         (test-name (intern (format "%s?" struct-name)))
         pos opt positional optional is-opt arg)
    ;;ignoring OPTIONS

    (dolist (field fields)
      (if (symbolp field)
          (if optional
              (error "positional arg following optional args is not allowed (in definition of struct '%s')"
                     struct-name)
            (push field positional))
        ;;else
        (assert (and (consp field)
                     (> (length field) 1)))
        (setq arg (pop field)
              is-opt nil)

        (dolist (opt field)
          (unless (or (eq (__fix opt) 'auto)
                      (eq (__fix opt) 'mutable)) ;;ignoring mutable optional - everything is mutable
            (error "field option '%s' is not supported  (in definition of struct '%s')"
                   opt struct-name))
          (when (eq (__fix opt) 'auto)
            (setq is-opt t)))
        (when is-opt
          (push arg optional))
        ))

    (setq pos (nreverse positional)
          opt (nreverse optional)
          args (append pos opt)
          n-pos (length pos)
          n-opt (length opt)
          max-args (+ n-pos n-opt))

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
                    (let ((name (concat "set-" (symbol-name struct-name) "-" (symbol-name arg) "!")))
                      `(if (and (fboundp ',(intern name))
                                (not (get ',(intern name) 'is-racket-fn)))
                           (error ,(format "error: %s is already defined as a non rkt function" name)) ;;TODO: should be an error
                         (put ',(intern name) 'is-racket-fn t)
                         (defun ,(intern name) (s val)
                           (unless (,test-name s)
                             (error ,(format " %s -- expected struct type '%s'. got this instead: %%s" name struct-name) s))
                           (aset s ,i val))
                         (setq ,(intern name) ',(intern name)))))
                  args (number-sequence 1 max-args))

       ,@(mapcar* (lambda (arg i)
                    (let ((name (concat (symbol-name struct-name) "-" (symbol-name arg))))
                      `(if (and (fboundp ',(intern name))
                                (not (get ',(intern name) 'is-racket-fn)))
                           (error ,(format "error: %s is already defined as a non rkt function" name)) ;;TODO: should be an error

                         (put ',(intern name) 'is-racket-fn t)
                         (defun ,(intern name) (s)
                           (unless (,test-name s)
                             (error ,(format " %s -- expected struct type '%s'. got this instead: %%s"
                                             name struct-name) s))
                           (aref s ,i))
                         (setq ,(intern name) ',(intern name)))))
                  args (number-sequence 1 max-args))
       )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; printing

(defun rkt-format (fmt &rest args)
  ;; incomplete but good enough!
  (dolist (x '(("~~" . "~")
               ("%" . "%%")
               ("~a" . "%s")
               ("~x" . "%x")
               ))
    (setq fmt (replace-regexp-in-string (regexp-quote (car x)) (cdr x) fmt)))
  (apply 'format fmt args))

(defun printf (fmt &rest args)
  (let ((s (apply 'rkt-format fmt args)))
    (when (stringp s)
      (princ (format "%s" s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; file loading

(setq elisp? t
      compatibility/defmacro nil)

(defun all-defined-out () nil)

(setq rkt-loaded-files (make-set))

(defun rkt-load (file)
  (setq file (concat (file-name-directory (or buffer-file-name load-file-name)) file))

  (unless (file-exists-p file)
    (message "Error: rkt-load -- file does not exist: %s" file)
    (exit 0))

  (when (or (not (set-member? rkt-loaded-files (file-truename file)))
            (not racket-script-mode)) ;; always reload when working interactively

    (set-add rkt-loaded-files (file-truename file))

    (let* ((lexical-binding t)
           (racket? nil)
           (elisp? t)
           (compatibility/defmacro nil) ;; to prevent void-variable errors
           (compiled-filename (concat file ".elc"))
           (use-byte-compiled (file-exists-p compiled-filename)))

      (with-temp-buffer
        (setq lexical-binding t)
        (setq buffer-file-name file)
        (unless use-byte-compiled
          (insert-file-contents-literally file)
          (goto-char (point-min))
          (forward-line)) ;; skip  #lang ...
        ;; require from loaded files is disabled because it is so slow - load them manually
        (flet (;;(require (&rest files) (rkt-require files))
               (require (&rest files) nil)
               (provide (&rest syms) nil))
          (condition-case err
              (if use-byte-compiled
                  ;;(load (expand-file-name compiled-filename) nil t)
                  (load compiled-filename nil t)
                (eval-region (point) (point-max)))
            (error (message (format "rkt-load error. file='%s', error=%s" file err)))))
        (setq buffer-file-name nil)
        ))))

(defun rkt-byte-compile (file)
  (let ((lexical-binding t)
        (racket? nil)
        (elisp? t)
        (compatibility/defmacro nil) ;; to prevent void-variable errors
        (compiled-filename (concat file ".el")))

    (with-temp-buffer
      (insert-file-contents-literally file)
      (setq lexical-binding t)
      (goto-char (point-min))
      (kill-line)
      (insert ";; -*- lexical-binding: t  -*-")
      (flet ((require (&rest files) nil) ;;TODO: have require recursively byte compile
             (provide (&rest syms) nil))
        (write-file compiled-filename)
        (byte-compile-file compiled-filename)
        (delete-file compiled-filename))
      )))

(defun rkt-require (files)
  (when (stringp files)
    (setq files (list files)))
  (dolist (file files)
    (when (and (stringp file)
               (not (equal (car (last (split-string file "/"))) "el.rkt")))
      (rkt-load file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro _def (syms)
  (let ((code '()))
    (dolist (sym (cadr syms))
      (setq code (cons (list 'setq sym nil) code))
      (setq code (cons `(put ',sym 'is-racket-var t) code)))
    (cons 'progn code)))

(defmacro set! (var val)
  (list 'setq var val))

(defvar racket-script-mode nil "true if started run as script")

(defun exit (&optional code)
  (if racket-script-mode
      (kill-emacs (or code 0))
    (signal 'racket-exit code)))

(defun with-output-to-file (filename function &rest options)
  (let ((prev-output standard-output))
    (unwind-protect
        (with-temp-buffer
          (setq standard-output (current-buffer))
          (funcall function)
          (write-file filename))
      (setq standard-output prev-output))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes

(let ((vars '(class-type-index
              object-type-index
              object-method-mapping-index
              object-field-mapping-index
              object-n-special-fields ))
      (i 0))
  (dolist (var vars)
    (set var i)
    (setq i (1+ i))))


(defun get-param-syms (arglist)
  (remove '&rest (remove '&optional arglist)))

(defun convert-let-form (type bindings body v-map f-map exclude)
  (let ((exclude (append (mapcar (lambda (x) (if (consp x) (car x) x)) bindings) exclude)))
    (cons type (cons (mapcar (lambda (x)
                               ;; TODO: this will not work in all cases, such as
                               ;; (setq b 1)
                               ;; (let ((a (1+ b)) (b 3)) ...)
                               (if (listp x)
                                   (list (car x)
                                         (replace-variable-references (cadr x) v-map f-map exclude))
                                 x))
                             bindings)
                     (mapcar (lambda (x) (replace-variable-references x v-map f-map exclude)) body)))))

(defun replace-variable-references (form v-map f-map &optional exclude)
  (let (x)
    (pcase form
      (`(send_ ,x ',method ,args)
       `(send_ ,(replace-variable-references x v-map f-map exclude)
               ',method
               ,(mapcar (lambda (x) (replace-variable-references x v-map f-map exclude)) args)))
      (`(setq ,var ,value)
       (setq x (assoc var v-map))
       (if (and x (not (member var exclude)))
           `(aset this ,(cdr x) ,(replace-variable-references value v-map f-map exclude))
         `(setq ,var ,(replace-variable-references value v-map f-map exclude))))
      (`(lambda ,params . ,body)
       (let ((exclude (append (get-param-syms params) exclude)))
         `(lambda ,params ,@(mapcar (lambda (x) (replace-variable-references x v-map f-map exclude)) body))))

      (`(let ,bindings . ,body) (convert-let-form 'let bindings body v-map f-map exclude))
      (`(let* ,bindings . ,body) (convert-let-form 'let* bindings body v-map f-map exclude))

      ((pred listp)
       (mapcar (lambda (x) (replace-variable-references x v-map f-map exclude)) form))

      ((pred symbolp)
       (cond ((and (setq x (assoc form v-map))
                   (not (member form exclude)))
              `(aref this ,(cdr x)))

             ((and (setq x (assoc form f-map))
                   (not (member form exclude)))
              (cdr x))
             (t form)))
      (_ form))))

(defun replace-method-calls (mappings form)
  (macroexpand-all
   `(cl-macrolet (,@mappings) ,form)))

(setq anon-class-counter 0)

(defun make-arg-sym (base)
  (concat-sym "__optional__" base))

(defun rkt-alist->hash (alist)
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (x alist)
      (puthash (car x) (cdr x) ht))
    ht))

(defmacro class (super &rest body)
  (let* ((class-name (intern (format "class%s" (incf anon-class-counter))))
         (attr-start-index object-n-special-fields)
         public-methods private-methods method-start-index attributes
         trans-methods init-forms new-method-names init-fields len
         class-vec method-name-map name)

    (dolist (form body)
      (pcase form
        (`(init-field . ,fields) (setq init-fields fields))
        (`(define ,name . ,body)
         (if (symbolp name)
             (progn (push name attributes)
                    ;; use seq instead of define here or the 'define' macro will let-bind
                    ;; 'name' and it wont get replaced with the 'this' reference later
                    (push `(setq ,name ,@body) init-forms))
           (push form private-methods)))
        (`(define/public ,name . ,body)
         (push form public-methods))
        ('(super-new) nil)
        (_ (push form init-forms))))
    ;;args don't work with the init method and the 'define' macro because
    ;;the arguments are also fields and the (setq <arg> (or <arg> <default>))
    ;;code that gets generated
    ;;the initial reference needs to come from argument symbol, not the object array
    ;;create a temp arg value to get around this for
    (push `(define (__init__ ,@(mapcar (lambda (x) (if (consp x)
                                                       (cons (make-arg-sym (car x)) (cdr x))
                                                     (make-arg-sym x)))
                                       init-fields))
             ,@(mapcar (lambda (x) (if (consp x)
                                       (list 'setq (car x) (make-arg-sym (car x)))
                                     (list 'setq x (make-arg-sym x))))
                       init-fields)
             ,@(reverse init-forms))
          private-methods)

    (setq attributes (append (mapcar (lambda (x) (if (consp x) (car x) x))
                                     init-fields)  ;;; xxx
                             attributes)
          method-start-index (+ attr-start-index (length attributes))
          methods (append private-methods public-methods)
          method-names (mapcar 'caadr methods)
          new-method-names (mapcar (lambda (x)
                                     (concat-sym class-name "-" x))
                                   method-names)
          len (+ method-start-index (length methods))
          var-indices (number-sequence attr-start-index method-start-index)
          method-indices (number-sequence method-start-index
                                          (+ method-start-index (length methods)))
          var-mappings (mapcar* 'cons attributes var-indices)
          method-mappings (mapcar* 'cons method-names new-method-names)
          method-call-mappings (mapcar* (lambda (name new-name i)
                                          (list name '(&rest args)  (list '\` (list new-name 'this (list '\,@ 'args)))))
                                        method-names new-method-names method-indices)
          _method-call-mappings method-call-mappings
          class-vec (make-vector len nil)
          method-map (rkt-alist->hash (mapcar* 'cons method-names new-method-names)))

    (dolist (method methods)
      (setq name (car new-method-names)
            ;; need to expand the 'define' method so that all the generated 'setq's are visible
            body (macroexpand-all (cons 'define (cons (cons name (cons 'this (cdadr method))) (cddr method))) )
            ;;args (cdadr body)
            args (cadr body)
            ;;;;body (cons 'progn (cddr body))
            ;; body (replace-method-calls setter-mappings body)
            body (replace-method-calls method-call-mappings body)
            body (replace-variable-references body var-mappings method-mappings)
            new-method-names (cdr new-method-names))
      (push body trans-methods))

    (aset class-vec class-type-index class-name)
    (aset class-vec object-method-mapping-index method-map)
    (aset class-vec object-field-mapping-index var-mappings)
    ;;TODO: only export define/public

    `(progn ,@trans-methods
            ,class-vec
            )))

(defun new (type &rest args)
  (let ((obj (vector-copy type)))
    (set-object-type obj (class-type type))
    (send_ obj '__init__ args)
    obj))

(defsubst class-type (obj)
  (aref obj class-type-index))

(defsubst object-type (obj)
  (aref obj object-type-index))

(defsubst set-object-type (obj type)
  (aset obj object-type-index type))

(defmacro send (obj method &rest args)
  `(send_ ,obj ',method ,(cons 'list args)))

(defun send_ (obj method args)
  (let* ((methods (aref obj object-method-mapping-index))
         (fn (gethash method methods)))
    (if fn
        (apply fn obj args)
      (error (format "object type %s does not define method '%s'"
                     (class-type obj) method )))))

(defmacro get-field (field obj)
  `(get-field_ ',field ,obj))

(defun get-field_ (field obj)
  (let ((index (get-field-index field obj)))
    (when index
      (aref obj index))))

(defmacro set-field! (field obj value)
  `(set-field_ ',field ,obj ,value))

(defun set-field_ (field obj value)
  (let ((index (get-field-index field obj)))
    (when index
      (aset obj index value))))

(defun get-field-index (field obj)
  (let* ((fields (aref obj object-field-mapping-index))
         (fd (assoc field fields)))
    (if fd
        (cdr fd)
      (error "object type %s does not define field '%s'"
             (class-type obj) field))))

(defun concat-sym (&rest args)
  (intern (apply 'concat (mapcar (lambda (x) (if (stringp x) x (symbol-name x))) args))))

(defmacro define/public ()) ;;just for the syntax colors

(defun rkt-classes-test ()
  (let* ((test-class (class object$
                            (super-new)
                            (init-field a b (c 1) (d "s"))
                            (define x (+ 1 a))
                            (define order t)
                            (assert order)
                            (define order nil)
                            (define (get-a-internal)
                              a)
                            (define (get-x-internal)
                              x)
                            (define/public (inc-a)
                              (set! a (+ a 1)))
                            (define/public (set-x val)
                              (set! x val))
                            (define/public (double-x)
                              (set! x (+ x (get-x-internal))))
                            (define/public (get-a)
                              (get-a-internal))
                            (define/public (get-x)
                              x)
                            (define/public (test-call)
                              (funcall get-x this ))
                            ))
         (obj1 (new test-class 11 22))
         (obj2 (new test-class 12 22 33 "S")))

    (assert (= (send obj1 get-x) 12))
    (assert (= (send obj2 get-x) 13))
    (assert (= (send obj2 test-call) 13))

    (send obj1 double-x)
    (assert (= (send obj1 get-x) 24))

    (send obj1 inc-a)
    (assert (= (send obj1 get-a) 12))
    (assert (= (send obj2 get-a) 12))

    (assert (= (get-field b obj1) 22))
    (set-field! b obj1 111)
    (assert (= (get-field b obj1) 111))
    ))

;; (rkt-classes-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; temporary

(defsubst funcall1 (fn a)
  (funcall fn a))

(defsubst funcall2 (fn a b)
  (funcall fn a b))


(provide 'rkt)
