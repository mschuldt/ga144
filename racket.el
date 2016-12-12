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
        ((vector x) (mapcar 'identity x))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lists
(defalias 'list? listp)

(defun filter (func list)
  (let ((newlist ()))
    (dolist (e list)
      (if (funcall func e)
          (setq newlist (cons e newlist))))
    (reverse newlist)))

(defalias 'map 'mapcar)
(defalias 'null? 'null)

(defun list->set (lst)
  (let ((s (make-set)))
    (dolist (x lst)
      (puthash x t s))
    s))

(defun string-join (sequence separator)
  (mapconcat 'identity sequence separator))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hash tables
(defun make-hash (alist)
  (let ((ht (make-hash-table)))
    (dolist (x alist)
      (puthash (car x) (cdr x) ht))
    ht)) ;;TODO: do list instead of cons work?

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sets

;; the racket function 'set' cannot be used in elisp, use make-set instead

(setq racket-magic-set-key '__racket_set_key__)
(defun make-set (&rest items)
  (let ((s (make-hash-table)))
    (puthash racket-magic-set-key t s)
    (dolist (x items)
      (puthash x t s))
    s))

(defun set? (s)
  (gethash racket-magic-set-key s))

(defun set-member? (s item)
  (gethash item s))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; strings
;;; same: make-string

(defsubst 'string? 'stringp)
(defsubst 'string-length 'length)
(defsubst 'string->number 'string-to-number)
(defsubst 'string-append 'concat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; numbers

(defalias 'sub1 '1+)
(defalias 'add1 '1-)
(defalias number? 'numberp)
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
