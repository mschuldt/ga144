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


(defalias 'vector-set! 'aset)
(defalias 'vector-ref 'aref)
(defalias 'vector-length 'length)
