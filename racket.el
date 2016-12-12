
(defmacro begin (&rest body)
  (cons 'progn body))


(defun racket-gather-local-vars (form &optional exclude)
  (let (vars)
    (cond ((not (consp form)) nil)
          ((or (eq (car form) 'setq)
               (eq (car form) 'set!)
               (and (eq (car form) 'define)
                    (symbolp (cadr form))))
           (unless (member (cadr form) exclude)
             (push (cadr form) vars))
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
    vars))

(defmacro define (form &rest body)
  (if (consp form)
      (let ((fn-name (car form))
            (args (cdr form))
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
        (when local-vars
          (setq body `((let ,local-vars ,@body))))

        `(defun ,fn-name ,(append (reverse positional) (if optional '(&optional) nil) (reverse optional))
           ,@(reverse defaults)
           ,@body))
    (assert (symbolp form))
    `(defvar ,form ,body)))


(defalias 'vector-set! 'aset)
(defalias 'vector-ref 'aref)
(defalias 'vector-length 'length)
