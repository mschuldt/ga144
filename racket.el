
(defmacro begin (&rest body)
  (cons 'progn body))

(defmacro define (form &rest body)
  (if (consp form)
      (let ((fn-name (car form))
            (args (cdr form))
            name positional optional defaults)
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

        `(defun ,fn-name ,(append (reverse positional) (if optional '(&optional) nil) (reverse optional))
           ,@(reverse defaults)
           ,@body))
    (assert (symbolp form))
    `(defvar ,form ,body)))


(defalias 'vector-set! 'aset)
(defalias 'vector-ref 'aref)
(defalias 'vector-length 'length)
