(defun verify-commandline-spec (spec)
  (let ((spec-checkers '((lambda (x) (unless (eq x 'position)
                                       (let (ret)
                                         (dolist (name x)
                                           (unless (stringp name)
                                             (setq ret "invalid name type")))
                                         ret)))
                         (lambda (x) (unless (listp x) "expect list type for arglist" ))
                         (lambda (x) (unless (stringp x) "expect string type for docstring"))))
        error-str fn s funcs current-form)
    (while (and spec (not error-str))
      (setq s (car spec)
            current-form s
            spec (cdr spec)
            funcs spec-checkers)
      (unless (>= (length s) 4)
        (error "expected length >= 4: '%s' " s))
      (while (and spec-checkers s funcs (not error-str))
        (setq fn (car funcs)
              funcs (cdr funcs)
              item (car s)
              s (cdr s))
        (setq error-str (funcall fn item))))
    (when error-str
      (error "command-line option spec error: '%s', form='%s" error-str current-form))))

(defmacro popn (lst n)
  "remove and return the first N items of list LST as a list"
  (assert (symbolp lst))
  `(let ((__list ,lst)
         (__n ,n)
         ret)
     (while (and __list (> __n 0))
       (setq ret (cons (car __list) ret)
             __list (cdr __list)
             __n (1- __n)))
     (setq ,lst __list)
     (nreverse ret)))

(defun parse-args (spec &optional arg-list no-default)
  (verify-commandline-spec spec)

  (let ((args (or arg-list (and (not no-default) (cdr command-line-args))))
        names doc body positional-args named-args fn)
    
    (dolist (s spec)
      (if (eq (car s) 'position)
          (setq positional-args (cons (cons 'lambda (cdr s)) positional-args))

        (dolist (name (car s))
          (setq named-args (cons `(,name . (lambda ,@(cdr s))) named-args)))))

    (while args
      (setq a (car args)
            args (cdr args)) 
      (setq fn (cdr (assoc a named-args)))
      (if fn
          (progn
            (setq nargs (length (cadr fn)))
            (setq fn-args (popn args nargs))
            (when (not (= (length fn-args) nargs))
              (error "option %s expected at least %d args" nargs))
            (apply fn fn-args))

        (setq fn (car positional-args)
              positional-args (cdr positional-args))
        (if fn
            (funcall fn a)
          (error "unexpected positional arg: %s" a)))
      )))

(provide 'arg-parser)
