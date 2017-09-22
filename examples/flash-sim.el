(setq _current-fragment nil)
(setq _current-fragment-len nil)

(setq _fragments (make-hash-table))

(defun _fragment (addr &rest words)
  (puthash addr words _fragments))

(setq _boot-fragment nil)
(defun _boot (n)
  (setq _boot-fragment n))

(setq flash-file "flash.el")

(let ((file (expand-file-name flash-file)))
  (if (file-exists-p file)
      (load file)
    (message "file %s does not exist" file)
    (kill-emacs)))

(puthash 0 (gethash _boot-fragment _fragments) _fragments)

(ga-define set-addr
           (let* ((addr (send node d-pop!))
                  (fragment (gethash addr _fragments)))
             (message "set-addr:  %s" addr)
             (cond ((= addr #x3ffff)
                    (message "(fragment request 0x3ffff)")
                    (ga-stop-sim!))

                   ((not fragment)
                    (message "  Error: fragment %s not found" addr)
                    (kill-emacs))
                   (t (setq _current-fragment-len (car fragment)
                            _current-fragment (cdr fragment))))))

(ga-define read-len
           (message "read-len: %s" (1- _current-fragment-len))
           (send node d-push! (1- _current-fragment-len)))

(ga-define read-next
           (let ((x (car _current-fragment)))
             (message "read-next: %s" x)
             (unless (null x)
               (send node d-push! x)
               (setq _current-fragment (cdr _current-fragment)))))
