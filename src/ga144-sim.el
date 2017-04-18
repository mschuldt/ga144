(rkt-load "ga144.rkt")
(rkt-load "f18a.rkt")
(rkt-load "stack.rkt")

(require 'aforth-compile)
(require 'rkt)

(defun ga144-run-file (file)
  (let* ((compiled (aforth-compile-file file))
         (assembled (assemble compiled))
         (chip (make-ga144 (file-name-base file) nil)))

    (send chip load assembled)
    (send (send chip coord->node 705) set-pin! 0 t) ;; set 705.17 high to prevent spi boot
    (send chip step-program!*)
    ))

(provide 'ga144-sim)
