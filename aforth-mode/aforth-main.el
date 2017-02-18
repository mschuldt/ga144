(setq debug-on-error t)

(add-to-list 'load-path "~/a/projects/ga144/aforth-mode")
(add-to-list 'load-path "~/a/projects/ga144/")

(require 'cl)
(put 'flet 'byte-obsolete-info nil) ;;prevent message "‘flet’ is an obsolete macro.."
(require 'rkt)
(rkt-require "../aforth-compile-print.rkt")
(require 'aforth-compile)
(require 'ga144-load)
(require 'arg-parser)

(setq racket-script-mode t)

(setq bootstream? nil)
(setq bootstream-type "async")
(setq symbols? nil)
(setq pretty? nil)
(setq count? nil)
(setq hex? nil)
(setq in-file nil)

(parse-args '((("-b" "--bootstream") nil "include bootstream"
               (setq bootstream? t))
              (("--bootstream-type") (type) "bootstream type"
               (setq bootstream-type type))
              (("-s" "--symbols") nil "include symboltable"
               (setq symbols? t))
              (("-p" "--pretty") nil "print in human readable"
               (setq pretty? t))
              (("-c" "--count") nil "count ram usage"
               (setq count? t))
              (("-x" "--hex") nil  "print numbers in hexadecimal format"
               (setq hex? t))
              (position (file) "aforth file"
                        (setq in-file file))
              )
            (cdddr command-line-args) t)


(progn ;;for .rkt compatibility
  (defun bootstream-type () bootstream-type)
  (defun symbols? () symbols?)
  (defun hex? () hex?))

(if count?
    (print-count in-file)
  (if pretty?
      (print-pretty in-file hex?)
    (print-json in-file bootstream-type symbols?)))

(kill-emacs 0)
