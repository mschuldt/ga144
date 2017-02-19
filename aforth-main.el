(setq debug-on-error t)
(setq load-start-time (current-time))

(require 'profiler)
(profiler-start 'cpu)

(add-to-list 'load-path "~/a/projects/ga144/aforth-mode") ;;TODO: remove
(add-to-list 'load-path "~/a/projects/ga144/")

(require 'cl)
(put 'flet 'byte-obsolete-info nil) ;;prevent message "‘flet’ is an obsolete macro.."
(require 'rkt)
(rkt-require "aforth-compile-print.rkt")
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
(setq byte-compile? nil)

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
              (("--byte-compile") nil  "byte compile .rkt files"
               (setq byte-compile? t))
              (position (file) "aforth file"
                        (setq in-file file))
              )
            (cdddr command-line-args) t)

(message "load time: %s" (float-time (time-since load-start-time)))

(when byte-compile?
  (setq _start-time (current-time))
  (require 'vc-git) ;; for vc-git-root, because basic-save-buffer calls vc-after-save, but why?
  (dolist (file (set->list rkt-loaded-files))
    (rkt-byte-compile (expand-file-name file)))
  (message "byte-compile time: %s" (float-time (time-since _start-time)))
  (exit))

(progn ;;for .rkt compatibility
  (defun bootstream-type () bootstream-type)
  (defun symbols? () symbols?)
  (defun hex? () hex?))

(setq _start-time (current-time))

(if count?
    (print-count in-file)
  (if pretty?
      (print-pretty in-file hex?)
    (print-json in-file bootstream-type symbols?)))

(message "compile time: %s" (float-time (time-since _start-time)))

;; view profile with M-x profiler-find-profile
(profiler-report)
(profiler-report-write-profile "profile")

(kill-emacs 0)
