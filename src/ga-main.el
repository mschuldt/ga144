;; -*- lexical-binding: t -*-

(setq aforth-file-extensions '("aforth" "af" "ga"))

(setq debug-on-error t)
(setq load-start-time (current-time))

(require 'cl)
(require 'gv)
(put 'flet 'byte-obsolete-info nil) ;;prevent message "‘flet’ is an obsolete macro.."

(setq bootstream? nil)
(setq bootstream-type "async")
(setq symbols? nil)
(setq pretty? nil)
(setq count? nil)
(setq hex? nil)
(setq in-file nil)
(setq byte-compile? nil)
(setq profile? nil)
(setq create-docs? nil)
(setq test? nil)
(setq test-all? nil)
(setq only-bootstream? nil)
(setq run? nil)
(setq verbose? nil)
(setq working-dir nil)
(setq bowman-format? nil)
(setq bowman-expand? nil)
(setq print-bowman? nil)
(setq sim? nil)
(setq sim-bootstream? "")
(setq ga-only-print-nodes nil)
(setq ga-print-execution-time? nil)

(let ((base (file-name-directory (or buffer-file-name load-file-name))))
  (setq ga-base-dir (file-name-directory (substring base 0 -1))))

(add-to-list 'load-path (concat ga-base-dir "src"))
(add-to-list 'load-path (concat ga-base-dir "tests"))

(require 'arg-parser)

(setq ga-arg-spec '((("-b" "--bootstream") nil "include bootstream"
                    (setq bootstream? t))
                   (("--bootstream-type") (type) "bootstream type"
                    (setq bootstream-type type))
                   (("--only-bootstream") nil "only output loadable bootstream"
                    (setq only-bootstream? t))
                   (("-s" "--symbols") nil "include symboltable"
                    (setq symbols? t))
                   (("-p" "--pretty") nil "print in human readable"
                    (setq pretty? t))
                   (("-c" "--count") nil "count ram usage"
                    (setq count? t))
                   (("-x" "--hex") nil  "print numbers in hexadecimal format"
                    (setq hex? t))
                   (("-n" "--node") (n)  "Only print data for select nodes"
                    (setq ga-only-print-nodes (cons n ga-only-print-nodes)))
                   (("--byte-compile") nil  "byte compile .rkt files"
                    (setq byte-compile? t))
                   (("--profile") nil "save cpu profile data in file INFILE_profile"
                    (setq profile? t))
                   (("--create-docs") nil "generate documentation"
                    (setq create-docs? t))
                   (("-t" "--test") nil "run compiler tests" ;;TODO -t, -T does not work "Option '-T' requires an argument"
                    (setq test? t))
                   (("-T" "--test-all") nil "run all tests (compiler + simulator)"
                    (setq test? t
                          test-all? t))
                   (("-r" "--run") nil "run in simulator" ;;TODO: -r option does not work
                    (setq run? t))
                   (("-v" "--verbose") nil ""
                    (setq verbose? t))
                   (("-h") nil "print usage"
                    (ga-print-help-and-exit))
                   (("--wd") (dir) "" ;; for internal use
                    ;;(cd dir)
                    (setq working-dir dir))
                   (("--bowman") nil ""
                    (setq bowman-format? t))
                   (("--bowman-expand") nil "expand .ga into aforth compatible format and print"
                    (setq bowman-expand? t))
                   (("--print-as-bowman") nil ""
                    (setq print-bowman? t))
                   (("--sim") nil ""
                    (setq sim? t))
                   (("--sim-bootstream") nil ""
                    (setq sim? t
                          sim-bootstream? "--sim-bootstream"))
                   (("--print-gc") nil ""
                    (add-hook 'post-gc-hook (lambda () (message "GC: %ss(%s)" gc-elapsed gcs-done))))
                   (("--print-time") nil "print an estimate node execution time when the program exists"
                    (setq ga-print-execution-time? t))
                   (("--no-gc") nil ""
                    (setq gc-cons-threshold most-positive-fixnum))
                   (position (file) "aforth file"
                             (setq in-file file))
                   ))

(defun ga-print-help-and-exit ()
  (print-arg-help ga-arg-spec "ga")
  (kill-emacs))

(when (<= (length command-line-args) 5)
  (ga-print-help-and-exit))

(parse-args ga-arg-spec (cdddr command-line-args) t)

(when profile?
  (require 'profiler)
  (profiler-start 'cpu))

;(require 'aforth-compile)
;(require 'ga-serial)

(setq racket-script-mode t)

(load "ga-loadup.el" nil t)
(ga-compiler-loadup)

(setq bowman-format bowman-format?)

(when only-bootstream?
  (setq verbose? nil))

(when verbose?
  (message "load time: %s" (float-time (time-since load-start-time))))

(defun ga-byte-compile-files ()
  (setq lexical-binding t)
  (dolist (file '("src/bootstream.rkt"
                  "src/assemble.rkt"
                  "src/compile.rkt"
                  "src/disassemble.rkt"
                  "src/ga-compile-print.rkt"
                  "src/common.rkt"
                  "src/rom.rkt"
                  "src/rom-dump.rkt"
                  "tests/test-compiler.rkt"
                  "src/ga144.rkt"
                  "src/f18a.rkt"
                  "src/stack.rkt"
                  ))
    (rkt-byte-compile (expand-file-name file)))

  (dolist (file '("src/ga-main.el"
                  "src/aforth-compile.el"
                  "src/aforth-mode.el"
                  "src/ga144-map.el"
                  "src/aforth-parse.el"
                  "src/arg-parser.el"
                  "src/rkt.el"
                  "src/ga-loadup.el"
                  "src/ga144-sim.el"
                  "src/ga-run-simulator.el"
                  ))
    (byte-compile-file (expand-file-name file))))

(defun ga-main-exit ()
  (when profile?
    (let ((profile-filename (concat "_profile_" (or in-file ""))))
      (profiler-report)
      (profiler-report-write-profile profile-filename)
      (message "saved profile data in file: '%s', view with M-x profiler-find-profile" profile-filename)
      ))
  (exit))

(when byte-compile?
  (setq _start-time (current-time))
  (require 'vc-git) ;; for vc-git-root, because basic-save-buffer calls vc-after-save, but why?

  ;;(dolist (file (set->list rkt-loaded-files))
  ;;  (rkt-byte-compile (expand-file-name file)))
  (ga-byte-compile-files)
  (when verbose?
    (message "byte-compile time: %s" (float-time (time-since _start-time))))
  (ga-main-exit))

(when create-docs?
  (require 'vc-git)
  (write-directive-docs "compiler-directives")
  (ga-main-exit))

(when test?
  (ga-tests-loadup)
  (message (if (run-compiler-tests)
               "ok"
             "fail"))

  (when test-all?
    (require 'ga-tests)
    (run-simulation-tests)
    (ga-main-exit))
  (ga-main-exit))

(when (and in-file (not (file-exists-p in-file)))
  (message "File does not exist: %s\n" in-file)
  (ga-main-exit))

(setq file-extension (file-name-extension in-file))

(when (not (or (member file-extension aforth-file-extensions)
               (string= file-extension "el")))
  (message "Error: unknown file type")
  (ga-main-exit))

(require 'ga144-sim)

(defun ga-require-ga144-sim ()
  (require 'ga144-sim)
  (setq ga-print-execution-time ga-print-execution-time?))

(when (string= file-extension "el")
  (ga-require-ga144-sim)
  (load (expand-file-name in-file working-dir))
  (ga-main-exit))

(when run?
  (ga-require-ga144-sim)
  (ga144-run-file (expand-file-name in-file))
  (ga-main-exit))

(when sim?
  (shell-command  (concat "ga-sim " in-file " " sim-bootstream?))
  (ga-main-exit))

(when bowman-expand?
  (with-temp-buffer
    (convert-from-bowman-format in-file)
    (message (buffer-string)))
  (ga-main-exit))

(progn ;;for .rkt compatibility
  (defun bootstream-type () bootstream-type)
  (defun symbols? () symbols?)
  (defun hex? () hex?))

(setq _start-time (current-time))

(when only-bootstream?
  (princ (mapconcat 'number-to-string (compile-file-to-bootstream in-file bootstream-type) " ") standard-output)
  (ga-main-exit))

(cond (count? (print-count in-file))
      (pretty? (print-pretty in-file hex?))
      (print-bowman? (print-bowman-format in-file hex?))
      (t (print-json in-file bootstream-type symbols?)))

(when verbose?
  (message "compile time: %s" (float-time (time-since _start-time))))

(ga-main-exit)
