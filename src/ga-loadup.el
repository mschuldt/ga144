;; -*- lexical-binding: t -*-

(setq byte-compiled-p (file-exists-p "ga-main.elc"))

(require 'cl)

(defun ga-rkt-load (file)
  (let ((lexical-binding t)
        (racket? nil))
    (flet ((require (&rest files) nil)
           (provide (&rest syms) nil))
      (if byte-compiled-p
          (load (concat file ".elc") nil t)
        (rkt-load file))))
  t)

(defun ga-el-load (file)
  
  (load file nil t))

(setq _ga_loadup_file load-file-name)

(defun ga-compiler-loadup ()
  (let ((lexical-binding t)
        (buffer-file-name _ga_loadup_file))
    (ga-el-load "rkt.el")

    (ga-rkt-load "rom.rkt")
    (ga-rkt-load "rom-dump.rkt")
    (ga-rkt-load "common.rkt")
    (ga-rkt-load "compile.rkt")
    (ga-rkt-load "bootstream.rkt")
    (ga-rkt-load "assemble.rkt")
    (ga-rkt-load "disassemble.rkt")
    (ga-rkt-load "ga-compile-print.rkt")

    (ga-el-load "rkt")
    (ga-el-load "aforth-parse")
    (ga-el-load "aforth-mode")
    (ga-el-load "arg-parser")
    ;;(ga-el-load "ga144-sim")
    (ga-el-load "aforth-compile"))
  t)

(defun ga-tests-loadup()
  (let ((lexical-binding t)
        (buffer-file-name _ga_loadup_file))  
    (ga-rkt-load "../tests/test-compiler.rkt")))

(defun ga-sim-loadup()
  (let ((lexical-binding t)
        (buffer-file-name _ga_loadup_file))
    (ga-rkt-load "f18a.rkt")
    (ga-rkt-load "ga144.rkt")
    (ga-rkt-load "stack.rkt")
    (ga-el-load "ga144-sim")
    ))
;; (ga-compiler-loadup)
;; (ga-sim-loadup)

(defun ga-loadup()
  (ga-compiler-loadup)
  (ga-sim-loadup)
  ;;(message (if (featurep  'ga144-sim) "YES" "NO"))
  )


(provide 'ga-loadup)
