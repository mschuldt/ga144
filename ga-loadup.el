;; -*- lexical-binding: t -*-

(setq byte-compiled-p (file-exists-p "ga-main.elc"))

(defun ga-rkt-load (file)
  (let ((lexical-binding t)
        (racket? nil))
    (flet ((require (&rest files) nil)
           (provide (&rest syms) nil))
      (if byte-compiled-p
          (load (concat file ".elc") nil t)
        (rkt-load file)))))

(defun ga-el-load (file)
  (load file nil t))

(defun ga-compiler-loadup ()

  (ga-el-load "rkt.el")

  (ga-rkt-load "rom.rkt")
  (ga-rkt-load "rom-dump.rkt")
  (ga-rkt-load "common.rkt")
  (ga-rkt-load "compiler/compile.rkt")
  (ga-rkt-load "compiler/bootstream.rkt")
  (ga-rkt-load "compiler/assemble.rkt")
  (ga-rkt-load "compiler/disassemble.rkt")
  (ga-rkt-load "ga-compile-print.rkt")
  (ga-rkt-load "tests/test-compiler.rkt")


  (ga-el-load "rkt")
  (ga-el-load "aforth-mode/aforth-parse")
  (ga-el-load "aforth-mode/aforth-mode")
  (ga-el-load "aforth-mode/arg-parser")

  (ga-el-load "aforth-mode/aforth-compile"))


(provide 'ga-loadup)
