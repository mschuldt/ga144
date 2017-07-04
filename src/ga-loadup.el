;; -*- lexical-binding: t -*-

(add-to-list 'load-path "~/a/projects/ga144/src")
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
  (assert lexical-binding)
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

(defun ga-tests-loadup()
  (ga-rkt-load "../tests/test-compiler.rkt"))


(defun ga-sim-loadup()
  (ga-rkt-load "f18a.rkt")
  (ga-rkt-load "ga144.rkt")
  (ga-rkt-load "stack.rkt")  )


;; (ga-compiler-loadup)


(provide 'ga-loadup)
