;; -*- lexical-binding: t  -*-

;; script for running the simulation as a standalone application
;; 
;;  emacs -l ga-run-simulator.el FILE.aforth

(toggle-debug-on-error)
(load-theme 'wombat t)
(set-cursor-color "#ff4500")
(setq frame-title-format "GA144")

(require 'cl)
(require 'gv)
(put 'flet 'byte-obsolete-info nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-message t)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;;(set-fringe-mode 0)
;;(kill-buffer "*scratch*")
;;(setq message-log-max nil)
;;(kill-buffer "*Messages*")

(blink-cursor-mode 0)

(when (< (length command-line-args) 5)
  (princ "Usage: ga-sim FILE\n" #'external-debugging-output)
  (kill-emacs))

(setq base (file-name-directory (or buffer-file-name load-file-name))
      base (file-name-directory (substring base 0 -1)))

(add-to-list 'load-path (concat base "src"))

(setq dir (nth 3 command-line-args))
(setq filename (concat (file-name-as-directory dir)  (nth 4 command-line-args)))

(load "ga-loadup.el")
(ga-loadup)

(setq ga-load-bootstream (member "--sim-bootstream" command-line-args))

(setq ga-default-node-size 6)

(when (string= (file-name-extension filename) "ga")
  (setq bowman-format t))

(defun open-sim ()
  (find-file filename)
  (setq mode-line-format filename) 
  (read-only-mode 1)

  (buffer-disable-undo)
  (setq ga-sim-buffer (ga-open-map-for-simulation (current-buffer)))
  (switch-to-buffer ga-sim-buffer)
  ;;(pop-to-buffer-same-window ga-sim-buffer)
  ;;(set-window-buffer (selected-window) ga-sim-buffer)
  ;;(pop-to-buffer ga-sim-buffer)

  (setq mode-line-format "Simulation")
  ;;(redraw-display)
  ;;(redraw-frame)
  (delete-other-windows)
  (message ""))


;;for some reason this must be called after some delay or the
;;map buffer will not be visible
;;the delay must be > 0.0001
(run-at-time 0.001 nil 'open-sim)
(message "")
