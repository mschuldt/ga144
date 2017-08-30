;; -*- lexical-binding: t -*-

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

(setq ga144-chips nil)
(setq ga144-name-to-chip nil)
(setq num-chips nil)

(defun ga144-new (name)
  (let ((chip (make-ga144 name t)))
    (push chip ga144-chips)
    (hash-set! ga144-name-to-chip name chip)
    (set! num-chips (add1 num-chips))
    chip))

(defun ga144-get-node (chip coord)
  (send chip coord->node coord))

(defun ga144-connect-pins (from-node from-pin to-node to-pin)
  (let ((wire (lambda (x)
                ;;    (printf "(WIRE ~a) ~a.~a<-->~a.~a\n" x
                ;;            (send from-node get-coord) from-pin
                ;;            (send to-node get-coord) to-pin)
                (send to-node set-pin! to-pin (= x 3)))))
    (send from-node set-gpio-handler from-pin wire)))

(defun ga144-step* (&optional chip)
  (let ((chips (or (and chip (list chip))
                   ga144-chips))
        (again true)
        (breakpoint? nil))

    (while (and again
                (not breakpoint?))
      (set! again nil)
      (for ((chip chips))
           (when (and (> (send chip num-active-nodes) 0)
                      (not breakpoint?))
             (setq again t
                   breakpoint? (send chip step-program!)))))))

(defun ga144-clear-all ()
  (setq ga144-chips nil)
  (setq ga144-name-to-chip (make-hash-table))
  (setq num-chips 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ga-extern-functions (make-hash-table))

(defmacro ga-define (name &rest body)
  "define a function that can be called from a ga144 simulation node with !!name
the node object the function is called from will be bound to 'node'"
  `(puthash ',name (lambda (node) ,@body) ga-extern-functions))

(ga-define printT
           (princ (format "T: %s\n" (car (send node get-dstack-as-list)))))

(ga-define printS
           (princ "S: %s\n" (cadr (send node get-dstack-as-list))))

(ga-define break
           (send node break "source breakpoint")
           (message "breakpoint (node %s)" (send node get-coord)))

(ga-define _test-inc ;; used by tests
           (send node d-push! (1+ (send node d-pop!))))

(ga144-clear-all)

(provide 'ga144-sim)
