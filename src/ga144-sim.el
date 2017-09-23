;; -*- lexical-binding: t -*-

(rkt-load "ga144.rkt")
(rkt-load "f18a.rkt")
(rkt-load "stack.rkt")

(require 'aforth-compile)
(require 'rkt)

(setq ga-print-execution-time nil)

(defun ga144-run-file (file)
  (let* ((compiled (aforth-compile-file file))
         (assembled (assemble compiled))
         (chip (make-ga144 (file-name-base file) nil)))

    (send chip load assembled)
    (send (send chip coord->node 705) set-pin! 0 t) ;; set 705.17 high to prevent spi boot
    (send chip step-program!*)
    (when ga-print-execution-time
      (ga-print-execution-time chip))
    ))

(setq ga144-chips nil)
(setq ga144-name-to-chip nil)
(setq num-chips nil)

(defun ga144-new (name &optional buffer)
  (let ((chip (make-ga144 name t buffer)))
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


(setq ga-probe-count 0)
(setq ga-probes (make-hash-table))

(defun ga-new-probe ()
  (let ((id ga-probe-count))
    (puthash id nil ga-probes)
    (setq ga-probe-count (1+ id))
    id))

(defun ga-probe-record (node probe-id state)
  (let* ((state (if (= state 3) 1 0))
         (history (gethash probe-id ga-probes))
         (last-time (car (car history)))
         (last-state (cdr (car history)))
         (time (send (send node get-ga144) get-time)))
    (when last-state
      (setq history (cons (cons time last-state) history)))
    (puthash probe-id (cons (cons time state) history) ga-probes)))

(defun ga-connect-probe (node pin)
  (let* ((probe-id (ga-new-probe))
         (probe (lambda (x)
                  (ga-probe-record node probe-id x))))
    (send node set-gpio-handler pin probe)
    probe-id))

(defun ga144-probe-save (&optional filename)
  (let ((shift 0))
    (with-temp-buffer
      (insert "import matplotlib.pyplot as plt\n")
      (maphash (lambda (k v)
                 (setq v (nreverse v))
                 (when v
                   (insert (format "plt.plot([%s], [%s])\n"
                                   (mapconcat (lambda (x) (number-to-string (car x))) v ",")
                                   (mapconcat (lambda (x) (number-to-string (+ (cdr x) shift))) v ",")))
                   (setq shift (+ shift 1.02))))
               ga-probes)
      ;; TODO: labels
      (insert "plt.show()\n")
      (write-file (or filename "ga144-probe-graph.py"))
      )))

(defun ga144-step* (&optional chip)
  (let ((chips (or (and chip (list chip))
                   ga144-chips))
        (again true)
        (breakpoint? nil))
    (setq ga-run-sim t)

    (while (and again
                (not breakpoint?)
                ga-run-sim)
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

(defun ga-format-ps (time)
  (cond ((< time 1000000)
         (format "%sns" (/ time 1000)))
        ((< time 1000000000)
         (format "%sus" (/ time 1000000)))
        (t (error "todo"))
        ))

(defun ga-print-execution-time (&optional chip)
  (message "node | time\n-----------")
  (dolist (x (sort (send chip get-execution-time)
                   (lambda (a b) (< (cdr a) (cdr b)))))
    (if (> (cdr x) 5200)
        (printf (format "%-3s   %s\n" (car x) (ga-format-ps (cdr x)))))))

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
