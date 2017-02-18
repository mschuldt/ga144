;; -*- lexical-binding: t -*-

;; elisp aforth-compiler entrance

(add-to-list 'load-path "~/a/projects/ga144")
(require 'rkt)
(require 'aforth-parse)

(rkt-require "../compiler/compile.rkt")
(rkt-require "../compiler/bootstream.rkt")

(defun aforth-compile-buffer (&optional buffer)
  (setq buffer (or buffer (current-buffer)))
  (reset!)
  (dolist (node (aforth-parse-nodes (point-min) (point-max)))
    (start-new-node (aforth-node-coord node))
    (setq current-token-list (aforth-node-code node))
    (while current-token-list
      (compile-token (read-tok))))

  (when memory
    (fill-rest-with-nops) ;;make sure last instruction is full
    (set-node-len! current-node (sub1 next-addr)))

  (when DEBUG? (display-compiled (compiled used-nodes)))

  ;; errors from this point on are not associated with line numbers
  (setq current-tok-line nil
        current-tok-col nil)

  (map check-for-undefined-words used-nodes)

  (compiled (map remove-address-cells used-nodes)))


(defun aforth-compile (code) ;;shadows racket version
  (with-temp-buffer
    (insert code)
    (aforth-compile-buffer)))

(defun aforth-compile-file (filename)
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (aforth-compile-buffer)))

(provide 'aforth-compile)
