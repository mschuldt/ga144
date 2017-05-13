;; -*- lexical-binding: t -*-

;; tests loading a program into the target chip through node 300
;; using the 2-wire connection.
;; The host and target chip are connected with virtual wires.
;; [[ currently requires changes in function load-bootstream in f18a.rkt to load
;; bootstream from 708 instead of 300 ]]
;; The normal async bootstream is loaded into the host chip, that loads code
;; that carries the sync bootstream from node host.708 to host.300 where it is sent
;; over 2-wire to target.300, loading code for the target chip.

(define-test-fn "target chip"
  (lambda ()
    (setq host (ga144-new "host"))
    (setq target (ga144-new "target"))

    (setq code "
node 708
11 22 +
")

    (setq assembled (assemble (aforth-compile code)))
    ;;(define bootstream (make-sync-bootstream (compiled-nodes assembled)))
    (setq bs (make-bootstream assembled "async-target"))
    (ga144-connect-pins (ga144-get-node host 300) 0
                        (ga144-get-node target 300) 0)
    (ga144-connect-pins (ga144-get-node host 300) 1
                        (ga144-get-node target 300) 1)

    (ga144-step*)

    (send host load-bootstream bs)

    (ga144-step*)

    (setq node708 (send target coord->node 708))
    (setq memory (send node708 get-memory))
    (setq dstack (send node708 get-dstack-as-list))
    (assert (= (vector-ref memory 1) 11))
    (assert (= (vector-ref memory 2) 22))


    (assert (= (car dstack) 33))
    (assert (= (cadr dstack) #x15555))
    ;;(message "instruction count:\n")
    ;;(send node708 print-inst-counters)
    t))


(provide 'ga-test-target-chip)
