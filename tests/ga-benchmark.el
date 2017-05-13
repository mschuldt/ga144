;; -*- lexical-binding: t -*-

(setq chip (ga144-new "test-chip"))
(setq steps 1000000)

(setq code-fmt "
node %s
: fn2 ;
: fn  &fn2 push ex dup +* 4 for + unext or if 2/ then 2/ - 0 a! 3 for @ next fn2 ;
: main 12334 42212 fn  2* -if 2* then and or dup drop pop over a io b! @b !b 0 a! a 2* main 
")

;; test single node execution speed

(setq code (format code-fmt "1"))
(setq assembled (assemble (aforth-compile code)))
(setq start-time (current-time))
(send chip load assembled)
(setq node1 (send chip coord->node 1))
(send node1 set-break-at-step steps)
(send chip step-program!*)
;;(send node1 print-inst-counters)
(message "single node execution (%s steps): %s\n" steps (float-time (time-since start-time)))


;; test all nodes

(setq chip (ga144-new "test-chip"))
(setq code (mapconcat (lambda (x) (format code-fmt (index->coord x))) (number-sequence 0 143) "\n"))
(setq assembled (assemble (aforth-compile code)))
(setq start-time (current-time))
(setq node-steps (floor (/ steps 144)))
(send chip load assembled)
(dotimes (i 144)
  (send (send chip coord->node (index->coord i)) set-break-at-step node-steps))
(send chip step-program!*)
(message "all node execution (%s steps): %s\n" node-steps (float-time (time-since start-time)))


;; test boostream

(defun test-bootstream()
  (ga144-clear-all)
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
  )

(setq start-time (current-time))
(setq bootstream-iters 1)
(dotimes (i bootstream-iters)
  (test-bootstream)
  (message "testing bootstream"))
(message "bootstream test (%s times): %s\n" bootstream-iters (float-time (time-since start-time)))


;; test compile time
(setq code (mapconcat (lambda (x) (format code-fmt (index->coord x))) (number-sequence 0 143) "\n"))
(setq iters 50)
(setq start-time (current-time))
(dotimes (i iters)
  (assemble (aforth-compile code)))
(message "compile/assemble test (%s iters): %s\n" iters (float-time (time-since start-time)))

;; test initialization time
(setq code (mapconcat (lambda (x) (format code-fmt (index->coord x))) (number-sequence 0 143) "\n"))
(setq init-iters 50)
(setq assembled (assemble (aforth-compile code)))
(setq start-time (current-time))

(dotimes (i iters)
  (setq chip (ga144-new "test-chip"))
  (send chip load assembled))

(message "init test (%s iters): %s\n" init-iters (float-time (time-since start-time)))

