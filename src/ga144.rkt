#lang racket ;; -*- lexical-binding: t -*-

(define _PORT-DEBUG? false)
(define DISPLAY_STATE? false)
(define port-debug-list '(1 2))
(define (PORT-DEBUG? coord) (and _PORT-DEBUG? (member coord port-debug-list)))

(define ga-run-sim t) ;;global variable used for halting the simulation
(define (ga-stop-sim!)
  (set! ga-run-sim nil))

(define (make-ga144 name_ (interactive_ false) (source-buffer_ false))
  (new ga144% name_ interactive_ source-buffer_))

(define ga144%
  (class object%
    (super-new)
    (init-field (name false) (interactive false) (source-buffer false))

    (define time 0)
    (define breakpoint false) ;; set to t when a breakpoint is reached
    (define breakpoint-node false) ;;node where breakpoint originated
    ;;set by map when it wants the node to update the map with its activity
    (define display-node-activity false)
    (define map-buffer false)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 8x18 node matrix

    (define nodes (make-vector 144 false))
    (define/public (get-nodes) nodes)
    ;;builds matrix of 144 f18 nodes
    (define (build-node-matrix)
      (for ((i 144))
        (vector-set! nodes i (new f18a% i this source-buffer)))
      (vector-map (lambda (node) (send node init)) nodes))

    (define (index->node index)
      (vector-ref nodes index))

    (define/public (coord->node coord)
      (let ((index (coord->index coord)))
        (if (and (>= index 0)
                 (< index 144))
            (vector-ref nodes index)
            false ;;TODO: return pseudo node
            )))

    (define (fn:coord->node coord)
      (coord->node coord))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; suspension and wakeup

    ;;TODO: better way to clone vector
    (define active-nodes false)
    ;;index of last active node in the 'active-nodes' array
    (define last-active-index 143) ;;all nodes are initially active

    (define current-node-index 0) ;;index into 'active-nodes' of the current node
    (define current-node false)

    (define/public (remove-from-active-list node)
      (let ((last-active-node (vector-ref active-nodes last-active-index))
            (index (get-field active-index node)))
        ;;swap self with current node in 'active-nodes'
        (vector-set! active-nodes index last-active-node)
        (vector-set! active-nodes last-active-index node)
        ;;save the new node indices
        (set-field! active-index last-active-node index)
        (set-field! active-index node last-active-index)
        ;;decrement the number of active nodes
        (set! last-active-index (sub1 last-active-index)))
      (when show-io-changes?
        (print-active)))

    (define/public (add-to-active-list node)
      (set! last-active-index (add1 last-active-index))
      (let ((first-inactive-node (vector-ref active-nodes last-active-index))
            (index (get-field active-index node)))
        ;;swap self with first inactive node in 'active-nodes'
        (vector-set! active-nodes index first-inactive-node)
        (vector-set! active-nodes last-active-index node)
        ;;save the new node indices
        (set-field! active-index first-inactive-node index)
        (set-field! active-index node last-active-index))
      (when show-io-changes?
        (print-active)))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; breakpoints

    (define cli-active? false) ;; if true, we are in a cli session

    (define/public (break (node false))
      (set! breakpoint-node node)
      ;; set the breakpoint flag which returns control to the interpreter
      (set! breakpoint t))
    (define/public (get-breakpoint-node) breakpoint-node)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; program loading

    (define/public (load compiled)
      ;; Places code into each node's RAM/ROM
      (reset! false)
      (for ((n (compiled-nodes compiled)))
        (send (coord->node (node-coord n)) load n))
      ;;(fetch-I)
      )

    (define/public (load-bootstream bs (input-node 708))
      ;;Load a bootstream through INPUT-NODE
      (send (coord->node input-node) load-bootstream bs)
      (set! time 0))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; execution control

    ;; step functions return true if a breakpoint has been reached, else false

    (define/public (step-program! (display-update-ok true))
      (set! breakpoint false)
      (set! time (add1 time))
      (define index 0)
      ;;(setq inhibit-redisplay t)
      (when (>= last-active-index 0)
        (while (and (<= index last-active-index)
                    (not breakpoint))
          (begin
            (set! current-node (vector-ref active-nodes index))

            (unless (and (send current-node step-program!)
                         (not (= index last-active-index)))
              ;; if node gets suspended during this step it will swap itself
              ;; with the last active node, declrementing last-active-index.
              ;; if that happens we need to step the node at the same index again.
              (set! index (add1 index)))
            )))
      ;;(setq inhibit-redisplay nil)
      (when (and display-node-activity
                 display-update-ok)
        (update-activity))
      ;;TODO: use current-node-index to correctly resume after a breakpoint
      breakpoint)

    (define/public (step-program-n! n)
      (set! breakpoint false)
      (set! ga-run-sim true)
      (while (and (> n 0)
                  (not (or (= last-active-index -1)
                           breakpoint))
                   ga-run-sim)
        (setq breakpoint (step-program! false))
        (setq n (1- n)))
      (when display-node-activity
        (update-activity))
      breakpoint)

    ;;step program until all nodes are non-active
    (define/public (step-program!* (max-time false))
      (set! breakpoint false)
      (set! ga-run-sim true)
      (if max-time
          (while (and (not (or (= last-active-index -1)
                               breakpoint))
                      (< time 1000000)
                      ga-run-sim)
            (step-program!))
          (while (and (not (or (= last-active-index -1)
                               breakpoint))
                      ga-run-sim)
            (set! breakpoint (step-program! false))))
      (when display-node-activity
        (update-activity))

      ;; (when (= (num-active-nodes) 0)
      ;;   (when interactive
      ;;     (printf "[[ All nodes are suspended\n"))
      ;;   (set! breakpoint t))

      breakpoint)

    (define/public (fetch-I)
      (vector-map (lambda (node) (send node fetch-I)) nodes))

    (define/public (reset! (fetch true))
      (set! time 0)
      (set! active-nodes (vector-copy nodes))
      (set! last-active-index 143)
      (set! current-node-index 0)
      (set! current-node (vector-ref active-nodes current-node-index))
      (set! breakpoint false)
      (set! cli-active? false)
      (set! breakpoint-node false)
      (vector-map (lambda (node) (send node reset!)) nodes)
      (when fetch (fetch-I))
      (when display-node-activity
        (update-activity)))

    (define/public (show-activity state)
      (set! display-node-activity state)
       (when state
           (update-activity)))

    (define (update-activity)
      (vector-map (lambda (node)
                    (send node update-map-display time))
                  nodes)
      (redisplay))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; state display functions

    (define/public (get-active-nodes)
      (if (>= last-active-index 0)
          (for/list ((i (add1 last-active-index)))
            (vector-ref active-nodes i))
          '()))

    (define/public (num-active-nodes)
      (add1 last-active-index))

    (define/public (display-node-states (nodes false))
      (let ((nodes (if nodes
                       (map (lambda (n) (coord->node n)) nodes)
                       (get-active-nodes))))
        (for ((node nodes))
          (send node display-state))))

    (define/public (display-dstacks (nodes false))
      (let ((nodes (if nodes
                       (map (lambda (n) (coord->node n)) nodes)
                       (get-active-nodes))))
        (for ((node nodes))
          (send node display-dstack))))

    (define/public (display-memory coord (n MEM-SIZE))
      (send (fn:coord->node coord) display-memory n))


    (define/public (print-active)
      ;;print a chip diagram showing the active nodes
      (define (print-node coord)
        (let* ((node (coord->node coord))
               (suspended? (send node suspended?))
               (reading-port (send node get-current-reading-port))
               (writing-port (send node get-current-writing-port)))
          (printf "~a" (if suspended?
                           (or reading-port writing-port " ")
                           "*"))))

      (printf "--------------------\n")
      (for ((row (range 8)))
        (printf "|")
        (for ((column (range 18)))
          (print-node (+ (* (- 7 row) 100) column)))
        (printf "|\n"))
      (printf "--------------------\n"))

    (define/public (print-node coord)
      (send (coord->node coord) display-all))

    (define show-io-changes? false)
    (define/public (show-io-changes show)
      (set! show-io-changes? show))

    (define/public (get-time) time)
    (define/public (reset-time) (set! time 0))

    (define/public (disassemble-memory coord (start 0) (end #xff))
      ;;  disassemble and print a nodes memory
      (send (coord->node coord) disassemble-memory start end))

    (define/public (disassemble-local coord)
      ;;  disassemble and print a nodes memory
      (send (coord->node coord) disassemble-local))

    (define/public (get-execution-time)
      (let ((ret nil))
        (for ((node nodes))
          (push (send node get-execution-time) ret))
        ret))

    (build-node-matrix)
    (reset!)
    ))
