#lang racket

(require "stack.rkt"
         "f18a.rkt"
         "../common.rkt")

(provide (all-defined-out))

(define DEBUG? #f)
(define _PORT-DEBUG? #f)
(define DISPLAY_STATE? #f)
(define port-debug-list '(1 2))
(define (PORT-DEBUG? coord) (and _PORT-DEBUG? (member coord port-debug-list)))

(define ga144%
  (class object%
    (super-new)

    (define time 0)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; 8x18 node matrix

    (define nodes (make-vector 144 #f))

    ;;builds matrix of 144 f18 nodes
    (define (build-node-matrix)
      (for ([i 144])
        (vector-set! nodes i (new f18a% [index i] [ga144 this])))
      (vector-map (lambda (node) (send node init)) nodes))

    (define (index->node index)
      (vector-ref nodes index))

    (define/public (coord->node coord)
      (let ([index (coord->index coord)])
        (if (and (>= index 0)
                 (< index 144))
            (vector-ref nodes index)
            #f ;;TODO: return pseudo node
            )))
    (define (fn:coord->node coord)
      (coord->node coord))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; suspension and wakeup

    ;;TODO: better way to clone vector
    (define active-nodes #f)
    ;;index of last active node in the 'active-nodes' array
    (define last-active-index 143) ;;all nodes are initially active

    (define current-node-index 0) ;;index into 'active-nodes' of the current node
    (define current-node #f)

    (define/public (remove-from-active-list node)
      (let ((last-active-node (vector-ref active-nodes last-active-index)))
        ;;swap self with current node in 'active-nodes'
        (vector-set! active-nodes
                     (get-field active-index node)
                     last-active-node)
        (vector-set! active-nodes last-active-index node)
        ;;save the new node indices
        (set-field! active-index last-active-node (get-field active-index node))
        (set-field! active-index node last-active-index)
        ;;decrement the number of active nodes
        (set! last-active-index (sub1 last-active-index))))

    (define/public (add-to-active-list node)
      (set! last-active-index (add1 last-active-index))
      (let ((first-inactive-node (vector-ref active-nodes last-active-index)))
        ;;swap self with first inactive node in 'active-nodes'
        (vector-set! active-nodes
                     (get-field active-index node)
                     first-inactive-node)
        (vector-set! active-nodes last-active-index node)
        ;;save the new node indices
        (set-field! active-index first-inactive-node (get-field active-index node))
        (set-field! active-index node last-active-index)))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; program loading

    (define/public (load code)
      (for ([n code])
        (send (coord->node (node-coord n))
              load
              (node-mem n)
              (node-len n))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; execution control

    (define/public (step-program!)
      (set! time (add1 time))
      (when (> current-node-index last-active-index)
        (set! current-node-index 0))
      (set! current-node (vector-ref active-nodes current-node-index))
      (send current-node step-program!)
      (set! current-node-index (add1 current-node-index)))

    (define/public (step-program-n! n)
      (when (> n 0)
        (step-program!)
        (step-program-n! (sub1 n))))

    ;;step program until all nodes are non-active
    (define/public (step-program!* [max-time #f])
      (define (step)
        (unless (= last-active-index -1)
          (step-program!)
          (step)))
      (define (step-with-max)
        (unless (= last-active-index -1)
          (step-program!)
          (when (< time 1000000)
            (step-with-max))))
      (if max-time
          (step-with-max)
          (step)))

    (define/public (reset!)
      (set! time 0)
      (set! active-nodes (vector-copy nodes))
      (set! last-active-index 143)
      (set! current-node-index 0)
      (set! current-node (vector-ref active-nodes current-node-index))
      (vector-map (lambda (node) (send node reset!)) nodes))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; state display functions

    (define (list-active-nodes)
      (if (>= last-active-index 0)
          (for/list ([i (add1 last-active-index)])
            (vector-ref active-nodes i))
          '()))

    (define/public (display-node-states [nodes #f])
      (let ((nodes (if nodes
                       (map fn:coord->node nodes)
                       (list-active-nodes))))
        (for ([node nodes])
          (send node display-state))))

    (define/public (display-dstacks [nodes #f])
      (let ((nodes (if nodes
                       (map fn:coord->node nodes)
                       (list-active-nodes))))
        (for ([node nodes])
          (send node display-dstack))))

    (define/public (display-memory coord [n MEM-SIZE])
      (send (fn:coord->node coord) display-memory n))

    (build-node-matrix)
    (reset!)
    ))
