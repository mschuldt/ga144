#lang racket

(require "util.rkt"
         "assemble.rkt")

(provide make-bootstream)

(enum (N E S W))

(define dir-names (vector "north" "east" "south" "west"))

(define start 708);;first node we stream code into
;;path1 from DB004 page 31
(define path1 (let ((NENW (append (cons N (make-list 16 E))
                                  (cons N (make-list 16 W)))))
                (append (make-list 9 E)
                        (make-list 7 S)
                        (make-list 17 W)
                        NENW NENW NENW
                        (cons N (make-list 7 E)))))
(define path path1)

;; we generate the bootstream for the nodes backwards - the
;; last node in the chain first.
;; 'port-pump' generates code to move the bootstream
;; for the nodes later in the stream through the current node.
;; 'load-pump' loads the code for a given node into its ram.

(define (word a [b #f] [c #f] [d #f]) (assemble-word (vector a b c d)))
(define (port-pump coord dir len)
  (vector (word "@p" "dup" "a!" ".")
          (word "call" (get-direction coord dir))
          (word "@p" "push" "!" ".")
          (word (sub1 len))
          (word "@p" "!" "unext" ".")))
(define (load-pump len)
  (if len
      (vector (word "@p" "a!" "@p" ".")
              (word 0);;TODO: will be set by descriptors
              (word (sub1 len))
              (word "push" "." "." ".")
              (word "@p" "!+" "unext"))
      (vector (word ";"))))

(define (make-bootstream assembled)
  ;;ASSEMBLED is a list of (node-coor . assembled-code) pairs
  ;;returns an array of assembled words
  (let ((nodes (make-vector 144 #f))
        (ordered-nodes '())
        (coord-changes (vector 100 1 -100 -1));;N, E, S, W coordinate changes
        (coord start)
        (len 0)
        (code (vector))
        (node #f)
        (node-code #f)
        (nothing (vector)))
    ;;place nodes into 'nodes' array, a mapping of node indexes to nodes
    ;;this allows constant time node code lookup
    (for ([node assembled])
      (vector-set! nodes (coord->index (car node)) node))
    ;;create list of nodes in order the bootstream will visit them
    ;;If the node is not used then its value will be (coordinate . #f)
    (for ([dir path])
      (set! ordered-nodes (cons (or (vector-ref nodes (coord->index coord))
                                    (cons coord #f))
                                ordered-nodes))
      (set! coord (+ coord (vector-ref coord-changes dir))))
    ;;now generate the actual bootstream
    (for ([dir (reverse path)])
      (set! node (car ordered-nodes))
      (set! ordered-nodes (cdr ordered-nodes))

      (set! node-code (and (cdr node) (get-used-portion (cdr node))))
      (and node-code (printf "node-code = ~a\n"  node-code))
      (set! code (vector-append
                  ;;move all the previous code through this node
                  (if (> len 0)
                      (port-pump (car node) dir len)
                      nothing)
                  (or code nothing)
                  ;;then load this nodes code into ram
                  (load-pump (and node-code
                                  (vector-length node-code)))
                  (or node-code nothing)))
      (set! len (vector-length code)))
    code))

(define named-addresses '(("right" . #x1D5)
                          ("down" . #x115)
                          ("left" . #x175)
                          ("up" . #x145)
                          ("io" . #x15D)
                          ("ldata" . #x171)
                          ("data" . #x141)
                          ("warp" . #x157)
                          ("center" . #x1A5)
                          ("top" . #x1B5)
                          ("side" . #x185)
                          ("corner" . #x195)))

(define (get-direction coord dir)
  ;;converts dir={N, E, S, W} into an address for node COORD
  (cdr (assoc (convert-direction coord
                                 (vector-ref dir-names dir))
              named-addresses)))

(define (get-used-portion code)
  ;; [w1, ..., wn, #f, ..., #f] => [w1, ..., wn]
  (let ((used '())
        (word #f))
    (define (get node [index 0])
      (set! word (vector-ref code index))
      (when word
        (set! used (cons word used))
        (get node (add1 index))))
    (get code)
    (list->vector (reverse used))))
