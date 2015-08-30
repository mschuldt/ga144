#lang racket

(require "assemble.rkt"
         "disassemble.rkt"
         "../common.rkt")

(provide make-bootstream
         sget-convert
         print-bootstream)

(enum (N E S W))

(define dir-names (vector "north" "east" "south" "west"))

(define start 708);;first node we stream code into

;;paths are lists of N, E, S, and W directions,
;;which is the direction of the the current node (starting with `start')
;;that the stream will take.

;;path1 from DB004 page 31
(define path1 (let ((NENW (append (cons N (make-list 16 E))
                                  (cons N (make-list 16 W)))))
                (append (make-list 8 E)
                        (make-list 7 S)
                        (make-list 17 W)
                        NENW NENW NENW
                        (cons N (make-list 7 E))
                        (list #f))))
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
  ;;ASSEMBLED is a list of 'node' structs
  ;;returns an array of assembled words
  (let* ((nodes (make-vector 144 #f))
         ;; ordered-nodes  is a list of node objects in the reverse order
         ;; that the bootstream visites them - or in the order that they)
         ;; have code loaded into their ram.
         (ordered-nodes '())
         (coord-changes (vector 100 1 -100 -1));;N, E, S, W coordinate changes
         (coord (+ start (vector-ref coord-changes (car path))))
         (len 0)
         (code (vector))
         (node #f)
         (node-code #f)
         (nothing (vector)))
    ;;place nodes into 'nodes' array, a mapping of node indexes to nodes
    ;;this allows constant time node code lookup
    (for ([node assembled])
      (vector-set! nodes (coord->index (node-coord node)) node))
    ;;create list of nodes in order the bootstream will visit them
    ;;If the node is not used then its value will be (coordinate . #f)
    (for ([dir path])
      (set! ordered-nodes (cons (or (vector-ref nodes (coord->index coord))
                                    (create-node coord #f 0))
                                ordered-nodes))
      (when dir
        (set! coord (+ coord (vector-ref coord-changes dir)))))
    ;; now generate the actual bootstream
    (for ([dir (reverse path)])
      (set! node (car ordered-nodes))
      (set! ordered-nodes (cdr ordered-nodes))
      (set! node-code (and (node-mem node) (get-used-portion (node-mem node))))
      (set! code (vector-append
                  ;;move all the previous code through this node
                  (if (> len 0)
                      (port-pump (node-coord node) dir len)
                      nothing)
                  (or code nothing)
                  ;;then load this nodes code into ram
                  (load-pump (and node-code
                                  (vector-length node-code)))
                  (if node-code
                      (vector-append node-code
                                     (vector (word "jump" (or (node-p node) 0))))
                      nothing)
                  ))
      (set! len (vector-length code)))
    ;; create bootframes
    (define frame1 (vector-append
                    (vector #xae
                            (get-direction start (car path))
                            len)
                    code))
    (define start-node (vector-ref nodes (coord->index start)))
    (set! code (if start-node
                   (get-used-portion (node-mem start-node))
                   (vector)))
    (define frame2 (vector-append
                    (vector (or (node-p start-node) 0) 0 (vector-length code))
                    code))
    (vector-append frame1 frame2)))

(define (sget-convert bootstream)
  ;; convert bootstream words to the byte format expected by node 708
  ;; bootstream is an array of 18 bit words
  (define new '())
  (for ((n bootstream))
    (set! new (cons (^ (& (>> n 10) #xff) #xff)
                    (cons (^ (& (>> n 2) #xff) #xff)
                          (cons (^ (ior (& (<< n 6) #xc0) #x2d) #xff)
                                new)))))
  (reverse new))

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

(define (print-bootstream bs)
  (for ((word bs))
    (printf "~a    ~a\n" word (disassemble-word word))))
