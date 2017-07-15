#lang racket ;; -*- lexical-binding: t -*-

(require "assemble.rkt"
         "disassemble.rkt"
         "compile.rkt"
         "common.rkt"
         "el.rkt")

(when elisp?
  (_def '(make-bootstream-type
          make-bootstream
          make-async-bootstream
          make-sync-bootstream
          sget-convert
          print-bootstream
          async-bootstream
          sync-bootstream)))

(provide make-bootstream-type
         make-bootstream
         make-async-bootstream
         make-sync-bootstream
         sget-convert
         print-bootstream
         async-bootstream
         sync-bootstream)

;;paths are lists of N, E, S, and W directions,
;;which is the direction of the the current node (starting with `start')
;;that the stream will take.

;;path1 from DB004 page 31
(defconst path1 (let ((NENW (append (cons N (make-list 16 E))
                                    (cons N (make-list 16 W)))))
                  (append (make-list 9 E)
                          (make-list 7 S)
                          (make-list 17 W)
                          NENW NENW NENW
                          (cons N (make-list 7 E))
                          (list false))))

;;path0 from DB004 page 31
(defconst target-sync-path (let ((SWSE (append (cons S (make-list 16 W))
                                               (cons S (make-list 16 E))))
                                 (SW (cons S (make-list 17 W))))
                             (append (make-list 4 N)
                                     (make-list 17 E)
                                     SWSE SWSE SW
                                     (cons S (make-list 17 E))
                                     SW
                                     (list false))))

;; the host-sync-path only goes from 708 to 300
(defconst host-sync-path (cons S (append (make-list 8 W)
                                         (make-list 3 S))))

(defconst async-bootstream (bootstream "async" 708 path1))
(defconst sync-bootstream (bootstream "sync" 300 target-sync-path))
(defconst host-sync-bootstream (bootstream "host-sync" 708 host-sync-path))

;; we generate the bootstream for the nodes backwards - the
;; last node in the chain first.
;; 'port-pump' generates code to move the bootstream
;; for the nodes later in the stream through the current node.
;; 'load-pump' loads the code for a given node into its ram.

(define (word a (b false) (c false) (d false)) (assemble-word (vector a b c d)))

(define (port-pump coord dir len)
  ;;(printf "(~a)port-pump jump direction: ~a\n" coord (get-direction coord dir))
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
              (word "@p" "!+" "unext" "."))
      (vector (word ";"))))

(define (make-node-index-map assembled)
  ;; place nodes into an array that maps node indexes to nodes
  ;; this allows constant time node lookup
  (let ((nodes (make-vector 144 false)))
    (for ((node assembled))
      (vector-set! nodes (coord->index (node-coord node)) node))
    nodes))

(define coord-changes (vector 100 1 -100 -1)) ;; N, E, S, W coordinate changes

(define (make-async-frame1 nodes assembled bootstream)
  ;;make frame 1 of the async bootstream, loads code for all nodes except the first
  (let* (;; ordered-nodes  is a list of node objects in the reverse order
         ;; that the bootstream visites them - or in the order that they)
         ;; have code loaded into their ram.
         (ordered-nodes '())
         (start (bootstream-start bootstream))
         (path (bootstream-path bootstream))
         (first-dir (car path))
         (coord (+ start (vector-ref coord-changes first-dir)))
         (path (cdr path))
         (len 0)
         (code (vector))
         (node false)
         (node-code false)
         (nothing (vector)))
    (define nodes (make-node-index-map assembled))
    ;;create list of nodes in order the bootstream will visit them
    ;;If the node is not used then its value will be (coordinate . false)
    (for ((dir path))
      (set! ordered-nodes (cons (or (vector-ref nodes (coord->index coord))
                                    (create-node coord false 0))
                                ordered-nodes))
      (when dir
        (set! coord (+ coord (vector-ref coord-changes dir)))))
    ;; now generate the actual bootstream
    (define rpath (reverse path))
    (for ((dir rpath)
          (prev (cdr (append rpath (list first-dir)))))
      (set! node (car ordered-nodes))
      (set! ordered-nodes (cdr ordered-nodes))
      (set! node-code (and (node-mem node) (get-used-portion (node-mem node))))
      (set! code (vector-append
                  ;;focusing call
                  (vector (word "call"
                                (get-direction (node-coord node)
                                               (vector-ref (vector S W N E)
                                                           prev))))
                  ;;move all the previous code through this node
                  (if (> len 0)
                      (port-pump (node-coord node) dir len)
                      nothing)
                  (or code nothing)
                  ;;then load this nodes code into ram
                  (load-pump (and node-code
                                  (vector-length node-code)))
                  (if node-code node-code nothing)

                  (if node-code
                      (vector-append
                       ;; set a
                       (if (node-a node)
                           (vector (word "@p" "a!" "." ".")
                                   (word (node-a node)))
                           nothing)
                       ;; set io
                       (if (node-io node)
                           (vector (word "@p" "@p" "b!" ".")
                                   (node-io node)
                                   (word #x15D) ;; io
                                   (word "!b" "." "." "."))
                           nothing)
                       ;; set b
                       (if (node-b node)
                           (vector (word "@p" "b!" "." ".")
                                   (word (node-b node)))
                           nothing)
                       ;; load stack values
                       (if (node-stack node)
                           (vector (word "@p" "push")
                                   (word (sub1 (length (node-stack node))))
                                   (word "@p" "unext"))
                           nothing)
                       (if (node-stack node) (list->vector (node-stack node)) nothing)

                       ;; jump to starting address
                       (vector (word "jump" (or (node-p node) 0)))
                       )
                      nothing)
                  ))
      (set! len (vector-length code)))
    ;; create the bootframe
    (vector-append
     (vector #xae
             (get-direction start (car path))
             len)
     code)))

(define (make-bootstream-type assembled bootstream)
  ;; ASSEMBLED is a list of 'node' structs
  ;; returns an array of assembled words
  ;; BOOTSTREAM is of type struct bootstream

  (define nodes (make-node-index-map assembled))
  (define frame1 (make-async-frame1 nodes assembled bootstream))
  (define start-node
    (vector-ref nodes (coord->index (bootstream-start bootstream))))
  (define code (if start-node
                   (get-used-portion (node-mem start-node))
                   (vector)))
  (define nothing (vector))
  (define frame2 (vector-append
                  (vector (or (and start-node
                                   (node-p start-node)) 0) 0 (vector-length code))
                  code
                  ;; ;boot descriptors for first node
                  ;; (if (and start-node (node-a start-node))
                  ;;     (vector (word "@p" "a!" "." ".")
                  ;;             (word (node-a start-node)))
                  ;;     nothing)
                  ;; ;; set io
                  ;; (if (and start-node (node-io start-node))
                  ;;     (vector (word "@p" "@p" "b!" ".")
                  ;;             (node-io start-node)
                  ;;             (word #x15D) ;; io
                  ;;             (word "!b" "." "." "."))
                  ;;     nothing)
                  ;; ;; set b
                  ;; (if (and start-node (node-b start-node))
                  ;;     (vector (word "@p" "b!" "." ".")
                  ;;             (word (node-b start-node)))
                  ;;     nothing)
                  ))
  (vector-append frame1 frame2))

(define (make-bootstream assembled (type default-bootstream-type))
  (unless (member type bootstream-types)
    (error (rkt-format "Invalid bootstream type: ~a  (Options: ~a)\n"
                       type (string-join bootstream-types ", "))))

  (define nodes (compiled-nodes assembled))
  (cond ((equal? type "async")
         (make-async-bootstream nodes))
        ((equal? type "2wire")
         (make-bootstream-type nodes sync-bootstream))
        ((equal? type "async-target")
         (make-sync-bootstream nodes))))

(define (make-async-bootstream assembled)
  ;; Standard async bootstream. Starts at 708 and visits all nodes.
  (make-bootstream-type assembled async-bootstream))

(define (make-sync-bootstream assembled)
  ;; creates a bootstream to load ASSEMBED code into target chip through
  ;; the host chip over the node 300 synchronous port
  (define wire ": wire
260000 for @ !b unext
wire")
  ;; host-loader-code moves the target chips bootstream from the host chip's
  ;; node 708 to node 300 and sends it to the target chip using the async port.
  (define host-loader-code (rkt-format "
node 608 north a! west b! ~a
node 607 east a! west b! ~a
node 606 east a! west b! ~a
node 605 east a! west b! ~a
node 604 east a! west b! ~a
node 603 east a! west b! ~a
node 602 east a! west b! ~a
node 601 east a! west b! ~a
node 600 east a! south b! ~a
node 500  0x20000 io b! !b
 10000 for . . next
 0 !b
 north a! south b! ~a
node 400 north a! south b! ~a

node 300
 ( reference: block 632 )
  : dly !b 32 for unext ;
  : 1bt dup dly 0x10000 or dly ;
: c+d+ 0x30003 1bt ; ( set clock high, data high, etc)
: c+d- 0x30002 1bt ;
: c-d+ 0x20003 1bt ;
: c-d- 0x20002 1bt ;
: bit-
   -if c+d+ ; then c+d- ;
: bit+
   -if c-d+ ; then c-d- ;
: send
  8 for
    bit- 2*
    bit+ 2*
  next
: loop
 @ send
loop ;
: main
north a! io b!
@ 0x30000 dly send loop ;
" wire wire wire wire wire wire wire wire wire wire wire))

  ;; bootstream that is loaded into target chip through node 300 sync port
  (define target-bootstream (make-bootstream-type assembled sync-bootstream))
  ;;(define bootstream host-sync-bootstream)
  (define bootstream async-bootstream)
  (define host-start (bootstream-start bootstream))
  (define host-path (bootstream-path bootstream))
  ;; Compile the host loader code
  (define host-code (compiled-nodes (assemble (aforth-compile host-loader-code))))
  (define nodes (make-node-index-map assembled))
  ;; create bootstream for host chip. The first frame loads the code to move the
  ;; bootstream to node 300, the second frame contains the target chips bootstream

  (vector-append
   ;;frame 1
   (make-async-frame1 nodes host-code bootstream)
   ;;frame 2
   (vector-append (vector 0
                          ;;(get-direction host-start (car host-path))
                          (get-direction host-start S)
                          (vector-length target-bootstream))
                  target-bootstream)
   ;;(vector 0 0 0 0)
   ))

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

(define (get-used-portion mem)
  ;; trim empty words from end of memory vector
  (let ((mem (reverse (vector->list  mem))))
    (while (and (not (null mem))
                (not (car mem)))
      (set! mem (cdr mem)))
    (list->vector (reverse mem))))

(define (print-bootstream bs)
  (for ((word bs))
    (printf "~a    ~a\n" word (disassemble-word word))))
