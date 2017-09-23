#lang racket ;; -*- lexical-binding: t -*-

(define save-history false)
(define count-instructions true)
(define activity-history-length 10)
(define activity-history-bucket-size 10)
 ;;if true show total accumulated history,
 ;;otherwise display recent activity
(define activity-history-show-total false)

(define obj-map_ (make-hash)) ;;maps names to objects
(define obj-alist_ '()) ;;maps ojbects to names
(define obj-key-counter 0)

(define ga-show-source-position false)

(define (register-obj obj)
  ;; used to register a ga144 object and returns a key that can be used to access it
  (let* ((key (assq obj obj-alist_)))
    (if key
        (set! key (cdr key))
        (begin (set! key obj-key-counter)
               (set! obj-alist_  (cons (cons obj key) obj-alist_))
               (set! obj-key-counter (add1 obj-key-counter))
               (hash-set! obj-map_ key obj)))
    key))
(define (lookup-obj name)
  (hash-ref obj-map_ name))

(define f18a%
  (class object%
    (super-new)
    (init-field index ga144 (active-index 0) (source-buffer false))

    ;; register the ga144 object and keep a key to refer to it this prevents
    ;; the ga144 <-> f18a circular reference which results in the elisp error:
    ;;  'Apparently circular structure being printed'
    (define ga144-key (register-obj ga144))
    (define ga144-nodes-key (register-obj (send ga144 get-nodes)))
    (set! ga144 false)
    (set '__optional__ga144 false)
    (define/public (get-ga144)
      (lookup-obj ga144-key))

    (set! active-index index);;index of this node in the 'active-nodes' vector
    (define suspended false)
    (define coord (index->coord index))

    ;; stacks:
    (define dstack (make-stack 8))
    (define rstack (make-stack 8))

    ;; registers:
    (define A 0)
    (define B 0)
    (define P 0)
    (define I 0)
    (define I^ 0)
    (define R 0)
    (define S 0)
    (define T 0)
    (define IO #x15555)
    (define IO-read #x15555)
    (define data 0)

    (define iI 0) ;; opcode index
    (define I-index 0) ;; index of I in memory

    ;; Read operations return true if they have finished and left data
    ;; on the dstack. They return false if the node has suspened waiting
    ;; for the data.
    ;; When primary read operations finish they store the the
    ;; result in `fetched-data'. If the read was successful
    ;; fetched-data is pushed onto the stack to complete it.
    (define fetched-data false)
    ;;value of each gpio pin. t or false)
    ;;"All pins reset to weak pulldown"
    (define pin17 false)
    (define pin5 false)
    (define pin3 false)
    (define pin1 false)

    ;;t if we are suspended waiting for pin17 to change
    (define waiting-for-pin false)
    ;;state of WD bit in io register
    (define WD false)
    (define ~WD t)

    (define history '())

    (define symbols false) ;;names -> symbol structs
    (define ram-name->addr false)
    (define ram-addr->name false)

    (define debug false)
    (define debug-ports false)
    (define print-state false)
    (define print-io false)

    (define inst-counters false) ;; vector that records instruction execution counts
    (define step-counter 0) ;; number of steps this node has taken
    (define activity-history-index 0)
    (define activity-history false)
    (define activity-sum 0)

    (define extern-functions false) ;;vector of vectors with extern functions for each word
    (define extern-word-functions false) ;;vector containing extern functions for the current wor

    ;; Used if loaded with buffer mappings,
    ;; support for overlaying the position of the currently executing instruction
    (define buffer-map false) ;; mappings of instructions to buffer locations,
    (define word-buffer-map false) ;; mappings of instructions of the current word
    (define buffer-inst-overlay false) ;; overlay of the currently executing instruction
    (define map-node false)
    (define map-buffer false)
    ;;(define source-buffer false) ;; buffer with .aforth source
    (define/public (set-debug (general t) (ports false) (state false) (io false))
      (set! debug general)
      (set! debug-ports ports)
      (set! print-state state)
      (set! print-io io))

    (define (err msg)
      (printf "[~a] ERROR (chip: ~a): \"~a\"\n" coord (get-field name (get-ga144)) msg)
      (display-all)
      (when save-history
        (printf "Execution history(most recent first):\n ~a\n"
                (for/list ((op history)
                           (_ (range 15)))
                  (when (and (cons? op)
                             ;; some instructions don't have destination address
                             (or (< (car op) 2);; ';' and 'ex'
                                 (= (car op) 4)));; 'unext'
                    (set! op (car op)))
                  (if (cons? op)
                      (rkt-format "~a(~a)"
                                  (vector-ref opcodes (car op))
                                  (cdr op))
                      (vector-ref opcodes op)))))
      (error msg))

    (define (log msg)
      (define name (get-field name (get-ga144)))
      (printf "[~a~a] ~a\n" (if name (rkt-format "~a." name) "") coord msg))

    (define/public (set-pin! pin val)
      ;;val is false or t
      ;;???Racket equivalent of `set'?
      ;;(set (vector-ref (vector 'pin17 'pin5 'pin3 'pin1) pin) val)
      (cond ((= pin 0) (set! pin17 val))
            ((= pin 1) (set! pin1 val))
            ((= pin 2) (set! pin3 val))
            ((= pin 3) (set! pin5 val)))
      (when (and waiting-for-pin
                 (= pin 0)
                 (eq? val ~WD))
        ;;The node is suspend waiting for this value,
        ;;complete the read and wakeup the node
        (finish-port-read (if val 1 0))))

    (define (18bit n)
      (if (number? n);;temp fix
          (& n #x3ffff)
          n))

    ;;bit of each gpio pin in the io register
    (define pin17-bit (<< 1 17))
    (define pin5-bit (<< 1 6))
    (define pin3-bit (<< 1 4))
    (define pin1-bit 2)
    ;;bits of each read/write status bit in the io register
    (define Rr- (18bit (~ (<< 1 16))))
    (define Rw (<< 1 15))
    (define Dr- (18bit (~ (<< 1 14))))
    (define Dw (<< 1 13))
    (define Lr- (18bit (~ (<< 1 12))))
    (define Lw (<< 1 11))
    (define Ur- (18bit (~ (<< 1 10))))
    (define Uw (<< 1 9))

    (define memory false)
    (define instructions (make-vector 35 false))

    (define blocking-read false)
    (define blocking-write false)
    (define blocking false)
    (define multiport-read-ports false)

    (define carry-bit 0)
    (define extended-arith? false)

    (define rom-symbols (let ((ht (make-hash)))
                          (for ((sym (hash->list (get-node-rom coord))))
                            (hash-set! ht (region-index (cdr sym)) (car sym)))
                          ht))

    ;; Pushes to the data stack.
    (define/public (d-push! value)
      (push-stack! dstack S)
      (set! S T)
      (set! T (18bit value)))

    ;; Pushes to the rstack stack.
    (define/public (r-push! value)
      (push-stack! rstack R)
      (set! R value))

    ;; Pops from the data stack.
    (define/public (d-pop!)
      (let ((ret-val T))
        (set! T S)
        (set! S (pop-stack! dstack))
        ret-val))

    ;; Pops from the rstack stack.
    (define/public (r-pop!)
      (let ((ret-val R))
        (set! R (pop-stack! rstack))
        ret-val))

    ;; Return the value of p or a incremented as appropriately. If the
    ;; register points to an IO region, does nothing. Otherwise increment
    ;; the register circularly within the current memory region (RAM or
    ;; ROM).
    (define (incr curr) ;; DB001 section 2.2
      (if (> (& curr #x100) 0)
          curr
          (let ((bit9 (& curr #x200))
                (addr (& curr #xff)))
            (ior (cond ((< addr #x7F) (add1 addr))
                       ((= addr #x7F) 0)
                       ((< addr #xFF) (add1 addr))
                       ((= addr #xFF) #x80))
                 bit9))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; suspension and wakeup

    (define (remove-from-active-list)
      (if suspended
          (err "cannot remove suspended node from active list")
          (send (get-ga144) remove-from-active-list this))
      (set! suspended t))

    (define (add-to-active-list)
      (if suspended
          (send (get-ga144) add-to-active-list this)
          (err "cannot add active node to active list"))
      (set! suspended false))

    (define (suspend (reason false))
      (when debug
        (set! reason (or reason "<unspecified>"))
        (log (rkt-format "suspending due to ~a\n" reason)))
      (when debug-ports (log "suspending"))
      (remove-from-active-list)
      (when break-at-io-change
        (when break-at-io-change-autoreset
          (set! break-at-wakeup false)) ;;TODO:???
        (break "io change - suspend")))

    (define (wakeup)
      (when debug-ports (log "wakeup"))
      (add-to-active-list)
      (if break-at-wakeup
          (begin
            (when break-at-wakeup-autoreset
              (set! break-at-wakeup false))
            (break "wakeup"))
          (when break-at-io-change
            (when break-at-io-change-autoreset
              (set! break-at-wakeup false))
            (break "io change - wakup"))))
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; ludr port communication

    ;; adjacent nodes that are suspended, waiting for this node to read to a port
    (define writing-nodes (make-vector 4 false))

    ;;values written to a ludr port by a pending write
    ;;each node in the 'writing-nodes' array has a corresponding value here
    (define port-vals (make-vector 4 false))

    ;; adjacent nodes that are suspended, waiting for this node to write to a port
    (define reading-nodes (make-vector 4 false))

    ;;maps ludr ports to adjacent nodes
    (define ludr-port-nodes false)

    ;;port we are currently reading from
    ;;used for debugging. If not false, node must be suspended
    ;;This is a list if doing a multiport read.
    (define current-reading-port false)
    (define/public (get-current-reading-port)
      (and current-reading-port
           (if (list? current-reading-port)
               (length current-reading-port)
               (vector-ref (vector "l" "u" "d" "r") current-reading-port))))

    ;;port we are currently writing to
    ;;used for debugging. If not false, node must be suspended
    (define current-writing-port false)
    (define/public (get-current-writing-port)
      (and current-writing-port
           (vector-ref (vector "L" "U" "D" "R") current-writing-port)))

    (define (do-port-read port)
      (when debug-ports (log (rkt-format "(do-port-read ~a)\n" port)))
      ;;read a value from a ludr port
      ;;returns t if value is on the stack, false if we are suspended waiting for it
      (if (eq? port wake-pin-port)
          ;;reading from pin17
          (if (eq? pin17 ~WD)
              (begin (set! fetched-data (if pin17 1 0)) ;;TODO: correct return value?
                     true)
              (begin ;;else: suspend while waiting for pin to change
                (set! waiting-for-pin true)
                (suspend "pin17 read")
                false))
          ;;else: normal inter-node read
          (let ((writing-node (vector-ref writing-nodes port)))
            (if writing-node
                (begin ;;value was ready
                  (when debug-ports (printf "       value was ready: ~a\n"
                                            (vector-ref port-vals port)))
                  (set! fetched-data (vector-ref port-vals port))
                  ;;clear state from last reading
                  (vector-set! writing-nodes port false)
                  ;;let other node know we read the value
                  (send writing-node finish-port-write)
                  true)
                (begin ;;else: suspend while we wait for other node to write
                  (when debug-ports (printf "       suspending\n"))
                  (send (get-port-node port)
                        receive-port-read port this)
                  (set! current-reading-port port)
                  (suspend "port read")
                  false)))))

    ;; (define (port-read port)
    ;;   (when (do-port-read port)
    ;;     (d-push! fetched-data)
    ;;     (set! fetched-data false)))

    (define (do-multiport-read ports)
      (when debug-ports (log (rkt-format "(do-multiport-read ~a)\n" ports)))
      (let ((done false)
            (writing-node false)
            (other false))
        (for ((port ports))
          (if (eq? port wake-pin-port)
              ;;reading from pin17
              (begin (when (eq? pin17 ~WD)
                       (set! fetched-data (if pin17 1 0)) ;; TOOD: correct return value?
                       (set! done t)))
              (when (setq writing-node (vector-ref writing-nodes port))
                ;;an ajacent writing node is waiting for us to read its value
                (if done
                    (err "do-multiport-read -- more then one node writing")
                    (begin
                      (when debug-ports
                        (printf "       value was ready: ~a\n"
                                (vector-ref port-vals port)))
                      (set! fetched-data (vector-ref port-vals port))
                      (vector-set! writing-nodes port false)
                      (send writing-node finish-port-write)
                      (set! done true))))))
        (if done ;;successfully read a value from one of the ports
            true
            (begin ;;else: suspend while we wait for an other node to write
              (when (member wake-pin-port ports)
                (when debug (log "suspending. waiting-for-pin = True"))
                (set! waiting-for-pin t))
              (set! multiport-read-ports '())
              (for ((port ports))
                ;;(unless (vector-ref ludr-port-nodes port)
                ;;  (raise (rkt-format "do-multiport-read: node ~a does not have a ~a port"
                ;;                 coord (vector-ref port-names port))))
                ;;TODO: this is a temp fix:
                ;;     the current default instruction word is 'jump ludr'
                ;;     If the node is on the edge this does not work as at least
                ;;     one of the ports in ludr-port-nodes is false.
                ;;     Collect the valid nodes into 'multiport-read-ports'
                ;;     for use in 'finish-port-read()'
                (when (setq other (get-port-node port))
                  (send other receive-port-read port this)
                  (set! multiport-read-ports (cons port multiport-read-ports))))
              (set! current-reading-port ports)
              (suspend "multiport write")
              false))))

    ;;(define (multiport-read ports)
    ;;  (when (do-multiport-read ports)
    ;;    (d-push! fetched-data)
    ;;    (set! fetched-data nil)))

    (define (port-write port value)
      (when debug-ports
        (log (rkt-format "(port-write ~a  ~a)\n" port value)))
      ;;writes a value to a ludr port
      (let ((reading-node (vector-ref reading-nodes port)))
        (if reading-node
            (begin
              (when debug-ports (printf "       target is ready\n"))
              (vector-set! reading-nodes port false)
              (send reading-node finish-port-read value)
              t)
            (begin
              (send (get-port-node port)
                    receive-port-write port value this)
              (set! current-writing-port port)
              (suspend "port-write")
              false))))

    (define (multiport-write ports value)
      ;; "every node that intends to read the value written
      ;;  must already be doing so and suspended"
      (when debug-ports
        (log (rkt-format "(multiport-write ~a  ~a)\n" ports value)))
      (let ((reading-node false))
        (for ((port ports))
          (when (setq reading-node (vector-ref reading-nodes port))
            (when debug-ports (printf "       wrote to port: ~a\n" port))
            (vector-set! reading-nodes port false)
            (send reading-node finish-port-read value))))
      t)

    (define post-finish-port-read false)
    (define/public (set-post-finish-port-read fn)
      (set! post-finish-port-read fn))

    (define/public (finish-port-read val)
      ;;called by adjacent node when it writes to a port we are reading from
      ;;or when a pin change causes node to awaken
      (when debug-ports (log (rkt-format "(finish-port-read  ~a)\n" val)))
      (set! fetched-data val)
      (when suspended (wakeup))
      (if fetching-in-progress
          ;; call finish-fetch here instead of waiting for it to happen at the start
          ;; of the next step so that the received word is visable immediately in the map
          (finish-fetch)
          (error "finish-port-read expected fetching-in-prgresss"))

      (when multiport-read-ports
        ;;there may be other nodes that still think we are waiting for them to write
        (for ((port multiport-read-ports))
          ;;reuse 'receive-port-read' to cancel the read notification
          (send (get-port-node port) receive-port-read port false))
        (set! multiport-read-ports false))
      (set! current-reading-port false)
      (set! waiting-for-pin false)
      (and post-finish-port-read (funcall post-finish-port-read)))

    (define post-finish-port-write false)
    (define/public (finish-port-write)
      ;;called by adjacent node when it reads from a port we are writing to
      (when debug-ports (log "(finish-port-write)"))
      (set! current-writing-port false)
      (wakeup)
      (and post-finish-port-write (funcall post-finish-port-write)))

    (define/public (receive-port-read port node)
      ;;called by adjacent node when it is reading from one of our ports
      (when debug-ports
        (log (rkt-format "(receive-port-read ~a   ~a)" port (and node (send node str)))))
      (vector-set! reading-nodes port node))

    (define/public (receive-port-write port value node)
      (when debug-ports
        (log (rkt-format "(receive-port-write ~a  ~a  ~a)"
                         port value (send node str))))
      ;;called by adjacent node when it is writing to one of our ports
      (vector-set! writing-nodes port node)
      (vector-set! port-vals port value))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define prev-IO IO)
    (define num-gpio-pins (let ((pins (assoc coord node-to-gpio-pins)))
                            (if pins (cdr pins) 0)))
    ;;the wake-pin-port is the port to read from for pin17 wakeup, UP or LEFT
    (define wake-pin-port false)
    (when (> num-gpio-pins 0)
      (if (or (> coord 700)
              (< coord 17))
          (set! wake-pin-port UP)
          (set! wake-pin-port LEFT)))

    ;;the io read masks are used for isolating the parts of the io register
    ;;that are in use by a given node's io facilities. Bits that are not in
    ;;use read the inverse of what was last written to them
    (define io-read-mask 0)
    (define ~io-read-mask false)

    (define (init-io-mask)
      ;;this must be called after `ludr-port-nodes' is initialized
      (when (> num-gpio-pins 0)
        ;;add the gpio bits to the io read mask
        (set! io-read-mask
              (vector-ref (vector false #x20000 #x20002 #x2000a #x2002a)
                          num-gpio-pins)))
      ;;add the status bits
      (when (get-port-node 0)
        (set! io-read-mask (ior io-read-mask #x1800)))
      (when (get-port-node 1)
        (set! io-read-mask (ior io-read-mask #x600)))
      (when (get-port-node 2)
        (set! io-read-mask (ior io-read-mask #x6000)))
      (when (get-port-node 3)
        (set! io-read-mask (ior io-read-mask #x18000)))

      (set! ~io-read-mask (18bit (~ io-read-mask)))
      (set! io-read-default (& #x15555 io-read-mask)))

    ;;the default io register read bits excluding those
    ;;that are used to control io facilities.
    (define io-read-default false)

    (define pin1-ctl-mask #x30000)
    (define pin1-ctl-shift 16)
    (define pin2-ctl-mask #x3)
    (define pin2-ctl-shift 0)
    (define pin3-ctl-mask #xc)
    (define pin3-ctl-shift 2)
    (define pin4-ctl-mask #x30)
    (define pin4-ctl-shift 4)

    (define pin1-config 1)
    (define pin2-config 1)
    (define pin3-config 1)
    (define pin4-config 1)

    (define pin1-handler false)
    (define pin2-handler false)
    (define pin3-handler false)
    (define pin4-handler false)
    (define pin-handlers-set-p false)

    (enum (IMPED PULLDOWN SINK HIGH))

    (define/public (set-gpio-handlers a (b false) (c false) (d false))
      (set! pin1-handler a)
      (set! pin2-handler b)
      (set! pin3-handler c)
      (set! pin4-handler d)
      (set! pin-handlers-set-p t))

    (define/public (set-gpio-handler pin handler)
      (when (>= pin num-gpio-pins)
        (err (rkt-format "node ~a does not have pin ~a" coord pin)))
      (cond ((= pin 0) (set! pin1-handler handler))
            ((= pin 1) (set! pin2-handler handler))
            ((= pin 2) (set! pin3-handler handler))
            ((= pin 3) (set! pin4-handler handler))
            (else (err (rkt-format "invalid pin: ~a" pin)))))

    (define (read-io-reg)
      ;;sacrifice speed here to keep reads and writes as fast as possible
      (let ((io (ior (& (18bit (~ IO)) ~io-read-mask)
                     io-read-default)))
        ;;set node handshake read bits
        (when (vector-ref reading-nodes LEFT)
          (set! io (& io Lr-)))
        (when (vector-ref reading-nodes UP)
          (set! io (& io Ur-)))
        (when (vector-ref reading-nodes DOWN)
          (set! io (& io Dr-)))
        (when (vector-ref reading-nodes RIGHT)
          (set! io (& io Rr-)))

        ;;set node handshake write bits
        (when (vector-ref writing-nodes LEFT)
          (set! io (ior io Lw)))
        (when (vector-ref writing-nodes UP)
          (set! io (ior io Uw)))
        (when (vector-ref writing-nodes DOWN)
          (set! io (ior io Dw)))
        (when (vector-ref writing-nodes RIGHT)
          (set! io (ior io Rw)))

        ;;set io pin bits
        (when (> num-gpio-pins 0)
          (and pin17
               (set! io (ior io pin17-bit)))
          (when (> num-gpio-pins 1)
            (and pin1
                 (set! io (ior io pin1-bit)))
            (when (> num-gpio-pins 2)
              (and pin3
                   (set! io (ior io pin3-bit)))
              (and (> num-gpio-pins 3)
                   pin5
                   (set! io (ior io pin5-bit))))))
        io))

    (define (set-io-reg val)
      (when print-io (log (rkt-format "IO = ~a\n" val)))
      (set! prev-IO IO)
      (set! IO val)
      (set! WD (if (= (& (>> IO 11) 1) 1) true false))
      (set! ~WD (not WD))
      ;;if a digital pin control field changed, notify its handlers
      (when (and (> num-gpio-pins 0)
                 ;;pin-handlers-set-p
                 )
        (let ((changed (^ prev-IO IO)))
          (when (> (& changed pin1-ctl-mask) 0)
            (set! pin1-config (>> (& IO pin1-ctl-mask) pin1-ctl-shift))
            (and pin1-handler (funcall1 pin1-handler pin1-config)))
          (when (> num-gpio-pins 1)
            (when (> (& changed pin2-ctl-mask) 0)
              (set! pin2-config (>> (& IO pin2-ctl-mask) pin2-ctl-shift))
              (and pin2-handler (funcall1 pin2-handler pin2-config)))
            (when (> num-gpio-pins 2)
              (when (> (& changed pin3-ctl-mask) 0)
                (set! pin3-config (>> (& IO pin3-ctl-mask) pin3-ctl-shift))
                (and pin3-handler (funcall1 pin3-handler pin3-config)))
              (when (and (> num-gpio-pins 3)
                         (> (& changed pin4-ctl-mask) 0))
                (set! pin4-config (>> (& IO pin4-ctl-mask) pin4-ctl-shift))
                (and pin4-handler (funcall1 pin4-handler pin4-config)))))))
      t)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; memory accesses

    ;;Port address in the 'memory' vector (0x100 - 0x1ff) contain
    ;;vectors of functions to call when reading or writing to that port.
    ;;First element is the read function, second is the write function.
    ;;Invalid port numbers have the value false

    (define (read-memory addr)
      ;; if read finishes leave value in fetched-data and return true
      (set! addr (& addr #x1ff))
      (when (or (< addr 0)
                (>= addr MEM-SIZE))
        (err (rkt-format "(read-memory ~a) out of range" addr)))
      (if (port-addr? addr)
          (let ((x (vector-ref memory addr)))
            (if (vector? x)
                (funcall (vector-ref x 0))
                (err (rkt-format "read-memory(~a) - invalid port\n" addr))))
          (begin (set! fetched-data (vector-ref memory (region-index addr)))
                 true)))

    (define (read-memory-to-stack addr)
      (if (read-memory addr)
          (d-push! fetched-data)
          (set! fetching-in-progress 'stack)))

    (define (set-memory! addr value)
      (set! addr (& addr #x1ff))
      (when (or (< addr 0)
                (>= addr MEM-SIZE))
        (err (rkt-format "(read-memory ~a) out of range" addr)))
      (if (port-addr? addr)
          (let ((x (vector-ref memory addr)))
            (if x
                (if (vector? x)
                    (apply (vector-ref x 1) (list value))
                    (err (rkt-format "trying to access undefined port: 0x~x" addr)))
                (err (rkt-format "set-memory!(~a, ~a) - invalid port\n" addr value))))
          (vector-set! memory (region-index addr) value)))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; instruction execution

    (define unext-jump-p false)

    (define/public (execute! opcode (jump-addr-pos 0) (addr-mask false))
      (when count-instructions
        (assert (not (= most-positive-fixnum step-counter)))
        (set! step-counter (add1 step-counter))
        (when (eq? step-counter break-at-step)
          (break (rkt-format "step ~a" break-at-step)))
        (set! activity-sum (add1 activity-sum))
        (vector-set! activity-history activity-history-index
                     (add1 (vector-ref activity-history activity-history-index)))

        (vector-set! inst-counters opcode (add1 (vector-ref inst-counters opcode))))

      (when save-history
        (if (< opcode 8)
            (set! history (cons (cons opcode jump-addr-pos) history))
            (set! history (cons opcode history))))

      (if (< opcode 8)
          (apply (vector-ref instructions opcode)
                 (list (bitwise-bit-field I 0 jump-addr-pos)
                       addr-mask))
          (funcall (vector-ref instructions opcode))))

    (define fetching-in-progress false)
    (define/public (fetching?) fetching-in-progress)

    (define (finish-I-fetch)
      (set! I fetched-data)
      (when (null I)
        (log "Error: invalid null value for fetched data\n")
        (set! I 0))
      (if (and ga-show-source-position
               buffer-map
               (< (region-index I-index) 64) true)
          (begin
            (set! word-buffer-map (vector-ref buffer-map (region-index I-index))))
          (set! word-buffer-map false))
      (set! fetched-data false)
      (set! P (incr P))
      (if (or (eq? I 'end) (eq? I false))
          (begin (suspend "I=='end")
                 ;; set I to call warm to prevent a
                 ;; lot of special casing in `step!`
                 (set! I #x134a9)) ;; 0x134a9 = "warm" = 'call 169'
          (set! I^ (^ I #x15555))))

    (define/public (fetch-I)
      (set! I-index P)
      (when extern-functions
        (set! extern-word-functions (vector-ref extern-functions (region-index P))))
      (if (read-memory P) ;; read-memory returns t if the value is immediately available,
          ;; false if it results in suspending while waiting for the value
          (finish-I-fetch)
          (set! fetching-in-progress 'inst)))

    (define (finish-fetch)
      (assert fetching-in-progress)
      (cond ((eq? fetching-in-progress 'stack) ;;finishing reading a word to stack
             (d-push! fetched-data))
            ((eq? fetching-in-progress 'inst) ;;finish reading an instruction fetch
             (finish-I-fetch)))
      (set! fetching-in-progress false))

    (define fetch-next false)

    (define (step!)
      ;;(when suspended (log "(step!) SUSPENDED"))
      (cond (fetching-in-progress
             ;; This is the first step since the node suspended waiting for a read
             ;; Finish fetching an instruction or data word
             (finish-fetch))
            (fetch-next
             (set! fetch-next false)
             (fetch-I))
            (true (do-step!))))

    (define (do-step!)
      (define _i iI)
      (define nobreak true)

      (cond ((= iI 0)
             ;; check if this word has a breakpoint set
             (when (setq nobreak (funcall (vector-ref breakpoints (if (port-addr? I-index)
                                                                      (& I-index #x1ff)
                                                                      (region-index I-index)))))
               (set! iI (if (execute! (bitwise-bit-field I^ 13 18) 10 #x3fc00)
                            1
                            0))))

            ((= iI 1)
             (set! iI (if (execute! (bitwise-bit-field I^ 8 13) 8 #x3fe00)
                          2
                          0)))
            ((= iI 2)
             (set! iI (if (execute! (bitwise-bit-field I^ 3 8) 3 #x3fef8)
                          3
                          0)))
            ((= iI 3)
             (execute! (<< (bitwise-bit-field I^ 0 3) 2))
             (set! iI 0)))

      (when extern-word-functions
        (mapc (lambda (fn)
                (funcall (gethash fn ga-extern-functions 'identity) this))
              (vector-ref extern-word-functions _i)))

      ;; Fetch next word if index is zero and we are not in a unext loop
      ;; Don't fetch a word if a breakpoint is triggered, breakpoints trigger
      ;; at slot 0 but don't execute the instruction in slot 0. On next step
      ;; we need to resume at the same word.
      (when (and nobreak (= iI 0))
        (if unext-jump-p
            (set! unext-jump-p false)
            (if suspended
                (set! fetch-next true)
                ;;if not suspended fetch here instead of doing it on the next step.
                ;; This provides better visual indication in the simulator
                (fetch-I))))

      (when (and ga-show-source-position word-buffer-map)
        (ga-update-buffer-overlay (vector-ref word-buffer-map iI))))

    (define (get-current-slot-index) iI) ;;TODO: dup of get-inst-index

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; instructions

    (define _n 0)
    ;; Define a new instruction. An instruction can
    ;; abort the rest of the current word by returning false.

    (define (define-instruction! opcode fn)
      (begin (vector-set! instructions
                          _n
                          ;;(when debug (log ,(rkt-format "OPCODE: '~a'" opcode)))
                          fn)
             (set! _n (add1 _n))))

    (define (init-instructions)
      ;; `init-instructions` is seporated as its own method to keep the size of the __init__ method
      ;; down which avoids this error:
      ;;       "edebug-match-specs: Too deep - perhaps infinite loop in spec?"
      (define-instruction! ";"
        (lambda (_ __)
          (set! P R)
          (r-pop!)
          false))

      (define-instruction! "ex"
        (lambda (_ __)
          (define temp P)
          (set! P R)
          (set! R temp)
          false))

      (define-instruction! "jump"
        (lambda (addr mask)
          (when debug (log (rkt-format "jump to ~a  ~a"
                                       addr (or (get-memory-name addr) ""))))
          (set! extended-arith? (bitwise-bit-set? addr 9))
          (set! P (ior addr (& P mask)))
          false))

      (define-instruction! "call"
        (lambda (addr mask)
          (when debug
            (log (rkt-format "calling: ~a  ~a" addr (or (get-memory-name addr) ""))))
          (set! extended-arith? (bitwise-bit-set? addr 9))
          (r-push! P)
          (set! P (ior addr (& P mask)))
          false))

      (define-instruction! "unext"
        (lambda (_ __)
          (if (= R 0)
              (r-pop!)
              (begin (set! R (sub1 R))
                     (set! unext-jump-p t)
                     false))))

      (define-instruction! "next"
        (lambda (addr mask)
          (if (= R 0)
              (begin (r-pop!)
                     false)
              (begin (set! R (sub1 R))
                     (set! P (ior addr (& P mask)))
                     false))))

      (define-instruction! "if"
        (lambda (addr mask)
          (and (= T 0)
               (begin (when debug (log (rkt-format "If: jumping to ~a\n" addr)))
                      (set! P (ior addr (& P mask))))
               false)))

      (define-instruction! "-if"
        (lambda (addr mask)
          (and (not (bitwise-bit-set? T 17))
               (set! P (ior addr (& P mask)))
               false)))

      (define-instruction! "@p"
        (lambda ()
          (read-memory-to-stack P)
          (set! P (incr P))
          true))

      (define-instruction! "@+" ; fetch-plus
        (lambda ()
          (read-memory-to-stack (& A #x1ff))
          (set! A (incr A))
          true))

      (define-instruction! "@b" ;fetch-b
        (lambda ()
          (read-memory-to-stack B)
          true))

      (define-instruction! "@" ; fetch a
        (lambda ()
          (read-memory-to-stack (& A #x1ff)) t))

      (define-instruction! "!p" ; store p
        (lambda ()
          (set-memory! P (d-pop!))
          (set! P (incr P))
          true))

      (define-instruction! "!+" ;store plus
        (lambda ()
          (set-memory! A (d-pop!))
          (set! A (incr A))
          true))

      (define-instruction! "!b" ; store-b
        (lambda ()
          (set-memory! B (d-pop!))
          true))

      (define-instruction! "!" ; store
        (lambda ()
          (set-memory! (& A #x1ff) (d-pop!))
          true))

      (define-instruction! "+*" ; multiply-step
        (lambda ()
          ;;case 1 - If bit A0 is zero
          ;;  Treats T:A as a single 36 bit register and shifts it right by one
          ;;  bit. The most signficicant bit (T17) is kept the same.
          ;;case 2 - If bit A0 is one
          ;;  Sums T and S and concatenates the result with A, shifting
          ;;  everything to the right by one bit to replace T:A
          (if (= (& A 1) 1)
              ;;case 1:
              (let* ((sum (if extended-arith?
                              (let ((sum (+ T S carry-bit)))
                                (set! carry-bit (if (bitwise-bit-set? sum 18) 1 0))
                                sum)
                              (+ T S)))
                     (sum17 (& sum #x20000))
                     (result (ior (<< sum 17)
                                  (>> A 1))))
                (set! A (bitwise-bit-field result 0 18))
                (set! T (ior sum17 (bitwise-bit-field result 18 36))))
              ;;case 2:
              (let ((t17 (& T #x20000))
                    (t0  (& T #x1)))
                (set! T (ior t17 (>> T 1)))
                (set! A (ior (<< t0 17)
                             (>> A 1)))))
          true))

      (define-instruction! "2*"
        (lambda ()
          (set! T (18bit (<< T 1)))
          true))

      (define-instruction! "2/"
        (lambda ()
          (set! T (>> T 1))
          true))

      (define-instruction! "-" ;not
        (lambda ()
          (set! T (18bit (bitwise-not T)))
          true))

      (define-instruction! "+"
        (lambda ()
          (if extended-arith?
              (let ((sum (+ (d-pop!) (d-pop!) carry-bit)))
                (set! carry-bit (if (bitwise-bit-set? sum 18) 1 0))
                (d-push! (18bit sum)))
              (d-push! (18bit (+ (d-pop!) (d-pop!)))))
          true))

      (define-instruction! "and"
        (lambda ()
          (d-push! (& (d-pop!) (d-pop!)))
          true))

      (define-instruction! "or"
        (lambda ()
          (d-push! (^ (d-pop!) (d-pop!)))
          true))

      (define-instruction! "drop"
        (lambda ()
          (d-pop!)
          true))

      (define-instruction! "dup"
        (lambda ()
          (d-push! T)
          true))

      (define-instruction! "pop"
        (lambda ()
          (d-push! (r-pop!))
          true))

      (define-instruction! "over"
        (lambda ()
          (d-push! S)
          true))

      (define-instruction! "a" ; read a
        (lambda ()
          (d-push! A);;??
          true))

      (define-instruction! "."
        (lambda () true))

      (define-instruction! "push"
        (lambda ()
          (r-push! (d-pop!))
          true))

      (define-instruction! "b!" ;; store into b
        (lambda ()
          (set! B (& (d-pop!) #x1ff)) ;; B is 9 bits
          true))

      (define-instruction! "a!" ;store into a
        (lambda ()
          (set! A (d-pop!))
          true))
      )
    (init-instructions)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; public methods

    (define/public (get-memory) memory)
    (define/public (get-rstack) rstack)
    (define/public (get-dstack) dstack)
    (define/public (get-registers) (vector A B (region-index P) I R S T IO (region-index I-index)))
    (define/public (get-dstack-as-list)
      (cons T (cons S (stack->list dstack))))
    (define/public (get-rstack-as-list)
      (cons R (stack->list rstack)))
    (define/public (get-coord) coord)
    (define/public (get-inst-index) iI)

    (define/public (suspended?) suspended)
    (define/public (set-map-node node buffer)
      (printf "set-map-node: ~a ~a" (ga-node-coord node) coord)
      (set! map-node node)
      (set! map-buffer buffer))

    (define (update-display-color color)
      ;; if color is false, clear the display
      (with-current-buffer map-buffer
        (ga-set-usage-color coord color)))

    (define/public (update-map-display global-steps)
      ;; only called when map-node is non-nil
      ;;(assert map-node)
      (when (= (% global-steps activity-history-bucket-size) 0)
        ;; move to next activity history bucket
        (set! activity-history-index (% (add1 activity-history-index)
                                        activity-history-length))
        (set! activity-sum (- activity-sum
                              (vector-ref activity-history activity-history-index)))
        (vector-set! activity-history activity-history-index 0))

      ;;test
      (let ((global-steps (if activity-history-show-total
                              global-steps
                              (* activity-history-length activity-history-bucket-size)))
            (step-counter_ (if activity-history-show-total
                              step-counter
                              activity-sum)))

        (let* ((gsteps (max step-counter_ global-steps))
               (n (floor (* (/ step-counter_ (if (= gsteps 0)
                                                1.0 (* gsteps 1.0))) 255)))
               (s (to-hex-str (- 255 n)))
               (c (format "#ff%s%s" s s)))
          ;;(printf "[~a] ~a ~a ~a\n" coord c n s)
          (update-display-color c)
          )))

    (define/public (load node)
      ;;CODE is a vector of assembled code
      ;;load N words from CODE into memory, default = len(code)
      ;;(reset!) -> must be reset externally before loading
      (define code (node-mem node))
      (set! buffer-map (node-buffer-map node))
      (define n (or (node-len node) (vector-length code)))
      (define index 0)
      (while (< index n)
        (begin (vector-set! memory index (vector-ref code index))
               (set! index (add1 index))))
      ;; If P defaults to warm instead of 0:
      ;;reset calls fetch-I so reset P here again before calling fetch-I
      ;;or the default value (if used) will be incorrectly incremented
      ;;;(reset-p!)
      ;;;(set! P (or (node-p node) P))
      (set! P (or (node-p node) 0));;TODO: this boot descriptor should be set by the compiler

      (set! iI 0)
      (set! A (or (node-a node) A))
      (set! B (or (node-b node) B))
      (set! IO (or (node-io node) IO))
      (for ((v (node-stack node)))
        (d-push! v))
      (let ((funcs (node-extern-funcs node)))
        (when funcs
          (set! extern-functions (make-vector MEM-SIZE false))
          (for ((i (length funcs)))
            (vector-set! extern-functions i (vector-ref funcs i)))))

      (fetch-I)

      (let ((structs (make-hash))
            (addrs (make-hash))
            (names (make-hash)))
        (for ((sym (node-symbols node)))
          (hash-set! structs (symbol-val sym) sym)
          (hash-set! addrs (symbol-val sym) (symbol-address sym))
          (hash-set! names (symbol-address sym) (symbol-val sym)))
        (set! symbols structs)
        (set! ram-addr->name names)
        (set! ram-name->addr addrs)))

    (define/public (execute-array code)
      ;;Execute an array of words.
      ;;Make a fake port and port execute the code through it.
      ;;Used for loading bootstreams through a node.
      (define len (vector-length code))
      (log (rkt-format "execute-array(): len(code)=~a" len))
      (set! P #x300)
      (define index 0)
      (vector-set! memory #x300 (vector
                                 (lambda ()
                                   (if (< index len)
                                       (let ((word (vector-ref code index)))
                                         (set! index (add1 index))
                                         (d-push! word)
                                         t)
                                       (err "execute: invalid index")))
                                 (lambda (x) (err "execute: invalid port"))))

      (set! iI 0)
      (when suspended
        (wakeup)))

    (define/public (call-word! word)
      ;; only used for externally calling words from the debugger
      (if (hash-has-key? symbols word)
          (let ((addr (symbol-address (hash-ref symbols word))))
            (log (rkt-format "call-word!: ~a -> ~a\n" word addr))
            (funcall (vector-ref instructions 3) addr 0)
            (set! iI 0)
            (when suspended
              (wakeup))
            (set! fetching-in-progress false)
            (fetch-I)
            t)
          false))

    (define/public (load-bootstream frames)
      (define jump-addr false)
      (define dest-addr false)
      (define n-words false)
      (define index false)
      (define last false)
      (define word false)

      (define load-bootframe-fn false)

      (define (write-next)
        (if (< index last)
            (begin
              ;;(log (rkt-format "(write-next) index = ~a (last=~a)" index last))
              (set! word (vector-ref frames index))
              (set! index (add1 index))
              (set-memory! dest-addr word))
            (if (or (eq? jump-addr #xae) ;;async
                    (eq? jump-addr #xb6)) ;;sync
                (begin
                  (printf "SECOND FRAME\n")
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;)
                  ;;TEMP FIX:
                  ;; the target chip bootstream is special: the second
                  ;; frame is written to a different path
                  (send (send (get-ga144) coord->node 709)
                        set-post-finish-port-read
                        false)
                  (send (send (get-ga144) coord->node 608)
                        set-post-finish-port-read
                        write-next) ;;mbs: does this transform correctly?
                  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                  ;;(send ga144 show-io-changes t)
                  (apply load-bootframe-fn (list last)))
                (begin (printf "END FRAME\n")
                       (set! post-finish-port-write false)
                       ;;(send (send ga144 coord->node 400)
                       (send (send (get-ga144) coord->node 709)
                             set-post-finish-port-read
                             false)
                       (set! P jump-addr)
                       (fetch-I)
                       (set! iI 0)
                       ))))

      (set! post-finish-port-write write-next)
      ;; Cancel active reads. TODO: single port reads
      (when multiport-read-ports
        (for ((port multiport-read-ports))
          ;;reuse 'receive-port-read' to cancel the read notification
          (send (get-port-node port) receive-port-read port false))
        (set! multiport-read-ports false))
      ;;TODO: don't hardcode next node. convert dest-addr to ludr port

      (send (send (get-ga144) coord->node 709)
            ;;(send ga144 coord->node 400)
            set-post-finish-port-read
            write-next)

      (define (load-bootframe (index_ 0))
        (set! index index_)
        (set! jump-addr (vector-ref frames index))
        (set! dest-addr (vector-ref frames (+ index 1)))
        (set! n-words (vector-ref frames (+ index 2)))
        (set! index (+ index 3))
        (set! last (+ n-words index))

        (when suspended (wakeup))
        (if (port-addr? dest-addr)
            (begin
              ;;(set! step-fn void) ;;TODO:????
              (write-next))
            (begin
              (for ((i (range n-words)))
                (set-memory! i (vector-ref frames index))
                (set! index (add1 index)))
              (set! P jump-addr)
              ;;(error "TODO: Untested case")
              (set! iI 0)
              (fetch-I))))

      ;; stupid hack needed because of the way classes where implemented in elisp
      (set! load-bootframe-fn (lambda (x) (load-bootframe x)))

      (load-bootframe))

    (define (setup-ports)
      (vector-set! memory &LEFT (vector (lambda () (do-port-read LEFT))
                                        (lambda (v) (port-write LEFT v))))
      (vector-set! memory &RIGHT (vector (lambda () (do-port-read RIGHT))
                                         (lambda (v) (port-write RIGHT v))))
      (vector-set! memory &UP (vector (lambda () (do-port-read UP))
                                      (lambda (v) (port-write UP v))))
      (vector-set! memory &DOWN (vector (lambda () (do-port-read DOWN))
                                        (lambda (v) (port-write DOWN v))))
      (vector-set! memory &IO (vector (lambda () (set! fetched-data (read-io-reg)) true)
                                      (lambda (v) (set-io-reg v))))
      (vector-set! memory &--LU
                   (vector (lambda () (do-multiport-read (list LEFT UP)))
                           (lambda (v) (multiport-write (list LEFT UP) v))))
      (vector-set! memory &-D-U
                   (vector (lambda () (do-multiport-read (list DOWN UP)))
                           (lambda (v) (multiport-write (list DOWN UP) v))))
      (vector-set! memory &-DL-
                   (vector (lambda () (do-multiport-read (list DOWN LEFT)))
                           (lambda (v) (multiport-write (list DOWN LEFT) v))))
      (vector-set! memory &-DLU
                   (vector (lambda () (do-multiport-read (list DOWN LEFT UP)))
                           (lambda (v) (multiport-write (list DOWN LEFT UP) v))))
      (vector-set! memory &R--U
                   (vector (lambda () (do-multiport-read (list RIGHT UP)))
                           (lambda (v) (multiport-write (list RIGHT UP) v))))
      (vector-set! memory &R-L-
                   (vector (lambda () (do-multiport-read (list RIGHT LEFT)))
                           (lambda (v) (multiport-write (list RIGHT LEFT) v))))
      (vector-set! memory &R-LU
                   (vector (lambda () (do-multiport-read (list RIGHT LEFT UP)))
                           (lambda (v) (multiport-write (list RIGHT LEFT UP) v))))
      (vector-set! memory &RD--
                   (vector (lambda () (do-multiport-read (list RIGHT DOWN)))
                           (lambda (v) (multiport-write (list RIGHT DOWN) v))))
      (vector-set! memory &RD-U
                   (vector (lambda () (do-multiport-read (list RIGHT DOWN UP)))
                           (lambda (v) (multiport-write (list RIGHT DOWN UP) v))))
      (vector-set! memory &RDL-
                   (vector (lambda () (do-multiport-read (list RIGHT DOWN LEFT)))
                           (lambda (v) (multiport-write (list RIGHT DOWN LEFT) v))))
      (vector-set! memory &RDLU
                   (vector (lambda () (do-multiport-read (list RIGHT DOWN LEFT UP)))
                           (lambda (v) (multiport-write (list RIGHT DOWN LEFT UP) v))
                           ))
      (vector-set! memory &DATA
                   (vector (lambda () (set! fetched-data data) true)
                           (lambda (v) (set! data v) t)))
      )

    (define (load-rom)
      (for ((word (hash-ref rom-ht coord))
            (i (range #x80 #xc0)))
        (vector-set! memory i word)))

    (define/public (reset! (bit 18))
      (set! A 0)
      (set! B (cdr (assoc "io" named-addresses)))
      (set! P 0)
      (set! iI 0)
      (set! R #x15555)
      (set! S #x15555)
      (set! T #x15555)
      (set! IO #x15555)
      (set! memory (make-vector MEM-SIZE #x134a9)) ;; 0x134a9 => 'call 0xa9'
      (set! fetching-in-progress false)
      (set! fetched-data false)
      (set! dstack (make-stack 8 #x15555))
      (set! rstack (make-stack 8 #x15555))
      (set! blocking-read false)
      (set! blocking-write false)
      (set! blocking false)
      (set! multiport-read-ports false)
      (set! writing-nodes (make-vector 4 false))
      (set! reading-nodes (make-vector 4 false))
      (set! port-vals (make-vector 4 false))
      (set! pin-handlers-set-p false)
      (set! WD false)
      (set! ~WD t)
      (set! pin17 false)
      (set! pin5 false)
      (set! pin3 false)
      (set! pin1 false)
      (set! unext-jump-p false)
      (set! break-at-wakeup false)
      (set! break-at-io-change false)
      (set! break-at-step false)
      (set! symbols false)
      (set! history '())
      (set! suspended false)
      (set! inst-counters (and count-instructions (make-vector 32 0)))
      (set! step-counter 0)
      (set! extern-functions false)
      (set! extern-word-functions false)
      (set! activity-history-index 0)
      (set! activity-history (make-vector activity-history-length 0))
      (set! activity-sum 0)
      (reset-breakpoints)
      (reset-p!)
      (load-rom)
      (setup-ports))

    ;; Resets only p
    (define/public (reset-p! (start false))
      (if start
          (set! P start)
          (let ((rom (get-node-rom coord)))
            (if (hash-has-key? rom "cold")
                (set! P (hash-ref rom "cold"))
                (if (hash-has-key? rom "warm")
                    (set! P (hash-ref rom "warm"))
                    (err "ROM does not define 'warm' or 'cold'"))))))

    ;; Executes one step of the program by fetching a word, incrementing
    ;; p and executing the word.
    ;; returns false when P = 0, else t
    (define/public (step-program!)
      (if suspended
          (message "Node %s suspended" coord)
          (step!))
      suspended)

    ;; Steps the program n times.
    (define/public (step-program-n! n)
      (while (and (> n 0)
                  (not suspended))
        (step-program!)
        (setq n (1- n)))
      suspended)

    (define/public (get-inst-counters)
      inst-counters)

    (define/public (get-step-count)
      step-counter)

    (define/public (init)
      (init-ludr-port-nodes)
      (init-io-mask))

    (define (get-port-node port)
      (let ((index (vector-ref ludr-port-nodes port)))
        (if (number? index)
            (vector-ref (lookup-obj ga144-nodes-key) index)
            index)))

    (define (init-ludr-port-nodes)
      ;; ludr-port-nodes now stores the index of the node in ga144-nodes
      ;; instead of the actual node object.
      ;; This prevents circular structures as adjacent nodes no longer
      ;; point directly at each other.
      ;;  avoids elisp error: 'Apparently circular structure being printed'
      (define (convert dir)
        (let ((x (remainder coord 100))
              (y (quotient coord 100)))
          (cond ((equal? dir "north") (if (= (modulo y 2) 0) DOWN UP))
                ((equal? dir "south") (if (= (modulo y 2) 0) UP DOWN))
                ((equal? dir "east") (if (= (modulo x 2) 0) RIGHT LEFT))
                ((equal? dir "west") (if (= (modulo x 2) 0) LEFT RIGHT)))))
      (let (;;(west (send (get-ga144) coord->node (- coord 1)))
            ;;(north (send (get-ga144) coord->node (+ coord 100)))
            ;;(south (send (get-ga144) coord->node (- coord 100)))
            ;;(east (send (get-ga144) coord->node (+ coord 1)))
            (west (coord->index (- coord 1)))
            (north (coord->index (+ coord 100)))
            (south (coord->index (- coord 100)))
            (east (coord->index (+ coord 1))))

        (set! ludr-port-nodes (make-vector 4 false))

        ;;TEMPORARY FIX: create new dummy nodes around the edges of the chip
        (vector-set! ludr-port-nodes (convert "north")
                     (if (< coord 700)
                         north
                         (new f18a% 145 (get-ga144))))

        (vector-set! ludr-port-nodes (convert "east")
                     (if (< (modulo coord 100) 17)
                         east
                         (new f18a% 145 (get-ga144))))
        (vector-set! ludr-port-nodes (convert "south")
                     (if (> coord 17)
                         south
                         (new f18a% 145 (get-ga144))))
        (vector-set! ludr-port-nodes (convert "west")
                     (if (> (modulo coord 100) 0)
                         west
                         (new f18a% 145 (get-ga144))))))

    ;;(define/public (get-ludr-port-nodes) ludr-port-nodes) ;; update -- ludr-port-nodes does not contain the nodes any more

    (define/public (set-aindex index)
      (set! active-index index))

    (define/public (str)
      (rkt-format "<node ~a>" coord))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; breakpoints

    ;; `breakpoints' is a vector of functions that return nil
    ;; when that word has a breakpoint set
    (define breakpoints (make-vector MEM-SIZE false))
    (define break-at-wakeup false)
    (define break-at-step false) ;; false if no break, else number to break at
    (define break-at-io-change false)
    ;; when true, set unset break-at-wakeup everytime it fires
    (define break-at-wakeup-autoreset t)
    (define break-at-io-change-autoreset false)

    (define/public (break-at-next-wakeup)
      (set! break-at-wakeup t))

    (define/public (break-at-next-io-change)
      (set! break-at-io-change t))

    (define/public (set-break-at-step step)
      ;; call with false to unset breakpoint
      (set! break-at-step step))

    (define/public (set-breakpoint line-or-word)
      (define break-count -1)
      (define (break-fn)
        (let ((name (get-memory-name I-index)))
          ;; The first time this is called it triggers a breakpoint.
          ;; The instruction in the slot that triggers the breakpoint is not executed.
          ;; On next step the simulation resumes where it left off but
          ;; the breakpoint must not trigger.
          (set! break-count (1+ break-count))
          (if (eq (% break-count 2) 0) ;;trigger breakpoint on every other call
              (begin (break name)
                     false)
              true)))
      (define addr (get-breakpoint-address line-or-word))
      (when addr
        (log (rkt-format "setting breakpoint for '~a' at word ~a\n" line-or-word addr))
        (vector-set! breakpoints addr break-fn)))

    (define/public (unset-breakpoint line-or-word)
      (define addr (get-breakpoint-address line-or-word))
      (when addr
        (vector-set! breakpoints addr (lambda () t))))

    (define/public (reset-breakpoints)
      (set! breakpoints (make-vector MEM-SIZE (lambda () t)))
      (set! break-at-wakeup false)
      (set! break-at-step false))

    (define/public (set-word-hook-fn line-or-word fn)
      (define addr (get-breakpoint-address line-or-word))
      (if addr
          (vector-set! breakpoints addr (lambda () (funcall fn) t))
          (printf "[~a] set-word-hook-fn -- invalid word" coord line-or-word)))

    (define (get-breakpoint-address line-or-word)
      (cond ((string? line-or-word)
             ;;TODO: include ROM words
             (if (hash-has-key? symbols line-or-word)
                 (symbol-address (hash-ref symbols line-or-word))
                 (begin
                   (log (rkt-format "ERR: no record of word '~a'" line-or-word))
                   false)))
            ((number? line-or-word)
             (if (and (>= line-or-word 0)
                      (< line-or-word MEM-SIZE))
                 line-or-word
                 (begin
                   (log (rkt-format "ERR: invalid address '~a'" line-or-word))
                   false)))
            (else
             (log (rkt-format "ERR: invalid breakpoint '~a'" line-or-word))
             false)))

    (define/public (break (reason false))
      (log (rkt-format "Breakpoint: ~a "
                       (or reason (rkt-format "~x(~x)" P (region-index P)))))
      (send (get-ga144) break this))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; state display debug functions
    (define/public (display-state)
      (printf "_____Node ~a state_____\n" coord)
      (printf "P:~x(~x) A:~x B:~x R:~x IO:~x\n"
              P (region-index P) A B R (read-io-reg))
      (when extended-arith?
        (printf "[extended arith]  carry=~a\n" carry-bit))
      (display-dstack)
      (display-rstack))

    (define/public (display-dstack)
      (printf "|d> ~x ~x" T S)
      (display-stack dstack)
      (printf "\n"))

    (define (display-rstack)
      (printf "|r> ~x" R)
      (display-stack rstack)
      (printf "\n"))

    (define/public (display-memory (n MEM-SIZE))
      (let ((n (sub1 n))
            (i 0)
            (v false))
        (printf "node ~a memory: " coord)
        (while (not (or (eq? v 'end)
                        (>= i n)))
          (begin (set! v (vector-ref memory i))
                 (printf (if (numberp v) "~x " "%s") v)
                 (set! i (add1 i))))
        (printf "\n")))

    (define/public (display-all)
      (display-state)
      (display-memory 10);;TODO
      (let ((names (vector "Left" "Up" "Down" "Right")))
        (printf "reading from port(s): ~a\n"
                (if current-reading-port
                    (if (list? current-reading-port)
                        (string-join (for/list ((port current-reading-port))
                                       (vector-ref names port))
                                     ", ")
                        (vector-ref names current-reading-port))
                    "None"))

        ;;show the multiport read ports if they differ from the reading port list
        (when (and multiport-read-ports
                   (list? current-reading-port)
                   (not (= (length multiport-read-ports)
                           (length  current-reading-port))))
          (printf "multiport read ports: ~a\n"
                  (string-join (for/list ((port multiport-read-ports))
                                 (vector-ref names port))
                               ", ")))

        (printf "writing to port(s): ~a\n"
                (if current-writing-port
                    (if (list? current-writing-port)
                        (string-join (for/list ((port current-writing-port))
                                       (vector-ref names port))
                                     ", ")
                        (vector-ref names current-writing-port))
                    "None"))

        (let ((receiving-reads '())
              (receiving-writes '()))
          (for/list ((node reading-nodes))
            (when node
              (set! receiving-reads (cons node receiving-reads))))
          (for/list ((node writing-nodes))
            (when node
              (set! receiving-writes (cons node receiving-writes))))

          (printf "receiving reads from: ~a\n"
                  (if (null? receiving-reads)
                      "None"
                      (string-join (for/list ((port receiving-reads))
                                     (number->string (send port get-coord)))
                                   ", ")))
          (printf "receiving writes from: ~a\n"
                  (if (null? receiving-writes)
                      "None"
                      (string-join (for/list ((port receiving-writes))
                                     (number->string (send port get-coord)))
                                   ", "))))

        (printf "ludr ports: ~a\n"
                (string-join (for/list ((node 4))
                               (set! node (get-port-node node))
                               (if node
                                   (number->string (send node get-coord))
                                   "___"))
                             ", "))

        (printf "suspended: ~a\n" (if suspended "Yes" "No"))

        (when waiting-for-pin
          (printf "Waiting for pin 17\n"))
        ))

    (define/public (describe-io (describe false))
      ;;set node handshake read bits
      (define io (read-io-reg))
      (printf "IO = ~a, 0x~x, 0b~b\n" io io io)
      (when (vector-ref reading-nodes LEFT)
        (printf "   LEFT reading\n"))
      (when (vector-ref reading-nodes UP)
        (printf "   UP reading\n"))
      (when (vector-ref reading-nodes DOWN)
        (printf "   DOWN reading\n"))
      (when (vector-ref reading-nodes RIGHT)
        (printf "   RIGHT reading\n"))

      ;;set node handshake write bits
      (when (vector-ref writing-nodes LEFT)
        (printf "   LEFT writing\n"))

      (when (vector-ref writing-nodes UP)
        (printf "   UP writing\n"))
      (when (vector-ref writing-nodes DOWN)
        (printf "   DOWN writing\n"))
      (when (vector-ref writing-nodes RIGHT)
        (printf "   RIGHT writing\n"))

      (printf "   (~a gpio pins)\n" num-gpio-pins)
      (when (> num-gpio-pins 0)

        (printf "   pin17:\n")
        (printf "      config: ~a\n" pin1-config)
        (if pin17
            (printf "      pin: HIGH\n")
            (printf "      pin: LOW\n"))
        (when (> num-gpio-pins 1)
          (printf "   pin1:\n")
          (printf "      config: ~a\n" pin2-config)
          (if pin1
              (printf "      pin: HIGH\n")
              (printf "      pin: LOW\n"))
          (when (> num-gpio-pins 2)
            (printf "   pin3:\n")
            (printf "      config: ~a\n" pin3-config)
            (if pin3
                (printf "      pin: HIGH\n")
                (printf "      pin: LOW\n"))
            (and (> num-gpio-pins 3)
                 (printf "   pin5:\n")
                 (printf "      config: ~a\n" pin4-config)
                 (if pin5
                     (printf "      pin: HIGH\n")
                     (printf "      pin: LOW\n")))))))

    (define (get-memory-name index)
      (set! index (& index #xff)) ;; get rid of extended arithmetic bit
      (if (and ram-addr->name
               (hash-has-key? ram-addr->name index))
          (hash-ref ram-addr->name index)
          (if (and rom-symbols
                   (hash-has-key? rom-symbols index))
              (hash-ref rom-symbols index)
              false)))

    (define/public (disassemble-memory (start 0) (end #xff) (show-p? t))
      ;;  disassemble and print a node memory from START to END, inclusive
      (define (pad-print thing (pad 20))
        (let* ((s (rkt-format "~a" thing))
               (len (string-length s))
               (str (string-append s (make-string (- pad len) _char-space))))
          (printf str)))
      (let ((word false)
            (name false))
        (for ((i (range start (add1 end))))
          (set! i (region-index i))
          (when (setq name (get-memory-name i))
            (printf "~a:\n" name))
          (printf (if (and (equal? i P) show-p?) ">" " "))
          (set! word (vector-ref memory i))
          (pad-print i 4)
          (pad-print word)
          (define dis (disassemble-word word))
          (define ind (vector-member "call" dis))
          (when ind
            (let* ((addr (vector-ref dis (add1 ind)))
                   (name (get-memory-name (region-index addr))))
              (vector-set! dis (add1 ind) (rkt-format "~a(~a)" addr name))))
          (define slot-i (get-current-slot-index))

          (when (and (equal? i P) show-p?)
            (vector-set! dis slot-i (rkt-format "{{~a}}" (vector-ref dis slot-i))))
          (printf "~a\n" dis))))

    (define/public (disassemble-local)
      (disassemble-memory (- P 5) (+ P 5)))

    (define/public (get-tagged-memory)
      ;;returns a list of disassembled words

      (let ((mem (make-vector #x1ff false)))
        (for ((i (append (range 0 #x40)
                         (range #x80 #xc0))))
          (vector-set! mem i (cons (vector-ref memory i)
                                   (get-memory-name i))))

        (for ((i (range #xc0 #x1ff)))
          (vector-set! mem i (if (hash-has-key? address-names i)
                                 (hash-ref address-names i)
                                 nil)))
        mem))

    (define/public (print-inst-counters)
      (assert elisp?)
      (for ((i 32))
        (princ (format "%-5s %s\n" (vector-ref opcodes i) (vector-ref inst-counters i)))))

    (define/public (print-inst-counters-horizontal)
      (assert elisp?)
      (let ((len 5)
            (fmt false))
        (for ((i 32))
          (set! len (max len (length (number->string (vector-ref inst-counters i))))))
        (set! fmt (format "%%-%ss " len))
        (for ((i 16))
          (princ (format fmt (vector-ref opcodes i))))
        (princ "\n")
        (for ((i 16))
          (when (= i 16) (princ "\n"))
          (princ (format fmt (vector-ref inst-counters i))))
        (princ "\n\n")
        (for ((i 16))
          (princ (format fmt (vector-ref opcodes (+ i 16)))))
        (princ "\n")
        (for ((i 16))
          (when (= i 16) (princ "\n"))
          (princ (format fmt (vector-ref inst-counters (+ i 16)))))
        (princ "\n")))

    (define/public (get-extern-functions)
      extern-functions)

    (define/public (get-execution-time)
      (when inst-counters
        (let ((sum 0))
          (for ((i 32))
            (set! sum (+ sum (* (vector-ref inst-counters i)
                                (vector-ref opcode-time-v i)))))
          (cons coord sum))))

    (when (valid-coord? coord)
      ;;; dummy edge nodes have a invalid coord so things like rom
      ;;; hash table indexes fail in reset
      (reset!))

    (define _last-instruction-buffer false)
    (define (ga-update-buffer-overlay position)
      (if position
        (let* ((file (car position))
               (buf (and file (find-buffer-visiting file))))
          (when file
            (unless buf
              (set! buf (find-file-other-window file)))
            (set! _last-instruction-buffer buf)
            (with-current-buffer buf
              (unless buffer-inst-overlay
                (set! buffer-inst-overlay (make-overlay 0 0 buf)))
              (move-overlay buffer-inst-overlay (cadr position) (caddr position))
              (overlay-put buffer-inst-overlay 'face (list :background "#ff0000"))
              )))

        ;;if position is null, change the color of the overlay to yellow to indicate
        ;;that the current position is lost, but this is the last known location
        (when (and buffer-inst-overlay _last-instruction-buffer)
          (overlay-put buffer-inst-overlay 'face (list :background "#ffff00")))
        )
      )
    ))
