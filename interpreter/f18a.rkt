#lang racket

(require compatibility/defmacro
         "stack.rkt"
         "../common.rkt"
         "../compiler/disassemble.rkt")

(provide f18a%)

;;(defmacro DEBUG () `(member coord '(708 709 710)))
;;(defmacro DEBUG () `(= coord 709))
(defmacro DEBUG () #f)
(define DISPLAY_STATE? #f)
(defmacro PORT-DEBUG ()
  ;;port-debug-list is: #t, #f, or list of nodes
  ;;(define port-debug-list '(708 709 710))
  (define port-debug-list #f)
  (if (list? port-debug-list)
      `(member coord ',port-debug-list)
      port-debug-list))
(define save-history #t)

(define f18a%
  (class object%
    (super-new)
    (init-field index ga144 [active-index 0])

    (set! active-index index);;index of this node in the 'active-nodes' vector
    (define suspended #f)
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

    ;;value of each gpio pin. #t or #f)
    ;;"All pins reset to weak pulldown"
    (define pin17 #f)
    (define pin5 #f)
    (define pin3 #f)
    (define pin1 #f)

    ;;#t if we are suspended waiting for pin17 to change
    (define waiting-for-pin #f)
    ;;state of WD bit in io register
    (define WD #f)
    (define ~WD #t)

    (define history '())

    (define symbols #f) ;;names -> symbol structs
    (define ram-name->addr #f)
    (define ram-addr->name #f)

    (define (err msg)
      (printf "[~a] ERROR\n" coord)
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
                      (format "~a(~a)"
                              (vector-ref opcodes (car op))
                              (cdr op))
                      (vector-ref opcodes op)))))
      (error msg))

    (define (log msg)
      (printf "[~a] ~a\n" coord msg))

    (define/public (set-pin! pin val)
      ;;val is #f or #t
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
        (d-push! (if val 1 0))
        (finish-port-read val)))

    (define (18bit n)
      (if (number? n);;temp fix
          (& n #x3ffff)
          n))

    ;;bit of each gpio pin in the io register
    (define pin17-bit (<< 1 17))
    (define pin5-bit (<< 1 5))
    (define pin3-bit (<< 1 3))
    (define pin1-bit 1)
    ;;bits of each read/write status bit in the io register
    (define Rr- (18bit (~ (<< 1 16))))
    (define Rw (<< 1 15))
    (define Dr- (18bit (~ (<< 1 14))))
    (define Dw (<< 1 13))
    (define Lr- (18bit (~ (<< 1 12))))
    (define Lw (<< 1 11))
    (define Ur- (18bit (~ (<< 1 10))))
    (define Uw (<< 1 9))

    (define memory #f)
    (define instructions (make-vector 35))

    (define blocking-read #f)
    (define blocking-write #f)
    (define blocking #f)
    (define multiport-read-ports #f)

    (define carry-bit 0)
    (define extended-arith? #f)

    (define rom-symbols (let ((ht (make-hash)))
                          (for ((sym (hash->list (get-node-rom coord))))
                            (hash-set! ht (region-index (cdr sym)) (car sym)))
                          ht))

    ;; Pushes to the data stack.
    (define (d-push! value)
      (push-stack! dstack S)
      (set! S T)
      (set! T (18bit value)))

    ;; Pushes to the rstack stack.
    (define (r-push! value)
      (push-stack! rstack R)
      (set! R value))

    ;; Pops from the data stack.
    (define (d-pop!)
      (let ([ret-val T])
        (set! T S)
        (set! S (pop-stack! dstack))
        ret-val))

    ;; Pops from the rstack stack.
    (define (r-pop!)
      (let ([ret-val R])
        (set! R (pop-stack! rstack))
        ret-val))

    ;; Return the value of p or a incremented as appropriately. If the
    ;; register points to an IO region, does nothing. Otherwise increment
    ;; the register circularly within the current memory region (RAM or
    ;; ROM).
    (define (incr curr) ;; DB001 section 2.2
      (if (> (& curr #x100) 0)
          curr
          (let ([bit9 (& curr #x200)]
                [addr (& curr #xff)])
            (ior (cond [(< addr #x7F) (add1 addr)]
                       [(= addr #x7F) 0]
                       [(< addr #xFF) (add1 addr)]
                       [(= addr #xFF) #x80])
                 bit9))))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; suspension and wakeup

    (define (remove-from-active-list)
      (if suspended
          (err "cannot remove suspended node from active list")
          (send ga144 remove-from-active-list this)))

    (define (add-to-active-list)
      (if suspended
          (send ga144 add-to-active-list this)
          (err "cannot add active node to active list")))

    (define (suspend)
      (when (PORT-DEBUG) (printf "[~a] suspending\n" coord))
      (remove-from-active-list)
      (set! suspended #t)
      (when break-at-io-change
        (when break-at-io-change-autoreset
          (set! break-at-wakeup #f)) ;;TODO:???
        (break "io change - suspend")))

    (define (wakeup)
      (when (PORT-DEBUG) (printf "[~a] wakeup\n" coord))
      (add-to-active-list)
      (set! suspended #f)
      (if break-at-wakeup
          (begin
            (when break-at-wakeup-autoreset
              (set! break-at-wakeup #f))
            (break "wakeup"))
          (when break-at-io-change
            (when break-at-io-change-autoreset
              (set! break-at-wakeup #f))
            (break "io change - wakup"))))
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; ludr port communication

    ;; adjacent nodes that are suspended, waiting for this node to read to a port
    (define writing-nodes (make-vector 4 #f))

    ;;values written to a ludr port by a pending write
    ;;each node in the 'writing-nodes' array has a corresponding value here
    (define port-vals (make-vector 4 #f))

    ;; adjacent nodes that are suspended, waiting for this node to write to a port
    (define reading-nodes (make-vector 4 #f))

    ;;maps ludr ports to adjacent nodes
    (define ludr-port-nodes #f)

    ;;port we are currently reading from
    ;;used for debugging. If not #f, node must be suspended
    ;;This is a list if doing a multiport read.
    (define current-reading-port #f)
    (define/public (get-current-reading-port)
      (and current-reading-port
           (if (list? current-reading-port)
               (length current-reading-port)
               (vector-ref (vector "l" "u" "d" "r") current-reading-port))))

    ;;port we are currently writing to
    ;;used for debugging. If not #f, node must be suspended
    (define current-writing-port #f)
    (define/public (get-current-writing-port)
      (and current-writing-port
           (vector-ref (vector "L" "U" "D" "R") current-writing-port)))

    (define (port-read port)
      (when (PORT-DEBUG) (printf "[~a](port-read ~a)\n" coord port))
      ;;read a value from a ludr port
      ;;returns #t if value is on the stack, #f if we are suspended waiting for it
      (if (eq? port wake-pin-port)
          ;;reading from pin17
          (if (eq? pin17 ~WD)
              (begin (d-push! (if pin17 1 0))
                     #t)
              (begin ;;else: suspend while waiting for pin to change
                (set! waiting-for-pin #t)
                (suspend)
                #f))
          ;;else: normal inter-node read
          (let ((writing-node (vector-ref writing-nodes port)))
            (if writing-node
                (begin ;;value was ready
                  (when (PORT-DEBUG) (printf "       value was ready: ~a\n"
                                             (vector-ref port-vals port)))
                  (d-push! (vector-ref port-vals port))
                  ;;clear state from last reading
                  (vector-set! writing-nodes port #f)
                  ;;let other node know we read the value
                  (send writing-node finish-port-write)
                  #t)
                (begin ;;else: suspend while we wait for other node to write
                  (when (PORT-DEBUG) (printf "       suspending\n"))
                  (send (vector-ref ludr-port-nodes port)
                        receive-port-read port this)
                  (set! current-reading-port port)
                  (suspend)
                  #f)))))

    (define (multiport-read ports)
      (when (PORT-DEBUG) (printf "[~a](multiport-read ~a)\n" coord ports))
      (let ([done #f]
            [writing-node #f]
            [other #f])
        (for ([port ports])
          (if (eq? port wake-pin-port)
              ;;reading from pin17
              (begin (log "multiport-read: reading pin17")
                     (when (eq? pin17 ~WD)
                       (d-push! (if pin17 1 0))
                       (set! done #t)))
              (when (setq writing-node (vector-ref writing-nodes port))
                ;;an ajacent writing node is waiting for us to read its value
                (if done
                    (err "multiport-read -- more then one node writing")
                    (begin
                      (when (PORT-DEBUG)
                        (printf "       value was ready: ~a\n"
                                (vector-ref port-vals port)))
                      (d-push! (vector-ref port-vals port))
                      (vector-set! writing-nodes port #f)
                      (send writing-node finish-port-write)
                      (set! done #t))))))
        (if done ;;successfully read a value from one of the ports
            #t
            (begin ;;else: suspend while we wait for an other node to write
              (when (member wake-pin-port ports)
                (set! waiting-for-pin #t))
              (set! multiport-read-ports '())
              (for ([port ports])
                ;;(unless (vector-ref ludr-port-nodes port)
                ;;  (raise (format "multiport-read: node ~a does not have a ~a port"
                ;;                 coord (vector-ref port-names port))))
                ;;TODO: this is a temp fix:
                ;;     the current default instruction word is 'jump ludr'
                ;;     If the node is on the edge this does not work as at least
                ;;     one of the ports in ludr-port-nodes is #f.
                ;;     Collect the valid nodes into 'multiport-read-ports'
                ;;     for use in 'finish-port-read()'
                (when (setq other (vector-ref ludr-port-nodes port))
                  (send other receive-port-read port this)
                  (set! multiport-read-ports (cons port multiport-read-ports))))
              (set! current-reading-port ports)
              (suspend)
              #f))))

    (define (port-write port value)
      (when (PORT-DEBUG)
        (printf "[~a](port-write ~a  ~a)\n" coord port value))
      ;;writes a value to a ludr port
      (let ((reading-node (vector-ref reading-nodes port)))
        (if reading-node
            (begin
              (when (PORT-DEBUG) (printf "       target is ready\n"))
              (vector-set! reading-nodes port #f)
              (send reading-node finish-port-read value)
              #t)
            (begin
              (send (vector-ref ludr-port-nodes port)
                    receive-port-write port value this)
              (set! current-writing-port port)
              (suspend)
              #f))))

    (define (multiport-write ports value)
      ;; "every node that intends to read the value written
      ;;  must already be doing so and suspended"
      (when (PORT-DEBUG)
        (printf "[~a](multiport-write ~a  ~a)\n" coord ports value))
      (let ([reading-node #f])
        (for ([port ports])
          (when (setq reading-node (vector-ref reading-nodes port))
            (when (PORT-DEBUG) (printf "       wrote to port: ~a\n" port))
            (vector-set! reading-nodes port #f)
            (send reading-node finish-port-read value))))
      #t)

    (define post-finish-port-read #f)
    (define/public (set-post-finish-port-read fn)
      (set! post-finish-port-read fn))

    (define/public (finish-port-read val)
      ;;called by adjacent node when it writes to a port we are reading from)
      ;;or when a pin change causes node to awaken
      (when (PORT-DEBUG) (printf "[~a](finish-port-read  ~a)\n" coord val))
      (d-push! val)
      (when multiport-read-ports
        ;;there may be other nodes that still think we are waiting for them to write
        (for ([port multiport-read-ports])
          ;;reuse 'receive-port-read' to cancel the read notification
          (send (vector-ref ludr-port-nodes port) receive-port-read port #f))
        (set! multiport-read-ports #f))
      (set! current-reading-port #f)
      (set! waiting-for-pin #f)
      (wakeup)
      (and post-finish-port-read (post-finish-port-read)))

    (define post-finish-port-write #f)
    (define/public (finish-port-write)
      ;;called by adjacent node when it reads from a port we are writing to
      (when (PORT-DEBUG) (printf "[~a](finish-port-write)\n" coord))
      (set! current-writing-port #f)
      (wakeup)
      (and post-finish-port-write (post-finish-port-write)))

    (define/public (receive-port-read port node)
      ;;called by adjacent node when it is reading from one of our ports
      (when (PORT-DEBUG)
        (printf "[~a](receive-port-read ~a   ~a)\n"
                coord port (and node (send node str))))
      (vector-set! reading-nodes port node))

    (define/public (receive-port-write port value node)
      (when (PORT-DEBUG)
        (printf "[~a](receive-port-write ~a  ~a  ~a)\n"
                coord port value (send node str)))
      ;;called by adjacent node when it is writing to one of our ports
      (vector-set! writing-nodes port node)
      (vector-set! port-vals port value))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define prev-IO IO)
    (define num-gpio-pins (let ((pins (assoc coord node-to-gpio-pins)))
                            (if pins (cdr pins) 0)))
    ;;the wake-pin-port is the port to read from for pin17 wakeup, UP or LEFT
    (define wake-pin-port #f)
    (when (> num-gpio-pins 0)
      (if (or (> coord 700)
              (< coord 17))
          (set! wake-pin-port UP)
          (set! wake-pin-port LEFT)))

    ;;the io read masks are used for isolating the parts of the io register
    ;;that are in use by a given node's io facilities. Bits that are not in
    ;;use read the inverse of what was last written to them
    (define io-read-mask 0)
    (define ~io-read-mask #f)

    (define (init-io-mask)
      ;;this must be called after `ludr-port-nodes' is initialized
      (when (> num-gpio-pins 0)
        ;;add the gpio bits to the io read mask
        (set! io-read-mask
          (vector-ref (vector #f #x20000 #x20002 #x2000a #x2002a)
                      num-gpio-pins)))
      ;;add the status bits
      (when (vector-ref ludr-port-nodes 0)
        (set! io-read-mask (ior io-read-mask #x1800)))
      (when (vector-ref ludr-port-nodes 1)
        (set! io-read-mask (ior io-read-mask #x600)))
      (when (vector-ref ludr-port-nodes 2)
        (set! io-read-mask (ior io-read-mask #x6000)))
      (when (vector-ref ludr-port-nodes 3)
        (set! io-read-mask (ior io-read-mask #x18000)))

      (set! ~io-read-mask (18bit (~ io-read-mask)))
      (set! io-read-default (& #x15555 io-read-mask)))

    ;;the default io register read bits excluding those
    ;;that are used to control io facilities.
    (define io-read-default #f)

    (define pin1-ctl-mask #x30000)
    (define pin2-ctl-mask #x3)
    (define pin3-ctl-mask #x12)
    (define pin4-ctl-mask #x30)

    (define pin1-handler #f)
    (define pin2-handler #f)
    (define pin3-handler #f)
    (define pin4-handler #f)
    (define pin-handlers-set-p #f)

    (enum (IMPED PULLDOWN SINK HIGH))

    (define/public (set-gpio-handlers a [b #f] [c #f] [d #f])
      (set! pin1-handler a)
      (set! pin2-handler b)
      (set! pin3-handler c)
      (set! pin4-handler d)
      (set! pin-handlers-set-p #t))

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
            (and pin5
                 (set! io (ior io pin5-bit)))
            (when (> num-gpio-pins 2)
              (and pin3
                   (set! io (ior io pin3-bit)))
              (and (> num-gpio-pins 3)
                   pin1
                   (set! io (ior io pin1-bit))))))

        io))

    (define (push-io-reg)
      (d-push! (read-io-reg))
      #t)

    (define (set-io-reg val)
      (set! prev-IO IO)
      (set! IO val)
      (set! WD (if (= (& (>> IO 11) 1) 1) #t #f))
      (set! ~WD (not WD))
      ;;if a digital pin control field changed, notify its handlers
      (when (and (> num-gpio-pins 0)
                 pin-handlers-set-p)
        (let ((changed (^ prev-IO IO)))
          (and (& changed pin1-ctl-mask)
               pin1-handler
               (pin1-handler (& IO pin1-ctl-mask)))
          (when (> num-gpio-pins 1)
            (and (& changed pin2-ctl-mask)
                 pin2-handler
                 (pin2-handler (& IO pin2-ctl-mask)))
            (when (> num-gpio-pins 2)
              (and (& changed pin3-ctl-mask)
                   pin3-handler
                   (pin3-handler (& IO pin3-ctl-mask)))
              (and (> num-gpio-pins 3)
                   (& changed pin4-ctl-mask)
                   pin4-handler
                   (pin4-handler (& IO pin4-ctl-mask)))))))
      #t)
    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; memory accesses

    (define (port-addr? addr)
      ;;True if ADDR is an IO port or register, else False
      (> (& addr #x100) 0))

    ;;Port address in the 'memory' vector (0x100 - 0x1ff) contain
    ;;vectors of functions to call when reading or writing to that port.
    ;;First element is the read function, second is the write function.
    ;;Invalid port numbers have the value #f

    (define (read-memory addr)
      ;;pushes the value at memory location ADDR onto the data stack
      (set! addr (& addr #x1ff))
      (when (or (< addr 0)
                (>= addr MEM-SIZE))
        (err (format "(read-memory ~a) out of range" addr)))
      (if (port-addr? addr)
          (let ((x (vector-ref memory addr)))
            (if (vector? x)
                ((vector-ref x 0))
                (err (format "read-memory(~a) - invalid port\n" addr))))
          (d-push! (vector-ref memory (region-index addr)))))

    (define (set-memory! addr value)
      (set! addr (& addr #x1ff))
      (when (or (< addr 0)
                (>= addr MEM-SIZE))
        (err (format "(read-memory ~a) out of range" addr)))
      (if (port-addr? addr)
          (let ((x (vector-ref memory addr)))
            (if x
                (if (vector? x)
                    ((vector-ref x 1) value)
                    (err (format "trying to access undefined port: 0x~x" addr)))
                (err (format "set-memory!(~a, ~a) - invalid port\n" addr value))))
          (vector-set! memory (region-index addr) value)))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; instruction execution

    (define unext-jump-p #f)

    (define/public (execute! opcode [jump-addr-pos 0] [addr-mask #f])
      (when save-history
        (if (< opcode 8)
            (set! history (cons (cons opcode jump-addr-pos) history))
            (set! history (cons opcode history))))
      (if (< opcode 8)
          ((vector-ref instructions opcode)
           (bitwise-bit-field I 0 jump-addr-pos)
           addr-mask)
          ((vector-ref instructions opcode))))

    (define (step0-helper)
      (set! I (d-pop!))
      (set! I^ (^ I #x15555))
      (if (vector-ref breakpoints (if (port-addr? P) (& P #x1ff) (region-index P)))
          (begin (let ((name (get-memory-name P)))
                   (set! P (incr P))
                   (set! step-fn step-0-execute)
                   (break name)))
          (begin (set! P (incr P))
                 (if (eq? I 'end)
                     (suspend)
                     (step-0-execute)))))

    (define (step-0-execute)
      (set! step-fn (if (execute! (bitwise-bit-field I^ 13 18) 10 #x3fc00)
                        step1
                        step0)))

    (define (step0)
      (if unext-jump-p
          (begin
            (set! unext-jump-p #f)
            (step-0-execute))
          (if (read-memory P)
              ;;TODO: FIX: this should not use the stack
              ;;           we could loose the last item on the stack this way
              (step0-helper)
              ;;else: we are now suspended waiting for the value
              (set! step-fn step0-helper))))

    (define (step1)
      (set! step-fn (if (execute! (bitwise-bit-field I^ 8 13) 8 #x3ff00)
                        step2
                        step0)))

    (define (step2)
      (set! step-fn (if (execute! (bitwise-bit-field I^ 3 8) 3 #x3fff8)
                        step3
                        step0)))

    (define (step3)
      (execute! (<< (bitwise-bit-field I^ 0 3) 2))
      (set! step-fn step0))

    (define step-fn step0)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; instructions
    (define (init-instructions)
      (define _n 0)
      ;; Define a new instruction. An instruction can
      ;; abort the rest of the current word by returning #f.
      (define-syntax-rule (define-instruction! opcode args body ...)
        (begin (vector-set! instructions
                            _n
                            (if (DEBUG)
                                (lambda args
                                  (printf (format "[~a]OPCODE: '~a'\n" coord opcode))
                                  body ...)
                                (lambda args
                                  body ...)))
               (set! _n (add1 _n))))

      (define-instruction! ";" (_ __)
        (set! P R)
        (r-pop!)
        #f)

      (define-instruction! "ex" (_ __)
        (define temp P)
        (set! P R)
        (set! R temp)
        #f)

      (define-instruction! "jump" (addr mask)
        (when (DEBUG) (printf "jump to ~a\n" addr))
        (set! extended-arith? (bitwise-bit-set? addr 9))
        (set! P (ior addr (& P mask)))
        #f)

      (define-instruction! "call" (addr mask)
        (when (DEBUG) (printf "calling: ~a\n" addr))
        (set! extended-arith? (bitwise-bit-set? addr 9))
        (r-push! P)
        (set! P (ior addr (& P mask)))
        #f)

      (define-instruction! "unext" (_ __)
        (if (= R 0)
            (r-pop!)
            (begin (set! R (sub1 R))
                   (set! unext-jump-p #t)
                   (set! step-fn step0)
                   #f)))

      (define-instruction! "next" (addr mask)
        (if (= R 0)
            (begin (r-pop!)
                   #f)
            (begin (set! R (sub1 R))
                   (set! P (ior addr (& P mask)))
                   #f)))

      (define-instruction! "if" (addr mask)
        (and (= T 0)
             (begin (when (DEBUG) (printf "If: jumping to ~a\n" addr))
                    (set! P (ior addr (& P mask))))
             #f))

      (define-instruction! "-if" (addr mask)
        (and (not (bitwise-bit-set? T 17))
             (set! P (ior addr (& P mask)))
             #f))

      (define-instruction! "@p" ()
        (read-memory P)
        (set! P (incr P)))

      (define-instruction! "@+" () ; fetch-plus
        (read-memory (& A #x1ff))
        (set! A (incr A)))

      (define-instruction! "@b" () ;fetch-b
        (read-memory B)
        #t)

      (define-instruction! "@" (); fetch a
        (read-memory (& A #x1ff)) #t)

      (define-instruction! "!p" () ; store p
        (set-memory! P (d-pop!))
        (set! P (incr P)) #t)

      (define-instruction! "!+" () ;store plus
        (set-memory! A (d-pop!))
        (set! A (incr A)) #t)

      (define-instruction! "!b" (); store-b
        (set-memory! B (d-pop!)) #t)

      (define-instruction! "!" (); store
        (set-memory! (& A #x1ff)  (d-pop!)) #t)

      (define-instruction! "+*" () ; multiply-step
        ;;case 1 - If bit A0 is zero
        ;;  Treats T:A as a single 36 bit register and shifts it right by one
        ;;  bit. The most signficicant bit (T17) is kept the same.
        ;;case 2 - If bit A0 is one
        ;;  Sums T and S and concatenates the result with A, shifting
        ;;  everything to the right by one bit to replace T:A
        (if (& A 1)
            ;;case 2:
            (let* ([sum (if extended-arith?
                            (let ([sum (+ T S carry-bit)])
                              (set! carry-bit (if (bitwise-bit-set? sum 18) 1 0))
                              sum)
                            (+ T S))]
                   [sum17 (& sum #x20000)]
                   [result (ior (<< sum 17)
                                (>> A 1))])
              (set! A (bitwise-bit-field result 0 18))
              (set! T (ior sum17 (bitwise-bit-field result 18 36))))
            ;;case 2:
            (let ([t17 (& T #x20000)]
                  [t0  (& T #x1)])
              (set! T (ior t17 (>> T 1)))
              (set! A (ior (<< t0 17)
                           (>> A 1))))))

      (define-instruction! "2*" ()
        (set! T (18bit (<< T 1))))

      (define-instruction! "2/" ()
        (set! T (>> T 1)))

      (define-instruction! "-" () ;not
        (set! T (18bit (bitwise-not T))))

      (define-instruction! "+" ()
        (if extended-arith?
            (let ([sum (+ (d-pop!) (d-pop!) carry-bit)])
              (set! carry-bit (if (bitwise-bit-set? sum 18) 1 0))
              (d-push! (18bit sum)))
            (d-push! (18bit (+ (d-pop!) (d-pop!))))))

      (define-instruction! "and" ()
        (d-push! (& (d-pop!) (d-pop!))))

      (define-instruction! "or" ()
        (d-push! (^ (d-pop!) (d-pop!))))

      (define-instruction! "drop" ()
        (d-pop!))

      (define-instruction! "dup" ()
        (d-push! T))

      (define-instruction! "pop" ()
        (d-push! (r-pop!)))

      (define-instruction! "over" ()
        (d-push! S))

      (define-instruction! "a" ()  ; read a
        (d-push! A));;??

      (define-instruction! "." ()
        (void))

      (define-instruction! "push" ()
        (r-push! (d-pop!)))

      (define-instruction! "b!" () ;; store into b
        (set! B (& (d-pop!) #x1ff)))

      (define-instruction! "a!" () ;store into a
        (set! A (d-pop!))))

    (init-instructions)

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; public methods

    (define/public (get-memory) memory)
    (define/public (get-rstack) rstack)
    (define/public (get-dstack) dstack)
    (define/public (get-registers) (vector A B P I R S T IO))
    (define/public (get-dstack-as-list)
      (cons T (cons S (stack->list dstack))))
    (define/public (get-rstack-as-list)
      (cons R (stack->list rstack)))
    (define/public (get-coord) coord)

    (define/public (suspended?) suspended)

    (define/public (load node)
      ;;CODE is a vector of assembled code
      ;;load N words from CODE into memory, default = len(code)
      (define code (node-mem node))
      (define n (or (node-len node) (vector-length code)))
      (define (load index)
        (when (< index n)
          (vector-set! memory index (vector-ref code index))
          (load (add1 index))))
      (load 0)
      (set! P (or (node-p node) 0))
      (let ((structs (make-hash))
            (addrs (make-hash))
            (names (make-hash)))
        (for ((sym (node-symbols node)))
          (hash-set! structs (symbol-name sym) sym)
          (hash-set! addrs (symbol-name sym) (symbol-address sym))
          (hash-set! names (symbol-address sym) (symbol-name sym)))
        (set! symbols structs)
        (set! ram-addr->name names)
        (set! ram-name->addr addrs)))

    (define/public (execute-array code)
      ;;Execute an array of words.
      ;;Make a fake port and port execute the code through it.
      ;;Used for loading bootstreams through a node.
      (define len (vector-length code))
      (printf "execute-array(): len(code)=~a\n" len)
      (set! P #x300)
      (define index 0)
      (vector-set! memory #x300 (vector
                                 (lambda ()
                                   (if (< index len)
                                       (let ((word (vector-ref code index)))
                                         (set! index (add1 index))
                                         (d-push! word)
                                         #t)
                                       (err "execute: invalid index")))
                                 (lambda (x) (err "execute: invalid port"))))

      (set! step-fn step0)
      (when suspended
        (wakeup)))

    (define/public (load-bootstream frames)
      (define jump-addr #f)
      (define dest-addr #f)
      (define n-words #f)
      (define index #f)
      (define last #f)
      (define word #f)
      (define (write-next)
        (if (< index last)
            (begin
              (set! word (vector-ref frames index))
              (set! index (add1 index))
              (set-memory! dest-addr word))
            (if (eq? jump-addr #xae)
                (load-bootframe last)
                (begin (set! post-finish-port-write #f)
                       (send (send ga144 coord->node 709)
                             set-post-finish-port-read
                             #f)
                       (set! P jump-addr)
                       (set! step-fn step0)))))

      (set! post-finish-port-write write-next)
      ;; Cancel active reads. TODO: single port reads
      (when multiport-read-ports
        (for ([port multiport-read-ports])
          ;;reuse 'receive-port-read' to cancel the read notification
          (send (vector-ref ludr-port-nodes port) receive-port-read port #f))
        (set! multiport-read-ports #f))
      ;;TODO: don't hardcode next node. convert dest-addr to ludr port
      (send (send ga144 coord->node 709)
            set-post-finish-port-read
            write-next)

      (define (load-bootframe [index_ 0])
        (set! index index_)
        (set! jump-addr (vector-ref frames index))
        (set! dest-addr (vector-ref frames (+ index 1)))
        (set! n-words (vector-ref frames (+ index 2)))
        (set! index (+ index 3))
        (set! last (+ n-words index))
        (when suspended (wakeup))
        (if (port-addr? dest-addr)
            (write-next)
            (begin
              (for ((i (range n-words)))
                (set-memory! i (vector-ref frames index))
                (set! index (add1 index)))
              (set! P jump-addr)
              (set! step-fn step0))))
      (load-bootframe))

    (define (setup-ports)
      (vector-set! memory &LEFT (vector (lambda () (port-read LEFT))
                                        (lambda (v) (port-write LEFT v))))
      (vector-set! memory &RIGHT (vector (lambda () (port-read RIGHT))
                                         (lambda (v) (port-write RIGHT v))))
      (vector-set! memory &UP (vector (lambda () (port-read UP))
                                      (lambda (v) (port-write UP v))))
      (vector-set! memory &DOWN (vector (lambda () (port-read DOWN))
                                        (lambda (v) (port-write DOWN v))))
      (vector-set! memory &IO (vector (lambda () (push-io-reg))
                                      (lambda (v) (set-io-reg v))))
      (vector-set! memory &--LU
                   (vector (lambda () (multiport-read (list LEFT UP)))
                           (lambda (v) (multiport-write (list LEFT UP) v))))
      (vector-set! memory &-D-U
                   (vector (lambda () (multiport-read (list DOWN UP)))
                           (lambda (v) (multiport-write (list DOWN UP) v))))
      (vector-set! memory &-DL-
                   (vector (lambda () (multiport-read (list DOWN LEFT)))
                           (lambda (v) (multiport-write (list DOWN LEFT) v))))
      (vector-set! memory &-DLU
                   (vector (lambda () (multiport-read (list DOWN LEFT UP)))
                           (lambda (v) (multiport-write (list DOWN LEFT UP) v))))
      (vector-set! memory &R--U
                   (vector (lambda () (multiport-read (list RIGHT UP)))
                           (lambda (v) (multiport-write (list RIGHT UP) v))))
      (vector-set! memory &R-L-
                   (vector (lambda () (multiport-read (list RIGHT LEFT)))
                           (lambda (v) (multiport-write (list RIGHT LEFT) v))))
      (vector-set! memory &R-LU
                   (vector (lambda () (multiport-read (list RIGHT LEFT UP)))
                           (lambda (v) (multiport-write (list RIGHT LEFT UP) v))))
      (vector-set! memory &RD--
                   (vector (lambda () (multiport-read (list RIGHT DOWN)))
                           (lambda (v) (multiport-write (list RIGHT DOWN) v))))
      (vector-set! memory &RD-U
                   (vector (lambda () (multiport-read (list RIGHT DOWN UP)))
                           (lambda (v) (multiport-write (list RIGHT DOWN UP) v))))
      (vector-set! memory &RDL-
                   (vector (lambda () (multiport-read (list RIGHT DOWN LEFT)))
                           (lambda (v) (multiport-write (list RIGHT DOWN LEFT) v))))
      (vector-set! memory &RDLU
                   (vector (lambda () (multiport-read (list RIGHT DOWN LEFT UP)))
                           (lambda (v) (multiport-write (list RIGHT DOWN LEFT UP) v))
                           ))
      (vector-set! memory &DATA
                   (vector (lambda () (d-push! data) #t)
                           (lambda (v) (set! data v) #t)))
      )

    (define (load-rom)
      (for ((word (hash-ref rom-ht coord))
            (i (range #x80 #xc0)))
        (vector-set! memory i word)))

    (define/public (reset! [bit 18])
      (set! A 0)
      (set! B (cdr (assoc "io" named-addresses)))
      (set! P 0)
      (set! I 0)
      (set! R #x15555)
      (set! S #x15555)
      (set! T #x15555)
      (set! IO #x15555)
      (set! memory (make-vector MEM-SIZE #x134a9)) ;; 0x134a9 => 'call 0xa9'
      (set! dstack (make-stack 8 #x15555))
      (set! rstack (make-stack 8 #x15555))
      (set! blocking-read #f)
      (set! blocking-write #f)
      (set! blocking #f)
      (set! multiport-read-ports #f)
      (set! writing-nodes (make-vector 4 #f))
      (set! reading-nodes (make-vector 4 #f))
      (set! port-vals (make-vector 4 #f))
      (set! step-fn step0)
      (set! pin-handlers-set-p #f)
      (set! WD #f)
      (set! ~WD #t)
      (set! pin17 #f)
      (set! pin5 #f)
      (set! pin3 #f)
      (set! pin1 #f)
      (set! unext-jump-p #f)
      (set! break-at-wakeup #f)
      (set! break-at-io-change #f)
      (set! symbols #f)
      (reset-breakpoints)
      (reset-p!)
      (load-rom)
      (setup-ports))

    ;; Resets only p
    (define/public (reset-p! [start #f])
      (if start
          (set! P start)
          (let ((rom (get-node-rom coord)))
            (if (hash-has-key? rom "cold")
                (set! P (hash-ref rom "cold"))
                (if (hash-has-key? rom "warm")
                    (set! P (hash-ref rom "warm"))
                    (err "ROM does not define 'warm' or 'cold')"))))))

    ;; Executes one step of the program by fetching a word, incrementing
    ;; p and executing the word.
    ;; returns #f when P = 0, else #t
    (define/public (step-program!)
      (when (DEBUG) (printf "\nstep-program! node ~a\n" coord))
      (step-fn)
      (when (and (DEBUG) DISPLAY_STATE?) (send ga144 display-node-states
                                               (list coord)))
      )

    ;; Steps the program n times.
    (define/public (step-program-n! n)
      (for ([i (in-range 0 n)]) (step-program!)))

    (define/public (init)
      (init-ludr-port-nodes)
      (init-io-mask))

    (define (init-ludr-port-nodes)
      (define (convert dir)
        (let ([x (remainder coord 100)]
              [y (quotient coord 100)])
          (cond [(equal? dir "north") (if (= (modulo y 2) 0) DOWN UP)]
                [(equal? dir "south") (if (= (modulo y 2) 0) UP DOWN)]
                [(equal? dir "east") (if (= (modulo x 2) 0) RIGHT LEFT)]
                [(equal? dir "west") (if (= (modulo x 2) 0) LEFT RIGHT)])))
      (let ([west (send ga144 coord->node (- coord 1))]
            [north (send ga144 coord->node (+ coord 100))]
            [south (send ga144 coord->node (- coord 100))]
            [east (send ga144 coord->node (+ coord 1))])
        (set! ludr-port-nodes (make-vector 4))

        ;;TEMPORARY FIX: create new dummy nodes around the edges of the chip
        (vector-set! ludr-port-nodes (convert "north")
                     (if (< coord 700)
                         north
                         (new f18a% [index 145] [ga144 ga144])))

        (vector-set! ludr-port-nodes (convert "east")
                     (if (< (modulo coord 100) 17)
                         east
                         (new f18a% [index 145] [ga144 ga144])))
        (vector-set! ludr-port-nodes (convert "south")
                     (if (> coord 17)
                         south
                         (new f18a% [index 145] [ga144 ga144])))
        (vector-set! ludr-port-nodes (convert "west")
                     (if (> (modulo coord 100) 0)
                         west
                         (new f18a% [index 145] [ga144 ga144])))))

    (define/public (get-ludr-port-nodes) ludr-port-nodes)

    (define/public (set-aindex index)
      (set! active-index index))

    (define/public (str)
      (format "<node ~a>" coord))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; breakpoints

    (define breakpoints (make-vector MEM-SIZE #f))
    (define break-at-wakeup #f)
    (define break-at-io-change #f)
    ;; when true, set unset break-at-wakeup everytime it fires
    (define break-at-wakeup-autoreset #t)
    (define break-at-io-change-autoreset #f)

    (define/public (break-at-next-wakeup)
      (set! break-at-wakeup #t))

    (define/public (break-at-next-io-change)
      (set! break-at-io-change #t))

    (define/public (set-breakpoint line-or-word)
      (define addr (get-breakpoint-address line-or-word))
      (when addr
        (vector-set! breakpoints addr #t)))

    (define/public (unset-breakpoint line-or-word)
      (define addr (get-breakpoint-address line-or-word))
      (when addr
        (vector-set! breakpoints addr #f)))

    (define/public (reset-breakpoints)
      (set! breakpoints (make-vector MEM-SIZE #f))
      (set! break-at-wakeup #f))

    (define (get-breakpoint-address line-or-word)
      (cond ((string? line-or-word)
             ;;TODO: include ROM words
             (if (hash-has-key? symbols line-or-word)
                 (symbol-address (hash-ref symbols line-or-word))
                 (begin
                   (printf "[~a] ERR: no record of word '~a'\n" coord line-or-word)
                   #f)))
            ((number? line-or-word)
             (if (and (>= line-or-word 0)
                      (< line-or-word MEM-SIZE))
                 line-or-word
                 (begin
                   (printf "[~a] ERR: invalid address '~a'\n" coord line-or-word)
                   #f)))
            (else
             (printf "[~a] ERR: invalid breakpoint '~a'\n" coord line-or-word)
             #f)))

    (define (break [reason #f])
      (printf "[~a] Breakpoint: ~a \n"
              coord
              (or reason (format "~x(~x)" P (region-index P))))
      (send ga144 break this))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; state display debug functions
    (define/public (display-state)
      (printf "_____Node ~a state_____\n" coord)
      (printf "P:~x(~x) A:~x B:~x R:~x\n" P (region-index P) A B R)
      (when extended-arith?
        (printf "[extended arith]  carry=~a\n" carry-bit))
      (display-dstack)
      (display-rstack))

    (define/public (display-dstack)
      (printf "|d> ~x ~x" T S)
      (display-stack dstack)
      (newline))

    (define (display-rstack)
      (printf "|r> ~x" R)
      (display-stack rstack)
      (newline))

    (define/public (display-memory [n MEM-SIZE])
      (let ((n (sub1 n)))
        (define (print i)
          (let ((v (vector-ref memory i)))
            (printf "~a " v)
            (unless (or (eq? v 'end)
                        (>= i n))
              (print (add1 i)))))
        (printf "node ~a memory: " coord)
        (print 0)
        (newline)))

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
                (string-join (for/list ((node ludr-port-nodes))
                               (if node
                                   (number->string (send node get-coord))
                                   "___"))
                             ", "))

        (printf "suspended: ~a\n" (if suspended "Yes" "No"))

        (when waiting-for-pin
          (printf "Waiting for pin 17\n"))
        ))

    (define/public (describe-io-reg [describe #f])
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
        (if pin17
            (printf "    pin17 HIGH\n")
            (printf "    pin17 LOW\n"))
        (when (> num-gpio-pins 1)
          (if pin1
              (printf "    pin1 HIGH\n")
              (printf "    pin1 LOW\n"))
          (when (> num-gpio-pins 2)
            (if pin3
                (printf "    pin3 HIGH\n")
                (printf "    pin3 LOW\n"))
            (and (> num-gpio-pins 3)
                 (if pin5
                     (printf "    pin3 HIGH\n")
                     (printf "    pin5 LOW\n")))))))

    (define (get-memory-name index)
      (set! index (& index #xff)) ;; get rid of extended arithmetic bit
      (if (and ram-addr->name
               (hash-has-key? ram-addr->name index))
          (hash-ref ram-addr->name index)
          (if (and rom-symbols
                   (hash-has-key? rom-symbols index))
              (hash-ref rom-symbols index)
              #f)))
    (define/public (disassemble-memory [start 0] [end #xff])
      ;;  disassemble and print a node memory from START to END, inclusive
      (define (pad-print thing [pad 20])
        (let* ((s (format "~a" thing))
               (len (string-length s))
               (str (string-append s (make-string (- pad len) #\ ))))
          (printf str)))
      (let ((word #f)
            (name #f))
        (for ((i (range start (add1 end))))
          (set! i (region-index i))
          (when (setq name (get-memory-name i))
            (printf "~a:\n" name))
          (set! word (vector-ref memory i))
          (pad-print i 4)
          (pad-print word)
          (printf "~a\n" (disassemble-word word)))))
    ))
