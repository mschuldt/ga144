#lang racket

(require "stack.rkt"
         "../common.rkt")

(provide f18a%)

(define DEBUG? #f)
(define _PORT-DEBUG? #f)
(define DISPLAY_STATE? #f)
(define port-debug-list '(1 2))
(define (PORT-DEBUG? coord) (and _PORT-DEBUG? (member coord port-debug-list)))

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

    ;;value of each gpio pin. 0 or 1
    (define pin17 #t)
    (define pin5 #t)
    (define pin3 #t)
    (define pin1 #t)

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
      (send ga144 remove-from-active-list this))

    (define (add-to-active-list)
      (send ga144 add-to-active-list this))

    (define (suspend)
      (remove-from-active-list)
      (set! suspended #t))

    (define (wakeup)
      (add-to-active-list)
      (set! suspended #f))
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

    (define (port-read port)
      (when (PORT-DEBUG? coord) (printf "[~a](port-read ~a)\n" coord port))
      ;;read a value from a ludr port
      ;;returns #t if value is one the stack, #f if we are suspended waiting for it
      (let ((writing-node (vector-ref writing-nodes port)))
        (if writing-node
            (begin ;;value was ready
              (when (PORT-DEBUG? coord) (printf "       value was ready: ~a\n"
                                                (vector-ref port-vals port)))
              (d-push! (vector-ref port-vals port))
              ;;clear state from last reading
              (vector-set! writing-nodes port #f)
              ;;let other node know we read the value
              (send writing-node finish-port-write)
              #t)
            (begin ;;else: suspend while we wait for other node to write
              (when (PORT-DEBUG? coord) (printf "       suspending\n"))
              (send (vector-ref ludr-port-nodes port) receive-port-read port this)
              (suspend)
              #f))))

    (define (multiport-read ports)
      (when (PORT-DEBUG? coord) (printf "[~a](multiport-read ~a)\n" coord ports))
      (let ([done #f]
            [writing-node #f]
            [other #f])
        (for ([port ports])
          (when (setq writing-node (vector-ref writing-nodes port))
            (if done
                (printf "Error: multiport-read -- more then one node writing")
                (begin
                  (when (PORT-DEBUG? coord)
                    (printf "       value was ready: ~a\n"
                            (vector-ref port-vals port)))
                  (d-push! (vector-ref port-vals port))
                  (vector-set! writing-nodes port #f)
                  (send writing-node finish-port-write)
                  (set! done #t)))))
        (if done ;;successfully read a value from one of the ports
            #t
            (begin ;;else: suspend while we wait for an other node to write
              (when (PORT-DEBUG? coord) (printf "       suspending\n"))
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
                  (send (vector-ref ludr-port-nodes port)
                        receive-port-read port this)
                  (set! multiport-read-ports (cons port multiport-read-ports))))
              (suspend)
              #f))))

    (define (port-write port value)
      (when (PORT-DEBUG? coord)
        (printf "[~a](port-write ~a  ~a)\n" coord port value))
      ;;writes a value to a ludr port
      (let ((reading-node (vector-ref reading-nodes port)))
        (if reading-node
            (begin
              (when (PORT-DEBUG? coord) (printf "       target is ready\n"))
              (vector-set! reading-nodes port #f)
              (send reading-node finish-port-read value)
              #t)
            (begin
              (when (PORT-DEBUG? coord) (printf "       suspending\n"))
              (send (vector-ref ludr-port-nodes port)
                    receive-port-write port value this)
              (suspend)
              #f))))

    (define (multiport-write ports value)
      ;; "every node that intends to read the value written
      ;;  must already be doing so and suspended"
      (when (PORT-DEBUG? coord)
        (printf "[~a](multiport-write ~a  ~a)\n" coord ports value))
      (let ([reading-node #f])
        (for ([port ports])
          (when (setq reading-node (vector-ref reading-nodes port))
            (when (PORT-DEBUG? coord) (printf "       wrote to port: ~a" port))
            (vector-set! reading-nodes port #f)
            (send reading-node finish-port-read value))))
      #t)

    (define/public (finish-port-read val)
      ;;called by adjacent node when it writes to a port we are reading from
      (when (PORT-DEBUG? coord) (printf "[~a](finish-port-read  ~a)\n" coord val))
      (d-push! val)
      (when multiport-read-ports
        ;;there may be other nodes that still think we are waiting for them to write
        (for ([port multiport-read-ports])
          ;;reuse 'receive-port-read' to cancel the read notification
          (send (vector-ref ludr-port-nodes port) receive-port-read port #f))
        (set! multiport-read-ports #f))
      (wakeup))

    (define/public (finish-port-write)
      ;;called by adjacent node when it reads from a port we are writing to
      (when (PORT-DEBUG? coord) (printf "[~a](finish-port-write)\n" coord))
      (wakeup))

    (define/public (receive-port-read port node)
      ;;called by adjacent node when it is reading from one of our ports
      (when (PORT-DEBUG? coord)
        (printf "[~a](receive-port-read ~a   ~a)\n"
                coord port (and node (send node str))))
      (vector-set! reading-nodes port node))

    (define/public (receive-port-write port value node)
      (when (PORT-DEBUG? coord)
        (printf "[~a](receive-port-write ~a  ~a  ~a)\n"
                coord port value (send node str)))
      ;;called by adjacent noe when it is writing to one of our ports
      (vector-set! writing-nodes port node)
      (vector-set! port-vals port value))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (define prev-IO IO)
    (define num-gpio-pins (let ((pins (assoc coord node-to-gpio-pins)))
                            (if pins (cdr pins) 0)))
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

        (d-push! io))
      #t)

    (define (set-io-reg val)
      (set! prev-IO IO)
      (set! IO val)
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

    ;;Determine if we are in RAM or ROM, return the index for that memory location
    ;;DB001 section 2.2  Top half of RAM and ROM is repeated
    (define (region-index addr)
      (if (>= addr #x80);;ROM
          (if (> addr #x0BF)
              (- addr #x0BF)
              addr)
          (if (> addr #x3F)

              (- addr #x3F)
              addr)))

    (define (port-addr? addr)
      ;;True if ADDR is an IO port or register, else False
      (> (& addr #x100) 0))

    ;;Port address in the 'memory' vector (0x100 - 0x1ff) contain
    ;;vectors of functions to call when reading or writing to that port.
    ;;First element is the read function, second is the write function.
    ;;Invalid port numbers have the value #f

    (define (read-memory addr)
      ;;pushes the value at memory location ADDR onto the data stack
      (if (port-addr? addr)
          (let ((x (vector-ref memory addr)))
            (if (vector? x)
                ((vector-ref x 0))
                (printf "ERROR: read-memory(~a) - invalid port\n" addr)))
          (d-push! (vector-ref memory (region-index addr)))))

    (define (set-memory! addr value)
      (if (port-addr? addr)
          (let ((x (vector-ref memory addr)))
            (if x
                ((vector-ref x 1) value)
                (printf "ERROR: set-memory!(~a, ~a) - invalid port\n" addr value)))
          (vector-set! memory (region-index addr) value)))

    ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; instruction execution

    (define (execute! opcode [jump-addr-pos 0])
      (if (< opcode 8)
          ((vector-ref instructions opcode) (bitwise-bit-field I 0 jump-addr-pos))
          ((vector-ref instructions opcode))))

    (define (step0-helper)
      (set! I (d-pop!))
      (set! I^ (^ I #x15555))
      (set! P (incr P))
      (if (eq? I 'end)
          (suspend)
          (set! step-fn (if (execute! (bitwise-bit-field I^ 13 18) 10)
                            step1
                            step0))))
    (define (step0)
      (if (read-memory P)
          ;;TODO: FIX: this should not use the stack
          ;;           we could loose the last item on the stack this way
          (step0-helper)
          ;;else: we are now suspended waiting for the value
          (set! step-fn step0-helper)))

    (define (step1)
      (set! step-fn (if (execute! (bitwise-bit-field I^ 8 13) 8)
                        step2
                        step0)))

    (define (step2)
      (set! step-fn (if (execute! (bitwise-bit-field I^ 3 8) 3)
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
                            (if DEBUG?
                                (lambda args
                                  (printf (format "[~a]OPCODE: '~a'\n" coord opcode))
                                  body ...)
                                (lambda args
                                  body ...)))
               (set! _n (add1 _n))))

      (define-instruction! ";" (_)
        (set! P R)
        (r-pop!)
        #f)

      (define-instruction! "ex" (_)
        (define temp P)
        (set! P R)
        (set! R temp)
        #f)

      (define-instruction! "jump" (addr)
        (when DEBUG? (printf "jump to ~a\n" addr))
        (set! P addr)
        #f)

      (define-instruction! "call" (addr)
        (when DEBUG? (printf "calling: ~a\n" addr))
        (r-push! P)
        (set! P addr)
        #f)

      (define-instruction! "unext" (_) ;; -- hacky!
        (if (= R 0)
            (r-pop!)
            (begin (set! R (sub1 R))
                   (set! P (sub1 P))
                   #f)))

      (define-instruction! "next" (addr)
        (if (= R 0)
            (begin (r-pop!)
                   #f)
            (begin (set! R (sub1 R))
                   (set! P addr)
                   #f)))

      (define-instruction! "if" (addr)
        (and (= T 0)
             (begin (when DEBUG? (printf "If: jumping to ~a\n" addr))
                    (set! P addr))
             #f))

      (define-instruction! "-if" (addr)
        (and (not (bitwise-bit-set? T 17))
             (set! P addr)
             #f))

      (define-instruction! "@p" ()
        (read-memory P)
        (set! P (incr P)))

      (define-instruction! "@+" () ; fetch-plus
        (read-memory A)
        (set! A (incr A)))

      (define-instruction! "@b" () ;fetch-b
        (read-memory B)
        #t)

      (define-instruction! "@" (); fetch a
        (read-memory A) #t)

      (define-instruction! "!p" () ; store p
        (set-memory! P (d-pop!))
        (set! P (incr P)) #t)

      (define-instruction! "!+" () ;store plus
        (set-memory! A (d-pop!))
        (set! A (incr A)) #t)

      (define-instruction! "!b" (); store-b
        (set-memory! B (d-pop!)) #t)

      (define-instruction! "!" (); store
        (set-memory! A (d-pop!)) #t)

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
                              (set! carry-bit (& sum #x40000))
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
              (set! carry-bit (& sum #x40000))
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
        (set! B (d-pop!)))

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

    (define/public (load code [n #f])
      ;;CODE is a vector of assembled code
      ;;load N words from CODE into memory, default = len(code)
      (set! n (or n (vector-length code)))
      (define (load index)
        (when (< index n)
          (vector-set! memory index (vector-ref code index))
          (load (add1 index))))
      (load 0))

    (define (setup-ports)
      (vector-set! memory &LEFT (vector (lambda () (port-read LEFT))
                                        (lambda (v) (port-write LEFT v))))
      (vector-set! memory &RIGHT (vector (lambda () (port-read RIGHT))
                                         (lambda (v) (port-write RIGHT v))))
      (vector-set! memory &UP (vector (lambda () (port-read UP))
                                      (lambda (v) (port-write UP v))))
      (vector-set! memory &DOWN (vector (lambda () (port-read DOWN))
                                        (lambda (v) (port-write DOWN v))))
      (vector-set! memory &IO (vector (lambda () (read-io-reg))
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
                           )))

    (define/public (reset! [bit 18])
      (set! A 0)
      (set! B 0)
      (set! P 0)
      (set! I 0)
      (set! R 0)
      (set! S 0)
      (set! T 0)
      (set! IO #x15555)
      (set! memory (make-vector MEM-SIZE 65957)) ;;65957 => ludr multiport jump
      (set! dstack (make-stack 8))
      (set! rstack (make-stack 8))
      (set! blocking-read #f)
      (set! blocking-write #f)
      (set! blocking #f)
      (set! multiport-read-ports #f)
      (set! writing-nodes (make-vector 4 #f))
      (set! reading-nodes (make-vector 4 #f))
      (set! port-vals (make-vector 4 #f))
      (set! step-fn step0)
      (set! pin-handlers-set-p #f)
      (setup-ports))

    ;; Resets only p
    (define/public (reset-p! [start 0])
      (set! P start))

    ;; Executes one step of the program by fetching a word, incrementing
    ;; p and executing the word.
    ;; returns #f when P = 0, else #t
    (define/public (step-program!)
      (when DEBUG? (printf "\nstep-program! node ~a\n" coord))
      (step-fn)
      (when (and DEBUG? DISPLAY_STATE?) (send ga144 display-node-states
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
        (vector-set! ludr-port-nodes (convert "north") north)
        (vector-set! ludr-port-nodes (convert "east") east)
        (vector-set! ludr-port-nodes (convert "south") south)
        (vector-set! ludr-port-nodes (convert "west") west)))

    (define/public (get-ludr-port-nodes) ludr-port-nodes)

    (define/public (set-aindex index)
      (set! active-index index))

    (define/public (str)
      (format "<node ~a>" coord))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; state display debug functions
    (define/public (display-state)
      (printf "_____Node ~a state_____\n" coord)
      (printf "p:~a a:~a b:~a r:~a\n" P A B R)
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

    (define (display-memory [n MEM-SIZE])
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

    ))
