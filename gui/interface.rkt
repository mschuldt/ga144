#lang racket/gui

(define key-map (make-hash))

(define (set-key char func)
  (hash-set! key-map char func))

(define (has-mapping? char)
  (hash-has-key? key-map char))

(define (get-key-func key)
  (let ((k (chars->strings key)))
    (and (has-mapping? k)
         (hash-ref key-map k))))

(define-syntax-rule (setq sym val)
  (begin (set! sym val) sym))

(define Frame%
  (class frame%
    (super-new)
    (define/override (on-subwindow-char r e)
      (let ((fn (get-key-func (send e get-key-code))))
        (and fn (fn))))))

(define frame (new Frame%
                   [label "GA144"]
                   ;;[style '(no-caption no-resize-border
                   ;;                    hide-menu-bar no-system-menu)]
                   [width 500]
                   [height 400]))

(set-key 'escape (lambda () (send frame show #f)))

(define node-canvas%
  (class canvas%
    (super-new)
    (inherit get-dc)

    (define paint-cb #f)
    (define data #f)
    (define/public (set-paint-cb cb)
      (set! paint-cb cb))
    (define/override (on-paint)
      (paint-cb this (get-dc)))
    ))

(define node%
  (class object%
    (super-new)
    (init-field [data #f])
    (define (paint-fn canvas dc)
      (send dc set-scale 1 1)
      (send dc set-text-foreground "blue")
      (send dc draw-text (format "(~a, ~a)" (car data) (cdr data)) 0 0))
    (define/public (set-data d)
      (set! data d))
    (define/public (get-paint-fn)
      paint-fn)
    ))

(define display-nodes-height 4)
(define display-nodes-width 8)
(define n-display-nodes (* display-nodes-height display-nodes-width))
(define node-canvases (make-vector n-display-nodes))
(define nodes (make-vector 144))

(define (canvas:xy->index x y)
  (+ (* display-nodes-width y) x))

(define (node:xy->index x y)
  (+ (* 18 y) x))

(define (nodes:coord->index x y)
  0;;TODO
  )

(define node-rows-panel (new vertical-panel% [parent frame]))

(define (build-node-canvas-matrix)
  (let ((row #f))
    (for ([y display-nodes-height])
      (set! row (new horizontal-panel% [parent node-rows-panel]))
      (for ([x display-nodes-width])
        (vector-set! node-canvases
                     (canvas:xy->index x y)
                     (new node-canvas% [parent row] [style (list 'border)]))))))

(define (create-nodes)
  (for ([x 18])
    (for ([y 8])
      (vector-set! nodes
                   (node:xy->index x y)
                   (new node% [data (cons x y)])))))

(define view-rect-offset-x 0)
(define view-rect-offset-y 0)

(define (valid-view-rect-offset x y)
  (and (>= x 0)
       (>= y 0)
       (<= display-nodes-width (- 18 x))
       (<= display-nodes-height (- 8 y))))

(define (set-view-rect-offset x y)
  (if (valid-view-rect-offset x y)
      (begin
        (set! view-rect-offset-x x)
        (set! view-rect-offset-y y)
        (update-display-functions)
        (send frame refresh)
        (printf "offset x = ~a\n" view-rect-offset-x)
        (printf "offset y = ~a\n" view-rect-offset-y)
        )
      (printf "invalid offset\n"))
  )

(define (move-left)
  (set-view-rect-offset (sub1 view-rect-offset-x) view-rect-offset-y))
(define (move-up)
  (set-view-rect-offset view-rect-offset-x (sub1 view-rect-offset-y)))
(define (move-down)
  (set-view-rect-offset view-rect-offset-x (add1 view-rect-offset-y)))
(define (move-right)
  (set-view-rect-offset (add1 view-rect-offset-x) view-rect-offset-y))

(set-key "u" move-left)
(set-key "i" move-up)
(set-key "o" move-down)
(set-key "p" move-right)

(define (update-display-functions)
  (for ([x display-nodes-width])
    (for ([y display-nodes-height])
      (send (vector-ref node-canvases (canvas:xy->index x y))
            set-paint-cb
            (send (vector-ref nodes
                              (node:xy->index (+ x view-rect-offset-x)
                                              (+ y view-rect-offset-y)))
                  get-paint-fn)))))

(define (char->string char)
  (format "~a" char))
(define (chars->strings c)
  (if (char? c)
      (char->string c)
      c))


(build-node-canvas-matrix)
(create-nodes)

(update-display-functions)
;; (move-down)
;; (move-right)(move-right)
;; (move-up)
;; (move-left)(move-left)

(send frame show #t)
