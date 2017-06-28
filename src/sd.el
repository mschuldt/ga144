(require 'cl)

(defstruct sd buffer length data-length data overlays offset display-functions)
;;            1      2      3           4    5         6     7
;; because: Symbol’s function definition is void: \(setf\ sd-data-len\)
(defun set-sd-data-len! (sd x)
  (aset sd 3 x))
(defun set-sd-data! (sd x)
  (aset sd 4 x))
(defun set-sd-offset! (sd x)
  (aset sd 6 x))

(setq sd-display-list nil)

(defun sd-create (data line column length width)
  "creates a scroll display in the current buffer
DATA - array of strings to display
LINE, COLUMN - location of top left corner of display
LENGTH - length of display in lines
WIDTH - width of display in characters

"
  (let* ((data-len (length data))
         (overlays (make-vector length nil))
         buf-lines col sd)

    (save-excursion
      (setq buf-lines (count-lines (point-min) (point-max)))
      (when (< buf-lines line)
        (end-of-buffer)
        (insert (make-string (- line buf-lines) ?\n)))
      (goto-char 0)
      (forward-line (1- line))

      (dotimes (i length)
        ;;TODO: ok to have zero length overlay?
        (end-of-line)
        (setq col (current-column))
        (when (< col column) ;;expand line if needed
          (insert (make-string (- column col) ? )))

        (beginning-of-line)
        (forward-char column)

        (aset overlays i (make-overlay (point) (point)))
        (end-of-line)
        (if (eobp)
            (insert "\n")
          (forward-line))))

    (setq sd (make-sd :buffer (current-buffer)
                      :length length
                      :data-length data-len
                      :data data
                      :overlays overlays
                      :display-functions (make-vector data-len nil)
                      :offset 0))
    (push sd sd-display-list)
    (sd-update sd)
    sd))

(defun sd-update (sd)
  "update display overlays"
  (let* ((data (sd-data sd))
         (offset (sd-offset sd))
         (overlays (sd-overlays sd))
         (fn-array (sd-display-functions sd))
         fn row)
    (dotimes (i (sd-length sd))
      (setq fn (aref fn-array i)
            row (aref data (+ i offset)))
      (when fn
        (setq row (funcall fn i row)))
      (overlay-put (aref overlays i) 'after-string row))))

(defun sd-set-data (sd data)
  "set the display DATA array"
  (assert data)
  (set-sd-data! sd data)
  (let ((data-len (length data))
        (offset (sd-offset sd)))
    (set-sd-data-len! sd data-len)
    (set-sd-offset! sd (max (min offset (- data-len (sd-length sd))) 0)))
  (sd-update sd))

(defun sd-move-to_ (sd curr new)
  (setq new (max 0 new)
        new (min new (- (sd-data-length sd)
                        (sd-length sd))))

  (when (not (= new curr))
    (set-sd-offset! sd new)
    (sd-update sd)))

(defun sd-move-to (sd offset)
  (sd-move-to_ sd (sd-offset sd) offset)
  (sd-update sd))

(defun sd-center-on (sd n)
  (sd-move-to_ sd
               (sd-offset sd)
               (- n (/ (sd-length sd) 2))))

(defun sd-move-up (sd &optional n)
  "scrolls the data in the display window up"
  (let* ((curr (sd-offset sd))
         (new (+ curr (or n 1))))
    (sd-move-to_ sd curr new)))

(defun sd-move-down (sd &optional n)
  "scrolls the data in the display window down"
  (sd-move-up sd (- (or n 1))))

(defun sd-remove (sd)
  "remove a scroll display from its buffer"
  (mapc (lambda (o)
          (delete-overlay o))
        (sd-overlays sd))
  (setq sd-display-list (remove sd sd-display-list)))

(defun sd-set-display-function (sd line function)
  ;;FUNCTION accepts two args: 'line' and 'data'.
  ;; 'index' is the line of the display being updated
  ;; 'data' is the data that is to be displayed on that line
  ;; the function must return a string, which would replace 'data' on the display
  ;; by default function is the identity function
  (let ((fn-array (sd-display-functions sd)))
    (assert (and (>= line 0) (<= line (sd-length sd))))
    (aset fn-array line function)))

(defun sd-delete-all ()
  "global cleanup off all displays"
  (mapc 'sd-remove sd-display-list)
  (setq sd-display-list nil))

(defun sd-realign ()
  "redraw the sd if the overlays have been moved horizontally"
  (error "sd-realign unimplemented"))

(provide 'sd)
