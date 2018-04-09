;; -*- lexical-binding: t -*-
(require 'cl)
(require 'gv)
(load "sd" nil t)

(defmacro def-local (name &optional default docs)
  `(progn (defvar ,name ,default, docs)
          (make-variable-buffer-local ',name)
          (put ',name 'permanent-local t)))

(setq ga-default-node-size 3)
(def-local ga-project-name nil)
(def-local ga-project-file nil)
(def-local ga-nodes nil)
(def-local ga-current-coord nil)
(def-local ga-prev-coord nil)
(def-local ga-node-size nil)
(def-local ga-project-aforth-file nil) ;; top level aforth project file
(def-local ga-project-aforth-buffer nil)
;; list of source files for searching. Currently thi sis
(def-local ga-project-aforth-files nil)
(def-local ga-project-aforth-buffers nil)
(def-local ga-project-aforth-file-overlay nil)
(def-local ga-has-unsaved-changes nil)
(def-local ga-project-aforth-compile-status nil)
(def-local ga-project-aforth-compile-status-overlay nil)
(def-local ga-mark-active nil)
(def-local ga-mark-coord nil)
(def-local ga-region-nodes nil) ;; ordered collection of all node coordinates in the region
(def-local ga-region-path-p  nil) ;; t if ga-region-path-p represents a node path
(def-local ga-visited-nodes nil) ;; hash table of visited nodes
(def-local ga-color-select-buffer nil)
(def-local ga-color-select-buffer-p nil)
(def-local ga-color-select-map-buffer nil)
(def-local ga-color-select-coord nil)
(def-local ga-parse-data nil)
(def-local ga-compilation-data nil)
(def-local ga-compiled-nodes nil) ;; hash mapping nodes to compiled node memory array
(def-local ga-compilation-data-changed nil) ;; set true after compilation data is updated
(def-local ga-assembly-data nil)
(def-local ga-node-usage-list nil)
(def-local ga-node-usage-hash nil)
(def-local ga-node-locations nil)
(def-local ga-ram-display nil) ;; displays instructions (follows P)
(def-local ga-ram-data-display nil) ;; displays data (follows A or B )
(def-local ga-sim-ga144 nil) ;; ga44 simulation - object type ga144%
(def-local ga-sim-current-node nil) ;;The currently selected f18a node
(def-local ga-sim-p nil) ;; true if simulation is active
(def-local ga-sim-node-P 0)
(def-local ga-sim-node-inst-i 0)
(def-local ga-sim-node-word-i 0)
(def-local ga-buffer-valid-p nil) ;;true if map is in valid state. Used for cleanup
(def-local ga-data-display nil) ;; data stack
(def-local ga-return-display nil) ;;return stack
(def-local ga-register-display nil)
(def-local ga-sim-step-increment 1) ;; instructions to execute with each step
(def-local ga-update-node-colors-fn 'ga-update-usage-colors)

(setq ga-empty-node-ram-display-data (make-vector 64 "~ ~ ~ ~"))
;; maps nodes to their current position in their ram display window
(def-local ga-node-ram-display-position nil)
(def-local ram-display-moved nil) ;;true when the ram display of the current node has been moved

(defvar ga-auto-resize-map-on-window-change t)

(setq ga-persistent-variables '(ga-nodes-sans-overlays ga-node-size ga-current-coord ga-project-aforth-file))


(defface ga-node-coord-face '((((background light)) (:foreground "black")) ;;gold1
                              (((background dark)) (:foreground "black"))
                              (t (:bold t)))
  "ga face for node coordinate numbers")

(defface ga-default-face-1 '((((background light)) (:background "LightSkyBlue1"))
                             (((background dark)) (:background "LightSkyBlue1")))
  "default ga node face 1")

(defface ga-default-face-2 '((((background light)) (:background "LightSkyBlue2"))
                             (((background dark)) (:background "LightSkyBlue2")))
  "default ga node face 2")

(defface ga-select-face '((((background light)) (:background "SeaGreen3"))
                          (((background dark)) (:background "SeaGreen3")))
  "default ga selected node face")

(defface ga-unfocused-face '((((background light)) (:background "LightSkyBlue4"))
                             (((background dark)) (:background "LightSkyBlue4")))
  "default ga face for the selected, unfocused node")

(defface ga-region-face-1 '((((background light)) (:background "gold1"))
                            (((background dark)) (:background "gold1")))
  "default ga node region face 2")

(defface ga-region-face-2 '((((background light)) (:background "gold2"))
                            (((background dark)) (:background "gold2")))
  "default ga node region face 2")

(setq ga-node-coord-face 'ga-node-coord-face)
(setq ga-default-face-1 'ga-default-face-1)
(setq ga-default-face-2 'ga-default-face-2)
(setq ga-select-face 'ga-select-face)
(setq ga-unfocused-face 'ga-unfocused-face)
(setq ga-region-face-1 'ga-region-face-1)
(setq ga-region-face-2 'ga-region-face-2)

(defun ga-get-project-file-buffer (filepath)
  (let ((buff (find-buffer-visiting filepath)))
    (unless buff
      (find-file filepath)
      (setq buff (current-buffer))
      (bury-buffer))
    buff))

(defun ga-aforth-files (dir)
  (let ((ok '())
        (files (directory-files dir)))
    (dolist (file files)
      (when (string-match "\\.aforth$" file)
        (push file ok)))
    ok))

(defun ga-render( node-size )
  (setq ga-buffer-valid-p nil)
  (ga-draw-map node-size)
  (goto-char 1)
  (update-position)
  (setq ga-buffer-valid-p t))

(defun ga-move-to-node (coord &optional middle node-size)
  (goto-char 1)
  (let ((row (- 7 (ga-coord->row coord)))
        (col (ga-coord->col coord))
        (node-size (or node-size ga-node-size)))
    (forward-line (+ (* row  node-size) (if middle (/ node-size 2) 0)))
    (forward-char (+ (* col node-size) (if middle (floor (/ node-size 2)) 0)))))

(defun ga-set-compilation-status (status)
  (setq ga-project-aforth-compile-status status)
  (overlay-put ga-project-aforth-compile-status-overlay 'after-string status))

(setq ga-current-node-display-format "node %s  (%s/64, %s%%)")

(setq ga-display-hex t)
(defun ga-toggle-hex ()
  (interactive)
  (setq ga-display-hex (not ga-display-hex))
  (ga-sim-update-display))

(defun ga-format-num (n &optional 2comp)
  (let ((fmt (if ga-display-hex "%s%x" "%s%d"))
        (num (if (and 2comp (> n #x20000)) (& (+ (~ n) 1) #x1ffff) n))
        (sgn (if (and 2comp (> (& n #x20000) 0)) "-" "")))
    (format fmt sgn num)))

(defun ga-current-node-display-fn (_ _)
  (let ((s (format "node %s" ga-current-coord))
        (n (if ga-node-usage-hash
               (gethash ga-current-coord ga-node-usage-hash)
             0)))
    (put-text-property 0 (length s) 'font-lock-face '(:foreground "yellow") s)
    (if n
        (format "%s  %s words, %s%%" s n (/ (* n 100) 64))
      s)))

(defun ga-set-source-buffer-overlay (&optional name)
  (overlay-put ga-project-aforth-file-overlay 'after-string
               (or (or name
                       (and ga-project-aforth-buffer
                            (with-current-buffer ga-project-aforth-buffer
                              (buffer-name))))
                   "None")))

(defun ga-draw-map (node-size)
  (read-only-mode -1)
  (erase-buffer)
  (goto-char 1)
  (let ((map-height (* node-size 8))
        (map-width (* node-size 18))
        x coord l o n)
    ;; insert map chars
    (dotimes (_ map-height)
      (insert (make-string map-width ? ) "\n" ))

    ;; set map overlays
    (loop-nodes node
      (setq coord (ga-node-coord node))
      (ga-move-to-node coord nil node-size)
      (setq s (number-to-string coord)
            l (length s))
      (delete-char l)
      (insert s)
      (setq o (ga-node-coord-overlay node))
      (move-overlay o (- (point) l) (point))
      (set-ga-node-coord-overlay! node o))
    ;; set aforth file overlay string
    (ga-set-source-buffer-overlay)
    (ga-create-overlays node-size)
    ;;;; set compile status overlay string
    ;;(ga-set-compilation-status ga-project-aforth-compile-status)

    (when ga-ram-display
      (sd-remove ga-ram-display))
    (when ga-ram-data-display
      (sd-remove ga-ram-data-display))
    (when ga-data-display
      (sd-remove ga-data-display))
    (when ga-return-display
      (sd-remove ga-return-display))
    (when ga-register-display
      (sd-remove ga-register-display))

    (setq ga-ram-display (sd-create (if ga-sim-p
                                        (make-vector 769 "~ ~ ~ ~")
                                      ga-empty-node-ram-display-data)
                                    1 (+ map-width 3) ;; line column position
                                    map-height ;; display length
                                    27)) ;; display width
    ;; ram displays
    ;; in simulation mode show two displays
    ;;    (let ((ram-display-x-pos (+ map-width 3))
    ;;          (ram-data-display-width 10))
    ;;
    ;;      (when ga-sim-p
    ;;        (setq ga-ram-data-display
    ;;              (sd-create (make-vector 769 "~ ~ ~ ~")
    ;;                         1 ram-display-x-pos  ;; line column position
    ;;                         map-height
    ;;                         ram-data-display-width)) ;; display width
    ;;
    ;;        (setq ram-display-x-pos (+ ram-display-x-pos ram-data-display-width  1)))
    ;;
    ;;      (setq ga-ram-display (sd-create (if ga-sim-p
    ;;                                          (make-vector 769 "~ ~ ~ ~")
    ;;                                        ga-empty-node-ram-display-data)
    ;;                                      1 ram-display-x-pos ;; line column position
    ;;                                      map-height ;; display length
    ;;                                      27)) ;; display width
    ;;      )
    (sd-set-display-function ga-ram-display 0 'ga-current-node-display-fn)

    (goto-char (point-max))
    (when ga-sim-p
      (insert "\n")
      (dotimes (i 10)
        (insert (format "%d\n" i)))
      (setq ga-data-display (sd-create (make-vector 10 "  ~")
                                       (+ map-height 2) 3 ;; line column position
                                       10 ;; display length
                                       5)) ;; display width
      (setq ga-return-display (sd-create (make-vector 9 "  ~")
                                         (+ map-height 2) 10 ;; line column position
                                         9 ;; display length
                                         5)) ;; display width
      (setq ga-register-display (sd-create (make-vector 5 "  ~")
                                           (+ map-height 2) 16 ;; line column position
                                           5 ;; display length
                                           14)))  ;; display width

    ;; aforth file chars and overlay
    (let ((s "Source: ") p)
      (setq n (- (* node-size 8) (1+ (length s)))
            n (> n 0) n 0)
      (insert "\n" (make-string n ? ))
      (beginning-of-line)
      (setq p (point))
      (insert s)
      (move-overlay ga-project-aforth-file-overlay p (point)))
    ;;compile status overlay
    (let ((s "Compilation: ")
	  p)
      (setq n (- (* node-size 8) (1+ (length s)))
            n (> n 0) n 0)
      (insert "\n" (make-string n ? ))
      (beginning-of-line)
      (setq p (point))
      (insert s)
      (move-overlay ga-project-aforth-compile-status-overlay p (point)))
    (insert "\n")
    )
  (ga-update-overlay-faces)
  (set-buffer-modified-p t)
  (read-only-mode 1))

(defun ga-delete-overlays ()
  (let (o overlays coord face column)
    (loop-nodes node
      (dolist (o (ga-node-overlays node))
        (delete-overlay o))
      (set-ga-node-overlays! node nil))))

(defun ga-create-overlays (node-size)
  (ga-delete-overlays)
  (loop-nodes node
    (setq coord (ga-node-coord node)
          overlays nil)
    (ga-move-to-node coord nil node-size)
    (setq column (current-column)
          face (ga-get-node-face node))
    (dotimes (i node-size)
      (setq o (make-overlay (point) (+ (point) node-size)))
      (overlay-put o 'face face)
      (push o overlays)
      (when (< i (- node-size 1))
        (forward-line)
        (beginning-of-line)
        (forward-char column)))
    (set-ga-node-overlays! node overlays)))

(defmacro loop-nodes (var &rest body)
  (declare (indent 1) (debug (symbolp body)))
  (assert (symbolp var))
  `(mapc (lambda (,var)
           ,@body)
         ga-nodes))

(defun ga-get-node-face (node)
  ;; get the current face to display
  (let ((faces (ga-node-faces node)))
    (assert (and (arrayp faces) (= (length faces) ga-num-faces)))
    (if (and (aref faces 5)
             (aref faces 2))
        (list :background (ga-get-scaled-color "#%s%sff" (ga-node-usage node)))
      (or (aref faces 4) ;; tmp high
          (aref faces 3) ;; point
          (aref faces 2) ;; tmp low  ( region selection )
          (aref faces 5) ;; node usage
          (aref faces 1) ;; base
          (aref faces 0) ;; default
          ))))

(defun ga-update-node-overlays (node)
  (let ((face (ga-get-node-face node)))
    (dolist (o (ga-node-overlays node))
      (overlay-put o 'face face))))

(defun ga-update-overlay-faces ()
  (loop-nodes node
    (ga-update-node-overlays node)))

(defun ga-set-node-face-internal (coord idx face &optional node)
  (let* ((node (or node (ga-coord->node coord)))
         (faces (ga-node-faces node)))
    ;;(assert (and (arrayp faces) (= (length faces) ga-num-faces)))
    (aset faces idx face)
    (set-ga-node-faces! node faces)
    (ga-update-node-overlays node)
    ))

(defun ga-reset-temp-faces ()
  (let (faces)
    (loop-nodes node
      (setq faces (ga-node-faces node))
      (aset faces 2 nil) ;; temp low
      (aset faces 3 nil) ;; point
      (aset faces 4 nil) ;; temp high
      (set-ga-node-faces! node faces))))

(defun ga-set-node-default-face (coord face)
  (ga-set-node-face-internal coord 0 face))

(defun ga-set-node-base-face (coord face)
  (ga-set-node-face-internal coord 1 face))

(defun ga-set-node-tmp-low-face (coord face)
  (ga-set-node-face-internal coord 2 face))

(defun ga-set-node-point-face (coord face)
  (ga-set-node-face-internal coord 3 face))

(defun ga-set-node-tmp-high-face (coord face)
  (ga-set-node-face-internal coord 4 face))

(defun ga-set-node-usage-face (coord face)
  (ga-set-node-face-internal coord 5 face))

(defun ga-set-region-face (coord &optional remove)
  (let ((node (ga-coord->node coord)))
    (ga-set-node-face-internal coord 2 (if remove nil (ga-node-region-face node)))))

(defstruct ga-node coord special-function node-type text color overlays region-face faces coord-overlay usage)
;;                 1     2                3         4    5     6        7           8     9             10
;; work around for errors like:
;;   Symbol’s function definition is void: \(setf\ ga-node-coord-overlay\)
;; internet says (require 'cl) should have fixed this but it doesn't
(defun set-ga-node-overlays! (node x)
  (aset node 6 x))
(defun set-ga-node-faces! (node x)
  (aset node 8 x))
(defun set-ga-node-coord-overlay! (node x)
  (aset node 9 x))
(defun set-ga-node-usage! (node x)
  (aset node 10 x))

(defun ga-valid-node-index-p(n)
  (and (>= n 0) (< n 144)))

(defun ga-coord->index (n)
  (assert (ga-valid-coord-p n))
  (+ (* (floor (/ n 100)) 18) (mod n 100)))

(defun ga-index->coord (n)
  (assert (ga-valid-node-index-p n))
  (+ (* (floor (/ n 18)) 100) (mod n 18)))

(defun ga-coord->row (coord)
  (assert (ga-valid-coord-p coord))
  (floor (/ coord 100)))

(defun ga-coord->col (coord)
  (assert (ga-valid-coord-p coord))
  (mod coord 100))

(defun ga-coord->node (coord)
  (assert (ga-valid-coord-p coord))
  (aref ga-nodes (ga-coord->index coord)))

(defun ga-get-node-type (coord)
  )

(defun ga-get-node-default-faces (coord)
  (let ((a (= (mod (/ coord 100) 2) 0))
        (b (= (mod (mod coord 100) 2) 0)))
    (if (eq a b)
        (cons ga-default-face-1 ga-region-face-1)
      (cons ga-default-face-2 ga-region-face-2))))

(setq ga-num-faces 6)

(defun ga-make-face-vector (default-face)
  (let ((v (make-vector ga-num-faces nil)))
    (aset v 0 default-face)
    v))

(defun ga-create-new ()
  (let (faces coord coord-overlay default region-face)
    (setq ga-nodes (make-vector 144 nil))
    (dotimes (i 144)
      (setq coord (ga-index->coord i))
      (setq coord-overlay (make-overlay 0 0 ))
      (setq default-faces (ga-get-node-default-faces coord))
      (overlay-put coord-overlay 'face ga-node-coord-face)
      (aset ga-nodes i (make-ga-node :coord coord
                                     :special-function (ga-get-node-type coord)
                                     :faces (ga-make-face-vector (car default-faces))
                                     :region-face (cdr default-faces)
                                     :coord-overlay coord-overlay)))
    (setq ga-current-coord 0)
    (setq ram-display-moved nil)
    (unless ga-map-view-mode
      (ga-save))
    ))

(defun ga-startup-reset ()
  ;; The current position and buffer selection faces get saved with everything else
  ;; everything could be easily restored but then everything would need to be saved
  ;; everytime there was movement to remain consistent (or the map would have to
  ;; be marked modified at every movement)
  ;; Instead load everything and reset the state the user does not expect to be saved.
  (setq ga-current-coord 0
        ram-display-moved nil)
  (ga-reset-temp-faces))

(defun ga-save ()
  (interactive)
  (unless ga-map-view-mode
    (let ((ga-nodes-sans-overlays (vconcat (mapcar 'copy-sequence ga-nodes)))
          node)
      (dotimes (i 144)
        (setq node (aref ga-nodes-sans-overlays i))
        (set-ga-node-overlays! node nil)
        (set-ga-node-coord-overlay! node nil))

      (let ((print-level nil)
            (print-length nil)
            (values (mapcar (lambda (x) (cons x (eval x))) ga-persistent-variables))) ;;the values are buffer-local
        (with-temp-file ga-project-file
          (dolist (v values)
            (insert "(setq " (symbol-name (car v)))
            (print (cdr v) (current-buffer))
            (insert ")\n\n")))))
    (message "saved in %s" ga-project-file)
    (set-buffer-modified-p nil)))

(defun ga-inc-node-size ()
  (interactive)
  (if (< (* (1+ ga-node-size) 18)  (window-max-chars-per-line))
      (progn (setq ga-node-size (1+ ga-node-size))
             (ga-render ga-node-size)
             (ga-sim-update-display))
    (message "Map cannot be made larger")))

(defun ga-dec-node-size ()
  (interactive)
  (if (> ga-node-size 3)
      (progn
        (setq ga-node-size (1- ga-node-size))
        (ga-render ga-node-size)
        (ga-sim-update-display))
    (message "Map is cannot be made smaller")))

(defun ga-move-left ()
  (interactive)
  (ga-move-selected-node -1))

(defun ga-move-right ()
  (interactive)
  (ga-move-selected-node 1))

(defun ga-move-up ()
  (interactive)
  (ga-move-selected-node 100))

(defun ga-move-down ()
  (interactive)
  (ga-move-selected-node -100))

(defun ga-move-right-end ()
  (interactive)
  (ga-move-selected-node (- 17 (mod ga-current-coord 100))))

(defun ga-move-left-end ()
  (interactive)
  (ga-move-selected-node (- (mod ga-current-coord 100))))

(defun ga-move-left-half ()
  (interactive)
  (ga-move-selected-node (1- (/ (- (mod ga-current-coord 100)) 2))))

(defun ga-move-right-half ()
  (interactive)
  (ga-move-selected-node (/ (- 17 (1- (mod ga-current-coord 100))) 2)))

(defun ga-move-top-half ()
  (interactive)
  (ga-move-selected-node (* (/ (- 7 (1- (/ ga-current-coord 100))) 2) 100)))

(defun ga-move-bottom-half ()
  (interactive)
  (ga-move-selected-node (- (* (1+ (/ (/ ga-current-coord 100) 2)) 100))))

(defun ga-move-top ()
  (interactive)
  (ga-move-selected-node (* (- 7 (/ ga-current-coord 100)) 100)))

(defun ga-move-bottom ()
  (interactive)
  (ga-move-selected-node (- (* (/ ga-current-coord 100) 100))))

(defun ga-valid-coord-p (coord)
  (and (>= coord 0)
       (< (mod coord 100) 18)
       (< (/ coord 100) 8)))

(defun ga-set-selected-node (coord)
  (assert (ga-valid-coord-p coord))
  ;; prev-coord is valid only when updating the display after
  ;; moving the ga-current-coord
  (setq ga-prev-coord ga-current-coord
        ga-current-coord coord)
  (when ga-sim-p
    (ga-sim-set-current-node coord))
  (update-position)
  ;; ram-display-moved is set to nil after all the update functions run
  ;; so that they can tell that the display for ga-prev-coord moved
  (setq ram-display-moved nil
        ga-prev-coord nil))

(defun ga-move-selected-node (n)
  (let ((next (+ ga-current-coord n)))
    (when (ga-valid-coord-p next)
      (ga-set-selected-node next))))

(defun move-selected-node-overlay (from to)
  (ga-set-node-point-face from nil)
  (ga-set-node-point-face to ga-select-face))

(defun ga-goto-current-node ()
  (interactive)
  (ga-goto-node ga-current-coord))

(defun ga-return-key-fn ()
  (interactive)
  (if (> (length ga-region-nodes) 1)
      (message "region nodes : %s" ga-region-nodes)
    (ga-goto-current-node)))

(defun ga-goto-node (node)
  (if (ga-valid-coord-p node)
      (let ((buffers ga-project-aforth-buffers)
            buff point found-buff)
        (while buffers
          (setq buff (car buffers)
                buffers (cdr buffers))
          (with-current-buffer buff
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward (format "node[\t\n ]+%s" node) nil :noerror)
                (setq point (point)
                      found-buff buff
                      buffers nil)))))
        (if found-buff
            (progn
              (switch-to-buffer-other-window found-buff)
              (goto-char point))

          (message "Node %s not found." node)))
    (message "Error: invalid node: %s" node)))

(defun ga-goto-node (node)
  "Jump to node location in source if it exists, if not jump to end"
  (if (ga-valid-coord-p node)
      (let ((point (cdr (assoc node ga-node-locations))))
        (switch-to-buffer-other-window ga-project-aforth-buffer)
        (if point
            (goto-char point)
          (goto-char (point-max))
          (insert (format "\nnode %s\n" node))))
    (message "Error: invalid node: %s" node)))

(defun ga-goto-source-buffer ()
  "switch to the aforth source buffer"
  (interactive)
  (if ga-project-aforth-buffer
      (switch-to-buffer-other-window ga-project-aforth-buffer)
    (message "aforth source buffer not set")))

(defun ga-alist->hash (alist)
  (let ((ht (make-hash-table :test 'eq)))
    (dolist (x alist)
      (puthash (car x) (cdr x) ht))
    ht))

(defun clear-compilation-data ()
  (setq ga-compilation-data nil
        ga-assembly-data nil
        ga-node-usage-list nil
        ga-node-usage-hash nil
        ga-node-locations nil))

(defun ga-set-compilation-data (data)
  (setq ga-error-data (compiled-error-info data))
  (if ga-error-data
      (progn (ga-set-compilation-status (format "FAIL: %s" (error-data-message ga-error-data)))
             (message "Compilation fail: %s" (error-data-message ga-error-data))
             (clear-compilation-data)
             )
    (progn
      (setq ga-compilation-data data)
      (setq ga-compiled-nodes (let ((ht (make-hash-table)))
                                (dolist (node (compiled-nodes data))
                                  (puthash (node-coord node)
					   (vector-copy (node-mem node))ht))
                                ht))
      (setq ga-assembly-data (assemble data))
      (setq ga-node-usage-list (ga-calculate-node-usage ga-assembly-data)) ;;TODO: remove
      (setq ga-node-usage-hash (ga-alist->hash ga-node-usage-list)) ;;TODO: remove
      (setq ga-node-locations (compiled-node-locations data))
      (funcall ga-update-node-colors-fn)
      (ga-update-node-usage ga-assembly-data)
      (when ga-ram-display
        (ga-update-ram-display-node)
        ;;(sd-set-data ga-ram-display (ga-create-ram-display-data ga-current-coord)))
        )
      (ga-update-stack-displays)
      (ga-set-compilation-status "Ok"))))

(defun ga-update-compilation-data (&optional compilation-data)
  (let ((old-ga-assembly-data ga-assembly-data))
    (if compilation-data
        (ga-set-compilation-data compilation-data)
      (when ga-project-aforth-file
        (unless ga-project-aforth-buffer
          (setq ga-get-project-file-buffer ga-project-aforth-file))
        (if ga-project-aforth-buffer
            (ga-set-compilation-data (aforth-compile-buffer ga-project-aforth-buffer))
          (error "unable to retrieve project aforth buffer")))
      ;;TODO: maybe assemble, bootstream
      )
    (when ga-sim-p
      (if (not (eq ga-assembly-data old-ga-assembly-data))
          (ga-sim-load ga-assembly-data)
        (message "Error: failed to update compilation. ga14 sim not updated"))))
  (setq ga-compilation-data-changed t)) ;;TODO: what is this used for?

(defun ga-update-node-usage (assembled)
  (dolist (node (compiled-nodes assembled))
    (set-ga-node-usage! (ga-coord->node (node-coord node))
                        (length (filter 'identity (vector->list (node-mem node)))))))

(defun ga-calculate-node-usage (assembled)
  (let ((nodes (compiled-nodes assembled))
        usage)
    (dolist (node nodes)
      (push (cons (node-coord node)
                  (length (filter 'identity (vector->list (node-mem node)))))
            usage))
    usage))

(defun to-hex-str (n)
  (format "%s%x" (if (< n 16) "0" "") n))

(setq ga-node-overflow-color "#9b30ff")

(defun ga-set-usage-color (node color)
  (when color
    (setq color (list :background color)))
  (ga-set-node-usage-face node color))

(defun ga-get-scaled-color (fmt n)
  ;; N is 0 to 64
  (setq n (+ (floor (* (/ (or n 0) 64.0) 240)) 15))
  (format fmt
          (to-hex-str (- 255 n))
          (to-hex-str (- 255 n))))

(defun ga-update-usage-colors ()
  ;; USAGE format: ((code . word-count)...)
  (loop-nodes node
    ;;reset node colors
    (ga-set-usage-color (ga-node-coord node) nil))
  (dolist (node ga-node-usage-list)
    (if (> (cdr node) 64)
        (ga-set-usage-color (car node) ga-node-overflow-color)

      (ga-set-usage-color (car node)
                          (ga-get-scaled-color "#ff%s%s" (cdr node))))))

(defun ga-update-activity-colors ()
  (send ga-sim-ga144 show-activity t)
  )

(defun ga-switch-node-color-method (method)
  ;; cleanup current method
  (cond ((eq ga-update-node-colors-fn 'ga-update-activity-colors)
         (send ga-sim-ga144 show-activity nil))
        )
  ;; set new method
  (cond ((eq method 'usage)
         (setq ga-update-node-colors-fn 'ga-update-usage-colors))
        ((eq method 'activity)
         (setq ga-update-node-colors-fn 'ga-update-activity-colors))
        (t (error "unknown color update type: %s" method)))

  (funcall ga-update-node-colors-fn))

(defun ga-display-node-usage ()
  (interactive)
  (ga-switch-node-color-method 'usage)
  (message "Node Usage"))

(defun ga-display-node-activity ()
  (interactive)
  (ga-switch-node-color-method 'activity)
  (message "Node Activity"))

(defun ga-format-str (str color)
  (when color
    (put-text-property 0 (length str) 'font-lock-face (list :foreground color) str))
  str)

(defun ga-format-inst (inst)
  (cond ((stringp inst)
         (ga-format-str inst "green"))
        ((number? inst)
         (ga-format-str (ga-format-num inst) "red"))
        ((null inst) "")))

(defun ga-format-word (i word)
  (let* ((a " ")
         (b a)
         (c a)
         (d a))
    ;; empty words are [nil nil nil nil]
    ;; addresses are numbers (call N, if N,...)
    ;; nil may come after numbers
    (cond ((number? word)
           (setq a word))
          ((vector? word)
           (setq a (ga-format-inst (aref word 0)))
           (when (not (number? (aref word 0)))
             (setq b (ga-format-inst (aref word 1)))
             (when (not (number? (aref word 1)))
               (setq c (ga-format-inst (aref word 2)))
               (when (not (number? (aref word 2)))
                 (setq d (ga-format-inst (aref word 3)))))))
          ((null word) "---")
          (t (error "unknow type for compiled word: '%s'"  word)))
    (format "%2s  %-5s %-5s %-5s %-5s" (ga-format-num i) a b c d)))

(defun ga-create-ram-display-data (coord)
  "create an array of ram display data for node COORD"
  (let* ((mem (gethash coord ga-compiled-nodes))
         data word str)  ;;TODO: cache the ram data
    (if mem
        ;; reserve the first line in the display for node coord and usage
        (progn (setq data (make-vector 65 nil))
               (dotimes (i (min 64 (length mem)))
                 (setq str (ga-format-word i (aref mem i)))
		 (aset data (1+ i) str)
                 ;; (aset data i (mapconcat (lambda (x) (cond ((stringp x) x)
                 ;;                                           ((numberp x) (number-to-string x))
                 ;;                                           ((null x) "~")
                 ;;                                           (t (error "invalid compiled word: '%s'" x))))
                 ;;                         (vector->list (aref mem i))
                 ;;                         " "))
		 )
	       data)
      ga-empty-node-ram-display-data)))

(defun ga-format-sim-word (i word P slot)
  ;;WORD is (value . name), (value), or "port-string"
  (let (name)
    (when (consp word)
      (setq name (cdr word)
            word (car word)))
    (concat (format "%2s %5s " (ga-format-str (ga-format-num i)
                                              (cond ((and P slot) "green")
                                                    (P "magenta")
                                                    (slot "turquoise2")))
                    (ga-format-str (if (numberp word)
                                       (ga-format-num word)
                                     ".")
                                   "grey"))

            (if (numberp word)
                (concat (mapconcat 'identity
                                   (mapcar* (lambda (i inst)
                                              (if (eq i slot)
                                                  (ga-format-str (format "%s" inst) "yellow")
                                                (ga-format-inst inst)))
                                            (number-sequence 0 3)
                                            (disassemble-word word))
                                   " ")
                        (if name
                            (concat "   " name )
                          ""))
              (if (stringp word)
                  word ;;port name
                "~")
              ))))

(defun ga-create-sim-ram-display-data (node)
  (let* ((mem (send node get-tagged-memory))
         data word str)
    ;; reserve the first line in the display for node coord and usage
    (setq data (make-vector (1+ (length mem)) nil)) ;;TODO: compress the unused(repetitive) ram sections
    (dotimes (i (length mem)) ;;TODO: does the display auto adjust the length?
      (setq str (ga-format-sim-word i
                                    (aref mem i)
                                    (= i ga-sim-node-P)
                                    (if (= i ga-sim-node-word-i) ga-sim-node-inst-i nil)))
      (aset data (1+ i) str))
    data))

(defun ga-update-ram-display-with (data)
  ;; save the position of the previous node if manually moved
  (when (and ram-display-moved ga-prev-coord)
    (aset ga-node-ram-display-position
          (coord->index ga-prev-coord)
          (sd-offset ga-ram-display)))
  ;; swap data to current node
  (sd-set-data ga-ram-display data)
  ;; restore position of current node
  (if (or (not ga-sim-p)
          (not (null (aref ga-node-ram-display-position
                           (coord->index ga-current-coord)))))
      (sd-move-to ga-ram-display
                  (or (aref ga-node-ram-display-position (coord->index ga-current-coord))
                      0))
    ;;else: we are in simulation mode and the display has not been manually moved
    (sd-center-on ga-ram-display ga-sim-node-P)))

(defun ga-update-ram-display-node ()
  "Updates the ram display with the compiled data from the current selected node.
Called after ga-current-node is set"
  ;;(assert ga-ram-display)
  (when (and ga-ram-display
	     ga-current-coord)

    (if (and ga-sim-p
             ga-sim-current-node)
        ;;update with simulation ram
        (ga-update-ram-display-with (ga-create-sim-ram-display-data ga-sim-current-node))
      ;;update with compiled data
      (when ga-compiled-nodes
        (ga-update-ram-display-with (ga-create-ram-display-data ga-current-coord))))))

(defun show-buffer-local-variables ()
  (interactive)
  (let ((buf (get-buffer-create (format "*local variables of '%s'*" (buffer-name (current-buffer)))))
        (vars (buffer-local-variables)))
    (switch-to-buffer-other-window buf)
    (widen)
    (delete-region (point-min) (point-max))
    (dolist (v vars)
      (insert (format "%s   %s\n" (car v)  (cdr v))))
    (linum-mode 1)
    (goto-char 1)))

(defun ga-move-ram-view-down ()
  (interactive)
  (when ga-ram-display
    (setq ram-display-moved t)
    (sd-move-down ga-ram-display)))

(defun ga-move-ram-view-up ()
  (interactive)
  (when ga-ram-display
    (setq ram-display-moved t)
    (sd-move-up ga-ram-display)))

(defun ga-move-ram-to-0 ()
  (interactive)
  (when ga-ram-display
    (setq ram-display-moved t)
    (sd-move-to ga-ram-display 0)))

(defun ga-sim-ram-center-on-P ()
  (interactive)
  (ga-check-sim
   (when ga-ram-display
     (aset ga-node-ram-display-position (coord->index ga-current-coord) nil)
     (sd-center-on ga-ram-display ga-sim-node-P))))

(defun ga-set-aforth-source (file &optional buffer)
  (setq ga-project-aforth-file file)
  (setq ga-project-aforth-buffer (or buffer (ga-get-project-file-buffer file)))
  (add-to-list 'ga-project-aforth-buffers (ga-get-project-file-buffer file))
  ;; set aforth-map-buffer in the aforth buffer to point to the buffer of this map
  (let ((this-buffer (current-buffer)))
    (if ga-project-aforth-buffer
        (with-current-buffer ga-project-aforth-buffer
          ;;todo: warn if value is different
          (when (and (not (null aforth-map-buffer))
                     (not (eq aforth-map-buffer this-buffer)))
            (message "Warning: buffer '%s' appears to already be assocated with  another map buffer: %s (setting to '%s')"
                     (current-buffer) aforth-map-buffer this-buffer))

          (setq aforth-map-buffer this-buffer))
      (error "unable to get buffer for project source file '%s'"  file)))

  (ga-set-source-buffer-overlay)
  (ga-update-compilation-data))

(defun ga-select-aforth-source ()
  ;;select the aforth source file for the current ga project
  (interactive)
  (if (eq major-mode 'ga-mode)
      (let ((f (read-file-name "Set GA source: ")))
        (if f
            (ga-set-aforth-source f)
          (message "GA144 aforth source not set")))
    (message "Not in a GA144 project %s" major-mode)))

(defun ga-reset-region ()
  (dolist (coord ga-region-nodes)
    (ga-set-region-face coord 'remove)
    ;;(ga-set-node-overlay node (ga-node-face (ga-coord->node node)))
    )
  (setq ga-region-nodes nil)
  (clrhash ga-visited-nodes))

(defun ga-node-in-region-p (coord)
  (gethash coord ga-visited-nodes))

(defun ga-add-node-to-region (coord)
  (ga-set-region-face coord)
  (push coord ga-region-nodes)
  (puthash coord t ga-visited-nodes))

(defun ga-remove-node-from-region (coord)
  (ga-set-region-face coord 'remove)
  (setq ga-region-nodes (remove coord ga-region-nodes))
  (remhash coord ga-visited-nodes)
  )

;; the point node is part of the region (otherwise there is no way to select the whole map)
;; but it retains the normal point color instead of the region color
;; when the point moves it reverts back to the default color, reverting the

(defun ga-update-path-selection ()
  (assert ga-prev-coord)
  (let ((i 0)
        dir diff coord m count quit)
    (setq diff (- (mod ga-current-coord 100) (mod ga-prev-coord 100))
          m 1)
    (when (= diff 0)
      (setq diff (- (/ ga-current-coord 100)(/ ga-prev-coord 100))
            m 100))
    (when diff
      (setq dir (* (if (> diff 0) 1 -1) m)
            count (abs diff))
      (setq coord ga-prev-coord)
      (while (and (< i count)
                  (not quit))
        (setq i (1+ i))
        (setq coord (+ coord dir))

        (if (ga-node-in-region-p coord)
            (progn (message "Error: Cannot cross path")
                   (setq quit coord))
          (ga-add-node-to-region coord))
        ))))

(defun ga-get-rectangle-nodes (c1 c2)
  ;; returns a list of all nodes in the rectangle defined by corners C1 and C2
  (let* ((y1 (/ c1 100))
         (y2 (/ c2 100))
         (x1 (mod c1 100))
         (x2 (mod c2 100))
         (bottom-left (+ (* (min y1 y2) 100)
                         (min x1 x2)))
         coords)
    (dotimes (x (1+ (abs (- x1 x2))))
      (dotimes (y (1+ (abs (- y1 y2))))
        (push (+ (* y 100) x bottom-left) coords)))
    coords))

(defun ga-update-rectangle-selection ()
  (let ((rectangle-nodes (ga-get-rectangle-nodes ga-mark-coord ga-current-coord)))
    ;; remove nodes that are no longer part of the selection
    (dolist (coord ga-region-nodes)
      (unless (member coord rectangle-nodes)
        (ga-remove-node-from-region coord)))
    ;; add nodes tha are now part of the selection
    (dolist (coord rectangle-nodes)
      (unless (ga-node-in-region-p coord)
        (ga-add-node-to-region coord)))))

(defun ga-clear-selection ()
  )

(defun update-position ()
  ;;(setq ga-modified-p t)
  ;;(ga-move-to-node ga-current-coord 'middle)
  (let ((prev-coord (or ga-prev-coord 0)))
    (if ga-mark-active
        (if ga-region-path-p
            (ga-update-path-selection)
          (ga-update-rectangle-selection))
      (ga-clear-selection))
    (move-selected-node-overlay prev-coord ga-current-coord))
  ;;(message "current coord: %s" ga-current-coord)
  (ga-sim-update-display)
  )

(defun ga-draw-map-in-frame-limits ()
  (let ((max-size (/ (window-max-chars-per-line) 18)))
    (if (> ga-node-size max-size)
        ;; renders the map as large as possible but does not set ga-node-size so the change is not persistent
        (ga-render max-size)
      (ga-render ga-node-size)
      )))

(defun ga-handle-window-size-change (frame)
  ;;TODO: fix, this needs to set the map buffer as current or local variables cannot be accessed
  ;;  (and ga-auto-resize-map-on-window-change
  ;;       (ga-draw-map-in-frame-limits))

  )

(setq ga-current-focus-buffer nil) ;;buffer that is currently in focus
(setq ga-maps nil);;maps buffer names to buffers

(defun ga-set-map-focus (state)
  (if state
      (progn (dolist (coord ga-region-nodes)
               (ga-set-node-point-face coord nil))
             (ga-set-node-point-face ga-current-coord ga-select-face))
    (progn (dolist (coord ga-region-nodes)
             (ga-set-node-point-face coord ga-unfocused-face))
           (ga-set-node-point-face ga-current-coord ga-unfocused-face))))

(defun ga-set-map-buffer-focus (buffer focus)
  (with-current-buffer buffer
    (ga-set-map-focus focus)))

(defun ga-rescan-buffers-for-maps()
  ;; reconstruct the value for the variale `ga-maps` in the case that it gets corrupted
  ;; this should not normally be needed. but is helpfull when ga-maps get set to nil,
  ;; for example when eval-buffer is run
  (message "Something is wrong. re-scanning buffers for maps...")
  (let (maps)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'ga-mode)
          (push (cons (buffer-name) buffer) maps))))
    maps))

(defun ga-assoc-delete-all (key alist)
  "Delete from ALIST all elements whose car is `equal' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (while (and (consp (car alist)) (equal (car (car alist)) key))  (setq alist  (cdr alist)))
  (let ((tail  alist)
        tail-cdr)
    (while (setq tail-cdr  (cdr tail))
      (if (and (consp (car tail-cdr))  (equal (car (car tail-cdr)) key))
          (setcdr tail (cdr tail-cdr))
        (setq tail  tail-cdr))))
  alist)

(defun ga-update-map-focus ()
  (when (eq (window-buffer (selected-window))
	    (current-buffer))
    (when (not (buffer-live-p ga-current-focus-buffer))
      (setq ga-current-focus-buffer nil))
    ;; selected window has current buffer
    (when (and ga-current-focus-buffer
               (not (eq ga-current-focus-buffer
                        (current-buffer))))
      ;; current map lost focus
      (ga-set-map-buffer-focus ga-current-focus-buffer nil)
      (setq ga-current-focus-buffer nil))
    (when (eq major-mode 'ga-mode)
      ;; set focus on new map
      (ga-set-map-focus t)
      (setq ga-current-focus-buffer (current-buffer)))))

(defun ga-kill-buffer-handler ()
  (when (eq (cdr (assoc (buffer-name) ga-maps)) (current-buffer))
    (when (eq ga-current-focus-buffer (current-buffer))
      (setq ga-current-focus-buffer nil))

    (setq ga-maps (ga-assoc-delete-all (buffer-name) ga-maps))
    (unless ga-maps
      ;; There are no more maps so there is no need to track focus, and
      ;; if the hook is not removed we will end up iterating through all buffers with ga-rescan-buffers-for-maps
      (remove-hook 'buffer-list-update-hook 'ga-update-map-focus)
      ))

  (when ga-project-aforth-buffer
    (with-current-buffer ga-project-aforth-buffer
      (setq aforth-map-buffer nil))))

(defun ga-set-mark ()
  (interactive)
  (ga-reset-region)
  (if (and ga-mark-coord
           (eq ga-mark-coord ga-current-coord))
      (progn (setq ga-mark-active (not ga-mark-active))
             (if ga-mark-active
                 (message "GA144 mark activated")
               (message "GA144 mark deactivated")))
    (progn (setq ga-mark-active t)
           (message "GA144 mark set")))
  (when ga-mark-active
    (push ga-current-coord ga-region-nodes)
    (ga-set-region-face ga-current-coord))
  (setq ga-mark-coord ga-current-coord))

(defun ga-exchange-point-and-mark ()
  (interactive)
  (let ((mark ga-mark-coord))
    (setq ga-mark-coord ga-current-coord)
    (ga-set-selected-node mark)))

(defun ga-keyboard-quit ()
  "cancel the current operation"
  (interactive)
  (ga-reset-region)
  (setq ga-mark-active nil)
  (keyboard-quit))

(defun ga-kill-map ()
  (interactive)
  ;;TODO: cleanup map reference in .aforth buffer
  (let ((del (cond ((not (eq major-mode 'ga-mode)) nil)
                   (ga-sim-p t)
                   ((and (not ga-map-view-mode)
                         (buffer-modified-p))
                    (yes-or-no-p "map modified, kill anyways?"))
                   (t (and (y-or-n-p "kill map?")
                           t)))))
    (when del
      (delete-windows-on (current-buffer))
      (kill-buffer))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; color selection

(defun ga-color-node (coord color)
  (set-buffer-modified-p t)
  (ga-set-node-default-face coord (list :background color)))

(defun ga-color-nodes (nodes color)
  ;; nodes is a list of coordinates
  (dolist (node nodes)
    (ga-color-node node color)))

(defun ga-select-color-at-line ()
  (interactive)
  (if ga-color-select-buffer-p
      (let* ((start (progn (beginning-of-line) (point)))
             (end (progn (end-of-line) (point)))
             (str (buffer-substring-no-properties start end))
             (name (car (split-string str "  ")))
             (code (last (split-string str)))
             (coord  ga-color-select-coord))
        (with-current-buffer ga-color-select-map-buffer
          (ga-color-select-callback coord name code)))
    (message "Not in GA144 color selection buffer")))

(defun ga-kill-color-select-buffer ()
  (when ga-color-select-buffer
    (kill-buffer ga-color-select-buffer)
    (setq ga-color-select-buffer nil)))

(defun ga-quit-color-select ()
  (interactive)
  (when (and ga-color-select-buffer-p
             ga-color-select-map-buffer)
    (with-current-buffer ga-color-select-map-buffer
      (ga-kill-color-select-buffer))))

(defun ga-color-select-callback (coord str code)
  (when t ;;; (or (eq coord ga-current-coord)
          ;;;   (y-or-n-p (format "Selected node from %s to %s. Apply color '%s' to node %s?"
          ;;;                     coord ga-current-coord str ga-current-coord)))
    (when (consp code)
      (if(= (length code) 1)
          (setq code (car code))
        (error (format "invalid color code: %s" code))))

    (if (> (length ga-region-nodes) 1)
        (ga-color-nodes ga-region-nodes code)
      (ga-color-node ga-current-coord code)))

  (ga-quit-color-select)
  (switch-to-buffer (current-buffer)))

(defun ga-select-node-color ()
  (interactive)
  (if (and (eq major-mode 'ga-mode)
           (not (null ga-current-coord)))

      (let ((map-buf (current-buffer))
            (coord ga-current-coord))

        (ga-kill-color-select-buffer) ;;cleanup old color select buffer

        (setq ga-color-select-buffer (get-buffer-create
                                      (concat ga-project-name " GA144 color select" )))

        (switch-to-buffer ga-color-select-buffer)
        (setq ga-color-select-buffer-p t)
        (setq ga-color-select-map-buffer map-buf)
        (setq ga-color-select-coord coord)
        (list-colors-display nil (buffer-name))
        (use-local-map
         (let ((map (make-sparse-keymap 'ga-color-select-map)))
           (define-key map (kbd "<return>") 'ga-select-color-at-line)
           (define-key map (kbd "q") 'ga-quit-color-select)
           map)))
    (message "Not in GA144 map buffer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ga-draw-arrow ()
  (interactive)
  (error "TODO"))

(defun ga-edit-node-text ()
  (interactive)
  (error "TODO"))

(setq ga-svg-supported (require 'svg nil :no-error))

(defun ga-export-as-svg ()
  (interactive)
  (if ga-svg-supported
      (error "TODO")
    (message "svg export is not supported without the svg library")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ga-check-in-map-buffer()
  (and (boundp 'ga-nodes)
       (not (null ga-nodes))))

(defun ga-view-project-file ()
  (interactive)
  (assert (ga-check-in-map-buffer))
  (let ((filename ga-project-file))
    (switch-to-buffer (get-buffer-create (format "*%s-project-file*" ga-project-name)))
    (insert-file-contents-literally filename))
  (emacs-lisp-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ga-goto-first-non-empty-node ()
  ;; move selected node to first non-empty node on the current row
  (interactive)
  (let* ((coord (* (/ ga-current-coord 100) 100))
         (max (+ coord 18))
         found)
    (while (and (null found)
                (< coord max))
      (setq found (assoc coord ga-node-locations))
      (when (not found)
        (incf coord)))
    (when found
      (ga-set-selected-node coord))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; simulation support

(defmacro ga-check-sim (&rest body)
  `(if ga-sim-p
       (progn ,@body)
     (message "Not in GA144 simulation mode")))


;; set by ga-run-simulator.el to force loading the simulated bootstream without
;; having to reset the chip (which is currently broken)
(setq ga-load-bootstream nil)

(defun ga-sim-load (assembly)
  (if ga-load-bootstream
      (progn
        (message "Simulating bootstream")
        (let ((bs (make-bootstream assembly "async"))) ;;TODO: other bootstream types
          (ga-sim-continue) ;;leaves all nodes in port exec mode
          (send ga-sim-ga144 load-bootstream bs))
        (send ga-sim-ga144 reset-time))
    (send ga-sim-ga144 load assembly))
  (ga-update-current-node-registers))

(defun ga-sim-recompile ()
  "Recompile the simulation source files and update ga144 simulation
This resets the simulation"
  (interactive)
  (assert ga-project-aforth-buffer)
  (let ((this-buffer (current-buffer)))
    (with-current-buffer ga-project-aforth-buffer
      (aforth-update-compilation-for-map-buffer this-buffer))))

(defun ga-sim-reset (&optional bootstream)
  (send ga-sim-ga144 reset!)
  ;;TODO: rese also wipes the nodes - need to reload
  (if bootstream
      (let ((bs (make-bootstream ga-assembly-data "async")))
        (send ga-sim-ga144 load-bootstream bs))
    (send ga-sim-ga144 load ga-assembly-data))
  (ga-update-current-node-registers)
  (ga-sim-update-display))

(defun ga-sim-reset-command ()
  (interactive)
  (ga-check-sim
   (when (or t (y-or-n-p "Reset simulation?"))
     (ga-sim-reset)
     (message "reset"))))

(defun ga-load-from-bootstream ()
  (interactive)
  (ga-check-sim
   (when (or t (y-or-n-p "Reset simulation?"))
     (ga-sim-reset t)
     (message "Reset! *loading from bootstream*"))))

(defun ga-sim-set-current-node (coord)
  (assert ga-sim-ga144)
  (setq ga-sim-current-node (send ga-sim-ga144 coord->node coord))
  (ga-update-current-node-registers))

(defun ga-update-current-node-registers ()
  (let* ((registers (send ga-sim-current-node get-registers)))
    (setq ga-sim-node-P (aref registers 2))
    (setq ga-sim-node-inst-i (send ga-sim-current-node get-inst-index))
    (setq ga-sim-node-word-i (aref registers 8))
    ;;(message "ga-sim-node-inst-i = %s" ga-sim-node-inst-i)
    ;;(message "ga-sim-node-word-i = %s" ga-sim-node-inst-i)
    ))

(defun ga-sim-step-node ()
  "Steps the current selected node one instruction"
  ;;TODO: support for stepping variable
  (interactive)
  (ga-check-sim
   (send ga-sim-current-node step-program-n! ga-sim-step-increment)
   (when (send ga-sim-current-node suspended?)
     (message "node suspended"))
   (ga-update-current-node-registers)
   (ga-sim-update-display)
   ))

(defun ga-sim-step-chip ()
  "Steps all the active nodes one instruction"
  ;;TODO: support for stepping variable
  (interactive)
  (ga-check-sim
   (send ga-sim-ga144 step-program-n! ga-sim-step-increment)
   (message "%s active nodes" (send ga-sim-ga144 num-active-nodes))
   (ga-update-current-node-registers)
   (ga-sim-update-display)
   ))

(defun ga-set-step-increment ()
  (interactive)
  (ga-check-sim
   (let* ((s (number-to-string ga-sim-step-increment))
          (n (string-to-number (read-string "step incrment> " s nil s))))
     (if (> n 0)
         (setq ga-sim-step-increment n)
       (message "invalid step increment: %s" n)))))

;;;TODO: need to update the display on some interval
(defun ga-sim-continue ()
  (interactive)
  (ga-check-sim
   (let ((again true)
         (breakpoint? nil)
         (nactive nil))
     (setq ga-run-sim t)
     (while (and again
                 (not breakpoint?)
                 ga-run-sim)
       (set! again nil)
       (setq nactive (send ga-sim-ga144 num-active-nodes))
       (when (and (> nactive 0)
                  (not breakpoint?))
         (setq again t)
         (setq breakpoint? (send ga-sim-ga144 step-program-n! 100)))
       (ga-sim-update-display)
       (message "%s active"  nactive) ;;TODO: don't display as message (or disable messages), display total steps
       ))))

(defun ga-convert-stack-list (data)
  (list->vector (mapcar (lambda (x) (format "%-5s" (ga-format-num x t))) data)))

(defun ga-disassemble-I (val)
  (if (send ga-sim-current-node fetching?)
      ;;fetching is in progress so the current vaue of I is invalid
      "fetching..."
    (mapconcat 'identity
               (mapcar* (lambda (i inst)
                          (if (eq i ga-sim-node-inst-i)
                              (ga-format-str (format "%s" inst) "yellow")
                            (ga-format-inst inst)))
                        (number-sequence 0 3)
                        (disassemble-word val))
               " ")))

(defun ga-reg-address-name (n)
  (let ((name (gethash n address-names)))
    (if name
        (ga-format-str name "aquamarine")
      "")))

(defun ga-convert-reg-list (data)
  (list->vector (append (mapcar (lambda (x) (format "%-3s %s  %s"
                                                    (car x)
                                                    (ga-format-num (cdr x))
                                                    (ga-reg-address-name (cdr x))))
                                `(("A" . ,(aref data 0))
                                  ("B" . ,(aref data 1))
                                  ("P" . ,(aref data 2))
                                  ;;("I" . ,(aref data 3))
                                  ("IO" . ,(aref data 7))))
                        (list (format "I   %s  %s"
                                      (if (port-addr? (aref data 2))
                                          "<port>"
                                        (format "%x" (aref data 3)))
                                      (ga-disassemble-I (aref data 3))))
                        )))

(defun ga-update-stack-displays()
  (when (and ga-sim-p
             ga-data-display
             ga-return-display
             ga-register-display)
    (let ((dstack (ga-convert-stack-list (send ga-sim-current-node get-dstack-as-list)))
          (rstack (ga-convert-stack-list (send ga-sim-current-node get-rstack-as-list)))
          (registers (ga-convert-reg-list (send ga-sim-current-node get-registers))))
      ;;(message "dstack = %s" dstack)
      ;;(message "rstack = %s" rstack)
      (sd-set-data ga-data-display dstack)
      (sd-set-data ga-return-display rstack)
      (sd-set-data ga-register-display registers))))

(defun ga-sim-update-display (&optional node)
  (when ga-ram-display
    (ga-update-ram-display-node)
    )
  (ga-update-stack-displays)
  (redisplay))

(defun ga-sim-set-breakpoint ()
  )

(defun ga-node-debug-dump ()
  (interactive)
  (let ((node ga-sim-current-node))
    (if node
        (message (with-temp-buffer
                   (send node describe-io)
                   (send node display-all)
                   (buffer-string)))
      (message "no node selected"))))

(define (ga-call-word)
  (interactive)
  (assert ga-sim-current-node)
  (let ((word (read-string "word> ")))
    (when word
      (send ga-sim-current-node call-word! word)
      (ga-update-current-node-registers)
      (ga-sim-update-display)
      )))

(defun ga-toggle-source-view ()
  (interactive)
  (setq ga-show-source-position (not ga-show-source-position))
  (message (if ga-show-source-position
               "Source view enabled"
             "Source view disabled")))

(defun ga-toggle-history-display-length ()
  (interactive)
  (setq activity-history-show-total (not activity-history-show-total))
  (message (if activity-history-show-total
               "Showing total activity"
             "Showing recent activity")))

(setq ga-mode-map
      (let ((map (make-sparse-keymap 'ga-mode-map)))
        (define-key map "+" 'ga-inc-node-size)
        (define-key map "=" 'ga-inc-node-size)
        (define-key map "-" 'ga-dec-node-size)
        (define-key map (kbd "<up>") 'ga-move-up)
        (define-key map (kbd "<down>") 'ga-move-down)
        (define-key map (kbd "<left>") 'ga-move-left)
        (define-key map (kbd "<right>") 'ga-move-right)
        (define-key map (kbd "C-x C-s") 'ga-save)
        (define-key map (kbd "C-e") 'ga-move-right-end)
        (define-key map (kbd "C-a") 'ga-move-left-end)
        (define-key map (kbd "C-b") 'ga-move-left)
        (define-key map (kbd "M-b") 'ga-move-left-half)
        (define-key map (kbd "C-f") 'ga-move-right)
        (define-key map (kbd "M-f") 'ga-move-right-half)
        (define-key map (kbd "C-p") 'ga-move-up)
        (define-key map (kbd "M-p") 'ga-move-top-half)
        (define-key map (kbd "C-n") 'ga-move-down)
        (define-key map (kbd "M-n") 'ga-move-bottom-half)
        (define-key map (kbd "M-<") 'ga-move-top)
        (define-key map (kbd "M->") 'ga-move-bottom)
        (define-key map (kbd "<return>") 'ga-return-key-fn)
        (define-key map (kbd "C-c C-f") 'ga-select-aforth-source)
        (define-key map (kbd "C-SPC") 'ga-set-mark)
        (define-key map (kbd "C-x C-x") 'ga-exchange-point-and-mark)
        (define-key map (kbd "C-g") 'ga-keyboard-quit)
        (define-key map (kbd "C-x k") 'ga-kill-map)
        ;;(define-key map (kbd "q") 'ga-kill-map)
        (define-key map (kbd "C-c b") 'bury-buffer)
	(define-key map (kbd "C-c v") 'ga-goto-source-buffer)
        (define-key map (kbd "<") 'ga-move-ram-view-down)
        (define-key map (kbd ",") 'ga-move-ram-view-down)
        (define-key map (kbd ">") 'ga-move-ram-view-up)
        (define-key map (kbd ".") 'ga-move-ram-view-up)
        (define-key map (kbd "0") 'ga-move-ram-to-0)
        (define-key map (kbd "M-m") 'ga-goto-first-non-empty-node)
        ;;simulation keys
        (define-key map (kbd "s") 'ga-sim-step-node)
        (define-key map (kbd "S") 'ga-sim-step-chip)
        (define-key map (kbd "g") 'ga-sim-reset-command)
        (define-key map (kbd "Q") 'ga-kill-map)
        (define-key map (kbd "D") 'ga-node-debug-dump)
        (define-key map (kbd "x") 'ga-toggle-hex)
        (define-key map (kbd ".") 'ga-sim-ram-center-on-P)
        (define-key map (kbd "b") 'ga-load-from-bootstream)
        (define-key map (kbd "n") 'ga-set-step-increment)
        (define-key map (kbd "c") 'ga-sim-continue)
        (define-key map (kbd "w") 'ga-call-word)
        (define-key map (kbd "p") 'ga-toggle-source-view)
        (define-key map (kbd "t") 'ga-toggle-history-display-length)
        ;;display views
        (define-key map (kbd "u") 'ga-display-node-usage)
        (define-key map (kbd "a") 'ga-display-node-activity)
        map))

(defun ga-map-buffer-valid (buf)
  "verifies that the map buffer BUF is still alive"
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (and ga-buffer-valid-p
           (buffer-live-p ga-project-aforth-buffer)
           ))))

(defun ga-set-map-nodes ()
  (let (f18node coord)
    (assert ga-sim-ga144)
    (loop-nodes node
      (setq coord (ga-node-coord node))
      (assert coord)
      (setq f18node (send ga-sim-ga144 coord->node coord))
      (assert f18node)
      (send f18node set-map-node node (current-buffer)))))

(defun ga-open-map-for-buffer (aforth-buffer &optional buf-name-fmt)
  (let* ((filename (buffer-file-name aforth-buffer))
         (buffer-name (format (or buf-name-fmt "*GA144-%s*") (file-name-base filename)))
         (buf (get-buffer buffer-name)))
    (unless (ga-map-buffer-valid buf)
      (setq buf nil))
    (or buf
        (progn
          (setq buf (get-buffer-create buffer-name))
          (with-current-buffer buf
            (setq ga-map-view-mode t)
            (setq ga-project-aforth-buffer aforth-buffer)
            (setq ga-project-aforth-file filename)
            (ga-mode)
            ;;(ga-set-aforth-source filename)
            )
          buf))))

(defun ga-open-map-for-simulation (aforth-buffer)
  (let ((buf (ga-open-map-for-buffer aforth-buffer "*GA144 SIM: %s*")))
    (with-current-buffer buf
      (setq ga-sim-p t)
      (setq ga-sim-ga144 (make-ga144 buffer-file-name nil aforth-buffer))
      (ga-set-map-nodes)
      (ga-sim-set-current-node ga-current-coord)
      (ga-sim-recompile)
      (ga-draw-map-in-frame-limits);;need to redraw to display simulation
      ;(ga-sim-reset)
      ;;TODO: this duplicate display should not be necessary
      )
    buf))

(define-derived-mode ga-mode nil "GA144"
  "A major mode for programming the GA144."
  (setq ga-buffer-valid-p nil)
  (use-local-map ga-mode-map)
  (setq show-trailing-whitespace nil)

  (when (not buffer-file-name)
    (setq ga-map-view-mode t))

  (if (or ga-map-view-mode
          (string-match "ga144$" buffer-file-name))
      (progn
        (unless ga-map-view-mode
          (setq ga-project-file buffer-file-name)
          (setq ga-project-name (file-name-base buffer-file-name))
          (assert ga-project-name)
          (assert (not (string= ga-project-name "nil"))))
        ;; open all files associated with this map, collect their buffers
        (unless ga-map-view-mode
          (setq ga-project-aforth-files (ga-aforth-files (file-name-directory  buffer-file-name)))
          (setq ga-project-aforth-buffers (mapcar 'ga-get-project-file-buffer ga-project-aforth-files)))
        ;; set buffer local variables defaults
        (setq ga-project-aforth-file-overlay (make-overlay 0 0))
        (setq ga-node-size ga-default-node-size)
        (setq ga-project-aforth-compile-status-overlay (make-overlay 0 1))
        (setq ga-node-ram-display-position (make-vector 144 nil))
	(ga-set-compilation-status "Unknown")

        (if ga-map-view-mode
            (push (cons (buffer-name) (current-buffer)) ga-maps)
          (let ((buffer-name (format "*GA144-%s*" ga-project-name)))
            (when (get-buffer buffer-name)
              (kill-buffer buffer-name))
            (rename-buffer buffer-name)
            (push (cons buffer-name (current-buffer)) ga-maps)))

        (setq buffer-file-name nil
              ga-nodes nil
              ga-nodes-sans-overlays nil
              ga-current-coord nil)
        (unless ga-map-view-mode
          (eval-buffer)

          (when ga-nodes-sans-overlays
            (setq ga-nodes (ga-restore-node-overlays ga-nodes-sans-overlays))))

        (if ga-nodes
            (message "Loading GA144 project map...")
          (unless ga-map-view-mode (print "Creating new ga144 map.."))
          (ga-create-new))

        (ga-startup-reset)
        (ga-draw-map-in-frame-limits)
        (set-buffer-modified-p nil)
        (setq truncate-lines t) ;; any line wrap will ruin the map
        (read-only-mode 1)
        (setq visible-cursor nil
              cursor-type nil
              ga-region-nodes nil
              ga-region-path-p nil
              ga-visited-nodes (make-hash-table))
        (when ga-project-aforth-file
          ;;need to call ga-set-aforth-source so that it can update varous things
          (ga-set-aforth-source ga-project-aforth-file))
        (add-hook 'window-size-change-functions 'ga-handle-window-size-change)
        (ga-set-map-focus t)
        (add-hook 'buffer-list-update-hook 'ga-update-map-focus)
        (add-hook 'kill-buffer-hook 'ga-kill-buffer-handler)
        (ga-set-selected-node ga-current-coord)
        (setq ga-buffer-valid-p t))
    (message "ga144-mode: invalid file format")))

(defun ga-restore-node-overlays ( ga-nodes )
  (let (o)
    (loop-nodes node
      (setq o (make-overlay 0 0))
      (overlay-put o 'face ga-node-coord-face)
      (set-ga-node-coord-overlay! node o)))
  ga-nodes)

(add-to-list 'auto-mode-alist '("\\.ga144$" . ga-mode))

(provide 'ga144-map)
