(require 'cl)
(require 'gv)

(defmacro def-local (name &optional default docs)
  `(progn (defvar ,name ,default, docs)
          (make-variable-buffer-local ',name)
          (put ',name 'permanent-local t)))

(def-local ga144-default-node-size 3)
(def-local ga144-project-name nil)
(def-local ga144-project-file nil)
(def-local ga144-nodes nil)
(def-local ga144-current-coord nil)
(def-local ga144-prev-coord nil)
(def-local ga144-modified-p nil)
(def-local ga144-node-size nil)
(def-local ga144-project-aforth-files nil)
(def-local ga144-project-aforth-buffers nil)
(def-local ga144-project-aforth-file nil)
(def-local ga144-project-aforth-file-overlay nil)
(def-local ga144-has-unsaved-changes nil)
(def-local ga144-project-aforth-compile-status nil)
(def-local ga144-project-aforth-compile-status-overlay nil)
(def-local ga144-mark-active nil)
(def-local ga144-mark-coord nil)
(def-local ga144-region-nodes nil) ;; ordered collection of all node coordinates in the region
(def-local ga144-region-path-p  nil) ;; t if ga144-region-path-p represents a node path
(def-local ga144-visited-nodes nil) ;; hash table of visited nodes


(defvar ga144-auto-resize-map-on-window-change t)

(setq ga144-persistent-variables '(ga144-nodes-sans-overlays ga144-node-size ga144-current-coord ga144-project-aforth-file))


(defface ga144-node-coord-face '((((background light)) (:foreground "black")) ;;gold1
                                 (((background dark)) (:foreground "black"))
                                 (t (:bold t)))
  "ga144 face for node coordinate numbers")

(defface ga144-default-face-1 '((((background light)) (:background "LightSkyBlue1"))
                                (((background dark)) (:background "LightSkyBlue1")))
  "default ga144 node face 1")

(defface ga144-default-face-2 '((((background light)) (:background "LightSkyBlue2"))
                                (((background dark)) (:background "LightSkyBlue2")))
  "default ga144 node face 2")

(defface ga144-select-face '((((background light)) (:background "SeaGreen3"))
                             (((background dark)) (:background "SeaGreen3")))
  "default ga144 selected node face")

(defface ga144-unfocused-face '((((background light)) (:background "LightSkyBlue4"))
                                (((background dark)) (:background "LightSkyBlue4")))
  "default ga144 face for the selected, unfocused node")

(defface ga144-region-face-1 '((((background light)) (:background "gold1"))
                               (((background dark)) (:background "gold1")))
  "default ga144 node region face 2")

(defface ga144-region-face-2 '((((background light)) (:background "gold2"))
                               (((background dark)) (:background "gold2")))
  "default ga144 node region face 2")


(setq ga144-node-coord-face 'ga144-node-coord-face)
(setq ga144-default-face-1 'ga144-default-face-1)
(setq ga144-default-face-2 'ga144-default-face-2)
(setq ga144-select-face 'ga144-select-face)
(setq ga144-unfocused-face 'ga144-unfocused-face)
(setq ga144-region-face-1 'ga144-region-face-1)
(setq ga144-region-face-2 'ga144-region-face-2)


(defun ga144-get-project-file-buffer (filepath)
  (let ((buff (find-buffer-visiting filepath)))
    (unless buff
      (find-file filepath)
      (setq buff (current-buffer))
      (bury-buffer))
    buff))

(defun ga144-aforth-files (dir)
  (let ((ok '())
        (files (directory-files dir)))
    (dolist (file files)
      (when (string-match "\\.aforth$" file)
        (push file ok)))
    ok))

(defun ga144-render( node-size )
  (ga144-draw-map node-size)
  (goto-char 1)
  (update-position))

(defun ga144-move-to-node (coord &optional middle node-size)
  (goto-char 1)
  (let ((row (- 7 (coord->row coord)))
        (col (coord->col coord))
        (node-size (or node-size ga144-node-size)))
    (forward-line (+ (* row  node-size) (if middle (/ node-size 2) 0)))
    (forward-char (+ (* col node-size) (if middle (floor (/ node-size 2)) 0)))))

(defun ga144-draw-map (node-size)
  (read-only-mode -1)
  (erase-buffer)
  (goto-char 1)
  (let (x coord l o)
    ;; insert map chars
    (dotimes (_ (* node-size 8))
      (insert (make-string (* node-size 18) ? ) "\n" ))
    ;; aforth file chars and overlay
    (let ((s "source file: ") p)
      (insert "\n" (- (* node-size 8) (1+ (length s))))
      (beginning-of-line)
      (setq p (point))
      (insert s)
      (move-overlay ga144-project-aforth-file-overlay p (point)))
    ;;compile status overlay
    (let ((s "Compilation status: ") p)
      (insert "\n" (- (* node-size 8) (1+ (length s))))
      (beginning-of-line)
      (setq p (point))
      (insert s)
      (move-overlay ga144-project-aforth-compile-status-overlay p (point)))

    ;; set map overlays
    (loop-nodes node
      (setq coord (ga144-node-coord node))
      (ga144-move-to-node coord nil node-size)
      (setq s (number-to-string coord)
            l (length s))
      (delete-char l)
      (insert s)
      (setq o (ga144-node-coord-overlay node))
      (move-overlay o (- (point) l) (point))
      (setf (ga144-node-coord-overlay node) o))
    ;; set aforth file overlay string
    (overlay-put ga144-project-aforth-file-overlay 'after-string (or ga144-project-aforth-file "None"))
    (read-only-mode 1)
    (ga144-create-overlays node-size)
    ;; set compile status overlay string
    (overlay-put ga144-project-aforth-compile-status-overlay 'after-string  ga144-project-aforth-compile-status))
  (ga144-update-overlay-faces))


(defun ga144-delete-overlays ()
  (let (o overlays coord face column)
    (loop-nodes node
      (dolist (o (ga144-node-overlays node))
        (delete-overlay o))
      (setf (ga144-node-overlays node) nil))))

(defun ga144-create-overlays (node-size)
  (ga144-delete-overlays)
  (loop-nodes node
    (setq coord (ga144-node-coord node)
          overlays nil)
    (ga144-move-to-node coord nil node-size)
    (setq column (current-column)
          face (ga144-get-node-face node))
    (dotimes (i node-size)
      (setq o (make-overlay (point) (+ (point) node-size)))
      (overlay-put o 'face face)
      (push o overlays)
      (when (< i (- node-size 1))
        (forward-line)
        (beginning-of-line)
        (forward-char column)))
    (setf (ga144-node-overlays node) overlays)))

(defmacro loop-nodes (var &rest body)
  (declare (indent 1) (debug (symbolp body)))
  (assert (symbolp var))
  `(mapc (lambda (,var)
           ,@body)
         ga144-nodes))

(defun ga144-get-node-face (node)
  ;; get the current face to display
  (let ((faces (ga144-node-faces node)))
    (assert (and (arrayp faces) (= (length faces) ga144-num-faces)))
    (or (aref faces 4) ;; tmp high
        (aref faces 3) ;; point
        (aref faces 2) ;; tmp low
        (aref faces 1) ;; base
        (aref faces 0) ;; default
        )))

(defun ga144-update-node-overlays (node)
  (let ((face (ga144-get-node-face node)))
    (dolist (o (ga144-node-overlays node))
      (overlay-put o 'face face))))

(defun ga144-update-overlay-faces ()
  (loop-nodes node
    (ga144-update-node-overlays node)))

(defun ga144-set-node-face-internal (coord idx face)
  (let* ((node (coord->node coord))
         (faces (ga144-node-faces node)))
    (assert (and (arrayp faces) (= (length faces) ga144-num-faces)))
    (aset faces idx face)
    (setf (ga144-node-faces node) faces)
    (ga144-update-node-overlays node)
    ))

(defun ga144-set-node-default-face (coord face)
  (ga144-set-node-face-internal coord 0 face))

(defun ga144-set-node-base-face (coord face)
  (ga144-set-node-face-internal coord 1 face))

(defun ga144-set-node-tmp-low-face (coord face)
  (ga144-set-node-face-internal coord 2 face))

(defun ga144-set-node-point-face (coord face)
  (ga144-set-node-face-internal coord 3 face))

(defun ga144-set-node-tmp-high-face (coord face)
  (ga144-set-node-face-internal coord 4 face))

(defun ga144-set-region-face (coord &optional remove)
  (let ((node (coord->node coord)))
    (ga144-set-node-face-internal coord 2 (if remove nil (ga144-node-region-face node)))))

(defstruct ga144-node coord special-function node-type text color overlays region-face faces coord-overlay)

(defun ga144-valid-node-index-p(n)
  (and (>= n 0) (< n 144)))

(defun coord->index (n)
  (assert (ga144-valid-coord-p n))
  (+ (* (floor (/ n 100)) 18) (mod n 100)))

(defun index->coord (n)
  (assert (ga144-valid-node-index-p n))
  (+ (* (floor (/ n 18)) 100) (mod n 18)))

(defun coord->row (coord)
  (assert (ga144-valid-coord-p coord))
  (floor (/ coord 100)))

(defun coord->col (coord)
  (assert (ga144-valid-coord-p coord))
  (mod coord 100))

(defun coord->node (coord)
  (assert (ga144-valid-coord-p coord))
  (aref ga144-nodes (coord->index coord)))

(defun ga144-get-node-type (coord)
  )

(defun ga144-get-node-default-faces (coord)
  (let ((a (= (mod (/ coord 100) 2) 0))
        (b (= (mod (mod coord 100) 2) 0)))
    (if (eq a b)
        (cons ga144-default-face-1 ga144-region-face-1)
      (cons ga144-default-face-2 ga144-region-face-2))))

(setq ga144-num-faces 5)

(defun ga144-make-face-vector (default-face)
  (let ((v (make-vector ga144-num-faces nil)))
    (aset v 0 default-face)
    v))

(defun ga144-create-new ()
  (let (faces coord coord-overlay default region-face)
    (setq ga144-nodes (make-vector 144 nil))
    (dotimes (i 144)
      (setq coord (index->coord i))
      (setq coord-overlay (make-overlay 0 0 ))
      (setq default-faces (ga144-get-node-default-faces coord))
      (overlay-put coord-overlay 'face ga144-node-coord-face)
      (aset ga144-nodes i (make-ga144-node :coord coord
                                           :special-function (ga144-get-node-type coord)
                                           :faces (ga144-make-face-vector (car default-faces))
                                           :region-face (cdr default-faces)
                                           :coord-overlay coord-overlay)))
    (setq ga144-current-coord 700)
    (ga144-save)
    ))

(defun ga144-save ()
  (interactive)
  (let ((ga144-nodes-sans-overlays (vconcat (mapcar 'copy-sequence ga144-nodes)))
        node)
    (dotimes (i 144)
      (setq node (aref ga144-nodes-sans-overlays i))
      (setf (ga144-node-overlays node) nil)
      (setf (ga144-node-coord-overlay node) nil))

    (let ((print-level nil)
          (print-length nil)
          (values (mapcar (lambda (x) (cons x (eval x))) ga144-persistent-variables))) ;;the values are buffer-local
      (with-temp-file ga144-project-file
        (dolist (v values)
          (insert "(setq " (symbol-name (car v)))
          (print (cdr v) (current-buffer))
          (insert ")\n\n")))))
  (message "saved in %s" ga144-project-file)
  (setq ga144-modified-p nil))

(defun ga144-inc-node-size ()
  (interactive)
  (if (< (* (1+ ga144-node-size) 18)  (window-max-chars-per-line))
      (progn (setq ga144-node-size (1+ ga144-node-size))
             (ga144-render ga144-node-size))
    (message "Map cannot be made larger")))

(defun ga144-dec-node-size ()
  (interactive)
  (if (> ga144-node-size 3)
      (progn
        (setq ga144-node-size (1- ga144-node-size))
        (ga144-render ga144-node-size))
    (message "Map is cannot be made smaller")))

(defun ga144-move-left ()
  (interactive)
  (ga144-move-selected-node -1))

(defun ga144-move-right ()
  (interactive)
  (ga144-move-selected-node 1))

(defun ga144-move-up ()
  (interactive)
  (ga144-move-selected-node 100))

(defun ga144-move-down ()
  (interactive)
  (ga144-move-selected-node -100))

(defun ga144-move-right-end ()
  (interactive)
  (ga144-move-selected-node (- 17 (mod ga144-current-coord 100))))

(defun ga144-move-left-end ()
  (interactive)
  (ga144-move-selected-node (- (mod ga144-current-coord 100))))

(defun ga144-move-left-half ()
  (interactive)
  (ga144-move-selected-node (1- (/ (- (mod ga144-current-coord 100)) 2))))

(defun ga144-move-right-half ()
  (interactive)
  (ga144-move-selected-node (/ (- 17 (1- (mod ga144-current-coord 100))) 2)))

(defun ga144-move-top-half ()
  (interactive)
  (ga144-move-selected-node (* (/ (- 7 (1- (/ ga144-current-coord 100))) 2) 100)))

(defun ga144-move-bottom-half ()
  (interactive)
  (ga144-move-selected-node (- (* (1+ (/ (/ ga144-current-coord 100) 2)) 100))))

(defun ga144-move-top ()
  (interactive)
  (ga144-move-selected-node (* (- 7 (/ ga144-current-coord 100)) 100)))

(defun ga144-move-bottom ()
  (interactive)
  (ga144-move-selected-node (- (* (/ ga144-current-coord 100) 100))))


(defun ga144-valid-coord-p (coord)
  (and (>= coord 0)
       (< (mod coord 100) 18)
       (< (/ coord 100) 8)))

(defun ga144-set-selected-node (coord)
  (assert (ga144-valid-coord-p coord))
  (setq ga144-prev-coord ga144-current-coord
        ga144-current-coord coord)
  (update-position))

(defun ga144-move-selected-node (n)
  (let ((next (+ ga144-current-coord n)))
    (when (ga144-valid-coord-p next)
      (ga144-set-selected-node next))))

(defun move-selected-node-overlay (from to)
  (ga144-set-node-point-face from nil)
  (ga144-set-node-point-face to ga144-select-face))

(defun ga144-goto-current-node ()
  (interactive)
  (ga144-goto-node ga144-current-coord))

(defun ga144-goto-node (node) ;;TODO: test
  (if (ga144-valid-coord-p node)
      (let ((buffers ga144-project-aforth-buffers)
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


(defun ga144-select-aforth-source ()
  ;;select the aforth source file for the current ga144 project
  (interactive)
  (if (eq major-mode 'ga144-mode)
      (let ((f (read-file-name "Set GA144 source: ")))
        (if f
            (progn (setq ga144-project-aforth-file f)
                   (overlay-put ga144-project-aforth-file-overlay 'after-string (or ga144-project-aforth-file "None")))
          (message "GA144 aforth source not set")))

    (message "Not in a GA144 project %s" major-mode)))

(defun ga144-reset-region ()
  (dolist (coord ga144-region-nodes)
    (ga144-set-region-face coord 'remove)
    ;;(ga144-set-node-overlay node (ga144-node-face (coord->node node)))
    )
  (setq ga144-region-nodes nil)
  (clrhash ga144-visited-nodes))

(defun ga144-node-in-region-p (coord)
  (gethash coord ga144-visited-nodes))


(defun ga144-add-node-to-region (coord)
  (ga144-set-region-face coord)
  (push coord ga144-region-nodes)
  (puthash coord t ga144-visited-nodes))

;; the point node is part of the region (otherwise there is no way to select the whole map)
;; but it retains the normal point color instead of the region color
;; when the point moves it reverts back to the default color, reverting the

(defun ga144-update-path-selection ()
  (let ((i 0)
        dir diff coord m count quit)
    (setq diff (- (mod ga144-current-coord 100) (mod ga144-prev-coord 100))
          m 1)
    (when (= diff 0)
      (setq diff (- (/ ga144-current-coord 100)(/ ga144-prev-coord 100))
            m 100))
    (when diff
      (setq dir (* (if (> diff 0) 1 -1) m)
            count (abs diff))
      (setq coord ga144-prev-coord)
      (while (and (< i count)
                  (not quit))
        (setq i (1+ i))
        (setq coord (+ coord dir))

        (if (ga144-node-in-region-p coord)
            (progn (message "Error: Cannot cross path")
                   (setq quit coord))
          (ga144-add-node-to-region coord))
        ))))

(defun ga144-update-rectangle-selection ()
  ;; find all hte nodes
  ;; if not in visited add them
  )

(defun ga144-clear-selection ()
  )


(defun update-position ()
  ;;(setq ga144-modified-p t)
  ;;(ga144-move-to-node ga144-current-coord 'middle)
  (setq ga144-prev-coord (or ga144-prev-coord 0))
  (if ga144-mark-active
      (if ga144-region-path-p
          (ga144-update-path-selection)
        (ga144-update-rectangle-selection))
    (ga144-clear-selection))
  (move-selected-node-overlay ga144-prev-coord ga144-current-coord)
  (message "current coord: %s" ga144-current-coord))

(defun update-position ()
  ;;(setq ga144-modified-p t)
  ;;(ga144-move-to-node ga144-current-coord 'middle)
  (setq ga144-prev-coord (or ga144-prev-coord 0))
  (if ga144-mark-active
      (if (not ga144-region-path-p)
          (ga144-update-path-selection)
        (ga144-update-rectangle-selection))
    (ga144-clear-selection))
    (move-selected-node-overlay ga144-prev-coord ga144-current-coord)
  (message "current coord: %s" ga144-current-coord))

(defun ga144-draw-map-in-frame-limits()
  (let ((max-size (/ (window-max-chars-per-line) 18)))
    (if (> ga144-node-size max-size)
        ;; renders the map as large as possible but does not set ga144-node-size so the change is not persistent
        (ga144-render max-size)
      (ga144-render ga144-node-size)
      )))

(defun ga144-handle-window-size-change (frame)
  ;;TODO: fix, this needs to set the map buffer as current or local variables cannot be accessed
  (and ga144-auto-resize-map-on-window-change
       (ga144-draw-map-in-frame-limits)))

(setq ga144-current-focus-buffer nil) ;;buffer that is currently in focus
(setq ga144-maps nil);;maps buffer names to buffers

(defun ga144-set-map-focus (state)
  (if state
      (progn (dolist (coord ga144-region-nodes)
               (ga144-set-node-point-face coord nil))
             (ga144-set-node-point-face ga144-current-coord ga144-select-face))
    (progn (dolist (coord ga144-region-nodes)
             (ga144-set-node-point-face coord ga144-unfocused-face))
           (ga144-set-node-point-face ga144-current-coord ga144-unfocused-face))))

(defun ga144-set-map-buffer-focus (buffer focus)
  (with-current-buffer buffer
    (ga144-set-map-focus focus)))

(defun ga144-rescan-buffers-for-maps()
  ;; reconstruct the value for the variale `ga144-maps` in the case that it gets corrupted
  ;; this should not normally be needed. but is helpfull when ga144-maps get set to nil,
  ;; for example when eval-buffer is run
  (message "Something is wrong. re-scanning buffers for maps...")
  (let (maps)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'ga144-mode)
          (push (cons (buffer-name) buffer) maps))))
    maps))

(defun assoc-delete-all (key alist)
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

(setq ga144-updateing-map-focus nil)
(setq ga444-focus-hook-call-count 0)

(defun ga144-update-map-focus ()
  (setq ga444-focus-hook-call-count (1+ ga444-focus-hook-call-count))
  (if ga144-updateing-map-focus
      (message "already in update focus handler")
    (setq ga144-updateing-map-focus t)
    ;;(condition-case nil
    (progn
      ;; If another map was previously selected, remove focus
      (when ga144-current-focus-buffer
	(ga144-set-map-buffer-focus ga144-current-focus-buffer nil)
	(setq ga144-current-focus-buffer nil))
      ;;    (message "updating focus: major-mode='%s' buffer-name='%s'"
      ;;             major-mode (buffer-name))
      ;; if the current buffer is a ga144-map, add focus
      (when (eq major-mode 'ga144-mode)
	(unless ga144-maps
	  (setq ga144-maps (ga144-rescan-buffers-for-maps)))

	(let ((curr (cdr (assoc (buffer-name) ga144-maps))))

	  (unless (equal curr (current-buffer))
	    ;; The name of the current buffer maps to a different ga144 map buffer
	    ;; This would be a bug, no sure how to recover, just delete all mappings)
	    ;; for that buffer name and hope things will be alright.
	    (message "Bug: ga144 map buffer mismatch in ga144-update-map-focus")
	    (message " debug info: (current-buffer) = %s" (current-buffer))
	    (message " debug info: ga144-maps = %s" ga144-maps)
	    (setq ga144-maps (assoc-delete-all (buffer-name) ga144-maps)))

	  (when curr
	    (ga144-set-map-buffer-focus curr t)
	    (setq ga144-current-focus-buffer curr)))))
    ;;  (error (message "Error in ga144-update-map-focus")
    ;;         (setq ga144-updateing-map-focus nil))
    ;; )
    (setq ga144-updateing-map-focus nil)))


(defun ga144-kill-buffer-handler ()
  (when (eq (cdr (assoc (buffer-name) ga144-maps)) (current-buffer))
    (when (eq ga144-current-focus-buffer (current-buffer))
      (setq ga144-current-focus-buffer nil))

    (setq ga144-maps (assoc-delete-all (buffer-name) ga144-maps))
    (unless ga144-maps
      ;; There are no more maps so there is no need to track focus, and
      ;; if the hook is not removed we will end up iterating through all buffers with ga144-rescan-buffers-for-maps
      (remove-hook 'buffer-list-update-hook 'ga144-update-map-focus)
      )))

(defun ga144-set-mark ()
  (interactive)
  (ga144-reset-region)
  (if (and ga144-mark-coord
           (eq ga144-mark-coord ga144-current-coord))
      (progn (setq ga144-mark-active (not ga144-mark-active))
             (if ga144-mark-active
                 (message "GA144 mark activated")
               (message "GA144 mark deactivated")))
    (progn (setq ga144-mark-active t)
           (message "GA144 mark set")))
  (when ga144-mark-active
    (push ga144-current-coord ga144-region-nodes)
    (ga144-set-region-face ga144-current-coord))
  (setq ga144-mark-coord ga144-current-coord))

(defun ga144-exchange-point-and-mark ()
  (interactive)
  (let ((mark ga144-mark-coord))
    (setq ga144-mark-coord ga144-current-coord)
    (ga144-set-selected-node mark)))

(defun ga144-keyboard-quit ()
  "cancel the current operation"
  (interactive)
  (ga144-reset-region)
  (setq ga144-mark-active nil)
  (keyboard-quit))

(setq ga144-mode-map
      (let ((map (make-sparse-keymap 'ga144-mode-map)))
        (define-key map "+" 'ga144-inc-node-size)
        (define-key map "=" 'ga144-inc-node-size)
        (define-key map "-" 'ga144-dec-node-size)
        (define-key map (kbd "<up>") 'ga144-move-up)
        (define-key map (kbd "<down>") 'ga144-move-down)
        (define-key map (kbd "<left>") 'ga144-move-left)
        (define-key map (kbd "<right>") 'ga144-move-right)
        (define-key map (kbd "C-x C-s") 'ga144-save)
        (define-key map (kbd "C-e") 'ga144-move-right-end)
        (define-key map (kbd "C-a") 'ga144-move-left-end)
        (define-key map (kbd "C-b") 'ga144-move-left)
        (define-key map (kbd "M-b") 'ga144-move-left-half)
        (define-key map (kbd "C-f") 'ga144-move-right)
        (define-key map (kbd "M-f") 'ga144-move-right-half)
        (define-key map (kbd "C-p") 'ga144-move-up)
        (define-key map (kbd "M-p") 'ga144-move-top-half)
        (define-key map (kbd "C-n") 'ga144-move-down)
        (define-key map (kbd "M-n") 'ga144-move-bottom-half)
        (define-key map (kbd "M-<") 'ga144-move-top)
        (define-key map (kbd "M->") 'ga144-move-bottom)
        (define-key map (kbd "<return>") 'ga144-goto-current-node)
        (define-key map (kbd "C-c C-f") 'ga144-select-aforth-source)
        (define-key map (kbd "C-SPC") 'ga144-set-mark)
        (define-key map (kbd "C-x C-x") 'ga144-exchange-point-and-mark)
        (define-key map (kbd "C-g") 'ga144-keyboard-quit)

        map))

(define-derived-mode ga144-mode nil "GA144"
  "A major mode for programming the GA144."

  (use-local-map ga144-mode-map)
  (setq show-trailing-whitespace nil)

  (if (string-match "ga144$" buffer-file-name)
      (progn
        (setq ga144-project-file buffer-file-name)
        (setq ga144-project-name (file-name-base buffer-file-name))
        (assert ga144-project-name)
        (assert (not (string= ga144-project-name "nil")))
        (setq ga144-project-aforth-files (ga144-aforth-files (file-name-directory  buffer-file-name)))
        (setq ga144-project-aforth-buffers (mapcar 'ga144-get-project-file-buffer ga144-project-aforth-files))
        (setq ga144-project-aforth-file-overlay (make-overlay 0 0))
        (setq ga144-node-size ga144-default-node-size)
        (setq ga144-project-aforth-compile-status "Unknown")
        (setq ga144-project-aforth-compile-status-overlay (make-overlay 0 1))

        (let ((buffer-name (format "*GA144-%s*" ga144-project-name)))
          (when (get-buffer buffer-name)
            (kill-buffer buffer-name))
          (rename-buffer buffer-name)
          (push (cons buffer-name (current-buffer)) ga144-maps))

        (setq buffer-file-name nil
              ga144-nodes nil
              ga144-nodes-sans-overlays nil
              ga144-current-coord nil)
        (eval-buffer)

        (when ga144-nodes-sans-overlays
          (setq ga144-nodes (ga144-restore-node-overlays ga144-nodes-sans-overlays)))

        (if ga144-nodes
            (message "Loading GA144 project map...")
          (print "Creating new ga144 map..")
          (ga144-create-new))

        (ga144-draw-map-in-frame-limits)
        (setq truncate-lines t) ;; any line wrap will ruin the map
        (read-only-mode 1)
        (setq visible-cursor nil
              cursor-type nil
              ga144-region-nodes nil
              ga144-region-path-p nil
              ga144-visited-nodes (make-hash-table))
        (add-hook 'window-size-change-functions 'ga144-handle-window-size-change)
        (ga144-set-map-focus t)
        (add-hook 'buffer-list-update-hook 'ga144-update-map-focus)
        (add-hook 'kill-buffer-hook 'ga144-kill-buffer-handler)
        (ga144-move-selected-node ga144-current-coord)
        )
    (message "ga144-mode: invalid file format")))

(defun ga144-restore-node-overlays ( ga144-nodes )
  (let (o)
    (loop-nodes node
      (setq o (make-overlay 0 0))
      (overlay-put o 'face ga144-node-coord-face)
      (setf (ga144-node-coord-overlay node) o)))
  ga144-nodes)

(add-to-list 'auto-mode-alist '("\\.ga144$" . ga144-mode))
