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

(setq ga144-node-coord-face 'ga144-node-coord-face)
(setq ga144-default-face-1 'ga144-default-face-1)
(setq ga144-default-face-2 'ga144-default-face-2)
(setq ga144-select-face 'ga144-select-face)


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
    (overlay-put ga144-project-aforth-compile-status-overlay 'after-string  ga144-project-aforth-compile-status)
    ))

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
          face (ga144-node-face node))
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

(defun ga144-update-overlays ()
  (let (face)
    (loop-nodes node
      (setq ga144-node-face node)
      (dolist (o (ga144-node-overlays node))
        (overlay-put o 'face face)))))

(defstruct ga144-node coord special-function node-type text color overlays face coord-overlay)

(defun ga144-valid-node-index-p(n)
  (and (>= n 0) (< n 144)))

(defun coord->index (n)
  (assert (ga144-valid-node-index-p n))
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

(defun ga144-get-node-default-face (coord)
  (let ((a (= (mod (/ coord 100) 2) 0))
        (b (= (mod (mod coord 100) 2) 0)))
    (if (eq a b)
        ga144-default-face-1
      ga144-default-face-2)))

(defun ga144-create-new ()
  (let (coord coord-overlay)
    (setq ga144-nodes (make-vector 144 nil))
    (dotimes (i 144)
      (setq coord (index->coord i))
      (setq coord-overlay (make-overlay 0 0 ))
      (overlay-put coord-overlay 'face ga144-node-coord-face)
      (aset ga144-nodes i (make-ga144-node :coord coord
                                           :special-function (ga144-get-node-type coord)
                                           :face (ga144-get-node-default-face coord)
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

(defun ga144-move-selected-node (n)
  (let ((next (+ ga144-current-coord n)))
    (when (ga144-valid-coord-p next)
      (setq ga144-prev-coord ga144-current-coord
            ga144-current-coord next)
      (update-position))))

(defun move-selected-node-overlay (from to)
  (let ((node-from (coord->node from))
        (node-to (coord->node to))
        face)

    (setq face (ga144-node-face node-from))
    (dolist (o (ga144-node-overlays node-from))
      (overlay-put o 'face face))

    (dolist (o (ga144-node-overlays node-to))
      (overlay-put o 'face ga144-select-face))
    ))

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

(defun update-position ()
  ;;(setq ga144-modified-p t)
  ;;(ga144-move-to-node ga144-current-coord 'middle)
  (setq ga144-prev-coord (or ga144-prev-coord 0))
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
        map))

(define-derived-mode ga144-mode nil "GA144"
  "A major mode for programming the GA144."

  (use-local-map ga144-mode-map)
  (setq show-trailing-whitespace nil)

  (if (string-match "ga144$" buffer-file-name)
      (progn
        (setq ga144-project-file buffer-file-name)
        (setq ga144-project-name (file-name-base buffer-file-name))
        (setq ga144-project-aforth-files (ga144-aforth-files (file-name-directory  buffer-file-name)))
        (setq ga144-project-aforth-buffers (mapcar 'ga144-get-project-file-buffer ga144-project-aforth-files))
        (setq ga144-project-aforth-file-overlay (make-overlay 0 0))
        (setq ga144-node-size ga144-default-node-size)
        (setq ga144-project-aforth-compile-status "Unknown")
        (setq ga144-project-aforth-compile-status-overlay (make-overlay 0 1))

        (let ((buffer-name (format "*GA144-%s*" ga144-project-name)))
          (when (get-buffer buffer-name)
            (kill-buffer buffer-name))
          (rename-buffer buffer-name))
        (setq buffer-file-name nil
              ga144-nodes nil
              ga144-current-coord nil)
        (eval-buffer)
        (unless ga144-nodes
          (ga144-create-new))
        (message "Loading GA144 project map...")
        (ga144-draw-map-in-frame-limits)
        (setq truncate-lines t) ;; any line wrap will ruin the map
        (read-only-mode 1)
        (setq visible-cursor nil
              cursor-type nil)
        (add-hook 'window-size-change-functions 'ga144-handle-window-size-change)
        )
    (message "ga144-mode: invalid file format")))


(add-to-list 'auto-mode-alist '("\\.ga144$" . ga144-mode))
