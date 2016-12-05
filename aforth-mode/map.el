(setq ga144-map-buffer "*GA144-map*")
(setq ga144-default-node-size 3)
(setq ga144-node-size ga144-default-node-size)
(setq ga144-has-unsaved-changes nil) ;;how to save buffer-local


(defvar ga144-project-name nil)
(defvar ga144-project-file nil)
(defvar ga144-nodes nil)
(defvar ga144-current-coord nil)
(defvar ga144-prev-coord nil)
(defvar ga144-modified-p nil)
(defvar ga144-node-size nil)


(make-variable-buffer-local 'ga144-has-changes)
(make-variable-buffer-local 'ga144-project-name)
(make-variable-buffer-local 'ga144-project-file)
(make-variable-buffer-local 'ga144-nodes)
(make-variable-buffer-local 'ga144-current-coord)
(make-variable-buffer-local 'ga144-modified-p)
(make-variable-buffer-local 'ga144-node-size)
(make-variable-buffer-local 'ga144-prev-coord)

;;;file format:
;;;   <project-name>.ga144

(defun test ()
  (interactive)
  (insert "hello!"))

;;(defvar ga144-mode-map
(setq ga144-mode-map
      (let ((map (make-sparse-keymap 'ga144-mode-map)))
        (define-key map "+"		'ga144-inc-node-size)
        (define-key map "-"		'ga144-dec-node-size)
        (define-key map (kbd "<up>") 'ga144-move-up)
        (define-key map (kbd "C-p") 'ga144-move-up)
        (define-key map (kbd "<down>") 'ga144-move-down)
        (define-key map (kbd "C-n") 'ga144-move-down)
        (define-key map (kbd "<left>") 'ga144-move-left)
        (define-key map (kbd "C-b") 'ga144-move-left)
        (define-key map (kbd "<right>") 'ga144-move-right)
        (define-key map (kbd "C-f") 'ga144-move-right)
        (define-key map (kbd "C-x C-s") 'ga144-save)
        (define-key map (kbd "C-e") 'ga144-move-right-end)
        (define-key map (kbd "C-a") 'ga144-move-left-end)
        (define-key map (kbd "M-<") 'ga144-move-top)
        (define-key map (kbd "M-<") 'ga144-move-top)
        (define-key map (kbd "M->") 'ga144-move-bottom)

        map))

(defface ga144-coord-face '((((background light)) (:foreground "yellow"))
                            (((background dark)) (:foreground "yellow")))
  "Default face for GA144 node coordinates") ;;TODO: bold

(setq ga144-coord-face 'ga144-coord-face)

(setq ga144-font-lock-keywords
      `(("[0-9]\\{1,3\\}" . ga144-coord-face)
	))

(setq ga144-persistent-variables '(ga144-nodes-sans-overlays ga144-node-size ga144-current-coord))

(define-derived-mode ga144-mode nil "GA144"
  "A major mode for programming the GA144."

  (use-local-map ga144-mode-map)
  (setq show-trailing-whitespace nil)

  (if (string-match "ga144$" buffer-file-name)
      (progn
        (setq ga144-project-file buffer-file-name)
        (setq ga144-project-name (file-name-base buffer-file-name))
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
        (ga144-render)
        (read-only-mode 1)
        (setq visible-cursor nil
              cursor-type nil)
        (setq font-lock-defaults '((ga144-font-lock-keywords))))
    (message "ga144-mode: invalid file format")))

(defun ga144-render()
  (ga144-draw-map)
  (goto-char 1)
  (update-position))

(defun ga144-move-to-node (coord &optional middle)
  (goto-char 1)
  (let ((row (- 7 (coord->row coord)))
        (col (coord->col coord)))
    (next-line (+ (* row  ga144-node-size) (if middle (/ ga144-node-size 2) 0)))
    (forward-char (+ (* col ga144-node-size) (if middle (floor (/ ga144-node-size 2)) 0)))))


(defun ga144-draw-map ()
  (read-only-mode -1)
  (erase-buffer)
  (goto-char 1)
  (let (x coord s l o)
    (dotimes (row 8)
      (setq x (not x))
      (dotimes (row-height ga144-node-size)
        (dotimes (c 18)
          (setq x (not x))
          (dotimes (c-width ga144-node-size)
            (insert (if x "x" "="))))
        (insert "\n")))

    (loop-nodes node
      (setq coord (ga144-node-coord node))
      (ga144-move-to-node coord)
      (setq s (number-to-string coord)
            l (length s))
      (delete-char l)
      (insert s)
      (setq o (ga144-node-coord-overlay node))
      (move-overlay o (- (point) l) (point))
      (setf (ga144-node-coord-overlay node) o))
    (read-only-mode 1)
    (ga144-create-overlays)))

(defun test ()
  (interactive)
  (setq _x ga144-nodes))

(defface ga144-node-coord-face '((((background light)) (:foreground "yellow"))
                                 (((background dark)) (:foreground "yellow")))
  "ga144 face for node coordinate numbers")

(defface ga144-default-face-11 '((((background light)) (:foreground "green"))
                                 (((background dark)) (:foreground "green")))
  "default ga144 node face 1")

(defface ga144-default-face-22 '((((background light)) (:foreground "blue"))
                                 (((background dark)) (:foreground "blue")))
  "default ga144 node face 2")

(defface ga144-select-face '((((background light)) (:foreground "red"))
                             (((background dark)) (:foreground "red")))
  "default ga144 selected node face")

(setq ga144-node-coord-face 'ga144-node-coord-face)
(setq ga144-default-face-1 'ga144-default-face-11)
(setq ga144-default-face-2 'ga144-default-face-22)
(setq ga144-select-face 'ga144-select-face)

(defun ga144-delete-overlays ()
  (let (o overlays coord face column)
    (loop-nodes node
      (dolist (o (ga144-node-overlays node))
        (delete-overlay o))
      (setf (ga144-node-overlays node) nil))))

(defun ga144-create-overlays ()
  (ga144-delete-overlays)
  (loop-nodes node
    (setq coord (ga144-node-coord node)
          overlays nil)
    (ga144-move-to-node coord)
    (setq column (current-column)
          face (ga144-node-face node))
    (dotimes (i ga144-node-size)
      (setq o (make-overlay (point) (+ (point) ga144-node-size)))
      (overlay-put o 'face face)
      (push o overlays)
      (when (< i (- ga144-node-size 1))
        (next-line)
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

(defun coord->index (n)
  (+ (* (floor (/ n 100)) 18) (mod n 100)))

(defun index->coord (n)
  (+ (* (floor (/ n 18)) 100) (mod n 18)))

(defun coord->row (coord)
  (floor (/ coord 100)))

(defun coord->col (coord)
  (mod coord 100))

(defun coord->node (coord)
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
          (insert (format "(setq %s %s)\n" (car v) (cdr v))))
        ;;(insert (format "(setq %s " v))
        ;;(print (eval v) (current-buffer))
        ;;(insert ")\n")
        )))
  (message "saved in %s" ga144-project-file)
  (setq ga144-modified-p nil))

(defun ga144-inc-node-size ()
  (interactive)
  (setq ga144-node-size (1+ ga144-node-size))
  (ga144-render))

(defun ga144-dec-node-size ()
  (interactive)
  (setq ga144-node-size (1- ga144-node-size))
  (ga144-render))

(defun create-ga144-map (&optional size)
  (let ((size (or size ga144-default-node-size))
        (x nil))
    (switch-to-buffer (get-buffer-create ga144-map-buffer))
    (erase-buffer)
    (goto-char 1)

    (dotimes (row 8)
      (setq x (not x))
      (dotimes (row-height size)
        (dotimes (c 18)
          (setq x (not x))
          (dotimes (c-width size)
            (insert (if x "x" "="))))
        (insert "\n")))))

(defun ga144 ()
  (interactive)
  (unless (get-buffer ga144-map-buffer)
    (setq ga144-node-size ga144-default-node-size)
    (create-ga144-map))
  (switch-to-buffer ga144-map-buffer))

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

(defun ga144-move-top ()
  (interactive)
  (ga144-move-selected-node (* (- 7 (/ ga144-current-coord 100)) 100)))

(defun ga144-move-bottom ()
  (interactive)
  (ga144-move-selected-node (- (* (/ ga144-current-coord 100) 100))))


(defun ga144-move-selected-node (n)
  (setq ga144-prev-coord ga144-current-coord
        ga144-current-coord (+ ga144-current-coord n))
  (update-position))

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

(defun update-position ()
  ;;(setq ga144-modified-p t)
  ;;(ga144-move-to-node ga144-current-coord 'middle)
  (setq ga144-prev-coord (or ga144-prev-coord 0))
  (move-selected-node-overlay ga144-prev-coord ga144-current-coord)
  (message "current coord: %s" ga144-current-coord))

(defun goto-node ()
  "jump to the source of the given node"
  )

(add-to-list 'auto-mode-alist '("\\.ga144$" . ga144-mode))
