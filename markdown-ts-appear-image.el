;;; markdown-ts-appear-image.el --- Internal sliced image previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; Author: Thysrael <thysrael@163.com>
;; Keywords: text, convenience
;; URL: https://github.com/Thysrael/markdown-ts-appear
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Local block images use shared image descriptors and line-sized display
;; slices.  Source text is never replaced or padded in the buffer.  A selected
;; slice is a window-local visual position, not a fabricated source position.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'markdown-ts-mode)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(declare-function markdown-ts-appear--active-p "markdown-ts-appear")
(declare-function markdown-ts-appear--node-ancestor "markdown-ts-appear")
(declare-function markdown-ts-appear--literal-block-at "markdown-ts-appear")
(declare-function markdown-ts-appear--markdown-node-at "markdown-ts-appear")
(declare-function markdown-ts-appear--region-visible-p "markdown-ts-appear")
(declare-function markdown-ts-appear--update "markdown-ts-appear")

(defvar markdown-ts-appear-table-style)

(cl-defstruct (markdown-ts-appear-image--view
               (:constructor markdown-ts-appear-image--make-view))
  "Window-specific slices of OWNER, all sharing IMAGE."
  owner window overlay image key slices text index (top 0) rows)

(defvar-local markdown-ts-appear-image--objects nil
  "Source anchor overlays for discovered local block images.")
(defvar-local markdown-ts-appear-image--assets nil
  "File fingerprints and cached image data, indexed by absolute filename.")
(defvar-local markdown-ts-appear-image--tick nil
  "Text modification tick at the last image validation.")
(defvar-local markdown-ts-appear-image--cursor nil
  "Image view whose visual slice currently holds the selected cursor.")

(defvar markdown-ts-appear-image--map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] #'ignore)
    (define-key map [mouse-1] #'markdown-ts-appear-image--mouse-select)
    map)
  "Mouse bindings for selecting a displayed image slice.")

(defun markdown-ts-appear-image--windows ()
  "Return graphical windows displaying the current buffer."
  (seq-filter (lambda (window) (display-images-p (window-frame window)))
              (get-buffer-window-list (current-buffer) nil t)))

(defun markdown-ts-appear-image--file (node)
  "Return a local filename for standalone image NODE, or nil."
  (when-let* (((not (and (eq markdown-ts-appear-table-style 'wrapped)
                         (markdown-ts-appear--node-ancestor
                          (markdown-ts-appear--markdown-node-at (treesit-node-start node))
                          "pipe_table"))))
              (dest (treesit-search-subtree node "\\`link_destination\\'"))
              (url (treesit-node-text dest t)))
    (save-excursion
      (goto-char (treesit-node-start node))
      (when (and (string-match-p
                  "\\`[> \t]*\\(?:[-+*] \\|[0-9]+[.)] \\)?\\'"
                  (buffer-substring-no-properties (line-beginning-position) (point)))
                 (progn (goto-char (treesit-node-end node))
                        (skip-chars-forward " \t") (eolp)))
        (setq url (string-remove-suffix ">" (string-remove-prefix "<" url)))
        (unless (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*:" url)
          (let ((file (expand-file-name (url-unhex-string url) default-directory)))
            (unless (file-remote-p file) file)))))))

(defun markdown-ts-appear-image--asset (file)
  "Return a cached local image for FILE, refreshing changed file contents."
  (unless markdown-ts-appear-image--assets
    (setq markdown-ts-appear-image--assets (make-hash-table :test #'equal)))
  (let* ((attributes (condition-case nil (file-attributes file) (file-error nil)))
         (stamp (and attributes (list (file-attribute-modification-time attributes)
                                      (file-attribute-size attributes))))
         (cached (gethash file markdown-ts-appear-image--assets)))
    (if (and cached (equal stamp (car cached)))
        (cdr cached)
      (let ((image
             (when (and attributes (file-regular-p file) (file-readable-p file)
                        (image-supported-file-p file))
               (condition-case nil
                   (with-temp-buffer
                     (set-buffer-multibyte nil)
                     (insert-file-contents-literally file)
                      (when-let* ((image (create-image (buffer-string) nil t :scale 1 :ascent 'center))
                                  (size (image-size image t))
                                  ((and (> (car size) 0) (> (cdr size) 0))))
                        image))
                 (error nil)))))
        (puthash file (cons stamp image) markdown-ts-appear-image--assets)
        image))))

(defun markdown-ts-appear-image--visible-p (owner)
  "Return non-nil when OWNER's image should be displayed."
  (and markdown-ts-inline-images (markdown-ts-appear--active-p)
       (overlay-buffer owner)
       (not (markdown-ts-appear--region-visible-p (overlay-start owner) (overlay-end owner)))
       (not (and (memq #'markdown-ts-appear--update post-command-hook)
                 (<= (overlay-start owner) (point)) (< (point) (overlay-end owner))))
       (not (markdown-ts--outline-invisible-p (overlay-start owner)))))

(defun markdown-ts-appear-image--present (view)
  "Show VIEW's cached slices, or hide them when its source is revealed."
  (let* ((overlay (markdown-ts-appear-image--view-overlay view))
         (index (markdown-ts-appear-image--view-index view))
         (state (if (markdown-ts-appear-image--visible-p
                     (markdown-ts-appear-image--view-owner view))
                    (or index 'passive) 'hidden)))
    (unless (equal state (overlay-get overlay 'markdown-ts-appear-image--state))
      (let* ((text (markdown-ts-appear-image--view-text view))
             (count (length (markdown-ts-appear-image--view-slices view)))
             (rows (markdown-ts-appear-image--view-rows view))
             (top (markdown-ts-appear-image--view-top view))
             (offset (and (integerp state) (1+ (* 2 state)))))
        (when offset
          (setq top (max 0 (min top index)))
          (when (>= index (+ top rows)) (setq top (1+ (- index rows)))))
        (setq top (min top (max 0 (- count rows))))
        (setf (markdown-ts-appear-image--view-top view) top)
        (let* ((start (1+ (* 2 top)))
               (end (* 2 (min count (+ top rows))))
               (prefix
                (if (< rows count)
                    (let* ((digits (length (number-to-string count)))
                           (label (format (format " [%%%dd-%%%dd/%d]\n" digits digits count)
                                          (1+ top) (min count (+ top rows)))))
                      (if (< (length label)
                             (window-body-width (markdown-ts-appear-image--view-window view)))
                          (propertize label 'face 'shadow)
                        "\n"))
                  "\n")))
          ;; A before-string on hidden Markdown punctuation is itself skipped.
          ;; The non-hidden category overrides that punctuation while the image
          ;; replaces it; the source's original invisibility is never modified.
          (overlay-put overlay 'invisible (and offset 'markdown-ts-appear-image--anchor))
          (overlay-put overlay 'before-string
                       (and offset (concat prefix (substring text start offset))))
          (overlay-put overlay 'display
                       (and offset (aref (markdown-ts-appear-image--view-slices view) index)))
          (overlay-put overlay 'after-string
                       (cond (offset (substring text (1+ offset) end))
                             ((eq state 'passive) (concat prefix (substring text start end)))))
          (overlay-put overlay 'markdown-ts-appear-image--state state))))))

(defun markdown-ts-appear-image--clear-cursor ()
  "Restore the selected image to its passive display."
  (when-let* ((view markdown-ts-appear-image--cursor))
    (setq markdown-ts-appear-image--cursor nil)
    (setf (markdown-ts-appear-image--view-index view) nil)
    (when (overlay-buffer (markdown-ts-appear-image--view-overlay view))
      (markdown-ts-appear-image--present view))))

(defun markdown-ts-appear-image--delete (owner)
  "Delete OWNER and its window-specific display overlays."
  (dolist (view (overlay-get owner 'markdown-ts-appear-image--views))
    (when (eq view markdown-ts-appear-image--cursor)
      (setq markdown-ts-appear-image--cursor nil))
    (delete-overlay (markdown-ts-appear-image--view-overlay view)))
  (delete-overlay owner)
  (setq markdown-ts-appear-image--objects (delq owner markdown-ts-appear-image--objects)))

(defun markdown-ts-appear-image--layout (view base)
  "Build VIEW's slices from BASE only when its size or font height changes."
  (let* ((window (markdown-ts-appear-image--view-window view))
         (width (max 1 (if (numberp markdown-ts-image-max-width)
                          markdown-ts-image-max-width (window-body-width window t))))
         (height (max 1 (window-font-height window)))
         (body-height (window-body-height window t))
         (key (list base width height body-height (frame-parameter (window-frame window) 'font))))
    (unless (equal key (markdown-ts-appear-image--view-key view))
      (let* ((image (cons 'image (plist-put (copy-sequence (cdr base)) :max-width width)))
             (size (image-size image t (window-frame window)))
             (w (ceiling (car size))) (h (ceiling (cdr size)))
             (overlay (markdown-ts-appear-image--view-overlay view))
             (old-count (length (markdown-ts-appear-image--view-slices view)))
             (old-index (markdown-ts-appear-image--view-index view))
             pieces slices)
        (cl-loop with rows = (max 1 (/ h height))
                 for index below rows
                 for y = (/ (* index h) rows)
                 for next-y = (/ (* (1+ index) h) rows)
                 for slice = `((slice 0 ,y ,w ,(- next-y y)) ,image)
                 do (push slice slices)
                  do (push (propertize " " 'display slice 'face 'default
                                      'keymap markdown-ts-appear-image--map
                                      'markdown-ts-appear-image--view view
                                      'markdown-ts-appear-image--slice index)
                          pieces))
        (setq slices (vconcat (nreverse slices)))
        ;; Keep the active before-string comfortably inside a window.  An
        ;; unbounded one can trigger native bidi iterator assertions when
        ;; redisplay tries to scroll to its source anchor (Emacs 32).
        (setf (markdown-ts-appear-image--view-image view) image
              (markdown-ts-appear-image--view-key view) key
              (markdown-ts-appear-image--view-slices view) slices
              (markdown-ts-appear-image--view-rows view)
              (min (length slices) (max 1 (/ body-height 2 (ceiling h (length slices)))))
              (markdown-ts-appear-image--view-text view)
              (concat "\n" (mapconcat #'identity (nreverse pieces)
                                       (propertize "\n" 'line-height t 'line-spacing 0
                                                   'face 'default))))
        (when (> old-count 0)
          (setf (markdown-ts-appear-image--view-top view)
                (/ (* (markdown-ts-appear-image--view-top view) (length slices)) old-count)))
        (when old-index
          (setf (markdown-ts-appear-image--view-index view)
                (min (/ (* old-index (length slices)) old-count) (1- (length slices)))))
        (overlay-put overlay 'markdown-ts-appear-image--state 'uninitialized)))))

(defun markdown-ts-appear-image--sync (owner)
  "Synchronize OWNER's cached previews with visibility and graphical windows."
  (let ((windows (markdown-ts-appear-image--windows))
        (base (overlay-get owner 'markdown-ts-appear-image--image))
        views)
    (dolist (view (overlay-get owner 'markdown-ts-appear-image--views))
      (if (memq (markdown-ts-appear-image--view-window view) windows)
          (push view views)
        (when (eq view markdown-ts-appear-image--cursor)
          (setq markdown-ts-appear-image--cursor nil))
        (delete-overlay (markdown-ts-appear-image--view-overlay view))))
    (when (and (markdown-ts-appear-image--visible-p owner) windows)
      (unless base
        (setq base (markdown-ts-appear-image--asset
                    (overlay-get owner 'markdown-ts-appear-image--file)))
        (overlay-put owner 'markdown-ts-appear-image--image base))
      (when base
        (dolist (window windows)
          (let ((view (seq-find (lambda (candidate)
                                 (eq window (markdown-ts-appear-image--view-window candidate)))
                               views)))
            (unless view
              (let* ((end (overlay-end owner))
                     (overlay (make-overlay (1- end) end nil t nil)))
                (overlay-put overlay 'window window)
                (overlay-put overlay 'priority '(nil . 2))
                (overlay-put overlay 'face 'default)
                (overlay-put overlay 'keymap markdown-ts-appear-image--map)
                (overlay-put overlay 'help-echo (overlay-get owner 'markdown-ts-appear-image--file))
                (setq view (markdown-ts-appear-image--make-view
                            :owner owner :window window :overlay overlay))
                (overlay-put overlay 'markdown-ts-appear-image--view view)
                (push view views)
                (overlay-put owner 'markdown-ts-appear-image--views views)))
            (markdown-ts-appear-image--layout view base)))))
    (overlay-put owner 'markdown-ts-appear-image--views views)
    (dolist (view views) (markdown-ts-appear-image--present view))))

(defun markdown-ts-appear-image--fontify (node file)
  "Maintain a sliced local image for NODE referring to FILE."
  (let* ((beg (treesit-node-start node)) (end (treesit-node-end node))
         (source (treesit-node-text node t))
         (owner (seq-find (lambda (overlay)
                            (and (overlay-get overlay 'markdown-ts-appear-image--source)
                                 (= beg (overlay-start overlay)) (= end (overlay-end overlay))))
                          (overlays-in beg end))))
    (when (and owner
               (not (and (equal source (overlay-get owner 'markdown-ts-appear-image--source))
                         (equal file (overlay-get owner 'markdown-ts-appear-image--file)))))
      (markdown-ts-appear-image--delete owner)
      (setq owner nil))
    (unless owner
      (setq owner (make-overlay beg end nil t nil))
      (overlay-put owner 'markdown-ts-appear-image--source source)
      (overlay-put owner 'markdown-ts-appear-image--file file)
      (push owner markdown-ts-appear-image--objects))
    (when (and (markdown-ts-appear-image--visible-p owner)
               (markdown-ts-appear-image--windows))
      (let ((image (markdown-ts-appear-image--asset file)))
        (unless (eq image (overlay-get owner 'markdown-ts-appear-image--image))
          (dolist (view (overlay-get owner 'markdown-ts-appear-image--views))
            (when (eq view markdown-ts-appear-image--cursor)
              (setq markdown-ts-appear-image--cursor nil))
            (delete-overlay (markdown-ts-appear-image--view-overlay view)))
          (overlay-put owner 'markdown-ts-appear-image--views nil)
          (overlay-put owner 'markdown-ts-appear-image--image image))))
    (markdown-ts-appear-image--sync owner)))

(defun markdown-ts-appear-image--update ()
  "Update visibility after an explicit source-tracking state change."
  (when (and markdown-ts-appear-image--cursor
             (not (markdown-ts-appear-image--visible-p
                   (markdown-ts-appear-image--view-owner markdown-ts-appear-image--cursor))))
    (markdown-ts-appear-image--clear-cursor))
  (dolist (owner markdown-ts-appear-image--objects)
    (when (overlay-buffer owner) (markdown-ts-appear-image--sync owner))))

(defun markdown-ts-appear-image--before-change (beg end)
  "Discard previews whose source is edited between BEG and END."
  (dolist (owner markdown-ts-appear-image--objects)
    (when (and (overlay-buffer owner) (< beg (overlay-end owner))
               (> end (overlay-start owner)))
      (markdown-ts-appear-image--delete owner))))

(defun markdown-ts-appear-image--post-command ()
  "Validate edited source and maintain the selected visual image position."
  (unless markdown-ts-inline-images
    (dolist (owner markdown-ts-appear-image--objects)
      (markdown-ts-appear-image--delete owner)))
  (unless (equal markdown-ts-appear-image--tick (buffer-chars-modified-tick))
    (dolist (owner markdown-ts-appear-image--objects)
      (unless (and (overlay-buffer owner)
                   (save-restriction
                     (widen)
                     (let ((beg (overlay-start owner)) (end (overlay-end owner)))
                       (treesit-update-ranges beg end)
                       (when-let* ((node (markdown-ts-appear--node-ancestor
                                         (treesit-node-at beg 'markdown-inline) "image")))
                         (and (= beg (treesit-node-start node)) (= end (treesit-node-end node))
                              (not (markdown-ts-appear--literal-block-at beg))
                              (equal (markdown-ts-appear-image--file node)
                                     (overlay-get owner 'markdown-ts-appear-image--file))
                              (equal (treesit-node-text node t)
                                     (overlay-get owner 'markdown-ts-appear-image--source)))))))
        (markdown-ts-appear-image--delete owner)))
    (setq markdown-ts-appear-image--tick (buffer-chars-modified-tick))
    (markdown-ts-appear-image--update))
  (when-let* ((view markdown-ts-appear-image--cursor))
    (if (and (eq (selected-window) (markdown-ts-appear-image--view-window view))
             (eq (window-buffer (selected-window)) (current-buffer))
             (overlay-buffer (markdown-ts-appear-image--view-owner view))
             (= (point) (1- (overlay-end (markdown-ts-appear-image--view-owner view))))
             (markdown-ts-appear-image--visible-p (markdown-ts-appear-image--view-owner view)))
        (setq disable-point-adjustment t)
      (markdown-ts-appear-image--clear-cursor))))

(defun markdown-ts-appear-image--view-at-point ()
  "Return the selected window's visible block image on the source line."
  (when-let* ((owner (seq-find
                     (lambda (overlay) (overlay-get overlay 'markdown-ts-appear-image--source))
                     (overlays-in (line-beginning-position)
                                  (min (point-max) (1+ (line-end-position)))))))
    (when (markdown-ts-appear-image--visible-p owner)
      (seq-find (lambda (view) (eq (selected-window) (markdown-ts-appear-image--view-window view)))
                (overlay-get owner 'markdown-ts-appear-image--views)))))

(defun markdown-ts-appear-image--select (view index)
  "Place the visual cursor on VIEW's slice INDEX without altering source."
  (unless (eq view markdown-ts-appear-image--cursor)
    (markdown-ts-appear-image--clear-cursor))
  (setq markdown-ts-appear-image--cursor view)
  (setf (markdown-ts-appear-image--view-index view) index)
  (goto-char (1- (overlay-end (markdown-ts-appear-image--view-owner view))))
  (markdown-ts-appear-image--present view)
  (setq disable-point-adjustment t)
  t)

(defun markdown-ts-appear-image--mouse-select (event)
  "Select the image slice clicked by mouse EVENT."
  (interactive "e")
  (let* ((position (event-start event))
         (window (posn-window position))
         (string (posn-string position))
         (view (if string
                   (get-text-property (cdr string) 'markdown-ts-appear-image--view (car string))
                 (get-char-property (posn-point position) 'markdown-ts-appear-image--view window)))
         (index (and string (get-text-property
                             (cdr string) 'markdown-ts-appear-image--slice (car string)))))
    (when (and view (window-live-p window)
               (overlay-buffer (markdown-ts-appear-image--view-overlay view)))
      (select-window window)
      (with-current-buffer (window-buffer window)
        (markdown-ts-appear-image--select
         view (or index (markdown-ts-appear-image--view-index view) 0))))))

(defun markdown-ts-appear-image--step (function step noerror rest)
  "Move one image-aware visual STEP, using FUNCTION for ordinary text.
NOERROR and REST are the native line-motion arguments."
  (let* ((view (markdown-ts-appear-image--view-at-point))
         (index (and view (eq view markdown-ts-appear-image--cursor)
                     (markdown-ts-appear-image--view-index view))))
    (cond
     ((and view index
           (>= (+ index step) 0)
           (< (+ index step) (length (markdown-ts-appear-image--view-slices view))))
      (markdown-ts-appear-image--select view (+ index step)))
     ((and view (null index) (> step 0))
      (markdown-ts-appear-image--select view 0))
     ((and view index)
      (let ((owner (markdown-ts-appear-image--view-owner view)))
        (if (and (> step 0) (= (line-end-position) (point-max)))
            (unless noerror (signal 'end-of-buffer nil))
          (markdown-ts-appear-image--clear-cursor)
          (goto-char (overlay-start owner))
          (when (> step 0) (forward-line 1))
          (let ((last-command 'next-line)) (apply function 0 noerror rest))
          t)))
     (t
      (let ((result (apply function step noerror rest)))
        (when (and result (< step 0))
          (when-let* ((target (markdown-ts-appear-image--view-at-point))
                      ((not (eq target view))))
            (markdown-ts-appear-image--select
             target (1- (length (markdown-ts-appear-image--view-slices target))))))
        result)))))

(defun markdown-ts-appear-image--line-move (function count &optional noerror &rest rest)
  "Move COUNT visual lines through sliced images, falling back to FUNCTION.
NOERROR and REST preserve native line-motion options."
  (if (not (and markdown-ts-inline-images markdown-ts-appear-image--objects
                (/= count 0) line-move-visual (markdown-ts-appear--active-p)
                (display-images-p)
                (eq (window-buffer (selected-window)) (current-buffer))
                (not (memq #'markdown-ts-appear--update post-command-hook))))
      (apply function count noerror rest)
    (let ((complete t) (last-command last-command))
      (unless (or goal-column (memq last-command '(next-line previous-line)))
        (setq temporary-goal-column (current-column)))
      (catch 'boundary
        (dotimes (index (abs count))
          (when (> index 0) (setq last-command 'next-line))
          (unless (markdown-ts-appear-image--step function (if (< count 0) -1 1) noerror rest)
            (setq complete nil)
            (throw 'boundary nil))))
      complete)))

(defun markdown-ts-appear-image--setup ()
  "Install the image lifecycle, using the native inline-image option."
  (add-hook 'post-command-hook #'markdown-ts-appear-image--post-command 30 t)
  (add-hook 'before-change-functions #'markdown-ts-appear-image--before-change nil t)
  (add-hook 'window-configuration-change-hook #'markdown-ts-appear-image--update nil t)
  (with-suppressed-warnings ((obsolete outline-view-change-hook))
    (add-hook 'outline-view-change-hook #'markdown-ts-appear-image--update nil t))
  (when markdown-ts-inline-images
    (treesit-font-lock-recompute-features '(image-preview))
    (font-lock-flush)))

(defun markdown-ts-appear-image--detach ()
  "Detach inherited image state without modifying another buffer's overlays."
  (remove-hook 'post-command-hook #'markdown-ts-appear-image--post-command t)
  (remove-hook 'before-change-functions #'markdown-ts-appear-image--before-change t)
  (remove-hook 'window-configuration-change-hook #'markdown-ts-appear-image--update t)
  (with-suppressed-warnings ((obsolete outline-view-change-hook))
    (remove-hook 'outline-view-change-hook #'markdown-ts-appear-image--update t))
  (setq markdown-ts-appear-image--objects nil markdown-ts-appear-image--assets nil
        markdown-ts-appear-image--cursor nil markdown-ts-appear-image--tick nil))

(defun markdown-ts-appear-image--teardown ()
  "Remove owned previews and image lifecycle hooks."
  (dolist (owner markdown-ts-appear-image--objects) (markdown-ts-appear-image--delete owner))
  (markdown-ts-appear-image--detach))

(provide 'markdown-ts-appear-image)
;;; markdown-ts-appear-image.el ends here
