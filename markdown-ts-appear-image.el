;;; markdown-ts-appear-image.el --- Internal whole-image previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; Author: Thysrael <thysrael@163.com>
;; Keywords: text, convenience
;; URL: https://github.com/Thysrael/markdown-ts-appear
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Local block images are displayed whole and scaled to the available window.
;; Source text is never replaced or padded, and ordinary line motion keeps its
;; native meaning.  Window-edge clipping scrolls the document, not the image.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'markdown-ts-mode)
(require 'pixel-scroll)
(require 'seq)
(require 'subr-x)
(require 'url-util)

(declare-function image-size "image.c" (spec &optional pixels frame))
(declare-function markdown-ts-appear--active-p "markdown-ts-appear")
(declare-function markdown-ts-appear--node-ancestor "markdown-ts-appear")
(declare-function markdown-ts-appear--literal-block-at "markdown-ts-appear")
(declare-function markdown-ts-appear--markdown-node-at "markdown-ts-appear")
(declare-function markdown-ts-appear--region-visible-p "markdown-ts-appear")
(declare-function markdown-ts-appear--update "markdown-ts-appear")

(defvar markdown-ts-appear-table-style)

(cl-defstruct (markdown-ts-appear-image--view
               (:constructor markdown-ts-appear-image--make-view))
  "Window-specific whole IMAGE for OWNER."
  owner window overlay picture image key size)

(defvar-local markdown-ts-appear-image--objects nil
  "Source anchor overlays for discovered local block images.")
(defvar-local markdown-ts-appear-image--assets nil
  "File fingerprints and cached image data, indexed by absolute filename.")
(defvar-local markdown-ts-appear-image--tick nil
  "Text modification tick at the last image validation.")

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
  "Show VIEW's whole image, or hide it when its source is revealed."
  (let* ((overlay (markdown-ts-appear-image--view-overlay view))
         (picture (markdown-ts-appear-image--view-picture view))
         (visible (markdown-ts-appear-image--visible-p
                   (markdown-ts-appear-image--view-owner view))))
    (unless (eq visible (overlay-get overlay 'markdown-ts-appear-image--state))
      (unless visible (markdown-ts-appear-image--reset-scroll view))
      (dolist (part (list overlay picture))
        (overlay-put part 'invisible (and visible 'markdown-ts-appear-image--anchor)))
      (overlay-put overlay 'display (and visible "\n"))
      (overlay-put picture 'display (and visible (markdown-ts-appear-image--view-image view)))
      (overlay-put overlay 'markdown-ts-appear-image--state visible))))

(defun markdown-ts-appear-image--delete-view (view)
  "Delete VIEW's separator and whole-image overlays."
  (markdown-ts-appear-image--reset-scroll view)
  (delete-overlay (markdown-ts-appear-image--view-overlay view))
  (delete-overlay (markdown-ts-appear-image--view-picture view)))

(defun markdown-ts-appear-image--reset-scroll (view)
  "Discard a window offset that belongs to VIEW before hiding its image."
  (let ((window (markdown-ts-appear-image--view-window view))
        (picture (markdown-ts-appear-image--view-picture view)))
    (when (and (window-live-p window) (overlay-buffer picture)
               (eq (window-buffer window) (overlay-buffer picture))
               (= (window-start window) (overlay-start picture)))
      (set-window-start window (overlay-start (markdown-ts-appear-image--view-owner view)) t)
      (set-window-vscroll window 0 t))))

(defun markdown-ts-appear-image--make-overlay (view position)
  "Make a window-local display overlay for VIEW at POSITION."
  (let ((overlay (make-overlay position (1+ position) nil t nil)))
    (dolist (property '(window priority face help-echo markdown-ts-appear-image--view))
      (overlay-put overlay property (overlay-get (markdown-ts-appear-image--view-overlay view) property)))
    overlay))

(defun markdown-ts-appear-image--delete (owner)
  "Delete OWNER and its window-specific display overlays."
  (dolist (view (overlay-get owner 'markdown-ts-appear-image--views))
    (markdown-ts-appear-image--delete-view view))
  (delete-overlay owner)
  (setq markdown-ts-appear-image--objects (delq owner markdown-ts-appear-image--objects)))

(defun markdown-ts-appear-image--layout (view base)
  "Fit the whole BASE image to VIEW's available width and height."
  (let* ((window (markdown-ts-appear-image--view-window view))
         (width (max 1 (min (window-body-width window t)
                           (or markdown-ts-image-max-width (window-body-width window t)))))
         (line-height (max 1 (window-font-height window)))
         (margin (min scroll-margin (floor (* maximum-scroll-margin (window-body-height window)))))
         (height (max line-height (- (window-body-height window t) (* (+ 2 (* 2 margin)) line-height))))
         (key (list base width height (frame-parameter (window-frame window) 'font))))
    (unless (equal key (markdown-ts-appear-image--view-key view))
      (let ((image (cons 'image (append (list :max-width width :max-height height)
                                      (copy-sequence (cdr base))))))
        (setf (markdown-ts-appear-image--view-image view) image
              (markdown-ts-appear-image--view-key view) key
              (markdown-ts-appear-image--view-size view) (image-size image t (window-frame window)))
        (overlay-put (markdown-ts-appear-image--view-overlay view)
                     'markdown-ts-appear-image--state 'uninitialized)))))

(defun markdown-ts-appear-image--sync (owner)
  "Synchronize OWNER's cached previews with visibility and graphical windows."
  (let ((windows (markdown-ts-appear-image--windows))
        (base (overlay-get owner 'markdown-ts-appear-image--image))
        views)
    (dolist (view (overlay-get owner 'markdown-ts-appear-image--views))
      (if (memq (markdown-ts-appear-image--view-window view) windows)
          (push view views)
        (markdown-ts-appear-image--delete-view view)))
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
              (let* ((start (- (overlay-end owner) 2))
                     (overlay (make-overlay start (1+ start) nil t nil)))
                (overlay-put overlay 'window window)
                (overlay-put overlay 'priority '(nil . 2))
                (overlay-put overlay 'face 'default)
                (overlay-put overlay 'help-echo (overlay-get owner 'markdown-ts-appear-image--file))
                (setq view (markdown-ts-appear-image--make-view
                            :owner owner :window window :overlay overlay))
                (overlay-put overlay 'markdown-ts-appear-image--view view)
                (setf (markdown-ts-appear-image--view-picture view)
                      (markdown-ts-appear-image--make-overlay view (1+ start)))
                (push view views)
                (overlay-put owner 'markdown-ts-appear-image--views views)))
            (markdown-ts-appear-image--layout view base)))))
    (overlay-put owner 'markdown-ts-appear-image--views views)
    (dolist (view views) (markdown-ts-appear-image--present view))))

(defun markdown-ts-appear-image--fontify (node file)
  "Maintain a whole local image for NODE referring to FILE."
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
            (markdown-ts-appear-image--delete-view view))
          (overlay-put owner 'markdown-ts-appear-image--views nil)
          (overlay-put owner 'markdown-ts-appear-image--image image))))
    (markdown-ts-appear-image--sync owner)))

(defun markdown-ts-appear-image--update ()
  "Update visibility after an explicit source-tracking state change."
  (dolist (owner markdown-ts-appear-image--objects)
    (when (overlay-buffer owner) (markdown-ts-appear-image--sync owner))))

(defun markdown-ts-appear-image--before-change (beg end)
  "Discard previews whose source is edited between BEG and END."
  (dolist (owner markdown-ts-appear-image--objects)
    (when (and (overlay-buffer owner) (< beg (overlay-end owner))
               (> end (overlay-start owner)))
      (markdown-ts-appear-image--delete owner))))

(defun markdown-ts-appear-image--post-command ()
  "Validate edited image source without rereading unchanged files."
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
    (markdown-ts-appear-image--update)))

(defun markdown-ts-appear-image--line-move (function count &optional noerror &rest rest)
  "Move COUNT lines with FUNCTION and preserve whole-image edge clipping.
NOERROR and REST preserve native line-motion options."
  (if (not (and markdown-ts-inline-images markdown-ts-appear-image--objects
                (/= count 0) line-move-visual (markdown-ts-appear--active-p)
                (display-images-p)))
      (apply function count noerror rest)
    (let* ((window (selected-window))
           (start (window-start window))
           (vscroll (window-vscroll window t))
           (old-view (get-char-property start 'markdown-ts-appear-image--view window)))
      (prog1 (apply function count noerror rest)
        (when (and (eq window (selected-window)) (eq (window-buffer window) (current-buffer)))
          ;; Native line motion can reset a partially clipped first line before
          ;; redisplay.  Preserve the document offset while moving point normally.
          (when-let* (((> vscroll 0))
                      (view (get-char-property start 'markdown-ts-appear-image--view window))
                      ((= start (overlay-start (markdown-ts-appear-image--view-picture view)))))
            (set-window-start window start t)
            (set-window-vscroll window vscroll t))
          (let ((view (or (get-char-property (point) 'markdown-ts-appear-image--view window)
                          (and old-view
                               (< (point) (overlay-start (markdown-ts-appear-image--view-picture old-view)))
                               old-view))))
            (when (and view (= (abs count) 1)
                       (markdown-ts-appear-image--visible-p (markdown-ts-appear-image--view-owner view)))
              (set-window-start window start t)
              (set-window-vscroll window vscroll t)
              (markdown-ts-appear-image--reveal-whole view)))
          (markdown-ts-appear-image--scroll-edge count))))))

(defun markdown-ts-appear-image--reveal-whole (view)
  "Bring VIEW into the document window with a short pixel transition.
Point is restored to the destination chosen by native line motion."
  (let* ((window (markdown-ts-appear-image--view-window view))
         (anchor (overlay-start (markdown-ts-appear-image--view-picture view)))
         (height (ceiling (cdr (markdown-ts-appear-image--view-size view))))
         (line-height (window-font-height window))
         (margin (* line-height (min scroll-margin
                                     (floor (* maximum-scroll-margin (window-body-height window))))))
         (delay (/ pixel-scroll-precision-interpolation-total-time
                   (max 1 (ceiling height line-height))))
         (remaining (* 2 (window-body-height window))))
    (save-excursion
      (goto-char anchor)
      (catch 'done
        (while (> remaining 0)
          (setq height (ceiling (cdr (markdown-ts-appear-image--view-size view))))
          (let* ((position (pos-visible-in-window-p anchor window t))
                 (up (if position (or (> (or (nth 2 position) 0) 0)
                                      (< (cadr position) margin))
                       (< anchor (window-start window))))
                 (down (or (null position)
                           (> (or (nth 3 position) 0) 0)
                           (> (+ (cadr position) height) (- (window-body-height window t) margin)))))
            (when (or (not (or up down))
                      (and up (= (window-start window) (point-min)) (= (window-vscroll window t) 0)))
              (throw 'done nil))
            (condition-case nil
                (if up (pixel-scroll-precision-scroll-up line-height)
                  (pixel-scroll-precision-scroll-down line-height))
              ((beginning-of-buffer end-of-buffer) (throw 'done nil)))
            (redisplay t)
            (unless (or executing-kbd-macro (input-pending-p)) (sit-for delay))
            (cl-decf remaining)))))))

(defun markdown-ts-appear-image--scroll-edge (step)
  "Scroll the document by STEP text rows across a whole image's top edge."
  (let* ((window (selected-window))
         (start (max (point-min) (min (point-max) (window-start window))))
         (owner (seq-find
                 (lambda (overlay) (overlay-get overlay 'markdown-ts-appear-image--source))
                 (overlays-in (max (point-min) (- start 2))
                              (min (point-max) (1+ start))))))
    (when (and owner (markdown-ts-appear-image--visible-p owner))
      (let* ((position (pos-visible-in-window-p (point) window t))
             (height (window-font-height window))
             (margin (* height (min scroll-margin
                                    (floor (* maximum-scroll-margin (window-body-height window))))))
             (scroll (if (> step 0)
                         (or (null position)
                             (> (+ (cadr position) height) (- (window-body-height window t) margin)))
                       (or (null position) (< (cadr position) margin)))))
        (when scroll
          (let* ((view (seq-find (lambda (item) (eq window (markdown-ts-appear-image--view-window item)))
                                 (overlay-get owner 'markdown-ts-appear-image--views)))
                 (anchor (and view (overlay-start (markdown-ts-appear-image--view-picture view))))
                 (pixels (+ (window-vscroll window t) (* step height))))
            (cond
             ((and anchor (= start anchor) (<= 0 pixels)
                   (< pixels (cdr (markdown-ts-appear-image--view-size view))))
              (set-window-start window start t)
              (set-window-vscroll window pixels t))
             (t
              (let ((next (save-excursion
                            (goto-char start)
                            (vertical-motion (if (< step 0) -1 1) window)
                            (point))))
                (set-window-start window next t)
                (set-window-vscroll
                 window (if (and anchor (= next anchor) (< step 0))
                            (max 0 (+ (ceiling (cdr (markdown-ts-appear-image--view-size view)))
                                      (* step height)))
                          0) t))))))))))

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
        markdown-ts-appear-image--tick nil))

(defun markdown-ts-appear-image--teardown ()
  "Remove owned previews and image lifecycle hooks."
  (dolist (owner markdown-ts-appear-image--objects) (markdown-ts-appear-image--delete owner))
  (markdown-ts-appear-image--detach))

(provide 'markdown-ts-appear-image)
;;; markdown-ts-appear-image.el ends here
