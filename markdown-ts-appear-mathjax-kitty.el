;;; markdown-ts-appear-mathjax-kitty.el --- Experimental terminal display -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Optional display adapter, not another renderer.  MathJax owns requests and
;; source validation; kitty-graphics owns conversion and terminal placements.
;; This experiment uses private kitty-graphics APIs for placement cleanup.

;;; Code:
(require 'cl-lib)
(require 'kitty-graphics)
(require 'markdown-ts-appear-mathjax)

(defun markdown-ts-appear-mathjax-kitty--position (original view &optional window)
  "Center our display formula VIEW in WINDOW without changing buffer text.
ORIGINAL supplies visibility checks and the vertical terminal coordinate."
  (let ((position (funcall original view window))
        (source (overlay-get view 'markdown-ts-appear-mathjax-kitty--source)))
    (when (and position source (overlay-buffer source))
      (with-current-buffer (overlay-buffer source)
        (when (and (not (eq (overlay-get source 'markdown-ts-appear-mathjax--kind) 'inline))
                   (or markdown-ts-appear-mathjax-center-display
                       (markdown-ts-appear-mathjax--editing-p source)))
          (let* ((window (or window (get-buffer-window (current-buffer))))
                 (numbers (kitty-graphics--window-line-number-width window))
                 (width (- (window-body-width window) numbers))
                 (padding (max 0 (/ (- width (overlay-get view 'kitty-graphics-cols)) 2))))
            (setcdr position (+ 1 (car (window-body-edges window)) numbers padding))))))
    position))

;; kitty-graphics centers its own buffers by inserting spaces.  Markdown must
;; stay intact, so adjust only our placements, independently for each window.
(advice-add 'kitty-graphics--overlay-screen-pos :around #'markdown-ts-appear-mathjax-kitty--position)

(defun markdown-ts-appear-mathjax-kitty--refresh (original view window bottom)
  "Reassert VIEW's placement after terminal redisplay, even if unmoved.
ORIGINAL handles visibility, movement and transmission for WINDOW and BOTTOM.
TTY line erasure can remove an image without changing its recorded position."
  (let ((previous (cdr (kitty-graphics--image-placement view window))))
    (funcall original view window bottom)
    (when (and previous (overlay-get view 'markdown-ts-appear-mathjax-kitty--source)
               (eq previous (cdr (kitty-graphics--image-placement view window))))
      (kitty-graphics--kitty-place
       view (overlay-get view 'kitty-graphics-id) (plist-get previous :pid)
       (plist-get previous :cols) (plist-get previous :rows)
       (plist-get previous :row) (plist-get previous :col)))))

(advice-add 'kitty-graphics--refresh-overlay :around #'markdown-ts-appear-mathjax-kitty--refresh)

(defun markdown-ts-appear-mathjax-kitty--repaint ()
  "Schedule cached placements after command-driven terminal redraws."
  (when (and (bound-and-true-p kitty-graphics-mode) markdown-ts-appear-mathjax-mode)
    ;; Use the backend's debounce; no SVG/PNG work or overlay reconstruction.
    (kitty-graphics--schedule-refresh)))

(defun markdown-ts-appear-mathjax-kitty--redraw (&rest _)
  "Restore visible math after an explicit terminal screen clear."
  (when (and (bound-and-true-p kitty-graphics-mode) (not (display-graphic-p)))
    (kitty-graphics--schedule-refresh)))

(advice-add 'redraw-display :after #'markdown-ts-appear-mathjax-kitty--redraw)

(defun markdown-ts-appear-mathjax-kitty--source-rows (ov)
  "Shade OV's complete source lines in the terminal, including their newline."
  (let ((previous (overlay-get ov 'markdown-ts-appear-mathjax--rows)))
    (markdown-ts-appear-mathjax--source-rows ov)
    (unless (eq previous (overlay-get ov 'markdown-ts-appear-mathjax--rows))
      (dolist (row (overlay-get ov 'markdown-ts-appear-mathjax--rows))
        ;; GUI stretch-space tails stop one cell short on TTY, while hl-line
        ;; shades the cursor row through its newline.  Use the same newline
        ;; coverage for every row instead of a synthetic right-edge tail.
        (overlay-put row 'after-string nil)
        (overlay-put row 'face 'markdown-ts-appear-mathjax-source)
        (when (eq (char-after (overlay-end row)) ?\n)
          (move-overlay row (overlay-start row) (1+ (overlay-end row))))))))

(defun markdown-ts-appear-mathjax-kitty--clear (ov)
  "Remove OV's terminal image without invalidating its compiled formula."
  (when-let* ((process (overlay-get ov 'markdown-ts-appear-mathjax-kitty--process)))
    (overlay-put ov 'markdown-ts-appear-mathjax-kitty--process nil)
    (when (process-live-p process) (delete-process process)))
  (when-let* ((view (overlay-get ov 'markdown-ts-appear-mathjax-kitty--view)))
    (kitty-graphics--remove-overlay view)
    (overlay-put ov 'markdown-ts-appear-mathjax-kitty--view nil)
    ;; Revealing source changes the screen positions of neighboring images,
    ;; even though their overlays and compiled formulas have not changed.
    (kitty-graphics--schedule-refresh t)))

(defun markdown-ts-appear-mathjax-kitty--available-p (ov)
  "Return non-nil when OV is a supported terminal formula."
  (and (not (display-graphic-p))
       (bound-and-true-p kitty-graphics-mode)
       (eq kitty-graphics--active-backend 'kitty)
       (overlay-buffer ov)
       (or (eq (overlay-get ov 'markdown-ts-appear-mathjax--kind) 'inline)
           (save-excursion
         (goto-char (overlay-start ov)) (skip-chars-backward " \t")
         (and (bolp) (progn (goto-char (overlay-end ov))
                            (skip-chars-forward " \t") (eolp)))))))

(defun markdown-ts-appear-mathjax-kitty--geometry (entry inline)
  "Return pixel and cell dimensions for ENTRY, fitting INLINE to one row."
  (unless (plist-get entry :height)
    (setf (plist-get entry :height)
          (string-to-number (markdown-ts-appear-mathjax--attribute (plist-get entry :svg) "height"))))
  (let* ((cw (or (terminal-parameter nil 'kitty-graphics-cell-w) kitty-graphics--cell-pixel-width 8))
         (ch (or (terminal-parameter nil 'kitty-graphics-cell-h) kitty-graphics--cell-pixel-height 16))
         (scale (/ (* ch 0.45 (if inline 0.8 1.0) markdown-ts-appear-mathjax-scale)
                   (plist-get entry :ex)))
         (width (* scale (plist-get entry :width)))
         (height (* scale (plist-get entry :height)))
         (fit (min 1.0 (/ (* cw (window-body-width)) width)
                    (if inline (/ (* ch 0.8) height) 1.0)))
         (w (max 1 (round (* width fit))))
         (h (max 1 (round (* height fit)))))
    (list w h (ceiling w cw) (if inline 1 (ceiling h ch)) cw ch)))

(defun markdown-ts-appear-mathjax-kitty--place (ov file geometry editing inline)
  "Place FILE for OV with GEOMETRY, respecting EDITING and INLINE."
  (let* ((beg (if editing (1+ (overlay-end ov)) (overlay-start ov)))
         (end (if editing (1+ beg) (overlay-end ov)))
         (view (kitty-graphics-display-image file beg end (nth 2 geometry) (nth 3 geometry))))
    (when view
      (add-hook 'post-command-hook #'markdown-ts-appear-mathjax-kitty--repaint 95 t)
      (overlay-put view 'markdown-ts-appear-mathjax-kitty--source ov)
      (when editing
        (let ((face '(:inherit markdown-ts-appear-mathjax-preview :box nil :extend t)))
          (add-face-text-property 0 (length (overlay-get view 'display)) face nil
                                  (overlay-get view 'display))
          (overlay-put view 'face face))))
    (when (and view inline)
      ;; The upstream block API appends a newline; inline math reserves cells
      ;; only, so following prose stays on the same terminal row.
      (overlay-put view 'display (make-string (nth 2 geometry) ?\s)))
    (overlay-put ov 'markdown-ts-appear-mathjax-kitty--view view)))

(defun markdown-ts-appear-mathjax-kitty--rasterize (ov svg file geometry)
  "Convert SVG asynchronously to transparent FILE at GEOMETRY for OV."
  (let* ((program (or (executable-find "magick") (executable-find "convert")
                      (user-error "Kitty math requires ImageMagick")))
         (input (make-temp-file (concat file "-") nil ".svg"))
         (output (make-temp-file (concat file "-") nil ".png"))
         (density (ceiling (/ (* 72.0 4 (nth 1 geometry))
                              (string-to-number (markdown-ts-appear-mathjax--attribute svg "height")))))
         (key (overlay-get ov 'markdown-ts-appear-mathjax-kitty--key)))
    (with-temp-file input (insert svg))
    (overlay-put
     ov 'markdown-ts-appear-mathjax-kitty--process
     (make-process
      :name "appear-kitty-png" :noquery t :buffer nil
      ;; Keep four raster pixels per layout pixel.  Terminal cell queries may
      ;; report logical pixels (or fall back to 8x16), especially on HiDPI.
      ;; Placement still uses the original cell counts, not the PNG dimensions.
      :command (list program "-background" "none" "-density" (number-to-string density) input
                     "-resize" (format "%dx%d!" (* 4 (nth 0 geometry)) (* 4 (nth 1 geometry)))
                     "-gravity" "center" "-extent"
                     (format "%dx%d" (* 4 (nth 2 geometry) (nth 4 geometry))
                             (* 4 (nth 3 geometry) (nth 5 geometry))) output)
      :sentinel
      (lambda (process _event)
        (when (memq (process-status process) '(exit signal))
          (when (file-exists-p input) (delete-file input))
          (if (zerop (process-exit-status process))
              (rename-file output file t)
            (when (file-exists-p output) (delete-file output)))
          (when (and (overlay-buffer ov)
                     (eq process (overlay-get ov 'markdown-ts-appear-mathjax-kitty--process)))
            (overlay-put ov 'markdown-ts-appear-mathjax-kitty--process nil)
            (when (and (zerop (process-exit-status process))
                       (equal key (overlay-get ov 'markdown-ts-appear-mathjax-kitty--key)))
              (with-current-buffer (overlay-buffer ov)
                (markdown-ts-appear-mathjax-kitty--sync ov))))))))))

(defun markdown-ts-appear-mathjax-kitty--sync (ov)
  "Display OV's ready formula, optionally below its editable source."
  (when (overlay-buffer ov)
    (let* ((editing (and (markdown-ts-appear-mathjax--editing-p ov) t))
           (inline (eq (overlay-get ov 'markdown-ts-appear-mathjax--kind) 'inline))
           (entry (overlay-get ov 'markdown-ts-appear-mathjax--entry))
           (svg (plist-get entry :svg))
           (foreground (or (face-foreground 'markdown-ts-latex nil t) "white"))
           (geometry (and svg (markdown-ts-appear-mathjax-kitty--geometry entry inline)))
           (key (list (overlay-get ov 'markdown-ts-appear-mathjax--key) foreground editing geometry))
           (view (overlay-get ov 'markdown-ts-appear-mathjax-kitty--view)))
      (if (and editing (not inline) markdown-ts-appear-mathjax-edit-preview
               (markdown-ts-appear-mathjax-kitty--available-p ov))
          (markdown-ts-appear-mathjax-kitty--source-rows ov)
        (markdown-ts-appear-mathjax--clear-panel ov))
      (when (or (and view (not (overlay-buffer view)))
                (not (equal key (overlay-get ov 'markdown-ts-appear-mathjax-kitty--key))))
        (markdown-ts-appear-mathjax-kitty--clear ov))
      (if (or (not (markdown-ts-appear-mathjax-kitty--available-p ov))
              (overlay-get ov 'markdown-ts-appear-mathjax--modified)
              (not svg)
              (and editing
                    (not (and (not inline) markdown-ts-appear-mathjax-edit-preview
                             ;; Reserve an existing empty line, never edit text.
                             (eq (char-after (overlay-end ov)) ?\n)
                             (eq (char-after (1+ (overlay-end ov))) ?\n)))))
          (markdown-ts-appear-mathjax-kitty--clear ov)
        (unless (or (overlay-get ov 'markdown-ts-appear-mathjax-kitty--view)
                    (overlay-get ov 'markdown-ts-appear-mathjax-kitty--process))
          (let* ((rgb (or (color-values foreground) '(65535 65535 65535)))
                 (color (apply #'format "#%02x%02x%02x" (mapcar (lambda (v) (/ v 257)) rgb)))
                 (directory (expand-file-name "kitty/" markdown-ts-appear-mathjax-cache-directory))
                 (tinted (replace-regexp-in-string "currentColor" color svg t t))
                 (file (expand-file-name
                        (concat (secure-hash 'sha256 (concat "raster-4x\0" (prin1-to-string geometry) tinted)) ".png") directory)))
            (make-directory directory t)
            (overlay-put ov 'markdown-ts-appear-mathjax-kitty--key key)
            (if (file-exists-p file)
                (markdown-ts-appear-mathjax-kitty--place ov file geometry editing inline)
              (markdown-ts-appear-mathjax-kitty--rasterize ov tinted file geometry))))))))

(provide 'markdown-ts-appear-mathjax-kitty)
;;; markdown-ts-appear-mathjax-kitty.el ends here
