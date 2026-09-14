;;; markdown-ts-appear-math.el --- Internal MathJax previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael

;; Author: Thysrael <thysrael@163.com>
;; Assisted-by: OpenCode:gpt-5.6-sol
;; Maintainer: Thysrael <thysrael@163.com>
;; Keywords: text, convenience
;; URL: https://github.com/Thysrael/markdown-ts-appear

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Internal MathJax preview support for `markdown-ts-appear-mode'.
;; This library does not define a separate minor mode.

;;; Code:

(require 'cl-lib)
(require 'markdown-ts-mode)
(require 'seq)
(require 'subr-x)

(declare-function mathjax-available-p "mathjax")
(declare-function mathjax-display "mathjax")
(declare-function markdown-ts-appear--active-p "markdown-ts-appear")
(declare-function markdown-ts-appear--literal-block-at "markdown-ts-appear")
(declare-function markdown-ts-appear--node-ancestor "markdown-ts-appear")
(declare-function markdown-ts-appear--region-visible-p "markdown-ts-appear")
(declare-function markdown-ts-appear--update "markdown-ts-appear")

(defvar markdown-ts-appear-enable-math-preview)
(defvar markdown-ts-appear--region)

(defvar-local markdown-ts-appear-math--objects nil
  "Formula overlays, each holding its source, image and pending render buffer.")

(defvar markdown-ts-appear-math--query nil
  "Compiled math query, shared by all inline parsers.")

(defvar-local markdown-ts-appear-math--scan-tick nil
  "Text modification tick of the last formula scan.")

(defvar-local markdown-ts-appear-math--view nil
  "Last (POINT REVEAL-BEG REVEAL-END), or nil to recheck all previews.
POINT is nil when source tracking is paused.")

(defun markdown-ts-appear-math--delete (preview)
  "Remove PREVIEW and invalidate its pending render, if any."
  (let ((staging (overlay-get preview 'markdown-ts-appear-math--buffer)))
    (delete-overlay preview)
    (setq markdown-ts-appear-math--scan-tick nil
          markdown-ts-appear-math--view nil)
    (setq markdown-ts-appear-math--objects
          (delq preview markdown-ts-appear-math--objects))
    (when (buffer-live-p staging)
      (kill-buffer staging))))

(defun markdown-ts-appear-math--clear (&optional beg end)
  "Clear previews, or only those whose source is edited between BEG and END."
  (setq markdown-ts-appear-math--scan-tick nil
        markdown-ts-appear-math--view nil)
  (dolist (preview markdown-ts-appear-math--objects)
    (when (or (null beg) (not (overlay-buffer preview))
              (and (< (overlay-start preview) end)
                   (> (overlay-end preview) beg)))
      (markdown-ts-appear-math--delete preview))))

(defun markdown-ts-appear-math--eligible-p (beg end)
  "Return non-nil when BEG through END may cover source with a preview."
  (and markdown-ts-appear-enable-math-preview (markdown-ts-appear--active-p)
       (not (and (memq #'markdown-ts-appear--update post-command-hook)
                 (<= beg (point)) (< (point) end)))
       (not (markdown-ts-appear--region-visible-p beg end))
       (not (markdown-ts--outline-invisible-p beg))))

(defun markdown-ts-appear-math--display (preview)
  "Show PREVIEW's saved image unless its source is currently revealed."
  (let* ((visible (markdown-ts-appear-math--eligible-p
                   (overlay-start preview) (overlay-end preview)))
         (image (and visible (overlay-get preview 'markdown-ts-appear-math--image))))
    (unless (eq image (overlay-get preview 'display))
      (overlay-put preview 'display image))
    (overlay-put preview 'face
                 (and visible (overlay-get preview 'mathjax-error) 'error))))

(defun markdown-ts-appear-math--request (preview math display-p)
  "Render MATH into PREVIEW; DISPLAY-P selects display rather than inline math."
  (let ((target (current-buffer))
        (source (overlay-get preview 'markdown-ts-appear-math--source))
        (staging (generate-new-buffer " *markdown-ts-appear-math*")))
    (overlay-put preview 'markdown-ts-appear-math--buffer staging)
    ;; MathJax deletes existing `mathjax' overlays before calling :after.
    ;; Isolate that operation, then copy valid results to our anchored preview.
    (with-current-buffer staging
      (insert source)
      (condition-case err
          (mathjax-display
           (point-min) (point-max) math :options (list :display display-p)
           :after
           (lambda (overlay)
             (unwind-protect
                 (when (eq (overlay-buffer preview) target)
                   (with-current-buffer target
                     (save-restriction
                       (widen)
                       (let ((beg (overlay-start preview))
                             (end (overlay-end preview)))
                         (treesit-update-ranges beg end)
                         (let ((node (markdown-ts-appear--node-ancestor
                                      (treesit-node-at beg 'markdown-inline)
                                      "latex_block")))
                           (if (and markdown-ts-appear-enable-math-preview
                                    (markdown-ts-appear--active-p)
                                    node (= beg (treesit-node-start node))
                                    (= end (treesit-node-end node))
                                    (not (markdown-ts-appear--literal-block-at beg))
                                    (equal source (buffer-substring-no-properties beg end)))
                               (progn
                                 (overlay-put preview 'markdown-ts-appear-math--image
                                              (overlay-get overlay 'display))
                                 (overlay-put preview 'mathjax-error
                                              (overlay-get overlay 'mathjax-error))
                                 (setq markdown-ts-appear-math--view nil)
                                 (markdown-ts-appear-math--display preview))
                             (markdown-ts-appear-math--delete preview)))))))
               (overlay-put preview 'markdown-ts-appear-math--buffer nil)
               (delete-overlay overlay)
               (when (buffer-live-p staging) (kill-buffer staging)))))
        (error
         (overlay-put preview 'markdown-ts-appear-math--buffer nil)
         (kill-buffer staging)
         (message "Markdown MathJax preview failed: %s" (error-message-string err)))))))

(defun markdown-ts-appear-math--scan ()
  "Reconcile formula overlays with the current text, without rendering."
  (treesit-update-ranges (point-min) (point-max))
  (unless markdown-ts-appear-math--query
    (setq markdown-ts-appear-math--query
          (treesit-query-compile 'markdown-inline '((latex_block) @math))))
  (let ((existing (make-hash-table :test #'eql))
        (current (make-hash-table :test #'eq)))
    (dolist (preview markdown-ts-appear-math--objects)
      (when (eq (overlay-buffer preview) (current-buffer))
        (puthash (overlay-start preview) preview existing)))
    (dolist (parser (treesit-parser-list nil 'markdown-inline t))
      (dolist (node (treesit-query-capture
                     (treesit-parser-root-node parser)
                     markdown-ts-appear-math--query nil nil t))
        (let* ((beg (treesit-node-start node))
               (end (treesit-node-end node))
               (opening (treesit-node-child node 0))
               (closing (treesit-node-child node -1)))
          (when (and (markdown-ts--latex-block-valid-p node)
                     (equal (treesit-node-type opening) "latex_span_delimiter")
                     (equal (treesit-node-type closing) "latex_span_delimiter")
                     (< (treesit-node-start opening) (treesit-node-start closing)))
            (let* ((source (treesit-node-text node t))
                   (candidate (gethash beg existing))
                   (preview
                    (and candidate (= end (overlay-end candidate))
                         (equal source (overlay-get
                                        candidate 'markdown-ts-appear-math--source))
                         candidate)))
              (unless preview
                (setq preview (make-overlay beg end nil t nil))
                (overlay-put preview 'category 'mathjax)
                (overlay-put preview 'evaporate t)
                (overlay-put preview 'markdown-ts-appear-math--source source)
                (overlay-put preview 'markdown-ts-appear-math--input
                             (list (buffer-substring-no-properties
                                    (treesit-node-end opening) (treesit-node-start closing))
                                   (and (member (treesit-node-text opening t)
                                                '("$$" "\\["))
                                        t)))
                (push preview markdown-ts-appear-math--objects)
                (puthash beg preview existing))
              (puthash preview t current))))))
    (dolist (preview markdown-ts-appear-math--objects)
      (unless (gethash preview current)
        (markdown-ts-appear-math--delete preview)))))

(defun markdown-ts-appear-math--refresh (&optional force)
  "Update formulas after text edits, and visibility after cursor movement.
FORCE rechecks all preview visibility, for example after outline folding."
  (if (not (and markdown-ts-appear-enable-math-preview
                (markdown-ts-appear--active-p)))
      (markdown-ts-appear-math--clear)
    (save-restriction
      (widen)
      (let* ((tick (buffer-chars-modified-tick))
             (changed (not (equal tick markdown-ts-appear-math--scan-tick)))
             (region markdown-ts-appear--region)
             (view (list (and (memq #'markdown-ts-appear--update post-command-hook)
                              (point))
                         (and region (marker-position (car region)))
                         (and region (marker-position (cdr region))))))
        (when changed
          (markdown-ts-appear-math--scan)
          (setq markdown-ts-appear-math--scan-tick tick))
        (when (or force changed (not (equal view markdown-ts-appear-math--view)))
          (let ((previews
                 (if (or force changed (null markdown-ts-appear-math--view))
                     markdown-ts-appear-math--objects
                   (let (nearby)
                     (dolist (state (list markdown-ts-appear-math--view view))
                       (when (car state)
                         (setq nearby (append (overlays-at (car state)) nearby)))
                       (when (and (nth 1 state) (nth 2 state))
                         (setq nearby (append (overlays-in (nth 1 state) (nth 2 state))
                                              nearby))))
                     (seq-filter
                      (lambda (preview)
                        (overlay-get preview 'markdown-ts-appear-math--source))
                      (seq-uniq nearby #'eq))))))
            ;; Callbacks may invalidate this view; commit it before rendering.
            (setq markdown-ts-appear-math--view view)
            (dolist (preview previews)
              (when (overlay-buffer preview)
                (markdown-ts-appear-math--display preview)
                (when-let* ((input (overlay-get preview 'markdown-ts-appear-math--input))
                            ((markdown-ts-appear-math--eligible-p
                              (overlay-start preview) (overlay-end preview))))
                  (overlay-put preview 'markdown-ts-appear-math--input nil)
                  (apply #'markdown-ts-appear-math--request preview input))))))))))

(defun markdown-ts-appear-math--outline-change ()
  "Refresh folded previews without rescanning unchanged text."
  (markdown-ts-appear-math--refresh t))

(defun markdown-ts-appear-math--setup ()
  "Install math preview hooks and render eligible formulas."
  (unless (and (require 'mathjax nil t) (fboundp 'mathjax-display)
               (mathjax-available-p) (image-type-available-p 'svg))
    (user-error "MathJax previews require the mathjax package, Node.js and SVG support"))
  (add-hook 'post-command-hook #'markdown-ts-appear-math--refresh 90 t)
  ;; Outline still emits this hook and provides no replacement.
  (with-suppressed-warnings ((obsolete outline-view-change-hook))
    (add-hook 'outline-view-change-hook
              #'markdown-ts-appear-math--outline-change nil t))
  (add-hook 'before-change-functions #'markdown-ts-appear-math--clear nil t)
  (markdown-ts-appear-math--refresh))

(defun markdown-ts-appear-math--teardown ()
  "Remove math preview hooks and dispose of pending and displayed results."
  (remove-hook 'post-command-hook #'markdown-ts-appear-math--refresh t)
  (with-suppressed-warnings ((obsolete outline-view-change-hook))
    (remove-hook 'outline-view-change-hook
                 #'markdown-ts-appear-math--outline-change t))
  (remove-hook 'before-change-functions #'markdown-ts-appear-math--clear t)
  (markdown-ts-appear-math--clear))

(provide 'markdown-ts-appear-math)
;;; markdown-ts-appear-math.el ends here
