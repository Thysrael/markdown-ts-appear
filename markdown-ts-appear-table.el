;;; markdown-ts-appear-table.el --- Internal table rendering -*- lexical-binding: t; -*-

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

;; Internal Unicode and width-aware table rendering for
;; `markdown-ts-appear-mode'.  This library does not define a separate
;; minor mode.

;;; Code:

(require 'cl-lib)
(require 'markdown-ts-mode)
(require 'markdown-table-wrap-pretty)
(require 'seq)
(require 'subr-x)

(declare-function markdown-ts-appear--active-p "markdown-ts-appear")
(declare-function markdown-ts-appear--decorate "markdown-ts-appear")
(declare-function markdown-ts-appear--direct-children-of-type
                  "markdown-ts-appear")
(declare-function markdown-ts-appear--node-visible-p "markdown-ts-appear")
(declare-function markdown-ts-appear--region-visible-p "markdown-ts-appear")

(defvar markdown-ts-appear-table-style)
(defvar markdown-ts-appear--region)

(defcustom markdown-ts-appear-table-wrap-resize-delay 0.2
  "Seconds to debounce wrapped-table rendering after window changes."
  :type 'number
  :group 'markdown-ts-appear)

(defconst markdown-ts-appear-table--row-types
  '("pipe_table_header" "pipe_table_delimiter_row" "pipe_table_row")
  "Tree-sitter node types representing source rows of a pipe table.")

(defvar markdown-ts-appear-table--query nil
  "Compiled pipe-table query shared by Markdown parsers.")

(defvar-local markdown-ts-appear-table--overlays nil
  "Display overlays used for wrapped tables in the current buffer.")

(defvar-local markdown-ts-appear-table--dirty nil
  "Non-nil when wrapped tables need rebuilding after an edit.")

(defvar-local markdown-ts-appear-table--view nil
  "Last visible source region applied to wrapped-table overlays.")

(defvar-local markdown-ts-appear-table--resize-timer nil
  "Idle timer used to debounce wrapped-table rendering after resize.")

(defun markdown-ts-appear--fontify-table-row (row start limit)
  "Render delimiter characters in table ROW between START and LIMIT."
  (unless (markdown-ts-appear--node-visible-p row)
    (if (equal (treesit-node-type row) "pipe_table_delimiter_row")
        (let* ((row-end (treesit-node-end row))
               (content-start
                (save-excursion
                  (goto-char (treesit-node-start row))
                  (skip-chars-forward " \t" row-end)
                  (point)))
               (content-end
                (save-excursion
                  (goto-char row-end)
                  (skip-chars-backward " \t" content-start)
                  (point)))
               (pos (max start content-start))
               (end (min limit row-end)))
          (while (< pos end)
            (markdown-ts-appear--decorate
             pos (1+ pos)
             (if (eq (char-after pos) ?|)
                 (cond ((eq pos content-start) "├")
                       ((eq pos (1- content-end)) "┤")
                       (t "┼"))
               "─")
             'markdown-ts-table-delimiter-cell)
            (setq pos (1+ pos))))
      (dolist (pipe (markdown-ts-appear--direct-children-of-type row "|"))
        (when (and (<= start (treesit-node-start pipe))
                   (< (treesit-node-start pipe) limit))
          (markdown-ts-appear--decorate
           (treesit-node-start pipe) (treesit-node-end pipe) "│"
           'markdown-ts-table-delimiter-cell))))))

(defun markdown-ts-appear--fontify-table
    (node _override start limit &rest _)
  "Render Markdown pipe table NODE between START and LIMIT."
  (when (and (markdown-ts-appear--active-p)
             (eq markdown-ts-appear-table-style 'unicode)
             (< (max start (treesit-node-start node))
                (min limit (treesit-node-end node))))
    (let ((row (treesit-node-first-child-for-pos
                node (max start (treesit-node-start node)))))
      (while (and row (< (treesit-node-start row) limit))
        (when (member (treesit-node-type row)
                      markdown-ts-appear-table--row-types)
          (markdown-ts-appear--fontify-table-row row start limit))
        (setq row (treesit-node-next-sibling row))))))

(defconst markdown-ts-appear--table-font-lock-settings
  (treesit-font-lock-rules
   :language 'markdown
   :feature 'paragraph
   :override 'append
   '(((pipe_table) @markdown-ts-appear--fontify-table)))
  "Additional Tree-sitter font-lock settings for rendered tables.")

(defun markdown-ts-appear-table--delete-overlays ()
  "Delete all wrapped-table overlays in the current buffer."
  (dolist (overlay markdown-ts-appear-table--overlays)
    (delete-overlay overlay))
  (setq markdown-ts-appear-table--overlays nil
        markdown-ts-appear-table--view nil))

(defun markdown-ts-appear-table--rows (table)
  "Return source row nodes directly below TABLE."
  (seq-filter
   (lambda (node)
     (member (treesit-node-type node) markdown-ts-appear-table--row-types))
   (treesit-node-children table 'named)))

(defun markdown-ts-appear-table--row-cells (row)
  "Return direct cell nodes below ROW."
  (let ((cell-type (if (equal (treesit-node-type row)
                              "pipe_table_delimiter_row")
                       "pipe_table_delimiter_cell"
                     "pipe_table_cell")))
    (seq-filter
     (lambda (node) (equal (treesit-node-type node) cell-type))
     (treesit-node-children row 'named))))

(defun markdown-ts-appear-table--canonical-row (row)
  "Return ROW as a canonical edge-pipe line for the wrapping backend."
  (let* ((line-beg (save-excursion
                     (goto-char (treesit-node-start row))
                     (line-beginning-position)))
         (line-end (save-excursion
                     (goto-char (treesit-node-end row))
                     (line-end-position)))
         (line (buffer-substring-no-properties line-beg line-end))
         (cells (markdown-ts-appear-table--row-cells row))
         (first (car cells))
         (last (car (last cells)))
         (prefix (and first
                      (buffer-substring-no-properties
                       line-beg (treesit-node-start first))))
         (suffix (and last
                      (buffer-substring-no-properties
                       (treesit-node-end last) line-end))))
    (if (and prefix suffix (string-search "|" prefix)
             (string-search "|" suffix))
        line
      (let* ((pipe (and prefix (string-search "|" prefix)))
             (container-prefix (if pipe (substring prefix 0 pipe) prefix))
             (contents (mapcar (lambda (cell)
                                 (string-trim (treesit-node-text cell t)))
                               cells)))
        (concat container-prefix "| "
                (mapconcat #'identity contents " | ") " |")))))

(defun markdown-ts-appear-table--window-width (window)
  "Return the number of table-face columns available in WINDOW."
  (if (and window (window-live-p window))
      (max 1 (window-max-chars-per-line window 'markdown-ts-table))
    80))

(defun markdown-ts-appear-table--display-windows ()
  "Return live non-minibuffer windows showing the current buffer.
Return a single nil entry when the buffer is not currently displayed."
  (or (seq-remove #'window-minibuffer-p
                  (get-buffer-window-list (current-buffer) nil t))
      (list nil)))

(defun markdown-ts-appear-table--display-string (lines newline-p)
  "Join display LINES and preserve the source newline when NEWLINE-P."
  (let ((display (concat (mapconcat #'identity lines "\n")
                         (and newline-p "\n"))))
    (add-face-text-property 0 (length display) 'markdown-ts-table t display)
    display))

(defun markdown-ts-appear-table--make-overlay
    (row lines window)
  "Display LINES in place of source ROW, restricted to WINDOW when non-nil."
  (let* ((beg (save-excursion
                (goto-char (treesit-node-start row))
                (line-beginning-position)))
         (line-end (save-excursion
                     (goto-char (treesit-node-end row))
                     (line-end-position)))
         (newline-p (< line-end (point-max)))
         (end (if newline-p (1+ line-end) line-end))
         (display (markdown-ts-appear-table--display-string lines newline-p))
         (overlay (make-overlay beg end nil nil nil)))
    (overlay-put overlay 'display display)
    (overlay-put overlay 'markdown-ts-appear-table--display display)
    (overlay-put overlay 'markdown-ts-appear-table--wrapped t)
    (overlay-put overlay 'evaporate t)
    (when window
      (overlay-put overlay 'window window))
    (push overlay markdown-ts-appear-table--overlays)))

(defun markdown-ts-appear-table--render-table (table window width)
  "Render TABLE for WINDOW using WIDTH columns."
  (let* ((rows (markdown-ts-appear-table--rows table))
         (raw-lines (mapcar #'markdown-ts-appear-table--canonical-row rows))
         (markdown-table-wrap-pretty-prettify t)
         (groups
          (markdown-table-wrap-pretty--table-display-groups raw-lines width)))
    (when (and groups (= (length groups) (length rows)))
      (cl-mapc (lambda (row lines)
                 (markdown-ts-appear-table--make-overlay row lines window))
               rows groups))))

(defun markdown-ts-appear-table--tables ()
  "Return current buffer's pipe-table syntax nodes."
  (unless markdown-ts-appear-table--query
    (setq markdown-ts-appear-table--query
          (treesit-query-compile 'markdown '((pipe_table) @table))))
  (when-let* ((parser (car (treesit-parser-list nil 'markdown t))))
    (treesit-query-capture
     (treesit-parser-root-node parser) markdown-ts-appear-table--query
     nil nil t)))

(defun markdown-ts-appear-table--render (&optional width)
  "Rebuild wrapped tables, overriding each window width with WIDTH when set."
  (markdown-ts-appear-table--delete-overlays)
  (when (and (eq markdown-ts-appear-table-style 'wrapped)
             (markdown-ts-appear--active-p))
    (save-restriction
      (widen)
      (dolist (table (markdown-ts-appear-table--tables))
        (unless (markdown-ts--outline-invisible-p (treesit-node-start table))
          (dolist (window (if width
                              (list nil)
                            (markdown-ts-appear-table--display-windows)))
            (markdown-ts-appear-table--render-table
             table window (or width
                              (markdown-ts-appear-table--window-width window))))))))
  (setq markdown-ts-appear-table--dirty nil)
  (markdown-ts-appear-table--update-visibility))

(defun markdown-ts-appear-table--set-region-display (region display-p)
  "Restore or hide wrapped-table overlays overlapping REGION.
DISPLAY-P non-nil restores their rendered display."
  (when region
    (let ((beg (car region))
          (end (cdr region)))
      (dolist (overlay (overlays-in beg end))
        (when (overlay-get overlay 'markdown-ts-appear-table--wrapped)
          (overlay-put
           overlay 'display
           (and display-p
                (overlay-get overlay
                             'markdown-ts-appear-table--display))))))))

(defun markdown-ts-appear-table--visible-region ()
  "Return numeric bounds of source currently revealed by the main mode."
  (when-let* ((region markdown-ts-appear--region)
              (beg (marker-position (car region)))
              (end (marker-position (cdr region))))
    (cons beg end)))

(defun markdown-ts-appear-table--update-visibility ()
  "Synchronize wrapped rows with the main mode's visible source region."
  (let ((view (markdown-ts-appear-table--visible-region)))
    (unless (equal view markdown-ts-appear-table--view)
      (markdown-ts-appear-table--set-region-display
       markdown-ts-appear-table--view t)
      (markdown-ts-appear-table--set-region-display view nil)
      (setq markdown-ts-appear-table--view view))))

(defun markdown-ts-appear-table--post-command ()
  "Rebuild edited tables and synchronize source visibility after a command."
  (when (and (eq markdown-ts-appear-table-style 'wrapped)
             (markdown-ts-appear--active-p))
    (if markdown-ts-appear-table--dirty
        (markdown-ts-appear-table--render)
      (markdown-ts-appear-table--update-visibility))))

(defun markdown-ts-appear-table--after-change (_beg _end _old-length)
  "Invalidate wrapped tables after a buffer edit."
  (setq markdown-ts-appear-table--dirty t)
  (markdown-ts-appear-table--delete-overlays))

(defun markdown-ts-appear-table--schedule-render (&rest _)
  "Schedule a debounced rebuild of wrapped tables."
  (when markdown-ts-appear-table--resize-timer
    (cancel-timer markdown-ts-appear-table--resize-timer))
  (let ((buffer (current-buffer)))
    (setq markdown-ts-appear-table--resize-timer
          (run-with-idle-timer
           markdown-ts-appear-table-wrap-resize-delay nil
           (lambda ()
             (when (buffer-live-p buffer)
               (with-current-buffer buffer
                 (setq markdown-ts-appear-table--resize-timer nil)
                 (when (and (eq markdown-ts-appear-table-style 'wrapped)
                            (markdown-ts-appear--active-p))
                   (markdown-ts-appear-table--render)))))))))

(defun markdown-ts-appear-table--setup ()
  "Install wrapped-table rendering when requested by the main mode."
  (when (eq markdown-ts-appear-table-style 'wrapped)
    (unless (fboundp 'markdown-table-wrap-pretty--table-display-groups)
      (user-error "Wrapped tables require markdown-table-wrap 0.2.0"))
    (add-hook 'post-command-hook #'markdown-ts-appear-table--post-command 20 t)
    (add-hook 'after-change-functions #'markdown-ts-appear-table--after-change 80 t)
    (add-hook 'window-configuration-change-hook
              #'markdown-ts-appear-table--schedule-render nil t)
    (with-suppressed-warnings ((obsolete outline-view-change-hook))
      (add-hook 'outline-view-change-hook
                #'markdown-ts-appear-table--schedule-render nil t))
    (markdown-ts-appear-table--render)))

(defun markdown-ts-appear-table--teardown ()
  "Remove wrapped-table hooks, timers and overlays."
  (remove-hook 'post-command-hook #'markdown-ts-appear-table--post-command t)
  (remove-hook 'after-change-functions #'markdown-ts-appear-table--after-change t)
  (remove-hook 'window-configuration-change-hook
               #'markdown-ts-appear-table--schedule-render t)
  (with-suppressed-warnings ((obsolete outline-view-change-hook))
    (remove-hook 'outline-view-change-hook
                 #'markdown-ts-appear-table--schedule-render t))
  (when markdown-ts-appear-table--resize-timer
    (cancel-timer markdown-ts-appear-table--resize-timer)
    (setq markdown-ts-appear-table--resize-timer nil))
  (setq markdown-ts-appear-table--dirty nil)
  (markdown-ts-appear-table--delete-overlays))

(defun markdown-ts-appear-table--detach ()
  "Detach table state inherited by an indirect buffer.

Do not delete overlays or cancel a timer here: those objects belong to the
base buffer."
  (remove-hook 'post-command-hook #'markdown-ts-appear-table--post-command t)
  (remove-hook 'after-change-functions #'markdown-ts-appear-table--after-change t)
  (remove-hook 'window-configuration-change-hook
               #'markdown-ts-appear-table--schedule-render t)
  (with-suppressed-warnings ((obsolete outline-view-change-hook))
    (remove-hook 'outline-view-change-hook
                 #'markdown-ts-appear-table--schedule-render t))
  (setq markdown-ts-appear-table--overlays nil
        markdown-ts-appear-table--resize-timer nil
        markdown-ts-appear-table--dirty nil))

(provide 'markdown-ts-appear-table)
;;; markdown-ts-appear-table.el ends here
