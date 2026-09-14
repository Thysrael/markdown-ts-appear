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

(defvar-local markdown-ts-appear-table--cursor-overlays nil
  "Three display overlays anchoring the rendered row at point.")

(defvar-local markdown-ts-appear-table--cursor-row nil
  "Bounds and window of the rendered row made interactive at point.")

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

(defun markdown-ts-appear-table--delete-cursor-overlays ()
  "Delete interactive display overlays for the rendered row at point."
  (dolist (overlay markdown-ts-appear-table--cursor-overlays)
    (delete-overlay overlay))
  (setq markdown-ts-appear-table--cursor-overlays nil
        markdown-ts-appear-table--cursor-row nil))

(defun markdown-ts-appear-table--delete-overlays ()
  "Delete all wrapped-table overlays in the current buffer."
  (markdown-ts-appear-table--delete-cursor-overlays)
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

(defun markdown-ts-appear-table--cell (node)
  "Render cell NODE with original source positions on surviving characters."
  (let* ((beg (treesit-node-start node))
         (text (treesit-node-text node t)))
    (if (not (string-match-p "[\\\\`*~[!]" text))
        ;; Plain cells only lose whitespace during wrapping.  Carry one
        ;; source run instead of allocating a property interval per character.
        (propertize (string-trim text)
                    'markdown-ts-appear-table--source (cons beg text))
      (dotimes (index (length text))
        (put-text-property index (1+ index)
                           'markdown-ts-appear-table--source (+ beg index) text))
      ;; Protect escaped pipes from the backend's unannotated replacement.
      ;; Restore only after inline rendering so a preceding backslash cannot
      ;; cause a second unescape.
      (let ((index 0) pipes)
        (setq text
              (replace-regexp-in-string
               "\\\\|"
               (lambda (match)
                 (let ((token (format "\0APPEAR%d\0" index)))
                   (push (substring match 1) pipes)
                   (setq index (1+ index))
                   token))
               (string-trim text) t t))
        (setq text (markdown-table-wrap-pretty-render-inline-spans text))
        (if (null pipes) text
          (setq pipes (vconcat (nreverse pipes)))
          (replace-regexp-in-string
           "\0APPEAR[0-9]+\0"
           (lambda (match)
             (apply #'propertize
                    (copy-sequence (aref pipes (string-to-number (substring match 7 -1))))
                    (text-properties-at 0 match)))
           text t t))))))

(defun markdown-ts-appear-table--prefix (row)
  "Return the container prefix before table ROW."
  (buffer-substring-no-properties
   (save-excursion (goto-char (treesit-node-start row))
                   (line-beginning-position))
   (treesit-node-start row)))

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

(defun markdown-ts-appear-table--row-bounds (row)
  "Return full source-line bounds for ROW, including its newline."
  (let* ((beg (save-excursion
                (goto-char (treesit-node-start row))
                (line-beginning-position)))
         (line-end (save-excursion
                     (goto-char (treesit-node-end row))
                     (line-end-position))))
    (cons beg (if (< line-end (point-max)) (1+ line-end) line-end))))

(defun markdown-ts-appear-table--map-pipes (row display)
  "Associate ROW's source pipes with generated borders in DISPLAY."
  (let ((cells (markdown-ts-appear-table--row-cells row))
        (column 0)
        (offset 0)
        (limit (or (string-search "\n" display) (length display)))
        borders)
    (save-match-data
      (while (and (setq offset (string-match "[│├┼┤]" display offset))
                  (< offset limit))
        (unless (get-text-property offset 'markdown-ts-appear-table--source display)
          (push offset borders))
        (setq offset (1+ offset))))
    (setq borders (vconcat (nreverse borders)))
    (dolist (pipe (markdown-ts-appear--direct-children-of-type row "|"))
      (let ((source (treesit-node-start pipe)))
        (while (and cells (<= (treesit-node-end (car cells)) source))
          (pop cells)
          (setq column (1+ column)))
        (when (< column (length borders))
          (let ((index (aref borders column)))
            (put-text-property index (1+ index)
                               'markdown-ts-appear-table--source source display)))))))

(defun markdown-ts-appear-table--make-overlay
    (row lines window)
  "Display LINES over source ROW, restricted to WINDOW when non-nil."
  (pcase-let* ((`(,beg . ,end) (markdown-ts-appear-table--row-bounds row))
               (newline-p (eq (char-before end) ?\n))
               (display
                (markdown-ts-appear-table--display-string lines newline-p))
               (overlay (make-overlay beg end nil nil nil)))
    (markdown-ts-appear-table--map-pipes row display)
    (overlay-put overlay 'display display)
    (overlay-put overlay 'markdown-ts-appear-table--display display)
    (overlay-put overlay 'markdown-ts-appear-table--wrapped t)
    (overlay-put overlay 'markdown-ts-appear-table--row-beg beg)
    (overlay-put overlay 'markdown-ts-appear-table--row-end end)
    (overlay-put overlay 'evaporate t)
    (when window
      (overlay-put overlay 'window window))
    (push overlay markdown-ts-appear-table--overlays)))

(defun markdown-ts-appear-table--render-table (table windows width)
  "Render TABLE in WINDOWS, optionally overriding window widths with WIDTH."
  (let* ((rows (markdown-ts-appear-table--rows table))
         (markdown-table-wrap-pretty-prettify t)
         (cells (mapcar (lambda (row)
                          (unless (equal (treesit-node-type row)
                                         "pipe_table_delimiter_row")
                            (mapcar #'markdown-ts-appear-table--cell
                                    (markdown-ts-appear-table--row-cells row))))
                        rows))
         (aligns (mapcar
                  (lambda (cell)
                    (let ((text (string-trim (treesit-node-text cell t))))
                      (cond ((and (string-prefix-p ":" text)
                                  (string-suffix-p ":" text)) 'center)
                            ((string-suffix-p ":" text) 'right)
                            (t 'left))))
                  (markdown-ts-appear-table--row-cells (cadr rows))))
         (prefixes (mapcar #'markdown-ts-appear-table--prefix rows))
         (prefix-width (apply #'max 0 (mapcar #'string-width prefixes))))
    (dolist (window windows)
      (let ((widths (markdown-table-wrap-compute-widths
                     (car cells) (cddr cells)
                     (max 1 (- (or width (markdown-ts-appear-table--window-width window))
                               prefix-width))
                     (length aligns))))
        (cl-mapc
         (lambda (row contents prefix)
           (markdown-ts-appear-table--make-overlay
            row
            (mapcar
             (lambda (line) (concat prefix line))
             (if (equal (treesit-node-type row) "pipe_table_delimiter_row")
                 (list (markdown-table-wrap-pretty--render-separator-line widths aligns))
               (markdown-table-wrap-pretty--render-row-lines contents widths aligns)))
            window))
         rows cells prefixes)))))

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
          (markdown-ts-appear-table--render-table
           table (if width (list nil) (markdown-ts-appear-table--display-windows))
           width)))))
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

(defun markdown-ts-appear-table--source-map (display beg end)
  "Map source BEG..END to (GLYPH-BEG GLYPH-END LINE-BOUNDS) in DISPLAY.
Hidden markup and discarded whitespace use the nearest surviving source
character.  The mapping can run backwards when multiple cells wrap."
  (let ((map (make-vector (- end beg) nil))
        (offset 0)
        (line (cons 0 (or (string-search "\n" display) (length display))))
        (runs (make-hash-table :test #'eq))
        previous)
    (dolist (glyph (string-glyph-split display))
      (when (> offset (cdr line))
        (setq line (cons offset (or (string-search "\n" display offset)
                                   (length display)))))
      (let ((bounds (list offset (+ offset (length glyph)) line)))
        (dotimes (index (length glyph))
          (when-let* ((source (get-text-property
                              index 'markdown-ts-appear-table--source glyph)))
            (when (consp source)
              (let ((position (gethash source runs 0))
                    (text (cdr source)))
                (while (and (< position (length text))
                            (/= (aref text position) (aref glyph index)))
                  (setq position (1+ position)))
                (puthash source (1+ position) runs)
                (setq source (+ (car source) position))
                (put-text-property (+ offset index) (+ offset index 1)
                                   'markdown-ts-appear-table--source source display)))
            (when (and (<= beg source) (< source end))
              (aset map (- source beg) bounds))))
        (setq offset (cadr bounds))))
    ;; Fill gaps in source order, never in visual order across columns.
    (dotimes (index (length map))
      (when (aref map index)
        (let ((left (or previous -1)))
          (cl-loop for gap from (1+ left) below index
                   for boundary = (and previous (cadr (aref map left)))
                   do (aset
                       map gap
                       (cond
                        ((and boundary (< boundary (length display))
                              (memq (char-after (+ beg gap)) '(?\s ?\t))
                              (eq (aref display boundary) ?\s))
                         (list boundary (1+ boundary) (nth 2 (aref map left))))
                        ((and previous (< (- gap left) (- index gap)))
                         (aref map left))
                        (t (aref map index))))))
        (setq previous index)))
    (cl-loop for index from (if previous (1+ previous) 0) below (length map)
             do (aset map index (if previous (aref map previous)
                                 (list 0 1 (cons 0 (length display))))))
    map))

(defun markdown-ts-appear-table--cursor-row-current-p (window)
  "Return non-nil when the interactive row still contains point in WINDOW."
  (pcase markdown-ts-appear-table--cursor-row
    (`(,beg ,end ,row-window ,base-overlay)
     (and (eq window row-window)
          (overlay-buffer base-overlay)
          (<= beg (point))
          (or (< (point) end)
              (and (= end (point-max)) (= (point) end)
                   (not (eq (char-before) ?\n))))))))

(defun markdown-ts-appear-table--row-overlay-at-point (window)
  "Return the effective wrapped-row overlay at point in WINDOW."
  (let ((positions (list (point))))
    (when (and (= (point) (point-max)) (> (point) (point-min))
               (not (eq (char-before) ?\n)))
      (setq positions (append positions (list (1- (point))))))
    (catch 'overlay
      (dolist (position positions)
        (pcase-let ((`(,display . ,overlay)
                     (get-char-property-and-overlay
                      position 'display window)))
          (when (and display overlay
                     (overlay-get overlay
                                  'markdown-ts-appear-table--wrapped))
            (throw 'overlay overlay)))))))

(defun markdown-ts-appear-table--make-cursor-overlays (row-overlay window)
  "Make ROW-OVERLAY interactive in WINDOW without revealing its source."
  (let* ((beg (overlay-get row-overlay 'markdown-ts-appear-table--row-beg))
         (end (overlay-get row-overlay 'markdown-ts-appear-table--row-end))
         (display (overlay-get row-overlay 'markdown-ts-appear-table--display)))
    (unless (overlay-get row-overlay 'markdown-ts-appear-table--map)
      (overlay-put row-overlay 'markdown-ts-appear-table--map
                   (markdown-ts-appear-table--source-map display beg end)))
    (dotimes (_ 3)
      (let ((overlay (make-overlay beg beg)))
        (overlay-put overlay 'display "")
        (overlay-put overlay 'markdown-ts-appear-table--cursor t)
        (overlay-put overlay 'priority '(nil . 1))
        (overlay-put overlay 'window window)
        (push overlay markdown-ts-appear-table--cursor-overlays)))
    (setq markdown-ts-appear-table--cursor-row
          (list beg end window row-overlay))))

(defun markdown-ts-appear-table--place-cursor ()
  "Anchor the row's display at the real source character under point."
  (pcase-let* ((`(,beg ,end ,_window ,row) markdown-ts-appear-table--cursor-row)
               (`(,before ,anchor ,after) markdown-ts-appear-table--cursor-overlays)
               (position (min (point) (1- end)))
               (bounds (aref (overlay-get row 'markdown-ts-appear-table--map)
                             (- position beg)))
               (line (nth 2 bounds))
               (display (overlay-get row 'markdown-ts-appear-table--display)))
    (unless (and (= (overlay-start anchor) position)
                 (= (overlay-end anchor) (1+ position)))
      (move-overlay before beg position)
      (move-overlay anchor position (1+ position))
      (move-overlay after (1+ position) end))
    ;; Reuse the surrounding strings throughout a visual line.  Only the
    ;; short line's cursor property changes on ordinary horizontal motion.
    (unless (equal line (overlay-get anchor 'markdown-ts-appear-table--line))
      (overlay-put anchor 'before-string (substring display 0 (car line)))
      (overlay-put anchor 'display (substring display (car line) (cdr line)))
      (overlay-put anchor 'after-string (substring display (cdr line)))
      (overlay-put anchor 'markdown-ts-appear-table--line line)
      (overlay-put anchor 'markdown-ts-appear-table--offset nil))
    (let ((offset (- (car bounds) (car line)))
          (old (overlay-get anchor 'markdown-ts-appear-table--offset))
          (text (overlay-get anchor 'display)))
      (unless (eq offset old)
        (when old (remove-text-properties old (1+ old) '(cursor nil) text))
        (put-text-property offset (1+ offset) 'cursor t text)
        (overlay-put anchor 'markdown-ts-appear-table--offset offset)))
    (setq disable-point-adjustment t)))

(defun markdown-ts-appear-table--update-cursor-row ()
  "Keep only the rendered row at point split into interactive display units."
  (let ((window (selected-window)))
    (cond
     ((or markdown-ts-appear-table--view
          (not (eq (window-buffer window) (current-buffer))))
      (markdown-ts-appear-table--delete-cursor-overlays))
     ((markdown-ts-appear-table--cursor-row-current-p window))
     (t
      (markdown-ts-appear-table--delete-cursor-overlays)
      (when-let* ((overlay
                   (markdown-ts-appear-table--row-overlay-at-point window)))
        (markdown-ts-appear-table--make-cursor-overlays overlay window))))
    (when markdown-ts-appear-table--cursor-row
      (markdown-ts-appear-table--place-cursor))))

(defun markdown-ts-appear-table--update-visibility ()
  "Synchronize wrapped rows with the main mode's visible source region."
  (let ((view (markdown-ts-appear-table--visible-region)))
    (unless (equal view markdown-ts-appear-table--view)
      (markdown-ts-appear-table--set-region-display
       markdown-ts-appear-table--view t)
      (markdown-ts-appear-table--set-region-display view nil)
      (setq markdown-ts-appear-table--view view))
    (markdown-ts-appear-table--update-cursor-row)))

(defun markdown-ts-appear-table--post-command ()
  "Rebuild edited tables and synchronize source visibility after a command."
  (when (and (eq markdown-ts-appear-table-style 'wrapped)
             (markdown-ts-appear--active-p))
    (if markdown-ts-appear-table--dirty
        (markdown-ts-appear-table--render)
      (markdown-ts-appear-table--update-visibility))))

(defun markdown-ts-appear-table--selection-change (_window)
  "Update the interactive rendered row after a window selection change."
  (when (and (eq markdown-ts-appear-table-style 'wrapped)
             (markdown-ts-appear--active-p))
    (markdown-ts-appear-table--update-cursor-row)))

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
    (unless (fboundp 'markdown-table-wrap-pretty--render-row-lines)
      (user-error "Wrapped tables require markdown-table-wrap 0.2.0"))
    (add-hook 'post-command-hook #'markdown-ts-appear-table--post-command 20 t)
    (add-hook 'after-change-functions #'markdown-ts-appear-table--after-change 80 t)
    (add-hook 'window-configuration-change-hook
              #'markdown-ts-appear-table--schedule-render nil t)
    (add-hook 'window-selection-change-functions
              #'markdown-ts-appear-table--selection-change nil t)
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
  (remove-hook 'window-selection-change-functions
               #'markdown-ts-appear-table--selection-change t)
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
  (remove-hook 'window-selection-change-functions
               #'markdown-ts-appear-table--selection-change t)
  (with-suppressed-warnings ((obsolete outline-view-change-hook))
    (remove-hook 'outline-view-change-hook
                 #'markdown-ts-appear-table--schedule-render t))
  (setq markdown-ts-appear-table--overlays nil
        markdown-ts-appear-table--cursor-overlays nil
        markdown-ts-appear-table--cursor-row nil
        markdown-ts-appear-table--resize-timer nil
        markdown-ts-appear-table--dirty nil))

(provide 'markdown-ts-appear-table)
;;; markdown-ts-appear-table.el ends here
