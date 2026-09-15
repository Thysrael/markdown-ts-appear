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
  "Source-row overlay currently made interactive at point.")

(cl-defstruct (markdown-ts-appear-table--layout
               (:constructor markdown-ts-appear-table--make-layout))
  "A row's display and lazily constructed, shared motion geometry."
  display map lines)

(cl-defstruct (markdown-ts-appear-table--line
               (:constructor markdown-ts-appear-table--make-line))
  "A visual line's string bounds, display width and source cursor stops."
  beg end width stops)

(cl-defstruct (markdown-ts-appear-table--position
               (:constructor markdown-ts-appear-table--make-position))
  "A source character's display bounds, visual line index and column."
  beg end line column)

(defvar-local markdown-ts-appear-table--dirty nil
  "Non-nil when wrapped tables need rebuilding after an edit.")

(defvar-local markdown-ts-appear-table--view nil
  "Last visible source region applied to wrapped-table overlays.")

(defvar-local markdown-ts-appear-table--resize-timer nil
  "Idle timer used to debounce wrapped-table rendering after resize.")

(defvar-local markdown-ts-appear-table--windows nil
  "Window widths and font attributes used by the last successful render.")

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
        markdown-ts-appear-table--windows nil
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

(defun markdown-ts-appear-table--window-state (&optional width)
  "Return window, column width and font records, optionally overriding WIDTH."
  (mapcar
   (lambda (window)
     (list window (or width (markdown-ts-appear-table--window-width window))
           (mapcar (lambda (attribute)
                     (face-attribute 'markdown-ts-table attribute
                                     (and window (window-frame window)) 'default))
                   '(:family :foundry :width :height :weight :slant :fontset))))
   (if width (list nil) (markdown-ts-appear-table--display-windows))))

(defun markdown-ts-appear-table--display-string (lines newline-p)
  "Join display LINES and preserve the source newline when NEWLINE-P."
  (let ((display (concat (mapconcat #'identity lines "\n")
                         (and newline-p "\n"))))
    ;; A replacement string otherwise inherits unspecified attributes from
    ;; its source character.  Moving its anchor across a shadow-faced pipe
    ;; would recolor the entire row, including its before/after strings.
    (add-face-text-property 0 (length display)
                            '(markdown-ts-table default) t display)
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
    (overlay-put overlay 'markdown-ts-appear-table--layout
                 (markdown-ts-appear-table--make-layout :display display))
    (overlay-put overlay 'markdown-ts-appear-table--wrapped t)
    (overlay-put overlay 'evaporate t)
    (when window
      (overlay-put overlay 'window window))
    (push overlay markdown-ts-appear-table--overlays)))

(defun markdown-ts-appear-table--render-table (table windows)
  "Render TABLE using the window and width records in WINDOWS."
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
    (pcase-dolist (`(,window ,width ,_font) windows)
      (let ((widths (markdown-table-wrap-compute-widths
                     (car cells) (cddr cells)
                     (max 1 (- width prefix-width))
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
  (let ((windows (markdown-ts-appear-table--window-state width)))
    (markdown-ts-appear-table--delete-overlays)
    (when (and (eq markdown-ts-appear-table-style 'wrapped)
               (markdown-ts-appear--active-p))
      (save-restriction
        (widen)
        (dolist (table (markdown-ts-appear-table--tables))
          (unless (markdown-ts--outline-invisible-p (treesit-node-start table))
            (markdown-ts-appear-table--render-table table windows))))
      (setq markdown-ts-appear-table--windows windows)))
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
                (markdown-ts-appear-table--layout-display
                 (overlay-get overlay 'markdown-ts-appear-table--layout)))))))))

(defun markdown-ts-appear-table--visible-region ()
  "Return numeric bounds of source currently revealed by the main mode."
  (when-let* ((region markdown-ts-appear--region)
              (beg (marker-position (car region)))
              (end (marker-position (cdr region))))
    (cons beg end)))

(defun markdown-ts-appear-table--build-layout (layout beg end)
  "Build LAYOUT's source map and visual lines for BEG..END in one glyph pass.
Hidden markup and discarded whitespace use the nearest surviving source
character.  Navigation stops are offsets from the source-row overlay."
  (let ((display (markdown-ts-appear-table--layout-display layout))
        (map (make-vector (- end beg) nil))
        (offset 0)
        (line-beg 0) (line-index 0) (column 0)
        (runs (make-hash-table :test #'eq))
        stops lines previous)
    (dolist (glyph (string-glyph-split display))
      (let ((position (markdown-ts-appear-table--make-position
                       :beg offset :end (+ offset (length glyph))
                       :line line-index :column column)))
        (dotimes (index (length glyph))
          (when-let* ((source (get-text-property
                              index 'markdown-ts-appear-table--source glyph)))
            (when (consp source)
              (let ((source-index (gethash source runs 0))
                    (text (cdr source)))
                (while (and (< source-index (length text))
                            (/= (aref text source-index) (aref glyph index)))
                  (setq source-index (1+ source-index)))
                (puthash source (1+ source-index) runs)
                (setq source (+ (car source) source-index))
                (put-text-property (+ offset index) (+ offset index 1)
                                   'markdown-ts-appear-table--source source display)))
            (when (and (<= beg source) (< source end))
              (aset map (- source beg) position)
              (when (and (= index 0) (not (equal glyph "\n")))
                (push (cons column (- source beg)) stops)))))
        (if (equal glyph "\n")
            (progn
              (push (markdown-ts-appear-table--make-line
                     :beg line-beg :end offset :width column
                     :stops (nreverse stops)) lines)
              (setq line-beg (1+ offset) line-index (1+ line-index)
                    column 0 stops nil))
          (setq column (+ column (string-width glyph))))
        (setq offset (markdown-ts-appear-table--position-end position))))
    (when (or (> offset line-beg) (null lines))
      (push (markdown-ts-appear-table--make-line
             :beg line-beg :end offset :width column :stops (nreverse stops)) lines))
    ;; Fill gaps in source order, never in visual order across columns.
    (dotimes (index (length map))
      (when (aref map index)
        (let* ((left (or previous -1))
               (position (and previous (aref map left)))
               (boundary (and position (markdown-ts-appear-table--position-end position)))
               (padding
                (when (and (> index (1+ left)) boundary (< boundary (length display))
                           (eq (aref display boundary) ?\s))
                  (markdown-ts-appear-table--make-position
                   :beg boundary :end (1+ boundary)
                   :line (markdown-ts-appear-table--position-line position)
                   :column (+ (markdown-ts-appear-table--position-column position)
                              (string-width
                               (substring display
                                          (markdown-ts-appear-table--position-beg position)
                                          boundary)))))))
          (cl-loop for gap from (1+ left) below index
                   do (aset
                       map gap
                       (cond
                        ((and padding (memq (char-after (+ beg gap)) '(?\s ?\t)))
                         padding)
                        ((and previous (< (- gap left) (- index gap)))
                         (aref map left))
                        (t (aref map index))))))
        (setq previous index)))
    (let ((last (if previous (aref map previous)
                  (markdown-ts-appear-table--make-position
                   :beg 0 :end 1 :line 0 :column 0))))
      (cl-loop for index from (if previous (1+ previous) 0) below (length map)
               do (aset map index last)))
    (setf (markdown-ts-appear-table--layout-map layout) map
          (markdown-ts-appear-table--layout-lines layout) (vconcat (nreverse lines)))
    layout))

(defun markdown-ts-appear-table--layout (row)
  "Return ROW's layout, lazily preparing its motion geometry."
  (let ((layout (overlay-get row 'markdown-ts-appear-table--layout)))
    (unless (markdown-ts-appear-table--layout-map layout)
      (markdown-ts-appear-table--build-layout layout (overlay-start row) (overlay-end row)))
    layout))

(defun markdown-ts-appear-table--cursor-row-current-p (window)
  "Return non-nil when the interactive row still contains point in WINDOW."
  (when-let* ((row markdown-ts-appear-table--cursor-row)
              ((overlay-buffer row))
              (beg (overlay-start row))
              (end (overlay-end row)))
    (and (eq window (overlay-get (cadr markdown-ts-appear-table--cursor-overlays) 'window))
         (<= beg (point))
         (or (< (point) end)
             (and (= end (point-max)) (= (point) end)
                  (not (eq (char-before) ?\n)))))))

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
  (let ((beg (overlay-start row-overlay)))
    (markdown-ts-appear-table--layout row-overlay)
    (dotimes (_ 3)
      (let ((overlay (make-overlay beg beg)))
        (overlay-put overlay 'display "")
        (overlay-put overlay 'markdown-ts-appear-table--cursor t)
        (overlay-put overlay 'priority '(nil . 1))
        (overlay-put overlay 'window window)
        (push overlay markdown-ts-appear-table--cursor-overlays)))
    (setq markdown-ts-appear-table--cursor-row row-overlay)))

(defun markdown-ts-appear-table--goto-column (row line column)
  "Move to ROW's source stop on visual LINE nearest COLUMN.
A negative LINE counts backwards from the last visual line."
  (let* ((lines (markdown-ts-appear-table--layout-lines (markdown-ts-appear-table--layout row)))
         (stops (markdown-ts-appear-table--line-stops
                 (aref lines (if (< line 0) (+ (length lines) line) line))))
         best distance)
    (dolist (stop stops)
      (let ((delta (abs (- column (car stop)))))
        (when (and (<= (point-min) (+ (overlay-start row) (cdr stop)) (point-max))
                   (or (null distance) (< delta distance)))
          (setq best (+ (overlay-start row) (cdr stop)) distance delta))))
    (goto-char (or best (overlay-start row)))))

(defun markdown-ts-appear-table--source-column (&optional target)
  "Return the raw source column, or move to column TARGET, ignoring display."
  (let ((end (if target (line-end-position) (point)))
        (column 0))
    (goto-char (line-beginning-position))
    (while (and (< (point) end) (or (null target) (< column target)))
      (let ((next (+ column (if (eq (char-after) ?\t)
                               (- tab-width (% column tab-width))
                             (char-width (char-after))))))
        (if (and target (> next target))
            (setq end (point))
          (setq column next)
          (forward-char))))
    column))

(defun markdown-ts-appear-table--motion-row (window)
  "Return the wrapped row at point in WINDOW, including the active anchor."
  (if (markdown-ts-appear-table--cursor-row-current-p window)
      markdown-ts-appear-table--cursor-row
    (markdown-ts-appear-table--row-overlay-at-point window)))

(defun markdown-ts-appear-table--visual-column (row)
  "Return point's rendered column in ROW without counting hidden source."
  (let* ((layout (markdown-ts-appear-table--layout row))
         (lines (markdown-ts-appear-table--layout-lines layout)))
    (if (= (point) (overlay-end row))
        (markdown-ts-appear-table--line-width (aref lines (1- (length lines))))
      (markdown-ts-appear-table--position-column
       (aref (markdown-ts-appear-table--layout-map layout)
             (- (point) (overlay-start row)))))))

(defun markdown-ts-appear-table--visual-step (function step column noerror rest)
  "Move one visual STEP toward COLUMN, using FUNCTION outside wrapped rows.
NOERROR and REST are passed to the native line motion when needed."
  (let* ((window (selected-window))
         (row (markdown-ts-appear-table--motion-row window)))
    (if (null row)
        (let ((last-command 'next-line))
          (apply function step noerror rest)
          (when-let* ((target (markdown-ts-appear-table--row-overlay-at-point window)))
            (markdown-ts-appear-table--goto-column
             target (if (> step 0) 0 -1) column)))
      (let* ((layout (markdown-ts-appear-table--layout row))
             (lines (markdown-ts-appear-table--layout-lines layout))
             (map (markdown-ts-appear-table--layout-map layout))
             (bounds (aref map (min (1- (length map)) (- (point) (overlay-start row)))))
             (line (+ step (if (= (point) (overlay-end row))
                               (1- (length lines))
                             (markdown-ts-appear-table--position-line bounds)))))
        (if (and (>= line 0) (< line (length lines)))
            (markdown-ts-appear-table--goto-column row line column)
          (let ((next (if (> step 0) (overlay-end row) (1- (overlay-start row)))))
            (if (or (< next (point-min)) (> next (point-max))
                    (and (> step 0) (= next (point-max))
                         (not (eq (char-before next) ?\n))))
                (unless noerror
                  (signal (if (> step 0) 'end-of-buffer 'beginning-of-buffer) nil))
              (goto-char next)
              (if-let* ((target (markdown-ts-appear-table--row-overlay-at-point window)))
                  (markdown-ts-appear-table--goto-column
                   target (if (> step 0) 0 -1) column)
                (vertical-motion (cons column 0))))))))))

(defun markdown-ts-appear-table--line-move
    (function count &optional noerror &rest rest)
  "Call line motion FUNCTION with COUNT, preserving wrapped-table columns.
NOERROR and REST retain the native command's boundary behavior and options."
  (if (not (and (eq markdown-ts-appear-table-style 'wrapped)
                (markdown-ts-appear--active-p)
                (not markdown-ts-appear--region)
                (eq (window-buffer (selected-window)) (current-buffer))))
      (apply function count noerror rest)
    (let* ((window (selected-window))
           (row (markdown-ts-appear-table--motion-row window)))
      (if (null row)
          (prog1 (apply function count noerror rest)
            (when-let* ((target (markdown-ts-appear-table--row-overlay-at-point window))
                        (column (or goal-column
                                    (if (consp temporary-goal-column)
                                        (car temporary-goal-column)
                                      temporary-goal-column)))
                        ((numberp column)))
              (if line-move-visual
                  (markdown-ts-appear-table--goto-column
                   target (if (< count 0) -1 0) column)
                (markdown-ts-appear-table--source-column column))
              (setq disable-point-adjustment t)))
        (let* ((saved (if (consp temporary-goal-column)
                          (car temporary-goal-column) temporary-goal-column))
               (column (or goal-column
                           (and (memq last-command '(next-line previous-line))
                                (numberp saved) saved)
                           (if line-move-visual
                               (markdown-ts-appear-table--visual-column row)
                             (save-excursion
                               (markdown-ts-appear-table--source-column)))))
               (complete t))
          (setq temporary-goal-column column)
          (if line-move-visual
              (catch 'boundary
                (dotimes (_ (abs count))
                  (let ((position (point)))
                    (markdown-ts-appear-table--visual-step
                     function (if (< count 0) -1 1) column noerror rest)
                    (when (= position (point))
                      (setq complete nil)
                      (throw 'boundary nil)))))
            (let ((remaining (forward-line count)))
              ;; Native callers such as `move-end-of-line' deliberately
              ;; overshoot a narrowed buffer with NOERROR.  Keep its boundary
              ;; position instead of moving back into the same source line.
              (if (and (= remaining 0)
                       (or (<= count 0) (< (point) (point-max))
                           (eq (char-before) ?\n)))
                  (markdown-ts-appear-table--source-column column)
                (setq complete nil)
                (unless noerror
                  (signal (if (> count 0) 'end-of-buffer 'beginning-of-buffer) nil)))))
          (setq disable-point-adjustment t)
          complete)))))

(defun markdown-ts-appear-table--place-cursor ()
  "Anchor the row's display at the real source character under point."
  (pcase-let* ((row markdown-ts-appear-table--cursor-row)
               (beg (overlay-start row)) (end (overlay-end row))
               (`(,before ,anchor ,after) markdown-ts-appear-table--cursor-overlays)
               (position (min (point) (1- end)))
               (layout (overlay-get row 'markdown-ts-appear-table--layout))
               (bounds (aref (markdown-ts-appear-table--layout-map layout)
                             (- position beg)))
               (line (aref (markdown-ts-appear-table--layout-lines layout)
                            (markdown-ts-appear-table--position-line bounds)))
               (display (markdown-ts-appear-table--layout-display layout)))
    (unless (and (= (overlay-start anchor) position)
                 (= (overlay-end anchor) (1+ position)))
      (move-overlay before beg position)
      (move-overlay anchor position (1+ position))
      (move-overlay after (1+ position) end))
    ;; Keep the strings on fixed row endpoints: attaching them to the moving
    ;; anchor leaks the following source face into extended backgrounds.
    ;; Reuse them throughout a visual line; only its cursor property changes.
    (unless (eq line (overlay-get anchor 'markdown-ts-appear-table--line))
      (overlay-put before 'before-string
                   (substring display 0 (markdown-ts-appear-table--line-beg line)))
      (overlay-put anchor 'display
                   (substring display (markdown-ts-appear-table--line-beg line)
                              (markdown-ts-appear-table--line-end line)))
      (overlay-put after 'after-string
                   (substring display (markdown-ts-appear-table--line-end line)))
      (overlay-put anchor 'markdown-ts-appear-table--line line)
      (overlay-put anchor 'markdown-ts-appear-table--offset nil))
    (let ((offset (- (markdown-ts-appear-table--position-beg bounds)
                     (markdown-ts-appear-table--line-beg line)))
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

(defun markdown-ts-appear-table--refresh-windows ()
  "Rebuild tables only when the window geometry or fonts have changed."
  (let ((windows (markdown-ts-appear-table--window-state)))
    ;; Selection changes may reorder the windows without changing any layout.
    (unless (and (= (length windows) (length markdown-ts-appear-table--windows))
                 (cl-every (lambda (state)
                             (equal (cdr state)
                                    (cdr (assq (car state) markdown-ts-appear-table--windows))))
                           windows))
      (markdown-ts-appear-table--render))))

(defun markdown-ts-appear-table--outline-change ()
  "Invalidate visibility-dependent layouts even when window widths agree."
  (setq markdown-ts-appear-table--windows nil)
  (markdown-ts-appear-table--schedule-render))

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
                   (markdown-ts-appear-table--refresh-windows)))))))))

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
                #'markdown-ts-appear-table--outline-change nil t))
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
                 #'markdown-ts-appear-table--outline-change t))
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
                 #'markdown-ts-appear-table--outline-change t))
  (setq markdown-ts-appear-table--overlays nil
        markdown-ts-appear-table--windows nil
        markdown-ts-appear-table--cursor-overlays nil
        markdown-ts-appear-table--cursor-row nil
        markdown-ts-appear-table--resize-timer nil
        markdown-ts-appear-table--dirty nil))

(provide 'markdown-ts-appear-table)
;;; markdown-ts-appear-table.el ends here
