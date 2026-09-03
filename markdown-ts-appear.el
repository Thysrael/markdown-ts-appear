;;; markdown-ts-appear.el --- Reveal Markdown source and preview math -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael

;; Author: Thysrael <thysrael@163.com>
;; Assisted-by: OpenCode:gpt-5.6-sol
;; Maintainer: Thysrael <thysrael@163.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1"))
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

;; `markdown-ts-appear-mode' hides rendered Markdown markup and reveals the
;; smallest semantic element at point.  It can also render LaTeX fragments
;; asynchronously with MathJax while they are not being edited.
;;
;; Enable it with:
;;
;;   (add-hook 'markdown-ts-mode-hook #'markdown-ts-appear-mode)

;;; Code:

(require 'markdown-ts-mode)
(require 'seq)
(require 'svg)

(declare-function mathjax-available-p "mathjax")
(declare-function mathjax-render "mathjax")

(defvar evil-insert-state-entry-hook)
(defvar evil-insert-state-exit-hook)
(defvar evil-state)
(defvar meow-insert-enter-hook)
(defvar meow-insert-exit-hook)
(defvar meow-insert-mode)

(defgroup markdown-ts-appear nil
  "Reveal rendered Markdown source at point."
  :group 'markdown-ts)

(defun markdown-ts-appear--set-positive-number (symbol value)
  "Set SYMBOL to positive numeric VALUE."
  (unless (and (numberp value) (> value 0))
    (user-error "%s must be a positive number" symbol))
  (set-default symbol value))

(defcustom markdown-ts-appear-trigger 'always
  "When `markdown-ts-appear-mode' should reveal source.
With `always', track point whenever the mode is enabled.  With
`evil-insert' or `meow-insert', track point only while the corresponding
modal editor is in insert state; both editors remain optional dependencies."
  :type '(choice (const :tag "Whenever the mode is enabled" always)
                 (const :tag "Only in Evil insert state" evil-insert)
                 (const :tag "Only in Meow insert state" meow-insert))
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-enable-math-preview nil
  "Whether `markdown-ts-appear-mode' should preview LaTeX with MathJax.
The optional `mathjax' package must be installed separately when this is
non-nil."
  :type 'boolean
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-math-timeout 10
  "Seconds to wait for a MathJax rendering result."
  :type 'number
  :set #'markdown-ts-appear--set-positive-number
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-center-display-math t
  "Whether display math should be centered in its window."
  :type 'boolean
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-math-scale 1.1
  "Scale factor applied to rendered MathJax formulas.
This scales the SVG's intrinsic dimensions without replacing Emacs' automatic
  high-DPI image scaling."
  :type 'number
  :set #'markdown-ts-appear--set-positive-number
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-link-icon nil
  "Icon displayed before rendered Markdown links.
The value may be a string, a cons of preferred and fallback strings, or nil."
  :type '(choice (const :tag "No icon" nil)
                 (string :tag "Icon")
                 (cons :tag "Preferred and fallback"
                       (string :tag "Preferred")
                       (string :tag "Fallback")))
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-image-icon nil
  "Icon displayed before rendered Markdown images.
The value has the same form as `markdown-ts-appear-link-icon'."
  :type '(choice (const :tag "No icon" nil)
                 (string :tag "Icon")
                 (cons :tag "Preferred and fallback"
                       (string :tag "Preferred")
                       (string :tag "Fallback")))
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-wikilink-icon nil
  "Icon displayed before rendered Markdown Wiki links.
The value has the same form as `markdown-ts-appear-link-icon'."
  :type '(choice (const :tag "No icon" nil)
                 (string :tag "Icon")
                 (cons :tag "Preferred and fallback"
                       (string :tag "Preferred")
                       (string :tag "Fallback")))
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-code-fence-style 'raw
  "How rendered fenced code blocks should display their delimiters."
  :type '(choice (const :tag "Raw Markdown" raw)
                  (const :tag "Connected Unicode lines" connected))
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-label-caps nil
  "Left and right strings surrounding rendered labels.
When nil, labels contain only their text with surrounding padding.
Powerline users can set this to a cons such as (\"\" . \"\")."
  :type '(choice (const :tag "No caps" nil)
                  (cons :tag "Left and right caps"
                        (string :tag "Left cap")
                        (string :tag "Right cap")))
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-render-callouts nil
  "Whether to render callout labels at the start of block quotes."
  :type 'boolean
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-block-quote-marker nil
  "Marker replacing each marker of a rendered block quote.
The value has the same form as `markdown-ts-appear-link-icon'."
  :type '(choice (const :tag "Display raw marker" nil)
                 (string :tag "Marker")
                 (cons :tag "Preferred and fallback"
                       (string :tag "Preferred")
                       (string :tag "Fallback")))
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-table-style 'raw
  "How rendered Markdown pipe tables should display their delimiters."
  :type '(choice (const :tag "Raw Markdown" raw)
                 (const :tag "Unicode delimiters" unicode))
  :group 'markdown-ts-appear)

(defface markdown-ts-appear-code-fence-marker
  '((t :inherit (markdown-ts-language-keyword markdown-ts-code-block)))
  "Face used for rendered fenced code block markers."
  :group 'markdown-ts-appear)

(defface markdown-ts-appear-label
  '((t :inverse-video t))
  "Face used to invert rendered code language and callout labels."
  :group 'markdown-ts-appear)

(defface markdown-ts-appear-block-quote
  '((t :inherit markdown-ts-block-quote))
  "Face used for rendered block quotes."
  :group 'markdown-ts-appear)

(defface markdown-ts-appear-block-quote-marker
  '((t :inherit font-lock-comment-face))
  "Face used for rendered block quote markers."
  :group 'markdown-ts-appear)

(defface markdown-ts-appear-table-border
  '((t :inherit markdown-ts-table-delimiter-cell))
  "Face used for rendered table borders."
  :group 'markdown-ts-appear)

(defvar-local markdown-ts-appear-mode nil
  "Non-nil when Markdown TS Appear mode is enabled.")

(defvar-local markdown-ts-appear--math-preview-active-p nil
  "Non-nil when this buffer actively renders MathJax previews.")

(defvar-local markdown-ts-appear--region nil
  "Markers delimiting the semantic Markdown source currently visible.")

(defvar-local markdown-ts-appear--last-point nil
  "Buffer position checked by the most recent appear update.")

(defvar-local markdown-ts-appear--last-tick nil
  "Buffer modification tick checked by the most recent appear update.")

(defvar-local markdown-ts-appear--last-range-line nil
  "Line beginning whose inline Tree-sitter ranges were last updated.")

(defvar-local markdown-ts-appear--last-range-tick nil
  "Modification tick of the last inline Tree-sitter range update.")

(defvar-local markdown-ts-appear--previous-hide-markup nil
  "Value of `markdown-ts-hide-markup' before appear mode was enabled.")

(defvar-local markdown-ts-appear--previous-hide-markup-local-p nil
  "Whether `markdown-ts-hide-markup' was buffer-local before appear mode.")

(defvar-local markdown-ts-appear--setup-p nil
  "Non-nil when Markdown TS Appear buffer integration is active.")

(defvar-local markdown-ts-appear--managed-line-height-p nil
  "Non-nil when appear mode added `line-height' to managed properties.")

(defvar-local markdown-ts-appear--managed-code-prefix-properties nil
  "Code prefix properties added to `font-lock-extra-managed-props'.")

(defvar-local markdown-ts-appear--notified-parsers nil
  "Tree-sitter parsers carrying the package change notifier.")

(defvar-local markdown-ts-appear--inline-parser-notified-p nil
  "Non-nil when the inline parser carries the package notifier.")

(defvar-local markdown-ts-appear--base-owner nil
  "Base buffer whose appear rendering is shared by an indirect clone.")

(defvar-local markdown-ts-appear--math-filter-installed-p nil
  "Non-nil when the math preview copy filter is installed.")

(defvar-local markdown-ts-appear--decoration-filter-installed-p nil
  "Non-nil when the decoration copy filter is installed.")

(defvar-local markdown-ts-appear--block-font-lock-installed-p nil
  "Non-nil when block rendering font-lock rules are installed.")

(defvar-local markdown-ts-appear--indirect-clones nil
  "Live indirect buffers sharing this buffer's appear rendering.")

(defvar markdown-ts-appear--tearing-down-buffer-p nil
  "Non-nil while cleanup is running for a buffer being discarded.")

(defvar markdown-ts-appear--refreshing-clone-edit-p nil
  "Non-nil while an indirect clone edit refreshes its owner buffer.")

(defvar markdown-ts-appear--quote-font-lock-settings)
(defvar markdown-ts-appear--code-font-lock-settings)
(defvar markdown-ts-appear--table-font-lock-settings)

(defvar markdown-ts-appear--math-cache (make-hash-table :test #'equal)
  "MathJax results keyed by formula text and display style.")

(defvar markdown-ts-appear--math-pending (make-hash-table :test #'equal)
  "Pending MathJax requests keyed by formula text and display style.")

(defconst markdown-ts-appear--math-cache-miss
  (make-symbol "markdown-ts-appear-math-cache-miss")
  "Sentinel used for missing MathJax cache entries.")

(defun markdown-ts-appear--active-p ()
  "Return non-nil when appear rendering owns this buffer's text."
  (or markdown-ts-appear-mode
      (and (buffer-live-p markdown-ts-appear--base-owner)
           (buffer-local-value
            'markdown-ts-appear-mode markdown-ts-appear--base-owner))))

(defun markdown-ts-appear--math-active-p ()
  "Return non-nil when math preview owns this buffer's shared text."
  (or markdown-ts-appear--math-preview-active-p
      (and (buffer-live-p markdown-ts-appear--base-owner)
           (buffer-local-value
            'markdown-ts-appear--math-preview-active-p
            markdown-ts-appear--base-owner))))

(defun markdown-ts-appear--live-indirect-clones ()
  "Return and retain only live indirect clones of the current buffer."
  (let ((base (current-buffer)))
    (setq markdown-ts-appear--indirect-clones
          (seq-filter
           (lambda (clone)
             (and (buffer-live-p clone)
                  (eq (buffer-local-value
                       'markdown-ts-appear--base-owner clone)
                      base)))
           markdown-ts-appear--indirect-clones))))

(defun markdown-ts-appear--deactivate-local-mode (symbol)
  "Set local mode variable SYMBOL to nil and unregister it."
  (set symbol nil)
  (setq local-minor-modes (delq symbol local-minor-modes)))

(defun markdown-ts-appear--save-hide-markup ()
  "Save the current value and locality of `markdown-ts-hide-markup'."
  (setq markdown-ts-appear--previous-hide-markup markdown-ts-hide-markup)
  (setq markdown-ts-appear--previous-hide-markup-local-p
        (local-variable-p 'markdown-ts-hide-markup)))

(defun markdown-ts-appear--restore-hide-markup ()
  "Restore the saved value and locality of `markdown-ts-hide-markup'."
  (if markdown-ts-appear--previous-hide-markup-local-p
      (setq markdown-ts-hide-markup
            markdown-ts-appear--previous-hide-markup)
    (kill-local-variable 'markdown-ts-hide-markup))
  (markdown-ts--set-hide-markup markdown-ts-hide-markup))

(defun markdown-ts-appear--validate-math-options ()
  "Signal a user error when a math preview option is invalid."
  (dolist (option `((markdown-ts-appear-math-timeout
                     . ,markdown-ts-appear-math-timeout)
                    (markdown-ts-appear-math-scale
                     . ,markdown-ts-appear-math-scale)))
    (unless (and (numberp (cdr option)) (> (cdr option) 0))
      (user-error "%s must be a positive number" (car option)))))

(defun markdown-ts-appear--display-string (value)
  "Resolve customizable display VALUE to a usable string."
  (cond
   ((stringp value) value)
   ((and (consp value) (stringp (car value)) (stringp (cdr value)))
    (if (and (> (length (car value)) 0)
             (seq-every-p #'char-displayable-p
                          (string-to-list (car value))))
        (car value)
      (cdr value)))))

(defun markdown-ts-appear--decorate (beg end string face)
  "Display STRING with FACE instead of text between BEG and END."
  (let ((display (copy-sequence string)))
    (when face
      (add-face-text-property 0 (length display) face t display))
    (with-silent-modifications
      (put-text-property beg end 'display display)
      (put-text-property beg end 'markdown-ts-appear--decoration t))))

(defun markdown-ts-appear--decorate-line-prefix (beg end prefix face)
  "Display PREFIX with FACE before visual lines between BEG and END."
  (let ((display (copy-sequence prefix)))
    (add-face-text-property 0 (length display) face t display)
    (with-silent-modifications
      (add-text-properties
       beg end `(line-prefix ,display wrap-prefix ,display
                 markdown-ts-appear--decoration t)))))

(defun markdown-ts-appear--label (text face)
  "Render TEXT as a padded inverse-video label over FACE."
  (let ((label-face (list 'markdown-ts-appear-label face)))
    (if-let* ((caps markdown-ts-appear-label-caps))
        (concat
         (propertize (car caps) 'face face)
         (propertize (concat " " text " ") 'face label-face)
         (propertize (cdr caps) 'face face))
      (propertize (concat " " text " ") 'face label-face))))

(defun markdown-ts-appear--code-quote-prefix (source)
  "Render quoted code prefix SOURCE followed by a code block marker."
  (let ((marker
         (or (markdown-ts-appear--display-string
              markdown-ts-appear-block-quote-marker)
             ">")))
    (concat
     (mapconcat
      (lambda (character)
        (if (eq character ?>)
            (propertize
             marker 'face '(markdown-ts-appear-block-quote-marker
                            markdown-ts-appear-code-fence-marker))
          (propertize
           (string character) 'face 'markdown-ts-appear-code-fence-marker)))
      source)
     (propertize "│ " 'face 'markdown-ts-appear-code-fence-marker))))

(defun markdown-ts-appear--node-ancestor (node type)
  "Return NODE or its nearest ancestor whose type is TYPE."
  (while (and node (not (equal (treesit-node-type node) type)))
    (setq node (treesit-node-parent node)))
  node)

(defun markdown-ts-appear--direct-children-of-type (node type)
  "Return direct children of NODE whose type is TYPE."
  (let (children)
    (dotimes (index (treesit-node-child-count node))
      (let ((child (treesit-node-child node index)))
        (when (equal (treesit-node-type child) type)
          (push child children))))
    (nreverse children)))

(defun markdown-ts-appear--first-direct-child-of-type (node type)
  "Return the first direct child of NODE whose type is TYPE."
  (catch 'child
    (dotimes (index (treesit-node-child-count node))
      (let ((child (treesit-node-child node index)))
        (when (equal (treesit-node-type child) type)
          (throw 'child child))))))

(defun markdown-ts-appear--visible-region ()
  "Return the reveal region governing the current buffer's text."
  (if (and (buffer-live-p markdown-ts-appear--base-owner)
           (buffer-local-value
            'markdown-ts-appear-mode markdown-ts-appear--base-owner))
      (buffer-local-value
       'markdown-ts-appear--region markdown-ts-appear--base-owner)
    markdown-ts-appear--region))

(defun markdown-ts-appear--region-visible-p (beg end)
  "Return non-nil when BEG through END overlaps visible Markdown source."
  (when-let* ((region (markdown-ts-appear--visible-region))
              (visible-beg (marker-position (car region)))
              (visible-end (marker-position (cdr region))))
    (and (< beg visible-end) (> end visible-beg))))

(defun markdown-ts-appear--node-visible-p (node)
  "Return non-nil when NODE overlaps visible semantic Markdown source."
  (markdown-ts-appear--region-visible-p
   (treesit-node-start node) (treesit-node-end node)))

(defun markdown-ts-appear--literal-block-at (position)
  "Return the literal Markdown block containing POSITION, if any."
  (let ((node (treesit-node-at position 'markdown)) block)
    (while (and node (not block))
      (when (and (member (treesit-node-type node)
                         '("fenced_code_block" "indented_code_block"
                           "html_block"))
                 (<= (treesit-node-start node) position)
                 (< position (treesit-node-end node)))
        (setq block node))
      (setq node (treesit-node-parent node)))
    block))

(defun markdown-ts-appear--node-ancestor-of-type (node type)
  "Return NODE's nearest ancestor whose type is TYPE."
  (let ((ancestor (treesit-node-parent node)) found)
    (while (and ancestor (not found))
      (if (equal (treesit-node-type ancestor) type)
          (setq found ancestor)
        (setq ancestor (treesit-node-parent ancestor))))
    found))

(defun markdown-ts-appear--wikilink-bounds-for-node (node)
  "Return Wiki link bounds around shortcut link NODE, if any."
  (when (and node (equal (treesit-node-type node) "shortcut_link"))
    (let ((previous (treesit-node-prev-sibling node))
          (next (treesit-node-next-sibling node)))
      (when (and previous next
                 (equal (treesit-node-type previous) "[")
                 (equal (treesit-node-type next) "]")
                 (= (treesit-node-end previous) (treesit-node-start node))
                 (= (treesit-node-start next) (treesit-node-end node))
                 (not (markdown-ts-appear--node-ancestor-of-type
                       node "image")))
        (cons (treesit-node-start previous) (treesit-node-end next))))))

(defun markdown-ts-appear--node-child-of-type (node type)
  "Return NODE's first direct child whose type is TYPE."
  (let ((index 0)
        (count (treesit-node-child-count node))
        child)
    (while (and (< index count) (not child))
      (let ((candidate (treesit-node-child node index)))
        (when (equal (treesit-node-type candidate) type)
          (setq child candidate)))
      (setq index (1+ index)))
    child))

(defun markdown-ts-appear--wikilink-bounds-at (position)
  "Return syntax-aware Wiki link bounds containing POSITION."
  (let ((positions (delete-dups
                    (list position
                          (max (point-min) (1- position))
                          (min (point-max) (1+ position)))))
        bounds)
    (while (and positions (not bounds))
      (let ((node (treesit-node-at (pop positions) 'markdown-inline)))
        (while (and node (not bounds))
          (setq bounds (markdown-ts-appear--wikilink-bounds-for-node node)
                node (treesit-node-parent node)))))
    (when (and bounds
               (<= (car bounds) position)
               (< position (cdr bounds)))
      bounds)))

(defun markdown-ts-appear--restore ()
  "Restore hidden markup in the previously revealed region."
  (when-let* ((region markdown-ts-appear--region)
              (beg (marker-position (car region)))
              (end (marker-position (cdr region))))
    (set-marker (car region) nil)
    (set-marker (cdr region) nil)
    (setq markdown-ts-appear--region nil)
    (save-restriction
      (widen)
      (font-lock-flush beg end)
      (condition-case nil
          (font-lock-ensure beg end)
        (treesit-parser-deleted nil)))))

(defun markdown-ts-appear--bounds-at-point (pos)
  "Return source bounds for the smallest rendered element at POS."
  (let* ((line-beg (line-beginning-position))
         (line-end (line-end-position))
         (wikilink-bounds (markdown-ts-appear--wikilink-bounds-at pos))
         (contains-p
          (lambda (node)
            (let ((beg (treesit-node-start node))
                  (end (treesit-node-end node)))
              (and (<= beg pos)
                   (or (< pos end)
                       (and (= pos end) (> pos beg)
                            (not (eq (char-before pos) ?\n))
                            (or (>= pos (point-max))
                                (memq (char-after pos)
                                      '(?\s ?\t ?\n ?\r)))))))))
         inline-node)
    (if wikilink-bounds
        wikilink-bounds
      (let ((node (treesit-node-at pos 'markdown-inline)))
        (while (and node (not inline-node))
          (let ((type (treesit-node-type node)))
            (when (and
                   (funcall contains-p node)
                   (member type
                           '("emphasis" "strong_emphasis" "strikethrough"
                             "code_span" "inline_link" "full_reference_link"
                             "collapsed_reference_link" "shortcut_link"
                             "image" "uri_autolink" "email_autolink"
                             "entity_reference" "numeric_character_reference"
                             "backslash_escape" "hard_line_break"
                             "latex_block"))
                   (not (and (equal type "shortcut_link")
                             (markdown-ts-appear--node-ancestor-of-type
                              node "image")))
                   (or (not (equal type "latex_block"))
                       (markdown-ts--latex-block-valid-p node)))
              (setq inline-node node)))
          (setq node (treesit-node-parent node))))
      ;; The inline grammar represents `~~text~~' as nested strikethroughs.
      (when (and inline-node
                 (equal (treesit-node-type inline-node) "strikethrough"))
        (let ((parent (treesit-node-parent inline-node)))
          (while (and parent
                      (equal (treesit-node-type parent) "strikethrough"))
            (setq inline-node parent
                  parent (treesit-node-parent parent)))))
      (if inline-node
          (let ((beg (treesit-node-start inline-node))
                (end (treesit-node-end inline-node)))
            (when-let* ((wikilink-bounds
                         (markdown-ts-appear--wikilink-bounds-for-node
                          inline-node)))
              (setq beg (car wikilink-bounds)
                    end (cdr wikilink-bounds)))
            (cons beg end))
        (let (structural-node)
          (let ((node (treesit-node-at pos 'markdown)))
            (while (and node (not structural-node))
              (when (and
                     (funcall contains-p node)
                     (member (treesit-node-type node)
                             '("atx_heading" "setext_heading" "list_item"
                               "task_list_marker_unchecked"
                               "task_list_marker_checked"
                               "pipe_table_header" "pipe_table_row"
                               "pipe_table_delimiter_row" "thematic_break"
                               "link_reference_definition")))
                (setq structural-node node))
              (setq node (treesit-node-parent node))))
          (pcase (and structural-node (treesit-node-type structural-node))
            ("atx_heading"
             (when-let* ((marker
                          (treesit-node-child structural-node 0 'named)))
               (cons (treesit-node-start marker)
                     (save-excursion
                       (goto-char (treesit-node-end marker))
                       (skip-chars-forward " \t" line-end)
                       (point)))))
            ("setext_heading"
             (when-let* ((underline
                          (treesit-search-subtree
                           structural-node "\\`setext_h[12]_underline\\'")))
               (cons (treesit-node-start underline)
                     (treesit-node-end underline))))
            ("list_item"
             (when-let* ((marker
                          (treesit-node-child structural-node 0 'named))
                         ((string-prefix-p
                           "list_marker_" (treesit-node-type marker)))
                         ((= line-beg
                             (save-excursion
                               (goto-char (treesit-node-start marker))
                               (line-beginning-position)))))
               (cons (treesit-node-start marker)
                     (treesit-node-end marker))))
            ((or "task_list_marker_unchecked" "task_list_marker_checked"
                 "pipe_table_header" "pipe_table_row"
                 "pipe_table_delimiter_row" "thematic_break"
                 "link_reference_definition")
             (cons (treesit-node-start structural-node)
                   (treesit-node-end structural-node)))))))))

(defun markdown-ts-appear--markdown-node-at (position)
  "Return the Markdown node at POSITION, including at node boundaries."
  (or (treesit-node-at position 'markdown)
      (and (> position (point-min))
           (treesit-node-at (1- position) 'markdown))))

(defun markdown-ts-appear--fence-bounds-at (position)
  "Return fenced code block delimiter bounds at POSITION."
  (when-let* ((block (markdown-ts-appear--node-ancestor
                      (markdown-ts-appear--markdown-node-at position)
                      "fenced_code_block"))
              (delimiters
               (markdown-ts-appear--direct-children-of-type
                block "fenced_code_block_delimiter")))
    (let* ((opening (car delimiters))
           (closing (car (last delimiters)))
           (info (markdown-ts-appear--first-direct-child-of-type
                  block "info_string"))
           (opening-end (if info (treesit-node-end info)
                          (treesit-node-end opening))))
      (cond
       ((and (<= (treesit-node-start opening) position)
             (<= position opening-end))
        (cons (treesit-node-start opening) opening-end))
       ((and closing
             (<= (treesit-node-start closing) position)
             (<= position (treesit-node-end closing)))
        (cons (treesit-node-start closing) (treesit-node-end closing)))))))

(defun markdown-ts-appear--quote-marker-bounds-at (position)
  "Return block quote marker bounds at POSITION."
  (let ((nodes (delq nil
                     (list (treesit-node-at position 'markdown)
                           (and (> position (point-min))
                                (treesit-node-at
                                 (1- position) 'markdown)))))
        marker)
    (while (and nodes (not marker))
      (let ((node (pop nodes)))
        (while (and node (not marker))
          (when (and (member (treesit-node-type node)
                             '("block_quote_marker" "block_continuation"))
                     (<= (treesit-node-start node) position)
                     (<= position (treesit-node-end node))
                     (string-search ">" (treesit-node-text node t)))
            (setq marker node))
          (setq node (treesit-node-parent node)))))
    (when marker
      (cons (treesit-node-start marker) (treesit-node-end marker)))))

(defun markdown-ts-appear--callout-data (quote)
  "Return (BEG END TYPE) for a callout marker starting block QUOTE."
  (when-let* ((opening
               (markdown-ts-appear--first-direct-child-of-type
                quote "block_quote_marker"))
              (beg (treesit-node-end opening)))
    (save-excursion
      (goto-char beg)
      (let ((case-fold-search t))
        (when (re-search-forward
               "\\=\\[!\\([[:alnum:]_-]+\\)\\]\\([+-]\\)?"
               (line-end-position) t)
          (list beg (point) (match-string-no-properties 1)))))))

(defun markdown-ts-appear--callout-bounds-at (position)
  "Return rendered callout marker bounds containing POSITION."
  (when (and markdown-ts-appear-render-callouts
             (markdown-ts-appear--markdown-node-at position))
    (when-let* ((quote
                 (markdown-ts-appear--node-ancestor
                  (markdown-ts-appear--markdown-node-at position)
                  "block_quote"))
                (data (markdown-ts-appear--callout-data quote))
                (beg (nth 0 data))
                (end (nth 1 data))
                ((<= beg position))
                ((< position end)))
      (cons beg end))))

(defun markdown-ts-appear--callout-link-p (link)
  "Return non-nil when shortcut LINK is a rendered callout marker."
  (when (and markdown-ts-appear-render-callouts
             (equal (treesit-node-type link) "shortcut_link"))
    (when-let* ((markdown-node
                 (markdown-ts-appear--markdown-node-at
                  (treesit-node-start link)))
                (quote
                 (markdown-ts-appear--node-ancestor
                  markdown-node "block_quote"))
                (data (markdown-ts-appear--callout-data quote))
                (beg (nth 0 data))
                (end (nth 1 data))
                (label-end
                 (if (memq (char-before end) '(?+ ?-)) (1- end) end)))
      (and (= (treesit-node-start link) beg)
           (= (treesit-node-end link) label-end)))))

(defun markdown-ts-appear--bounds ()
  "Return source bounds for the smallest rendered element at point."
  (let ((pos (point)))
    (or (markdown-ts-appear--fence-bounds-at pos)
        (markdown-ts-appear--callout-bounds-at pos)
        (markdown-ts-appear--quote-marker-bounds-at pos)
        (unless (markdown-ts-appear--literal-block-at pos)
          (markdown-ts-appear--bounds-at-point pos)))))

(defun markdown-ts-appear--update-inline-ranges ()
  "Update inline Tree-sitter ranges around point when needed."
  (let ((line-beg (line-beginning-position))
        (tick (buffer-chars-modified-tick)))
    (unless (and (equal line-beg markdown-ts-appear--last-range-line)
                 (equal tick markdown-ts-appear--last-range-tick))
      (treesit-update-ranges
       line-beg (min (point-max) (line-beginning-position 2)))
      (setq markdown-ts-appear--last-range-line line-beg)
      (setq markdown-ts-appear--last-range-tick tick)
      (when (and markdown-ts-appear--setup-p
                 (not markdown-ts-appear--inline-parser-notified-p))
        (markdown-ts-appear--install-parser-notifiers)))))

(defun markdown-ts-appear--update ()
  "Reveal source for the rendered Markdown element at point."
  (let ((position (point))
        (tick (buffer-chars-modified-tick)))
    (unless (and (equal position markdown-ts-appear--last-point)
                 (equal tick markdown-ts-appear--last-tick))
      (let* ((region markdown-ts-appear--region)
             (old-beg (and region (marker-position (car region))))
             (old-end (and region (marker-position (cdr region)))))
        (save-restriction
          (widen)
          (markdown-ts-appear--update-inline-ranges)
          (let* ((bounds (markdown-ts-appear--bounds))
                 (beg (car-safe bounds))
                 (end (cdr-safe bounds)))
            (unless (if bounds
                        (and old-beg old-end
                             (= beg old-beg) (= end old-end))
                      (null region))
              (markdown-ts-appear--restore)
              (when bounds
                (setq markdown-ts-appear--region
                      (cons (copy-marker beg) (copy-marker end t)))
                (font-lock-flush beg end)
                (font-lock-ensure beg end)))))
	(setq markdown-ts-appear--last-point position)
	(setq markdown-ts-appear--last-tick tick)))))

(defun markdown-ts-appear--start ()
  "Start tracking the semantic Markdown element at point."
  (setq markdown-ts-appear--last-point nil)
  (setq markdown-ts-appear--last-tick nil)
  (setq markdown-ts-appear--last-range-line nil)
  (setq markdown-ts-appear--last-range-tick nil)
  (add-hook 'post-command-hook #'markdown-ts-appear--update nil t)
  (markdown-ts-appear--update))

(defun markdown-ts-appear--stop ()
  "Stop tracking point and restore hidden Markdown markup."
  (remove-hook 'post-command-hook #'markdown-ts-appear--update t)
  (setq markdown-ts-appear--last-point nil)
  (setq markdown-ts-appear--last-tick nil)
  (setq markdown-ts-appear--last-range-line nil)
  (setq markdown-ts-appear--last-range-tick nil)
  (markdown-ts-appear--restore))

(defun markdown-ts-appear--math-node-data (node)
  "Return (BEG END MATH DISPLAY-P) for a LaTeX block NODE."
  (let (opening closing)
    (dotimes (index (treesit-node-child-count node))
      (let ((child (treesit-node-child node index)))
        (when (equal (treesit-node-type child) "latex_span_delimiter")
          (unless opening
            (setq opening child))
          (setq closing child))))
    (when (and opening closing
               (< (treesit-node-start opening) (treesit-node-start closing)))
      (let ((opener (treesit-node-text opening t)))
        (list (treesit-node-start node)
              (treesit-node-end node)
              (buffer-substring-no-properties
               (treesit-node-end opening) (treesit-node-start closing))
              (and (member opener '("$$" "\\[")) t))))))

(defun markdown-ts-appear--math-node-at (position)
  "Return the valid Markdown LaTeX block at POSITION, if any."
  (when-let* ((node (treesit-node-at position 'markdown-inline))
              (math-node (treesit-parent-until node "\\`latex_block\\'" t))
              ((<= (treesit-node-start math-node) position))
              ((< position (treesit-node-end math-node)))
              ((markdown-ts--latex-block-valid-p math-node)))
    math-node))

(defun markdown-ts-appear--view-buffers ()
  "Return live buffers that display the current buffer's shared text."
  (let ((owner (if (buffer-live-p markdown-ts-appear--base-owner)
                   markdown-ts-appear--base-owner
                 (current-buffer))))
    (with-current-buffer owner
      (cons owner (markdown-ts-appear--live-indirect-clones)))))

(defun markdown-ts-appear--math-delete-alignment (buffer &optional beg end)
  "Delete package math alignment overlays in BUFFER between BEG and END."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (dolist (overlay (overlays-in (or beg (point-min))
                                      (or end (point-max))))
          (when (overlay-get overlay 'markdown-ts-appear--math-alignment)
            (delete-overlay overlay)))))))

(defun markdown-ts-appear--math-clear-text (beg end)
  "Remove shared math preview text properties between BEG and END."
  (with-silent-modifications
    (remove-text-properties
     beg end '(display nil markdown-ts-appear--math-state nil))))

(defun markdown-ts-appear--math-clear (beg end)
  "Remove a rendered formula between BEG and END from every shared view."
  (dolist (buffer (markdown-ts-appear--view-buffers))
    (markdown-ts-appear--math-delete-alignment buffer beg end))
  (markdown-ts-appear--math-clear-text beg end))

(defun markdown-ts-appear--math-clear-buffer ()
  "Remove all rendered formulas from the current buffer."
  (save-restriction
    (widen)
    (dolist (buffer (markdown-ts-appear--view-buffers))
      (markdown-ts-appear--math-delete-alignment buffer))
    (let ((pos (point-min)))
      (while (< pos (point-max))
        (let ((next (next-single-property-change
                     pos 'markdown-ts-appear--math-state nil (point-max))))
          (when (get-text-property pos 'markdown-ts-appear--math-state)
            (with-silent-modifications
              (remove-text-properties
               pos next '(display nil markdown-ts-appear--math-state nil))))
          (setq pos next))))))

(defun markdown-ts-appear--math-state (beg end)
  "Return the uniform math preview state between BEG and END."
  (let ((state (get-text-property beg 'markdown-ts-appear--math-state)))
    (when (and state
               (= (next-single-property-change
                   beg 'markdown-ts-appear--math-state nil end)
                  end))
      state)))

(defun markdown-ts-appear--math-scale-svg (svg scale)
  "Return SVG with its intrinsic dimensions multiplied by SCALE."
  (unless (and (numberp scale) (> scale 0))
    (error "Math scale must be a positive number"))
  (dolist (attribute '("width" "height") svg)
    (let* ((prefix (concat attribute "=\""))
           (regexp (concat (regexp-quote prefix) "\\([-.0-9]+\\)")))
      (when (string-match regexp svg)
        (setq svg
              (replace-match
               (concat prefix
                       (format "%.12g"
                               (* scale
                                  (string-to-number (match-string 1 svg)))))
               t t svg))))))

(defun markdown-ts-appear--math-key (math display-p)
  "Return the cache key for MATH rendered with DISPLAY-P styling."
  (list display-p markdown-ts-appear-math-scale math))

(defun markdown-ts-appear--math-image (svg &optional scale)
  "Create an image from MathJax SVG with a suitable baseline and SCALE."
  (let* ((scale (or scale markdown-ts-appear-math-scale))
         (height
          (and (string-match "height=\"\\([-.0-9]+\\)" svg)
               (string-to-number (match-string 1 svg))))
         (vertical-align
          (and (string-match "vertical-align: \\([-.0-9]+\\)" svg)
               (string-to-number (match-string 1 svg))))
         (ascent (if (and height vertical-align (> height 0))
                     (round (* 100 (/ (+ height vertical-align) height)))
                   100)))
    (svg-image (markdown-ts-appear--math-scale-svg svg scale)
               :ascent (max 0 (min 100 ascent)))))

(defun markdown-ts-appear--math-center (beg end image)
  "Center IMAGE displayed between BEG and END with an owned overlay."
  (when (and markdown-ts-appear-center-display-math (< beg end))
    (let ((overlay (make-overlay beg (1+ beg) nil t nil)))
      (overlay-put overlay 'markdown-ts-appear--math-alignment t)
      (overlay-put
       overlay 'before-string
       (propertize
        " " 'face 'default
        'display `(space :align-to (- center (0.5 . ,image)))))
      (overlay-put overlay 'evaporate t))))

(defun markdown-ts-appear--math-display-result (request data)
  "Display MathJax DATA for a still-valid rendering REQUEST."
  (pcase-let ((`(,buffer ,beg-marker ,end-marker ,source ,key) request))
    (unwind-protect
        (when (and (buffer-live-p buffer)
                   (marker-position beg-marker)
                   (marker-position end-marker))
          (with-current-buffer buffer
            (save-restriction
              (widen)
              (let* ((beg (marker-position beg-marker))
                     (end (marker-position end-marker))
                     (eligible-p
                      (and (markdown-ts-appear--math-active-p)
                           (< beg end)
                           (equal source
                                  (buffer-substring-no-properties beg end))
                           (not (markdown-ts--outline-invisible-p beg))
                           (not (markdown-ts-appear--region-visible-p beg end))))
                     (node-data
                      (when eligible-p
                        (when-let* ((node
                                     (markdown-ts-appear--math-node-at beg)))
                          (markdown-ts-appear--math-node-data node))))
                     displayed-p)
                (when node-data
                  (pcase-let ((`(,node-beg ,node-end ,math ,display-p)
                               node-data))
                    (when (and (= beg node-beg) (= end node-end)
                               (equal key
                                      (markdown-ts-appear--math-key
                                       math display-p)))
                      (setq displayed-p t)
                      (markdown-ts-appear--math-clear-text beg end)
                      (dolist (view-buffer
                               (markdown-ts-appear--view-buffers))
                        (markdown-ts-appear--math-delete-alignment
                         view-buffer beg end))
                      (with-silent-modifications
                        (if-let* ((svg (alist-get 'svg data)))
                            (let ((image
                                   (or
                                    (alist-get
                                     'markdown-ts-appear--math-image data)
                                    (markdown-ts-appear--math-image
                                     svg (nth 1 key)))))
                              (put-text-property beg end 'display image)
                              (when display-p
                                (dolist
                                    (view-buffer
                                     (markdown-ts-appear--view-buffers))
                                  (with-current-buffer view-buffer
                                    (save-restriction
                                      (widen)
                                      (markdown-ts-appear--math-center
                                       beg end image)))))
                              (put-text-property
                               beg end 'markdown-ts-appear--math-state
                               (list 'rendered key)))
                          (unless (alist-get 'transient data)
                            (put-text-property
                             beg end 'markdown-ts-appear--math-state
                             (list 'error key))))))))
                (when (and (not displayed-p)
                           (equal (markdown-ts-appear--math-state beg end)
                                  (list 'pending key)))
                  (markdown-ts-appear--math-clear-text beg end)
                  (dolist (view-buffer
                           (markdown-ts-appear--view-buffers))
                    (markdown-ts-appear--math-delete-alignment
                     view-buffer beg end)))))))
      (set-marker beg-marker nil)
      (set-marker end-marker nil))))

(defun markdown-ts-appear--math-finish-render (key generation data)
  "Complete GENERATION of the pending MathJax render for KEY with DATA."
  (when-let* ((pending (gethash key markdown-ts-appear--math-pending))
              ((eq generation (nth 1 pending))))
    (when-let* ((svg (alist-get 'svg data)))
      (push (cons 'markdown-ts-appear--math-image
                  (markdown-ts-appear--math-image svg (nth 1 key)))
            data))
    (unless (alist-get 'transient data)
      (when (>= (hash-table-count markdown-ts-appear--math-cache) 512)
        (clrhash markdown-ts-appear--math-cache))
      (puthash key data markdown-ts-appear--math-cache))
    (remhash key markdown-ts-appear--math-pending)
    (cancel-timer (car pending))
    (maphash
     (lambda (_ requests)
       (dolist (request requests)
         (condition-case error
             (markdown-ts-appear--math-display-result request data)
           (error
            (message "Markdown math preview failed: %s"
                     (error-message-string error))))))
     (nth 2 pending))))

(defun markdown-ts-appear--math-render-timeout (key generation)
  "Time out GENERATION of requests waiting for MathJax KEY."
  (markdown-ts-appear--math-finish-render
   key generation
   '((error . "MathJax rendering timed out") (transient . t))))

(defun markdown-ts-appear--math-cancel-pending ()
  "Cancel all pending MathJax requests and release their markers."
  (maphash
   (lambda (_key pending)
     (cancel-timer (car pending))
     (maphash
      (lambda (_request-key requests)
        (dolist (request requests)
          (set-marker (cadr request) nil)
          (set-marker (caddr request) nil)))
      (nth 2 pending)))
   markdown-ts-appear--math-pending)
  (clrhash markdown-ts-appear--math-pending))

(defun markdown-ts-appear--math-cancel-buffer-requests ()
  "Remove requests belonging to the current buffer from pending renders."
  (let ((buffer (current-buffer)) empty-keys)
    (maphash
     (lambda (key pending)
       (let ((requests (nth 2 pending)) changes)
         (maphash
          (lambda (request-key bucket)
            (let (kept)
              (dolist (request bucket)
                (if (eq (car request) buffer)
                    (progn
                      (set-marker (cadr request) nil)
                      (set-marker (caddr request) nil))
                  (push request kept)))
              (push (cons request-key (nreverse kept)) changes)))
          requests)
         (dolist (change changes)
           (if (cdr change)
               (puthash (car change) (cdr change) requests)
             (remhash (car change) requests)))
         (when (zerop (hash-table-count requests))
           (cancel-timer (car pending))
           (push key empty-keys))))
     markdown-ts-appear--math-pending)
    (dolist (key empty-keys)
      (remhash key markdown-ts-appear--math-pending))))

(defun markdown-ts-appear--math-request (beg end math display-p)
  "Render MATH asynchronously for the region from BEG to END."
  (markdown-ts-appear--validate-math-options)
  (let* ((owner (if (buffer-live-p markdown-ts-appear--base-owner)
                    markdown-ts-appear--base-owner
                  (current-buffer)))
         (key (markdown-ts-appear--math-key math display-p))
         (state (markdown-ts-appear--math-state beg end))
         (cached (gethash key markdown-ts-appear--math-cache
                          markdown-ts-appear--math-cache-miss)))
    (if (and (consp state) (equal (cadr state) key))
        (when (and display-p (eq (car state) 'rendered))
          (when-let* ((image (get-text-property beg 'display)))
            (markdown-ts-appear--math-delete-alignment
             (current-buffer) beg end)
            (markdown-ts-appear--math-center beg end image)))
      (markdown-ts-appear--math-clear beg end)
      (with-silent-modifications
        (put-text-property beg end 'markdown-ts-appear--math-state
                           (list 'pending key)))
      (let* ((source (buffer-substring-no-properties beg end))
             (request-key (list owner beg end source))
             (request
              (with-current-buffer owner
                (list owner
                      (copy-marker beg t)
                      (copy-marker end)
                      source key))))
        (if (not (eq cached markdown-ts-appear--math-cache-miss))
            (markdown-ts-appear--math-display-result request cached)
          (let ((pending (gethash key markdown-ts-appear--math-pending)))
            (if pending
                (let* ((requests (nth 2 pending))
                       (bucket (gethash request-key requests)))
                  (unless (seq-some
                           (lambda (waiting)
                             (and (equal (marker-position (cadr waiting)) beg)
                                  (equal (marker-position (caddr waiting)) end)
                                  (equal (nth 3 waiting) source)))
                           bucket)
                    (puthash request-key (cons request bucket) requests)))
              (let ((requests (make-hash-table :test #'equal))
                    (generation (make-symbol "markdown-ts-appear-render")))
                (puthash request-key (list request) requests)
                (setq pending
                      (list
                       (run-at-time
                        markdown-ts-appear-math-timeout nil
                        (lambda ()
                          (when (fboundp
                                 'markdown-ts-appear--math-render-timeout)
                            (markdown-ts-appear--math-render-timeout
                             key generation))))
                       generation requests)))
              (puthash key pending markdown-ts-appear--math-pending)
              (condition-case error
                  (mathjax-render
                   (lambda (data)
                     (when (fboundp
                            'markdown-ts-appear--math-finish-render)
                       (markdown-ts-appear--math-finish-render
                        key (nth 1 pending) data)))
                   math :options (list :display display-p))
                (error
                 (markdown-ts-appear--math-finish-render
                  key (nth 1 pending)
                  `((error . ,(error-message-string error))
                    (transient . t))))))))))))

(defun markdown-ts-appear--math-preview-node (node)
  "Render the valid Markdown LaTeX block NODE when it is not being edited."
  (when (and (markdown-ts-appear--math-active-p)
             (markdown-ts--latex-block-valid-p node))
    (when-let* ((data (markdown-ts-appear--math-node-data node)))
      (pcase-let ((`(,beg ,end ,math ,display-p) data))
        (if (markdown-ts-appear--region-visible-p beg end)
            (markdown-ts-appear--math-clear beg end)
          (unless (markdown-ts--outline-invisible-p beg)
            (markdown-ts-appear--math-request beg end math display-p)))))))

(defun markdown-ts-appear--math-preview-window (window)
  "Enable math preview when WINDOW shows this buffer graphically."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (display-graphic-p (window-frame window))
             (not markdown-ts-appear--math-preview-active-p))
    (markdown-ts-appear--math-enable)))

(defun markdown-ts-appear--math-preview-owner-window (window)
  "Enable base-buffer math preview when WINDOW displays this clone."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (display-graphic-p (window-frame window))
             (buffer-live-p markdown-ts-appear--base-owner))
    (with-current-buffer markdown-ts-appear--base-owner
      (when (and markdown-ts-appear--setup-p
                 markdown-ts-appear-enable-math-preview
                 (not markdown-ts-appear--math-preview-active-p))
        (markdown-ts-appear--math-enable)))))

(defun markdown-ts-appear--math-graphic-window ()
  "Return a graphical window displaying this buffer or one of its clones."
  (seq-some
   (lambda (buffer)
     (seq-find
      (lambda (window)
        (display-graphic-p (window-frame window)))
      (get-buffer-window-list buffer nil t)))
   (markdown-ts-appear--view-buffers)))

(defun markdown-ts-appear--math-filter-copied-text (text)
  "Remove math preview properties from copied Markdown TEXT."
  (let ((pos 0)
        (end (length text)))
    (while (< pos end)
      (let ((next (next-single-property-change
                   pos 'markdown-ts-appear--math-state text end)))
        (when (get-text-property pos 'markdown-ts-appear--math-state text)
          (remove-text-properties
           pos next '(display nil markdown-ts-appear--math-state nil) text))
        (setq pos next))))
  text)

(defun markdown-ts-appear--decoration-filter-copied-text (text)
  "Remove package decoration properties from copied Markdown TEXT."
  (let ((pos 0)
        (end (length text)))
    (while (< pos end)
      (let ((next (next-single-property-change
                   pos 'markdown-ts-appear--decoration text end)))
        (when (get-text-property pos 'markdown-ts-appear--decoration text)
          (remove-text-properties
           pos next '(display nil line-prefix nil wrap-prefix nil
                      markdown-ts-appear--decoration nil)
           text))
        (setq pos next))))
  text)

(defun markdown-ts-appear--decoration-install-filter ()
  "Install the decoration copy filter in the current buffer."
  (unless markdown-ts-appear--decoration-filter-installed-p
    (add-function :filter-return (local 'filter-buffer-substring-function)
                  #'markdown-ts-appear--decoration-filter-copied-text)
    (setq markdown-ts-appear--decoration-filter-installed-p t)))

(defun markdown-ts-appear--decoration-remove-filter ()
  "Remove the decoration copy filter from the current buffer."
  (when markdown-ts-appear--decoration-filter-installed-p
    (remove-function (local 'filter-buffer-substring-function)
                     #'markdown-ts-appear--decoration-filter-copied-text)
    (setq markdown-ts-appear--decoration-filter-installed-p nil)))

(defun markdown-ts-appear--install-block-font-lock ()
  "Install block rendering rules in the current buffer."
  (unless markdown-ts-appear--block-font-lock-installed-p
    (let ((settings
           (append
             (and (or markdown-ts-appear-block-quote-marker
                      markdown-ts-appear-render-callouts)
                  markdown-ts-appear--quote-font-lock-settings)
             (and (eq markdown-ts-appear-code-fence-style 'connected)
                  markdown-ts-appear--code-font-lock-settings)
             (and (eq markdown-ts-appear-table-style 'unicode)
                  markdown-ts-appear--table-font-lock-settings))))
      (when settings
        (setq treesit-font-lock-settings
              (append treesit-font-lock-settings settings))
        (setq markdown-ts-appear--block-font-lock-installed-p t))))
  (add-to-list 'font-lock-extra-managed-props
               'markdown-ts-appear--decoration)
  (when (eq markdown-ts-appear-code-fence-style 'connected)
    (dolist (property '(line-prefix wrap-prefix))
      (unless (memq property font-lock-extra-managed-props)
        (push property markdown-ts-appear--managed-code-prefix-properties)
        (add-to-list 'font-lock-extra-managed-props property))))
  (markdown-ts-appear--decoration-install-filter))

(defun markdown-ts-appear--release-code-prefix-properties ()
  "Stop managing code prefix properties added by appear mode."
  (dolist (property markdown-ts-appear--managed-code-prefix-properties)
    (setq font-lock-extra-managed-props
          (remove property font-lock-extra-managed-props)))
  (setq markdown-ts-appear--managed-code-prefix-properties nil))

(defun markdown-ts-appear--remove-block-font-lock ()
  "Remove block rendering rules from the current buffer."
  (when markdown-ts-appear--block-font-lock-installed-p
    (setq treesit-font-lock-settings
          (seq-remove
           (lambda (setting)
              (or (member setting markdown-ts-appear--quote-font-lock-settings)
                  (member setting markdown-ts-appear--code-font-lock-settings)
                  (member setting markdown-ts-appear--table-font-lock-settings)))
           treesit-font-lock-settings))
    (setq markdown-ts-appear--block-font-lock-installed-p nil))
  (markdown-ts-appear--decoration-remove-filter))

(defun markdown-ts-appear--install-view-rendering ()
  "Install view-local rendering integration in the current buffer."
  (if (buffer-live-p markdown-ts-appear--base-owner)
      (markdown-ts-appear--decoration-install-filter)
    (markdown-ts-appear--install-block-font-lock)))

(defun markdown-ts-appear--remove-view-rendering ()
  "Remove view-local rendering integration from the current buffer."
  (markdown-ts-appear--remove-block-font-lock)
  (markdown-ts-appear--delete-rendering-overlays))

(defun markdown-ts-appear--install-all-view-rendering ()
  "Install rendering integration in the base and indirect views."
  (dolist (buffer (markdown-ts-appear--view-buffers))
    (with-current-buffer buffer
      (markdown-ts-appear--install-view-rendering))))

(defun markdown-ts-appear--remove-all-view-rendering ()
  "Remove rendering integration from the base and indirect views."
  (dolist (buffer (markdown-ts-appear--view-buffers))
    (with-current-buffer buffer
      (markdown-ts-appear--remove-view-rendering))))

(defun markdown-ts-appear--math-outline-view-change ()
  "Refresh math previews after the outline visibility changes."
  (when markdown-ts-appear--math-preview-active-p
    (markdown-ts-appear--math-clear-buffer)
    (font-lock-flush)))

(defun markdown-ts-appear--math-install-filter ()
  "Install the math preview copy filter in the current buffer."
  (unless markdown-ts-appear--math-filter-installed-p
    (add-function :filter-return (local 'filter-buffer-substring-function)
                  #'markdown-ts-appear--math-filter-copied-text)
    (setq markdown-ts-appear--math-filter-installed-p t)))

(defun markdown-ts-appear--math-remove-filter ()
  "Remove the math preview copy filter from the current buffer."
  (when markdown-ts-appear--math-filter-installed-p
    (remove-function (local 'filter-buffer-substring-function)
                     #'markdown-ts-appear--math-filter-copied-text)
    (setq markdown-ts-appear--math-filter-installed-p nil)))

(defun markdown-ts-appear--math-install-clone-filters ()
  "Install math integration in indirect clones of the current buffer."
  (dolist (clone (markdown-ts-appear--live-indirect-clones))
    (with-current-buffer clone
      (markdown-ts-appear--math-install-filter)
      (add-to-list 'font-lock-extra-managed-props
                   'markdown-ts-appear--math-state))))

(defun markdown-ts-appear--math-remove-clone-filters ()
  "Remove math integration from indirect clones of the current buffer."
  (dolist (clone (markdown-ts-appear--live-indirect-clones))
    (with-current-buffer clone
      (markdown-ts-appear--math-remove-filter)
      (setq font-lock-extra-managed-props
            (delq 'markdown-ts-appear--math-state
                  font-lock-extra-managed-props)))))

(defun markdown-ts-appear--math-setup ()
  "Enable math preview now or when this buffer reaches a graphical frame."
  (add-hook 'window-buffer-change-functions
            #'markdown-ts-appear--math-preview-window nil t)
  (when (markdown-ts-appear--math-graphic-window)
    (markdown-ts-appear--math-enable)))

(defun markdown-ts-appear--math-enable ()
  "Enable MathJax rendering when its runtime dependencies are available."
  (markdown-ts-appear--validate-math-options)
  (condition-case error-data
      (when (and (or (display-graphic-p)
                     (markdown-ts-appear--math-graphic-window))
                 (image-type-available-p 'svg)
                 (require 'mathjax nil t)
                 (mathjax-available-p))
        (markdown-ts-appear--install-advice)
        (setq markdown-ts-appear--math-preview-active-p t)
        (remove-hook 'window-buffer-change-functions
                     #'markdown-ts-appear--math-preview-window t)
        (markdown-ts-appear--math-install-filter)
        (markdown-ts-appear--math-install-clone-filters)
        (with-suppressed-warnings ((obsolete outline-view-change-hook))
          (add-hook 'outline-view-change-hook
                    #'markdown-ts-appear--math-outline-view-change nil t))
        (add-to-list 'font-lock-extra-managed-props
                     'markdown-ts-appear--math-state)
        (font-lock-flush)
        t)
    (error
     (markdown-ts-appear--math-teardown)
     (display-warning
      'markdown-ts-appear
      (format "Math preview unavailable: %s"
              (error-message-string error-data))
      :warning)
     nil)))

(defun markdown-ts-appear--math-teardown ()
  "Disable MathJax rendering and remove its buffer-local integration."
  (remove-hook 'window-buffer-change-functions
               #'markdown-ts-appear--math-preview-window t)
  (with-suppressed-warnings ((obsolete outline-view-change-hook))
    (remove-hook 'outline-view-change-hook
                 #'markdown-ts-appear--math-outline-view-change t))
  (setq markdown-ts-appear--math-preview-active-p nil)
  (markdown-ts-appear--math-cancel-buffer-requests)
  (markdown-ts-appear--math-remove-filter)
  (markdown-ts-appear--math-remove-clone-filters)
  (unless markdown-ts-appear--tearing-down-buffer-p
    (markdown-ts-appear--math-clear-buffer))
  (setq font-lock-extra-managed-props
        (delq 'markdown-ts-appear--math-state
              font-lock-extra-managed-props))
  (unless markdown-ts-appear--tearing-down-buffer-p
    (font-lock-flush)))

(defun markdown-ts-appear--fontify-node (function node &rest arguments)
  "Call FUNCTION with NODE and ARGUMENTS without covering visible source."
  (if (not (markdown-ts-appear--active-p))
      (apply function node arguments)
    (let ((markdown-ts-hide-markup
           (and markdown-ts-hide-markup
                (not (markdown-ts-appear--node-visible-p node)))))
      (apply function node arguments))))

(defun markdown-ts-appear--fence-opening-p (node)
  "Return non-nil when fenced delimiter NODE opens its code block."
  (when-let* ((block (markdown-ts-appear--node-ancestor
                      (treesit-node-parent node) "fenced_code_block"))
              (opening
               (markdown-ts-appear--first-direct-child-of-type
                block "fenced_code_block_delimiter")))
    (= (treesit-node-start node) (treesit-node-start opening))))

(defun markdown-ts-appear--fontify-fence (node visible-p start limit)
  "Render fence NODE between START and LIMIT unless VISIBLE-P."
  (when (and (not visible-p)
             (eq markdown-ts-appear-code-fence-style 'connected)
             (<= start (treesit-node-start node))
             (<= (treesit-node-end node) limit))
    (if (markdown-ts-appear--fence-opening-p node)
        (let* ((block (treesit-node-parent node))
               (info
                (markdown-ts-appear--first-direct-child-of-type
                 block "info_string"))
               (header-end (if info (treesit-node-end info)
                             (treesit-node-end node))))
          (when (<= header-end limit)
            (let ((display
                   (if info
                       (let ((language (treesit-node-text info t)))
                          (concat
                           (propertize
                            "╭─" 'face 'markdown-ts-appear-code-fence-marker)
                          (markdown-ts-appear--label
                           language 'markdown-ts-appear-code-fence-marker)))
                     "╭─")))
              (markdown-ts-appear--decorate
               (treesit-node-start node) (treesit-node-end node)
               display (unless info 'markdown-ts-appear-code-fence-marker)))
            (when (< (treesit-node-end node) header-end)
              (markdown-ts-appear--decorate
               (treesit-node-end node) header-end "" nil))))
      (markdown-ts-appear--decorate
       (treesit-node-start node) (treesit-node-end node)
       "╰─"
       'markdown-ts-appear-code-fence-marker))))

(defun markdown-ts-appear--fontify-code-block
    (node _override start limit &rest _)
  "Render the content prefix of fenced code block NODE in START through LIMIT."
  (when (and (markdown-ts-appear--active-p)
             (eq markdown-ts-appear-code-fence-style 'connected))
    (when-let* ((content
                 (markdown-ts-appear--first-direct-child-of-type
                  node "code_fence_content")))
      (let* ((delimiters
              (markdown-ts-appear--direct-children-of-type
               node "fenced_code_block_delimiter"))
             (closing (and (cdr delimiters) (car (last delimiters))))
             (body-beg
              (save-excursion
                (goto-char (treesit-node-start content))
                (line-beginning-position)))
             (body-end
              (if closing
                  (save-excursion
                    (goto-char (treesit-node-start closing))
                    (line-beginning-position))
                (treesit-node-end content)))
             (quote
              (markdown-ts-appear--node-ancestor
               (treesit-node-parent node) "block_quote")))
        (if (not quote)
            (let ((beg (max start body-beg))
                  (end (min limit body-end)))
              (when (< beg end)
                (markdown-ts-appear--decorate-line-prefix
                 beg end "│ " 'markdown-ts-appear-code-fence-marker)))
          (let* ((content-beg (treesit-node-start content))
                 (source-prefix
                  (buffer-substring-no-properties body-beg content-beg))
                 (wrap-prefix
                  (markdown-ts-appear--code-quote-prefix source-prefix))
                 (beg (max start content-beg))
                 (end (min limit body-end)))
            (when (< beg end)
              (with-silent-modifications
                (add-text-properties
                 beg end `(wrap-prefix ,wrap-prefix
                           markdown-ts-appear--decoration t))))
          (save-excursion
            (goto-char (max start body-beg))
            (beginning-of-line)
            (when (< (point) body-beg)
              (goto-char body-beg))
            (while (< (point) (min limit body-end))
              (let ((line-end (min limit body-end (line-end-position))))
                (back-to-indentation)
                (when (looking-at "\\(?:>[ \\t]?\\)+")
                  (let ((prefix-end (min line-end (match-end 0)))
                        marker-beg)
                    (while (search-forward ">" prefix-end t)
                      (setq marker-beg (1- (point))))
                    (when (and marker-beg
                               (<= start marker-beg)
                               (not (markdown-ts-appear--region-visible-p
                                     marker-beg (1+ marker-beg))))
                      (let* ((marker
                              (or (markdown-ts-appear--display-string
                                   markdown-ts-appear-block-quote-marker)
                                  ">"))
                             (marker-end
                              (if (memq (char-after (1+ marker-beg))
                                        '(?\s ?\t))
                                  (+ marker-beg 2)
                                (1+ marker-beg)))
                             (display
                              (concat
                               (propertize
                                marker
                                'face
                                '(markdown-ts-appear-block-quote-marker
                                  markdown-ts-appear-code-fence-marker))
                               (propertize
                                " │ "
                                'face
                                'markdown-ts-appear-code-fence-marker))))
                        (markdown-ts-appear--decorate
                         marker-beg marker-end display nil)))))
                (forward-line 1))))))))))

(defun markdown-ts-appear--fontify-callout (node start limit)
  "Render a callout label at the start of block quote NODE in START to LIMIT."
  (when markdown-ts-appear-render-callouts
    (when-let* ((data (markdown-ts-appear--callout-data node))
                (beg (nth 0 data))
                (end (nth 1 data))
                (label-end
                 (if (memq (char-before end) '(?+ ?-)) (1- end) end))
                ((<= start beg))
                ((<= label-end limit))
                ((not (markdown-ts-appear--region-visible-p beg end))))
      (markdown-ts-appear--decorate
       beg label-end
       (markdown-ts-appear--label
        (nth 2 data) 'markdown-ts-appear-block-quote-marker)
       nil))))

(defun markdown-ts-appear--fontify-quote-marker (node visible-p start limit)
  "Render quote marker NODE between START and LIMIT unless VISIBLE-P."
  (when-let* (((not visible-p))
              ((< (treesit-node-start node) limit))
              ((< start (treesit-node-end node)))
              (marker
               (markdown-ts-appear--display-string
                markdown-ts-appear-block-quote-marker)))
    (save-excursion
      (goto-char (max start (treesit-node-start node)))
      (while (search-forward ">" (min limit (treesit-node-end node)) t)
        (markdown-ts-appear--decorate
         (1- (point)) (point) marker
         'markdown-ts-appear-block-quote-marker)))))

(defun markdown-ts-appear--fontify-delimiter
    (function node &rest arguments)
  "Call FUNCTION with NODE and ARGUMENTS, rendering structural delimiters."
  (if (not (markdown-ts-appear--active-p))
      (apply function node arguments)
    (let* ((type (treesit-node-type node))
           (hide-markup-p markdown-ts-hide-markup)
           (visible-p (markdown-ts-appear--node-visible-p node))
           (start (nth 1 arguments))
           (limit (nth 2 arguments))
           (quote-marker-p
            (and (member type '("block_quote_marker" "block_continuation"))
                 (save-excursion
                   (goto-char (treesit-node-start node))
                   (skip-chars-forward " \t" (treesit-node-end node))
                   (eq (char-after) ?>))))
           (fence-p (member type '("fenced_code_block_delimiter"
                                   "info_string")))
           (markdown-ts-hide-markup
            (and markdown-ts-hide-markup
                 (not (or quote-marker-p fence-p
                          visible-p)))))
      (apply function node arguments)
      (when (equal type "fenced_code_block_delimiter")
        (let ((face (if hide-markup-p
                        'markdown-ts-code-block-markup-hidden
                      'markdown-ts-code-block)))
          (save-excursion
            (goto-char (treesit-node-start node))
            (let ((beg (max start (line-beginning-position)))
                  (end (min limit (point-max) (1+ (line-end-position)))))
              (when (< beg end)
                (add-face-text-property beg end face t)))))
        (markdown-ts-appear--fontify-fence node visible-p start limit))
      (when quote-marker-p
        (markdown-ts-appear--fontify-quote-marker
         node visible-p start limit)))))

(defun markdown-ts-appear--quote-prefix-end ()
  "Return the end of the block quote prefix on the current line."
  (save-excursion
    (back-to-indentation)
    (when (looking-at "\\(?:>[ \\t]?\\)+")
      (match-end 0))))

(defun markdown-ts-appear--fontify-block-quote
    (node _override start limit &rest _)
  "Render block quote NODE between START and LIMIT."
  (when (markdown-ts-appear--active-p)
    (let ((beg (treesit-node-start node))
          (end (treesit-node-end node))
          (marker
           (markdown-ts-appear--display-string
            markdown-ts-appear-block-quote-marker)))
      (when (< (max beg start) (min end limit))
        (add-face-text-property
         (max beg start) (min end limit)
         'markdown-ts-appear-block-quote t))
      (markdown-ts-appear--fontify-callout node start limit)
      (when marker
        (save-excursion
          (goto-char (max beg start))
          (beginning-of-line)
          (when (< (point) beg)
            (goto-char beg))
          (while (< (point) (min end limit))
            (let ((line-end (min end limit (line-end-position)))
                  (prefix-end (markdown-ts-appear--quote-prefix-end)))
              (when prefix-end
                (while (search-forward ">" (min line-end prefix-end) t)
                  (let ((marker-beg (1- (point))))
                    (when (and (<= start marker-beg)
                               (not (markdown-ts-appear--region-visible-p
                                     marker-beg (point))))
                      (markdown-ts-appear--decorate
                       marker-beg (point) marker
                       'markdown-ts-appear-block-quote-marker)))))
              (forward-line 1))))))))

(defun markdown-ts-appear--table-rows-in-range (table start limit)
  "Return row children of TABLE intersecting START through LIMIT."
  (let ((child
         (treesit-node-first-child-for-pos
          table (max start (treesit-node-start table))))
        rows)
    (while (and child (< (treesit-node-start child) limit))
      (when (member (treesit-node-type child)
                    '("pipe_table_header" "pipe_table_delimiter_row"
                      "pipe_table_row"))
        (push child rows))
      (setq child (treesit-node-next-sibling child)))
    (nreverse rows)))

(defun markdown-ts-appear--table-pipes (row)
  "Return direct pipe children of ROW."
  (markdown-ts-appear--direct-children-of-type row "|"))

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
               (pos (max start content-start))
               (end (min limit row-end)))
          (while (< pos end)
            (markdown-ts-appear--decorate
             pos (1+ pos) (if (eq (char-after pos) ?|) "┼" "─")
             'markdown-ts-appear-table-border)
            (setq pos (1+ pos))))
      (dolist (pipe (markdown-ts-appear--table-pipes row))
        (when (and (<= start (treesit-node-start pipe))
                   (< (treesit-node-start pipe) limit))
          (markdown-ts-appear--decorate
           (treesit-node-start pipe) (treesit-node-end pipe) "│"
           'markdown-ts-appear-table-border))))))

(defun markdown-ts-appear--fontify-table
    (node _override start limit &rest _)
  "Render Markdown pipe table NODE between START and LIMIT."
  (when (and (markdown-ts-appear--active-p)
             (eq markdown-ts-appear-table-style 'unicode))
    (dolist (row (markdown-ts-appear--table-rows-in-range node start limit))
      (markdown-ts-appear--fontify-table-row row start limit))))

(defconst markdown-ts-appear--quote-font-lock-settings
  (treesit-font-lock-rules
   :language 'markdown
   :feature 'paragraph
   :override 'append
   '(((block_quote) @markdown-ts-appear--fontify-block-quote)))
  "Additional Tree-sitter font-lock settings for rendered block quotes.")

(defconst markdown-ts-appear--code-font-lock-settings
  (treesit-font-lock-rules
   :language 'markdown
   :feature 'paragraph
   :override 'append
   '(((fenced_code_block) @markdown-ts-appear--fontify-code-block)))
  "Additional Tree-sitter font-lock settings for rendered code blocks.")

(defconst markdown-ts-appear--table-font-lock-settings
  (treesit-font-lock-rules
   :language 'markdown
   :feature 'paragraph
   :override 'append
   '(((pipe_table) @markdown-ts-appear--fontify-table)))
  "Additional Tree-sitter font-lock settings for rendered tables.")

(defun markdown-ts-appear--fontify-visible-markup
    (function node &rest arguments)
  "Call FUNCTION with NODE and ARGUMENTS without covering visible markup."
  (if (not (markdown-ts-appear--active-p))
      (apply function node arguments)
    (let* ((type (treesit-node-type node))
           (markup-node
            (pcase type
              ("atx_heading" (treesit-node-child node 0 'named))
              ("setext_heading"
               (treesit-search-subtree node "\\`setext_h[12]_underline\\'"))
              (_ node)))
           (markdown-ts-hide-markup
            (and markdown-ts-hide-markup
                 (not (and markup-node
                           (markdown-ts-appear--node-visible-p markup-node))))))
      (apply function node arguments))))

(defun markdown-ts-appear--icon (type)
  "Return the configured Markdown icon for TYPE."
  (when-let* ((icon
               (markdown-ts-appear--display-string
                (pcase type
                  ('image markdown-ts-appear-image-icon)
                  ('wikilink markdown-ts-appear-wikilink-icon)
                  (_ markdown-ts-appear-link-icon)))))
    (propertize icon 'face 'markdown-ts-link)))

(defun markdown-ts-appear--fontify-link-destination
    (function node &rest arguments)
  "Call FUNCTION with NODE and ARGUMENTS, preserving useful image labels."
  (if (not (markdown-ts-appear--active-p))
      (apply function node arguments)
    (let* ((parent (treesit-node-parent node))
           (image-p (equal (treesit-node-type parent) "image"))
           (beg (and image-p (treesit-node-start parent)))
           (end (and image-p (treesit-node-end parent)))
           (visible-p (markdown-ts-appear--node-visible-p node))
           (markdown-ts-hide-markup
            (and markdown-ts-hide-markup (not visible-p))))
      (apply function node arguments)
      (when image-p
        (dolist (overlay (overlays-in beg end))
          (when (overlay-get overlay 'markdown-ts-appear--image-label)
            (delete-overlay overlay)))
        (when (and markdown-ts-hide-markup (not visible-p)
                   (not (markdown-ts--outline-invisible-p beg)))
          (let* ((description
                  (treesit-search-subtree parent "\\`image_description\\'"))
                 (url (treesit-node-text node t))
                 (label (file-name-nondirectory url))
                 (icon (markdown-ts-appear--icon 'image)))
            (with-silent-modifications
              (when (not description)
                (remove-text-properties
                 (treesit-node-start node) (treesit-node-end node)
                 '(invisible nil))
                (markdown-ts-appear--decorate
                 (treesit-node-start node) (treesit-node-end node)
                 (if (equal label "") url label) nil))
              (markdown-ts--make-link-button beg end url))
            (when icon
              (let ((overlay
                     (make-overlay beg (min (1+ beg) end) nil t nil)))
                (overlay-put overlay 'markdown-ts-appear--image-label t)
                (overlay-put overlay 'before-string (concat icon " "))
                (overlay-put overlay 'help-echo url)
                (overlay-put overlay 'mouse-face 'highlight)
                (overlay-put overlay 'evaporate t)))))))))

(defun markdown-ts-appear--fontify-image (function node &rest arguments)
  "Call FUNCTION with NODE and ARGUMENTS without covering image source."
  (if (not (markdown-ts-appear--active-p))
      (apply function node arguments)
    (let ((markdown-ts-inline-images
           (and markdown-ts-inline-images
                (not (markdown-ts-appear--node-visible-p node)))))
      (apply function node arguments))))

(defun markdown-ts-appear--fontify-link (function node &rest arguments)
  "Call FUNCTION with NODE and ARGUMENTS, prefixing links with an icon."
  (apply function node arguments)
  (when (markdown-ts-appear--active-p)
    (let* ((parent (treesit-node-parent node))
           (reference-label-p
            (and (equal (treesit-node-type node) "link_label")
                 (treesit-search-subtree parent "\\`link_text\\'")))
           (image-p
            (and (markdown-ts-appear--node-ancestor-of-type node "image") t))
           (parent-beg (treesit-node-start parent))
           (parent-end (treesit-node-end parent))
           (callout-p (markdown-ts-appear--callout-link-p parent))
           (wikilink-p
            (and (markdown-ts-appear--wikilink-bounds-for-node parent) t))
           (beg (treesit-node-start node))
           (end (treesit-node-end node))
           (alias-delimiter
            (and wikilink-p
                 (markdown-ts-appear--node-child-of-type node "|")))
           (alias-beg
            (and alias-delimiter (treesit-node-end alias-delimiter)))
           (icon-beg (or alias-beg beg))
           (visible-region (markdown-ts-appear--visible-region))
           (visible-beg
            (and visible-region (marker-position (car visible-region))))
           (visible-end
            (and visible-region (marker-position (cdr visible-region))))
           (visible-p
            (or (markdown-ts-appear--region-visible-p parent-beg beg)
                (markdown-ts-appear--region-visible-p end parent-end))))
      (if callout-p
          (progn
            (with-silent-modifications
              (remove-list-of-text-properties
               parent-beg parent-end
               '(action button category follow-link help-echo keymap
                        mouse-face)))
            (dolist (overlay (overlays-in parent-beg parent-end))
              (when (overlay-get overlay 'markdown-ts-appear--link-icon)
                (delete-overlay overlay))))
        (when reference-label-p
          (with-silent-modifications
            (if (and markdown-ts-hide-markup
                     (not (markdown-ts-appear--node-visible-p node)))
                (put-text-property beg end 'invisible 'markdown-ts--markup)
              (remove-text-properties beg end '(invisible nil)))))
        (unless (or reference-label-p image-p)
          (when alias-beg
            (with-silent-modifications
              (markdown-ts--make-link-button
               beg end
               (buffer-substring-no-properties
                beg (treesit-node-start alias-delimiter)))))
          (dolist (overlay (overlays-in beg end))
            (when (overlay-get overlay 'markdown-ts-appear--link-icon)
              (delete-overlay overlay)))
          (when (and markdown-ts-hide-markup
                     (not (equal (treesit-node-type parent) "image"))
                     (not visible-p))
            (when wikilink-p
              (with-silent-modifications
                (put-text-property (1- parent-beg) parent-beg
                                   'invisible 'markdown-ts--markup)
                (put-text-property parent-end (1+ parent-end)
                                   'invisible 'markdown-ts--markup)
                (when alias-beg
                  (if (and visible-beg visible-end
                           (< visible-beg alias-beg) (> visible-end beg))
                      (let ((reveal-beg (max beg visible-beg))
                            (reveal-end (min alias-beg visible-end)))
                        (when (< beg reveal-beg)
                          (put-text-property beg reveal-beg
                                             'invisible 'markdown-ts--markup))
                        (when (< reveal-end alias-beg)
                          (put-text-property reveal-end alias-beg
                                             'invisible 'markdown-ts--markup)))
                    (put-text-property beg alias-beg
                                       'invisible 'markdown-ts--markup)))))
            (when-let* ((icon (markdown-ts-appear--icon
                               (if wikilink-p 'wikilink 'link))))
              (let ((overlay (make-overlay icon-beg (min (1+ icon-beg) end)
                                           nil t nil)))
                (overlay-put overlay 'markdown-ts-appear--link-icon t)
                (overlay-put overlay 'before-string (concat icon " "))
                (overlay-put overlay 'evaporate t)))))))))

(defun markdown-ts-appear--delete-rendering-overlays (&optional beg end)
  "Delete package rendering overlays between BEG and END."
  (save-restriction
    (widen)
    (dolist (overlay (overlays-in (or beg (point-min)) (or end (point-max))))
      (when (or (overlay-get overlay 'markdown-ts-appear--image-label)
                (overlay-get overlay 'markdown-ts-appear--link-icon)
                (overlay-get overlay 'markdown-ts-appear--math-alignment))
        (delete-overlay overlay)))))

(defun markdown-ts-appear--delete-indirect-rendering-overlays ()
  "Delete package overlays in indirect clones of the current buffer."
  (dolist (clone (markdown-ts-appear--live-indirect-clones))
    (with-current-buffer clone
      (markdown-ts-appear--delete-rendering-overlays))))

(defun markdown-ts-appear--delete-shared-rendering-overlays (&optional beg end)
  "Delete package overlays between BEG and END in every shared view."
  (dolist (buffer (markdown-ts-appear--view-buffers))
    (with-current-buffer buffer
      (markdown-ts-appear--delete-rendering-overlays beg end))))

(defun markdown-ts-appear--sync-indirect-clone-markup (enabled-p)
  "Synchronize registered clones for markup being ENABLED-P."
  (let ((manage-line-height
         (and enabled-p markdown-ts-appear--managed-line-height-p)))
    (dolist (clone (markdown-ts-appear--live-indirect-clones))
      (with-current-buffer clone
        (if enabled-p
            (unless markdown-ts-hide-markup
              (setq markdown-ts-hide-markup t)
              (markdown-ts--set-hide-markup t))
          (markdown-ts-appear--restore-hide-markup))
        (cond
         (manage-line-height
          (unless (memq 'line-height font-lock-extra-managed-props)
            (add-to-list 'font-lock-extra-managed-props 'line-height)
            (setq markdown-ts-appear--managed-line-height-p t)))
         (markdown-ts-appear--managed-line-height-p
           (setq font-lock-extra-managed-props
                 (remove 'line-height font-lock-extra-managed-props))
           (setq markdown-ts-appear--managed-line-height-p nil)))))))

(defun markdown-ts-appear--adopt-existing-indirect-clones ()
  "Register existing Markdown indirect clones of the current buffer."
  (let ((base (current-buffer)))
    (dolist (buffer (buffer-list))
      (when (and (not (eq buffer base))
                 (eq (buffer-base-buffer buffer) base))
        (with-current-buffer buffer
          (when (derived-mode-p 'markdown-ts-mode)
            (markdown-ts-appear--detach-indirect-clone t)))))))

(defun markdown-ts-appear--after-change (beg end _old-length)
  "Refresh rendering on lines changed between BEG and END."
  (let ((owner (if (buffer-live-p markdown-ts-appear--base-owner)
                   markdown-ts-appear--base-owner
                 (current-buffer))))
    (unless (buffer-local-value
             'markdown-ts-appear--inline-parser-notified-p owner)
      (with-current-buffer owner
        (save-excursion
          (save-restriction
            (widen)
            (goto-char beg)
            (markdown-ts-appear--update-inline-ranges)))))
    (save-excursion
      (let ((line-beg (progn (goto-char beg) (line-beginning-position)))
            (line-end
             (progn
               (goto-char end)
               (min (point-max) (1+ (line-end-position))))))
        (markdown-ts-appear--delete-shared-rendering-overlays
         line-beg line-end)
        (unless (eq owner (current-buffer))
          (with-current-buffer owner
            (save-restriction
              (widen)
              (let ((markdown-ts-appear--refreshing-clone-edit-p t))
                (font-lock-flush line-beg line-end)
                (font-lock-ensure line-beg line-end)))))))))

(defun markdown-ts-appear--parser-changed (ranges _parser)
  "Invalidate rendering in Tree-sitter changed RANGES."
  (let ((owner (if (buffer-live-p markdown-ts-appear--base-owner)
                   markdown-ts-appear--base-owner
                 (current-buffer))))
    (with-current-buffer owner
      (save-restriction
        (widen)
        (dolist (range ranges)
          (let ((beg (max (point-min) (1- (car range))))
                (end (min (point-max) (1+ (cdr range)))))
            (markdown-ts-appear--delete-shared-rendering-overlays beg end)
            (when markdown-ts-appear--refreshing-clone-edit-p
              (font-lock-unfontify-region beg end)
              (font-lock-flush beg end))))))))

(defun markdown-ts-appear--install-parser-notifiers ()
  "Install package change notifiers on the Markdown parsers."
  (let ((inline-parsers (treesit-parser-list nil 'markdown-inline t)))
    (dolist (parser (append (treesit-parser-list nil 'markdown t)
                            inline-parsers))
      (unless (memq parser markdown-ts-appear--notified-parsers)
        (treesit-parser-add-notifier parser #'markdown-ts-appear--parser-changed)
        (push parser markdown-ts-appear--notified-parsers)))
    (setq markdown-ts-appear--inline-parser-notified-p
          (and inline-parsers t))))

(defun markdown-ts-appear--remove-parser-notifiers ()
  "Remove package change notifiers from the Markdown parsers."
  (dolist (parser markdown-ts-appear--notified-parsers)
    (condition-case nil
        (treesit-parser-remove-notifier
         parser #'markdown-ts-appear--parser-changed)
      (treesit-parser-deleted nil)))
  (setq markdown-ts-appear--notified-parsers nil)
  (setq markdown-ts-appear--inline-parser-notified-p nil))

(defun markdown-ts-appear--forget-indirect-clone ()
  "Remove the current indirect buffer from its base buffer's registry."
  (let ((clone (current-buffer)))
    (when (buffer-live-p markdown-ts-appear--base-owner)
      (with-current-buffer markdown-ts-appear--base-owner
        (setq markdown-ts-appear--indirect-clones
              (delq clone markdown-ts-appear--indirect-clones))))))

(defun markdown-ts-appear--indirect-clone-teardown ()
  "Remove package integration before discarding an indirect clone."
  (remove-hook 'window-buffer-change-functions
               #'markdown-ts-appear--math-preview-owner-window t)
  (remove-hook 'after-change-functions #'markdown-ts-appear--after-change t)
  (markdown-ts-appear--remove-view-rendering)
  (markdown-ts-appear--math-remove-filter)
  (markdown-ts-appear--forget-indirect-clone)
  (setq markdown-ts-appear--base-owner nil))

(defun markdown-ts-appear--indirect-clone-major-mode-teardown ()
  "Disable shared rendering before this clone changes major mode."
  (if (and (buffer-live-p markdown-ts-appear--base-owner)
           (buffer-local-value
            'markdown-ts-appear--setup-p markdown-ts-appear--base-owner))
      (with-current-buffer markdown-ts-appear--base-owner
        (markdown-ts-appear-mode -1))
    (markdown-ts-appear--indirect-clone-teardown)))

(defun markdown-ts-appear--release-indirect-clones ()
  "Remove all package-local integration from registered indirect clones."
  (markdown-ts-appear--sync-indirect-clone-markup nil)
  (dolist (clone (markdown-ts-appear--live-indirect-clones))
    (with-current-buffer clone
      (remove-hook 'kill-buffer-hook
                   #'markdown-ts-appear--indirect-clone-teardown t)
      (remove-hook 'change-major-mode-hook
                   #'markdown-ts-appear--indirect-clone-major-mode-teardown t)
      (remove-hook 'window-buffer-change-functions
                   #'markdown-ts-appear--math-preview-owner-window t)
      (remove-hook 'after-change-functions
                   #'markdown-ts-appear--after-change t)
      (markdown-ts-appear--remove-view-rendering)
      (markdown-ts-appear--math-remove-filter)
      (setq font-lock-extra-managed-props
            (remove 'markdown-ts-appear--decoration
                    (remove 'markdown-ts-appear--math-state
                            font-lock-extra-managed-props)))
      (markdown-ts-appear--release-code-prefix-properties)
      (add-to-list 'font-lock-extra-managed-props 'display)
      (when markdown-ts-appear--managed-line-height-p
        (setq font-lock-extra-managed-props
              (remove 'line-height font-lock-extra-managed-props))
        (setq markdown-ts-appear--managed-line-height-p nil))
      (setq markdown-ts-appear--base-owner nil)))
  (setq markdown-ts-appear--indirect-clones nil))

(defun markdown-ts-appear--detach-indirect-clone (&optional existing-p)
  "Detach inherited integration from an indirect clone.
When EXISTING-P is non-nil, preserve its prior Markdown display settings."
  (when-let* ((base (buffer-base-buffer)))
    (when existing-p
      (markdown-ts-appear--save-hide-markup)
      (setq markdown-ts-appear--managed-line-height-p nil))
    (let ((clone (current-buffer)))
      (with-current-buffer base
        (setq markdown-ts-appear--last-point nil)
        (setq markdown-ts-appear--last-tick nil)
        (unless (memq clone markdown-ts-appear--indirect-clones)
          (push clone markdown-ts-appear--indirect-clones))))
    ;; Text properties are shared with the base, so normal teardown here would
    ;; unfontify the base buffer and invalidate its reveal markers.
    (setq markdown-ts-appear-mode nil)
    (setq markdown-ts-appear--math-preview-active-p nil)
    (setq markdown-ts-appear--setup-p nil)
    (setq markdown-ts-appear--region nil)
    (setq markdown-ts-appear--notified-parsers nil)
    (setq markdown-ts-appear--base-owner base)
    (setq markdown-ts-appear--indirect-clones nil)
    (setq post-command-hook
          (remove #'markdown-ts-appear--update post-command-hook))
    (add-hook 'after-change-functions #'markdown-ts-appear--after-change t t)
    (setq change-major-mode-hook
          (remove #'markdown-ts-appear--buffer-teardown
                  change-major-mode-hook))
    (setq kill-buffer-hook
          (remove #'markdown-ts-appear--kill-buffer-teardown
                  kill-buffer-hook))
    (add-hook 'kill-buffer-hook
              #'markdown-ts-appear--indirect-clone-teardown nil t)
    (add-hook 'change-major-mode-hook
              #'markdown-ts-appear--indirect-clone-major-mode-teardown nil t)
    (when (boundp 'evil-insert-state-entry-hook)
      (setq evil-insert-state-entry-hook
            (remove #'markdown-ts-appear--start
                    evil-insert-state-entry-hook)))
    (when (boundp 'evil-insert-state-exit-hook)
      (setq evil-insert-state-exit-hook
            (remove #'markdown-ts-appear--stop
                    evil-insert-state-exit-hook)))
    (when (boundp 'meow-insert-enter-hook)
      (setq meow-insert-enter-hook
            (remove #'markdown-ts-appear--start
                    meow-insert-enter-hook)))
    (when (boundp 'meow-insert-exit-hook)
      (setq meow-insert-exit-hook
            (remove #'markdown-ts-appear--stop
                    meow-insert-exit-hook)))
    (setq window-buffer-change-functions
          (remove #'markdown-ts-appear--math-preview-window
                  window-buffer-change-functions))
    (add-hook 'window-buffer-change-functions
              #'markdown-ts-appear--math-preview-owner-window nil t)
    (with-suppressed-warnings ((obsolete outline-view-change-hook))
      (setq outline-view-change-hook
            (remove #'markdown-ts-appear--math-outline-view-change
                    outline-view-change-hook)))
    (setq clone-indirect-buffer-hook
          (remove #'markdown-ts-appear--detach-indirect-clone
                  clone-indirect-buffer-hook))
    (setq local-minor-modes
          (remove 'markdown-ts-appear-mode local-minor-modes))
    (markdown-ts-appear--remove-block-font-lock)
    ;; The clone shares rendered text properties but has no parser to restore
    ;; them.
    (setq font-lock-extra-managed-props
          (remove 'display
                  (remove 'markdown-ts-appear--decoration
                          font-lock-extra-managed-props)))
    (markdown-ts-appear--release-code-prefix-properties)
    (markdown-ts-appear--install-view-rendering)))

(defun markdown-ts-appear--buffer-teardown ()
  "Disable Markdown TS Appear before replacing the current major mode."
  (when markdown-ts-appear--setup-p
    (markdown-ts-appear-mode -1))
  (markdown-ts-appear--release-indirect-clones))

(defun markdown-ts-appear--kill-buffer-teardown ()
  "Disable Markdown TS Appear before discarding the current buffer."
  (let ((markdown-ts-appear--tearing-down-buffer-p t))
    (markdown-ts-appear--buffer-teardown)))

(defun markdown-ts-appear--fontify-math (function node &rest arguments)
  "Call FUNCTION with NODE and ARGUMENTS, then render the LaTeX block."
  (if (not (or (markdown-ts-appear--active-p)
               (markdown-ts-appear--math-active-p)))
      (apply function node arguments)
    (let ((markdown-ts-hide-markup
           (and markdown-ts-hide-markup
                (not (and (markdown-ts-appear--active-p)
                          (markdown-ts-appear--node-visible-p node))))))
      (apply function node arguments))
    (markdown-ts-appear--math-preview-node node)))

(defun markdown-ts-appear--enable-trigger ()
  "Enable point tracking according to `markdown-ts-appear-trigger'."
  (pcase markdown-ts-appear-trigger
    ('always
     (markdown-ts-appear--start))
    ('evil-insert
     (add-hook 'evil-insert-state-entry-hook
               #'markdown-ts-appear--start nil t)
     (add-hook 'evil-insert-state-exit-hook
               #'markdown-ts-appear--stop nil t)
     (when (eq (bound-and-true-p evil-state) 'insert)
       (markdown-ts-appear--start)))
    ('meow-insert
     (add-hook 'meow-insert-enter-hook
               #'markdown-ts-appear--start nil t)
     (add-hook 'meow-insert-exit-hook
               #'markdown-ts-appear--stop nil t)
     (when (bound-and-true-p meow-insert-mode)
       (markdown-ts-appear--start)))))

(defun markdown-ts-appear--disable-trigger ()
  "Disable point tracking hooks in the current buffer."
  (when (boundp 'evil-insert-state-entry-hook)
    (remove-hook 'evil-insert-state-entry-hook
                 #'markdown-ts-appear--start t))
  (when (boundp 'evil-insert-state-exit-hook)
    (remove-hook 'evil-insert-state-exit-hook
                 #'markdown-ts-appear--stop t))
  (when (boundp 'meow-insert-enter-hook)
    (remove-hook 'meow-insert-enter-hook
                 #'markdown-ts-appear--start t))
  (when (boundp 'meow-insert-exit-hook)
    (remove-hook 'meow-insert-exit-hook
                 #'markdown-ts-appear--stop t))
  (markdown-ts-appear--stop))

;;;###autoload
(define-minor-mode markdown-ts-appear-mode
  "Reveal rendered Markdown source at point and preview unedited math."
  :lighter nil
  (cond
   ((and markdown-ts-appear-mode (not markdown-ts-appear--setup-p))
    (unless (derived-mode-p 'markdown-ts-mode)
      (markdown-ts-appear--deactivate-local-mode
       'markdown-ts-appear-mode)
      (user-error "Markdown TS Appear mode requires markdown-ts-mode"))
    (when (buffer-base-buffer)
      (markdown-ts-appear--deactivate-local-mode
       'markdown-ts-appear-mode)
      (user-error "Markdown TS Appear does not support indirect buffers"))
    (condition-case error-data
        (progn
          (when markdown-ts-appear-enable-math-preview
            (markdown-ts-appear--validate-math-options))
          (markdown-ts-appear--install-advice))
      (error
       (markdown-ts-appear--deactivate-local-mode
        'markdown-ts-appear-mode)
       (signal (car error-data) (cdr error-data))))
    (markdown-ts-appear--save-hide-markup)
    (setq markdown-ts-appear--setup-p t)
    (markdown-ts-appear--adopt-existing-indirect-clones)
    (markdown-ts-appear--install-all-view-rendering)
    (unless (memq 'line-height font-lock-extra-managed-props)
      (setq markdown-ts-appear--managed-line-height-p t)
      (add-to-list 'font-lock-extra-managed-props 'line-height))
    (unless markdown-ts-hide-markup
      (setq markdown-ts-hide-markup t)
      (markdown-ts--set-hide-markup t))
    (markdown-ts-appear--sync-indirect-clone-markup t)
    (add-hook 'after-change-functions #'markdown-ts-appear--after-change t t)
    (add-hook 'change-major-mode-hook
              #'markdown-ts-appear--buffer-teardown nil t)
    (add-hook 'kill-buffer-hook
              #'markdown-ts-appear--kill-buffer-teardown nil t)
    (add-hook 'clone-indirect-buffer-hook
              #'markdown-ts-appear--detach-indirect-clone nil t)
    (markdown-ts-appear--enable-trigger)
    (save-restriction
      (widen)
      (markdown-ts-appear--update-inline-ranges))
    (markdown-ts-appear--install-parser-notifiers)
    (when markdown-ts-appear-enable-math-preview
      (condition-case error-data
          (if (require 'mathjax nil t)
              (markdown-ts-appear--math-setup)
            (display-warning
             'markdown-ts-appear
             "Math preview enabled, but optional package `mathjax' is unavailable"
             :warning))
        (error
         (markdown-ts-appear--math-teardown)
         (display-warning
          'markdown-ts-appear
          (format "Math preview unavailable: %s"
                  (error-message-string error-data))
          :warning)))))
   ((and (not markdown-ts-appear-mode) markdown-ts-appear--setup-p)
    (remove-hook 'after-change-functions #'markdown-ts-appear--after-change t)
    (remove-hook 'change-major-mode-hook
                 #'markdown-ts-appear--buffer-teardown t)
    (remove-hook 'kill-buffer-hook
                 #'markdown-ts-appear--kill-buffer-teardown t)
    (remove-hook 'clone-indirect-buffer-hook
                 #'markdown-ts-appear--detach-indirect-clone t)
    (markdown-ts-appear--remove-parser-notifiers)
    (markdown-ts-appear--disable-trigger)
    (markdown-ts-appear--remove-all-view-rendering)
    (markdown-ts-appear--math-teardown)
    (save-restriction
      (widen)
      (markdown-ts-appear--delete-rendering-overlays)
      (markdown-ts-appear--delete-indirect-rendering-overlays)
      (markdown-ts-appear--sync-indirect-clone-markup nil)
      (markdown-ts-appear--restore-hide-markup)
      (unless markdown-ts-appear--tearing-down-buffer-p
        (condition-case nil
            (font-lock-ensure (point-min) (point-max))
          (treesit-parser-deleted
           (font-lock-flush (point-min) (point-max)))))
      (when markdown-ts-appear--managed-line-height-p
        (setq font-lock-extra-managed-props
              (remove 'line-height font-lock-extra-managed-props))
        (setq markdown-ts-appear--managed-line-height-p nil))
      (setq font-lock-extra-managed-props
            (remove 'markdown-ts-appear--decoration
                    font-lock-extra-managed-props)))
    (markdown-ts-appear--release-code-prefix-properties)
    (markdown-ts-appear--release-indirect-clones)
    (setq markdown-ts-appear--setup-p nil)))
  (markdown-ts-appear--refresh-advice))

(defconst markdown-ts-appear--visible-fontifiers
  '(markdown-ts--fontify-atx-heading
    markdown-ts--fontify-setext-heading
    markdown-ts--fontify-link-ref-label
    markdown-ts--fontify-link-ref-destination
    markdown-ts--fontify-unordered-list-marker
    markdown-ts--fontify-checkbox
    markdown-ts--fontify-autolink
    markdown-ts--fontify-backslash-escape
    markdown-ts--fontify-entity
    markdown-ts--fontify-hard-line-break
    markdown-ts--fontify-thematic-break)
  "Markdown fontifiers that replace or hide source markup.")

(defvar markdown-ts-appear--advice-installed-p nil
  "Non-nil when Markdown fontification advice is installed.")

(defun markdown-ts-appear--advice-bindings ()
  "Return the private Markdown functions and their package advice."
  (append
   `((markdown-ts--fontify-delimiter
      . ,#'markdown-ts-appear--fontify-delimiter)
     (markdown-ts--fontify-atx-delimiter
      . ,#'markdown-ts-appear--fontify-node)
     (markdown-ts--fontify-link-destination
      . ,#'markdown-ts-appear--fontify-link-destination)
     (markdown-ts--fontify-link-node
      . ,#'markdown-ts-appear--fontify-link)
     (markdown-ts--fontify-image
      . ,#'markdown-ts-appear--fontify-image)
     (markdown-ts--fontify-latex-block
      . ,#'markdown-ts-appear--fontify-math))
   (mapcar (lambda (function)
             (cons function #'markdown-ts-appear--fontify-visible-markup))
           markdown-ts-appear--visible-fontifiers)))

(defun markdown-ts-appear--private-api-contracts ()
  "Return expected signatures for private `markdown-ts-mode' functions."
  (append
   (mapcar (lambda (binding)
             (cons (car binding)
                   '(argument argument argument argument &rest argument)))
           (markdown-ts-appear--advice-bindings))
   '((markdown-ts--latex-block-valid-p argument)
     (markdown-ts--outline-invisible-p argument)
     (markdown-ts--make-link-button argument argument argument)
     (markdown-ts--set-hide-markup argument))))

(defun markdown-ts-appear--arglist-shape (arguments)
  "Return calling-convention shape of function ARGUMENTS."
  (mapcar (lambda (argument)
            (if (memq argument '(&optional &rest &key &allow-other-keys &aux))
                argument
              'argument))
          arguments))

(defun markdown-ts-appear--private-function-signature (function)
  "Return FUNCTION's calling convention, including while it is advised."
  (when (fboundp function)
    (when-let* ((arguments (help-function-arglist function t))
                ((listp arguments)))
      (markdown-ts-appear--arglist-shape arguments))))

(defun markdown-ts-appear--private-api-incompatibilities ()
  "Return private Markdown API contracts that no longer match."
  (seq-filter
   (lambda (contract)
     (let ((function (car contract))
           (expected (cdr contract)))
       (or (not (fboundp function))
           (not (equal (markdown-ts-appear--private-function-signature function)
                       expected)))))
   (markdown-ts-appear--private-api-contracts)))

(defun markdown-ts-appear--add-advice (symbol function)
  "Add FUNCTION around SYMBOL unless it is already present."
  (unless (advice-member-p function symbol)
    (advice-add symbol :around function)))

(defun markdown-ts-appear--install-advice ()
  "Install Markdown fontification advice."
  (unless markdown-ts-appear--advice-installed-p
    (let ((bindings (markdown-ts-appear--advice-bindings))
          (incompatible
           (markdown-ts-appear--private-api-incompatibilities)))
      (when incompatible
        (error
         "Private markdown-ts-mode API changed: %s"
         (mapconcat
          (lambda (contract)
            (let ((function (car contract)))
              (format "%s expected %S, got %S"
                      function (cdr contract)
                      (markdown-ts-appear--private-function-signature
                       function))))
          incompatible "; ")))
      (dolist (binding bindings)
        (markdown-ts-appear--add-advice (car binding) (cdr binding)))
      (setq markdown-ts-appear--advice-installed-p t))))

(defun markdown-ts-appear--remove-advice ()
  "Remove Markdown fontification advice."
  (when markdown-ts-appear--advice-installed-p
    (dolist (binding (markdown-ts-appear--advice-bindings))
      (advice-remove (car binding) (cdr binding)))
    (setq markdown-ts-appear--advice-installed-p nil)))

(defun markdown-ts-appear--refresh-advice ()
  "Remove global advice when no live buffer needs it."
  (unless
      (seq-some
       (lambda (buffer)
         (and (buffer-live-p buffer)
              (buffer-local-value 'markdown-ts-appear--setup-p buffer)))
       (buffer-list))
    (markdown-ts-appear--remove-advice)))

(defun markdown-ts-appear-unload-function ()
  "Remove global integration before unloading Markdown TS Appear."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when markdown-ts-appear--setup-p
        (markdown-ts-appear-mode -1))
      (when (or markdown-ts-appear--math-preview-active-p
                markdown-ts-appear--math-filter-installed-p)
        (markdown-ts-appear--math-teardown))))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when markdown-ts-appear--indirect-clones
        (markdown-ts-appear--release-indirect-clones))))
  (markdown-ts-appear--math-cancel-pending)
  (clrhash markdown-ts-appear--math-cache)
  (markdown-ts-appear--remove-advice)
  nil)

(provide 'markdown-ts-appear)

;;; markdown-ts-appear.el ends here
