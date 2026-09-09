;;; markdown-ts-appear.el --- Reveal Markdown source at point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael

;; Author: Thysrael <thysrael@163.com>
;; Assisted-by: OpenCode:gpt-5.6-sol
;; Maintainer: Thysrael <thysrael@163.com>
;; Version: 0.2.1
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
;; smallest semantic element at point, with optional visual decorations.
;;
;; Enable it with:
;;
;;   (add-hook 'markdown-ts-mode-hook #'markdown-ts-appear-mode)

;;; Code:

(require 'markdown-ts-mode)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function mathjax-available-p "mathjax")
(declare-function mathjax-display "mathjax")

;;; Options and faces

(defgroup markdown-ts-appear nil
  "Reveal rendered Markdown source at point."
  :group 'markdown-ts)

(defcustom markdown-ts-appear-enable-math-preview nil
  "Whether to preview unedited formulas with the optional MathJax package.
Re-enable `markdown-ts-appear-mode' after changing this option."
  :type 'boolean)

(defcustom markdown-ts-appear-link-icon ""
  "Icon displayed before rendered Markdown links.
An empty string disables the icon."
  :type 'string)

(defcustom markdown-ts-appear-image-icon ""
  "Icon displayed before rendered Markdown images.
An empty string disables the icon."
  :type 'string)

(defcustom markdown-ts-appear-wikilink-icon ""
  "Icon displayed before rendered Markdown Wiki links.
An empty string disables the icon."
  :type 'string)

(defcustom markdown-ts-appear-code-fence-style 'raw
  "How rendered fenced code blocks should display their delimiters."
  :type '(choice (const :tag "Raw Markdown" raw)
                 (const :tag "Connected Unicode lines" connected)))

(defcustom markdown-ts-appear-render-callouts nil
  "Whether to render callout labels at the start of block quotes."
  :type 'boolean)

(defcustom markdown-ts-appear-block-quote-marker nil
  "Marker replacing each marker of a rendered block quote.
When nil, preserve the original Markdown marker."
  :type '(choice (const :tag "Display raw marker" nil)
                 (string :tag "Marker")))

(defcustom markdown-ts-appear-table-style 'raw
  "How rendered Markdown pipe tables should display their delimiters."
  :type '(choice (const :tag "Raw Markdown" raw)
                 (const :tag "Unicode delimiters" unicode)))

(defface markdown-ts-appear-code-fence-marker
  '((t :inherit (markdown-ts-language-keyword markdown-ts-code-block)))
  "Face used for rendered fenced code block markers.")

(defface markdown-ts-appear-label
  '((t :inverse-video t))
  "Face used to invert rendered code language and callout labels.")

(defface markdown-ts-appear-block-quote-marker
  '((t :inherit font-lock-comment-face))
  "Face used for rendered block quote markers.")

(defvar-local markdown-ts-appear-mode nil
  "Non-nil when Markdown TS Appear mode is enabled.")

(defvar-local markdown-ts-appear--region nil
  "Markers delimiting the semantic Markdown source currently visible.")

(defvar-local markdown-ts-appear-math--objects nil
  "Formula overlays, each holding its source, image and pending render buffer.")

(defvar-local markdown-ts-appear--last-point nil
  "Buffer position checked by the most recent reveal update.")

(defvar-local markdown-ts-appear--last-tick nil
  "Buffer modification tick checked by the most recent reveal update.")

(defvar-local markdown-ts-appear--last-range-line nil
  "Line beginning whose inline Tree-sitter ranges were last updated.")

(defvar-local markdown-ts-appear--last-range-tick nil
  "Modification tick of the last inline Tree-sitter range update.")

(defvar-local markdown-ts-appear--managed-properties nil
  "Properties added by reveal mode to `font-lock-extra-managed-props'.")

(defvar-local markdown-ts-appear--block-font-lock-settings nil
  "Tree-sitter font-lock settings installed in the current buffer.")

(defvar markdown-ts-appear--tearing-down-buffer-p nil
  "Non-nil while cleanup is running for a buffer being discarded.")

(defvar markdown-ts-appear--quote-font-lock-settings)
(defvar markdown-ts-appear--code-font-lock-settings)
(defvar markdown-ts-appear--table-font-lock-settings)

(defun markdown-ts-appear--active-p ()
  "Return non-nil when reveal rendering owns this buffer's text."
  (and markdown-ts-appear-mode
       (memq #'markdown-ts-appear--after-change after-change-functions)))

(defun markdown-ts-appear--deactivate-mode ()
  "Clear Markdown TS Appear mode and unregister it locally."
  (setq markdown-ts-appear-mode nil)
  (setq local-minor-modes
        (delq 'markdown-ts-appear-mode local-minor-modes)))

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

(defun markdown-ts-appear--remove-markup-invisibility (beg end)
  "Remove Markdown markup invisibility between BEG and END."
  (let ((pos beg))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'invisible nil end)))
        (when (eq (get-text-property pos 'invisible) 'markdown-ts--markup)
          (remove-text-properties pos next '(invisible nil)))
        (setq pos next)))))

(defun markdown-ts-appear--label (text face)
  "Render TEXT as a padded inverse-video label over FACE."
  (propertize (concat " " text " ")
              'face (list 'markdown-ts-appear-label face)))

(defun markdown-ts-appear--code-quote-prefix (source)
  "Render quoted code prefix SOURCE followed by a code block marker."
  (let ((marker (or markdown-ts-appear-block-quote-marker ">")))
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

;;; Source bounds and point tracking

(defun markdown-ts-appear--node-ancestor (node type)
  "Return NODE or its nearest ancestor whose type is TYPE."
  (treesit-parent-until
   node (lambda (candidate) (equal (treesit-node-type candidate) type)) t))

(defun markdown-ts-appear--direct-children-of-type (node type)
  "Return direct children of NODE whose type is TYPE."
  (treesit-filter-child
   node (lambda (child) (equal (treesit-node-type child) type))))

(defun markdown-ts-appear--first-direct-child-of-type (node type)
  "Return the first direct child of NODE whose type is TYPE."
  (catch 'child
    (dotimes (index (treesit-node-child-count node))
      (let ((child (treesit-node-child node index)))
        (when (equal (treesit-node-type child) type)
          (throw 'child child))))))

(defun markdown-ts-appear--region-visible-p (beg end)
  "Return non-nil when BEG through END overlaps visible Markdown source."
  (when-let* ((region markdown-ts-appear--region)
              (visible-beg (marker-position (car region)))
              (visible-end (marker-position (cdr region))))
    (and (< beg visible-end) (> end visible-beg))))

(defun markdown-ts-appear--node-visible-p (node)
  "Return non-nil when NODE overlaps visible semantic Markdown source."
  (markdown-ts-appear--region-visible-p
   (treesit-node-start node) (treesit-node-end node)))

(defun markdown-ts-appear--literal-block-at (position)
  "Return the literal Markdown block containing POSITION, if any."
  (when-let* ((block
               (treesit-parent-until
                (treesit-node-at position 'markdown)
                "\\`\\(?:fenced_code_block\\|indented_code_block\\|html_block\\)\\'"
                t))
              (_ (<= (treesit-node-start block) position))
              (_ (< position (treesit-node-end block))))
    block))

(defun markdown-ts-appear--wikilink-bounds-for-node (node)
  "Return Wiki link bounds around shortcut link NODE, if any."
  (when-let* ((_ node)
              (_ (equal (treesit-node-type node) "shortcut_link"))
              (previous (treesit-node-prev-sibling node))
              (next (treesit-node-next-sibling node))
              (_ (equal (treesit-node-type previous) "["))
              (_ (equal (treesit-node-type next) "]"))
              (_ (= (treesit-node-end previous) (treesit-node-start node)))
              (_ (= (treesit-node-start next) (treesit-node-end node)))
              (_ (not (markdown-ts-appear--node-ancestor
                       (treesit-node-parent node) "image"))))
    (cons (treesit-node-start previous) (treesit-node-end next))))

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
  (when-let* ((region markdown-ts-appear--region))
    (let ((beg (marker-position (car region)))
          (end (marker-position (cdr region))))
      (set-marker (car region) nil)
      (set-marker (cdr region) nil)
      (setq markdown-ts-appear--region nil)
      (when (and beg end (not markdown-ts-appear--tearing-down-buffer-p))
        (save-restriction
          (widen)
          (font-lock-flush beg end)
          (condition-case nil
              (font-lock-ensure beg end)
            (treesit-parser-deleted nil)))))))

(defun markdown-ts-appear--bounds-at-point (pos)
  "Return source bounds for the smallest rendered element at POS."
  (save-excursion
    (goto-char pos)
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
           (inline-node
            (treesit-parent-until
             (treesit-node-at pos 'markdown-inline)
             (lambda (node)
               (let ((type (treesit-node-type node)))
                 (and
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
                            (markdown-ts-appear--node-ancestor
                             (treesit-node-parent node) "image")))
                  (or (not (equal type "latex_block"))
                      (markdown-ts--latex-block-valid-p node)))))
             t)))
      (cond
       (wikilink-bounds)
       (inline-node
        ;; The inline grammar represents `~~text~~' as nested strikethroughs.
        (when (equal (treesit-node-type inline-node) "strikethrough")
          (setq inline-node
                (treesit-parent-while inline-node "\\`strikethrough\\'")))
        (cons (treesit-node-start inline-node)
              (treesit-node-end inline-node)))
       (t
        (let ((structural-node
               (treesit-parent-until
                (treesit-node-at pos 'markdown)
                (lambda (node)
                  (and
                   (funcall contains-p node)
                   (member
                    (treesit-node-type node)
                    '("atx_heading" "setext_heading" "list_item"
                      "task_list_marker_unchecked"
                      "task_list_marker_checked"
                      "pipe_table_header" "pipe_table_row"
                      "pipe_table_delimiter_row" "thematic_break"
                      "link_reference_definition"))))
                t)))
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
                         (_ (string-prefix-p
                             "list_marker_" (treesit-node-type marker)))
                         (_ (= line-beg
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
                   (treesit-node-end structural-node))))))))))

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
       ((and (<= (treesit-node-start closing) position)
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
      (setq marker
            (treesit-parent-until
             (pop nodes)
             (lambda (node)
               (and (member (treesit-node-type node)
                            '("block_quote_marker" "block_continuation"))
                    (<= (treesit-node-start node) position)
                    (<= position (treesit-node-end node))
                    (string-search ">" (treesit-node-text node t))))
             t)))
    (when marker
      (cons (treesit-node-start marker) (treesit-node-end marker)))))

(defun markdown-ts-appear--callout-data (quote)
  "Return (BEG END TYPE) for a callout marker starting block QUOTE."
  (save-match-data
    (when-let* ((opening
                 (markdown-ts-appear--first-direct-child-of-type
                  quote "block_quote_marker"))
                (beg (treesit-node-end opening)))
      (save-excursion
        (goto-char beg)
        (when (re-search-forward
               "\\=\\[!\\([[:alnum:]_-]+\\)\\]\\([+-]\\)?"
               (line-end-position) t)
          (list beg (point) (match-string-no-properties 1)))))))

(defun markdown-ts-appear--callout-bounds-at (position)
  "Return rendered callout marker bounds containing POSITION."
  (when-let* ((_ markdown-ts-appear-render-callouts)
              (node (markdown-ts-appear--markdown-node-at position))
              (quote (markdown-ts-appear--node-ancestor node "block_quote"))
              (data (markdown-ts-appear--callout-data quote))
              (beg (nth 0 data))
              (end (nth 1 data))
              (_ (<= beg position))
              (_ (< position end)))
    (cons beg end)))

(defun markdown-ts-appear--callout-link-p (link)
  "Return non-nil when shortcut LINK is a rendered callout marker."
  (when-let* ((_ markdown-ts-appear-render-callouts)
              (_ (equal (treesit-node-type link) "shortcut_link"))
              (markdown-node
               (markdown-ts-appear--markdown-node-at
                (treesit-node-start link)))
              (quote
               (markdown-ts-appear--node-ancestor markdown-node "block_quote"))
              (data (markdown-ts-appear--callout-data quote))
              (beg (nth 0 data))
              (end (nth 1 data))
              (label-end
               (if (memq (char-before end) '(?+ ?-)) (1- end) end)))
    (and (= (treesit-node-start link) beg)
         (= (treesit-node-end link) label-end))))

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
      (setq markdown-ts-appear--last-range-tick tick))))

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

(defun markdown-ts-appear-start ()
  "Start tracking the semantic Markdown element at point."
  (interactive)
  (when (markdown-ts-appear--active-p)
    (setq markdown-ts-appear--last-point nil)
    (setq markdown-ts-appear--last-tick nil)
    (setq markdown-ts-appear--last-range-line nil)
    (setq markdown-ts-appear--last-range-tick nil)
    (add-hook 'post-command-hook #'markdown-ts-appear--update nil t)
    (markdown-ts-appear--update)))

(defun markdown-ts-appear-stop ()
  "Stop tracking point and restore hidden Markdown markup."
  (interactive)
  (remove-hook 'post-command-hook #'markdown-ts-appear--update t)
  (setq markdown-ts-appear--last-point nil)
  (setq markdown-ts-appear--last-tick nil)
  (setq markdown-ts-appear--last-range-line nil)
  (setq markdown-ts-appear--last-range-tick nil)
  (markdown-ts-appear--restore))

;;; Fontification and decorations

(defun markdown-ts-appear--decoration-filter-copied-text (text)
  "Remove package decoration properties from copied Markdown TEXT."
  (let ((pos 0)
        (end (length text)))
    (while (< pos end)
      (let ((next (next-single-property-change
                   pos 'markdown-ts-appear--decoration text end)))
        (when (get-text-property pos 'markdown-ts-appear--decoration text)
          (remove-text-properties
           pos next '(display nil line-height nil line-prefix nil wrap-prefix nil
			      markdown-ts-appear--decoration nil)
           text))
        (setq pos next))))
  text)

(defun markdown-ts-appear--install-block-font-lock ()
  "Install block rendering rules in the current buffer."
  (unless markdown-ts-appear--block-font-lock-settings
    (when-let* ((settings
                 (append
                  (and (or markdown-ts-appear-block-quote-marker
                           markdown-ts-appear-render-callouts)
                       markdown-ts-appear--quote-font-lock-settings)
                  (and (eq markdown-ts-appear-code-fence-style 'connected)
                       markdown-ts-appear--code-font-lock-settings)
                  (and (eq markdown-ts-appear-table-style 'unicode)
                       markdown-ts-appear--table-font-lock-settings))))
      (let ((previous-settings treesit-font-lock-settings))
        (treesit-add-font-lock-rules settings)
        (setq markdown-ts-appear--block-font-lock-settings
              (seq-remove
               (lambda (setting) (memq setting previous-settings))
               treesit-font-lock-settings)))))
  (add-to-list 'font-lock-extra-managed-props
               'markdown-ts-appear--decoration)
  (dolist (property (cons 'line-height
                          (and (eq markdown-ts-appear-code-fence-style 'connected)
                               '(line-prefix wrap-prefix))))
    (unless (memq property font-lock-extra-managed-props)
      (push property markdown-ts-appear--managed-properties)
      (add-to-list 'font-lock-extra-managed-props property)))
  (unless (advice-function-member-p
           #'markdown-ts-appear--decoration-filter-copied-text
           filter-buffer-substring-function)
    (add-function :filter-return (local 'filter-buffer-substring-function)
                  #'markdown-ts-appear--decoration-filter-copied-text)))

(defun markdown-ts-appear--remove-block-font-lock ()
  "Remove block rendering rules from the current buffer."
  (when markdown-ts-appear--block-font-lock-settings
    (setq treesit-font-lock-settings
          (seq-remove
           (lambda (setting)
             (memq setting markdown-ts-appear--block-font-lock-settings))
           treesit-font-lock-settings))
    (setq markdown-ts-appear--block-font-lock-settings nil))
  (remove-function (local 'filter-buffer-substring-function)
                   #'markdown-ts-appear--decoration-filter-copied-text))

(defun markdown-ts-appear--fontify-node
    (function node override start limit &rest rest)
  "Call FUNCTION for NODE without covering visible source."
  (if (not (markdown-ts-appear--active-p))
      (apply function node override start limit rest)
    (let ((markdown-ts-hide-markup
           (and markdown-ts-hide-markup
                (not (markdown-ts-appear--node-visible-p node)))))
      (apply function node override start limit rest))))

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
            (markdown-ts-appear--decorate
             (treesit-node-start node) (treesit-node-end node)
             (if info
                 (concat
                  (propertize "╭─" 'face 'markdown-ts-appear-code-fence-marker)
                  (markdown-ts-appear--label
                   (treesit-node-text info t) 'markdown-ts-appear-code-fence-marker))
               "╭─")
             (unless info 'markdown-ts-appear-code-fence-marker))
            (when (< (treesit-node-end node) header-end)
              (markdown-ts-appear--decorate
               (treesit-node-end node) header-end "" nil))))
      (markdown-ts-appear--decorate
       (treesit-node-start node) (treesit-node-end node)
       "╰─"
       'markdown-ts-appear-code-fence-marker))))

(defun markdown-ts-appear--quote-prefixes (node start limit)
  "Return structural quote prefixes in NODE intersecting START through LIMIT.
Each prefix is a list of marker bounds in source order.  Bounds include
at most one following space or tab and are not clipped to START or LIMIT."
  (save-match-data
    (save-excursion
      (let (prefixes)
        (dolist (prefix (treesit-query-capture
                         node '([(block_quote_marker) (block_continuation)] @prefix)
                         start limit t))
          (goto-char (treesit-node-start prefix))
          (let ((end (treesit-node-end prefix))
                markers)
            (while (search-forward ">" end t)
              (push (cons (1- (point))
                          (if (and (< (point) end)
                                   (memq (char-after) '(?\s ?\t)))
                              (1+ (point))
                            (point)))
                    markers))
            (when markers
              (push (nreverse markers) prefixes))))
        (nreverse prefixes)))))

(defun markdown-ts-appear--fontify-code-block
    (node _override start limit &rest _)
  "Render the content prefix of fenced code block NODE in START through LIMIT."
  (when-let* (((markdown-ts-appear--active-p))
              ((eq markdown-ts-appear-code-fence-style 'connected))
              (content (markdown-ts-appear--first-direct-child-of-type
                        node "code_fence_content")))
    (let* ((delimiters (markdown-ts-appear--direct-children-of-type
                        node "fenced_code_block_delimiter"))
           (closing (and (cdr delimiters) (car (last delimiters))))
           (content-beg (treesit-node-start content))
           (body-beg (save-excursion
                       (goto-char content-beg)
                       (line-beginning-position)))
           (body-end (if closing
                         (save-excursion
                           (goto-char (treesit-node-start closing))
                           (line-beginning-position))
                       (treesit-node-end content)))
           (beg (max start body-beg))
           (end (min limit body-end)))
      (when (< beg end)
        (if (not (markdown-ts-appear--node-ancestor
                  (treesit-node-parent node) "block_quote"))
            (markdown-ts-appear--decorate-line-prefix
             beg end "│ " 'markdown-ts-appear-code-fence-marker)
          (let ((wrap-beg (max beg content-beg))
                (wrap-prefix
                 (markdown-ts-appear--code-quote-prefix
                  (buffer-substring-no-properties body-beg content-beg))))
            (when (< wrap-beg end)
              (with-silent-modifications
                (add-text-properties
                 wrap-beg end `(wrap-prefix ,wrap-prefix
                                            markdown-ts-appear--decoration t)))))
          (dolist (prefix (markdown-ts-appear--quote-prefixes node beg end))
            (pcase-let ((`(,marker-beg . ,marker-end) (car (last prefix))))
              (when (and (<= beg marker-beg) (< marker-beg end)
                         (not (markdown-ts-appear--region-visible-p
                               marker-beg (1+ marker-beg))))
                (markdown-ts-appear--decorate
                 marker-beg (min end marker-end)
                 (markdown-ts-appear--code-quote-prefix "> ") nil)))))))))

(defun markdown-ts-appear--fontify-callout (node start limit)
  "Render a callout label at the start of block quote NODE in START to LIMIT."
  (when-let* ((markdown-ts-appear-render-callouts)
              (data (markdown-ts-appear--callout-data node))
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
      (nth 2 data)
      (pcase (upcase (nth 2 data))
        ("NOTE" 'link)
        ("TIP" 'success)
        ("IMPORTANT" 'font-lock-keyword-face)
        ("WARNING" 'warning)
        ("CAUTION" 'error)
        (_ 'markdown-ts-appear-block-quote-marker)))
     nil)))

(defun markdown-ts-appear--fontify-quote-marker (node visible-p start limit)
  "Render NODE's quote markers between START and LIMIT unless VISIBLE-P."
  (when (and (not visible-p) markdown-ts-appear-block-quote-marker
             (< (max start (treesit-node-start node))
                (min limit (treesit-node-end node))))
    (dolist (prefix (markdown-ts-appear--quote-prefixes node start limit))
      (dolist (bounds prefix)
        (let ((beg (car bounds)))
          (when (and (<= start beg) (< beg limit)
                     (not (markdown-ts-appear--region-visible-p beg (1+ beg))))
            (markdown-ts-appear--decorate
             beg (1+ beg) markdown-ts-appear-block-quote-marker
             'markdown-ts-appear-block-quote-marker)))))))

(defun markdown-ts-appear--fontify-delimiter
    (function node override start limit &rest rest)
  "Call FUNCTION for NODE, rendering structural delimiters."
  (if (not (markdown-ts-appear--active-p))
      (apply function node override start limit rest)
    (let* ((type (treesit-node-type node))
           (hide-markup-p markdown-ts-hide-markup)
           (visible-p (markdown-ts-appear--node-visible-p node))
           (quote-marker-p
            (and (member type '("block_quote_marker" "block_continuation"))
                 (string-search ">" (treesit-node-text node t))))
           (fence-p (member type '("fenced_code_block_delimiter"
                                   "info_string")))
           (markdown-ts-hide-markup
            (and markdown-ts-hide-markup
                 (not (or quote-marker-p fence-p
                          visible-p)))))
      (apply function node override start limit rest)
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

(defun markdown-ts-appear--fontify-block-quote
    (node _override start limit &rest _)
  "Render block quote NODE between START and LIMIT."
  (when (markdown-ts-appear--active-p)
    (let ((beg (max start (treesit-node-start node)))
          (end (min limit (treesit-node-end node))))
      (when (< beg end)
        (add-face-text-property beg end 'markdown-ts-block-quote t)
        (markdown-ts-appear--fontify-callout node beg end)
        (markdown-ts-appear--fontify-quote-marker node nil beg end)))))

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
                      '("pipe_table_header" "pipe_table_delimiter_row"
                        "pipe_table_row"))
          (markdown-ts-appear--fontify-table-row row start limit))
        (setq row (treesit-node-next-sibling row))))))

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
    (function node override start limit &rest rest)
  "Call FUNCTION for NODE without covering visible markup."
  (if (not (markdown-ts-appear--active-p))
      (apply function node override start limit rest)
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
      (apply function node override start limit rest)
      (when (and markdown-ts-hide-markup
                 (equal type "setext_heading")
                 markup-node
                 (text-property-any
                  (treesit-node-start markup-node) (treesit-node-end markup-node)
                  'line-height 0))
        (with-silent-modifications
          (put-text-property
           (treesit-node-start markup-node) (treesit-node-end markup-node)
           'markdown-ts-appear--decoration t))))))

(defun markdown-ts-appear--icon (type)
  "Return the configured Markdown icon for TYPE, or nil when empty."
  (let ((icon (pcase type
                ('image markdown-ts-appear-image-icon)
                ('wikilink markdown-ts-appear-wikilink-icon)
                (_ markdown-ts-appear-link-icon))))
    (unless (string-empty-p icon)
      (propertize icon 'face 'markdown-ts-link))))

(defun markdown-ts-appear--fontify-link-destination
    (function node override start limit &rest rest)
  "Call FUNCTION for NODE, preserving useful image labels."
  (if (not (markdown-ts-appear--active-p))
      (apply function node override start limit rest)
    (let* ((parent (treesit-node-parent node))
           (image-p (equal (treesit-node-type parent) "image"))
           (beg (and image-p (treesit-node-start parent)))
           (end (and image-p (treesit-node-end parent)))
           (visible-p (markdown-ts-appear--node-visible-p node))
           (markdown-ts-hide-markup
            (and markdown-ts-hide-markup (not visible-p))))
      (apply function node override start limit rest)
      (when image-p
        (dolist (overlay (overlays-in beg end))
          (when (overlay-get overlay 'markdown-ts-appear--image-label)
            (delete-overlay overlay)))
        (when (and markdown-ts-hide-markup
                   (not (markdown-ts--outline-invisible-p beg)))
          (let* ((description
                  (treesit-search-subtree parent "\\`image_description\\'"))
                 (url (treesit-node-text node t))
                 (label (file-name-nondirectory url))
                 (icon (markdown-ts-appear--icon 'image)))
            (with-silent-modifications
              (unless description
                (markdown-ts-appear--remove-markup-invisibility
                 (treesit-node-start node) (treesit-node-end node))
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

(defun markdown-ts-appear--fontify-image
    (function node override start limit &rest rest)
  "Call FUNCTION for NODE without covering image source."
  (if (not (markdown-ts-appear--active-p))
      (apply function node override start limit rest)
    (let ((markdown-ts-inline-images
           (and markdown-ts-inline-images
                (not (markdown-ts-appear--node-visible-p node)))))
      (apply function node override start limit rest))))

(defun markdown-ts-appear--fontify-link
    (function node override start limit &rest rest)
  "Call FUNCTION for NODE, prefixing links with an icon."
  (if (not (markdown-ts-appear--active-p))
      (apply function node override start limit rest)
    (apply function node override start limit rest)
    (let* ((parent (treesit-node-parent node))
           (parent-beg (treesit-node-start parent))
           (parent-end (treesit-node-end parent))
           (beg (treesit-node-start node))
           (end (treesit-node-end node)))
      (cond
       ((markdown-ts-appear--callout-link-p parent)
        (with-silent-modifications
          (remove-list-of-text-properties
           parent-beg parent-end
           '(action button category follow-link help-echo keymap mouse-face)))
        (dolist (overlay (overlays-in parent-beg parent-end))
          (when (overlay-get overlay 'markdown-ts-appear--link-icon)
            (delete-overlay overlay))))
       ((and (equal (treesit-node-type node) "link_label")
             (treesit-search-subtree parent "\\`link_text\\'"))
        (with-silent-modifications
          (if (and markdown-ts-hide-markup
                   (not (markdown-ts-appear--node-visible-p node)))
              (put-text-property beg end 'invisible 'markdown-ts--markup)
            (markdown-ts-appear--remove-markup-invisibility beg end)))
        nil)
       ((not (markdown-ts-appear--node-ancestor parent "image"))
        (let* ((wikilink-p (markdown-ts-appear--wikilink-bounds-for-node parent))
               (alias-delimiter
                (and wikilink-p
                     (markdown-ts-appear--first-direct-child-of-type node "|")))
               (alias-beg (and alias-delimiter (treesit-node-end alias-delimiter)))
               (icon-beg (or alias-beg beg))
               (visible-p
                (or (markdown-ts-appear--region-visible-p parent-beg beg)
                    (markdown-ts-appear--region-visible-p end parent-end))))
          (when alias-beg
            (with-silent-modifications
              (markdown-ts--make-link-button
               beg end
               (buffer-substring-no-properties
                beg (treesit-node-start alias-delimiter)))))
          (dolist (overlay (overlays-in beg end))
            (when (overlay-get overlay 'markdown-ts-appear--link-icon)
              (delete-overlay overlay)))
          (when (and markdown-ts-hide-markup (not visible-p))
            (when wikilink-p
              (with-silent-modifications
                (put-text-property (1- parent-beg) parent-beg
                                   'invisible 'markdown-ts--markup)
                (put-text-property parent-end (1+ parent-end)
                                   'invisible 'markdown-ts--markup)
                (when alias-beg
                  (let* ((region markdown-ts-appear--region)
                         (visible-beg (and region (marker-position (car region))))
                         (visible-end (and region (marker-position (cdr region)))))
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
                                         'invisible 'markdown-ts--markup))))))
            (when-let* ((icon (markdown-ts-appear--icon
                               (if wikilink-p 'wikilink 'link))))
              (let ((overlay (make-overlay icon-beg (min (1+ icon-beg) end)
                                           nil t nil)))
                (overlay-put overlay 'markdown-ts-appear--link-icon t)
                (overlay-put overlay 'before-string (concat icon " "))
                (overlay-put overlay 'evaporate t))))))))))

;;; Buffer lifecycle

(defun markdown-ts-appear--delete-rendering-overlays ()
  "Delete package icon overlays throughout the buffer."
  (save-restriction
    (widen)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (or (overlay-get overlay 'markdown-ts-appear--image-label)
                (overlay-get overlay 'markdown-ts-appear--link-icon))
        (delete-overlay overlay)))))

(defun markdown-ts-appear--after-change (_beg _end _old-length)
  "Invalidate icons after edits, including distant structural changes."
  (save-restriction
    (widen)
    (markdown-ts-appear--delete-rendering-overlays)
    ;; Rebuild unchanged icons too: their text may already be fontified.
    (font-lock-flush (point-min) (point-max))))

(defun markdown-ts-appear--install-buffer-hooks ()
  "Install buffer-local lifecycle and change hooks."
  (add-hook 'after-change-functions #'markdown-ts-appear--after-change t t)
  (add-hook 'change-major-mode-hook
            #'markdown-ts-appear--buffer-teardown nil t)
  (add-hook 'kill-buffer-hook
            #'markdown-ts-appear--kill-buffer-teardown nil t)
  (add-hook 'clone-indirect-buffer-hook
            #'markdown-ts-appear--detach-indirect-clone nil t))

(defun markdown-ts-appear--remove-buffer-hooks ()
  "Remove buffer-local lifecycle and change hooks."
  (remove-hook 'after-change-functions #'markdown-ts-appear--after-change t)
  (remove-hook 'change-major-mode-hook
               #'markdown-ts-appear--buffer-teardown t)
  (remove-hook 'kill-buffer-hook
               #'markdown-ts-appear--kill-buffer-teardown t)
  (remove-hook 'clone-indirect-buffer-hook
               #'markdown-ts-appear--detach-indirect-clone t))

(defun markdown-ts-appear--release-managed-properties ()
  "Release font-lock properties managed by the current buffer."
  (dolist (property markdown-ts-appear--managed-properties)
    (setq font-lock-extra-managed-props
          (remove property font-lock-extra-managed-props)))
  (setq markdown-ts-appear--managed-properties nil)
  (setq font-lock-extra-managed-props
        (remove 'markdown-ts-appear--decoration
                font-lock-extra-managed-props)))

(defun markdown-ts-appear--detach-indirect-clone ()
  "Detach active tracking inherited by a new indirect clone.
Indirect buffers are otherwise unsupported and are not synchronized."
  (when (buffer-base-buffer)
    ;; Do not run normal teardown: text properties are shared with the base.
    (setq markdown-ts-appear-mode nil)
    (setq markdown-ts-appear--region nil)
    (setq markdown-ts-appear-math--objects nil)
    (markdown-ts-appear-math--teardown)
    (setq local-minor-modes
          (remove 'markdown-ts-appear-mode local-minor-modes))
    (remove-hook 'post-command-hook #'markdown-ts-appear--update t)
    (markdown-ts-appear--remove-buffer-hooks)
    (markdown-ts-appear--remove-block-font-lock)
    (setq font-lock-extra-managed-props
          (remove 'display
                  font-lock-extra-managed-props))
    (markdown-ts-appear--release-managed-properties)))

(defun markdown-ts-appear--buffer-teardown ()
  "Disable Markdown TS Appear before replacing the current major mode."
  (when (memq #'markdown-ts-appear--after-change after-change-functions)
    (markdown-ts-appear-mode -1)))

(defun markdown-ts-appear--kill-buffer-teardown ()
  "Disable Markdown TS Appear before discarding the current buffer."
  (let ((markdown-ts-appear--tearing-down-buffer-p t))
    (markdown-ts-appear--buffer-teardown)))

(defun markdown-ts-appear--enable-buffer ()
  "Install Markdown TS Appear integration in the current buffer."
  (markdown-ts-appear--install-buffer-hooks)
  (markdown-ts-appear--install-block-font-lock)
  (unless markdown-ts-hide-markup
    (setq markdown-ts-hide-markup t)
    (markdown-ts--set-hide-markup t))
  (save-restriction
    (widen)
    (font-lock-flush (point-min) (point-max)))
  (markdown-ts-appear-start)
  (when markdown-ts-appear-enable-math-preview
    (markdown-ts-appear-math--setup)))

(defun markdown-ts-appear--disable-buffer ()
  "Remove Markdown TS Appear integration from the current buffer."
  (markdown-ts-appear--remove-buffer-hooks)
  (markdown-ts-appear-math--teardown)
  (markdown-ts-appear-stop)
  (markdown-ts-appear--remove-block-font-lock)
  (markdown-ts-appear--delete-rendering-overlays)
  (save-restriction
    (widen)
    (kill-local-variable 'markdown-ts-hide-markup)
    (markdown-ts--set-hide-markup markdown-ts-hide-markup)
    (unless markdown-ts-appear--tearing-down-buffer-p
      (condition-case nil
          (font-lock-ensure (point-min) (point-max))
        (treesit-parser-deleted
         (font-lock-unfontify-region (point-min) (point-max))))))
  (markdown-ts-appear--release-managed-properties))

;;;###autoload
(define-minor-mode markdown-ts-appear-mode
  "Reveal rendered Markdown source at point.
Disabling the mode resets `markdown-ts-hide-markup' to its current default."
  :lighter nil
  ;; The mode variable is already set, so inspect the installed hook instead.
  (let ((initialized
         (memq #'markdown-ts-appear--after-change after-change-functions)))
    (if markdown-ts-appear-mode
        (unless initialized
          (unless (derived-mode-p 'markdown-ts-mode)
            (markdown-ts-appear--deactivate-mode)
            (user-error "Markdown TS Appear mode requires markdown-ts-mode"))
          (when (buffer-base-buffer)
            (markdown-ts-appear--deactivate-mode)
            (user-error "Markdown TS Appear does not support indirect buffers"))
          (markdown-ts-appear--enable-buffer))
      (when initialized
        (markdown-ts-appear--disable-buffer)))))

;;; Native fontifier advice

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
      . ,#'markdown-ts-appear--fontify-node))
   (mapcar (lambda (function)
             (cons function #'markdown-ts-appear--fontify-visible-markup))
           markdown-ts-appear--visible-fontifiers)))

(defconst markdown-ts-appear--required-private-functions
  '(markdown-ts--latex-block-valid-p
    markdown-ts--outline-invisible-p
    markdown-ts--make-link-button
    markdown-ts--set-hide-markup)
  "Private Markdown helpers required by Markdown TS Appear.")

(defun markdown-ts-appear--missing-private-functions ()
  "Return required private Markdown functions that are unavailable."
  (seq-remove
   #'fboundp
   (append (mapcar #'car (markdown-ts-appear--advice-bindings))
           markdown-ts-appear--required-private-functions)))

(defun markdown-ts-appear--set-advice (install-p)
  "Install package advice when INSTALL-P is non-nil; otherwise remove it."
  (dolist (binding (markdown-ts-appear--advice-bindings))
    (if install-p
        (unless (advice-member-p (cdr binding) (car binding))
          (advice-add (car binding) :around (cdr binding)))
      (when (advice-member-p (cdr binding) (car binding))
        (advice-remove (car binding) (cdr binding))))))

(defun markdown-ts-appear--install-advice ()
  "Install Markdown fontification advice."
  (when-let* ((missing (markdown-ts-appear--missing-private-functions)))
    (error "Required private markdown-ts-mode functions are unavailable: %S"
           missing))
  (markdown-ts-appear--set-advice t))

;;; Optional math previews

(defun markdown-ts-appear-math--delete (preview)
  "Remove PREVIEW and invalidate its pending render, if any."
  (let ((staging (overlay-get preview 'markdown-ts-appear-math--buffer)))
    (delete-overlay preview)
    (setq markdown-ts-appear-math--objects
          (delq preview markdown-ts-appear-math--objects))
    (when (buffer-live-p staging)
      (kill-buffer staging))))

(defun markdown-ts-appear-math--clear (&optional beg end)
  "Clear previews, or only those whose source is edited between BEG and END."
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
    ;; MathJax deletes existing `mathjax' overlays BEFORE calling :after.
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
                                 (markdown-ts-appear-math--display preview))
                             (markdown-ts-appear-math--delete preview)))))))
               (overlay-put preview 'markdown-ts-appear-math--buffer nil)
               (delete-overlay overlay)
               (when (buffer-live-p staging) (kill-buffer staging)))))
        (error
         (overlay-put preview 'markdown-ts-appear-math--buffer nil)
         (kill-buffer staging)
         (message "Markdown MathJax preview failed: %s" (error-message-string err)))))))

(defun markdown-ts-appear-math--refresh (&rest _)
  "Synchronize previews after core has updated its source reveal region."
  (if (not (and markdown-ts-appear-enable-math-preview
                (markdown-ts-appear--active-p)))
      (markdown-ts-appear-math--clear)
    (save-restriction
      (widen)
      (treesit-update-ranges (point-min) (point-max))
      (let ((existing (make-hash-table :test #'eql))
            (current (make-hash-table :test #'eq))
            requests)
        (dolist (preview markdown-ts-appear-math--objects)
          (when (eq (overlay-buffer preview) (current-buffer))
            (puthash (overlay-start preview) preview existing)))
        (dolist (parser (treesit-parser-list nil 'markdown-inline t))
          (dolist (node (treesit-query-capture
                         (treesit-parser-root-node parser)
                         '((latex_block) @math) nil nil t))
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
                  (when (and (not preview) (markdown-ts-appear-math--eligible-p beg end))
                    (setq preview (make-overlay beg end nil t nil))
                    (overlay-put preview 'category 'mathjax)
                    (overlay-put preview 'evaporate t)
                    (overlay-put preview 'markdown-ts-appear-math--source source)
                    (push preview markdown-ts-appear-math--objects)
                    (puthash beg preview existing)
                    (push (list preview
                                (buffer-substring-no-properties
                                 (treesit-node-end opening) (treesit-node-start closing))
                                (and (member (treesit-node-text opening t) '("$$" "\\[")) t))
                          requests))
                  (when (and preview (overlay-buffer preview))
                    (puthash preview t current)
                    (markdown-ts-appear-math--display preview)))))))
        (dolist (preview markdown-ts-appear-math--objects)
          (unless (gethash preview current)
            (markdown-ts-appear-math--delete preview)))
        ;; A synchronous renderer can reparse; finish reading all nodes first.
        (dolist (request (nreverse requests))
          (apply #'markdown-ts-appear-math--request request))))))

(defun markdown-ts-appear-math--setup ()
  "Install math preview hooks and render eligible formulas."
  (unless (and (require 'mathjax nil t) (fboundp 'mathjax-display)
               (mathjax-available-p) (image-type-available-p 'svg))
    (user-error "MathJax previews require the mathjax package, Node.js and SVG support"))
  (dolist (hook '(post-command-hook outline-view-change-hook))
    (add-hook hook #'markdown-ts-appear-math--refresh 90 t))
  (add-hook 'before-change-functions #'markdown-ts-appear-math--clear nil t)
  (markdown-ts-appear-math--refresh))

(defun markdown-ts-appear-math--teardown ()
  "Remove math preview hooks and dispose of pending and displayed results."
  (dolist (hook '(post-command-hook outline-view-change-hook))
    (remove-hook hook #'markdown-ts-appear-math--refresh t))
  (remove-hook 'before-change-functions #'markdown-ts-appear-math--clear t)
  (markdown-ts-appear-math--clear))

(defun markdown-ts-appear-unload-function ()
  "Remove global integration before unloading Markdown TS Appear."
  (dolist (buffer (buffer-list))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (markdown-ts-appear--buffer-teardown))))
  (markdown-ts-appear--set-advice nil)
  nil)

(markdown-ts-appear--install-advice)

(provide 'markdown-ts-appear)

;;; markdown-ts-appear.el ends here
