;;; markdown-ts-appear-test.el --- Tests for markdown-ts-appear -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael

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

;; Regression tests for Markdown TS Appear.

;;; Code:

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))

(require 'markdown-ts-appear)

(when (and (getenv "MARKDOWN_TS_APPEAR_REQUIRE_GRAMMARS")
           (not (treesit-ready-p '(markdown markdown-inline))))
  (error "Required Markdown Tree-sitter grammars are unavailable"))

(defvar markdown-ts-appear-test--decorations t
  "Whether buffer tests enable rendered decorations.")

(defun markdown-ts-appear-test--advice-installed-p ()
  "Return non-nil when all package advice is installed."
  (seq-every-p
   (lambda (binding)
     (advice-member-p (cdr binding) (car binding)))
   (markdown-ts-appear--advice-bindings)))

(defmacro markdown-ts-appear-test--with-buffer (content &rest body)
  "Create a Markdown buffer containing CONTENT and evaluate BODY."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (treesit-ready-p '(markdown markdown-inline)))
     (with-temp-buffer
       (insert ,content)
       (let ((treesit-font-lock-level 3)
             (markdown-ts-appear-link-icon
              (if markdown-ts-appear-test--decorations "[link]" ""))
             (markdown-ts-appear-image-icon
              (if markdown-ts-appear-test--decorations "[image]" ""))
             (markdown-ts-appear-wikilink-icon
              (if markdown-ts-appear-test--decorations "◆" ""))
             (markdown-ts-appear-code-fence-style
              (if markdown-ts-appear-test--decorations 'connected 'raw))
             (markdown-ts-appear-block-quote-marker
              (and markdown-ts-appear-test--decorations "▎"))
             (markdown-ts-appear-table-style
              (if markdown-ts-appear-test--decorations 'unicode 'raw))
             (markdown-ts-inline-images nil))
         (markdown-ts-mode)
         (unwind-protect
             (progn
               (markdown-ts-appear-mode 1)
               (font-lock-ensure)
               ,@body)
           (when markdown-ts-appear-mode
             (markdown-ts-appear-mode -1)))))))

(defun markdown-ts-appear-test--ancestor-type-p (position language type)
  "Return non-nil when TYPE contains POSITION in LANGUAGE's syntax tree."
  (let ((node (treesit-node-at position language)) found)
    (while (and node (not found))
      (when (equal (treesit-node-type node) type)
        (setq found t))
      (setq node (treesit-node-parent node)))
    found))

(defun markdown-ts-appear-test--rendered-source-p (beg end)
  "Return non-nil when source from BEG to END carries rendering properties."
  (or (text-property-not-all beg end 'invisible nil)
      (text-property-not-all beg end 'display nil)
      (text-property-any beg end 'line-height 0)))

(ert-deftest markdown-ts-appear-test-visual-decorations-are-opt-in ()
  (dolist (variable '(markdown-ts-appear-link-icon
                      markdown-ts-appear-image-icon
                      markdown-ts-appear-wikilink-icon))
    (should (equal (default-value variable) "")))
  (dolist (variable '(markdown-ts-appear-block-quote-marker
                      markdown-ts-appear-render-callouts))
    (should-not (default-value variable)))
  (should (eq (default-value 'markdown-ts-appear-code-fence-style) 'raw))
  (should (eq (default-value 'markdown-ts-appear-table-style) 'raw)))

(ert-deftest markdown-ts-appear-test-code-fence-face-inherits-block-background ()
  (should
   (memq 'markdown-ts-code-block
         (face-attribute 'markdown-ts-appear-code-fence-marker
                         :inherit nil))))

(ert-deftest markdown-ts-appear-test-renders-inverted-language-label ()
  (markdown-ts-appear-test--with-buffer "```c\nint x;\n```\n"
    (let ((display (get-text-property (point-min) 'display)))
      (should (equal display "╭─ c "))
      (dolist (position '(2 3 4))
        (should
         (equal (get-text-property position 'face display)
                '(markdown-ts-appear-label
                  markdown-ts-appear-code-fence-marker)))))))

(ert-deftest markdown-ts-appear-test-renders-callout-label ()
  (let ((markdown-ts-appear-render-callouts t))
    (markdown-ts-appear-test--with-buffer
        "> [!warning]- `fatal: The remote end hung up unexpectedly`\n"
      (goto-char (point-min))
      (search-forward "[!warning]-")
      (let ((beg (match-beginning 0)))
        (should-not (button-at (+ beg 2)))
        (should-not (get-text-property (+ beg 2) 'help-echo))
        (should-not
         (seq-find
          (lambda (overlay)
            (overlay-get overlay 'markdown-ts-appear--link-icon))
          (overlays-in beg (match-end 0))))
        (let ((display (get-text-property beg 'display)))
          (should (equal display " warning "))
          (dotimes (offset (length display))
            (should
             (equal (get-text-property offset 'face display)
                    '(markdown-ts-appear-label warning)))))
        (should-not (get-text-property (1- (match-end 0)) 'display))
        (goto-char (1- (match-end 0)))
        (markdown-ts-appear--update)
        (should-not (get-text-property beg 'display))
        (should-not (button-at (+ beg 2)))
        (goto-char (point-max))
        (markdown-ts-appear--update)
        (should (equal (get-text-property beg 'display) " warning "))))))

(ert-deftest markdown-ts-appear-test-callout-colors-follow-type ()
  (let ((markdown-ts-appear-render-callouts t))
    (dolist (case '(("NOTE" . link)
                    ("tip" . success)
                    ("Important" . font-lock-keyword-face)
                    ("WARNING" . warning)
                    ("caution" . error)
                    ("CUSTOM" . markdown-ts-appear-block-quote-marker)))
      (markdown-ts-appear-test--with-buffer
          (format "> [!%s] body\n" (car case))
        (goto-char (point-min))
        (search-forward "[!")
        (let ((beg (match-beginning 0)))
          (should (equal (get-text-property beg 'display)
                         (concat " " (car case) " ")))
          (should (equal (get-text-property
                          1 'face (get-text-property beg 'display))
                         (list 'markdown-ts-appear-label (cdr case))))
          (should-not (button-at beg))
          (markdown-ts-appear--update)
          (should-not (get-text-property beg 'display))
          (goto-char (point-max))
          (markdown-ts-appear--update)
          (should (equal (get-text-property
                          1 'face (get-text-property beg 'display))
                         (list 'markdown-ts-appear-label (cdr case)))))))))

(ert-deftest markdown-ts-appear-test-reveal-and-restore ()
  (markdown-ts-appear-test--with-buffer "**bold** plain\n"
    (goto-char 3)
    (markdown-ts-appear--update)
    (should-not (get-text-property (point-min) 'invisible))
    (goto-char 12)
    (markdown-ts-appear--update)
    (should (get-text-property (point-min) 'invisible))))

(ert-deftest markdown-ts-appear-test-initial-update-creates-inline-ranges ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (dolist (case '(("&amp;\n" 3 "&amp;")
                  ("line  \nbreak\n" 5 "  \n")))
    (pcase-let ((`(,content ,position ,expected) case))
      (with-temp-buffer
        (insert content)
        (markdown-ts-mode)
        (goto-char position)
        (unwind-protect
            (progn
              (markdown-ts-appear-mode 1)
              (should markdown-ts-appear--region)
              (should
               (equal
                (buffer-substring-no-properties
                 (marker-position (car markdown-ts-appear--region))
                 (marker-position (cdr markdown-ts-appear--region)))
                expected)))
          (when markdown-ts-appear-mode
            (markdown-ts-appear-mode -1)))))))

(ert-deftest markdown-ts-appear-test-update-cache ()
  (markdown-ts-appear-test--with-buffer "**bold**\n"
    (goto-char 3)
    (setq markdown-ts-appear--last-point nil)
    (setq markdown-ts-appear--last-tick nil)
    (let ((calls 0)
	  (original (symbol-function 'markdown-ts-appear--bounds)))
      (cl-letf (((symbol-function 'markdown-ts-appear--bounds)
		 (lambda ()
		   (setq calls (1+ calls))
		   (funcall original))))
	(markdown-ts-appear--update)
	(markdown-ts-appear--update)
	(should (= calls 1))
	(forward-char)
	(markdown-ts-appear--update)
	(should (= calls 2))
	(insert "x")
	(markdown-ts-appear--update)
	(should (= calls 3))))))

(ert-deftest markdown-ts-appear-test-prefers-smallest-nested-element ()
  (markdown-ts-appear-test--with-buffer "[**bold**](url)\n"
    (goto-char (point-min))
    (markdown-ts-appear--update)
    (should (equal (buffer-substring-no-properties
		    (marker-position (car markdown-ts-appear--region))
		    (marker-position (cdr markdown-ts-appear--region)))
		   "[**bold**](url)"))
    (goto-char 4)
    (markdown-ts-appear--update)
    (should (equal (buffer-substring-no-properties
		    (marker-position (car markdown-ts-appear--region))
		    (marker-position (cdr markdown-ts-appear--region)))
		   "**bold**"))))

(ert-deftest markdown-ts-appear-test-does-not-reveal-adjacent-text ()
  (dolist (case '(("**bold**plain\n" "plain")
                  ("[link](url)plain\n" "plain")))
    (pcase-let ((`(,content ,location) case))
      (markdown-ts-appear-test--with-buffer content
	(goto-char (point-min))
	(search-forward location)
	(backward-char (length location))
	(should-not (markdown-ts-appear--bounds))
	(markdown-ts-appear--update)
	(should (get-text-property (point-min) 'invisible))))))

(ert-deftest markdown-ts-appear-test-inline-node-bounds-matrix ()
  (dolist (case
           '(("*em*\n" "em" "*em*" "emphasis")
             ("~~gone~~\n" "gone" "~~gone~~" "strikethrough")
             ("`code`\n" "code" "`code`" "code_span")
             ("[text](url)\n" "url" "[text](url)" "inline_link")
             ("[text][label]\n\n[label]: url\n" "text" "[text][label]"
              "full_reference_link")
             ("[text][]\n\n[text]: url\n" "text" "[text][]"
              "collapsed_reference_link")
             ("[text]\n\n[text]: url\n" "text" "[text]" "shortcut_link")
             ("![alt](img.png)\n" "img.png" "![alt](img.png)" "image")
             ("<https://example.com>\n" "https" "<https://example.com>"
              "uri_autolink")
             ("<a@example.com>\n" "example" "<a@example.com>"
              "email_autolink")
             ("&amp;\n" "amp" "&amp;" "entity_reference")
             ("&#169;\n" "169" "&#169;" "numeric_character_reference")
             ("\\*literal*\n" "\\*" "\\*" "backslash_escape")
             ("line  \nbreak\n" 5 "  \n" "hard_line_break")
             ("$x$\n" "x" "$x$" "latex_block")))
    (pcase-let ((`(,content ,location ,expected ,node-type) case))
      (markdown-ts-appear-test--with-buffer content
	(goto-char (point-min))
	(if (stringp location)
	    (progn
	      (search-forward location)
	      (backward-char (length location)))
	  (goto-char location))
	(should (markdown-ts-appear-test--ancestor-type-p
		 (point) 'markdown-inline node-type))
	(let ((bounds (markdown-ts-appear--bounds)))
	  (should bounds)
	  (should (equal (buffer-substring-no-properties
			  (car bounds) (cdr bounds))
			 expected)))))))

(ert-deftest markdown-ts-appear-test-structural-node-bounds-matrix ()
  (dolist (case
           '(("# Heading\n" 1 "# " "atx_heading")
             ("Title\n=====\n" 7 "=====" "setext_heading")
             ("- item\n" 1 "- " "list_item")
             ("- [ ] todo\n" 3 "[ ]" "task_list_marker_unchecked")
             ("- [x] done\n" 3 "[x]" "task_list_marker_checked")
             ("| A | B |\n|---|---|\n| 1 | 2 |\n" "A" "| A | B |"
              "pipe_table_header")
             ("| A | B |\n|---|---|\n| 1 | 2 |\n" "---" "|---|---|"
              "pipe_table_delimiter_row")
             ("| A | B |\n|---|---|\n| 1 | 2 |\n" "1" "| 1 | 2 |"
              "pipe_table_row")
             ("---\n" 2 "---\n" "thematic_break")
             ("[label]: https://example.com\n" "label"
              "[label]: https://example.com\n"
              "link_reference_definition")))
    (pcase-let ((`(,content ,location ,expected ,node-type) case))
      (markdown-ts-appear-test--with-buffer content
	(goto-char (point-min))
	(if (stringp location)
	    (progn
	      (search-forward location)
	      (backward-char (length location)))
	  (goto-char location))
	(should (markdown-ts-appear-test--ancestor-type-p
		 (point) 'markdown node-type))
	(let ((bounds (markdown-ts-appear--bounds)))
	  (should bounds)
	  (should (equal (buffer-substring-no-properties
			  (car bounds) (cdr bounds))
			 expected)))))))

(ert-deftest markdown-ts-appear-test-private-fontifier-behavior-matrix ()
  (dolist (case
           '(("# Heading\n" "Heading" "# ")
             ("Title\n=====\n" "Title" "=====")
             ("- item\n" "item" "- ")
             ("- [ ] todo\n" "[ ]" "[ ]")
             ("<https://example.com>\n" "https" "<https://example.com>")
             ("&amp;\n" "amp" "&amp;")
             ("\\*literal*\n" "\\*" "\\*")
             ("line  \nbreak\n" "  \n" "  \n")
             ("---\n" "---" "---\n")
             ("$x$\n" "x" "$x$")
             ("$$x$$\n" "x" "$$x$$")))
    (pcase-let ((`(,content ,location ,source) case))
      (markdown-ts-appear-test--with-buffer content
	(goto-char (point-min))
	(search-forward source)
	(let ((source-beg (- (point) (length source)))
	      (source-end (point)))
	  (should
	   (markdown-ts-appear-test--rendered-source-p source-beg source-end))
	  (goto-char (point-min))
	  (search-forward location)
	  (backward-char (length location))
	  (markdown-ts-appear--update)
	  (should-not
	   (markdown-ts-appear-test--rendered-source-p source-beg source-end))
	  (goto-char (point-max))
	  (markdown-ts-appear--update)
	  (should
	   (markdown-ts-appear-test--rendered-source-p
	    source-beg source-end)))))))

(ert-deftest markdown-ts-appear-test-wikilink-boundaries ()
  (markdown-ts-appear-test--with-buffer "[[Markdown|an alias]]\n"
    (goto-char (point-min))
    (should (equal (markdown-ts-appear--bounds)
		   (cons (point-min) (1- (point-max)))))
    (goto-char (- (point-max) 2))
    (should (equal (markdown-ts-appear--bounds)
		   (cons (point-min) (1- (point-max)))))))

(ert-deftest markdown-ts-appear-test-wikilink-respects-syntax-context ()
  (markdown-ts-appear-test--with-buffer "`[[code]]` \\[[link]]\n"
    (goto-char (point-min))
    (search-forward "code")
    (let ((bounds (markdown-ts-appear--bounds)))
      (should (equal (buffer-substring-no-properties
		      (car bounds) (cdr bounds))
		     "`[[code]]`")))
    (search-forward "link")
    (let ((bounds (markdown-ts-appear--bounds)))
      (should (equal (buffer-substring-no-properties
		      (car bounds) (cdr bounds))
		     "[link]")))))

(ert-deftest markdown-ts-appear-test-escaped-link-is-not-a-wikilink ()
  (markdown-ts-appear-test--with-buffer "\\[[link]]\n"
    (should-not (get-text-property 2 'invisible))
    (let ((icon
	   (seq-find
	    (lambda (overlay)
	      (overlay-get overlay 'markdown-ts-appear--link-icon))
	    (overlays-in (point-min) (point-max)))))
      (should icon)
      (should-not (equal (overlay-get icon 'before-string) "◆ ")))))

(ert-deftest markdown-ts-appear-test-image-is-not-a-wikilink ()
  (let ((markdown-ts-appear-test--decorations nil))
    (markdown-ts-appear-test--with-buffer "![[x]]\n"
      (goto-char 4)
      (should (equal (markdown-ts-appear--bounds)
		     (cons (point-min) (1- (point-max)))))
      (should-not
       (seq-some
	(lambda (overlay)
	  (equal (overlay-get overlay 'before-string) "◆ "))
	(overlays-in (point-min) (point-max)))))))

(ert-deftest markdown-ts-appear-test-skips-code-block ()
  (markdown-ts-appear-test--with-buffer
      "```elisp\n(message \"hi\")\n```\n"
    (goto-char (point-min))
    (search-forward "message")
    (should-not (markdown-ts-appear--bounds))))

(ert-deftest markdown-ts-appear-test-skips-literal-block-fallback ()
  (dolist (case '(("    *literal*\n" "literal")
                  ("<div>\n*literal*\n</div>\n" "literal")))
    (markdown-ts-appear-test--with-buffer (car case)
      (goto-char (point-min))
      (search-forward (cadr case))
      (should-not (markdown-ts-appear--bounds)))))

(ert-deftest markdown-ts-appear-test-restores-hide-markup-setting ()
  (markdown-ts-appear-test--with-buffer "**bold**\n"
    (should markdown-ts-hide-markup)
    (markdown-ts-appear-mode 1)
    (markdown-ts-appear-mode -1)
    (should-not markdown-ts-hide-markup)
    (should-not
     (local-variable-p 'markdown-ts-hide-markup))
    (markdown-ts-appear-mode -1)
    (should-not markdown-ts-hide-markup)))

(ert-deftest markdown-ts-appear-test-redundant-disable-preserves-markup ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (with-temp-buffer
    (insert "**bold**\n")
    (markdown-ts-mode)
    (setq markdown-ts-hide-markup t)
    (markdown-ts--set-hide-markup t)
    (markdown-ts-appear-mode -1)
    (should markdown-ts-hide-markup)))

(ert-deftest markdown-ts-appear-test-disable-follows-current-markup-default ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (let ((original (default-value 'markdown-ts-hide-markup)))
    (unwind-protect
        (dolist (default '(nil t))
          (set-default 'markdown-ts-hide-markup (not default))
          (with-temp-buffer
            (insert "**bold**\n")
            (markdown-ts-mode)
            (setq-local markdown-ts-hide-markup (not default))
            (markdown-ts--set-hide-markup markdown-ts-hide-markup)
            (markdown-ts-appear-mode 1)
            (set-default 'markdown-ts-hide-markup default)
            (markdown-ts-appear-mode -1)
            (should (eq markdown-ts-hide-markup default))
            (should-not (local-variable-p 'markdown-ts-hide-markup))
            (should (eq (not (null (memq 'markdown-ts--markup
                                         buffer-invisibility-spec)))
                        default))
            (set-default 'markdown-ts-hide-markup (not default))
            (should (eq markdown-ts-hide-markup (not default)))))
      (set-default 'markdown-ts-hide-markup original))))

(ert-deftest markdown-ts-appear-test-rejects-indirect-buffer ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (with-temp-buffer
    (insert "**bold**\n")
    (markdown-ts-mode)
    (let ((clone (clone-indirect-buffer " *markdown-reveal-clone*" nil)))
      (unwind-protect
          (with-current-buffer clone
            (should-error (markdown-ts-appear-mode 1) :type 'user-error)
            (should-not markdown-ts-appear-mode)
            (should-not (markdown-ts-appear--active-p)))
        (kill-buffer clone)))))

(ert-deftest markdown-ts-appear-test-public-tracking-controls ()
  (markdown-ts-appear-test--with-buffer "A *word* here\n"
    (goto-char 5)
    (markdown-ts-appear--update)
    (should markdown-ts-appear--region)
    (markdown-ts-appear-stop)
    (should-not (memq #'markdown-ts-appear--update post-command-hook))
    (should-not markdown-ts-appear--region)
    (markdown-ts-appear-start)
    (should (memq #'markdown-ts-appear--update post-command-hook))
    (should markdown-ts-appear--region)))

(ert-deftest markdown-ts-appear-test-enable-is-idempotent ()
  (dolist (markdown-ts-appear-test--decorations '(nil t))
    (markdown-ts-appear-test--with-buffer "> quote\n\n**bold** plain\n"
      (let ((settings (copy-sequence treesit-font-lock-settings))
            (filter filter-buffer-substring-function)
            (properties (copy-sequence font-lock-extra-managed-props)))
        (dotimes (_ 3)
          (markdown-ts-appear-mode 1))
        (should (equal settings treesit-font-lock-settings))
        (should (eq filter filter-buffer-substring-function))
        (should (equal properties font-lock-extra-managed-props))
        (should (= 1 (cl-count #'markdown-ts-appear--after-change
                               after-change-functions)))
        (should (= 1 (cl-count #'markdown-ts-appear--update post-command-hook)))
        (markdown-ts-appear-stop)
        (markdown-ts-appear-mode 1)
        (should-not (memq #'markdown-ts-appear--update post-command-hook))
        (should (equal settings treesit-font-lock-settings))
        (markdown-ts-appear-start)
        (goto-char (point-min))
        (search-forward "bold")
        (markdown-ts-appear--update)
        (should markdown-ts-appear--region)
        (markdown-ts-appear-mode -1)
        (should-not markdown-ts-appear--block-font-lock-settings)
        (should-not (advice-function-member-p
                     #'markdown-ts-appear--decoration-filter-copied-text
                     filter-buffer-substring-function))))))

(ert-deftest markdown-ts-appear-test-restores-setext-line-height ()
  (markdown-ts-appear-test--with-buffer "Title\n=====\n"
    (should (text-property-any (point-min) (point-max) 'line-height 0))
    (markdown-ts-appear-mode -1)
    (font-lock-ensure)
    (should-not (text-property-any
		 (point-min) (point-max) 'line-height 0))))

(ert-deftest markdown-ts-appear-test-disable-widens-before-cleanup ()
  (markdown-ts-appear-test--with-buffer "One\n===\n\nTwo\n===\n"
    (goto-char (point-min))
    (search-forward "===")
    (search-forward "===")
    (let ((second-underline (- (point) 3)))
      (should (eq (get-text-property second-underline 'line-height) 0))
      (narrow-to-region (point-min) 8)
      (markdown-ts-appear-mode -1)
      (widen)
      (should-not (get-text-property second-underline 'line-height)))))

(ert-deftest markdown-ts-appear-test-rejects-other-major-modes-cleanly ()
  (with-temp-buffer
    (should-error (markdown-ts-appear-mode 1) :type 'user-error)
    (should-not markdown-ts-appear-mode)
    (should-not (memq 'markdown-ts-appear-mode local-minor-modes))))

(ert-deftest markdown-ts-appear-test-adds-link-icon ()
  (markdown-ts-appear-test--with-buffer "[Emacs](https://www.gnu.org/)\n"
    (let ((overlay
	   (seq-find
	    (lambda (candidate)
	      (overlay-get candidate 'markdown-ts-appear--link-icon))
	    (overlays-in (point-min) (point-max)))))
      (should overlay)
      (should (stringp (overlay-get overlay 'before-string))))))

(ert-deftest markdown-ts-appear-test-reveals-link-destination ()
  (markdown-ts-appear-test--with-buffer
      "[Emacs](https://www.gnu.org/) plain\n"
    (goto-char (point-min))
    (search-forward "https://www.gnu.org/")
    (let ((url-beg (- (point) (length "https://www.gnu.org/")))
          (url-end (point)))
      (markdown-ts-appear--update)
      (should-not (text-property-not-all url-beg url-end 'invisible nil)))))

(ert-deftest markdown-ts-appear-test-wikilink-alias-target ()
  (markdown-ts-appear-test--with-buffer "[[Markdown|an alias]]\n"
    (goto-char (point-min))
    (search-forward "alias")
    (let ((button (button-at (1- (point))))
	  opened)
      (should button)
      (should (equal (button-get button 'help-echo) "Markdown"))
      (cl-letf (((symbol-function 'find-file)
		 (lambda (file &rest _arguments)
		   (setq opened file))))
	(button-activate button))
      (should (equal opened "Markdown")))))

(ert-deftest markdown-ts-appear-test-adds-one-full-reference-icon ()
  (markdown-ts-appear-test--with-buffer
      "[text][label]\n\n[label]: https://example.com\n"
    (should
     (= 1
        (length
         (seq-filter
          (lambda (overlay)
            (overlay-get overlay 'markdown-ts-appear--link-icon))
          (overlays-in (point-min) (point-max))))))))

(ert-deftest markdown-ts-appear-test-hides-and-reveals-full-reference-label ()
  (markdown-ts-appear-test--with-buffer
      "[full reference][project]\n\n[project]: https://example.com\nplain\n"
    (goto-char (point-min))
    (search-forward "[project]")
    (let ((label-beg (- (point) (length "[project]")))
          (label-end (point)))
      (should (text-property-not-all label-beg label-end 'invisible nil))
      (goto-char (point-min))
      (search-forward "full")
      (markdown-ts-appear--update)
      (should-not
       (text-property-not-all label-beg label-end 'invisible nil))
      (goto-char (point-max))
      (markdown-ts-appear--update)
      (should (text-property-not-all label-beg label-end 'invisible nil)))))

(ert-deftest markdown-ts-appear-test-removes-stale-overlay-after-edit ()
  (markdown-ts-appear-test--with-buffer "[Emacs](https://www.gnu.org/)\n"
    (should
     (seq-some
      (lambda (overlay)
	(overlay-get overlay 'markdown-ts-appear--link-icon))
      (overlays-in (point-min) (point-max))))
    (goto-char (point-min))
    (delete-char 1)
    (should-not
     (seq-some
      (lambda (overlay)
	(overlay-get overlay 'markdown-ts-appear--link-icon))
      (overlays-in (point-min) (point-max))))))

(ert-deftest markdown-ts-appear-test-recreates-overlay-after-valid-edit ()
  (markdown-ts-appear-test--with-buffer "[Emacs](https://www.gnu.org/)\n"
    (goto-char (point-min))
    (search-forward "Emacs")
    (insert " GNU")
    (font-lock-ensure)
    (should
     (seq-some
      (lambda (overlay)
	(overlay-get overlay 'markdown-ts-appear--link-icon))
      (overlays-in (point-min) (point-max))))))

(ert-deftest markdown-ts-appear-test-removes-overlay-after-structural-edit ()
  (markdown-ts-appear-test--with-buffer
      "intro\n\n[Emacs](https://www.gnu.org/)\n"
    (should
     (seq-some
      (lambda (overlay)
        (overlay-get overlay 'markdown-ts-appear--link-icon))
      (overlays-in (point-min) (point-max))))
    (goto-char (point-min))
    (insert "```\n")
    (font-lock-ensure)
    (should-not
     (seq-some
      (lambda (overlay)
        (overlay-get overlay 'markdown-ts-appear--link-icon))
      (overlays-in (point-min) (point-max))))))

(ert-deftest markdown-ts-appear-test-removes-overlay-after-inline-reparse ()
  (markdown-ts-appear-test--with-buffer
      "plain\n[Emacs](https://www.gnu.org/)`"
    (should
     (seq-some
      (lambda (overlay)
        (overlay-get overlay 'markdown-ts-appear--link-icon))
      (overlays-in (point-min) (point-max))))
    (goto-char (point-min))
    (insert "`")
    (font-lock-ensure)
    (should (markdown-ts-appear-test--ancestor-type-p
             (1+ (line-end-position)) 'markdown-inline "code_span"))
    (should-not
     (seq-some
      (lambda (overlay)
        (overlay-get overlay 'markdown-ts-appear--link-icon))
      (overlays-in (point-min) (point-max))))))

(ert-deftest markdown-ts-appear-test-edit-preserves-fontification ()
  (markdown-ts-appear-test--with-buffer "**bold**plain\n"
    (should (get-text-property (point-min) 'invisible))
    (goto-char (point-max))
    (insert "more\n")
    (font-lock-ensure)
    (should (get-text-property (point-min) 'invisible))))

(ert-deftest markdown-ts-appear-test-structural-edit-after-empty-enable ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (with-temp-buffer
    (let ((markdown-ts-appear-link-icon "[link]")
          (markdown-ts-inline-images nil))
      (markdown-ts-mode)
      (unwind-protect
          (progn
            (markdown-ts-appear-mode 1)
            (insert "plain\n[Emacs](https://www.gnu.org/)`")
            (font-lock-ensure)
            (should
             (seq-some
              (lambda (overlay)
                (overlay-get overlay 'markdown-ts-appear--link-icon))
              (overlays-in (point-min) (point-max))))
            (goto-char (point-min))
            (insert "`")
            (font-lock-ensure)
            (should-not
             (seq-some
              (lambda (overlay)
                (overlay-get overlay 'markdown-ts-appear--link-icon))
              (overlays-in (point-min) (point-max)))))
        (when markdown-ts-appear-mode
          (markdown-ts-appear-mode -1))))))

(ert-deftest markdown-ts-appear-test-cleans-up-before-major-mode-change ()
  (markdown-ts-appear-test--with-buffer
      "[Emacs](https://www.gnu.org/)\n\n> quote\n\n```c\nint x;\n```\n\nTitle\n===\n"
    (should (markdown-ts-appear--active-p))
    (text-mode)
    (should-not markdown-ts-appear-mode)
    (should-not (markdown-ts-appear--active-p))
    (should-not (memq #'markdown-ts-appear--after-change after-change-functions))
    (should-not (memq #'markdown-ts-appear--update post-command-hook))
    (should-not markdown-ts-appear--block-font-lock-settings)
    (should-not (advice-function-member-p
                 #'markdown-ts-appear--decoration-filter-copied-text
                 filter-buffer-substring-function))
    (dolist (property '(display line-height line-prefix wrap-prefix
                                markdown-ts-appear--decoration))
      (should-not (text-property-not-all (point-min) (point-max) property nil)))
    (should-not
     (seq-some
      (lambda (overlay)
        (or (overlay-get overlay 'markdown-ts-appear--image-label)
            (overlay-get overlay 'markdown-ts-appear--link-icon)))
      (overlays-in (point-min) (point-max))))))

(ert-deftest markdown-ts-appear-test-rebuilds-distant-icons-after-narrowed-edit ()
  (markdown-ts-appear-test--with-buffer
      "intro\n\n[Emacs](https://www.gnu.org/)\n\n![](demo.png)\n\n[[target|alias]]\n"
    (dotimes (iteration 3)
      (let ((icons (seq-filter
                    (lambda (overlay)
                      (or (overlay-get overlay 'markdown-ts-appear--link-icon)
                          (overlay-get overlay 'markdown-ts-appear--image-label)))
                    (overlays-in (point-min) (point-max)))))
        (should (= (length icons) 3))
        (save-restriction
          (narrow-to-region (point-min) 7)
          (goto-char (point-min))
          (insert (if (= iteration 0) "```\n" "plain\n")))
        (dolist (icon icons)
          (should-not (overlay-buffer icon)))
        (font-lock-ensure)
        (when (= iteration 0)
          (should-not
           (seq-some
            (lambda (overlay)
              (or (overlay-get overlay 'markdown-ts-appear--link-icon)
                  (overlay-get overlay 'markdown-ts-appear--image-label)))
            (overlays-in (point-min) (point-max))))
          (delete-region (point-min) (+ (point-min) 4))
          (font-lock-ensure))))))

(ert-deftest markdown-ts-appear-test-detaches-indirect-clone ()
  (markdown-ts-appear-test--with-buffer "**bold** plain\n"
    (goto-char 3)
    (markdown-ts-appear--update)
    (let ((region markdown-ts-appear--region)
          (filter filter-buffer-substring-function)
          (settings (copy-tree treesit-font-lock-settings t))
          (text (buffer-substring (point-min) (point-max)))
          (clone (clone-indirect-buffer " *markdown-reveal-clone*" nil)))
      (unwind-protect
          (progn
            (with-current-buffer clone
              (should-not markdown-ts-appear-mode)
              (should-not (markdown-ts-appear--active-p))
              (should-not markdown-ts-appear--region)
              (should-not
               (memq #'markdown-ts-appear--update post-command-hook))
              (should-not
               (memq #'markdown-ts-appear--after-change
                     after-change-functions))
              (should-not (advice-function-member-p
                           #'markdown-ts-appear--decoration-filter-copied-text
                           filter-buffer-substring-function))
              (markdown-ts-appear-mode -1))
            (should markdown-ts-appear-mode)
            (should (markdown-ts-appear--active-p))
            (should (eq filter filter-buffer-substring-function))
            (should (advice-function-member-p
                     #'markdown-ts-appear--decoration-filter-copied-text filter))
            (should (equal settings treesit-font-lock-settings))
            (should (equal-including-properties
                     text (buffer-substring (point-min) (point-max))))
            (should (marker-position (car region)))
            (should (marker-position (cdr region))))
        (kill-buffer clone))
      (should (marker-position (car region)))
      (should (memq #'markdown-ts-appear--after-change after-change-functions))
      (goto-char (point-max))
      (markdown-ts-appear--update)
      (should (get-text-property (point-min) 'invisible)))))

(ert-deftest markdown-ts-appear-test-kill-detaches-markers-without-fontifying ()
  (markdown-ts-appear-test--with-buffer "**bold** plain\n"
    (goto-char 3)
    (markdown-ts-appear--update)
    (let ((region markdown-ts-appear--region))
      (cl-letf (((symbol-function 'font-lock-ensure)
                 (lambda (&rest _) (ert-fail "Fontified a dying buffer"))))
        (kill-buffer (current-buffer)))
      (should-not (marker-buffer (car region)))
      (should-not (marker-buffer (cdr region))))))

(ert-deftest markdown-ts-appear-test-teardown-survives-deleted-parser ()
  (markdown-ts-appear-test--with-buffer
      "[Emacs](https://www.gnu.org/)\n"
    (cl-letf (((symbol-function 'font-lock-ensure)
               (lambda (&rest _arguments)
                 (signal 'treesit-parser-deleted nil))))
      (markdown-ts-appear-mode -1))
    (should-not (markdown-ts-appear--active-p))))

(ert-deftest markdown-ts-appear-test-shows-empty-image-label ()
  (markdown-ts-appear-test--with-buffer "![](images/demo.gif)\n"
    (goto-char (point-min))
    (let* ((beg (search-forward "images/demo.gif"))
	   (destination-beg (- beg (length "images/demo.gif"))))
      (should-not
       (text-property-not-all destination-beg beg 'invisible nil))
      (should (equal (get-text-property destination-beg 'display)
		     "demo.gif")))
    (let ((overlay
	   (seq-find
	    (lambda (candidate)
	      (overlay-get candidate 'markdown-ts-appear--image-label))
	    (overlays-in (point-min) (point-max)))))
      (should overlay)
      (should (stringp (overlay-get overlay 'before-string)))
      (should (equal (overlay-get overlay 'help-echo) "images/demo.gif")))))

(ert-deftest markdown-ts-appear-test-renders-and-reveals-block-quote ()
  (markdown-ts-appear-test--with-buffer "> quote\n> continued\n"
    (goto-char (point-min))
    (should (equal (get-text-property 1 'display) "▎"))
    (should (equal (get-text-property 9 'display) "▎"))
    (markdown-ts-appear--update)
    (should-not (get-text-property 1 'display))
    (goto-char (point-max))
    (markdown-ts-appear--update)
    (should (equal (get-text-property 1 'display) "▎"))))

(ert-deftest markdown-ts-appear-test-reuses-native-faces ()
  (markdown-ts-appear-test--with-buffer "> quote\n\n| A | B |\n|---|---|\n"
    (goto-char (point-min))
    (search-forward "quote")
    (should (memq 'markdown-ts-block-quote
                  (get-text-property (1- (point)) 'face)))
    (search-forward "|---")
    (should (eq 'markdown-ts-table-delimiter-cell
                (get-text-property
                 0 'face (get-text-property (match-beginning 0) 'display))))))

(ert-deftest markdown-ts-appear-test-reveals-quote-marker-inside-list ()
  (dolist (case '(("- > quoted\n" 3)
                  ("> - > inner\n" 5)))
    (pcase-let ((`(,content ,marker-beg) case))
      (markdown-ts-appear-test--with-buffer content
        (should (equal (get-text-property marker-beg 'display) "▎"))
        (goto-char marker-beg)
        (markdown-ts-appear--update)
        (should-not (get-text-property marker-beg 'display))
        (should-not (get-char-property marker-beg 'invisible))
        (when (eq (char-after (point-min)) ?>)
          (should (equal (get-text-property (point-min) 'display) "▎")))
        (goto-char (point-max))
        (markdown-ts-appear--update)
        (should (equal (get-text-property marker-beg 'display) "▎"))))))

(ert-deftest markdown-ts-appear-test-renders-nested-quote-markers ()
  (markdown-ts-appear-test--with-buffer "> outer\n> > nested\n> outer again\n"
    (should (equal (get-text-property 1 'display) "▎"))
    (should (equal (get-text-property 9 'display) "▎"))
    (should (equal (get-text-property 11 'display) "▎"))))

(ert-deftest markdown-ts-appear-test-reveals-compact-quote-marker-boundary ()
  (markdown-ts-appear-test--with-buffer ">quote\n"
    (goto-char 2)
    (markdown-ts-appear--update)
    (should-not
     (get-text-property 1 'display))))

(ert-deftest markdown-ts-appear-test-renders-and-reveals-code-fences ()
  (dolist (content '("```emacs-lisp\n(message \"hi\")\n```\n"
		     "~~~ emacs-lisp\n(message \"hi\")\n~~~\n"))
    (markdown-ts-appear-test--with-buffer content
      (goto-char (point-min))
      (should (equal (get-text-property 1 'display)
		     "╭─ emacs-lisp "))
      (search-forward "emacs-lisp")
      (should (equal (get-text-property (1- (point)) 'display) ""))
      (forward-line 1)
      (should (equal (get-text-property (point) 'line-prefix) "│ "))
      (should (equal (get-text-property (point) 'wrap-prefix) "│ "))
      (goto-char (point-min))
      (markdown-ts-appear--update)
      (should-not (get-text-property 1 'display))
      (goto-char (point-max))
      (markdown-ts-appear--update)
      (goto-char (point-min))
      (forward-line 1)
      (search-forward
       (if (eq (char-after (point-min)) ?`) "```" "~~~"))
      (let ((closing-beg (match-beginning 0)))
	(should (equal (get-text-property
			closing-beg 'display)
		       "╰─"))
	(markdown-ts-appear--update)
	(should-not
	 (get-text-property closing-beg 'display)))
      (goto-char (point-min))
      (markdown-ts-appear--update)
      (should-not (get-text-property 1 'display))
      (goto-char (point-max))
      (markdown-ts-appear--update)
      (should (equal (get-text-property 1 'display)
		     "╭─ emacs-lisp ")))))

(ert-deftest markdown-ts-appear-test-code-prefix-respects-region ()
  (markdown-ts-appear-test--with-buffer
      "```text\nfirst\nsecond\n```\n"
    (let* ((block
            (markdown-ts-appear--node-ancestor
             (treesit-node-at (point-min) 'markdown) "fenced_code_block"))
           (content
            (markdown-ts-appear--first-direct-child-of-type
             block "code_fence_content"))
           (first-line (treesit-node-start content))
           (second-line
            (save-excursion
              (goto-char first-line)
              (line-beginning-position 2))))
      (remove-text-properties
       first-line (treesit-node-end content)
       '(line-prefix nil wrap-prefix nil markdown-ts-appear--decoration nil))
      (markdown-ts-appear--fontify-code-block
       block 'append second-line (treesit-node-end content))
      (should-not (get-text-property first-line 'line-prefix))
      (should (equal (get-text-property second-line 'line-prefix) "│ ")))))

(ert-deftest markdown-ts-appear-test-renders-code-fence-without-language ()
  (markdown-ts-appear-test--with-buffer "```\ncontent\n```\n"
    (should (equal (get-text-property (point-min) 'display) "╭─"))))

(ert-deftest markdown-ts-appear-test-removes-stale-code-prefix-after-edit ()
  (markdown-ts-appear-test--with-buffer "```text\ncontent\n```\n"
    (goto-char (point-min))
    (forward-line 1)
    (let ((content-beg (point)))
      (should (get-text-property content-beg 'line-prefix))
      (goto-char (point-min))
      (delete-char 1)
      (font-lock-ensure)
      (should-not (get-text-property (1- content-beg) 'line-prefix)))))

(ert-deftest markdown-ts-appear-test-renders-code-inside-quote ()
  (markdown-ts-appear-test--with-buffer
      "> ```text\n> git config http.postBuffer 524288000\n> ```\n"
    (let ((body-quote
           (save-excursion
             (goto-char (point-min))
             (forward-line 1)
             (point)))
          (body-content
           (save-excursion
             (goto-char (point-min))
             (forward-line 1)
             (search-forward "git")
             (match-beginning 0)))
          (closing-quote
           (save-excursion
             (goto-char (point-min))
             (forward-line 2)
             (point)))
          (closing-fence
           (save-excursion
             (goto-char (point-min))
             (forward-line 2)
             (search-forward "```")
             (match-beginning 0))))
      (should (equal (get-text-property body-quote 'display) "▎ │ "))
      (should-not (get-text-property body-quote 'line-prefix))
      (should (equal (get-text-property body-content 'wrap-prefix) "▎ │ "))
      (should (equal (get-text-property closing-quote 'display) "▎"))
      (should-not (get-text-property closing-quote 'line-prefix))
      (should-not (get-text-property closing-quote 'wrap-prefix))
      (should (equal (get-text-property closing-fence 'display) "╰─"))
      (goto-char (1+ body-quote))
      (markdown-ts-appear--update)
      (should-not (get-text-property body-quote 'display))
      (goto-char (point-max))
      (markdown-ts-appear--update)
      (should (equal (get-text-property body-quote 'display) "▎ │ ")))))

(ert-deftest markdown-ts-appear-test-quoted-code-preserves-literal-markers ()
  (dolist (case '(("> " "> ")
                  (">" ">")
                  (">\t" ">\t")
                  ("> > " "> > ")
                  (">>" ">>")
                  ("- > " "  > ")
                  ("> - > " ">   > ")))
    (pcase-let ((`(,opening ,prefix) case))
      (markdown-ts-appear-test--with-buffer
          (format "%s```text\n%s> literal\n%s> again\n%s```\n"
                  opening prefix prefix prefix)
        (dolist (marker '("▎" nil))
          (dolist (style '(connected raw))
            (let ((markdown-ts-appear-block-quote-marker marker)
                  (markdown-ts-appear-code-fence-style style))
              (markdown-ts-appear-mode -1)
              (goto-char (point-max))
              (markdown-ts-appear-mode 1)
              (font-lock-ensure)
              (goto-char (point-min))
              (dotimes (_ 2)
                (forward-line 1)
                (let* ((line-beg (point))
                       (literal (+ line-beg (length prefix)))
                       (last-marker (save-excursion
                                      (goto-char literal)
                                      (search-backward ">" line-beg))))
                  (should-not (get-text-property literal 'display))
                  (should-not (get-char-property literal 'invisible))
                  (should
                   (equal (get-text-property last-marker 'display)
                          (if (eq style 'connected)
                              (concat (or marker ">") " │ ")
                            marker)))
                  (should-not (get-text-property literal 'line-prefix))
                  (should
                   (equal (get-text-property literal 'wrap-prefix)
                          (and (eq style 'connected)
                               (concat (replace-regexp-in-string
                                        ">" (or marker ">") prefix t t)
                                       "│ "))))
                  (while (search-forward ">" last-marker t)
                    (should (equal (get-text-property (1- (point)) 'display)
                                   marker)))
                  (goto-char line-beg))))))))))

(ert-deftest markdown-ts-appear-test-quoted-code-prefix-respects-region ()
  (markdown-ts-appear-test--with-buffer
      "> > ```text\n> > > literal\n> > ```\n"
    (goto-char (point-min))
    (search-forward "literal")
    (let* ((literal (- (point) (length "> literal")))
           (last-marker (- literal 2))
           (body-end (line-beginning-position 2))
           (block (markdown-ts-appear--node-ancestor
                   (treesit-node-at literal 'markdown) "fenced_code_block")))
      (cl-loop for beg from (point-min) below (point-max)
               for end = (1+ beg) do
               (remove-list-of-text-properties
                (point-min) (point-max)
                '(display line-prefix wrap-prefix markdown-ts-appear--decoration))
               (markdown-ts-appear--fontify-code-block block 'append beg end)
               (dolist (property '(display line-prefix wrap-prefix
                                           markdown-ts-appear--decoration))
                 (should-not
                  (text-property-not-all (point-min) beg property nil))
                 (should-not
                  (text-property-not-all end (point-max) property nil)))
               (should (equal (get-text-property beg 'display)
                              (and (= beg last-marker) "▎ │ ")))
               (should-not (get-text-property beg 'line-prefix))
               (should (equal (get-text-property beg 'wrap-prefix)
                              (and (<= literal beg) (< beg body-end)
                                   "▎ ▎ │ ")))))))

(ert-deftest markdown-ts-appear-test-renders-unicode-table ()
  (markdown-ts-appear-test--with-buffer
      "| Element | Status |\n|:--------|-------:|\n| Heading | Ready  |\n"
    (should (equal (get-text-property 1 'display) "│"))
    (goto-char (point-min))
    (search-forward "|:--------")
    (should (equal (get-text-property (match-beginning 0) 'display) "┼"))
    (should (equal (get-text-property (1+ (match-beginning 0)) 'display) "─"))
    (goto-char (point-min))
    (search-forward "Element")
    (markdown-ts-appear--update)
    (should-not (get-text-property (point-min) 'display))))

(ert-deftest markdown-ts-appear-test-renders-table-without-edge-pipes ()
  (markdown-ts-appear-test--with-buffer
      "Element | Status\n--------|-------\nHeading | Ready\n"
    (goto-char (point-min))
    (search-forward "|")
    (should (equal (get-text-property (1- (point)) 'display) "│"))
    (search-forward "|")
    (should (equal (get-text-property (1- (point)) 'display) "┼"))))

(ert-deftest markdown-ts-appear-test-preserves-table-indentation ()
  (markdown-ts-appear-test--with-buffer
      "  | A | B |\n  |---|---|\n  | 1 | 2 |\n"
    (goto-char (point-min))
    (forward-line 1)
    (should-not (get-text-property (point) 'display))
    (should-not (get-text-property (1+ (point)) 'display))
    (search-forward "|")
    (should (equal (get-text-property (1- (point)) 'display) "┼"))))

(ert-deftest markdown-ts-appear-test-block-fontifiers-respect-region ()
  (markdown-ts-appear-test--with-buffer
      "> first\n> second\n\n| A | B |\n|---|---|\n| 1 | 2 |\n"
    (let* ((quote (markdown-ts-appear--node-ancestor
                   (treesit-node-at (point-min) 'markdown) "block_quote"))
           (table-pos (save-excursion
                        (goto-char (point-min))
                        (search-forward "| A")
                        (match-beginning 0)))
           (table (markdown-ts-appear--node-ancestor
                   (treesit-node-at table-pos 'markdown) "pipe_table"))
           (second-quote-marker (save-excursion
                                  (goto-char (point-min))
                                  (forward-line 1)
                                  (point))))
      (remove-text-properties
       (point-min) (point-max)
       '(display nil markdown-ts-appear--decoration nil))
      (markdown-ts-appear--fontify-block-quote
       quote 'append second-quote-marker (1+ second-quote-marker))
      (should-not (get-text-property (point-min) 'display))
      (should (equal (get-text-property second-quote-marker 'display) "▎"))
      (put-text-property (point-min) (1+ (point-min)) 'display 'foreign)
      (markdown-ts-appear--fontify-quote-marker
       (markdown-ts-appear--first-direct-child-of-type
        quote "block_quote_marker")
       nil second-quote-marker (1+ second-quote-marker))
      (should (eq (get-text-property (point-min) 'display) 'foreign))
      (remove-text-properties
       (point-min) (point-max)
       '(display nil markdown-ts-appear--decoration nil))
      (markdown-ts-appear--fontify-table
       table 'append table-pos (1+ table-pos))
      (should (equal (get-text-property table-pos 'display) "│"))
      (should-not
       (get-text-property
        (save-excursion
          (goto-char table-pos)
          (search-forward "|")
          (search-forward "|")
          (1- (point)))
        'display)))))

(ert-deftest markdown-ts-appear-test-can-disable-decorations ()
  (let ((markdown-ts-appear-test--decorations nil))
    (markdown-ts-appear-test--with-buffer
        "> quote\n\n[link](https://example.com)\n\n[[target|alias]]\n\n```c\nint x;\n```\n"
      (goto-char (point-min))
      (should-not (get-text-property (point) 'invisible))
      (should-not (get-text-property (point) 'display))
      (search-forward "```")
      (search-forward "```")
      (should-not (get-text-property (match-beginning 0) 'display))
      (should-not markdown-ts-appear--block-font-lock-settings)
      (should (advice-function-member-p
               #'markdown-ts-appear--decoration-filter-copied-text
               filter-buffer-substring-function))
      (should-not
       (seq-some
        (lambda (overlay)
          (overlay-get overlay 'markdown-ts-appear--link-icon))
        (overlays-in (point-min) (point-max)))))))

(ert-deftest markdown-ts-appear-test-empty-image-without-icon ()
  (let ((markdown-ts-appear-test--decorations nil))
    (markdown-ts-appear-test--with-buffer "![](images/demo.gif)\n"
      (goto-char (point-min))
      (let ((beg (search-forward "images/demo.gif")))
	(should-not
	 (text-property-not-all (- beg (length "images/demo.gif")) beg
				'invisible nil)))
      (should-not
       (seq-some
	(lambda (overlay)
	  (overlay-get overlay 'markdown-ts-appear--image-label))
	(overlays-in (point-min) (point-max)))))))

(ert-deftest markdown-ts-appear-test-setup-error-allows-manual-disable ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (with-temp-buffer
    (insert "**bold**\n")
    (markdown-ts-mode)
    (cl-letf (((symbol-function 'markdown-ts-appear-start)
               (lambda () (error "Setup failed"))))
      (should-error (markdown-ts-appear-mode 1) :type 'error))
    (should markdown-ts-appear-mode)
    (should (markdown-ts-appear--active-p))
    (should markdown-ts-hide-markup)
    (should (memq #'markdown-ts-appear--after-change after-change-functions))
    (should (markdown-ts-appear-test--advice-installed-p))
    (markdown-ts-appear-mode -1)
    (should-not markdown-ts-appear-mode)
    (should-not (markdown-ts-appear--active-p))
    (should-not markdown-ts-hide-markup)
    (should-not (memq #'markdown-ts-appear--after-change after-change-functions))
    (should (markdown-ts-appear-test--advice-installed-p))))

(ert-deftest markdown-ts-appear-test-decoration-copy-filter-preserves-other-display ()
  (let ((text (propertize "ab" 'display 'other-package)))
    (put-text-property 0 1 'markdown-ts-appear--decoration t text)
    (markdown-ts-appear--decoration-filter-copied-text text)
    (should-not (get-text-property 0 'display text))
    (should-not (get-text-property 0 'markdown-ts-appear--decoration text))
    (should (eq (get-text-property 1 'display text) 'other-package))))

(ert-deftest markdown-ts-appear-test-removes-only-markup-invisibility ()
  (with-temp-buffer
    (insert "ab")
    (put-text-property 1 2 'invisible 'other-package)
    (put-text-property 2 3 'invisible 'markdown-ts--markup)
    (markdown-ts-appear--remove-markup-invisibility 1 3)
    (should (eq (get-text-property 1 'invisible) 'other-package))
    (should-not (get-text-property 2 'invisible))))

(ert-deftest markdown-ts-appear-test-copy-removes-rendered-decorations ()
  (let ((source "> quote\n\n```c\nint x;\n```\n\n| A | B |\n|---|---|\n| 1 | 2 |\n"))
    (markdown-ts-appear-test--with-buffer source
      (let ((copy (filter-buffer-substring (point-min) (point-max))))
	(should (equal copy source))
	(should-not
	 (text-property-not-all 0 (length copy) 'display nil copy))
	(should-not
	 (text-property-not-all
	  0 (length copy) 'markdown-ts-appear--decoration nil copy))
	(should-not
	 (text-property-not-all
	  0 (length copy) 'line-prefix nil copy))
	(should-not
	 (text-property-not-all
	  0 (length copy) 'wrap-prefix nil copy))))))

(ert-deftest markdown-ts-appear-test-copy-removes-setext-line-height ()
  (let ((source "Title\n=====\n"))
    (markdown-ts-appear-test--with-buffer source
      (let ((copy (filter-buffer-substring (point-min) (point-max))))
        (should (equal copy source))
        (should-not (text-property-any 0 (length copy) 'line-height 0 copy))
        (should-not
         (text-property-not-all
          0 (length copy) 'markdown-ts-appear--decoration nil copy))))))

(ert-deftest markdown-ts-appear-test-icons-use-strings ()
  (dolist (value '("" "[icon]"))
    (let ((markdown-ts-appear-link-icon value)
          (markdown-ts-appear-image-icon value)
          (markdown-ts-appear-wikilink-icon value))
      (dolist (type '(link image wikilink))
        (let ((icon (markdown-ts-appear--icon type)))
          (if (string-empty-p value)
              (should-not icon)
            (should (equal icon value))
            (should (eq (get-text-property 0 'face icon)
                        'markdown-ts-link))))))))

(ert-deftest markdown-ts-appear-test-disable-cleans-block-rendering ()
  (markdown-ts-appear-test--with-buffer
      "> quote\n\n```c\nint x;\n```\n\n| A | B |\n|---|---|\n| 1 | 2 |\n"
    (should markdown-ts-appear--block-font-lock-settings)
    (should (advice-function-member-p
             #'markdown-ts-appear--decoration-filter-copied-text
             filter-buffer-substring-function))
    (should (text-property-not-all
             (point-min) (point-max) 'markdown-ts-appear--decoration nil))
    (markdown-ts-appear-mode -1)
    (font-lock-ensure)
    (should-not markdown-ts-appear--block-font-lock-settings)
    (should-not (advice-function-member-p
                 #'markdown-ts-appear--decoration-filter-copied-text
                 filter-buffer-substring-function))
    (should-not (text-property-not-all
                 (point-min) (point-max)
                 'markdown-ts-appear--decoration nil))
    (should-not (text-property-not-all
                 (point-min) (point-max) 'line-prefix nil))
    (should-not (text-property-not-all
                 (point-min) (point-max) 'wrap-prefix nil))))

(ert-deftest markdown-ts-appear-test-copy-filter-installation-is-idempotent ()
  (markdown-ts-appear-test--with-buffer "> quote\n"
    (let* ((calls 0)
           (foreign-filter (lambda (text) (setq calls (1+ calls)) text)))
      (add-function :filter-return (local 'filter-buffer-substring-function)
                    foreign-filter)
      (let ((installed filter-buffer-substring-function))
        (markdown-ts-appear--install-block-font-lock)
        (should (eq installed filter-buffer-substring-function)))
      (filter-buffer-substring (point-min) (point-max))
      (should (= calls 1))
      (markdown-ts-appear-mode -1)
      (should-not (advice-function-member-p
                   #'markdown-ts-appear--decoration-filter-copied-text
                   filter-buffer-substring-function))
      (should (advice-function-member-p foreign-filter
                                        filter-buffer-substring-function))
      (filter-buffer-substring (point-min) (point-max))
      (should (= calls 2)))))

(ert-deftest markdown-ts-appear-test-removes-only-owned-font-lock-rules ()
  (markdown-ts-appear-test--with-buffer "> quote\n"
    (let* ((owned (car markdown-ts-appear--block-font-lock-settings))
           (foreign (copy-tree owned t)))
      (should (memq owned treesit-font-lock-settings))
      (push foreign treesit-font-lock-settings)
      (markdown-ts-appear-mode -1)
      (should-not markdown-ts-appear--block-font-lock-settings)
      (should-not (memq owned treesit-font-lock-settings))
      (should (memq foreign treesit-font-lock-settings)))))

(ert-deftest markdown-ts-appear-test-advice-lasts-until-unload ()
  (should (markdown-ts-appear-test--advice-installed-p))
  (markdown-ts-appear-test--with-buffer "**bold**\n"
    (should (markdown-ts-appear-test--advice-installed-p)))
  (should (markdown-ts-appear-test--advice-installed-p)))

(ert-deftest markdown-ts-appear-test-inactive-wrappers-pass-through ()
  (with-temp-buffer
    (dolist (mode '(nil t))
      ;; A mode value alone must not claim a buffer during initialization.
      (let ((markdown-ts-appear-mode mode)
            (markdown-ts-hide-markup 'original)
            (markdown-ts-inline-images 'original)
            (arguments '(node override 1 2 extra)))
        (dolist (binding (markdown-ts-appear--advice-bindings))
          (let ((calls 0))
            (should
             (eq 'result
                 (apply (cdr binding)
                        (lambda (&rest actual)
                          (setq calls (1+ calls))
                          (should (equal actual arguments))
                          (should (eq markdown-ts-hide-markup 'original))
                          (should (eq markdown-ts-inline-images 'original))
                          'result)
                        arguments)))
            (should (= calls 1))))))))

(ert-deftest markdown-ts-appear-test-disabling-one-buffer-preserves-another ()
  (markdown-ts-appear-test--with-buffer "**base** plain\n"
    (let ((base (current-buffer)))
      (markdown-ts-appear-test--with-buffer "**other** plain\n"
        (markdown-ts-appear-mode -1)
        (should-not (get-text-property (point-min) 'invisible))
        (with-current-buffer base
          (should (markdown-ts-appear--active-p))
          (goto-char 3)
          (markdown-ts-appear--update)
          (should-not (get-text-property (point-min) 'invisible))
          (goto-char (point-max))
          (markdown-ts-appear--update)
          (should (get-text-property (point-min) 'invisible)))))))

(ert-deftest markdown-ts-appear-test-required-private-functions-available ()
  (should-not (markdown-ts-appear--missing-private-functions))
  (markdown-ts-appear-test--with-buffer "**bold**\n"
    (should (markdown-ts-appear-test--advice-installed-p))
    (should-not
     (markdown-ts-appear--missing-private-functions))))

(ert-deftest markdown-ts-appear-test-bounds-honor-position-argument ()
  (markdown-ts-appear-test--with-buffer "- first\n\n- second\n"
    (let ((position
           (save-excursion
             (goto-char (point-min))
             (search-forward "- second")
             (- (point) (length "- second")))))
      (goto-char (point-min))
      (pcase-let ((`(,beg . ,end)
                   (markdown-ts-appear--bounds-at-point position)))
        (should (equal (buffer-substring-no-properties beg end) "- "))))))

(ert-deftest markdown-ts-appear-test-helpers-preserve-match-data ()
  (markdown-ts-appear-test--with-buffer "> [!NOTE]\n"
    (let ((quote (markdown-ts-appear--node-ancestor
                  (treesit-node-at (point-min) 'markdown) "block_quote")))
      (string-match "\\(seed\\)" "seed")
      (let ((match-data (match-data)))
        (should (markdown-ts-appear--callout-data quote))
        (should (equal (match-data) match-data))))))

(ert-deftest markdown-ts-appear-test-bounds-do-not-force-fontification ()
  (markdown-ts-appear-test--with-buffer "**bold**\n"
    (goto-char 3)
    (cl-letf (((symbol-function 'font-lock-ensure)
	       (lambda (&rest _arguments)
		 (ert-fail "Bounds lookup forced fontification"))))
      (should (equal (markdown-ts-appear--bounds) '(1 . 9))))))

(ert-deftest markdown-ts-appear-test-unload-cleans-global-state ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (unwind-protect
      (markdown-ts-appear-test--with-buffer "[Emacs](https://www.gnu.org/)\n"
        (let ((icons (seq-filter
                      (lambda (overlay)
                        (overlay-get overlay 'markdown-ts-appear--link-icon))
                      (overlays-in (point-min) (point-max))))
              (foreign (make-overlay (point-min) (point-max))))
          (should icons)
          (markdown-ts-appear-unload-function)
          (should-not markdown-ts-appear-mode)
          (should-not (markdown-ts-appear--active-p))
          (should-not markdown-ts-appear--block-font-lock-settings)
          (should-not (advice-function-member-p
                       #'markdown-ts-appear--decoration-filter-copied-text
                       filter-buffer-substring-function))
          (dolist (icon icons)
            (should-not (overlay-buffer icon)))
          (should (overlay-buffer foreign))
          (should-not (markdown-ts-appear-test--advice-installed-p))))
    (markdown-ts-appear--install-advice)))

;;; markdown-ts-appear-test.el ends here
