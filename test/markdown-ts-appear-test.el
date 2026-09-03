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

(declare-function mathjax-available-p "mathjax")
(declare-function mathjax-render "mathjax")

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
(require 'markdown-ts-appear)

(when (and (getenv "MARKDOWN_TS_APPEAR_REQUIRE_GRAMMARS")
           (not (treesit-ready-p '(markdown markdown-inline))))
  (error "Required Markdown Tree-sitter grammars are unavailable"))

(defvar markdown-ts-appear-test--use-nerd-icons t
  "Whether buffer tests enable Nerd Icons integration.")

(defmacro markdown-ts-appear-test--with-buffer (content &rest body)
  "Create a Markdown buffer containing CONTENT and evaluate BODY."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (treesit-ready-p '(markdown markdown-inline)))
     (with-temp-buffer
       (insert ,content)
       (let ((treesit-font-lock-level 3)
             (markdown-ts-appear-enable-math-preview nil)
             (markdown-ts-appear-trigger 'always)
             (markdown-ts-appear-use-nerd-icons
              markdown-ts-appear-test--use-nerd-icons)
             (markdown-ts-appear-center-display-math t)
             (markdown-ts-inline-images nil))
         (markdown-ts-mode)
         (unwind-protect
             (progn
               (markdown-ts-appear-mode 1)
               (font-lock-ensure)
               ,@body)
           (when markdown-ts-appear--setup-p
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
        (let ((markdown-ts-appear-enable-math-preview nil))
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
            (when markdown-ts-appear--setup-p
              (markdown-ts-appear-mode -1))))))))

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
             ("---\n" "---" "---\n")))
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
  (let ((markdown-ts-appear-test--use-nerd-icons nil))
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

(ert-deftest markdown-ts-appear-test-restores-enabled-hide-markup ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (with-temp-buffer
    (insert "**bold**\n")
    (markdown-ts-mode)
    (setq markdown-ts-hide-markup t)
    (markdown-ts--set-hide-markup t)
    (markdown-ts-appear-mode 1)
    (markdown-ts-appear-mode -1)
    (should markdown-ts-hide-markup)))

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

(ert-deftest markdown-ts-appear-test-notifies-inline-parser-created-after-setup ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (with-temp-buffer
    (let ((markdown-ts-appear-enable-math-preview nil)
          (markdown-ts-appear-trigger 'always)
          (markdown-ts-inline-images nil))
      (markdown-ts-mode)
      (unwind-protect
          (progn
            (markdown-ts-appear-mode 1)
            (insert "plain\n[Emacs](https://www.gnu.org/)`")
            (font-lock-ensure)
            (should markdown-ts-appear--inline-parser-notified-p)
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
        (when markdown-ts-appear--setup-p
          (markdown-ts-appear-mode -1))))))

(ert-deftest markdown-ts-appear-test-cleans-up-before-major-mode-change ()
  (markdown-ts-appear-test--with-buffer "[Emacs](https://www.gnu.org/)\n"
					(with-silent-modifications
					  (put-text-property
					   (point-min) (1+ (point-min)) 'display 'math-image)
					  (put-text-property
					   (point-min) (1+ (point-min))
					   'markdown-ts-appear--math-state 'rendered))
					(should markdown-ts-appear--setup-p)
					(text-mode)
					(should-not (get-text-property (point-min) 'display))
					(should-not
					 (get-text-property
					  (point-min) 'markdown-ts-appear--math-state))
					(should-not
					 (seq-some
					  (lambda (overlay)
					    (or (overlay-get overlay 'markdown-ts-appear--link-icon)
						(overlay-get overlay 'markdown-ts-appear--image-label)
						(overlay-get overlay 'markdown-ts-appear--math-alignment)))
					  (overlays-in (point-min) (point-max))))))

(ert-deftest markdown-ts-appear-test-detaches-indirect-clone ()
  (let ((entry-bound-p (boundp 'evil-insert-state-entry-hook))
        (entry-value (and (boundp 'evil-insert-state-entry-hook)
                          evil-insert-state-entry-hook))
        (exit-bound-p (boundp 'evil-insert-state-exit-hook))
        (exit-value (and (boundp 'evil-insert-state-exit-hook)
                         evil-insert-state-exit-hook)))
    (unwind-protect
        (progn
          (makunbound 'evil-insert-state-entry-hook)
          (makunbound 'evil-insert-state-exit-hook)
          (markdown-ts-appear-test--with-buffer "**bold** plain\n"
						(goto-char 3)
						(markdown-ts-appear--update)
						(let ((region markdown-ts-appear--region)
						      (clone
						       (clone-indirect-buffer " *markdown-appear-clone*" nil)))
						  (unwind-protect
						      (progn
							(with-current-buffer clone
							  (should-not markdown-ts-appear-mode)
							  (should-not markdown-ts-appear--setup-p)
							  (should-not markdown-ts-appear--region)
							  (should-not
							   (memq #'markdown-ts-appear--update post-command-hook))
							  (font-lock-flush)
							  (font-lock-ensure))
							(should (marker-position (car region)))
							(should (marker-position (cdr region)))
							(should-not (get-text-property (point-min) 'invisible)))
						    (kill-buffer clone)))))
      (if entry-bound-p
          (set 'evil-insert-state-entry-hook entry-value)
        (makunbound 'evil-insert-state-entry-hook))
      (if exit-bound-p
          (set 'evil-insert-state-exit-hook exit-value)
        (makunbound 'evil-insert-state-exit-hook)))))

(ert-deftest markdown-ts-appear-test-cleans-indirect-clone-overlays ()
  (markdown-ts-appear-test--with-buffer
   "[Emacs](https://www.gnu.org/)\n"
   (let ((clone (clone-indirect-buffer " *markdown-appear-clone*" nil)))
     (unwind-protect
         (progn
           (with-current-buffer clone
             (font-lock-flush)
             (font-lock-ensure)
             (should
              (seq-some
               (lambda (overlay)
                 (overlay-get overlay 'markdown-ts-appear--link-icon))
               (overlays-in (point-min) (point-max)))))
           (markdown-ts-appear-mode -1)
           (should-not markdown-ts-appear--indirect-clones)
           (with-current-buffer clone
             (should-not markdown-ts-appear--base-owner)
             (should-not
              (memq #'markdown-ts-appear--after-change
                    after-change-functions))
             (should-not
              (memq #'markdown-ts-appear--math-preview-owner-window
                    window-buffer-change-functions))
             (font-lock-flush)
             (font-lock-ensure)
             (should-not (get-text-property (point-min) 'invisible))
             (should-not
              (seq-some
               (lambda (overlay)
                 (overlay-get overlay 'markdown-ts-appear--link-icon))
               (overlays-in (point-min) (point-max))))))
       (kill-buffer clone)))))

(ert-deftest markdown-ts-appear-test-teardown-survives-deleted-parser ()
  (markdown-ts-appear-test--with-buffer
   "[Emacs](https://www.gnu.org/)\n"
   (let ((clone (clone-indirect-buffer " *markdown-deleted-parser*" nil)))
     (unwind-protect
         (cl-letf (((symbol-function 'font-lock-ensure)
                    (lambda (&rest _arguments)
                      (signal 'treesit-parser-deleted nil))))
           (markdown-ts-appear-mode -1)
           (should-not markdown-ts-appear--setup-p)
           (should-not markdown-ts-appear--indirect-clones)
           (with-current-buffer clone
             (should-not markdown-ts-appear--base-owner)))
       (kill-buffer clone)))))

(ert-deftest markdown-ts-appear-test-invalidates-clone-overlays-after-edit ()
  (markdown-ts-appear-test--with-buffer
   "[Emacs](https://www.gnu.org/)\n"
   (let ((clone (clone-indirect-buffer " *markdown-appear-clone*" nil)))
     (unwind-protect
         (progn
           (with-current-buffer clone
             (font-lock-flush)
             (font-lock-ensure)
             (should
              (seq-some
               (lambda (overlay)
                 (overlay-get overlay 'markdown-ts-appear--link-icon))
               (overlays-in (point-min) (point-max))))
             (erase-buffer)
             (insert "plain\n"))
           (should-not
            (seq-some
             (lambda (overlay)
               (overlay-get overlay 'markdown-ts-appear--link-icon))
             (overlays-in (point-min) (point-max))))
           (with-current-buffer clone
             (should-not
              (seq-some
               (lambda (overlay)
                 (overlay-get overlay 'markdown-ts-appear--link-icon))
               (overlays-in (point-min) (point-max))))))
       (kill-buffer clone)))))

(ert-deftest markdown-ts-appear-test-clone-major-mode-change-disables-base ()
  (markdown-ts-appear-test--with-buffer "**bold**\n"
					(let ((clone (clone-indirect-buffer " *markdown-appear-clone*" nil)))
					  (unwind-protect
					      (progn
						(should (memq clone markdown-ts-appear--indirect-clones))
						(with-current-buffer clone
						  (text-mode))
						(should-not markdown-ts-appear-mode)
						(should-not markdown-ts-appear--setup-p)
						(should-not markdown-ts-appear--indirect-clones)
						(should-not (get-text-property (point-min) 'invisible))
						(with-current-buffer clone
						  (should-not markdown-ts-appear--base-owner)))
					    (when (buffer-live-p clone)
					      (kill-buffer clone))))))

(ert-deftest markdown-ts-appear-test-adopts-existing-indirect-clone ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (with-temp-buffer
    (insert "**bold**\n")
    (let ((markdown-ts-appear-enable-math-preview nil)
          (clone nil))
      (markdown-ts-mode)
      (setq clone (clone-indirect-buffer " *markdown-existing-clone*" nil))
      (with-current-buffer clone
        (setq markdown-ts-hide-markup t)
        (add-to-list 'font-lock-extra-managed-props 'line-height))
      (unwind-protect
          (progn
            (markdown-ts-appear-mode 1)
            (should (memq clone markdown-ts-appear--indirect-clones))
            (with-current-buffer clone
              (should (eq markdown-ts-appear--base-owner
                          (buffer-base-buffer)))
              (font-lock-flush)
              (font-lock-ensure)
              (should (get-text-property (point-min) 'invisible)))
            (markdown-ts-appear-mode -1)
            (with-current-buffer clone
              (should markdown-ts-hide-markup)
              (should (memq 'line-height font-lock-extra-managed-props))))
        (when markdown-ts-appear--setup-p
          (markdown-ts-appear-mode -1))
        (when (buffer-live-p clone)
          (kill-buffer clone))))))

(ert-deftest markdown-ts-appear-test-graphical-clone-enables-base-math ()
  (markdown-ts-appear-test--with-buffer "$x$\n"
					(let ((original-require (symbol-function 'require))
					      (window (selected-window))
					      (previous-buffer (window-buffer (selected-window)))
					      (clone (clone-indirect-buffer " *markdown-math-window*" nil)))
					  (unwind-protect
					      (let ((markdown-ts-appear-enable-math-preview t))
						(cl-letf (((symbol-function 'require)
							   (lambda (feature &optional filename noerror)
							     (if (eq feature 'mathjax)
								 t
							       (funcall original-require
									feature filename noerror))))
							  ((symbol-function 'display-graphic-p)
							   (lambda (&optional display)
							     (eq display (window-frame window))))
							  ((symbol-function 'image-type-available-p)
							   (lambda (_type) t))
							  ((symbol-function 'mathjax-available-p)
							   (lambda () t)))
						  (set-window-buffer window clone)
						  (with-current-buffer clone
						    (markdown-ts-appear--math-preview-owner-window window))
						  (should markdown-ts-appear--math-preview-active-p)
						  (with-current-buffer clone
						    (should markdown-ts-appear--math-filter-installed-p)
						    (should (memq 'markdown-ts-appear--math-state
								  font-lock-extra-managed-props)))))
					    (when (window-live-p window)
					      (set-window-buffer window previous-buffer))
					    (when (buffer-live-p clone)
					      (kill-buffer clone))))))

(ert-deftest markdown-ts-appear-test-existing-graphical-clone-enables-math ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (let ((original-require (symbol-function 'require))
        (window (selected-window))
        (previous-buffer (window-buffer (selected-window)))
        (base (generate-new-buffer " *markdown-math-base*"))
        clone)
    (unwind-protect
        (progn
          (with-current-buffer base
            (insert "$x$\n")
            (markdown-ts-mode)
            (setq clone
                  (clone-indirect-buffer " *markdown-existing-window*" nil)))
          (set-window-buffer window clone)
          (cl-letf (((symbol-function 'require)
                     (lambda (feature &optional filename noerror)
                       (if (eq feature 'mathjax)
                           t
                         (funcall original-require feature filename noerror))))
                    ((symbol-function 'display-graphic-p)
                     (lambda (&optional display)
                       (eq display (window-frame window))))
                    ((symbol-function 'image-type-available-p)
                     (lambda (_type) t))
                    ((symbol-function 'mathjax-available-p)
                     (lambda () t)))
            (with-current-buffer base
              (let ((markdown-ts-appear-enable-math-preview t))
                (markdown-ts-appear-mode 1))
              (should markdown-ts-appear--math-preview-active-p))))
      (when (window-live-p window)
        (set-window-buffer window previous-buffer))
      (when (buffer-live-p base)
        (with-current-buffer base
          (when markdown-ts-appear--setup-p
            (markdown-ts-appear-mode -1))))
      (when (buffer-live-p clone)
        (kill-buffer clone))
      (when (buffer-live-p base)
        (kill-buffer base)))))

(ert-deftest markdown-ts-appear-test-clone-refontification-restores-math ()
  (markdown-ts-appear-test--with-buffer "$$x$$\n"
					(let ((original-require (symbol-function 'require))
					      (original-request
					       (symbol-function 'markdown-ts-appear--math-request))
					      (original-display-result
					       (symbol-function 'markdown-ts-appear--math-display-result))
					      (markdown-ts-appear--math-cache (make-hash-table :test #'equal))
					      clone request-buffer request-owner)
					  (puthash '(t 1.1 "x")
						   '((svg . "<svg width=\"1ex\" height=\"1ex\"></svg>")
						     (markdown-ts-appear--math-image . test-image))
						   markdown-ts-appear--math-cache)
					  (cl-letf (((symbol-function 'require)
						     (lambda (feature &optional filename noerror)
						       (if (eq feature 'mathjax)
							   t
							 (funcall original-require feature filename noerror))))
						    ((symbol-function 'display-graphic-p)
						     (lambda (&optional _display) t))
						    ((symbol-function 'image-type-available-p)
						     (lambda (_type) t))
						    ((symbol-function 'mathjax-available-p)
						     (lambda () t))
						    ((symbol-function 'markdown-ts-appear--math-request)
						     (lambda (&rest arguments)
						       (setq request-buffer (current-buffer))
						       (apply original-request arguments)))
						    ((symbol-function
						      'markdown-ts-appear--math-display-result)
						     (lambda (request data)
						       (setq request-owner (car request))
						       (funcall original-display-result request data))))
					    (unwind-protect
						(progn
						  (setq clone
							(clone-indirect-buffer " *markdown-math-clone*" nil))
						  (markdown-ts-appear--math-enable)
						  (with-current-buffer clone
						    (should markdown-ts-appear--math-filter-installed-p)
						    (should (memq 'markdown-ts-appear--math-state
								  font-lock-extra-managed-props)))
						  (font-lock-ensure)
						  (should (eq (get-text-property (point-min) 'display)
							      'test-image))
						  (setq request-buffer nil)
						  (setq request-owner nil)
						  (with-current-buffer clone
						    (should (markdown-ts-appear--math-active-p))
						    (font-lock-flush)
						    (font-lock-ensure))
						  (should (eq request-buffer clone))
						  (should (eq request-owner (current-buffer)))
						  (should (eq (get-text-property (point-min) 'display)
							      'test-image))
						  (should
						   (seq-some
						    (lambda (overlay)
						      (overlay-get overlay
								   'markdown-ts-appear--math-alignment))
						    (overlays-in (point-min) (point-max))))
						  (with-current-buffer clone
						    (should
						     (seq-some
						      (lambda (overlay)
							(overlay-get overlay
								     'markdown-ts-appear--math-alignment))
						      (overlays-in (point-min) (point-max))))))
					      (when (buffer-live-p clone)
						(kill-buffer clone)))))))

(ert-deftest markdown-ts-appear-test-shows-empty-image-label ()
  (markdown-ts-appear-test--with-buffer "![](images/demo.gif)\n"
					(goto-char (point-min))
					(let ((beg (search-forward "images/demo.gif")))
					  (should-not
					   (text-property-not-all (- beg (length "images/demo.gif")) beg
								  'invisible nil)))
					(let ((overlay
					       (seq-find
						(lambda (candidate)
						  (overlay-get candidate 'markdown-ts-appear--image-label))
						(overlays-in (point-min) (point-max)))))
					  (should overlay)
					  (should (stringp (overlay-get overlay 'before-string)))
					  (should (equal (overlay-get overlay 'help-echo) "images/demo.gif")))))

(ert-deftest markdown-ts-appear-test-keeps-quote-markers-visible ()
  (markdown-ts-appear-test--with-buffer "   > quote\n> > nested\n"
					(goto-char (point-min))
					(search-forward ">")
					(should-not (get-text-property (1- (point)) 'invisible))
					(search-forward ">")
					(should-not (get-text-property (1- (point)) 'invisible))
					(search-forward ">")
					(should-not (get-text-property (1- (point)) 'invisible))))

(ert-deftest markdown-ts-appear-test-keeps-code-fences-visible ()
  (markdown-ts-appear-test--with-buffer
   "```emacs-lisp\n(message \"hi\")\n```\n"
   (goto-char (point-min))
   (should-not (get-text-property (point) 'invisible))
   (search-forward "emacs-lisp")
   (should-not (get-text-property (1- (point)) 'invisible))
   (search-forward "```")
   (should-not (get-text-property (match-beginning 0) 'invisible))))

(ert-deftest markdown-ts-appear-test-can-disable-decorations ()
  (let ((markdown-ts-appear-test--use-nerd-icons nil))
    (markdown-ts-appear-test--with-buffer
     "> quote\n\n[link](https://example.com)\n\n```c\nint x;\n```\n"
     (goto-char (point-min))
     (should-not (get-text-property (point) 'invisible))
     (should-not
      (seq-some
       (lambda (overlay)
         (overlay-get overlay 'markdown-ts-appear--link-icon))
       (overlays-in (point-min) (point-max)))))))

(ert-deftest markdown-ts-appear-test-empty-image-without-icon ()
  (let ((markdown-ts-appear-test--use-nerd-icons nil))
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

(ert-deftest markdown-ts-appear-test-centers-display-math ()
  (with-temp-buffer
    (insert "$$x$$")
    (let ((image '(image :type svg :data "<svg/>")))
      (markdown-ts-appear--math-center (point-min) (point-max) image)
      (let ((overlay
             (seq-find
              (lambda (candidate)
                (overlay-get candidate 'markdown-ts-appear--math-alignment))
              (overlays-in (point-min) (point-max)))))
        (should overlay)
        (should (stringp (overlay-get overlay 'before-string))))
      (markdown-ts-appear--math-clear (point-min) (point-max))
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest markdown-ts-appear-test-scales-math-svg ()
  (let ((markdown-ts-appear-math-scale 1.1))
    (cl-letf (((symbol-function 'svg-image)
               (lambda (svg &rest properties)
                 (cons svg properties))))
      (let* ((image
              (markdown-ts-appear--math-image
               (concat "<svg width=\"2ex\" height=\"3ex\">"
                       "<rect width=\"4\" height=\"5\"/></svg>")))
             (svg (car image))
             (properties (cdr image)))
        (should (string-match-p "width=\"2.2ex\"" svg))
        (should (string-match-p "height=\"3.3ex\"" svg))
        (should (string-match-p "<rect width=\"4\" height=\"5\"" svg))
        (should-not (plist-member properties :scale))))))

(ert-deftest markdown-ts-appear-test-rejects-invalid-math-options ()
  (let ((markdown-ts-appear-math-timeout 0))
    (should-error (markdown-ts-appear--validate-math-options)
                  :type 'user-error))
  (let ((markdown-ts-appear-math-scale -1))
    (should-error (markdown-ts-appear--validate-math-options)
                  :type 'user-error)))

(ert-deftest markdown-ts-appear-test-invalid-math-config-rolls-back-main-mode ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (with-temp-buffer
    (markdown-ts-mode)
    (let ((markdown-ts-appear-enable-math-preview t)
          (markdown-ts-appear-math-timeout 0))
      (should-error (markdown-ts-appear-mode 1) :type 'user-error)
      (should-not markdown-ts-appear-mode)
      (should-not markdown-ts-appear--setup-p)
      (should-not markdown-ts-hide-markup)
      (should-not markdown-ts-appear--advice-installed-p))))

(ert-deftest markdown-ts-appear-test-math-cache-key-includes-scale ()
  (let ((markdown-ts-appear-math-scale 1.1))
    (should (equal (markdown-ts-appear--math-key "x" nil)
                   '(nil 1.1 "x"))))
  (let ((markdown-ts-appear-math-scale 1.25))
    (should (equal (markdown-ts-appear--math-key "x" t)
                   '(t 1.25 "x")))))

(ert-deftest markdown-ts-appear-test-mathjax-render-smoke ()
  (if (getenv "MARKDOWN_TS_APPEAR_REQUIRE_MATHJAX")
      (progn
        (should (require 'mathjax nil t))
        (should (mathjax-available-p)))
    (skip-unless (and (require 'mathjax nil t)
                      (mathjax-available-p))))
  (let (result)
    (mathjax-render (lambda (data) (setq result data)) "x^2")
    (let ((deadline (+ (float-time) 10)))
      (while (and (not result) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should result)
    (should-not (alist-get 'error result))
    (should (string-match-p "<svg\\b" (alist-get 'svg result)))))

(ert-deftest markdown-ts-appear-test-math-request-integration ()
  (markdown-ts-appear-test--with-buffer "$x$\n"
					(let ((original-require (symbol-function 'require))
					      (markdown-ts-appear--math-cache
					       (make-hash-table :test #'equal))
					      (markdown-ts-appear--math-pending
					       (make-hash-table :test #'equal))
					      callback formula arguments)
					  (cl-letf (((symbol-function 'require)
						     (lambda (feature &optional filename noerror)
						       (if (eq feature 'mathjax)
							   t
							 (funcall original-require feature filename noerror))))
						    ((symbol-function 'display-graphic-p)
						     (lambda (&optional _display) t))
						    ((symbol-function 'image-type-available-p)
						     (lambda (_type) t))
						    ((symbol-function 'mathjax-available-p)
						     (lambda () t))
						    ((symbol-function 'mathjax-render)
						     (lambda (render-callback math &rest render-arguments)
						       (setq callback render-callback
							     formula math
							     arguments render-arguments)))
						    ((symbol-function 'svg-image)
						     (lambda (svg &rest properties)
						       (cons svg properties))))
					    (unwind-protect
						(progn
						  (markdown-ts-appear--math-enable)
						  (font-lock-ensure)
						  (should (functionp callback))
						  (should (equal formula "x"))
						  (should (equal arguments '(:options (:display nil))))
						  (funcall callback
							   '((svg . "<svg width=\"1ex\" height=\"1ex\"></svg>")))
						  (should (get-text-property (point-min) 'display))
						  (should
						   (eq (car (get-text-property
							     (point-min) 'markdown-ts-appear--math-state))
						       'rendered)))
					      (markdown-ts-appear--math-teardown))))))

(ert-deftest markdown-ts-appear-test-copy-filter-preserves-owned-display ()
  (let ((text (propertize "ab" 'display 'other-package)))
    (put-text-property 0 1 'markdown-ts-appear--math-state 'rendered text)
    (markdown-ts-appear--math-filter-copied-text text)
    (should-not (get-text-property 0 'display text))
    (should (eq (get-text-property 1 'display text) 'other-package))))

(ert-deftest markdown-ts-appear-test-math-cleanup-preserves-other-display ()
  (with-temp-buffer
    (insert "ab")
    (put-text-property 1 2 'display 'other-package)
    (put-text-property 1 2 'markdown-ts-appear--math-state 'rendered)
    (put-text-property 2 3 'display 'other-package)
    (markdown-ts-appear--math-clear-buffer)
    (should-not (get-text-property 1 'display))
    (should (eq (get-text-property 2 'display) 'other-package))))

(ert-deftest markdown-ts-appear-test-cancels-pending-math ()
  (let ((markdown-ts-appear--math-pending (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert "$x$")
      (let* ((beg-marker (copy-marker (point-min)))
             (end-marker (copy-marker (point-max)))
             (request
              (list (current-buffer) beg-marker end-marker "$x$" 'key))
             (requests (make-hash-table :test #'equal))
             (timer (run-at-time 60 nil #'ignore)))
        (puthash 'request (list request) requests)
        (puthash 'key (list timer 'generation requests)
                 markdown-ts-appear--math-pending)
        (markdown-ts-appear--math-cancel-pending)
        (should (zerop (hash-table-count markdown-ts-appear--math-pending)))
        (should-not (marker-position beg-marker))
        (should-not (marker-position end-marker))))))

(ert-deftest markdown-ts-appear-test-ignores-stale-math-generation ()
  (let ((markdown-ts-appear--math-cache (make-hash-table :test #'equal))
        (markdown-ts-appear--math-pending (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert "$x$")
      (let* ((beg-marker (copy-marker (point-min)))
             (end-marker (copy-marker (point-max)))
             (request
              (list (current-buffer) beg-marker end-marker "$x$" 'key))
             (requests (make-hash-table :test #'equal))
             (timer (run-at-time 60 nil #'ignore))
             (pending (list timer 'current-generation requests)))
        (puthash 'request (list request) requests)
        (puthash 'key pending markdown-ts-appear--math-pending)
        (markdown-ts-appear--math-finish-render
         'key 'stale-generation '((error . "late") (transient . t)))
        (should (eq (gethash 'key markdown-ts-appear--math-pending) pending))
        (should (marker-position beg-marker))
        (should (marker-position end-marker))
        (markdown-ts-appear--math-finish-render
         'key 'current-generation '((error . "current") (transient . t)))
        (should-not (gethash 'key markdown-ts-appear--math-pending))
        (should-not (marker-position beg-marker))
        (should-not (marker-position end-marker))))))

(ert-deftest markdown-ts-appear-test-caches-math-image ()
  (let ((markdown-ts-appear--math-cache (make-hash-table :test #'equal))
        (markdown-ts-appear--math-pending (make-hash-table :test #'equal))
        (calls 0))
    (cl-letf (((symbol-function 'markdown-ts-appear--math-image)
               (lambda (_svg &optional _scale)
                 (setq calls (1+ calls))
                 (list 'image calls))))
      (puthash '(nil 1.1 "x")
               (list (run-at-time 60 nil #'ignore)
                     'generation (make-hash-table :test #'equal))
               markdown-ts-appear--math-pending)
      (markdown-ts-appear--math-finish-render
       '(nil 1.1 "x") 'generation
       '((svg . "<svg height=\"1\"></svg>")))
      (let ((data (gethash '(nil 1.1 "x")
                           markdown-ts-appear--math-cache)))
        (should (= calls 1))
        (should (equal (alist-get 'markdown-ts-appear--math-image data)
                       '(image 1)))))))

(ert-deftest markdown-ts-appear-test-clears-folded-pending-result ()
  (with-temp-buffer
    (insert "$x$")
    (let* ((markdown-ts-appear--math-preview-active-p t)
           (key '(nil . "x"))
           (beg-marker (copy-marker (point-min)))
           (end-marker (copy-marker (point-max)))
           (request (list (current-buffer) beg-marker end-marker "$x$" key)))
      (put-text-property (point-min) (point-max)
                         'markdown-ts-appear--math-state
                         (list 'pending key))
      (cl-letf (((symbol-function 'markdown-ts--outline-invisible-p)
                 (lambda (_position) t)))
        (markdown-ts-appear--math-display-result request '((svg . "<svg/>"))))
      (should-not (get-text-property
                   (point-min) 'markdown-ts-appear--math-state))
      (should-not (marker-position beg-marker))
      (should-not (marker-position end-marker)))))

(ert-deftest markdown-ts-appear-test-main-mode-manages-math-lifecycle ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (let ((original-require (symbol-function 'require))
        (window (selected-window))
        (previous-buffer (window-buffer (selected-window)))
        (buffer (generate-new-buffer " *markdown-math-main*")))
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional filename noerror)
                     (if (eq feature 'mathjax)
                         t
                       (funcall original-require feature filename noerror))))
                  ((symbol-function 'display-graphic-p)
                   (lambda (&optional _display) t))
                  ((symbol-function 'image-type-available-p)
                   (lambda (_type) t))
                  ((symbol-function 'mathjax-available-p)
                   (lambda () t)))
          (set-window-buffer window buffer)
          (with-current-buffer buffer
            (insert "$x$\n")
            (markdown-ts-mode)
            (let ((markdown-ts-appear-enable-math-preview t))
              (markdown-ts-appear-mode 1))
            (should markdown-ts-appear--math-preview-active-p)
            (should markdown-ts-appear--math-filter-installed-p)
            (with-suppressed-warnings ((obsolete outline-view-change-hook))
              (should (memq #'markdown-ts-appear--math-outline-view-change
                            outline-view-change-hook)))
            (markdown-ts-appear-mode -1)
            (should-not markdown-ts-appear--math-preview-active-p)
            (should-not markdown-ts-appear--math-filter-installed-p)
            (should-not (memq 'markdown-ts-appear--math-state
                              font-lock-extra-managed-props))))
      (when (window-live-p window)
        (set-window-buffer window previous-buffer))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest markdown-ts-appear-test-installs-advice-only-while-active ()
  (should-not markdown-ts-appear--advice-installed-p)
  (markdown-ts-appear-test--with-buffer "**bold**\n"
					(should markdown-ts-appear--advice-installed-p))
  (should-not markdown-ts-appear--advice-installed-p))

(ert-deftest markdown-ts-appear-test-private-api-contracts ()
  (should-not (markdown-ts-appear--private-api-incompatibilities))
  (cl-letf (((symbol-function 'markdown-ts--latex-block-valid-p)
             (lambda (renamed-node) renamed-node)))
    (should-not
     (assq 'markdown-ts--latex-block-valid-p
           (markdown-ts-appear--private-api-incompatibilities))))
  (markdown-ts-appear-test--with-buffer "**bold**\n"
					(should markdown-ts-appear--advice-installed-p)
					(should-not (markdown-ts-appear--private-api-incompatibilities))))

(ert-deftest markdown-ts-appear-test-bounds-do-not-force-fontification ()
  (markdown-ts-appear-test--with-buffer "**bold**\n"
					(goto-char 3)
					(cl-letf (((symbol-function 'font-lock-ensure)
						   (lambda (&rest _arguments)
						     (ert-fail "Bounds lookup forced fontification"))))
					  (should (equal (markdown-ts-appear--bounds) '(1 . 9))))))

(ert-deftest markdown-ts-appear-test-unload-cleans-global-and-clone-state ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (let ((original-require (symbol-function 'require))
        (markdown-ts-appear--math-cache (make-hash-table :test #'equal))
        (markdown-ts-appear--math-pending (make-hash-table :test #'equal))
        (buffer (generate-new-buffer " *markdown-unload*"))
        clone pending-marker)
    (unwind-protect
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &optional filename noerror)
                     (if (eq feature 'mathjax)
                         t
                       (funcall original-require feature filename noerror))))
                  ((symbol-function 'display-graphic-p)
                   (lambda (&optional _display) t))
                  ((symbol-function 'image-type-available-p)
                   (lambda (_type) t))
                  ((symbol-function 'mathjax-available-p)
                   (lambda () t))
                  ((symbol-function 'buffer-list)
                   (lambda (&optional _frame)
                     (delq nil (list buffer clone)))))
          (with-current-buffer buffer
            (insert "[Emacs](https://www.gnu.org/)\n")
            (let ((markdown-ts-appear-enable-math-preview nil))
              (markdown-ts-mode)
              (markdown-ts-appear-mode 1))
            (markdown-ts-appear--math-enable)
            (font-lock-ensure)
            (setq clone
                  (clone-indirect-buffer " *markdown-unload-clone*" nil))
            (setq pending-marker (copy-marker (point-min)))
            (let ((requests (make-hash-table :test #'equal))
                  (timer (run-at-time 60 nil #'ignore)))
              (puthash 'request
                       (list (list buffer pending-marker
                                   (copy-marker (point-max)) "source" 'key))
                       requests)
              (puthash 'key (list timer 'generation requests)
                       markdown-ts-appear--math-pending)))
          (puthash 'key 'value markdown-ts-appear--math-cache)
          (markdown-ts-appear-unload-function)
          (with-current-buffer buffer
            (should-not markdown-ts-appear-mode)
            (should-not markdown-ts-appear--math-preview-active-p)
            (should-not markdown-ts-appear--setup-p)
            (should-not markdown-ts-appear--math-filter-installed-p))
          (with-current-buffer clone
            (should-not markdown-ts-appear--base-owner)
            (should-not markdown-ts-appear--math-filter-installed-p))
          (should-not (marker-position pending-marker))
          (should (zerop (hash-table-count markdown-ts-appear--math-cache)))
          (should (zerop (hash-table-count markdown-ts-appear--math-pending)))
          (should-not markdown-ts-appear--advice-installed-p))
      (when (buffer-live-p clone)
        (kill-buffer clone))
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      (markdown-ts-appear--math-cancel-pending))))

;;; markdown-ts-appear-test.el ends here
