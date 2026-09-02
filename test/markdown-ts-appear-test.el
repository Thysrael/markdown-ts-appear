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
         (markdown-ts-appear-mode 1)
         (font-lock-ensure)
         ,@body))))

(ert-deftest markdown-ts-appear-test-reveal-and-restore ()
  (markdown-ts-appear-test--with-buffer "**bold** plain\n"
					(goto-char 3)
					(markdown-ts-appear--update)
					(should-not (get-text-property (point-min) 'invisible))
					(goto-char 12)
					(markdown-ts-appear--update)
					(should (get-text-property (point-min) 'invisible))))

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

(ert-deftest markdown-ts-appear-test-wikilink-boundaries ()
  (markdown-ts-appear-test--with-buffer "[[Markdown|an alias]]\n"
					(goto-char (point-min))
					(should (equal (markdown-ts-appear--bounds)
						       (cons (point-min) (1- (point-max)))))
					(goto-char (- (point-max) 2))
					(should (equal (markdown-ts-appear--bounds)
						       (cons (point-min) (1- (point-max)))))))

(ert-deftest markdown-ts-appear-test-skips-code-block ()
  (markdown-ts-appear-test--with-buffer
   "```elisp\n(message \"hi\")\n```\n"
   (goto-char (point-min))
   (search-forward "message")
   (should-not (markdown-ts-appear--bounds))))

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

(ert-deftest markdown-ts-appear-test-cleans-up-before-major-mode-change ()
  (markdown-ts-appear-test--with-buffer "[Emacs](https://www.gnu.org/)\n"
					(should markdown-ts-appear--setup-p)
					(text-mode)
					(should-not
					 (seq-some
					  (lambda (overlay)
					    (or (overlay-get overlay 'markdown-ts-appear--link-icon)
						(overlay-get overlay 'markdown-ts-appear--image-label)
						(overlay-get overlay 'markdown-ts-appear--math-alignment)))
					  (overlays-in (point-min) (point-max))))))

(ert-deftest markdown-ts-appear-test-detaches-indirect-clone ()
  (markdown-ts-appear-test--with-buffer "**bold** plain\n"
					(goto-char 3)
					(markdown-ts-appear--update)
					(let ((region markdown-ts-appear--region)
					      (clone (clone-indirect-buffer " *markdown-appear-clone*" nil)))
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
        (puthash 'key (cons timer requests)
                 markdown-ts-appear--math-pending)
        (markdown-ts-appear--math-cancel-pending)
        (should (zerop (hash-table-count markdown-ts-appear--math-pending)))
        (should-not (marker-position beg-marker))
        (should-not (marker-position end-marker))))))

(ert-deftest markdown-ts-appear-test-caches-math-image ()
  (let ((markdown-ts-appear--math-cache (make-hash-table :test #'equal))
        (calls 0))
    (cl-letf (((symbol-function 'markdown-ts-appear--math-image)
               (lambda (_svg &optional _scale)
                 (setq calls (1+ calls))
                 (list 'image calls))))
      (markdown-ts-appear--math-finish-render
       '(nil 1.1 "x") '((svg . "<svg height=\"1\"></svg>")))
      (let ((data (gethash '(nil 1.1 "x")
                           markdown-ts-appear--math-cache)))
        (should (= calls 1))
        (should (equal (alist-get 'markdown-ts-appear--math-image data)
                       '(image 1)))))))

(ert-deftest markdown-ts-appear-test-clears-folded-pending-result ()
  (with-temp-buffer
    (insert "$x$")
    (let* ((markdown-ts-appear-math-preview-mode t)
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

(ert-deftest markdown-ts-appear-test-standalone-math-mode-cleans-up ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (with-temp-buffer
    (markdown-ts-mode)
    (let ((original-require (symbol-function 'require)))
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
        (markdown-ts-appear-math-preview-mode 1)
        (should markdown-ts-appear-math-preview-mode)
        (should markdown-ts-appear--math-filter-installed-p)
        (with-suppressed-warnings ((obsolete outline-view-change-hook))
          (should (memq #'markdown-ts-appear--math-outline-view-change
                        outline-view-change-hook)))
        (markdown-ts-appear-math-preview-mode -1)
        (should-not markdown-ts-appear--math-filter-installed-p)
        (should-not (memq 'markdown-ts-appear--math-state
                          font-lock-extra-managed-props))))))

;;; markdown-ts-appear-test.el ends here
