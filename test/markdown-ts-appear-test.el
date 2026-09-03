;;; markdown-ts-appear-test.el --- Tests for markdown-ts-appear -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; Modifications Copyright (C) 2026 Dzming Li

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Code:

(require 'ert)
(require 'markdown-ts-appear)

(defmacro markdown-ts-appear-test--with-buffer (text &rest body)
  "Create an appear-enabled Markdown buffer containing TEXT, then run BODY."
  (declare (indent 1) (debug t))
  `(with-temp-buffer
     (markdown-ts-mode)
     (buffer-enable-undo)
     (insert ,text)
     (set-buffer-modified-p nil)
     (goto-char (point-min))
     (markdown-ts-appear-mode 1)
     (unwind-protect
         (progn ,@body)
       (when markdown-ts-appear-mode
         (markdown-ts-appear-mode -1)))))

(defun markdown-ts-appear-test--goto (text &optional offset)
  "Move point to TEXT plus OFFSET, returning point."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (+ (match-beginning 0) (or offset 0))))

(defun markdown-ts-appear-test--update-at (text &optional offset)
  "Move into TEXT at OFFSET and update appear rendering."
  (markdown-ts-appear-test--goto text offset)
  (markdown-ts-appear--update))

(ert-deftest markdown-ts-appear-test-requires-markdown-ts-mode ()
  (with-temp-buffer
    (setq buffer-invisibility-spec '(foreign))
    (should-error (markdown-ts-appear-mode 1) :type 'user-error)
    (should-not markdown-ts-appear-mode)
    (should-not markdown-ts-appear--setup-p)
    (should (equal buffer-invisibility-spec '(foreign)))))

(ert-deftest markdown-ts-appear-test-reveals-and-restores-emphasis ()
  (markdown-ts-appear-test--with-buffer "before *word* after\n"
    (markdown-ts-appear-test--update-at "word" 1)
    (let ((region markdown-ts-appear--region))
      (should region)
      (should (= (marker-position (car region)) 8))
      (should (= (marker-position (cdr region)) 14)))
    (should-not (get-text-property 8 'invisible))
    (should-not (get-text-property 13 'invisible))
    (goto-char (point-min))
    (markdown-ts-appear--update)
    (should-not markdown-ts-appear--region)
    (should (eq (get-text-property 8 'invisible)
                'markdown-ts--markup))
    (should (eq (get-text-property 13 'invisible)
                'markdown-ts--markup))))

(ert-deftest markdown-ts-appear-test-reveals-complete-link ()
  (markdown-ts-appear-test--with-buffer "A [label](target) here\n"
    (markdown-ts-appear-test--update-at "label" 2)
    (dolist (position '(3 9 10 17))
      (should-not (get-text-property position 'invisible)))
    (goto-char (point-min))
    (markdown-ts-appear--update)
    (should (eq (get-text-property 3 'invisible)
                'markdown-ts--markup))))

(ert-deftest markdown-ts-appear-test-reveals-code-span ()
  (markdown-ts-appear-test--with-buffer "Use `value` now\n"
    (markdown-ts-appear-test--update-at "value" 2)
    (should-not (get-text-property 5 'invisible))
    (should-not (get-text-property 11 'invisible))))

(ert-deftest markdown-ts-appear-test-reveals-wikilink ()
  (markdown-ts-appear-test--with-buffer "Open [[Page|Alias]] now\n"
    (markdown-ts-appear-test--update-at "Alias" 2)
    (should (equal
             (cons (marker-position (car markdown-ts-appear--region))
                   (marker-position (cdr markdown-ts-appear--region)))
             '(6 . 20)))))

(ert-deftest markdown-ts-appear-test-malformed-inline-remains-editable ()
  (markdown-ts-appear-test--with-buffer "An *unfinished token\n"
    (markdown-ts-appear-test--update-at "unfinished" 2)
    (should markdown-ts-appear--region)
    (should (= (marker-position (car markdown-ts-appear--region)) 4))))

(ert-deftest markdown-ts-appear-test-ignores-fenced-code-blocks ()
  (markdown-ts-appear-test--with-buffer
      (concat (make-string 3 ?`) "elisp\n*code*\n" (make-string 3 ?`) "\n")
    (markdown-ts-appear-test--update-at "code" 2)
    (should-not markdown-ts-appear--region)))

(ert-deftest markdown-ts-appear-test-ignores-inline-latex ()
  (markdown-ts-appear-test--with-buffer "Math $x + y$ here\n"
    (markdown-ts-appear-test--update-at "x + y" 2)
    (should-not markdown-ts-appear--region)))

(ert-deftest markdown-ts-appear-test-does-not-decompose-modern-structure ()
  (with-temp-buffer
    (markdown-ts-mode)
    (insert "# Heading\n\n- item\n\n| A |\n|---|\n| B |\n")
    (markdown-ts-modern-mode 1)
    (markdown-ts-appear-mode 1)
    (unwind-protect
        (progn
          (font-lock-ensure)
          (markdown-ts-appear-test--update-at "item" 2)
          (should-not markdown-ts-appear--region)
          (goto-char (point-min))
          (forward-line 2)
          (should (equal (substring-no-properties
                          (get-text-property (point) 'display))
                         "–"))
          (should (get-text-property (point-min) 'before-string))
          (goto-char (point-min))
          (forward-line 4)
          (should (equal (get-text-property (point) 'display)
                         '(space :width (3)))))
      (markdown-ts-appear-mode -1)
      (markdown-ts-modern-mode -1))))

(ert-deftest markdown-ts-appear-test-coexists-with-modern-inline ()
  (with-temp-buffer
    (markdown-ts-mode)
    (insert "# Heading with *emphasis*\n")
    (markdown-ts-modern-mode 1)
    (markdown-ts-appear-mode 1)
    (unwind-protect
        (progn
          (font-lock-ensure)
          (markdown-ts-appear-test--update-at "emphasis" 2)
          (should markdown-ts-appear--region)
          (should (get-text-property (point-min) 'before-string))
          (markdown-ts-appear-test--goto "*emphasis")
          (should-not (get-text-property (point) 'invisible)))
      (markdown-ts-appear-mode -1)
      (markdown-ts-modern-mode -1))))

(ert-deftest markdown-ts-appear-test-restores-host-state ()
  (with-temp-buffer
    (markdown-ts-mode)
    (setq-local markdown-ts-hide-markup nil)
    (setq buffer-invisibility-spec '(foreign))
    (let ((local-p (local-variable-p 'markdown-ts-hide-markup)))
      (markdown-ts-appear-mode 1)
      (should markdown-ts-hide-markup)
      (should (member 'markdown-ts--markup buffer-invisibility-spec))
      (markdown-ts-appear-mode -1)
      (should-not markdown-ts-hide-markup)
      (should (eq (local-variable-p 'markdown-ts-hide-markup) local-p))
      (should (equal buffer-invisibility-spec '(foreign))))))

(ert-deftest markdown-ts-appear-test-preserves-source-state ()
  (markdown-ts-appear-test--with-buffer "A *word* here\n"
    (let ((source (buffer-string))
          (undo-list buffer-undo-list))
      (markdown-ts-appear-test--update-at "word" 2)
      (goto-char (point-min))
      (markdown-ts-appear--update)
      (should (equal (buffer-string) source))
      (should-not (buffer-modified-p))
      (should (eq buffer-undo-list undo-list)))))

(ert-deftest markdown-ts-appear-test-advice-follows-active-buffers ()
  (let ((first (generate-new-buffer " *appear-first*"))
        (second (generate-new-buffer " *appear-second*")))
    (unwind-protect
        (progn
          (with-current-buffer first
            (markdown-ts-mode)
            (insert "*one*")
            (markdown-ts-appear-mode 1))
          (with-current-buffer second
            (markdown-ts-mode)
            (insert "*two*")
            (markdown-ts-appear-mode 1))
          (should markdown-ts-appear--advice-installed-p)
          (with-current-buffer first
            (markdown-ts-appear-mode -1))
          (should markdown-ts-appear--advice-installed-p)
          (with-current-buffer second
            (markdown-ts-appear-mode -1))
          (should-not markdown-ts-appear--advice-installed-p))
      (when (buffer-live-p first) (kill-buffer first))
      (when (buffer-live-p second) (kill-buffer second))
      (setq markdown-ts-appear--active-buffers nil)
      (markdown-ts-appear--remove-advice))))

(ert-deftest markdown-ts-appear-test-manual-trigger-waits-for-start ()
  (let ((markdown-ts-appear-trigger 'manual))
    (markdown-ts-appear-test--with-buffer "A *word* here\n"
      (should-not (memq #'markdown-ts-appear--update post-command-hook))
      (should-not markdown-ts-appear--region)
      (markdown-ts-appear-test--goto "word" 2)
      (markdown-ts-appear-manual-start)
      (should (memq #'markdown-ts-appear--update post-command-hook))
      (should markdown-ts-appear--region)
      (markdown-ts-appear-manual-stop)
      (should-not (memq #'markdown-ts-appear--update post-command-hook))
      (should-not markdown-ts-appear--region))))

(provide 'markdown-ts-appear-test)

;;; markdown-ts-appear-test.el ends here
