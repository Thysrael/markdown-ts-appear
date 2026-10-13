;;; markdown-ts-appear-compat-test.el --- Native integration contract -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Keep private Markdown dependencies at the integration boundary.  CI runs
;; this with the existing fontifier behavior matrix on each supported Emacs.

;;; Code:
(require 'ert)
(require 'markdown-ts-appear)

(ert-deftest markdown-ts-appear-compat-test-advice-lifetime ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (let ((first (generate-new-buffer " *appear-first*"))
        (second (generate-new-buffer " *appear-second*"))
        (foreign (lambda (&rest _))))
    (unwind-protect
        (progn
          (advice-add 'line-move :before foreign)
          (dolist (buffer (list first second))
            (with-current-buffer buffer
              (insert "**bold**\n")
              (markdown-ts-mode)
              (markdown-ts-appear-mode 1)))
          (with-current-buffer first (markdown-ts-appear-mode -1))
          (should (advice-member-p #'markdown-ts-appear-table--line-move 'line-move))
          (kill-buffer second)
          (dolist (binding markdown-ts-appear--advice-bindings)
            (should-not (advice-member-p (cdr binding) (car binding))))
          (should (advice-member-p foreign 'line-move))
          (should-not (memq #'markdown-ts-appear--appearance-change enable-theme-functions))
          (should-not (memq #'markdown-ts-appear--appearance-change after-setting-font-hook)))
      (advice-remove 'line-move foreign)
      (dolist (buffer (list first second))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(ert-deftest markdown-ts-appear-compat-test-private-dependency-boundary ()
  (let ((root (file-name-directory (locate-library "markdown-ts-appear"))))
    (dolist (file (directory-files root t "\\`markdown-ts-appear.*\\.el\\'"))
      (unless (equal (file-name-nondirectory file) "markdown-ts-appear-compat.el")
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (while t
                (dolist (symbol (flatten-tree (read (current-buffer))))
                  (when (symbolp symbol)
                    (should-not (string-prefix-p "markdown-ts--" (symbol-name symbol))))))
            (end-of-file nil)))))))

(provide 'markdown-ts-appear-compat-test)
;;; markdown-ts-appear-compat-test.el ends here
