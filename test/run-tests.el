;;; run-tests.el --- Batch and graphical test entry point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; CI runs batch tests with a real SVG-capable build where available.
;; Omit --batch to include graphical interaction tests locally.

;;; Code:
(require 'package)
(package-initialize)
(setq load-prefer-newer t native-comp-jit-compilation nil)
(let ((directory (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (directory-file-name (file-name-directory
                                               (directory-file-name directory))))
  (dolist (file '("markdown-ts-appear-test.el" "markdown-ts-appear-compat-test.el"
                  "markdown-ts-appear-math-test.el"
                  "markdown-ts-appear-mathjax-independent-test.el"
                  "markdown-ts-appear-mathjax-kitty-test.el"
                  "markdown-ts-appear-table-test.el"
                  "markdown-ts-appear-table-navigation-test.el"
                  "markdown-ts-appear-table-math-test.el"
                  "markdown-ts-appear-image-test.el" "markdown-ts-appear-mermaid-test.el"
                  "markdown-ts-appear-diagram-test.el"))
    (load (expand-file-name file directory) nil t)))

(defun markdown-ts-appear-run-tests ()
  "Run the suite and exit with its status, printing graphical-session results."
  (let ((code 1))
    (condition-case err
        (let ((stats (ert-run-tests-batch t)))
          (setq code (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))
      (error (message "Test runner failed: %s" (error-message-string err))))
    (unless noninteractive
      (with-current-buffer "*Messages*"
        (princ (buffer-string) #'external-debugging-output)))
    (kill-emacs code)))

(if noninteractive
    (markdown-ts-appear-run-tests)
  (run-at-time 1 nil #'markdown-ts-appear-run-tests))
;;; run-tests.el ends here
