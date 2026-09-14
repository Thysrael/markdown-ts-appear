;;; markdown-ts-appear-table-test.el --- Wrapped table tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Integration tests for width-aware tables in `markdown-ts-appear-mode'.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (file-name-directory
             (directory-file-name
              (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path root))

(require 'markdown-ts-appear)

(when (and (getenv "MARKDOWN_TS_APPEAR_REQUIRE_GRAMMARS")
           (not (treesit-ready-p '(markdown markdown-inline))))
  (error "Required Markdown Tree-sitter grammars are unavailable"))

(defmacro markdown-ts-appear-table-test--with-buffer
    (content width &rest body)
  "Create a wrapped-table buffer with CONTENT and WIDTH, then evaluate BODY."
  (declare (indent 2) (debug t))
  `(progn
     (skip-unless (treesit-ready-p '(markdown markdown-inline)))
     (with-temp-buffer
       (insert ,content)
       (let ((markdown-ts-appear-table-style 'wrapped)
             (markdown-ts-inline-images nil)
             (treesit-font-lock-level 3))
         (markdown-ts-mode)
         (goto-char (point-max))
         (unwind-protect
             (progn
               (markdown-ts-appear-mode 1)
               (font-lock-ensure)
               (markdown-ts-appear-table--render ,width)
               ,@body)
           (when markdown-ts-appear-mode
             (markdown-ts-appear-mode -1)))))))

(defun markdown-ts-appear-table-test--overlays ()
  "Return wrapped-table overlays ordered by source position."
  (sort
   (seq-filter
    (lambda (overlay)
      (overlay-get overlay 'markdown-ts-appear-table--wrapped))
    (overlays-in (point-min) (point-max)))
   (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun markdown-ts-appear-table-test--display (overlay)
  "Return OVERLAY's saved wrapped display regardless of reveal state."
  (overlay-get overlay 'markdown-ts-appear-table--display))

(ert-deftest markdown-ts-appear-table-test-wraps-cjk-cell-without-editing-source ()
  (let ((source
         "| 名称 | Description |\n|---|---|\n| 中文 | This is a long cell that should wrap nicely |\n"))
    (markdown-ts-appear-table-test--with-buffer source 32
      (let ((overlays (markdown-ts-appear-table-test--overlays)))
        (should (= 3 (length overlays)))
        (should
         (equal
          (substring-no-properties
           (markdown-ts-appear-table-test--display (nth 2 overlays)))
          (concat "│ 中文 │ This is a long cell   │\n"
                  "│      │ that should wrap      │\n"
                  "│      │ nicely                │\n")))
        (should (equal (buffer-substring-no-properties
                        (point-min) (point-max))
                       source))))))

(ert-deftest markdown-ts-appear-table-test-preserves-inline-rendering ()
  (markdown-ts-appear-table-test--with-buffer
      (concat "| A | Details |\n"
              "|---|---|\n"
              "| x | **bold text** and [link](https://example.com) |\n")
      26
    (let* ((overlay (nth 2 (markdown-ts-appear-table-test--overlays)))
           (display (markdown-ts-appear-table-test--display overlay))
           (bold (string-match "bold" display))
           (link (string-match "link" display)))
      (should (equal (substring-no-properties display)
                     "│ x │ bold text and link │\n"))
      (should (memq 'bold (get-text-property bold 'face display)))
      (should (memq 'link (get-text-property link 'face display)))
      (should (equal (get-text-property link 'help-echo display)
                     "https://example.com")))))

(ert-deftest markdown-ts-appear-table-test-supports-missing-edge-pipes ()
  (markdown-ts-appear-table-test--with-buffer
      (concat "Element | 状态\n"
              "--------|-------\n"
              "Heading | This is a long status value\n")
      28
    (let ((overlays (markdown-ts-appear-table-test--overlays)))
      (should (= 3 (length overlays)))
      (should
       (equal
        (substring-no-properties
         (markdown-ts-appear-table-test--display (nth 2 overlays)))
        (concat "│ Heading │ This is a long │\n"
                "│         │ status value   │\n"))))))

(ert-deftest markdown-ts-appear-table-test-measures-emoji-and-escaped-pipes ()
  (markdown-ts-appear-table-test--with-buffer
      (concat "| Item | Note |\n"
              "|---|---|\n"
              "| 👩‍💻 | 中文😀 escaped \\| pipe wraps here |\n")
      27
    (let* ((overlay (nth 2 (markdown-ts-appear-table-test--overlays)))
           (display (markdown-ts-appear-table-test--display overlay)))
      (should (string-match-p "中文😀 escaped |" display))
      (dolist (line (string-split display "\n" t))
        (should (= 27 (string-width line)))))))

(ert-deftest markdown-ts-appear-table-test-reveals-and-restores-source-row ()
  (markdown-ts-appear-table-test--with-buffer
      (concat "| A | Description |\n"
              "|---|---|\n"
              "| x | This is a long cell that should wrap |\n\n"
              "after\n")
      25
    (goto-char (point-min))
    (search-forward "long")
    (run-hooks 'post-command-hook)
    (let ((overlay (nth 2 (markdown-ts-appear-table-test--overlays)))
          (source (buffer-string)))
      (should-not (overlay-get overlay 'display))
      (should (equal source (buffer-string)))
      (markdown-ts-appear-stop)
      (should (equal (overlay-get overlay 'display)
                     (markdown-ts-appear-table-test--display overlay)))
      (goto-char (point-min))
      (search-forward "long")
      (markdown-ts-appear-start)
      (should-not (overlay-get overlay 'display)))))

(ert-deftest markdown-ts-appear-table-test-rebuilds-after-edit ()
  (markdown-ts-appear-table-test--with-buffer
      "| A | Description |\n|---|---|\n| x | before edit |\n\nafter\n"
      24
    (goto-char (point-min))
    (search-forward "before")
    (delete-region (match-beginning 0) (match-end 0))
    (insert "updated long value")
    (should markdown-ts-appear-table--dirty)
    (should-not markdown-ts-appear-table--overlays)
    (run-hooks 'post-command-hook)
    (let* ((overlay (nth 2 (markdown-ts-appear-table-test--overlays)))
           (display (markdown-ts-appear-table-test--display overlay)))
      (should-not markdown-ts-appear-table--dirty)
      (should (string-match-p "updated" display))
      (should (string-match-p "long value" display)))))

(ert-deftest markdown-ts-appear-table-test-disable-cleans-state ()
  (markdown-ts-appear-table-test--with-buffer
      "| A | B |\n|---|---|\n| x | y |\n"
      20
    (should markdown-ts-appear-table--overlays)
    (should (memq #'markdown-ts-appear-table--post-command post-command-hook))
    (should (memq #'markdown-ts-appear-table--after-change
                  after-change-functions))
    (markdown-ts-appear-mode -1)
    (should-not markdown-ts-appear-table--overlays)
    (should-not (memq #'markdown-ts-appear-table--post-command
                      post-command-hook))
    (should-not (memq #'markdown-ts-appear-table--after-change
                      after-change-functions))))

(ert-deftest markdown-ts-appear-table-test-indirect-buffer-keeps-base-overlays ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (let ((base (generate-new-buffer " *markdown-ts-appear-table-base*"))
        indirect)
    (unwind-protect
        (with-current-buffer base
          (insert "| A | Description |\n|---|---|\n| x | a long value to wrap |\n")
          (markdown-ts-mode)
          (setq-local markdown-ts-appear-table-style 'wrapped)
          (setq-local markdown-ts-inline-images nil)
          (goto-char (point-max))
          (markdown-ts-appear-mode 1)
          (font-lock-ensure)
          (markdown-ts-appear-table--render 22)
          (let ((base-overlays (copy-sequence
                                markdown-ts-appear-table--overlays)))
            (should (= 3 (length base-overlays)))
            (setq indirect
                  (clone-indirect-buffer
                   " *markdown-ts-appear-table-indirect*" nil))
            (with-current-buffer indirect
              (should-not markdown-ts-appear-mode)
              (should-not markdown-ts-appear-table--overlays))
            (should (cl-every #'overlay-buffer base-overlays))))
      (when (buffer-live-p indirect)
        (kill-buffer indirect))
      (when (buffer-live-p base)
        (with-current-buffer base
          (when markdown-ts-appear-mode
            (markdown-ts-appear-mode -1)))
        (kill-buffer base)))))

(ert-deftest markdown-ts-appear-table-test-renders-each-window-at-its-width ()
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (let ((buffer (generate-new-buffer " *markdown-ts-appear-table-windows*"))
        (configuration (current-window-configuration)))
    (unwind-protect
        (progn
          (switch-to-buffer buffer)
          (insert (concat "| A | Description |\n"
                          "|---|---|\n"
                          "| x | a long value that wraps per window |\n"))
          (markdown-ts-mode)
          (setq-local markdown-ts-appear-table-style 'wrapped)
          (setq-local markdown-ts-inline-images nil)
          (goto-char (point-max))
          (markdown-ts-appear-mode 1)
          (font-lock-ensure)
          (let ((other (condition-case nil
                           (split-window-right)
                         (error nil))))
            (skip-unless other)
            (set-window-buffer other buffer)
            (markdown-ts-appear-table--render)
            (let ((windows (get-buffer-window-list buffer nil t)))
              (should (= 2 (length windows)))
              (should (= 6 (length markdown-ts-appear-table--overlays)))
              (dolist (window windows)
                (let ((overlays
                       (seq-filter
                        (lambda (overlay)
                          (eq window (overlay-get overlay 'window)))
                        markdown-ts-appear-table--overlays))
                      (width (markdown-ts-appear-table--window-width window)))
                  (should (= 3 (length overlays)))
                  (dolist (overlay overlays)
                    (dolist (line
                             (string-split
                              (markdown-ts-appear-table-test--display overlay)
                              "\n" t))
                      (should (<= (string-width line) width)))))))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when markdown-ts-appear-mode
            (markdown-ts-appear-mode -1))))
      (set-window-configuration configuration)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'markdown-ts-appear-table-test)
;;; markdown-ts-appear-table-test.el ends here
