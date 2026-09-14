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

(defun markdown-ts-appear-table-test--row-overlays-at (position &optional window)
  "Return overlays for the source row at POSITION, optionally in WINDOW."
  (sort
   (seq-filter
    (lambda (overlay)
      (and (<= (overlay-get overlay 'markdown-ts-appear-table--row-beg)
               position)
           (< position
              (overlay-get overlay 'markdown-ts-appear-table--row-end))
           (or (null window) (eq window (overlay-get overlay 'window)))))
    (markdown-ts-appear-table-test--overlays))
   (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun markdown-ts-appear-table-test--row-display-at (position &optional window)
  "Return the complete rendered row at POSITION, optionally in WINDOW."
  (mapconcat #'markdown-ts-appear-table-test--display
             (markdown-ts-appear-table-test--row-overlays-at position window)
             ""))

(defun markdown-ts-appear-table-test--row-count (&optional overlays)
  "Return the source-row count represented by OVERLAYS."
  (length
   (delete-dups
    (mapcar
     (lambda (overlay)
       (overlay-get overlay 'markdown-ts-appear-table--row-beg))
     (or overlays (markdown-ts-appear-table-test--overlays))))))

(defun markdown-ts-appear-table-test--cursor-overlays ()
  "Return interactive row overlays ordered by source position."
  (sort (copy-sequence markdown-ts-appear-table--cursor-overlays)
        (lambda (a b) (< (overlay-start a) (overlay-start b)))))

(defun markdown-ts-appear-table-test--cursor-character (overlay)
  "Return the character actually carrying OVERLAY's cursor."
  (let* ((display (overlay-get overlay 'display))
         (offset (text-property-any 0 (length display) 'cursor t display)))
    (substring display offset (1+ offset))))

(ert-deftest markdown-ts-appear-table-test-wraps-cjk-cell-without-editing-source ()
  (let ((source
         "| 名称 | Description |\n|---|---|\n| 中文 | This is a long cell that should wrap nicely |\n"))
    (markdown-ts-appear-table-test--with-buffer source 32
      (let ((overlays (markdown-ts-appear-table-test--overlays)))
        (should (= 3 (markdown-ts-appear-table-test--row-count overlays)))
        (goto-char (point-min))
        (search-forward "中文")
        (should
         (equal
          (substring-no-properties
           (markdown-ts-appear-table-test--row-display-at (point)))
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
    (goto-char (point-min))
    (search-forward "bold")
    (let* ((display (markdown-ts-appear-table-test--row-display-at (point)))
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
      (should (= 3 (markdown-ts-appear-table-test--row-count overlays)))
      (goto-char (point-min))
      (search-forward "Heading")
      (should
       (equal
        (substring-no-properties
         (markdown-ts-appear-table-test--row-display-at (point)))
        (concat "│ Heading │ This is a long │\n"
                "│         │ status value   │\n"))))))

(ert-deftest markdown-ts-appear-table-test-measures-emoji-and-escaped-pipes ()
  (markdown-ts-appear-table-test--with-buffer
      (concat "| Item | Note |\n"
              "|---|---|\n"
              "| 👩‍💻 | 中文😀 escaped \\| pipe wraps here |\n")
      27
    (goto-char (point-min))
    (search-forward "escaped")
    (let ((display (markdown-ts-appear-table-test--row-display-at (point))))
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
    (let ((row-overlays
           (markdown-ts-appear-table-test--row-overlays-at (point)))
          (source (buffer-string)))
      (should (= 1 (length row-overlays)))
      (should-not (seq-some (lambda (overlay) (overlay-get overlay 'display))
                            row-overlays))
      (should (equal source (buffer-string)))
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (markdown-ts-appear-stop)
        (dolist (overlay row-overlays)
          (should (eq (overlay-get overlay 'display)
                      (markdown-ts-appear-table-test--display overlay))))
        (let ((cursor-overlays
               (markdown-ts-appear-table-test--cursor-overlays)))
          (should (= 3 (length cursor-overlays)))
          (let* ((anchor (cadr cursor-overlays))
                 (display (overlay-get anchor 'display)))
            (should (= (point) (overlay-start anchor)))
            (should (= 1 (- (overlay-end anchor) (overlay-start anchor))))
            (should (text-property-any 0 (length display) 'cursor t display))))
        (goto-char (overlay-start (car row-overlays)))
        (let ((position (point)))
          (local-set-key (kbd "C-f") #'forward-char)
          (dotimes (_ 5)
            (execute-kbd-macro (kbd "C-f")))
          (should (= (+ 5 position) (point))))
        (goto-char (point-min))
        (search-forward "long")
        (markdown-ts-appear-start)
        (should-not markdown-ts-appear-table--cursor-overlays)
        (should-not
         (seq-some (lambda (overlay) (overlay-get overlay 'display))
                   row-overlays))))))

(ert-deftest markdown-ts-appear-table-test-motion-does-not-rebuild-tables ()
  (markdown-ts-appear-table-test--with-buffer
      (concat "| A | Description |\n"
              "|---|---|\n"
              "| x | This is a long cell that should wrap |\n")
      25
    (markdown-ts-appear-stop)
    (goto-char (point-min))
    (search-forward "long")
    (let* ((row-overlays
            (markdown-ts-appear-table-test--row-overlays-at (point)))
           (beg (overlay-get (car row-overlays)
                             'markdown-ts-appear-table--row-beg))
           (end (overlay-get (car row-overlays)
                             'markdown-ts-appear-table--row-end))
           (renders 0))
      (save-window-excursion
        (switch-to-buffer (current-buffer))
        (cl-letf (((symbol-function 'markdown-ts-appear-table--render)
                   (lambda (&rest _) (setq renders (1+ renders)))))
          (run-hooks 'post-command-hook)
          (let ((cursor-overlays
                 (markdown-ts-appear-table-test--cursor-overlays)))
            (cl-letf (((symbol-function 'markdown-ts-appear-table--source-map)
                       (lambda (&rest _) (ert-fail "Motion rebuilt the source map")))
                      ((symbol-function 'markdown-ts-appear-table--tables)
                       (lambda (&rest _) (ert-fail "Motion scanned the document"))))
              (dotimes (offset (- end beg))
                (goto-char (+ beg offset))
                (run-hooks 'post-command-hook)
                (should (equal cursor-overlays
                               (markdown-ts-appear-table-test--cursor-overlays)))))))
      (should (= 0 renders))))))

(ert-deftest markdown-ts-appear-table-test-exact-source-through-inline-markup ()
  (markdown-ts-appear-table-test--with-buffer
      "| A | B |\n|---|---|\n| **repeat** repeat | [repeat](repeat) `repeat` \\| 中文😀 |\n"
      24
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (forward-line 2)
      (markdown-ts-appear-stop)
      (let* ((row (car (markdown-ts-appear-table-test--row-overlays-at (point))))
             (display (markdown-ts-appear-table-test--display row))
             (source (buffer-substring-no-properties (point-min) (point-max))))
        (dotimes (index (length display))
          (when-let* ((position (get-text-property
                                index 'markdown-ts-appear-table--source display)))
            (goto-char position)
            (markdown-ts-appear-table--post-command)
            (let ((anchor (cadr markdown-ts-appear-table--cursor-overlays)))
              (should (or (= (char-after) (aref display index))
                          (and (= (char-after) ?|) (= (aref display index) ?│))))
              (should (= position (overlay-start anchor)))
              (should (string-search
                       (string (aref display index))
                       (markdown-ts-appear-table-test--cursor-character anchor)))
              (markdown-ts-appear-start)
              (should (= position (point)))
              (should-not (overlay-get row 'display))
              (markdown-ts-appear-stop)
              (should (= position (point))))))
        (should (equal source (buffer-substring-no-properties (point-min) (point-max))))))))

(ert-deftest markdown-ts-appear-table-test-space-retains-insertion-boundary ()
  (markdown-ts-appear-table-test--with-buffer
      "| A | B |\n|---|---|\n| repeated words | repeated words |\n"
      50
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (search-forward "repeated")
      (markdown-ts-appear-stop)
      (let* ((anchor (cadr markdown-ts-appear-table--cursor-overlays))
             (display (overlay-get anchor 'display))
             (offset (text-property-any 0 (length display) 'cursor t display)))
        (should (equal " " (markdown-ts-appear-table-test--cursor-character anchor)))
        (should (string-suffix-p "repeated" (substring display 0 offset)))
        (should (string-prefix-p "words" (substring display (1+ offset))))))))

(ert-deftest markdown-ts-appear-table-test-escaped-pipe-retains-face-and-source ()
  (markdown-ts-appear-table-test--with-buffer
      "| A | B |\n|---|---|\n| x | **a\\|b** `c\\|d` |\n"
      40
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (search-forward "a\\|")
      (backward-char)
      (markdown-ts-appear-stop)
      (let ((display (markdown-ts-appear-table-test--cursor-character
                      (cadr markdown-ts-appear-table--cursor-overlays))))
        (should (equal "|" display))
        (should (memq 'bold (ensure-list (get-text-property 0 'face display)))))
      (search-forward "c\\|")
      (backward-char)
      (markdown-ts-appear-table--post-command)
      (let ((display (markdown-ts-appear-table-test--cursor-character
                      (cadr markdown-ts-appear-table--cursor-overlays))))
        (should (equal "|" display))
        (should (memq 'markdown-table-wrap-pretty-code-face
                      (ensure-list (get-text-property 0 'face display))))))))

(ert-deftest markdown-ts-appear-table-test-trailing-newline-is-outside-row ()
  (markdown-ts-appear-table-test--with-buffer
      "| A | B |\n|---|---|\n| x | y |\n"
      20
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (goto-char (point-min))
      (markdown-ts-appear-stop)
      (should markdown-ts-appear-table--cursor-overlays)
      (goto-char (point-max))
      (markdown-ts-appear-table--post-command)
      (should-not markdown-ts-appear-table--cursor-overlays))))

(ert-deftest markdown-ts-appear-table-test-window-migration-and-layout-reuse ()
  (markdown-ts-appear-table-test--with-buffer
      "| A | B |\n|---|---|\n| repeated words | 中文 long words that wrap |\n"
      25
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (let ((first (selected-window))
            (second (split-window-right))
            (cell-function (symbol-function 'markdown-ts-appear-table--cell))
            (calls 0)
            previous)
        (set-window-buffer second (current-buffer))
        (markdown-ts-appear-stop)
        (cl-letf (((symbol-function 'markdown-ts-appear-table--cell)
                   (lambda (node)
                     (cl-incf calls)
                     (funcall cell-function node))))
          (markdown-ts-appear-table--render))
        (should (= 4 calls))
        (dolist (window (list first second first))
          (select-window window)
          (goto-char (point-min))
          (search-forward "中文")
          (backward-char 2)
          (markdown-ts-appear-table--selection-change window)
          (when previous
            (should-not (seq-some #'overlay-buffer previous)))
          (setq previous (copy-sequence markdown-ts-appear-table--cursor-overlays))
          (should (= 3 (length previous)))
          (should (cl-every (lambda (overlay) (eq window (overlay-get overlay 'window)))
                            previous))
          (should (equal "中" (markdown-ts-appear-table-test--cursor-character
                                (cadr previous)))))
        (markdown-ts-appear-table--render 20)
        (should-not (seq-some #'overlay-buffer previous))
        (should (equal "中" (markdown-ts-appear-table-test--cursor-character
                              (cadr markdown-ts-appear-table--cursor-overlays))))))))

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
    (let ((display (markdown-ts-appear-table-test--row-display-at (point))))
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
    (should (memq #'markdown-ts-appear-table--selection-change
                  window-selection-change-functions))
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (markdown-ts-appear-stop)
      (goto-char (point-min))
      (run-hooks 'post-command-hook)
      (should markdown-ts-appear-table--cursor-overlays)
      (markdown-ts-appear-mode -1)
      (should-not markdown-ts-appear-table--overlays)
      (should-not markdown-ts-appear-table--cursor-overlays)
      (should-not (memq #'markdown-ts-appear-table--post-command
                        post-command-hook))
      (should-not (memq #'markdown-ts-appear-table--after-change
                        after-change-functions))
      (should-not (memq #'markdown-ts-appear-table--selection-change
                        window-selection-change-functions)))))

(ert-deftest markdown-ts-appear-table-test-deselect-cleans-cursor-overlays ()
  (markdown-ts-appear-table-test--with-buffer
      "| A | B |\n|---|---|\n| x | y |\n"
      20
    (save-window-excursion
      (let ((table-buffer (current-buffer))
            (other-buffer (generate-new-buffer
                           " *markdown-ts-appear-other*")))
        (unwind-protect
            (progn
              (switch-to-buffer table-buffer)
              (markdown-ts-appear-stop)
              (goto-char (point-min))
              (run-hooks 'post-command-hook)
              (should markdown-ts-appear-table--cursor-overlays)
              (switch-to-buffer other-buffer)
              (with-current-buffer table-buffer
                (markdown-ts-appear-table--selection-change
                 (selected-window))
                (should-not markdown-ts-appear-table--cursor-overlays)))
          (kill-buffer other-buffer))))))

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
            (should (= 3 (markdown-ts-appear-table-test--row-count
                          base-overlays)))
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
              (dolist (window windows)
                (let ((overlays
                       (seq-filter
                        (lambda (overlay)
                          (eq window (overlay-get overlay 'window)))
                        markdown-ts-appear-table--overlays))
                      (width (markdown-ts-appear-table--window-width window)))
                  (should (= 3 (markdown-ts-appear-table-test--row-count
                                overlays)))
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
