;;; markdown-ts-appear-table-redisplay-test.el --- Real display tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Run with python3 test/table-redisplay.py.  The driver checks the actual
;; terminal cursor; posn-at-point reports the start of before-string instead.

;;; Code:

(require 'ert)
(require 'markdown-ts-appear)

(defvar markdown-ts-appear-table-redisplay--checks 0
  "Number of cursor checkpoints emitted to the PTY driver.")

(defun markdown-ts-appear-table-redisplay--cursor (expected)
  "Ask the PTY driver to check the actual terminal cursor against EXPECTED."
  (cl-incf markdown-ts-appear-table-redisplay--checks)
  (let ((edges (window-inside-edges)))
    (send-string-to-terminal
     (format "\e]777;cursor;%d;%d\a"
             (+ (nth 1 edges) (cdr expected))
             (+ (car edges) (car expected))))))

(defconst markdown-ts-appear-table-redisplay--example
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "table-state-example.md"
                       (file-name-directory (or load-file-name buffer-file-name))))
    (buffer-string))
  "Original three-column table used to reproduce insert-state cursor drift.")

(ert-deftest markdown-ts-appear-table-redisplay-source-position ()
  (skip-unless (not noninteractive))
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert markdown-ts-appear-table-redisplay--example)
      (markdown-ts-mode)
      (setq-local markdown-ts-appear-table-style 'wrapped)
      (markdown-ts-appear-mode 1)
      (markdown-ts-appear-stop)
      (unwind-protect
          (dolist (width '(34 56 85))
            (markdown-ts-appear-table--render width)
            (goto-char (point-min))
            (search-forward "| 5-7 |")
            (markdown-ts-appear-table--post-command)
            (let* ((row (seq-find
                         (lambda (overlay)
                           (overlay-get overlay 'markdown-ts-appear-table--wrapped))
                         (overlays-at (point))))
                   (beg (overlay-start row))
                   (display (overlay-get row 'markdown-ts-appear-table--display)))
              (dotimes (index (length display))
                (when-let* ((source (get-text-property
                                    index 'markdown-ts-appear-table--source display)))
                  (goto-char source)
                  (markdown-ts-appear-table--post-command)
                  (set-window-start (selected-window) beg)
                  (redisplay t)
                  (let* ((prefix (substring-no-properties display 0 index))
                         (lines (split-string prefix "\n"))
                         (expected (cons (string-width (car (last lines)))
                                         (1- (length lines)))))
                    (markdown-ts-appear-table-redisplay--cursor expected)
                    (should (or (= (char-after) (aref display index))
                                (and (= (char-after) ?|) (= (aref display index) ?│))))
                    (markdown-ts-appear-start)
                    (should (= source (point)))
                    (redisplay t)
                    (markdown-ts-appear-stop)
                    (should (= source (point)))
                    (set-window-start (selected-window) beg)
                    (redisplay t)
                    (markdown-ts-appear-table-redisplay--cursor expected))))))
        (markdown-ts-appear-mode -1)))))

(ert-deftest markdown-ts-appear-table-redisplay-evil-insert ()
  (skip-unless (not noninteractive))
  (require 'evil)
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert markdown-ts-appear-table-redisplay--example)
      (markdown-ts-mode)
      (setq-local markdown-ts-appear-table-style 'wrapped)
      (markdown-ts-appear-mode 1)
      (evil-local-mode 1)
      (add-hook 'evil-insert-state-entry-hook #'markdown-ts-appear-start nil t)
      (add-hook 'evil-insert-state-exit-hook #'markdown-ts-appear-stop nil t)
      (evil-normal-state)
      (markdown-ts-appear-stop)
      (unwind-protect
          (dolist (key '("i" "a"))
            (markdown-ts-appear-table--render 56)
            (goto-char (point-min))
            (search-forward "extended")
            (goto-char (+ 2 (match-beginning 0)))
            (markdown-ts-appear-table--post-command)
            (let ((position (point))
                  (source (buffer-substring-no-properties (point-min) (point-max))))
              (execute-kbd-macro key)
              (should (eq evil-state 'insert))
              (let ((insertion (+ position (if (equal key "a") 1 0))))
                (should (= insertion (point)))
                (execute-kbd-macro "Q")
                (should (equal
                         (buffer-substring-no-properties (point-min) (point-max))
                         (concat (substring source 0 (1- insertion)) "Q"
                                 (substring source (1- insertion)))))
                (execute-kbd-macro (kbd "<escape>"))
                (should (eq evil-state 'normal))
                (should (= insertion (point)))
                (let* ((display (overlay-get
                                 (cadr markdown-ts-appear-table--cursor-overlays)
                                 'display))
                       (offset (text-property-any 0 (length display) 'cursor t display)))
                  (should (= ?Q (aref display offset)))))
              ;; Return to the fixture for the next independent insertion.
              (delete-region (point) (1+ (point)))
              (markdown-ts-appear-table--post-command)))
        (markdown-ts-appear-mode -1)
        (evil-local-mode -1)))))

(ert-deftest markdown-ts-appear-table-redisplay-unicode-without-final-newline ()
  (skip-unless (not noninteractive))
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "| A | B |\n|---|---|\n| 👩‍💻 中文 | 😀 é long words that wrap several times |")
      (markdown-ts-mode)
      (setq-local markdown-ts-appear-table-style 'wrapped)
      (markdown-ts-appear-mode 1)
      (markdown-ts-appear-stop)
      (unwind-protect
          (progn
            (markdown-ts-appear-table--render 26)
            (dolist (token '("👩‍💻" "中文" "😀" "é" "times"))
              (goto-char (point-min))
              (search-forward token)
              (goto-char (match-beginning 0))
              (markdown-ts-appear-table--post-command)
              (let* ((row (nth 3 markdown-ts-appear-table--cursor-row))
                     (display (overlay-get row 'markdown-ts-appear-table--display))
                     (index (string-match (regexp-quote token) display))
                     (lines (split-string (substring display 0 index) "\n")))
                (set-window-start (selected-window) (overlay-start row))
                (redisplay t)
                (markdown-ts-appear-table-redisplay--cursor
                 (cons (string-width (car (last lines))) (1- (length lines))))))
            (goto-char (point-max))
            (markdown-ts-appear-table--post-command)
            (redisplay t)
            (let* ((row (nth 3 markdown-ts-appear-table--cursor-row))
                   (display (overlay-get row 'markdown-ts-appear-table--display))
                   (lines (split-string display "\n")))
              (markdown-ts-appear-table-redisplay--cursor
               (cons (string-width (car (last lines))) (1- (length lines))))))
        (markdown-ts-appear-mode -1)))))

(defun markdown-ts-appear-table-redisplay-run ()
  "Run real display tests and exit with their result."
  (let ((stats (ert-run-tests "table-redisplay" #'ignore)))
    (dolist (test (ert-select-tests "table-redisplay" t))
      (let ((result (ert-test-most-recent-result test)))
        (when (ert-test-failed-p result)
          (message "FAILURE %S" (ert-test-failed-condition result)))))
    (message "REDISPLAY unexpected=%d" (ert-stats-completed-unexpected stats))
    (sit-for 0.1)
    (send-string-to-terminal
     (format "\e]777;done;%d\a" markdown-ts-appear-table-redisplay--checks))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(provide 'markdown-ts-appear-table-redisplay-test)
;;; markdown-ts-appear-table-redisplay-test.el ends here
