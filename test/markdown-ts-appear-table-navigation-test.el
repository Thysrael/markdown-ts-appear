;;; markdown-ts-appear-table-navigation-test.el --- Rendered table navigation -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Hidden source is cursor-intangible only while a table row is rendered.
;; Exercise ordinary commands and Evil through the same redisplay hooks.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'markdown-ts-appear)
(defvar evil-mode-buffers)

(defconst markdown-ts-appear-navigation-test--source
  "Before `hi` after.\n\n| 参数 | 含义 |\n|------|------|\n| `-t` | 加密方法 rsa ed25519 |\n| `-C` | 注释 邮箱 |\n| `-f` | 文件名 路径 |\n\nAfter table.\n")

(defmacro markdown-ts-appear-navigation-test--buffer (evil &rest body)
  "Run BODY with a rendered table; enable Evil when EVIL is non-nil."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (treesit-ready-p '(markdown markdown-inline)))
     (when ,evil (skip-unless (require 'evil nil t)))
     (let ((markdown-ts-appear-enable-math-preview nil)
           (markdown-ts-appear-diagram-renderers nil)
           (markdown-ts-inline-images nil)
           (evil-mode-buffers nil))
       (save-window-excursion
         (with-temp-buffer
           (switch-to-buffer (current-buffer))
           (insert markdown-ts-appear-navigation-test--source)
           (markdown-ts-mode)
           (goto-char (point-max))
           (unwind-protect
               (progn
                 (markdown-ts-appear-mode 1)
                 (when ,evil
                   (evil-local-mode 1)
                   (evil-normal-state))
                 (markdown-ts-appear-stop)
                 (font-lock-ensure)
                 ,@body)
             (markdown-ts-appear-mode -1)
             (when ,evil (evil-local-mode -1))))))))

(defun markdown-ts-appear-navigation-test--redisplay ()
  "Run the native and table hooks normally called before redisplay."
  (run-hook-with-args 'pre-redisplay-functions (selected-window)))

(defun markdown-ts-appear-navigation-test--at (text offset)
  "Move OFFSET characters from the start of TEXT, then synchronize display."
  (goto-char (point-min))
  (search-forward text)
  (goto-char (+ (- (point) (length text)) offset))
  (run-hooks 'post-command-hook)
  (markdown-ts-appear-navigation-test--redisplay))

(defun markdown-ts-appear-navigation-test--key (key)
  "Execute KEY and let native cursor adjustment run before checking it."
  (execute-kbd-macro (kbd key))
  (markdown-ts-appear-navigation-test--redisplay))

(defun markdown-ts-appear-navigation-test--cursor ()
  "Return the rendered cursor's row, visual line and offset."
  (let ((row markdown-ts-appear-table--cursor-row)
        (anchor (cadr markdown-ts-appear-table--cursor-overlays)))
    (when row
      (list (overlay-start row)
            (overlay-get anchor 'markdown-ts-appear-table--line)
            (overlay-get anchor 'markdown-ts-appear-table--offset)))))

(ert-deftest markdown-ts-appear-navigation-test-native-hidden-markup ()
  (markdown-ts-appear-navigation-test--buffer nil
    (dolist (key '("C-f" "M-f"))
      (markdown-ts-appear-navigation-test--at "`-C`" 2)
      (let ((before (markdown-ts-appear-navigation-test--cursor)))
        (markdown-ts-appear-navigation-test--key key)
        (should-not (equal before (markdown-ts-appear-navigation-test--cursor)))
        (should-not (get-pos-property (point) 'cursor-intangible))))
    (dolist (key '("C-b" "M-b"))
      (markdown-ts-appear-navigation-test--at "`-C`" 1)
      (let ((before (markdown-ts-appear-navigation-test--cursor)))
        (markdown-ts-appear-navigation-test--key key)
        (should-not (equal before (markdown-ts-appear-navigation-test--cursor)))
        (should-not (get-pos-property (point) 'cursor-intangible))))))

(ert-deftest markdown-ts-appear-navigation-test-evil-hidden-markup ()
  (markdown-ts-appear-navigation-test--buffer t
    (dolist (key '("l" "w" "e"))
      (markdown-ts-appear-navigation-test--at "`-C`" 2)
      (let ((before (markdown-ts-appear-navigation-test--cursor)))
        (markdown-ts-appear-navigation-test--key key)
        (should-not (equal before (markdown-ts-appear-navigation-test--cursor)))
        (should-not (eq (char-after) ?`))))
    (dolist (key '("h" "b" "g e"))
      (markdown-ts-appear-navigation-test--at "`-C`" 1)
      (let ((before (markdown-ts-appear-navigation-test--cursor)))
        (markdown-ts-appear-navigation-test--key key)
        (should-not (equal before (markdown-ts-appear-navigation-test--cursor)))
        (should-not (eq (char-after) ?`))))))

(ert-deftest markdown-ts-appear-navigation-test-evil-traversal ()
  (markdown-ts-appear-navigation-test--buffer t
    (dolist (key '("w" "b" "e" "g e" "W" "B" "E" "g E" "3 w" "3 b"))
      (markdown-ts-appear-navigation-test--at
       (if (member key '("b" "g e" "B" "g E" "3 b")) "`-f`" "`-t`") 2)
      (dotimes (_ 6)
        (let ((before (markdown-ts-appear-navigation-test--cursor)))
          (when before
            (markdown-ts-appear-navigation-test--key key)
            (should-not (equal before (markdown-ts-appear-navigation-test--cursor)))
            (should-not markdown-ts-appear-table--view)))))))

(ert-deftest markdown-ts-appear-navigation-test-source-and-operators ()
  (dolist (keys '("d w" "d 3 w" "c w X <escape>" "d e" "d b" "x" "r X"))
    (let (expected)
      (dolist (style '(raw wrapped))
        (let ((markdown-ts-appear-table-style style))
          (markdown-ts-appear-navigation-test--buffer t
            (markdown-ts-appear-navigation-test--at "`-C`" 2)
            (markdown-ts-appear-navigation-test--key keys)
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (if (eq style 'raw) (setq expected text)
                (should (equal text expected)))))))))
  (markdown-ts-appear-navigation-test--buffer nil
    (markdown-ts-appear-navigation-test--at "`-C`" 2)
    (markdown-ts-appear-start)
    (let ((start (point)))
      (markdown-ts-appear-navigation-test--key "C-f")
      (should (= (point) (1+ start)))
      (should (eq (char-after) ?`)))
    (should-not (seq-some (lambda (ov) (overlay-get ov 'cursor-intangible))
                          (overlays-in (point-min) (point-max))))))

(ert-deftest markdown-ts-appear-navigation-test-cleanup ()
  (dolist (already-enabled '(nil t))
    (markdown-ts-appear-navigation-test--buffer nil
      (markdown-ts-appear-mode -1)
      (cursor-intangible-mode (if already-enabled 1 -1))
      (markdown-ts-appear-mode 1)
      (markdown-ts-appear-stop)
      (markdown-ts-appear-navigation-test--at "`-C`" 2)
      (should (seq-some (lambda (ov) (overlay-get ov 'cursor-intangible))
                        (overlays-in (point-min) (point-max))))
      (markdown-ts-appear-mode -1)
      (should (eq cursor-intangible-mode already-enabled))
      (should-not (seq-some (lambda (ov) (overlay-get ov 'cursor-intangible))
                            (overlays-in (point-min) (point-max)))))))

;;; markdown-ts-appear-table-navigation-test.el ends here
