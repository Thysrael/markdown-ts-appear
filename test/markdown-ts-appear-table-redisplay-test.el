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

(defvar markdown-ts-appear-table-redisplay--references 0
  "Serial number for native-text terminal cursor references.")

(defun markdown-ts-appear-table-redisplay--cursor (expected)
  "Ask the PTY driver to check the actual terminal cursor against EXPECTED."
  (cl-incf markdown-ts-appear-table-redisplay--checks)
  (let ((edges (window-inside-edges)))
    (send-string-to-terminal
     (format "\e]777;cursor;%d;%d\a"
             (+ (nth 1 edges) (cdr expected))
             (+ (car edges) (car expected))))))

(defun markdown-ts-appear-table-redisplay--reference-position (display index)
  "Record DISPLAY at INDEX as ordinary text with no replacement overlays.
Emacs 31's string-width and posn-at-point both overcount composed ZWJ emoji.
Have the PTY driver capture the actual native-text cursor instead."
  (let ((reference (cl-incf markdown-ts-appear-table-redisplay--references)))
    (save-window-excursion
      (with-temp-buffer
        (switch-to-buffer (current-buffer))
        (insert display)
        (goto-char (1+ index))
        (set-window-start (selected-window) (point-min))
        (redisplay t)
        (send-string-to-terminal (format "\e]777;reference;%d\a" reference))))
    reference))

(defun markdown-ts-appear-table-redisplay--compare-reference (reference)
  "Compare the actual rendered cursor with native-text REFERENCE."
  (cl-incf markdown-ts-appear-table-redisplay--checks)
  (send-string-to-terminal (format "\e]777;compare;%d\a" reference)))

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
                     (expected (markdown-ts-appear-table-redisplay--reference-position
                                display index)))
                (markdown-ts-appear-table--post-command)
                (set-window-start (selected-window) (overlay-start row))
                (redisplay t)
                (markdown-ts-appear-table-redisplay--compare-reference expected)))
            (goto-char (point-max))
            (markdown-ts-appear-table--post-command)
            (redisplay t)
            (let* ((row (nth 3 markdown-ts-appear-table--cursor-row))
                   (display (overlay-get row 'markdown-ts-appear-table--display))
                   (expected (markdown-ts-appear-table-redisplay--reference-position
                              display (length display))))
              (markdown-ts-appear-table--post-command)
              (set-window-start (selected-window) (overlay-start row))
              (redisplay t)
              (markdown-ts-appear-table-redisplay--compare-reference expected)))
        (markdown-ts-appear-mode -1)))))

(ert-deftest markdown-ts-appear-table-redisplay-stable-faces ()
  (skip-unless (not noninteractive))
  (require 'evil)
  (let ((faces (mapcar (lambda (face) (cons face (face-all-attributes face)))
                       '(markdown-ts-table markdown-ts-table-cell
                         markdown-ts-table-delimiter-cell))))
    (unwind-protect
        (save-window-excursion
          (with-temp-buffer
            (switch-to-buffer (current-buffer))
            (insert markdown-ts-appear-table-redisplay--example)
            (markdown-ts-mode)
            (setq-local markdown-ts-appear-table-style 'wrapped)
            (set-face-attribute 'markdown-ts-table nil :background "#2b2d35" :extend t)
            (set-face-attribute 'markdown-ts-table-cell nil :inherit 'markdown-ts-table)
            (set-face-attribute 'markdown-ts-table-delimiter-cell nil
                                :inherit '(markdown-ts-table shadow))
            (markdown-ts-appear-mode 1)
            (markdown-ts-appear-stop)
            (evil-local-mode 1)
            (evil-normal-state)
            (font-lock-ensure)
            (markdown-ts-appear-table--render 34)
            (goto-char (point-min))
            (search-forward "| 5-7 |")
            (beginning-of-line)
            (markdown-ts-appear-table--post-command)
            (set-window-start (selected-window) (point))
            (redisplay t)
            (let* ((beg (point))
                   (length (- (line-end-position) beg 1))
                   (row (nth 3 markdown-ts-appear-table--cursor-row))
                   (display (overlay-get row 'markdown-ts-appear-table--display))
                   (top (nth 1 (window-inside-edges)))
                   (height (cl-count ?\n display)))
              (send-string-to-terminal (format "\e]777;watch;%d;%d\a" top (+ top height)))
              (dotimes (_ length)
                (let ((position (point)))
                  (condition-case err (execute-kbd-macro "l")
                    (error (ert-fail (list 'right position err))))
                  (should (= (1+ position) (point))))
                (redisplay t))
              (dotimes (_ length)
                (let ((position (point)))
                  (condition-case err (execute-kbd-macro "h")
                    (error (ert-fail (list 'left position err))))
                  (should (= (1- position) (point))))
                (redisplay t))
              (send-string-to-terminal "\e]777;unwatch\a")
              (cl-incf markdown-ts-appear-table-redisplay--checks)
              (should (= beg (point))))
            (markdown-ts-appear-mode -1)
            (evil-local-mode -1)))
      (dolist (face faces)
        (apply #'set-face-attribute (car face) nil
               (cl-mapcan (lambda (attribute) (list (car attribute) (cdr attribute)))
                          (cdr face)))))))

(ert-deftest markdown-ts-appear-table-redisplay-vertical-columns ()
  (skip-unless (not noninteractive))
  (require 'evil)
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (insert "| A | B |\n|---|---|\n| x | abcd efgh ijkl mnop qrst uvwx |\n| y | ABCD EFGH IJKL MNOP QRST UVWX |\n\nafter\n")
      (markdown-ts-mode)
      (setq-local markdown-ts-appear-table-style 'wrapped)
      (markdown-ts-appear-mode 1)
      (markdown-ts-appear-stop)
      (evil-local-mode 1)
      (evil-normal-state)
      (evil-local-set-key 'normal "j" #'evil-next-visual-line)
      (evil-local-set-key 'normal "k" #'evil-previous-visual-line)
      (evil-local-set-key 'normal "gj" #'evil-next-line)
      (evil-local-set-key 'normal "gk" #'evil-previous-line)
      (unwind-protect
          (progn
            (markdown-ts-appear-table--render 18)
            (goto-char (point-min))
            (search-forward "abcd")
            (goto-char (1+ (match-beginning 0)))
            (markdown-ts-appear-table--post-command)
            (set-window-start (selected-window) (line-beginning-position))
            (redisplay t)
            (let ((last-command nil) goal-column temporary-goal-column)
              (dolist (step '(("j" ?j 1) ("j" ?r 2) ("j" ?B 3)
                              ("k" ?r 2) ("k" ?j 1) ("k" ?b 0)
                              ("2j" ?r 2) ("2k" ?b 0)
                              ("gj" ?B 3) ("gk" ?b 0)))
                (execute-kbd-macro (car step))
                (should (= (char-after) (cadr step)))
                (redisplay t)
                (markdown-ts-appear-table-redisplay--cursor (cons 7 (nth 2 step)))))
            (erase-buffer)
            (insert "| A | B |\n|---|---|\n| x | 甲乙丙丁戊己庚辛壬癸 |\n| y | 一二三四五六七八九十 |\n")
            (font-lock-ensure)
            (markdown-ts-appear-table--render 18)
            (goto-char (point-min))
            (search-forward "乙")
            (backward-char)
            (markdown-ts-appear-table--post-command)
            (set-window-start (selected-window) (line-beginning-position))
            (redisplay t)
            (let ((last-command nil) goal-column temporary-goal-column)
              (dolist (step '(("j" ?庚 1) ("j" ?二 2)
                              ("k" ?庚 1) ("k" ?乙 0)))
                (execute-kbd-macro (car step))
                (should (= (char-after) (cadr step)))
                (redisplay t)
                (markdown-ts-appear-table-redisplay--cursor (cons 8 (nth 2 step))))))
        (markdown-ts-appear-mode -1)
        (evil-local-mode -1)))))

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
