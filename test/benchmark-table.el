;;; benchmark-table.el --- Reproducible table benchmarks -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Initialize package.el, then use -L to select the library being measured:
;; emacs --batch -Q --eval '(progn (require (quote package)) (package-initialize))' \
;;   -L . -l test/benchmark-table.el
;; Reports include garbage collection time.  Correctness is checked by ERT
;; and table-redisplay.py; these timings are not pass/fail thresholds.

;;; Code:

(require 'benchmark)
(require 'markdown-ts-appear)

(unless (treesit-ready-p '(markdown markdown-inline))
  (error "Table benchmarks require the Markdown grammars"))

(princ (format "Library: %s\n" (symbol-file 'markdown-ts-appear-table--render)))

(dolist (size '(1000 10000))
  (with-temp-buffer
    (insert "| A | B |\n|---|---|\n| x | " (make-string size ?x) " |\n\nafter\n")
    (markdown-ts-mode)
    (setq-local markdown-ts-appear-table-style 'wrapped)
    (save-window-excursion
      (switch-to-buffer (current-buffer))
      (goto-char (point-max))
      (markdown-ts-appear-mode 1)
      (markdown-ts-appear-stop)
      (unwind-protect
          (let ((beg (save-excursion (goto-char (point-min)) (forward-line 2) (+ (point) 6))))
            (garbage-collect)
            (princ
             (format "source=%d render-enter-vertical/10=%S\n" size
                     (benchmark-run 10
                       (goto-char (point-max))
                       (markdown-ts-appear-table--render 40)
                       (goto-char beg)
                       (markdown-ts-appear-table--post-command)
                       (let ((line-move-visual t) (last-command nil)
                             goal-column temporary-goal-column)
                         (line-move 1 t))
                       (markdown-ts-appear-table--post-command))))
            (princ
             (format "source=%d warm-motion/10000=%S\n" size
                     (benchmark-run 1
                       (dotimes (index 10000)
                         (goto-char (+ beg (% index 10)))
                         (markdown-ts-appear-table--post-command))))))
        (markdown-ts-appear-mode -1)))))

(with-temp-buffer
  (insert "| A | B |\n|---|---|\n")
  (dotimes (_ 500) (insert "| x | a long value that wraps in the table |\n"))
  (insert "\nafter\n")
  (markdown-ts-mode)
  (setq-local markdown-ts-appear-table-style 'wrapped)
  (save-window-excursion
    (switch-to-buffer (current-buffer))
    (goto-char (point-max))
    (markdown-ts-appear-mode 1)
    (markdown-ts-appear-stop)
    (unwind-protect
        (progn
          (markdown-ts-appear-table--render)
          (markdown-ts-appear-table--schedule-render)
          (let* ((timer markdown-ts-appear-table--resize-timer)
                 (callback (timer--function timer))
                 (args (timer--args timer))
                 (render (symbol-function 'markdown-ts-appear-table--render))
                 (calls 0))
            (cancel-timer timer)
            (cl-letf (((symbol-function 'markdown-ts-appear-table--render)
                       (lambda (&rest arguments) (cl-incf calls) (apply render arguments))))
              (princ (format "unchanged-window/100=%S "
                             (benchmark-run 100 (apply callback args))))
              (princ (format "renders=%d\n" calls)))))
      (markdown-ts-appear-mode -1))))

;;; benchmark-table.el ends here
