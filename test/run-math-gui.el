;;; run-math-gui.el --- Isolated graphical math tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Load with emacs -Q, this checkout and the optional math libraries on
;; load-path. MATH_GUI_OUTPUT names the results file; MATH_GUI_ENGINE defaults
;; to latex. MATH_BENCH_RATEX optionally names render-svg. Uses a fresh cache.

;;; Code:
(require 'package)
(package-initialize)
(defconst appear-math-gui-root
  (file-name-directory
   (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(add-to-list 'load-path appear-math-gui-root)
(setq load-prefer-newer t native-comp-jit-compilation nil inhibit-startup-screen t)
(require 'latex-to-svg-for-markdown)
(setq latex-to-svg-backend-cache-directory (make-temp-file "math-gui-cache-" t)
      latex-to-svg-backend-gc-interval nil
      latex-to-svg-frontend-engine (intern (or (getenv "MATH_GUI_ENGINE") "latex")))
(when (getenv "MATH_BENCH_RATEX")
  (setq latex-to-svg-backend-ratex-program (getenv "MATH_BENCH_RATEX")))

(defun appear-math-gui-run ()
  "Run math integration checks in an independent graphical Emacs."
  (let ((code 1))
    (condition-case err
        (progn
          (unless (display-graphic-p) (error "Graphical frame required"))
          (set-frame-size nil 100 40)
          (load (expand-file-name "test/markdown-ts-appear-math-test.el" appear-math-gui-root) nil t)
          (let ((stats (ert-run-tests-batch "markdown-ts-appear-math-test")))
            (setq code (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))
      (error (message "GUI check failed: %S" err)))
    (with-temp-file (getenv "MATH_GUI_OUTPUT")
      (insert (format "engine=%S exit=%d\n" latex-to-svg-frontend-engine code))
      (when (get-buffer "*Messages*")
        (insert (with-current-buffer "*Messages*" (buffer-string)))))
    (kill-emacs code)))

(add-hook 'window-setup-hook (lambda () (run-at-time 0.5 nil #'appear-math-gui-run)))
;;; run-math-gui.el ends here
