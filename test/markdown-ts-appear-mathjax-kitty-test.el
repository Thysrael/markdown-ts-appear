;;; markdown-ts-appear-mathjax-kitty-test.el --- Optional adapter lifecycle -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'ert)
(require 'markdown-ts-appear-mathjax-kitty)

(defconst appear-kitty--advice
  '((kitty-graphics--overlay-screen-pos . markdown-ts-appear-mathjax-kitty--position)
    (kitty-graphics--refresh-overlay . markdown-ts-appear-mathjax-kitty--refresh)
    (redraw-display . markdown-ts-appear-mathjax-kitty--redraw)))

(defmacro appear-kitty--lifecycle (&rest body)
  "Run BODY with real math modes and an inert optional terminal dependency."
  (declare (indent 0))
  `(progn
     (skip-unless (and (treesit-ready-p '(markdown markdown-inline))
                       (require 'mathjax nil t) (mathjax-available-p)))
     (let ((markdown-ts-appear-mathjax-terminal-preview t)
           (original-require (symbol-function 'require)))
       (cl-letf (((symbol-function 'display-graphic-p) #'ignore)
                 ((symbol-function 'require)
                  (lambda (feature &rest args)
                    (if (eq feature 'kitty-graphics) t
                      (apply original-require feature args)))))
         ,@body))))

(ert-deftest appear-kitty-multiple-buffers-and-unload ()
  (appear-kitty--lifecycle
    (let ((first (generate-new-buffer " *kitty-first*"))
          (second (generate-new-buffer " *kitty-second*")))
      (unwind-protect
          (progn
            (dolist (buffer (list first second))
              (with-current-buffer buffer
                (markdown-ts-mode)
                (markdown-ts-appear-mathjax-mode 1)))
            (with-current-buffer first (markdown-ts-appear-mathjax-mode -1))
            (dolist (pair appear-kitty--advice)
              (should (advice-member-p (cdr pair) (car pair))))
            (kill-buffer second)
            (dolist (pair appear-kitty--advice)
              (should-not (advice-member-p (cdr pair) (car pair))))
            (with-current-buffer first (markdown-ts-appear-mathjax-mode 1))
            (unload-feature 'markdown-ts-appear-mathjax-kitty t)
            (with-current-buffer first
              (should-not markdown-ts-appear-mathjax-mode)
              (should-not (memq 'markdown-ts-appear-mathjax-kitty--repaint post-command-hook)))
            (dolist (pair appear-kitty--advice)
              (should-not (advice-member-p (cdr pair) (car pair)))))
        (dolist (buffer (list first second))
          (when (buffer-live-p buffer) (kill-buffer buffer)))
        (require 'markdown-ts-appear-mathjax-kitty)))))

(ert-deftest appear-kitty-disable-cancels-pending-raster ()
  (appear-kitty--lifecycle
    (let ((directory (make-temp-file "appear-kitty-cancel-" t)) process)
      (unwind-protect
          (with-temp-buffer
            (insert "$x$\n")
            (markdown-ts-mode)
            (markdown-ts-appear-mathjax-mode 1)
            (let ((ov (car markdown-ts-appear-mathjax--objects))
                  (make-process (symbol-function 'make-process)))
              ;; Hold a real subprocess open so cancellation cannot race a fast
              ;; converter.  Keep the adapter's real sentinel and temp files.
              (cl-letf (((symbol-function 'executable-find) (lambda (_) "cat"))
                        ((symbol-function 'make-process)
                         (lambda (&rest args)
                           (setq args (plist-put args :command '("cat")))
                           (apply make-process (plist-put args :connection-type 'pipe)))))
                (markdown-ts-appear-mathjax-kitty--rasterize
                 ov "<svg height=\"12pt\"/>" (expand-file-name "result.png" directory) '(8 16 1 1 8 16)))
              (setq process (overlay-get ov 'markdown-ts-appear-mathjax-kitty--process))
              (should (process-live-p process))
              (should-not (overlay-get ov 'markdown-ts-appear-mathjax-kitty--view))
              (markdown-ts-appear-mathjax-mode -1)
              (accept-process-output process 0.01)
              (should-not (process-live-p process))
              (should-not (overlay-buffer ov))
              (should-not (overlay-get ov 'markdown-ts-appear-mathjax-kitty--process))
              (should-not (directory-files directory nil "^[^.]"))))
        (when (process-live-p process) (delete-process process))
        (delete-directory directory t)))))

(ert-deftest appear-kitty-missing-dependency-leaves-mode-disabled ()
  (appear-kitty--lifecycle
    (with-temp-buffer
      (markdown-ts-mode)
      (let ((require (symbol-function 'require)))
        (cl-letf (((symbol-function 'require)
                   (lambda (feature &rest args)
                     (if (eq feature 'kitty-graphics)
                         (signal 'file-missing '("Missing optional dependency" "kitty-graphics"))
                       (apply require feature args)))))
          (should-error (markdown-ts-appear-mathjax-mode 1) :type 'file-missing)))
      (should-not markdown-ts-appear-mathjax-mode)
      (should-not (memq 'markdown-ts-appear-mathjax-kitty--repaint post-command-hook))
      (dolist (pair appear-kitty--advice)
        (should-not (advice-member-p (cdr pair) (car pair)))))))

(provide 'markdown-ts-appear-mathjax-kitty-test)
;;; markdown-ts-appear-mathjax-kitty-test.el ends here
