;;; latex-to-svg-upstream-test.el --- Upstream async regressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Regression coverage for async completion and failed-render behavior.
;; No markdown-ts-appear code is loaded, so these also run independently.
;; Tracking: https://github.com/alberti42/latex-to-svg/issues/5
;; Failed-edit regression: https://github.com/Thysrael/markdown-ts-appear/issues/7
;; The backend stub follows its nil-now, callback, then cached-image contract.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'markdown-ts-mode)
(require 'latex-to-svg-for-markdown)

(defmacro appear-upstream-test--deferred (&rest body)
  "Run BODY with a controlled callback queue and real frontend."
  (declare (indent 0) (debug t))
  `(let ((ready (make-hash-table :test #'equal)) callbacks
         (buffer (generate-new-buffer " *upstream-async*")))
     (cl-letf (((symbol-function 'latex-to-svg-backend)
                (lambda (value &rest options)
                  (or (gethash value ready)
                      (progn
                        (push (cons value (plist-get options :callback)) callbacks)
                        nil))))
               ((symbol-function 'latex-to-svg-backend-engine-used) (lambda (&rest _) 'latex))
               ((symbol-function 'latex-to-svg-frontend--graphical-p) (lambda () t))
               ((symbol-function 'latex-to-svg-frontend--appearance)
                (lambda (&rest _) '("#000000" "#ffffff" 20))))
       (unwind-protect
           (with-current-buffer buffer
             (insert "Before $x$ after\n")
             (markdown-ts-mode)
              (setq-local latex-to-svg-frontend-reveal (lambda (_kind) t))
             (goto-char (point-max))
             (latex-to-svg-for-markdown-mode 1)
             (should (assoc "$x$" callbacks))
             ,@body)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (latex-to-svg-for-markdown-mode -1))
           (kill-buffer buffer))))))

(ert-deftest appear-upstream-test-old-result-after-new-result ()
  (appear-upstream-test--deferred
    (let ((old (cdr (assoc "$x$" callbacks))))
      (goto-char 10) (delete-char -1) (insert "y")
      (goto-char (point-max))
      (latex-to-svg-frontend--render-region (point-min) (point-max))
      (puthash "$y$" 'new-image ready)
      (funcall (cdr (assoc "$y$" callbacks)))
      (puthash "$x$" 'old-image ready)
      (funcall old)
      (should (equal
               (mapcar (lambda (ov) (overlay-get ov 'latex-to-svg-frontend-source))
                       (latex-to-svg-frontend--overlays-in (point-min) (point-max)))
               '("$y$"))))))

(ert-deftest appear-upstream-test-result-while-editing ()
  (appear-upstream-test--deferred
    (goto-char 9)
    (latex-to-svg-frontend--handle-cursor)
    (puthash "$x$" 'image ready)
    (funcall (cdr (assoc "$x$" callbacks)))
    (latex-to-svg-frontend--handle-cursor)
    (should-not (overlay-get
                 (car (latex-to-svg-frontend--overlays-in (point-min) (point-max)))
                 'display))))

(ert-deftest appear-upstream-test-result-after-disable ()
  (appear-upstream-test--deferred
    (latex-to-svg-for-markdown-mode -1)
    (puthash "$x$" 'image ready)
    (funcall (cdr (assoc "$x$" callbacks)))
    (should-not (latex-to-svg-frontend--overlays-in (point-min) (point-max)))))

(ert-deftest appear-upstream-test-failed-edit-keeps-source ()
  (appear-upstream-test--deferred
    (puthash "$x$" 'old-image ready)
    (funcall (cdr (assoc "$x$" callbacks)))
    (goto-char 9)
    (run-hooks 'post-command-hook)
    (delete-char 1)
    (insert "\\notacommand")
    (run-hooks 'post-command-hook)
    (goto-char (point-max))
    (run-hooks 'post-command-hook)
    ;; A failed compile returns no image and does not invoke its callback.
    (goto-char 9)
    (run-hooks 'post-command-hook)
    (goto-char (point-max))
    (run-hooks 'post-command-hook)
    (should-not (seq-some (lambda (ov) (overlay-get ov 'display))
                          (latex-to-svg-frontend--overlays-in (point-min) (point-max))))))

;;; latex-to-svg-upstream-test.el ends here
