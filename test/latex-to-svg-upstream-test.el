;;; latex-to-svg-upstream-test.el --- Upstream async regressions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Standalone reproducer: no markdown-ts-appear code is loaded.
;; These tests currently FAIL on latex-to-svg b9cf5af; run separately from
;; the integration suite. The stub follows the documented nil-now,
;; callback-with-no-arguments, then re-query-for-image backend contract.
;;
;; emacs --batch -Q -L /path/to/latex-to-svg-backend -L /path/to/latex-to-svg \
;;   -l test/latex-to-svg-upstream-test.el -f ert-run-tests-batch-and-exit

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
               ((symbol-function 'latex-to-svg-backend-engine-used)
                (lambda (&rest _) 'latex))
               ((symbol-function 'latex-to-svg-frontend--graphical-p) (lambda () t))
               ((symbol-function 'latex-to-svg-frontend--appearance)
                (lambda (&rest _) '("#000000" "#ffffff" 20))))
       (unwind-protect
           (with-current-buffer buffer
             (insert "Before $x$ after\n")
             (markdown-ts-mode)
             (setq-local latex-to-svg-frontend-reconcile-idle nil
                         latex-to-svg-frontend-number-equations nil
                         latex-to-svg-frontend-reveal (lambda (_kind) t))
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

;;; latex-to-svg-upstream-test.el ends here
