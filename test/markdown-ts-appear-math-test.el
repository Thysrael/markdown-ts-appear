;;; markdown-ts-appear-math-test.el --- Math integration tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Exercise the real latex-to-svg frontend with a controlled rendering backend.
;; Set MARKDOWN_TS_APPEAR_REQUIRE_LATEX_TO_SVG=1 to require the optional package.
;; The real-engine test requires SVG support and a working rendering runtime.

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'markdown-ts-appear)

(defvar markdown-ts-appear-math-test--available
  (require 'latex-to-svg-for-markdown nil t))

(when (and (member (getenv "MARKDOWN_TS_APPEAR_REQUIRE_LATEX_TO_SVG") '("1" "true"))
           (not markdown-ts-appear-math-test--available))
  (error "Required latex-to-svg-for-markdown package is unavailable"))

(defvar markdown-ts-appear-math-test--calls nil)
(defvar latex-to-svg-backend-cache-directory)
(defvar latex-to-svg-backend--image-cache)

(defmacro markdown-ts-appear-math-test--buffer (content &rest body)
  "Run BODY in a fresh Markdown buffer with CONTENT and real frontend hooks."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless markdown-ts-appear-math-test--available)
     (skip-unless (treesit-ready-p '(markdown markdown-inline)))
     (let ((buffer (generate-new-buffer " *math-integration-test*"))
           (markdown-ts-inline-images nil)
           (markdown-ts-appear-enable-math-preview nil)
           (markdown-ts-appear-table-style 'raw))
       (unwind-protect
           (with-current-buffer buffer
             (insert ,content)
             (markdown-ts-mode)
             (setq-local latex-to-svg-frontend-reconcile-idle nil
                         latex-to-svg-frontend-number-equations nil)
             (goto-char (point-max))
             ,@body)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (markdown-ts-appear-mode -1)
             (latex-to-svg-for-markdown-mode -1))
           (kill-buffer buffer))))))

(defmacro markdown-ts-appear-math-test--rendered (&rest body)
  "Run BODY with real frontend placement and a synchronous fake image."
  (declare (indent 0) (debug t))
  `(let (markdown-ts-appear-math-test--calls)
     (cl-letf (((symbol-function 'latex-to-svg-backend)
                (lambda (source &rest _)
                  (push source markdown-ts-appear-math-test--calls)
                  'fake-image))
               ((symbol-function 'latex-to-svg-backend-engine-used)
                (lambda (&rest _) 'latex))
               ((symbol-function 'latex-to-svg-frontend--graphical-p) (lambda () t))
               ((symbol-function 'latex-to-svg-frontend--appearance)
                (lambda (&rest _) '("#000000" "#ffffff" 20))))
       ,@body)))

(defun markdown-ts-appear-math-test--previews ()
  "Return math previews in the current buffer."
  (seq-filter (lambda (ov) (overlay-get ov 'latex-to-svg-frontend))
              (overlays-in (point-min) (point-max))))

(defun markdown-ts-appear-math-test--move (position)
  "Move to POSITION and execute the installed command hooks."
  (goto-char position)
  (run-hooks 'post-command-hook))

(ert-deftest markdown-ts-appear-math-test-optional-package ()
  ;; This test must also run when the optional package is genuinely absent.
  (skip-unless (treesit-ready-p '(markdown markdown-inline)))
  (with-temp-buffer
    (insert "$x$\n")
    (markdown-ts-mode)
    (let ((markdown-ts-appear-enable-math-preview nil))
      (unwind-protect
          (progn
            (markdown-ts-appear-mode 1)
            (should-not (bound-and-true-p latex-to-svg-frontend-mode))
            (markdown-ts-appear-mode -1)
            (let ((original (symbol-function 'require)))
              (cl-letf (((symbol-function 'require)
                         (lambda (feature &optional filename noerror)
                           (unless (eq feature 'latex-to-svg-for-markdown)
                             (funcall original feature filename noerror)))))
                (setq markdown-ts-appear-enable-math-preview t)
                (should-error (markdown-ts-appear-mode 1) :type 'user-error))))
        (markdown-ts-appear-mode -1)))))

(ert-deftest markdown-ts-appear-math-test-owned-lifecycle ()
  (markdown-ts-appear-math-test--buffer "$x$\n"
    (markdown-ts-appear-math-test--rendered
      (setq markdown-ts-appear-enable-math-preview t)
      (markdown-ts-appear-mode 1)
      (should latex-to-svg-for-markdown-mode)
      (should markdown-ts-appear-math--owned)
      (should (= 1 (length (markdown-ts-appear-math-test--previews))))
      (markdown-ts-appear-mode -1)
      (should-not latex-to-svg-for-markdown-mode)
      (should-not (markdown-ts-appear-math-test--previews))
      (should-not markdown-ts-appear-math--saved-reveal)
      (should-not (local-variable-p 'latex-to-svg-frontend-reveal)))))

(ert-deftest markdown-ts-appear-math-test-independent-mode-orders ()
  (dolist (first '(appear math))
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (markdown-ts-appear-math-test--rendered
        (setq-local latex-to-svg-frontend-reveal 'always)
        (if (eq first 'appear)
            (progn (markdown-ts-appear-mode 1) (latex-to-svg-for-markdown-mode 1))
          (latex-to-svg-for-markdown-mode 1)
          (markdown-ts-appear-mode 1))
        (should-not markdown-ts-appear-math--owned)
        (should (eq latex-to-svg-frontend-reveal #'markdown-ts-appear-math--reveal-p))
        (markdown-ts-appear-mode -1)
        (should latex-to-svg-frontend-mode)
        (should (eq latex-to-svg-frontend-reveal 'always))
        (should (local-variable-p 'latex-to-svg-frontend-reveal))))))

(ert-deftest markdown-ts-appear-math-test-preserves-new-user-option ()
  (markdown-ts-appear-math-test--buffer "$x$\n"
    (markdown-ts-appear-math-test--rendered
      (setq markdown-ts-appear-enable-math-preview t)
      (markdown-ts-appear-mode 1)
      (setq-local latex-to-svg-frontend-reveal nil)
      (markdown-ts-appear-mode -1)
      (should (local-variable-p 'latex-to-svg-frontend-reveal))
      (should-not latex-to-svg-frontend-reveal))))

(ert-deftest markdown-ts-appear-math-test-toggle-at-same-point ()
  (markdown-ts-appear-math-test--buffer "$x$\n"
    (markdown-ts-appear-math-test--rendered
      (markdown-ts-appear-mode 1)
      (markdown-ts-appear-math-test--move 2)
      (should markdown-ts-appear--region)
      (latex-to-svg-for-markdown-mode 1)
      (should-not markdown-ts-appear--region)
      (latex-to-svg-for-markdown-mode -1)
      (should markdown-ts-appear--region)
      (should-not markdown-ts-appear-math--saved-reveal))))

(ert-deftest markdown-ts-appear-math-test-reveal-stop-start ()
  (markdown-ts-appear-math-test--buffer "Before $x$ after\n"
    (markdown-ts-appear-math-test--rendered
      (setq markdown-ts-appear-enable-math-preview t)
      (markdown-ts-appear-mode 1)
      (let ((preview (car (markdown-ts-appear-math-test--previews))))
        (markdown-ts-appear-math-test--move 9)
        (font-lock-ensure)
        (should-not markdown-ts-appear--region)
        (should-not (overlay-get preview 'display))
        (should-not (get-text-property 8 'invisible))
        (markdown-ts-appear-stop)
        (run-hooks 'post-command-hook)
        (should (overlay-get preview 'display))
        (markdown-ts-appear-start)
        (run-hooks 'post-command-hook)
        (should-not (overlay-get preview 'display))
        (markdown-ts-appear-math-test--move (point-max))
        (should (overlay-get preview 'display))
        (should (= 1 (length markdown-ts-appear-math-test--calls)))))))

(ert-deftest markdown-ts-appear-math-test-edited-on-stop ()
  (markdown-ts-appear-math-test--buffer "$x$\n"
    (markdown-ts-appear-math-test--rendered
      (setq markdown-ts-appear-enable-math-preview t)
      (markdown-ts-appear-mode 1)
      (markdown-ts-appear-math-test--move 2)
      (delete-char 1) (insert "y")
      (run-hooks 'post-command-hook)
      (markdown-ts-appear-stop)
      (run-hooks 'post-command-hook)
      (should (equal (car markdown-ts-appear-math-test--calls) "$y$"))
      (should (overlay-get (car (markdown-ts-appear-math-test--previews)) 'display)))))

(ert-deftest markdown-ts-appear-math-test-syntax-and-exclusions ()
  (markdown-ts-appear-math-test--buffer
      "$x$. \\(y\\) \\[z\\]\n\n\\begin{equation}a=b\\end{equation}\n\n`$code$`\n\n```tex\n$block$\n```\n"
    (markdown-ts-appear-math-test--rendered
      (setq markdown-ts-appear-enable-math-preview t)
      (markdown-ts-appear-mode 1)
      (should (= 4 (length (markdown-ts-appear-math-test--previews))))
      (dolist (source '("$x$" "\\(y\\)" "\\[z\\]" "\\begin{equation}a=b\\end{equation}"))
        (goto-char (point-min)) (search-forward source)
        (run-hooks 'post-command-hook)
        (font-lock-ensure)
        (should (markdown-ts-appear-math--span-at (point)))
        (should-not (markdown-ts-appear--bounds))
        (should-not (text-property-not-all (- (point) (length source)) (point) 'invisible nil))))))

(ert-deftest markdown-ts-appear-math-test-pending-render-keeps-delimiters ()
  (markdown-ts-appear-math-test--buffer "$pending$ \\(pending\\)\n"
    ;; A nil result is also returned while compilation is still pending.
    (cl-letf (((symbol-function 'latex-to-svg-backend) (lambda (&rest _) nil)))
      (setq markdown-ts-appear-enable-math-preview t)
      (markdown-ts-appear-mode 1)
      (font-lock-ensure)
      (should-not (markdown-ts-appear-math-test--previews))
      (should-not (text-property-not-all (point-min) (point-max) 'invisible nil)))))

(ert-deftest markdown-ts-appear-math-test-foreign-overlays-and-major-mode ()
  (markdown-ts-appear-math-test--buffer "$x$\n"
    (markdown-ts-appear-math-test--rendered
      (let ((foreign (make-overlay 1 4)))
        (overlay-put foreign 'display "foreign")
        (setq markdown-ts-appear-enable-math-preview t)
        (markdown-ts-appear-mode 1)
        (text-mode)
        (should-not (markdown-ts-appear-math-test--previews))
        (should (overlay-buffer foreign))
        (should-not markdown-ts-appear-math--owned)))))

(ert-deftest markdown-ts-appear-math-test-real-render ()
  (skip-unless (image-type-available-p 'svg))
  (let ((latex-to-svg-backend-cache-directory (make-temp-file "appear-math-test-" t))
        (latex-to-svg-backend--image-cache (make-hash-table :test #'equal)))
    (unwind-protect
        ;; Fix only the display context for batch Emacs.  Compilation, SVG
        ;; generation and the frontend's asynchronous placement remain real.
        (cl-letf (((symbol-function 'latex-to-svg-frontend--graphical-p) (lambda () t))
                  ((symbol-function 'latex-to-svg-frontend--foreground-for)
                   (lambda (&rest _) "#000000"))
                  ((symbol-function 'latex-to-svg-frontend--x-height-at)
                   (lambda (&rest _) 10)))
          (markdown-ts-appear-math-test--buffer "$x^2+1$\n"
            (switch-to-buffer (current-buffer))
            (setq markdown-ts-appear-enable-math-preview t)
            (markdown-ts-appear-mode 1)
            (should-not (markdown-ts-appear-math-test--previews))
            (markdown-ts-appear-math-test--move 2)
            (font-lock-ensure)
            (let ((deadline (+ (float-time) 30)))
              (while (and (not (markdown-ts-appear-math-test--previews))
                          (< (float-time) deadline))
                (accept-process-output nil 0.01)))
            (let* ((preview (car (markdown-ts-appear-math-test--previews)))
                   (image (overlay-get preview 'latex-to-svg-frontend-image)))
              (should (eq (car image) 'image))
              (should (> (latex-to-svg-backend-image-width image) 0))
              (should (string-match-p "<svg" (image-property image :data)))
              (when (display-graphic-p)
                (should (> (car (image-size image t)) 0)))
              (should-not (overlay-get preview 'display))
              (should-not (get-text-property (point-min) 'invisible))
              (markdown-ts-appear-math-test--move (point-max))
              (should (overlay-get preview 'display)))))
      (delete-directory latex-to-svg-backend-cache-directory t))))

(ert-deftest markdown-ts-appear-math-test-no-markdown-link-in-math ()
  (markdown-ts-appear-math-test--buffer "\\(f[x](y)\\)\n"
    (markdown-ts-appear-math-test--rendered
      (setq markdown-ts-appear-enable-math-preview t)
      (markdown-ts-appear-mode 1)
      (font-lock-ensure)
      (should-not (seq-some
                   (lambda (ov) (overlay-get ov 'markdown-ts-appear--link-icon))
                   (overlays-in (point-min) (point-max)))))))

(ert-deftest markdown-ts-appear-math-test-fontification-shares-span-scan ()
  (markdown-ts-appear-math-test--buffer
      "Before **one** and **two**, \\(f[x](y)\\).\n"
    (markdown-ts-appear-math-test--rendered
      (setq markdown-ts-appear-enable-math-preview t)
      (markdown-ts-appear-mode 1)
      ;; Isolate appear's query budget from the adaptor's own unhide pass.
      (font-lock-remove-keywords nil latex-to-svg-for-markdown--font-lock-keywords)
      (let ((query (symbol-function 'latex-to-svg-frontend-spans))
            (query-at (symbol-function 'latex-to-svg-frontend-span-at))
            (calls 0))
        (cl-letf (((symbol-function 'latex-to-svg-frontend-spans)
                   (lambda (beg end)
                     (cl-incf calls)
                     (funcall query beg end)))
                  ((symbol-function 'latex-to-svg-frontend-span-at)
                   (lambda (position)
                     (cl-incf calls)
                     (funcall query-at position))))
          (font-lock-fontify-region (point-min) (point-max))
          (should (<= calls 1))
          (should-not (seq-some
                       (lambda (ov) (overlay-get ov 'markdown-ts-appear--link-icon))
                       (overlays-in (point-min) (point-max))))
          ;; Removing the math delimiters must expose a normal Markdown link
          ;; on the next pass, rather than reusing the old span list.
          (goto-char (point-min)) (search-forward "\\(") (delete-char -2)
          (search-forward "\\)") (delete-char -2)
          (setq calls 0)
          (font-lock-fontify-region (point-min) (point-max))
          (should (<= calls 1))
          (should (seq-some
                   (lambda (ov) (overlay-get ov 'markdown-ts-appear--link-icon))
                   (overlays-in (point-min) (point-max))))
          ;; Plain text has no decoration that needs a math containment check.
          (erase-buffer)
          (insert "Plain text without markup.\n")
          (setq calls 0)
          (font-lock-fontify-region (point-min) (point-max))
          (should (zerop calls)))))))

(ert-deftest markdown-ts-appear-math-test-wrapped-row-reveals-math ()
  (markdown-ts-appear-math-test--buffer "| A | B |\n|---|---|\n| $x$ | text |\n"
    (markdown-ts-appear-math-test--rendered
      (setq markdown-ts-appear-enable-math-preview t
            markdown-ts-appear-table-style 'wrapped)
      (markdown-ts-appear-mode 1)
      (goto-char (point-min)) (search-forward "$x")
      (run-hooks 'post-command-hook)
      (should-not markdown-ts-appear--region)
      (should markdown-ts-appear-table--view)
      (dolist (ov (overlays-at (point)))
        (when (or (overlay-get ov 'markdown-ts-appear-table--wrapped)
                  (overlay-get ov 'latex-to-svg-frontend))
          (should-not (overlay-get ov 'display)))))))

(ert-deftest markdown-ts-appear-math-test-clone-preserves-base ()
  ;; Appear detaches itself from a clone in `clone-indirect-buffer-hook'; the
  ;; clone keeps the adaptor, its own previews and the previous reveal option.
  ;; The base buffer keeps its pending preview whichever mode was turned on
  ;; first, and so whichever function on that hook was added last.
  (dolist (first '(appear math))
    (markdown-ts-appear-math-test--buffer "$x$ and $y$\n"
      (markdown-ts-appear-math-test--rendered
        (let (ready callback)
          (cl-letf (((symbol-function 'latex-to-svg-backend)
                     (lambda (source &rest options)
                       (if (or ready (equal source "$x$")) 'fake-image
                         (setq callback (plist-get options :callback))
                         nil))))
            (if (eq first 'appear)
                (progn (setq markdown-ts-appear-enable-math-preview t)
                       (markdown-ts-appear-mode 1))
              (latex-to-svg-for-markdown-mode 1)
              (markdown-ts-appear-mode 1))
            (markdown-ts-appear-math-test--move 2)
            (should callback)
            ;; The clone renders and requests `$y$' too, so keep the base
            ;; buffer's callback.
            (let* ((base-callback callback)
                   (preview (car (markdown-ts-appear-math-test--previews)))
                   (last-point latex-to-svg-frontend--last-point)
                   (clone (clone-indirect-buffer " *math-clone*" nil)))
              (unwind-protect
                  (progn
                    (with-current-buffer clone
                      (should-not markdown-ts-appear-mode)
                      (should latex-to-svg-frontend-mode)
                      (should-not (eq latex-to-svg-frontend-reveal
                                      #'markdown-ts-appear-math--reveal-p)))
                    (should (marker-position last-point))
                    (should (overlay-buffer preview))
                    (should latex-to-svg-frontend-mode)
                    (should (eq latex-to-svg-frontend-reveal
                                #'markdown-ts-appear-math--reveal-p))
                    (setq ready t)
                    (funcall base-callback nil)
                    (should (= 2 (length (markdown-ts-appear-math-test--previews))))
                    (funcall callback nil)
                    (with-current-buffer clone
                      (should (= 2 (length (markdown-ts-appear-math-test--previews))))))
                (kill-buffer clone)))))))))

(provide 'markdown-ts-appear-math-test)
;;; markdown-ts-appear-math-test.el ends here
