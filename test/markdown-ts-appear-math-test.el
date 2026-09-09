;;; markdown-ts-appear-math-test.el --- Math preview tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Standalone suite.  Set MARKDOWN_TS_APPEAR_REQUIRE_GRAMMARS=1 and
;; MARKDOWN_TS_APPEAR_REQUIRE_MATHJAX=1 to reject missing dependencies.
;; Deferred tests use the installed public mathjax-display, replacing only
;; transport.  Smoke tests also exercise the real Node process and SVG images.

;;; Code:
(require 'ert)
(require 'cl-lib)
(declare-function mathjax-available-p "mathjax")
(declare-function mathjax-display "mathjax")
(declare-function mathjax-render "mathjax")
(let ((root (file-name-directory
             (directory-file-name (file-name-directory
                                   (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path root))
(require 'markdown-ts-appear)

(when (and (equal (getenv "MARKDOWN_TS_APPEAR_REQUIRE_GRAMMARS") "1")
           (not (treesit-ready-p '(markdown markdown-inline))))
  (error "Required Markdown Tree-sitter grammars are unavailable"))
(when (and (member (getenv "MARKDOWN_TS_APPEAR_REQUIRE_MATHJAX") '("1" "true"))
           (not (and (require 'mathjax nil t) (mathjax-available-p)
                     (image-type-available-p 'svg))))
  (error "Required MathJax package, Node.js or SVG support is unavailable"))

(defvar markdown-ts-appear-math-test--callbacks nil)
(defconst markdown-ts-appear-math-test--svg
  '((svg . "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"2ex\" height=\"2ex\"><path d=\"M0 0L1 1\"/></svg>")))

(defmacro markdown-ts-appear-math-test--buffer (content &rest body)
  "Evaluate BODY in an independently configured Markdown buffer with CONTENT."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (treesit-ready-p '(markdown markdown-inline)))
     ;; Unlike with-temp-buffer, preserve kill-buffer-hook execution.
     (let ((buffer (generate-new-buffer " *math-test*")))
       (unwind-protect
           (with-current-buffer buffer
             (insert ,content)
             (let ((markdown-ts-inline-images nil) (treesit-font-lock-level 3)
                   (markdown-ts-appear-enable-math-preview nil))
               (markdown-ts-mode)
               (goto-char (point-max))
               (markdown-ts-appear-mode 1)
               (font-lock-ensure)
               ,@body))
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun markdown-ts-appear-math-test--enable ()
  "Restart core with math previews after the fixture's initial fontification."
  (markdown-ts-appear-mode -1)
  (setq markdown-ts-appear-enable-math-preview t)
  (markdown-ts-appear-mode 1)
  (font-lock-ensure))

(defun markdown-ts-appear-math-test--assert-clean ()
  "Assert that math state and its buffer-local hooks are detached."
  (should-not markdown-ts-appear-math--objects)
  (should-not markdown-ts-appear-math--sources)
  (should-not (memq #'markdown-ts-appear-math--refresh post-command-hook))
  (should-not (memq #'markdown-ts-appear-math--clear before-change-functions))
  (should-not (memq #'markdown-ts-appear-math--refresh outline-view-change-hook)))

(defmacro markdown-ts-appear-math-test--deferred (&rest body)
  "Run BODY with the real display API and a deferred transport."
  (declare (indent 0) (debug t))
  `(progn
     (skip-unless (and (require 'mathjax nil t) (image-type-available-p 'svg)))
     (let (markdown-ts-appear-math-test--callbacks)
       (cl-letf (((symbol-function 'mathjax-available-p) (lambda () t))
                 ((symbol-function 'mathjax-render)
                  (lambda (callback math &rest options)
                    (push (list callback math options)
                          markdown-ts-appear-math-test--callbacks))))
         ,@body))))

(defun markdown-ts-appear-math-test--deliver (request &optional data)
  "Deliver REQUEST on a timer with DATA, defaulting to a successful SVG."
  (should (functionp (car request)))
  (let ((deadline (+ (float-time) 5)) finished failure timer)
    (setq timer
          (run-at-time 0 nil
                       (lambda ()
                         (condition-case err
                             (funcall (car request)
                                      (or data markdown-ts-appear-math-test--svg))
                           (error (setq failure err)))
                         (setq finished t))))
    (unwind-protect
        (while (and (not finished) (< (float-time) deadline))
          (accept-process-output nil 0.01))
      (cancel-timer timer))
    (should finished)
    (when failure (signal (car failure) (cdr failure)))))

(defun markdown-ts-appear-math-test--overlays ()
  "Return only overlays owned by the current buffer's math previews."
  (seq-filter #'overlayp markdown-ts-appear-math--objects))

(defun markdown-ts-appear-math-test--move (position)
  "Move to POSITION and run normal command hooks, core first."
  (goto-char position)
  (run-hooks 'post-command-hook))

(ert-deftest markdown-ts-appear-math-test-opt-in-and-missing-package ()
  (markdown-ts-appear-math-test--buffer "$x$\n"
    (should-not markdown-ts-appear-enable-math-preview)
    (markdown-ts-appear-math-test--assert-clean)
    (let ((original (symbol-function 'require)))
      (cl-letf (((symbol-function 'require)
                 (lambda (feature &optional filename noerror)
                   (unless (eq feature 'mathjax)
                     (funcall original feature filename noerror)))))
        (should-error (markdown-ts-appear-math-test--enable) :type 'user-error)
        ;; Core setup deliberately does not roll back after an error.
        (should (markdown-ts-appear--active-p))
        (markdown-ts-appear-mode -1)
        (should-not (markdown-ts-appear--active-p))
        (markdown-ts-appear-math-test--assert-clean)
        (setq markdown-ts-appear-enable-math-preview nil)
        (markdown-ts-appear-mode 1)
        (font-lock-ensure)
        (run-hooks 'post-command-hook)
        (should (markdown-ts-appear--active-p))
        (should (get-char-property 1 'invisible))
        (markdown-ts-appear-math-test--assert-clean)))))

(ert-deftest markdown-ts-appear-math-test-missing-runtime-or-svg ()
  (markdown-ts-appear-math-test--deferred
    (dolist (unavailable '(mathjax-available-p image-type-available-p))
      (markdown-ts-appear-math-test--buffer "$x$\n"
        (cl-letf (((symbol-function unavailable) (lambda (&rest _) nil)))
          (should-error (markdown-ts-appear-math-test--enable) :type 'user-error)
          (should (markdown-ts-appear--active-p))
          (should-not markdown-ts-appear-math-test--callbacks)
          (markdown-ts-appear-mode -1)
          (markdown-ts-appear-math-test--assert-clean)
          (setq markdown-ts-appear-enable-math-preview nil)
          (markdown-ts-appear-mode 1)
          (should (markdown-ts-appear--active-p)))))))

(ert-deftest markdown-ts-appear-math-test-option-requires-core-reenable ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (setq markdown-ts-appear-enable-math-preview t)
      (markdown-ts-appear-mode 1)
      (run-hooks 'post-command-hook)
      (should-not markdown-ts-appear-math-test--callbacks)
      (markdown-ts-appear-math-test--assert-clean)
      (markdown-ts-appear-math-test--enable)
      (should (= 1 (length markdown-ts-appear-math-test--callbacks)))
      (should (memq #'markdown-ts-appear-math--refresh post-command-hook))
      (should (memq #'markdown-ts-appear-math--clear before-change-functions))
      (should (memq #'markdown-ts-appear-math--refresh outline-view-change-hook))
      (markdown-ts-appear-mode -1)
      (setq markdown-ts-appear-enable-math-preview nil)
      (markdown-ts-appear-mode 1)
      (markdown-ts-appear-math-test--assert-clean)
      (markdown-ts-appear-math-test--deliver
       (pop markdown-ts-appear-math-test--callbacks))
      (should-not markdown-ts-appear-math--objects)
      (should (markdown-ts-appear--active-p)))))

(ert-deftest markdown-ts-appear-math-test-option-off-rejects-late-results ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$ and $y$\n"
      (markdown-ts-appear-math-test--enable)
      (markdown-ts-appear-math-test--deliver
       (pop markdown-ts-appear-math-test--callbacks))
      (let ((overlay (car (markdown-ts-appear-math-test--overlays))))
        (should overlay)
        (setq markdown-ts-appear-enable-math-preview nil)
        ;; Eligibility must reject late results even before the next refresh.
        (markdown-ts-appear-math-test--deliver
         (pop markdown-ts-appear-math-test--callbacks))
        (should (equal (markdown-ts-appear-math-test--overlays) (list overlay)))
        (run-hooks 'post-command-hook)
        (should-not (overlay-buffer overlay))
        (should-not markdown-ts-appear-math--objects)
        (should-not markdown-ts-appear-math--sources)
        (should (markdown-ts-appear--active-p))
        (markdown-ts-appear-mode -1)
        (markdown-ts-appear-math-test--assert-clean)))))

(ert-deftest markdown-ts-appear-math-test-delimiters-and-syntax ()
  (markdown-ts-appear-math-test--deferred
    (dolist (case '(("$x$\n" "x" nil) ("$$x$$\n" "x" t)
                    ("$$x\n+y$$\n" "x\n+y" t)))
      (markdown-ts-appear-math-test--buffer (car case)
        (setq markdown-ts-appear-math-test--callbacks nil)
        (markdown-ts-appear-math-test--enable)
        (should (= 1 (length markdown-ts-appear-math-test--callbacks)))
        (pcase-let ((`(,_ ,math ,options)
                     (car markdown-ts-appear-math-test--callbacks)))
          (should (equal math (nth 1 case)))
          (should (eq (plist-get (plist-get options :options) :display)
                      (nth 2 case))))))
    (dolist (content '("`$x$`\n" "```tex\n$x$\n```\n" "    $x$\n"
                       "$x\n\n y$\n" "\\$x\n"))
      (markdown-ts-appear-math-test--buffer content
        (setq markdown-ts-appear-math-test--callbacks nil)
        (markdown-ts-appear-math-test--enable)
        (should-not markdown-ts-appear-math-test--callbacks)))))

(ert-deftest markdown-ts-appear-math-test-success-reveal-and-deduplication ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "text $x$ more\n"
      (markdown-ts-appear-math-test--enable)
      (dotimes (_ 3)
        (font-lock-flush 1 2)
        (font-lock-ensure 1 2)
        (run-hooks 'post-command-hook))
      (should (= 1 (length markdown-ts-appear-math-test--callbacks)))
      (markdown-ts-appear-math-test--deliver
       (pop markdown-ts-appear-math-test--callbacks))
      (let ((overlay (car (markdown-ts-appear-math-test--overlays))))
        (should (eq (car (overlay-get overlay 'display)) 'image))
        (should (= (overlay-start overlay) 6))
        (markdown-ts-appear-math-test--move 7)
        (should-not (overlay-buffer overlay))
        (should-not (get-char-property 6 'invisible))
        (should-not (get-char-property 7 'display))
        (markdown-ts-appear-math-test--move (point-max))
        (should (= 1 (length markdown-ts-appear-math-test--callbacks)))
        (markdown-ts-appear-math-test--deliver
         (pop markdown-ts-appear-math-test--callbacks))
        (should (= 1 (length (markdown-ts-appear-math-test--overlays))))))))

(ert-deftest markdown-ts-appear-math-test-late-callback-after-point-moves ()
  (markdown-ts-appear-math-test--deferred
    (dolist (position '(2 3 4))
      (markdown-ts-appear-math-test--buffer "$x$\n"
        (markdown-ts-appear-math-test--enable)
        ;; Core treats the position after the closing delimiter as outside.
        (goto-char position)
        (markdown-ts-appear-math-test--deliver
         (pop markdown-ts-appear-math-test--callbacks))
        (if (= position 4)
            (should (markdown-ts-appear-math-test--overlays))
          (should-not (markdown-ts-appear-math-test--overlays)))
        (run-hooks 'post-command-hook)
        (if (= position 4)
            (should (get-char-property 1 'invisible))
          (should-not (get-char-property 1 'invisible)))
        (markdown-ts-appear-math-test--move (point-max))
        (should (= (if (= position 4) 0 1)
                   (length markdown-ts-appear-math-test--callbacks)))
        (setq markdown-ts-appear-math-test--callbacks nil)))))

(ert-deftest markdown-ts-appear-math-test-edit-and-out-of-order-results ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (markdown-ts-appear-math-test--enable)
      (let ((old (pop markdown-ts-appear-math-test--callbacks)))
        (goto-char 2)
        (insert "y")
        (should-not markdown-ts-appear-math--objects)
        (markdown-ts-appear-math-test--move (point-max))
        (let ((new (pop markdown-ts-appear-math-test--callbacks)))
          (should (equal (cadr new) "yx"))
          (markdown-ts-appear-math-test--deliver new)
          (let ((overlay (car (markdown-ts-appear-math-test--overlays))))
            (markdown-ts-appear-math-test--deliver old)
            (should (eq (overlay-buffer overlay) (current-buffer)))
            (should (= 1 (length (markdown-ts-appear-math-test--overlays))))))))))

(ert-deftest markdown-ts-appear-math-test-source-check-with-inhibited-hooks ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (markdown-ts-appear-math-test--enable)
      (let ((inhibit-modification-hooks t))
        (goto-char 2)
        (delete-char 1)
        (insert "y"))
      (goto-char (point-max))
      (markdown-ts-appear-math-test--deliver
       (pop markdown-ts-appear-math-test--callbacks))
      (should-not (markdown-ts-appear-math-test--overlays)))))

(ert-deftest markdown-ts-appear-math-test-preserves-foreign-overlays ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (let ((foreign (make-overlay 1 4)))
        (overlay-put foreign 'category 'mathjax)
        (overlay-put foreign 'display "foreign")
        (markdown-ts-appear-math-test--enable)
        (markdown-ts-appear-math-test--deliver
         (pop markdown-ts-appear-math-test--callbacks))
        (should (overlay-buffer foreign))
        (narrow-to-region 4 (point-max))
        (markdown-ts-appear-mode -1)
        (should (eq (overlay-buffer foreign) (current-buffer)))
        (should (equal (overlay-get foreign 'display) "foreign"))
        (should-not markdown-ts-appear-math--objects)))))

(ert-deftest markdown-ts-appear-math-test-async-error-does-not-retry-on-fontification ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (markdown-ts-appear-math-test--enable)
      (markdown-ts-appear-math-test--deliver
       (pop markdown-ts-appear-math-test--callbacks) '((error . "Bad TeX")))
      (let ((overlay (car (markdown-ts-appear-math-test--overlays))))
        (should (equal (overlay-get overlay 'mathjax-error) "Bad TeX"))
        (should-not (overlay-get overlay 'display))
        (font-lock-flush)
        (font-lock-ensure)
        (run-hooks 'post-command-hook)
        (should-not markdown-ts-appear-math-test--callbacks)
        (markdown-ts-appear-math-test--move 2)
        (should-not (overlay-buffer overlay))))))

(ert-deftest markdown-ts-appear-math-test-synchronous-error-cleans-staging ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (cl-letf (((symbol-function 'mathjax-render)
                 (lambda (&rest _) (error "Transport unavailable"))))
        (markdown-ts-appear-math-test--enable))
      (should-not markdown-ts-appear-math--objects)
      (run-hooks 'post-command-hook)
      (should-not markdown-ts-appear-math-test--callbacks))))

(ert-deftest markdown-ts-appear-math-test-lifecycle-rejects-late-results ()
  (markdown-ts-appear-math-test--deferred
    (dolist (action '(disable major-mode kill))
      (markdown-ts-appear-math-test--buffer "$x$\n"
        (markdown-ts-appear-math-test--enable)
        (let ((target (current-buffer))
              (staging (car markdown-ts-appear-math--objects))
              (request (pop markdown-ts-appear-math-test--callbacks)))
          (pcase action
            ('disable (markdown-ts-appear-mode -1))
            ('major-mode (text-mode))
            ('kill (kill-buffer target)))
          (should-not (buffer-live-p staging))
          (when (buffer-live-p target)
            (should-not markdown-ts-appear-mode)
            (markdown-ts-appear-math-test--assert-clean)
            (font-lock-ensure)
            (run-hooks 'post-command-hook)
            (should-not markdown-ts-appear-math-test--callbacks))
          (markdown-ts-appear-math-test--deliver request)
          (when (buffer-live-p target)
            (should-not (get-char-property 2 'display))))))))

(ert-deftest markdown-ts-appear-math-test-disable-reenable-generation ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (markdown-ts-appear-math-test--enable)
      (let ((old (pop markdown-ts-appear-math-test--callbacks))
            (staging (car markdown-ts-appear-math--objects))
            (original-kill (symbol-function 'kill-buffer)))
        ;; Keep the old staging buffer alive deliberately: the library's
        ;; buffer-live-p guard must not be the only stale-result protection.
        (cl-letf (((symbol-function 'kill-buffer)
                   (lambda (&optional buffer)
                     (unless (eq buffer staging) (funcall original-kill buffer)))))
          (markdown-ts-appear-mode -1))
        (markdown-ts-appear-mode 1)
        (markdown-ts-appear-math-test--deliver
         (pop markdown-ts-appear-math-test--callbacks))
        (markdown-ts-appear-math-test--deliver old)
        (should-not (buffer-live-p staging))
        (should (= 1 (length (markdown-ts-appear-math-test--overlays))))))))

(ert-deftest markdown-ts-appear-math-test-independent-buffers-and-clone ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (markdown-ts-appear-math-test--enable)
      (let ((staging (car markdown-ts-appear-math--objects))
            (request (pop markdown-ts-appear-math-test--callbacks))
            (clone (clone-indirect-buffer " *math-clone*" nil)))
        (unwind-protect
            (with-current-buffer clone
              (should-not markdown-ts-appear-mode)
              (markdown-ts-appear-math-test--assert-clean))
          (kill-buffer clone))
        (should (buffer-live-p staging))
        (markdown-ts-appear-math-test--buffer "$y$\n"
          (markdown-ts-appear-math-test--enable)
          (markdown-ts-appear-mode -1))
        (should (buffer-live-p staging))
        (markdown-ts-appear-math-test--deliver request)
        (should (= 1 (length (markdown-ts-appear-math-test--overlays))))))))

(ert-deftest markdown-ts-appear-math-test-respects-paused-tracking ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (markdown-ts-appear-math-test--enable)
      (markdown-ts-appear-stop)
      (markdown-ts-appear-math-test--move 2)
      (markdown-ts-appear-math-test--deliver
       (pop markdown-ts-appear-math-test--callbacks))
      (let ((overlay (car (markdown-ts-appear-math-test--overlays))))
        (should overlay)
        (should (eq (car (overlay-get overlay 'display)) 'image))
        (markdown-ts-appear-math-test--move (point-max))
        (markdown-ts-appear-math-test--move 2)
        (should (overlay-buffer overlay))
        (should-not markdown-ts-appear-math-test--callbacks)
        (markdown-ts-appear-start)
        (run-hooks 'post-command-hook)
        (should-not (overlay-buffer overlay))
        (should-not (get-char-property 1 'invisible))))))

(ert-deftest markdown-ts-appear-math-test-unload-cleans-pending-and-displayed ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$ and $y$\n"
      (unwind-protect
          (progn
            (markdown-ts-appear-math-test--enable)
            (markdown-ts-appear-math-test--deliver
             (pop markdown-ts-appear-math-test--callbacks))
            (let ((objects (copy-sequence markdown-ts-appear-math--objects))
                  (request (pop markdown-ts-appear-math-test--callbacks)))
              (should (seq-some #'bufferp objects))
              (should (seq-some #'overlayp objects))
              (markdown-ts-appear-unload-function)
              (should-not markdown-ts-appear-mode)
              (markdown-ts-appear-math-test--assert-clean)
              (dolist (binding (markdown-ts-appear--advice-bindings))
                (should-not (advice-member-p (cdr binding) (car binding))))
              (dolist (object objects)
                (should-not (if (bufferp object)
                                (buffer-live-p object)
                              (overlay-buffer object))))
              (markdown-ts-appear-math-test--deliver request)
              (should-not markdown-ts-appear-math--objects)))
        (markdown-ts-appear--install-advice)))))

(ert-deftest markdown-ts-appear-math-test-real-display-smoke ()
  (skip-unless (and (require 'mathjax nil t) (mathjax-available-p)
                    (image-type-available-p 'svg)))
  (markdown-ts-appear-math-test--buffer "$x^2+1$\n"
    (markdown-ts-appear-math-test--enable)
    (let ((deadline (+ (float-time) 15)))
      (while (and (not (markdown-ts-appear-math-test--overlays))
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (let ((overlay (car (markdown-ts-appear-math-test--overlays))))
      (should overlay)
      (should (eq (car (overlay-get overlay 'display)) 'image))
      (should-not (overlay-get overlay 'mathjax-error))
      (markdown-ts-appear-math-test--move 3)
      (should-not (overlay-buffer overlay))
      (should-not (get-char-property 1 'invisible))
      (markdown-ts-appear-mode -1))))

(ert-deftest markdown-ts-appear-math-test-real-display-error-smoke ()
  (skip-unless (and (require 'mathjax nil t) (mathjax-available-p)
                    (image-type-available-p 'svg)))
  (markdown-ts-appear-math-test--buffer "$x$\n"
    (let ((display (symbol-function 'mathjax-display)))
      ;; Invalid TeX may be rendered as an SVG error message by MathJax.
      ;; An invalid format reliably exercises its actual error response.
      (cl-letf (((symbol-function 'mathjax-display)
                 (lambda (beg end math &rest args)
                   (apply display beg end math :format 'invalid args))))
        (markdown-ts-appear-math-test--enable)))
    (let ((deadline (+ (float-time) 15)))
      (while (and (not (markdown-ts-appear-math-test--overlays))
                  (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (let ((overlay (car (markdown-ts-appear-math-test--overlays))))
      (should overlay)
      (should (stringp (overlay-get overlay 'mathjax-error)))
      (should-not (overlay-get overlay 'display))
      (markdown-ts-appear-mode -1)
      (should-not (overlay-buffer overlay)))))

(ert-deftest markdown-ts-appear-math-test-real-display-late-cleanup-smoke ()
  (skip-unless (and (require 'mathjax nil t) (mathjax-available-p)
                    (image-type-available-p 'svg)))
  (dolist (action '(edit disable kill))
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (let ((render (symbol-function 'mathjax-render))
            (target (current-buffer)) result)
        ;; Observe the real transport without replacing its response or the
        ;; public display callback, even when staging is already dead.
        (cl-letf (((symbol-function 'mathjax-render)
                   (lambda (callback math &rest args)
                     (apply render
                            (lambda (data)
                              (funcall callback data)
                              (setq result data))
                            math args))))
          (markdown-ts-appear-math-test--enable))
        (let ((staging (car markdown-ts-appear-math--objects)))
          (should-not result)
          (should (bufferp staging))
          (pcase action
            ('edit (goto-char 2) (insert "y"))
            ('disable (markdown-ts-appear-mode -1))
            ('kill (kill-buffer target)))
          (should-not (buffer-live-p staging))
          (let ((deadline (+ (float-time) 15)))
            (while (and (not result) (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should (alist-get 'svg result))
          (when (buffer-live-p target)
            (with-current-buffer target
              (should-not markdown-ts-appear-math--objects)
              (should-not (get-char-property 2 'display)))))))))

(provide 'markdown-ts-appear-math-test)
;;; markdown-ts-appear-math-test.el ends here
