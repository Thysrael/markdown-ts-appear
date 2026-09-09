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
  "Return completed previews, including those temporarily showing source."
  (seq-filter
   (lambda (preview)
     (or (overlay-get preview 'markdown-ts-appear-math--image)
         (overlay-get preview 'mathjax-error)))
   markdown-ts-appear-math--objects))

(defun markdown-ts-appear-math-test--pending-buffer ()
  "Return a pending render buffer, if any."
  (seq-some (lambda (preview)
              (overlay-get preview 'markdown-ts-appear-math--buffer))
            markdown-ts-appear-math--objects))

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
        (should (overlay-buffer overlay))
        (should-not (overlay-get overlay 'display))
        (should-not (get-char-property 6 'invisible))
        (should-not (get-char-property 7 'display))
        (markdown-ts-appear-math-test--move (point-max))
        (should-not markdown-ts-appear-math-test--callbacks)
        (should (eq (car (overlay-get overlay 'display)) 'image))
        (should (equal (markdown-ts-appear-math-test--overlays) (list overlay)))))))

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
            (should (get-char-property 2 'display))
          (should-not (get-char-property 2 'display)))
        (run-hooks 'post-command-hook)
        (if (= position 4)
            (should (get-char-property 1 'invisible))
          (should-not (get-char-property 1 'invisible)))
        (markdown-ts-appear-math-test--move (point-max))
        (should (eq (car (get-char-property 2 'display)) 'image))
        (should-not markdown-ts-appear-math-test--callbacks)
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
        (should (overlay-buffer overlay))
        (should-not (overlay-get overlay 'face))
        (should-not (get-char-property 2 'display))))))

(ert-deftest markdown-ts-appear-math-test-synchronous-error-cleans-staging ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (cl-letf (((symbol-function 'mathjax-render)
                 (lambda (&rest _) (error "Transport unavailable"))))
        (markdown-ts-appear-math-test--enable))
      (should-not (markdown-ts-appear-math-test--overlays))
      (should-not (markdown-ts-appear-math-test--pending-buffer))
      (run-hooks 'post-command-hook)
      (should-not markdown-ts-appear-math-test--callbacks))))

(ert-deftest markdown-ts-appear-math-test-synchronous-results-preserve-node-traversal ()
  (markdown-ts-appear-math-test--deferred
    (dolist (result (list markdown-ts-appear-math-test--svg '((error . "Bad TeX"))))
      (markdown-ts-appear-math-test--buffer "$x$ and $y$ and $z$\n"
        (let ((calls 0))
          (cl-letf (((symbol-function 'mathjax-render)
                     (lambda (callback _math &rest _)
                       (setq calls (1+ calls))
                       (funcall callback result))))
            (markdown-ts-appear-math-test--enable)
            (dotimes (_ 5)
              (font-lock-flush)
              (font-lock-ensure)
              (run-hooks 'post-command-hook))
            (should (= calls 3))
            (should (= (length (markdown-ts-appear-math-test--overlays)) 3))
            (should-not (markdown-ts-appear-math-test--pending-buffer))))))))

(ert-deftest markdown-ts-appear-math-test-lifecycle-rejects-late-results ()
  (markdown-ts-appear-math-test--deferred
    (dolist (action '(disable major-mode kill))
      (markdown-ts-appear-math-test--buffer "$x$\n"
        (markdown-ts-appear-math-test--enable)
        (let ((target (current-buffer))
              (staging (markdown-ts-appear-math-test--pending-buffer))
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

(ert-deftest markdown-ts-appear-math-test-disable-reenable-rejects-old-request ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$\n"
      (markdown-ts-appear-math-test--enable)
      (let ((old (pop markdown-ts-appear-math-test--callbacks))
            (staging (markdown-ts-appear-math-test--pending-buffer))
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
      (let ((staging (markdown-ts-appear-math-test--pending-buffer))
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
        (should (overlay-buffer overlay))
        (should-not (overlay-get overlay 'display))
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
                  (staging (markdown-ts-appear-math-test--pending-buffer))
                  (request (pop markdown-ts-appear-math-test--callbacks)))
              (should (buffer-live-p staging))
              (should (markdown-ts-appear-math-test--overlays))
              (markdown-ts-appear-unload-function)
              (should-not markdown-ts-appear-mode)
              (markdown-ts-appear-math-test--assert-clean)
              (dolist (binding (markdown-ts-appear--advice-bindings))
                (should-not (advice-member-p (cdr binding) (car binding))))
              (dolist (object objects)
                (should-not (overlay-buffer object)))
              (should-not (buffer-live-p staging))
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
      (should (overlay-buffer overlay))
      (should-not (overlay-get overlay 'display))
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
        (let ((staging (markdown-ts-appear-math-test--pending-buffer)))
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

(ert-deftest markdown-ts-appear-math-test-multi-formula-reveal-preserves-cached-overlays ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "before $x$ between $y$ and $z$ after\n"
      (markdown-ts-appear-math-test--enable)
      (should (= 3 (length markdown-ts-appear-math-test--callbacks)))
      (while markdown-ts-appear-math-test--callbacks
        (markdown-ts-appear-math-test--deliver
         (pop markdown-ts-appear-math-test--callbacks)))
      (let* ((overlays (seq-sort-by #'overlay-start #'<
                                    (markdown-ts-appear-math-test--overlays)))
             (images (mapcar (lambda (overlay) (overlay-get overlay 'display))
                             overlays))
             (x (car overlays)))
        (should (= 3 (length overlays)))
        (should (equal (mapcar (lambda (overlay)
                                 (buffer-substring-no-properties
                                  (overlay-start overlay) (overlay-end overlay)))
                               overlays)
                       '("$x$" "$y$" "$z$")))
        (dolist (image images)
          (should (eq (car image) 'image)))
        (markdown-ts-appear-math-test--move (1+ (overlay-start x)))
        (cl-mapc (lambda (overlay image)
                   (should (eq (overlay-buffer overlay) (current-buffer)))
                   (should (memq overlay (markdown-ts-appear-math-test--overlays)))
                   (should (eq (overlay-get overlay 'display) image)))
                 (cdr overlays) (cdr images))
        (should (eq (overlay-buffer x) (current-buffer)))
        (should (memq x (markdown-ts-appear-math-test--overlays)))
        (should-not (overlay-get x 'display))
        (should-not (get-char-property (overlay-start x) 'invisible))
        (should-not (get-char-property (point) 'display))
        (should-not markdown-ts-appear-math-test--callbacks)
        (markdown-ts-appear-math-test--move (point-max))
        (should (equal (seq-sort-by #'overlay-start #'<
                                    (markdown-ts-appear-math-test--overlays))
                       overlays))
        (cl-mapc (lambda (overlay image)
                   (should (eq (overlay-get overlay 'display) image))
                   (should (eq (get-char-property (overlay-start overlay) 'display)
                               image)))
                 overlays images)
        (should-not markdown-ts-appear-math-test--callbacks)
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       "before $x$ between $y$ and $z$ after\n"))))))

(ert-deftest markdown-ts-appear-math-test-multi-formula-reveal-preserves-pending-request ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "before $x$ and $y$ after\n"
      (markdown-ts-appear-math-test--enable)
      (should (= 2 (length markdown-ts-appear-math-test--callbacks)))
      (let ((x-request (seq-find (lambda (request) (equal (cadr request) "x"))
                                 markdown-ts-appear-math-test--callbacks))
            (y-request (seq-find (lambda (request) (equal (cadr request) "y"))
                                 markdown-ts-appear-math-test--callbacks)))
        (should x-request)
        (should y-request)
        (setq markdown-ts-appear-math-test--callbacks nil)
        (markdown-ts-appear-math-test--deliver x-request)
        (should (= 1 (length (markdown-ts-appear-math-test--overlays))))
        (let* ((x (car (markdown-ts-appear-math-test--overlays)))
               (image (overlay-get x 'display))
               (y-object (seq-find (lambda (object) (not (eq object x)))
                                   markdown-ts-appear-math--objects)))
          (should (eq (car image) 'image))
          (should (= 2 (length markdown-ts-appear-math--objects)))
          (should y-object)
          (markdown-ts-appear-math-test--move (1+ (overlay-start x)))
          (should (memq y-object markdown-ts-appear-math--objects))
          (should-not markdown-ts-appear-math-test--callbacks)
          (should (eq (overlay-buffer x) (current-buffer)))
          (should-not (overlay-get x 'display))
          (markdown-ts-appear-math-test--move (point-max))
          (should (memq y-object markdown-ts-appear-math--objects))
          (should (eq (overlay-get x 'display) image))
          (should-not markdown-ts-appear-math-test--callbacks)
          ;; Deliver the original request, not a replacement from a refresh.
          (markdown-ts-appear-math-test--deliver y-request)
          (let ((overlays (seq-sort-by #'overlay-start #'<
                                       (markdown-ts-appear-math-test--overlays))))
            (should (= 2 (length overlays)))
            (should (eq (car overlays) x))
            (should (eq (cadr overlays) y-object))
            (should (equal (buffer-substring-no-properties
                            (overlay-start y-object) (overlay-end y-object))
                           "$y$"))
            (should (eq (car (overlay-get y-object 'display)) 'image))
            (should (eq (get-char-property (overlay-start y-object) 'display)
                        (overlay-get y-object 'display)))
            (should (eq (overlay-get x 'display) image)))
          (should-not markdown-ts-appear-math-test--callbacks))))))

(ert-deftest markdown-ts-appear-math-test-multi-formula-edit-preserves-pending-request ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "$x$ and $y$\n"
      (markdown-ts-appear-math-test--enable)
      (let ((x-request (seq-find (lambda (request) (equal (cadr request) "x"))
                                 markdown-ts-appear-math-test--callbacks))
            (y-request (seq-find (lambda (request) (equal (cadr request) "y"))
                                 markdown-ts-appear-math-test--callbacks)))
        (setq markdown-ts-appear-math-test--callbacks nil)
        (goto-char 2)
        (insert "x")
        ;; Complete y at its shifted position before any post-command refresh.
        (markdown-ts-appear-math-test--deliver y-request)
        (markdown-ts-appear-math-test--deliver x-request)
        (let* ((y (car (markdown-ts-appear-math-test--overlays)))
               (image (overlay-get y 'display)))
          (should (eq (car image) 'image))
          (should (equal (buffer-substring-no-properties
                          (overlay-start y) (overlay-end y)) "$y$"))
          (run-hooks 'post-command-hook)
          (should-not markdown-ts-appear-math-test--callbacks)
          (markdown-ts-appear-math-test--move (point-max))
          (should (equal (mapcar #'cadr markdown-ts-appear-math-test--callbacks)
                         '("xx")))
          (should (eq (overlay-get y 'display) image)))))))

(ert-deftest markdown-ts-appear-math-test-multi-formula-edit-preserves-unrelated-preview ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "before $x$ and $y$ after\n"
      (markdown-ts-appear-math-test--enable)
      (should (= 2 (length markdown-ts-appear-math-test--callbacks)))
      (while markdown-ts-appear-math-test--callbacks
        (markdown-ts-appear-math-test--deliver
         (pop markdown-ts-appear-math-test--callbacks)))
      (let* ((overlays (seq-sort-by #'overlay-start #'<
                                    (markdown-ts-appear-math-test--overlays)))
             (x (car overlays))
             (y (cadr overlays))
             (x-start (overlay-start x))
             (y-start (overlay-start y))
             (y-end (overlay-end y))
             (y-image (overlay-get y 'display)))
        (should (= 2 (length overlays)))
        (should (eq (car (overlay-get x 'display)) 'image))
        (should (eq (car y-image) 'image))
        ;; Do not refresh on entry: isolate invalidation by the edit hooks.
        (goto-char (1- (overlay-end x)))
        (insert "+1")
        (should (eq (overlay-buffer y) (current-buffer)))
        (should (memq y markdown-ts-appear-math--objects))
        (should (= (overlay-start y) (+ y-start 2)))
        (should (= (overlay-end y) (+ y-end 2)))
        (should (eq (overlay-get y 'display) y-image))
        (should-not (get-char-property (1+ x-start) 'display))
        (font-lock-ensure)
        (markdown-ts-appear-math-test--move (point-max))
        (should (equal (mapcar #'cadr markdown-ts-appear-math-test--callbacks)
                       '("x+1")))
        (should (memq y (markdown-ts-appear-math-test--overlays)))
        (should (eq (overlay-get y 'display) y-image))
        (markdown-ts-appear-math-test--deliver
         (pop markdown-ts-appear-math-test--callbacks))
        (let ((updated (seq-sort-by #'overlay-start #'<
                                    (markdown-ts-appear-math-test--overlays))))
          (should (= 2 (length updated)))
          (should (eq (cadr updated) y))
          (should (= (overlay-start (car updated)) x-start))
          (should (equal (mapcar (lambda (overlay)
                                   (buffer-substring-no-properties
                                    (overlay-start overlay) (overlay-end overlay)))
                                 updated)
                         '("$x+1$" "$y$")))
          (should (eq (car (overlay-get (car updated) 'display)) 'image)))
        (should (eq (get-char-property (overlay-start y) 'display) y-image))
        (should-not markdown-ts-appear-math-test--callbacks)
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       "before $x+1$ and $y$ after\n"))))))

(ert-deftest markdown-ts-appear-math-test-multi-formula-prefix-insertion-preserves-previews ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "before $x$ and $y$ after\n"
      (markdown-ts-appear-math-test--enable)
      (should (= 2 (length markdown-ts-appear-math-test--callbacks)))
      (while markdown-ts-appear-math-test--callbacks
        (markdown-ts-appear-math-test--deliver
         (pop markdown-ts-appear-math-test--callbacks)))
      (let* ((overlays (seq-sort-by #'overlay-start #'<
                                    (markdown-ts-appear-math-test--overlays)))
             (images (mapcar (lambda (overlay) (overlay-get overlay 'display))
                             overlays))
             (starts (mapcar #'overlay-start overlays))
             (ends (mapcar #'overlay-end overlays))
             (prefix "intro\n\n"))
        (should (= 2 (length overlays)))
        (dolist (image images)
          (should (eq (car image) 'image)))
        (goto-char (point-min))
        (insert prefix)
        (dolist (refresh '(nil t))
          (ert-info ((if refresh "After refresh" "Before refresh"))
            (when refresh
              (font-lock-ensure)
              (markdown-ts-appear-math-test--move (point-max)))
            (cl-mapc (lambda (overlay image start end)
                       (should (eq (overlay-buffer overlay) (current-buffer)))
                       (should (memq overlay markdown-ts-appear-math--objects))
                       (should (= (overlay-start overlay) (+ start (length prefix))))
                       (should (= (overlay-end overlay) (+ end (length prefix))))
                       (should (eq (overlay-get overlay 'display) image))
                       (should (eq (get-char-property (overlay-start overlay) 'display)
                                   image)))
                     overlays images starts ends)
            (should (equal (seq-sort-by #'overlay-start #'<
                                        (markdown-ts-appear-math-test--overlays))
                           overlays))
            (should (equal (mapcar (lambda (overlay)
                                     (buffer-substring-no-properties
                                      (overlay-start overlay) (overlay-end overlay)))
                                   overlays)
                           '("$x$" "$y$")))
            (should-not markdown-ts-appear-math-test--callbacks)))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       "intro\n\nbefore $x$ and $y$ after\n"))))))

(ert-deftest markdown-ts-appear-math-test-multi-formula-code-context-invalidates-only-affected-preview ()
  (markdown-ts-appear-math-test--deferred
    (markdown-ts-appear-math-test--buffer "lead\n\n$x$\n\n$y$\n\ntail\n"
      (markdown-ts-appear-math-test--enable)
      (should (= 2 (length markdown-ts-appear-math-test--callbacks)))
      (while markdown-ts-appear-math-test--callbacks
        (markdown-ts-appear-math-test--deliver
         (pop markdown-ts-appear-math-test--callbacks)))
      (let* ((overlays (seq-sort-by #'overlay-start #'<
                                    (markdown-ts-appear-math-test--overlays)))
             (x (car overlays))
             (y (cadr overlays))
             (x-start (overlay-start x))
             (y-start (overlay-start y))
             (y-image (overlay-get y 'display)))
        (should (= 2 (length overlays)))
        (should (eq (car (overlay-get x 'display)) 'image))
        (should (eq (car y-image) 'image))
        ;; Change only context: the unchanged $x$ becomes an indented code block.
        (goto-char x-start)
        (insert "    ")
        (font-lock-ensure)
        (markdown-ts-appear-math-test--move (point-max))
        (should-not (overlay-buffer x))
        (should-not (memq x markdown-ts-appear-math--objects))
        (should-not (get-char-property (+ x-start 4) 'display))
        (should (eq (overlay-buffer y) (current-buffer)))
        (should (equal (markdown-ts-appear-math-test--overlays) (list y)))
        (should (equal markdown-ts-appear-math--objects (list y)))
        (should (= (overlay-start y) (+ y-start 4)))
        (should (equal (buffer-substring-no-properties
                        (overlay-start y) (overlay-end y))
                       "$y$"))
        (should (eq (overlay-get y 'display) y-image))
        (should (eq (get-char-property (overlay-start y) 'display) y-image))
        (should-not markdown-ts-appear-math-test--callbacks)
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       "lead\n\n    $x$\n\n$y$\n\ntail\n"))))))

(provide 'markdown-ts-appear-math-test)
;;; markdown-ts-appear-math-test.el ends here
