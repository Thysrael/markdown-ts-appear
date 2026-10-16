;;; markdown-ts-appear-diagram-test.el --- Renderer interface tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'markdown-ts-appear)

(defconst markdown-ts-appear-diagram-test--svg
  "<svg xmlns='http://www.w3.org/2000/svg' width='32' height='16'><rect width='32' height='16'/></svg>")

(defmacro markdown-ts-appear-diagram-test--isolated (&rest body)
  "Run BODY with isolated diagram state and a controlled display context."
  (declare (indent 0))
  `(let ((markdown-ts-appear-diagram--cache
          (make-hash-table :test 'markdown-ts-appear-diagram--key-equal))
         markdown-ts-appear-diagram--queue markdown-ts-appear-diagram--job
         markdown-ts-appear-diagram--cancel
         (markdown-ts-appear-enable-math-preview nil)
         (markdown-ts-inline-images nil))
     (cl-letf (((symbol-function 'image-type-available-p) (lambda (_) t))
               ((symbol-function 'display-images-p) (lambda (&rest _) t)))
       (save-window-excursion ,@body))))

(defmacro markdown-ts-appear-diagram-test--buffer (text &rest body)
  "Show TEXT in a fresh Markdown buffer and evaluate BODY with previews enabled."
  (declare (indent 1))
  `(let ((buffer (generate-new-buffer " *diagram-test*")))
     (unwind-protect
         (with-current-buffer buffer
           (switch-to-buffer buffer)
           (insert ,text)
           (markdown-ts-mode)
           (goto-char (point-max))
           (markdown-ts-appear-mode 1)
           (font-lock-ensure)
           ,@body)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (when markdown-ts-appear-mode (markdown-ts-appear-mode -1)))
         (kill-buffer buffer)))))

(defun markdown-ts-appear-diagram-test--view ()
  "Return the current diagram's view."
  (car (overlay-get (car markdown-ts-appear-diagram--objects)
                    'markdown-ts-appear-diagram--views)))

(ert-deftest markdown-ts-appear-diagram-test-language-dispatch-and-reveal ()
  (markdown-ts-appear-diagram-test--isolated
    (let* (calls
           (find-executable (symbol-function 'executable-find))
           (renderer (lambda (language code theme callback)
                       (push (list language code theme) calls)
                       (funcall callback markdown-ts-appear-diagram-test--svg)))
           (markdown-ts-appear-diagram-renderers `(("d2" . ,renderer) ("dot" . ,renderer))))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (command &optional remote)
                   (should-not (equal command "mmdc"))
                   (funcall find-executable command remote))))
        (markdown-ts-appear-diagram-test--buffer
            "```D2\nx -> y\n```\n\n```dot\nx -> y\n```\n\n```python\nx -> y\n```\n\nAfter\n"
          (should (= 2 (length calls)))
          (should (equal (sort (mapcar #'car calls) #'string-lessp) '("d2" "dot")))
          (should (cl-every (lambda (call) (equal (cadr call) "x -> y\n")) calls))
          (should (= 2 (length markdown-ts-appear-diagram--objects)))
          (should (overlay-get (markdown-ts-appear-diagram-test--view) 'display))
          (goto-char (point-min)) (search-forward "x ->")
          (run-hooks 'post-command-hook)
          (should-not (overlay-get (markdown-ts-appear-diagram-test--view) 'display))
          (goto-char (point-max)) (run-hooks 'post-command-hook)
          (should (overlay-get (markdown-ts-appear-diagram-test--view) 'display))
          (should (= 2 (length calls))))))))

(ert-deftest markdown-ts-appear-diagram-test-stateful-renderer-cache ()
  (markdown-ts-appear-diagram-test--isolated
    (let* (calls
           (renderer (lambda (_language code _theme callback)
                       (push code calls)
                       (funcall callback markdown-ts-appear-diagram-test--svg)))
           (markdown-ts-appear-diagram-renderers `(("d2" . ,renderer))))
      (markdown-ts-appear-diagram-test--buffer "```d2\nfirst\n```\n\nAfter\n"
        (dolist (replacement '("second" "first"))
          (goto-char (point-min)) (forward-line)
          (delete-region (point) (line-end-position))
          (insert replacement)
          (goto-char (point-max)) (run-hooks 'post-command-hook))
        (should (equal (reverse calls) '("first\n" "second\n")))
        (cl-letf (((symbol-function 'markdown-ts-appear-diagram--scan)
                   (lambda (&rest _) (ert-fail "Unchanged motion rescanned fences")))
                  ((symbol-function 'create-image)
                   (lambda (&rest _) (ert-fail "Cached motion recreated an image"))))
          (dotimes (_ 20) (run-hooks 'post-command-hook)))))))

(ert-deftest markdown-ts-appear-diagram-test-errors-and-duplicate-callback ()
  (dolist (result '(callback-error signal quit decode-error duplicate))
    (markdown-ts-appear-diagram-test--isolated
      (let* (done
             (markdown-ts-appear-diagram-renderers
              `(("d2" . ,(lambda (_language _code _theme callback)
                            (setq done callback)
                            (pcase result
                              ('signal (error "Renderer failed"))
                              ('quit (signal 'quit nil))))))))
        (markdown-ts-appear-diagram-test--buffer "```d2\nx -> y\n```\n\nAfter\n"
          (pcase result
            ('callback-error (funcall done nil "Bad diagram"))
            ('decode-error
             (cl-letf (((symbol-function 'create-image)
                        (lambda (&rest _) (error "Image construction failed"))))
               (funcall done markdown-ts-appear-diagram-test--svg)))
            ('duplicate
             (funcall done markdown-ts-appear-diagram-test--svg)
             (funcall done nil "Ignore this duplicate")))
          (should-not markdown-ts-appear-diagram--job)
          (let ((source (car markdown-ts-appear-diagram--objects)))
            (if (eq result 'duplicate)
                (progn
                  (should (overlay-get source 'markdown-ts-appear-diagram--image))
                  (should-not (overlay-get source 'markdown-ts-appear-diagram--error)))
              (should (stringp (overlay-get source 'markdown-ts-appear-diagram--error)))
              (should-not (overlay-get source 'markdown-ts-appear-diagram--image)))))))))

(ert-deftest markdown-ts-appear-diagram-test-cancellation-releases-queue ()
  (dolist (cancellable '(nil t))
    (markdown-ts-appear-diagram-test--isolated
      (let* (requests (cancelled 0)
             (renderer (lambda (_language code _theme callback)
                         (push (cons code callback) requests)
                         (when cancellable
                           (lambda () (cl-incf cancelled) (funcall callback nil "Cancelled")))))
             (markdown-ts-appear-diagram-renderers `(("d2" . ,renderer))))
        (markdown-ts-appear-diagram-test--buffer "```d2\nfirst\n```\n\nAfter\n"
          (let ((first (current-buffer)) (late (cdar requests)))
            (markdown-ts-appear-diagram-test--buffer "```d2\nsecond\n```\n\nAfter\n"
              (should (= 1 (length requests)))
              (kill-buffer first)
              (should (= 2 (length requests)))
              (should (= cancelled (if cancellable 1 0)))
              (cl-letf (((symbol-function 'create-image)
                         (lambda (&rest _) (ert-fail "Retired results must not be decoded"))))
                (funcall late "old result"))
              (funcall (cdar requests) markdown-ts-appear-diagram-test--svg)
              (should-not markdown-ts-appear-diagram--job)
              (should (overlay-get (markdown-ts-appear-diagram-test--view) 'display)))))))))

(ert-deftest markdown-ts-appear-diagram-test-shared-job-survives-one-buffer ()
  (markdown-ts-appear-diagram-test--isolated
    (let* (done (calls 0) (cancelled 0)
           (renderer (lambda (&rest arguments)
                       (cl-incf calls) (setq done (car (last arguments)))
                       (lambda () (cl-incf cancelled))))
           (markdown-ts-appear-diagram-renderers `(("d2" . ,renderer))))
      (markdown-ts-appear-diagram-test--buffer "```d2\nsame\n```\n\nAfter\n"
        (let ((first (current-buffer)))
          (markdown-ts-appear-diagram-test--buffer "```d2\nsame\n```\n\nAfter\n"
            (kill-buffer first)
            (should (= calls 1))
            (should (= cancelled 0))
            (funcall done markdown-ts-appear-diagram-test--svg)
            (should (overlay-get (markdown-ts-appear-diagram-test--view) 'display))))))))

(ert-deftest markdown-ts-appear-diagram-test-synchronous-queue-is-bounded ()
  (markdown-ts-appear-diagram-test--isolated
    (let* ((calls 0)
           (renderer (lambda (_language _code _theme callback)
                       (cl-incf calls)
                       (funcall callback markdown-ts-appear-diagram-test--svg)))
           (markdown-ts-appear-diagram-renderers `(("d2" . ,renderer)))
           (max-lisp-eval-depth 180))
      (cl-letf (((symbol-function 'markdown-ts-appear-diagram--windows)
                 (lambda (&rest _)
                   (list (list (selected-window) nil nil (point-min) (point-max))))))
        (markdown-ts-appear-diagram-test--buffer
            (mapconcat (lambda (n) (format "```d2\nnode%d\n```\n\n" n)) (number-sequence 1 60) "")
          (should (= calls 60))
          (should-not markdown-ts-appear-diagram--job)
          (should-not markdown-ts-appear-diagram--queue))))))

(ert-deftest markdown-ts-appear-diagram-test-live-registration ()
  (markdown-ts-appear-diagram-test--isolated
    (let ((markdown-ts-appear-diagram-renderers nil)
          (renderer (lambda (_language _code _theme done)
                      (funcall done markdown-ts-appear-diagram-test--svg))))
      (markdown-ts-appear-diagram-test--buffer "```d2\nx -> y\n```\n\nAfter\n"
        (should-not markdown-ts-appear-diagram--objects)
        (setopt markdown-ts-appear-diagram-renderers `(("d2" . ,renderer)))
        (should (overlay-get (markdown-ts-appear-diagram-test--view) 'display))
        (setopt markdown-ts-appear-diagram-renderers nil)
        (should-not markdown-ts-appear-diagram--objects)
        (should-not (memq #'markdown-ts-appear-diagram--refresh post-command-hook))))))

(ert-deftest markdown-ts-appear-diagram-test-renderer-change-rejects-old-job ()
  (markdown-ts-appear-diagram-test--isolated
    (let* (callbacks
           (render (lambda (_config _language _code _theme done) (push done callbacks)))
           (first (apply-partially render "same"))
           (second (apply-partially render "same"))
           (markdown-ts-appear-diagram-renderers `(("d2" . ,first))))
      (markdown-ts-appear-diagram-test--buffer "```d2\nx -> y\n```\n\nAfter\n"
        (setq markdown-ts-appear-diagram-renderers `(("d2" . ,second)))
        (markdown-ts-appear-diagram--refresh)
        (should (= (length callbacks) 2))
        (funcall (cadr callbacks) markdown-ts-appear-diagram-test--svg)
        (should-not (overlay-get (car markdown-ts-appear-diagram--objects)
                                 'markdown-ts-appear-diagram--image))
        (funcall (car callbacks) markdown-ts-appear-diagram-test--svg)
        (should (overlay-get (markdown-ts-appear-diagram-test--view) 'display))))))

(ert-deftest markdown-ts-appear-diagram-test-custom-mermaid-renderer ()
  (markdown-ts-appear-diagram-test--isolated
    (let ((markdown-ts-appear-diagram-renderers
           `(("mermaid" . ,(lambda (_language _code _theme done)
                              (funcall done markdown-ts-appear-diagram-test--svg))))))
      (cl-letf (((symbol-function 'executable-find) #'ignore))
        (markdown-ts-appear-diagram-test--buffer "```mermaid\nA --> B\n```\n\nAfter\n"
          (should (overlay-get (markdown-ts-appear-diagram-test--view) 'display)))))))

(ert-deftest markdown-ts-appear-diagram-test-missing-mmdc-preserves-source ()
  (markdown-ts-appear-diagram-test--isolated
    (let ((markdown-ts-appear-diagram-renderers
           `(("mermaid" . ,(lambda (language code theme done)
                             (markdown-ts-appear-diagram-mmdc
                              language code theme done "markdown-ts-appear-missing-mmdc"))))))
      (markdown-ts-appear-diagram-test--buffer "```mermaid\nA --> B\n```\n\nAfter\n"
        (should-not markdown-ts-appear-diagram--job)
        (should (stringp (overlay-get (car markdown-ts-appear-diagram--objects)
                                     'markdown-ts-appear-diagram--error)))
        (should-not (overlay-get (markdown-ts-appear-diagram-test--view) 'display))
        (should (equal (buffer-string) "```mermaid\nA --> B\n```\n\nAfter\n"))))))

(provide 'markdown-ts-appear-diagram-test)
;;; markdown-ts-appear-diagram-test.el ends here
