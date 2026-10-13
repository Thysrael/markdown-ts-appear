;;; markdown-ts-appear-mermaid-test.el --- Mermaid preview tests -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Exercise preview lifecycle with deferred renders, plus real mmdc and GUI
;; checks when available.  MARKDOWN_TS_APPEAR_REQUIRE_MERMAID=1 requires mmdc.

;;; Code:

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (file-name-directory
              (directory-file-name (file-name-directory (or load-file-name buffer-file-name)))))
(require 'markdown-ts-appear)

(when (and (member (getenv "MARKDOWN_TS_APPEAR_REQUIRE_MERMAID") '("1" "true"))
           (not (and (executable-find "mmdc") (image-type-available-p 'svg))))
  (error "Required Mermaid CLI or SVG image support is unavailable"))

(defconst markdown-ts-appear-mermaid-test--diagram
  "Before\n\n```mermaid\nflowchart LR\nA --> B\n```\n\nAfter\n")
(defconst markdown-ts-appear-mermaid-test--svg
  "<svg xmlns='http://www.w3.org/2000/svg' width='240' height='240'><rect width='240' height='240' fill='#7dd3fc'/></svg>")
(defvar markdown-ts-appear-mermaid-test--jobs nil)

(defmacro markdown-ts-appear-mermaid-test--isolated (&rest body)
  "Run BODY with isolated render state."
  (declare (indent 0) (debug t))
  `(let ((markdown-ts-appear-diagram--cache
          (make-hash-table :test 'markdown-ts-appear-diagram--key-equal))
         markdown-ts-appear-diagram--queue markdown-ts-appear-diagram--job
         markdown-ts-appear-diagram--cancel
         markdown-ts-appear-mermaid-test--jobs)
     ,@body))

(defmacro markdown-ts-appear-mermaid-test--deferred (&rest body)
  "Run BODY using the real queue and a deferred renderer."
  (declare (indent 0) (debug t))
  `(markdown-ts-appear-mermaid-test--isolated
     (cl-letf (((symbol-function 'markdown-ts-appear-diagram--render)
                (lambda (job) (push job markdown-ts-appear-mermaid-test--jobs)))
               ((symbol-function 'executable-find) (lambda (_) "mmdc"))
               ((symbol-function 'image-type-available-p) (lambda (_) t))
               ((symbol-function 'display-images-p) (lambda (&rest _) t)))
       ,@body)))

(defmacro markdown-ts-appear-mermaid-test--buffer (content &rest body)
  "Evaluate BODY in a displayed Markdown buffer containing CONTENT."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (treesit-ready-p '(markdown markdown-inline)))
     (let ((buffer (generate-new-buffer " *mermaid-test*")))
       (unwind-protect
           (save-window-excursion
             (with-current-buffer buffer
               (insert ,content)
               (set-buffer-modified-p nil)
               (set-window-buffer (selected-window) buffer)
               (let ((markdown-ts-inline-images nil)
                     (markdown-ts-appear-enable-math-preview nil)
                     (markdown-ts-appear-diagram-renderers
                      '(("mermaid" . markdown-ts-appear-diagram-mmdc)))
                     (treesit-font-lock-level 4))
                 (markdown-ts-mode)
                 (goto-char (point-max))
                 (markdown-ts-appear-mode 1)
                 (font-lock-ensure)
                 ,@body)))
         (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun markdown-ts-appear-mermaid-test--deliver (&optional error job)
  "Complete JOB, or the running job, with an SVG or ERROR."
  (setq job (or job markdown-ts-appear-diagram--job))
  (should job)
  (markdown-ts-appear-diagram--complete
   job
   (unless error (list 'image :type 'svg :data markdown-ts-appear-mermaid-test--svg :scale 1))
   error))

(defun markdown-ts-appear-mermaid-test--view (&optional source)
  "Return the first view for SOURCE or the first diagram."
  (car (overlay-get (or source (car markdown-ts-appear-diagram--objects))
                    'markdown-ts-appear-diagram--views)))

(defun markdown-ts-appear-mermaid-test--wait ()
  "Wait for real Mermaid jobs to finish."
  (let ((deadline (+ (float-time) 60)))
    (while (and markdown-ts-appear-diagram--job (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (should-not markdown-ts-appear-diagram--job))

(ert-deftest markdown-ts-appear-mermaid-test-parses-native-fences-and-containers ()
  (dolist (text '("```mermaid\nflowchart LR\nA --> B\n```\n"
                  "~~~Mermaid\nflowchart LR\nA --> B\n~~~\n"
                  "> ```mermaid\n> flowchart LR\n> A --> B\n> ```\n"
                  "- ```mermaid\n  flowchart LR\n  A --> B\n  ```\n"))
    (markdown-ts-appear-mermaid-test--deferred
      (markdown-ts-appear-mermaid-test--buffer (concat text "\nAfter\n")
        (should (= (length markdown-ts-appear-diagram--objects) 1))
        (should (= (length markdown-ts-appear-mermaid-test--jobs) 1))
        (should (equal (nth 2 (car markdown-ts-appear-diagram--job)) "flowchart LR\nA --> B\n"))
        (should-not (buffer-modified-p))))))

(ert-deftest markdown-ts-appear-mermaid-test-ignores-other-and-unclosed-fences ()
  (dolist (text '("```text\nflowchart LR\nA --> B\n```\n"
                  "    ```mermaid\n    A --> B\n    ```\n"
                  "```mermaid\nflowchart LR\nA --> B\n"))
    (markdown-ts-appear-mermaid-test--deferred
      (markdown-ts-appear-mermaid-test--buffer text
        (should-not markdown-ts-appear-diagram--objects)
        (should-not markdown-ts-appear-mermaid-test--jobs)))))

(ert-deftest markdown-ts-appear-mermaid-test-reuses-preview-across-editing ()
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (markdown-ts-appear-mermaid-test--deliver)
      (let* ((view (markdown-ts-appear-mermaid-test--view))
             (image (overlay-get view 'display)))
        (should image)
        (overlay-put view 'extension-data 'retained)
        (goto-char (point-min))
        (search-forward "A -->")
        (markdown-ts-appear-start)
        (should-not (overlay-get view 'display))
        (dotimes (_ 10) (markdown-ts-appear-diagram--refresh))
        (markdown-ts-appear-stop)
        (should (eq image (overlay-get view 'display)))
        (should (eq (overlay-get view 'extension-data) 'retained))
        (should (= (length markdown-ts-appear-mermaid-test--jobs) 1))
        (should (equal (buffer-string) markdown-ts-appear-mermaid-test--diagram))
        (should-not (buffer-modified-p))))))

(ert-deftest markdown-ts-appear-mermaid-test-coalesces-identical-diagrams ()
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer
        (concat markdown-ts-appear-mermaid-test--diagram markdown-ts-appear-mermaid-test--diagram)
      (should (= (length markdown-ts-appear-diagram--objects) 2))
      (should (= (length markdown-ts-appear-mermaid-test--jobs) 1))
      (should (= (length (cdr markdown-ts-appear-diagram--job)) 2))
      (markdown-ts-appear-mermaid-test--deliver)
      (let ((image (overlay-get (car markdown-ts-appear-diagram--objects)
                                 'markdown-ts-appear-diagram--image)))
        (should (eq image (overlay-get (cadr markdown-ts-appear-diagram--objects)
                                      'markdown-ts-appear-diagram--image)))
        (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
          (should (eq image (overlay-get (car markdown-ts-appear-diagram--objects)
                                        'markdown-ts-appear-diagram--image)))
          (should (overlay-get (markdown-ts-appear-mermaid-test--view) 'display))
          (should (= (length markdown-ts-appear-mermaid-test--jobs) 1)))))))

(ert-deftest markdown-ts-appear-mermaid-test-serializes-distinct-renders ()
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer
        (concat markdown-ts-appear-mermaid-test--diagram
                "\n```mermaid\nsequenceDiagram\nAlice->>Bob: Hello\n```\n\nAfter\n")
      (should (= (length markdown-ts-appear-mermaid-test--jobs) 1))
      (should (= (length markdown-ts-appear-diagram--queue) 1))
      (should (string-prefix-p "flowchart" (nth 2 (car markdown-ts-appear-diagram--job))))
      (markdown-ts-appear-mermaid-test--deliver)
      (should (= (length markdown-ts-appear-mermaid-test--jobs) 2))
      (should markdown-ts-appear-diagram--job)
      (markdown-ts-appear-mermaid-test--deliver)
      (should-not markdown-ts-appear-diagram--queue)
      (should-not markdown-ts-appear-diagram--job)
      (dolist (source markdown-ts-appear-diagram--objects)
        (should (overlay-get (markdown-ts-appear-mermaid-test--view source) 'display))))))

(ert-deftest markdown-ts-appear-mermaid-test-rejects-stale-result-before-refresh ()
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (goto-char (point-min))
      (search-forward "A --> B")
      (replace-match "A --> C" t t)
      (markdown-ts-appear-mermaid-test--deliver)
      (should-not (overlay-get (car markdown-ts-appear-diagram--objects)
                               'markdown-ts-appear-diagram--image))
      (markdown-ts-appear-start)
      (should (= (length markdown-ts-appear-mermaid-test--jobs) 1))
      (markdown-ts-appear-stop)
      (should (= (length markdown-ts-appear-mermaid-test--jobs) 2))
      (should (string-match-p "A --> C" (nth 2 (car markdown-ts-appear-diagram--job))))
      (markdown-ts-appear-mermaid-test--deliver)
      (should (overlay-get (markdown-ts-appear-mermaid-test--view) 'display)))))

(ert-deftest markdown-ts-appear-mermaid-test-completion-does-not-hide-edited-source ()
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (goto-char (point-min))
      (search-forward "flowchart")
      (markdown-ts-appear-start)
      (markdown-ts-appear-mermaid-test--deliver)
      (should (overlay-get (car markdown-ts-appear-diagram--objects)
                           'markdown-ts-appear-diagram--image))
      (should-not (overlay-get (markdown-ts-appear-mermaid-test--view) 'display))
      (markdown-ts-appear-stop)
      (should (overlay-get (markdown-ts-appear-mermaid-test--view) 'display))
      (should (= (length markdown-ts-appear-mermaid-test--jobs) 1)))))

(ert-deftest markdown-ts-appear-mermaid-test-invalidates-distant-structural-edits ()
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (let ((source (car markdown-ts-appear-diagram--objects))
            (view (markdown-ts-appear-mermaid-test--view)))
        (goto-char (point-min))
        (insert "````text\n")
        (goto-char (point-max))
        (insert "````\n")
        (markdown-ts-appear-mermaid-test--deliver)
        (should-not (overlay-get source 'markdown-ts-appear-diagram--image))
        (markdown-ts-appear-diagram--refresh)
        (should-not markdown-ts-appear-diagram--objects)
        (should-not (overlay-buffer view))))))

(ert-deftest markdown-ts-appear-mermaid-test-new-image-wins-after-editing-resumes ()
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (markdown-ts-appear-mermaid-test--deliver)
      (let* ((view (markdown-ts-appear-mermaid-test--view))
             (old-image (overlay-get view 'display))
             (svg "<svg xmlns='http://www.w3.org/2000/svg' width='160' height='80'><rect width='160' height='80' fill='red'/></svg>"))
        (goto-char (point-min))
        (search-forward "A --> B")
        (replace-match "A --> C" t t)
        (markdown-ts-appear-stop)
        (markdown-ts-appear-start)
        (let ((markdown-ts-appear-mermaid-test--svg svg))
          (markdown-ts-appear-mermaid-test--deliver))
        (should-not (overlay-get view 'display))
        (markdown-ts-appear-stop)
        (should-not (eq old-image (overlay-get view 'display)))
        (should (equal (plist-get (cdr (overlay-get view 'display)) :data) svg))))))

(ert-deftest markdown-ts-appear-mermaid-test-folding-and-language-change ()
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer
        (concat "# Heading\n\n" markdown-ts-appear-mermaid-test--diagram "\n# End\n")
      (markdown-ts-appear-mermaid-test--deliver)
      (let* ((view (markdown-ts-appear-mermaid-test--view))
             (image (overlay-get view 'display)))
        (goto-char (point-min))
        (outline-hide-subtree)
        (should-not (overlay-get view 'display))
        (should-not (overlay-get view 'after-string))
        (outline-show-all)
        (should (eq image (overlay-get view 'display)))
        (should (= (length markdown-ts-appear-mermaid-test--jobs) 1))
        (search-forward "mermaid")
        (replace-match "text" t t)
        (markdown-ts-appear-diagram--refresh)
        (should-not markdown-ts-appear-diagram--objects)
        (should-not (overlay-buffer view))))))

(ert-deftest markdown-ts-appear-mermaid-test-errors-preserve-source-and-retry-after-edit ()
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (markdown-ts-appear-mermaid-test--deliver "Parse error")
      (should-not (overlay-get (markdown-ts-appear-mermaid-test--view) 'display))
      (should (equal (overlay-get (markdown-ts-appear-mermaid-test--view) 'help-echo) "Parse error"))
      (dotimes (_ 10) (markdown-ts-appear-diagram--invalidate))
      (should (= (length markdown-ts-appear-mermaid-test--jobs) 1))
      (should (equal (buffer-string) markdown-ts-appear-mermaid-test--diagram))
      (goto-char (point-min))
      (search-forward "A --> B")
      (replace-match "A --> C" t t)
      (markdown-ts-appear-stop)
      (should (= (length markdown-ts-appear-mermaid-test--jobs) 2))
      (markdown-ts-appear-mermaid-test--deliver)
      (should (overlay-get (markdown-ts-appear-mermaid-test--view) 'display))
      (should-not (overlay-get (markdown-ts-appear-mermaid-test--view) 'help-echo)))))

(ert-deftest markdown-ts-appear-mermaid-test-theme-change-rejects-old-render ()
  (markdown-ts-appear-mermaid-test--deferred
    (let ((theme 'light)
          (original (symbol-function 'frame-parameter)))
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (frame parameter)
                   (if (eq parameter 'background-mode) theme
                     (funcall original frame parameter)))))
        (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
          (setq theme 'dark)
          (markdown-ts-appear--appearance-change)
          (markdown-ts-appear-mermaid-test--deliver)
          (should-not (overlay-get (markdown-ts-appear-mermaid-test--view) 'display))
          (should (equal (cadr (car markdown-ts-appear-diagram--job)) "dark"))
          (markdown-ts-appear-mermaid-test--deliver)
          (should (overlay-get (markdown-ts-appear-mermaid-test--view) 'display)))))))

(ert-deftest markdown-ts-appear-mermaid-test-clone-and-teardown-preserve-foreign-overlays ()
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (markdown-ts-appear-mermaid-test--deliver)
      (let ((view (markdown-ts-appear-mermaid-test--view))
            (foreign (make-overlay (point-min) (point-max)))
            (clone (clone-indirect-buffer " *mermaid-clone*" nil)))
        (unwind-protect
            (progn
              (with-current-buffer clone
                (should-not markdown-ts-appear-mode)
                (should-not markdown-ts-appear-diagram--objects))
              (should (overlay-get view 'display))
              (markdown-ts-appear-mode -1)
              (should-not (overlay-buffer view))
              (should (overlay-buffer foreign))
              (should-not (memq #'markdown-ts-appear-diagram--refresh post-command-hook)))
          (kill-buffer clone))))))

(ert-deftest markdown-ts-appear-mermaid-test-coalesces-transient-theme-changes ()
  (markdown-ts-appear-mermaid-test--deferred
    (let ((theme 'light)
          (original (symbol-function 'frame-parameter)))
      (cl-letf (((symbol-function 'frame-parameter)
                 (lambda (frame parameter)
                   (if (eq parameter 'background-mode) theme
                     (funcall original frame parameter)))))
        (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
          (markdown-ts-appear-mermaid-test--deliver)
          (setq theme 'dark)
          (markdown-ts-appear--appearance-change)
          (setq theme 'light)
          (markdown-ts-appear--appearance-change)
          (sit-for 0.05)
          (should (= (length markdown-ts-appear-mermaid-test--jobs) 1))
          (should (overlay-get (markdown-ts-appear-mermaid-test--view) 'display)))))))

(ert-deftest markdown-ts-appear-mermaid-test-late-result-after-disable-or-kill ()
  (dolist (action '(disable kill option))
    (markdown-ts-appear-mermaid-test--deferred
      (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
        (let ((job markdown-ts-appear-diagram--job)
              (source (car markdown-ts-appear-diagram--objects))
              (view (markdown-ts-appear-mermaid-test--view)))
          (pcase action
            ('disable (markdown-ts-appear-mode -1))
            ('kill (kill-buffer (current-buffer)))
            ('option (setq markdown-ts-appear-diagram-renderers nil)))
          (markdown-ts-appear-mermaid-test--deliver nil job)
          (should-not (overlay-get source 'markdown-ts-appear-diagram--image))
          (should-not (overlay-get view 'display)))))))

(ert-deftest markdown-ts-appear-mermaid-test-unchanged-motion-does-not-rescan ()
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (markdown-ts-appear-mermaid-test--deliver)
      (redisplay t)
      (markdown-ts-appear-diagram--refresh)
      (cl-letf (((symbol-function 'markdown-ts-appear-diagram--scan)
                 (lambda (&rest _) (ert-fail "Unchanged syntax was scanned again")))
                ((symbol-function 'markdown-ts-appear-diagram--display)
                 (lambda (&rest _) (ert-fail "Plain cursor motion refreshed diagrams"))))
        (dotimes (_ 20) (markdown-ts-appear-diagram--refresh))
        (goto-char (point-min))
        (markdown-ts-appear-diagram--refresh))
      (cl-letf (((symbol-function 'markdown-ts-appear-diagram--scan)
                 (lambda (&rest _) (ert-fail "Unrelated prose edit rescanned diagrams"))))
        (goto-char 3)
        (insert "new text ")
        (run-hooks 'post-command-hook))
      (should (= (length markdown-ts-appear-mermaid-test--jobs) 1)))))

(ert-deftest markdown-ts-appear-mermaid-test-waits-until-source-editing-stops ()
  (dolist (fence '("```" "~~~"))
    (markdown-ts-appear-mermaid-test--deferred
      (markdown-ts-appear-mermaid-test--buffer
          (format "%smermaid\nflowchart LR\nA --> B\n%s" fence fence)
        (should markdown-ts-appear-diagram--objects)
        (should-not markdown-ts-appear-mermaid-test--jobs)
        (markdown-ts-appear-stop)
        (should (= (length markdown-ts-appear-mermaid-test--jobs) 1))
        (should (equal (nth 2 (car markdown-ts-appear-diagram--job)) "flowchart LR\nA --> B\n"))
        (markdown-ts-appear-mermaid-test--deliver)
        (should (overlay-get (markdown-ts-appear-mermaid-test--view) 'display))))))

(ert-deftest markdown-ts-appear-mermaid-test-missing-svg-preserves-source ()
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (markdown-ts-appear-mode -1)
      (cl-letf (((symbol-function 'image-type-available-p) (lambda (&rest _) nil)))
        (should-error (markdown-ts-appear-mode 1) :type 'user-error))
      (should (equal (buffer-string) markdown-ts-appear-mermaid-test--diagram))
      (should-not markdown-ts-appear-diagram--objects)
      (should-not (memq #'markdown-ts-appear-diagram--refresh post-command-hook)))))

(ert-deftest markdown-ts-appear-mermaid-test-gui-defers-offscreen-diagrams ()
  (skip-unless (display-graphic-p))
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer
        (concat (apply #'concat (make-list 100 "Filler paragraph.\n\n"))
                markdown-ts-appear-mermaid-test--diagram)
      (markdown-ts-appear-mode -1)
      (setq markdown-ts-appear-diagram--job nil
            markdown-ts-appear-diagram--queue nil
            markdown-ts-appear-mermaid-test--jobs nil)
      (goto-char (point-min))
      (set-window-start nil (point-min))
      (redisplay t)
      (markdown-ts-appear-mode 1)
      (should markdown-ts-appear-diagram--objects)
      (should-not markdown-ts-appear-mermaid-test--jobs)
      (goto-char (overlay-start (car markdown-ts-appear-diagram--objects)))
      (set-window-start nil (point))
      (markdown-ts-appear-stop)
      (markdown-ts-appear-diagram--invalidate)
      (should (= (length markdown-ts-appear-mermaid-test--jobs) 1)))))

(ert-deftest markdown-ts-appear-mermaid-test-gui-render-exposes-next-diagram ()
  (skip-unless (display-graphic-p))
  (markdown-ts-appear-mermaid-test--deferred
    (let ((markdown-ts-appear-mermaid-test--svg
           "<svg xmlns='http://www.w3.org/2000/svg' width='120' height='40'><rect width='120' height='40' fill='blue'/></svg>"))
      (markdown-ts-appear-mermaid-test--buffer
          (concat "Before\n\n```mermaid\nflowchart LR\n"
                  (apply #'concat (make-list 60 "%% A comment\n"))
                  "A --> B\n```\n\n" markdown-ts-appear-mermaid-test--diagram)
        (markdown-ts-appear-mode -1)
        (setq markdown-ts-appear-diagram--job nil
              markdown-ts-appear-diagram--queue nil
              markdown-ts-appear-mermaid-test--jobs nil)
        (goto-char (point-min))
        (set-window-start nil (point-min))
        (redisplay t)
        (markdown-ts-appear-mode 1)
        (should (= (length markdown-ts-appear-mermaid-test--jobs) 1))
        (markdown-ts-appear-mermaid-test--deliver)
        (let ((deadline (+ (float-time) 2)))
          (while (and (< (length markdown-ts-appear-mermaid-test--jobs) 2)
                      (< (float-time) deadline))
            (sit-for 0.01)))
        (should (= (length markdown-ts-appear-mermaid-test--jobs) 2))))))

(ert-deftest markdown-ts-appear-mermaid-test-real-cli-and-cache ()
  (skip-unless (and (executable-find "mmdc") (image-type-available-p 'svg)))
  (markdown-ts-appear-mermaid-test--isolated
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (markdown-ts-appear-mermaid-test--buffer
          "```mermaid\nflowchart LR\nA[开始] --> B[完成]\n```\n\nAfter\n"
        (markdown-ts-appear-mermaid-test--wait)
        (let* ((source (car markdown-ts-appear-diagram--objects))
               (image (overlay-get source 'markdown-ts-appear-diagram--image))
               (svg (plist-get (cdr image) :data)))
          (should image)
          (should (string-match-p "开始" svg))
          (should-not (string-match-p "<foreignObject" svg))
          (goto-char (point-min))
          (search-forward "flowchart")
          (markdown-ts-appear-start)
          (should-not (overlay-get (markdown-ts-appear-mermaid-test--view) 'display))
          (markdown-ts-appear-stop)
          (should (eq image (overlay-get source 'markdown-ts-appear-diagram--image)))
          (should-not markdown-ts-appear-diagram--job)
          (should-not (buffer-modified-p)))))))

(ert-deftest markdown-ts-appear-mermaid-test-real-cli-error ()
  (skip-unless (and (executable-find "mmdc") (image-type-available-p 'svg)))
  (markdown-ts-appear-mermaid-test--isolated
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (markdown-ts-appear-mermaid-test--buffer
          "```mermaid\nthis is not a diagram\n```\n\nAfter\n"
        (markdown-ts-appear-mermaid-test--wait)
        (should (overlay-get (car markdown-ts-appear-diagram--objects)
                             'markdown-ts-appear-diagram--error))
        (should-not (overlay-get (markdown-ts-appear-mermaid-test--view) 'display))
        (should-not (buffer-modified-p))))))

(ert-deftest markdown-ts-appear-mermaid-test-real-process-stops-with-buffer ()
  (skip-unless (and (executable-find "mmdc") (image-type-available-p 'svg)))
  (markdown-ts-appear-mermaid-test--isolated
    (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
      (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
        (let* ((process (get-process "markdown-ts-appear-diagram-mmdc"))
               (log (process-buffer process)))
          (should (process-live-p process))
          (kill-buffer (current-buffer))
          (should-not markdown-ts-appear-diagram--job)
          (let ((deadline (+ (float-time) 10)))
            ;; Process exit can precede delivery of its cleanup sentinel.
            (while (and (or (process-live-p process) (buffer-live-p log))
                        (< (float-time) deadline))
              (accept-process-output nil 0.05)))
          (should-not (process-live-p process))
          (should-not (buffer-live-p log)))))))

(ert-deftest markdown-ts-appear-mermaid-test-gui-preview-fits-source-height ()
  (skip-unless (display-graphic-p))
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (markdown-ts-appear-mermaid-test--deliver)
      (redisplay t)
      (let* ((source (car markdown-ts-appear-diagram--objects))
             (view (markdown-ts-appear-mermaid-test--view))
             (image (overlay-get view 'display))
             (height (plist-get (cdr image) :max-height)))
        (should (> height 0))
        (should (<= (cdr (image-size image t)) height))
        (goto-char (point-min))
        (search-forward "flowchart")
        (markdown-ts-appear-start)
        (redisplay t)
        (should-not (overlay-get view 'display))
        (should-not (overlay-get view 'after-string))
        (should (= height (cdr (window-text-pixel-size nil (overlay-start source) (overlay-end source)))))
        (markdown-ts-appear-stop)
        (should (eq image (overlay-get view 'display)))
        (should-not (overlay-get view 'after-string))))))

(ert-deftest markdown-ts-appear-mermaid-test-gui-small-diagrams-are-not-enlarged ()
  (skip-unless (display-graphic-p))
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (let ((markdown-ts-appear-diagram-size 'source)
            (markdown-ts-appear-mermaid-test--svg
             "<svg xmlns='http://www.w3.org/2000/svg' width='32' height='16'><rect width='32' height='16' fill='blue'/></svg>"))
        (markdown-ts-appear-mermaid-test--deliver)
        (let ((view (markdown-ts-appear-mermaid-test--view)))
          (dolist (size '(source 120))
            (setopt markdown-ts-appear-diagram-size size)
            (should (equal (image-size (overlay-get view 'display) t) '(32 . 16)))
            (should-not (overlay-get view 'after-string))))))))

(ert-deftest markdown-ts-appear-mermaid-test-gui-size-option-updates-cached-preview ()
  (skip-unless (display-graphic-p))
  (markdown-ts-appear-mermaid-test--deferred
    (let ((markdown-ts-appear-diagram-size 'source)
          (markdown-ts-image-max-width 120)
          (markdown-ts-appear-mermaid-test--svg
           "<svg xmlns='http://www.w3.org/2000/svg' width='240' height='960'><rect width='240' height='960' fill='blue'/></svg>"))
      (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
        (markdown-ts-appear-mermaid-test--deliver)
        (let* ((source (car markdown-ts-appear-diagram--objects))
               (view (markdown-ts-appear-mermaid-test--view))
               (image (overlay-get source 'markdown-ts-appear-diagram--image))
               (preview (overlay-get view 'display))
               (height (image-property preview :max-height)))
          (should (< height 480))
          (cl-letf (((symbol-function 'markdown-ts-appear-diagram--source-height)
                     (lambda (&rest _) (ert-fail "Unnecessary source measurement"))))
            ;; The width matches the default limit; only the sizing mode changes.
            (setopt markdown-ts-appear-diagram-size 120)
            (should-not (image-property (overlay-get view 'display) :max-height))
            (should (equal (image-size (overlay-get view 'display) t) '(120 . 480)))
            (setopt markdown-ts-appear-diagram-size 60)
            (should (equal (image-size (overlay-get view 'display) t) '(60 . 240)))
            (goto-char (+ 12 (overlay-start source)))
            (markdown-ts-appear-start)
            (should-not (overlay-get view 'display))
            (should-not (overlay-get view 'after-string))
            (markdown-ts-appear-stop)
            (should (equal (image-size (overlay-get view 'display) t) '(60 . 240)))
            (setopt markdown-ts-appear-diagram-size 'source)
            (should (equal (overlay-get view 'display) preview)))
          (should (eq (overlay-get source 'markdown-ts-appear-diagram--image) image))
          (should-not (image-property image :max-width))
          (should-not (image-property image :max-height))
          (should (= (length markdown-ts-appear-mermaid-test--jobs) 1)))))))

(ert-deftest markdown-ts-appear-mermaid-test-gui-height-follows-wrapped-code ()
  (skip-unless (display-graphic-p))
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer
        (concat "Before\n\n```mermaid\nflowchart LR\nA["
                (apply #'concat (make-list 30 "long label "))
                "] --> B\n```\n\nAfter\n")
      (let ((markdown-ts-image-max-width 2000)
            (markdown-ts-appear-mermaid-test--svg
             "<svg xmlns='http://www.w3.org/2000/svg' width='100' height='2000'><rect width='100' height='2000' fill='blue'/></svg>"))
        (visual-line-mode 1)
        (markdown-ts-appear-mermaid-test--deliver)
        (delete-other-windows)
        (let ((other (split-window-right -25)) heights)
          (set-window-buffer other (current-buffer))
          (markdown-ts-appear-diagram--invalidate)
          (let ((source (car markdown-ts-appear-diagram--objects)))
            (dolist (view (overlay-get source 'markdown-ts-appear-diagram--views))
              (let* ((window (overlay-get view 'window))
                     (image (overlay-get view 'display))
                     (limit (plist-get (cdr image) :max-height)))
                (push limit heights)
                (should (= (plist-get (cdr image) :max-width) 2000))
                (should (<= (cdr (image-size image t (window-frame window))) limit))))
            (goto-char (+ 12 (overlay-start source)))
            (markdown-ts-appear-start)
            (dolist (view (overlay-get source 'markdown-ts-appear-diagram--views))
              (let ((window (overlay-get view 'window)))
                (should (= (image-property (overlay-get view 'markdown-ts-appear-diagram--image) :max-height)
                           (cdr (window-text-pixel-size window (overlay-start source) (overlay-end source)))))))
            (should-not (= (car heights) (cadr heights))))
          (should (= (length markdown-ts-appear-mermaid-test--jobs) 1)))))))

(ert-deftest markdown-ts-appear-mermaid-test-gui-height-follows-layout-options ()
  (skip-unless (display-graphic-p))
  (dolist (change '(tabs partial hscroll numbers numbers-width))
    (ert-info ((format "Layout change: %s" change))
      (markdown-ts-appear-mermaid-test--deferred
        (markdown-ts-appear-mermaid-test--buffer
            (concat "Before\n\n```mermaid\nflowchart LR\nA["
                    (apply #'concat (make-list 20 "label\t"))
                    "] --> B\n```\n\nAfter\n")
          (delete-other-windows)
          (let ((other (split-window-right -25)))
            (set-window-buffer other (current-buffer))
            (setq-local truncate-lines nil
                        truncate-partial-width-windows nil
                        tab-width 2
                        display-line-numbers (eq change 'numbers-width))
            (markdown-ts-appear-mermaid-test--deliver)
            (let* ((source (car markdown-ts-appear-diagram--objects))
                   (view (seq-find (lambda (view) (eq (overlay-get view 'window) other))
                                   (overlay-get source 'markdown-ts-appear-diagram--views)))
                   (before (image-property (overlay-get view 'display) :max-height)))
              (pcase change
                ('tabs (setq-local tab-width 16))
                ('partial (setq-local truncate-partial-width-windows t))
                ('hscroll (set-window-hscroll other 4))
                ('numbers (setq-local display-line-numbers t))
                ('numbers-width (setq-local display-line-numbers-width 10)))
              (markdown-ts-appear-diagram--refresh)
              (let ((height (image-property (overlay-get view 'display) :max-height)))
                (goto-char (+ 12 (overlay-start source)))
                (markdown-ts-appear-start)
                (let ((actual (cdr (window-text-pixel-size
                                    other (overlay-start source) (overlay-end source)))))
                  (should-not (= before actual))
                  (should (= height actual))))
              (should (= (length markdown-ts-appear-mermaid-test--jobs) 1)))))))))

(ert-deftest markdown-ts-appear-mermaid-test-gui-height-follows-text-scale-and-spacing ()
  (skip-unless (display-graphic-p))
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (markdown-ts-appear-mermaid-test--deliver)
      (let* ((view (markdown-ts-appear-mermaid-test--view))
             (height (plist-get (cdr (overlay-get view 'display)) :max-height)))
        (text-scale-increase 2)
        (markdown-ts-appear-diagram--refresh)
        (let ((scaled (plist-get (cdr (overlay-get view 'display)) :max-height)))
          (should (> scaled height))
          (setq-local line-spacing 5)
          (markdown-ts-appear-diagram--refresh)
          (should (> (plist-get (cdr (overlay-get view 'display)) :max-height) scaled)))
        (should (= (length markdown-ts-appear-mermaid-test--jobs) 1))))))

(ert-deftest markdown-ts-appear-mermaid-test-gui-window-specific-scaling ()
  (skip-unless (display-graphic-p))
  (markdown-ts-appear-mermaid-test--deferred
    (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
      (let ((markdown-ts-image-max-width 'window)
            (markdown-ts-appear-mermaid-test--svg
             "<svg xmlns='http://www.w3.org/2000/svg' width='1200' height='240'><rect width='1200' height='240' fill='#7dd3fc'/></svg>"))
        (markdown-ts-appear-mermaid-test--deliver)
        (delete-other-windows)
        (let ((other (split-window-right)))
          (set-window-buffer other (current-buffer))
          (window-resize other -8 t)
          (let ((original (symbol-function 'image-size)))
            (cl-letf (((symbol-function 'image-size)
                       (lambda (spec &rest args)
                         (when (equal (plist-get (cdr spec) :data) markdown-ts-appear-mermaid-test--svg)
                           (ert-fail "Resizing eagerly measured a diagram"))
                         (apply original spec args))))
              (markdown-ts-appear-diagram--invalidate)))
          (let ((views (overlay-get (car markdown-ts-appear-diagram--objects)
                                     'markdown-ts-appear-diagram--views)))
            (should (= (length views) 2))
            (dolist (view views)
              (let* ((window (overlay-get view 'window))
                     (image (overlay-get view 'display))
                     (size (image-size image t (window-frame window))))
                (should (= (plist-get (cdr image) :max-width) (window-body-width window t)))
                (should (<= (car size) (window-body-width window t)))))
            (delete-window other)
            (markdown-ts-appear-diagram--invalidate)
            (should (= (length (overlay-get (car markdown-ts-appear-diagram--objects)
                                            'markdown-ts-appear-diagram--views)) 1))))))))

(ert-deftest markdown-ts-appear-mermaid-test-gui-evil-edit-and-navigation ()
  (skip-unless (and (display-graphic-p) (require 'evil nil t)))
  (dolist (evil-move-beyond-eol '(nil t))
    (markdown-ts-appear-mermaid-test--deferred
      (markdown-ts-appear-mermaid-test--buffer markdown-ts-appear-mermaid-test--diagram
        (let ((was-enabled evil-mode))
          (evil-mode 1)
          (visual-line-mode 1)
          (evil-local-set-key 'normal "j" #'evil-next-visual-line)
          (evil-local-set-key 'normal "k" #'evil-previous-visual-line)
          (add-hook 'evil-insert-state-entry-hook #'markdown-ts-appear-start nil t)
          (add-hook 'evil-insert-state-exit-hook #'markdown-ts-appear-stop nil t)
          (unwind-protect
              (progn
                (evil-normal-state)
                (markdown-ts-appear-stop)
                (markdown-ts-appear-mermaid-test--deliver)
                (goto-char (point-min))
                (set-window-start nil (point-min))
                (redisplay t)
                (execute-kbd-macro "jj")
                (let* ((source (car markdown-ts-appear-diagram--objects))
                       (view (markdown-ts-appear-mermaid-test--view)))
                  (should (<= (overlay-start source) (point)))
                  (should (<= (point) (overlay-end source)))
                  (should (overlay-get view 'display))
                  (execute-kbd-macro "i")
                  (redisplay t)
                  (should-not (overlay-get view 'display))
                  (execute-kbd-macro (kbd "<escape>"))
                  (redisplay t)
                  (should (overlay-get view 'display))
                  (execute-kbd-macro "jj")
                  (should (> (point) (overlay-end source)))
                  (should (= (length markdown-ts-appear-mermaid-test--jobs) 1))
                  (should (equal (buffer-string) markdown-ts-appear-mermaid-test--diagram))))
            (unless was-enabled (evil-mode -1))))))))

;;; markdown-ts-appear-mermaid-test.el ends here
