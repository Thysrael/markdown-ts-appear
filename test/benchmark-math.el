;;; benchmark-math.el --- Compare real math renderers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Run in a fresh Emacs for each sample, with the required libraries on
;; load-path. MATH_BENCH_PROVIDER is mathjax, latex or ratex; MATH_BENCH_OUTPUT
;; names the JSON output file. MATH_BENCH_ROOT selects the appear checkout:
;; use the pre-migration checkout for mathjax, this branch for latex/ratex.
;; MATH_BENCH_RATEX optionally names render-svg. Caches are fresh temporary
;; directories. Defaults (including LaTeX precompilation and job count) stay on.
;;
;; Batch mode measures backend image-spec completion. A graphical session also
;; measures actual image sizes and the integrated frontend. With -Q, load this
;; file and let window-setup-hook start the run; no personal init is required.
;; Output contains every sample, enqueue blocking time and overall latency.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'package)
(package-initialize)
(setq load-prefer-newer t native-comp-jit-compilation nil)

(defvar math-bench--provider (intern (or (getenv "MATH_BENCH_PROVIDER") "mathjax")))
(defvar math-bench--results nil)

(defun math-bench--formula (n)
  "Return (BODY DISPLAY-P) for corpus item N, with no shared render keys."
  (list (pcase (% n 5)
          (0 (format "x_{%d}^2+y_{%d}^2=z_{%d}^2" n n n))
          (1 (format "\\frac{a_{%d}+b}{1+\\sqrt{x}}" n))
          (2 (format "\\int_0^{%d} e^{-x^2}\\,dx" n))
          (3 (format "\\begin{pmatrix}%d&1\\\\0&%d\\end{pmatrix}" n n))
          (_ (format "\\sum_{k=1}^{%d}\\frac{1}{k^2}" n)))
        (cl-oddp n)))

(defun math-bench--source (formula)
  "Wrap FORMULA in its inline or display delimiters."
  (if (cadr formula) (concat "\\[" (car formula) "\\]")
    (concat "$" (car formula) "$")))

(defun math-bench--request (formula done)
  "Request FORMULA and call DONE with its completed image."
  (if (eq math-bench--provider 'mathjax)
      (let ((buffer (generate-new-buffer " *math-bench-staging*")))
        (with-current-buffer buffer
          (insert (math-bench--source formula))
          (mathjax-display
           (point-min) (point-max) (car formula)
           :options (list :display (cadr formula))
           :after (lambda (ov)
                    (unwind-protect
                        (if-let* ((image (overlay-get ov 'display)))
                            (funcall done image)
                          (error "MathJax failed: %S" (overlay-get ov 'mathjax-error)))
                      (kill-buffer buffer))))))
    (let ((source (math-bench--source formula)))
      (cl-labels ((request ()
                    (when-let* ((image
                                 (latex-to-svg-backend
                                  source :engine math-bench--provider :fallback nil
                                  :color "#000000" :x-height 10 :callback #'request)))
                      (funcall done image))))
        (request)))))

(defun math-bench--wait (predicate)
  "Wait up to 90 seconds for PREDICATE, servicing process callbacks."
  (let ((deadline (+ (float-time) 90)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.001))
    (unless (funcall predicate) (error "Render timed out"))))

(defun math-bench--one (formula)
  "Measure one FORMULA, including image rasterization in graphical sessions."
  (let ((start (float-time)) finished elapsed blocked)
    (math-bench--request
     formula (lambda (image)
               (when (display-graphic-p)
                 (unless (> (car (image-size image t)) 0) (error "Empty image")))
               (setq elapsed (* 1000 (- (float-time) start)) finished t)))
    (setq blocked (* 1000 (- (float-time) start)))
    (math-bench--wait (lambda () finished))
    `((total_ms . ,elapsed) (enqueue_ms . ,blocked))))

(defun math-bench--backend ()
  "Measure startup, uncached sequential requests, cache hits and a burst."
  (let ((cold (math-bench--one (math-bench--formula 100))) warm cached
        (burst-start nil) (remaining 30))
    (dotimes (n 20) (push (math-bench--one (math-bench--formula (+ 200 n))) warm))
    (dotimes (n 20) (push (math-bench--one (math-bench--formula (+ 200 n))) cached))
    (setq burst-start (float-time))
    (dotimes (n 30)
      (math-bench--request (math-bench--formula (+ 300 n))
                          (lambda (image)
                            (when (display-graphic-p) (image-size image t))
                            (cl-decf remaining))))
    (let ((blocked (* 1000 (- (float-time) burst-start))))
      (math-bench--wait (lambda () (zerop remaining)))
      `((first . ,cold)
        (uncached . ,(vconcat (nreverse warm)))
        (repeat . ,(vconcat (nreverse cached)))
        (burst_30 . ((total_ms . ,(* 1000 (- (float-time) burst-start)))
                     (enqueue_ms . ,blocked)))))))

(defun math-bench--previews ()
  "Return the current frontend's completed image overlays."
  (seq-filter
   (lambda (ov)
     (or (overlay-get ov 'markdown-ts-appear-math--image)
         (overlay-get ov 'latex-to-svg-frontend-image)))
   (overlays-in (point-min) (point-max))))

(defun math-bench--frontend ()
  "Measure the full appear pipeline in a real graphical window."
  (add-to-list 'load-path (getenv "MATH_BENCH_ROOT"))
  (require 'markdown-ts-appear)
  (unless (file-in-directory-p (symbol-file 'markdown-ts-appear-mode)
                              (getenv "MATH_BENCH_ROOT"))
    (error "Wrong appear checkout loaded: %s" (symbol-file 'markdown-ts-appear-mode)))
  (let ((markdown-ts-appear-enable-math-preview t)
        (markdown-ts-inline-images nil)
        (markdown-ts-appear-table-style 'raw)
        (results (list (cons 'library (symbol-file 'markdown-ts-appear-mode)))))
    (dotimes (pass 2)
      (let ((buffer (generate-new-buffer " *math-bench-document*")))
        (unwind-protect
            (with-current-buffer buffer
              (switch-to-buffer buffer)
              (dotimes (n 30)
                ;; Use only the syntax supported by BOTH frontend detectors.
                (let* ((formula (math-bench--formula (+ 500 n)))
                       (delimiter (if (cadr formula) "$$" "$")))
                  (insert "Paragraph " (number-to-string n) ".\n\n"
                          delimiter (car formula) delimiter "\n\n")))
              (insert "End.\n")
              (markdown-ts-mode)
              (unless (eq math-bench--provider 'mathjax)
                (setq-local latex-to-svg-frontend-engine math-bench--provider
                            latex-to-svg-frontend-fallback nil))
              (goto-char (point-max))
              (let ((start (float-time)))
                (markdown-ts-appear-mode 1)
                (font-lock-ensure)
                (let ((blocked (* 1000 (- (float-time) start))))
                  (math-bench--wait (lambda () (= 30 (length (math-bench--previews)))))
                  (dolist (ov (math-bench--previews))
                    (image-size (overlay-get ov 'display) t))
                  (redisplay t)
                  (push (cons (if (zerop pass) 'open_30 'reopen_30)
                              `((total_ms . ,(* 1000 (- (float-time) start)))
                                (enqueue_ms . ,blocked))) results)))
              (when (= pass 1)
                (let* ((ov (car (math-bench--previews)))
                       (inside (1+ (overlay-start ov)))
                       (outside (point-max))
                       (start (float-time)))
                  (dotimes (_ 500)
                    (goto-char inside) (run-hooks 'post-command-hook)
                    (goto-char outside) (run-hooks 'post-command-hook))
                  (push (cons 'cached_motion_ms_per_command
                              (/ (* 1000 (- (float-time) start)) 1000)) results))))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (markdown-ts-appear-mode -1))
            (kill-buffer buffer)))))
    (nreverse results)))

(defun math-bench-run ()
  "Run one isolated benchmark sample and write JSON to MATH_BENCH_OUTPUT."
  (let ((code 0))
    (condition-case err
        (progn
          (if (eq math-bench--provider 'mathjax)
              (require 'mathjax)
            (require 'latex-to-svg-backend)
            (setq latex-to-svg-backend-cache-directory (make-temp-file "math-bench-cache-" t)
                  latex-to-svg-backend-gc-interval nil)
            (when (getenv "MATH_BENCH_RATEX")
              (setq latex-to-svg-backend-ratex-program (getenv "MATH_BENCH_RATEX"))))
          (setq math-bench--results
                `((provider . ,(symbol-name math-bench--provider))
                  (emacs . ,emacs-version)
                  (processors . ,(num-processors))
                  (backend_library . ,(symbol-file (if (eq math-bench--provider 'mathjax)
                                                      'mathjax-display 'latex-to-svg-backend)))
                  (precompile . ,(and (not (eq math-bench--provider 'mathjax))
                                      latex-to-svg-backend-precompile))
                  (jobs . ,(and (not (eq math-bench--provider 'mathjax))
                                (or latex-to-svg-backend-jobs (num-processors))))
                  (graphical . ,(if (display-graphic-p) t :false))
                  (backend . ,(math-bench--backend))
                  (frontend . ,(when (and (display-graphic-p) (getenv "MATH_BENCH_ROOT"))
                                 (math-bench--frontend))))))
      (error (setq code 1 math-bench--results
                   `((provider . ,(symbol-name math-bench--provider))
                     (error . ,(error-message-string err))))))
    (with-temp-file (getenv "MATH_BENCH_OUTPUT")
      (insert (json-serialize math-bench--results :false-object :false :null-object nil))
      (json-pretty-print-buffer)
      (goto-char (point-max)) (insert "\n"))
    (kill-emacs code)))

(if noninteractive
    (math-bench-run)
  (add-hook 'window-setup-hook (lambda () (run-at-time 0.5 nil #'math-bench-run))))

;;; benchmark-math.el ends here
