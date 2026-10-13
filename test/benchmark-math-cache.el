;;; benchmark-math-cache.el --- Persistent-cache and idle math latency -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Run in a fresh graphical Emacs per sample, with package dependencies on
;; load-path. MATH_CACHE_ENGINE is mathjax, latex or ratex. MATH_CACHE_PHASE is
;; seed (prepare 30 formulas), reopen (same 30 in a fresh process), new (one
;; new formula with the existing preamble cache), or idle (wait a real 61 s).
;; Keep MATH_CACHE_DIRECTORY between processes. MATH_CACHE_RUN selects distinct
;; new formulas per sample. MATH_CACHE_OUTPUT names the result file.
;; RATEX_PROGRAM optionally selects the executable. No TTL setting is changed.
;;
;; M4 Pro / Emacs 32.0.50, 2026-10-14, medians of three real restart runs:
;; cached 30-formula reopen: MathJax 186.05 ms, LaTeX 49.61 ms, RaTeX 59.23 ms.
;; New LaTeX formula with .fmt present: 208.19 ms total, 1.40 ms enqueue time.
;; One real 61 s idle run: MathJax 2.91 ms warm -> 162.64 ms after Node exited.
;; Results include forced SVG rasterization, not just image-spec creation.

;;; Code:
(require 'cl-lib)
(require 'package)
(package-initialize)
(setq native-comp-jit-compilation nil)

(defvar math-cache-engine (intern (getenv "MATH_CACHE_ENGINE")))

(defun math-cache-formula (n)
  "Return (BODY DISPLAY-P) of corpus item N."
  (list (pcase (% n 5)
          (0 (format "x_{%d}^2+y_{%d}^2=z_{%d}^2" n n n))
          (1 (format "\\frac{a_{%d}+b}{1+\\sqrt{x}}" n))
          (2 (format "\\int_0^{%d} e^{-x^2}\\,dx" n))
          (3 (format "\\begin{pmatrix}%d&1\\\\0&%d\\end{pmatrix}" n n))
          (_ (format "\\sum_{k=1}^{%d}\\frac{1}{k^2}" n)))
        (cl-oddp n)))

(defun math-cache-request (n done)
  "Render item N, rasterize it and call DONE."
  (pcase-let* ((`(,body ,display-p) (math-cache-formula n))
               (source (if display-p (concat "\\[" body "\\]")
                         (concat "$" body "$")))
               (accept (lambda (image)
                         (unless (> (car (image-size image t)) 0) (error "Empty image"))
                         (funcall done))))
    (if (eq math-cache-engine 'mathjax)
        (let ((buffer (generate-new-buffer " *math-cache*")))
          (with-current-buffer buffer
            (insert source)
            (mathjax-display
             (point-min) (point-max) body :options (list :display display-p)
             :after (lambda (ov)
                      (unwind-protect
                          (funcall accept (overlay-get ov 'display))
                        (kill-buffer buffer))))))
      (cl-labels ((request ()
                    (when-let* ((image (latex-to-svg-backend
                                       source :engine math-cache-engine :fallback nil
                                       :x-height 10 :color "#000000" :callback #'request)))
                      (funcall accept image))))
        (request)))))

(defun math-cache-measure (ids)
  "Measure total and enqueue milliseconds for the corpus items IDS."
  (let ((remaining (length ids)) (start (float-time)) enqueue)
    (dolist (n ids) (math-cache-request n (lambda () (cl-decf remaining))))
    (setq enqueue (* 1000 (- (float-time) start)))
    (while (and (> remaining 0) (< (- (float-time) start) 60))
      (accept-process-output nil 0.001))
    (unless (zerop remaining) (error "Render timeout"))
    (list :total-ms (* 1000 (- (float-time) start)) :enqueue-ms enqueue)))

(defun math-cache-run ()
  "Execute the selected cache scenario and exit the isolated Emacs."
  (let ((phase (intern (getenv "MATH_CACHE_PHASE")))
        (base (* 1000 (string-to-number (or (getenv "MATH_CACHE_RUN") "1"))))
        (code 0) result)
    (condition-case err
        (progn
          (unless (display-graphic-p) (error "Graphical Emacs required"))
          (if (eq math-cache-engine 'mathjax)
              (require 'mathjax)
            (require 'latex-to-svg-backend)
            (setq latex-to-svg-backend-cache-directory (getenv "MATH_CACHE_DIRECTORY")
                  latex-to-svg-backend-gc-interval nil)
            (when (getenv "RATEX_PROGRAM")
              (setq latex-to-svg-backend-ratex-program (getenv "RATEX_PROGRAM"))))
          (setq result
                (pcase phase
                  ((or 'seed 'reopen) (math-cache-measure (number-sequence 0 29)))
                  ('new (math-cache-measure (list (+ base 50))))
                  ('idle
                   (let ((first (math-cache-measure (list base)))
                         (warm (math-cache-measure (list (+ base 10))))
                         (deadline (+ (float-time) 61)))
                     (while (< (float-time) deadline)
                       (accept-process-output nil 0.2))
                     (list :first first :warm warm
                           :node-after-idle (and (process-live-p (get-process "mathjax")) t)
                           :after-idle (math-cache-measure (list (+ base 20)))))))))
      (error (setq code 1 result (list :error (error-message-string err)))))
    (with-temp-file (getenv "MATH_CACHE_OUTPUT")
      (prin1 (list :engine math-cache-engine :phase phase :run base :pid (emacs-pid) :result result)
             (current-buffer))
      (insert "\n"))
    (kill-emacs code)))

(add-hook 'window-setup-hook (lambda () (run-at-time 0.5 nil #'math-cache-run)))
;;; benchmark-math-cache.el ends here
