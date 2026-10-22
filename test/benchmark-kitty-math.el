;;; benchmark-kitty-math.el --- Kitty math hot-path measurements -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Run in a fresh Kitty: emacs -nw -Q -l test/benchmark-kitty-math.el
;; KITTY_BENCH_ROOT selects a checkout; KITTY_BENCH_LOG selects the result file.
;; Uses real MathJax/PNG preparation and terminal placements. Timed handler
;; loops exclude idle waiting; the final case uses ordinary command-loop input.

(require 'package)
(package-initialize)
(setq load-prefer-newer t native-comp-jit-compilation nil)
(add-to-list 'load-path (or (getenv "KITTY_BENCH_ROOT")
                           (expand-file-name ".." (file-name-directory load-file-name))))
(require 'markdown-ts-appear-mathjax-kitty)
(require 'kitty-graphics)
(defvar kitty-bench--log (or (getenv "KITTY_BENCH_LOG") (make-temp-file "kitty-bench-")))
(defvar kitty-bench--counts (make-hash-table))
(defvar kitty-bench--buffer nil)
(defvar kitty-bench--source nil)
(defvar kitty-bench--results nil)
(defvar kitty-bench--steps 0)
(defvar kitty-bench--start nil)
(defvar kitty-bench--gc-start nil)
(defvar kitty-bench--samples nil)
(defvar kitty-bench--command-samples nil)
(defconst kitty-bench--command-count 200)
(defvar kitty-bench--deadline (+ (float-time) 30))
(dolist (fn '(markdown-ts-appear-mathjax-kitty--geometry
              markdown-ts-appear-mathjax-kitty--rasterize markdown-ts-appear-mathjax--send
              kitty-graphics--image-pixel-size kitty-graphics--place-image))
  (advice-add fn :before (lambda (&rest _)
                          (puthash fn (1+ (gethash fn kitty-bench--counts 0)) kitty-bench--counts))))

(defun kitty-bench--counts ()
  "Return measured call counts."
  (let (result)
    (maphash (lambda (fn count) (push (cons fn count) result)) kitty-bench--counts)
    result))

(defun kitty-bench--finish (&optional error)
  "Write measured results, with ERROR if interrupted, and exit."
  (when (and (not error)
             (not (equal kitty-bench--source (with-current-buffer kitty-bench--buffer (buffer-string)))))
    (setq error "Terminal input changed the benchmark source"))
  (with-temp-file kitty-bench--log
    (pp (list :error error :emacs emacs-version
              :library (symbol-file 'markdown-ts-appear-mathjax-kitty--sync)
              :columns (window-body-width) :rows (window-body-height)
              :gc-cons-threshold gc-cons-threshold
              :results (nreverse kitty-bench--results)) (current-buffer)))
  (let ((kill-emacs-hook nil)) (kill-emacs (if error 1 0))))

(defun kitty-bench--time (name count function)
  "Time COUNT calls of FUNCTION under NAME, reporting counters separately."
  (garbage-collect)
  (clrhash kitty-bench--counts)
  (let ((start (float-time)) (gc gc-elapsed))
    (dotimes (i count) (funcall function i))
    (push (list :case name :iterations count :ms (* 1000 (- (float-time) start))
                :gc-ms (* 1000 (- gc-elapsed gc)) :calls (kitty-bench--counts))
          kitty-bench--results)))

(defun kitty-bench--input ()
  "Queue a normal cursor-motion command."
  (push (if (zerop (% kitty-bench--steps 2)) ?\C-f ?\C-b) unread-command-events))

(defun kitty-bench--pre ()
  "Start measuring an actual movement command."
  (when (memq this-command '(forward-char backward-char))
    (setq kitty-bench--start (float-time) kitty-bench--gc-start gc-elapsed)))

(defun kitty-bench--post ()
  "Include Emacs redisplay in input latency; allow queued Kitty repaint to run."
  (when kitty-bench--start
    (let ((before-redisplay (float-time)))
      (redisplay t)
      (push (* 1000 (- (float-time) kitty-bench--start)) kitty-bench--samples)
      (push (list this-command :handler-ms (* 1000 (- before-redisplay kitty-bench--start))
                  :total-ms (car kitty-bench--samples) :gc-ms (* 1000 (- gc-elapsed kitty-bench--gc-start)))
            kitty-bench--command-samples))
    (setq kitty-bench--start nil)
    (if (< (cl-incf kitty-bench--steps) kitty-bench--command-count)
        (run-at-time 0.04 nil #'kitty-bench--input)
      (run-at-time 0.15 nil
                   (lambda ()
                     (let* ((sorted (sort kitty-bench--samples #'<))
                            (middle (/ kitty-bench--command-count 2)))
                       (push (list :case 'command-loop-reveal :iterations kitty-bench--command-count
                                   :median-ms (/ (+ (nth (1- middle) sorted) (nth middle sorted)) 2.0)
                                   :p95-ms (nth (1- (ceiling (* 0.95 kitty-bench--command-count))) sorted)
                                   :max-ms (car (last sorted))
                                   :commands (nreverse kitty-bench--command-samples)
                                   :calls (kitty-bench--counts)) kitty-bench--results))
                     (kitty-bench--finish))))))

(defun kitty-bench--measure ()
  "Wait for real images, then measure cached motion and reveal behavior."
  (condition-case err
      (with-current-buffer kitty-bench--buffer
        (if (not (cl-every (lambda (ov)
                            (when-let* ((view (overlay-get ov 'markdown-ts-appear-mathjax-kitty--view)))
                              (overlay-get view 'kitty-graphics-placements)))
                          markdown-ts-appear-mathjax--objects))
            (if (< (float-time) kitty-bench--deadline)
                (run-at-time 0.1 nil #'kitty-bench--measure)
              (error "Images did not become ready"))
          (let ((display (seq-find (lambda (ov) (eq (overlay-get ov 'markdown-ts-appear-mathjax--kind) 'display))
                                   markdown-ts-appear-mathjax--objects))
                (inline (seq-find (lambda (ov) (eq (overlay-get ov 'markdown-ts-appear-mathjax--kind) 'inline))
                                  markdown-ts-appear-mathjax--objects)))
            (goto-char (+ (overlay-start display) 4))
            (markdown-ts-appear-mathjax--post-command)
            (kitty-bench--time
             'display-motion 1000
             (lambda (i)
               (goto-char (+ (overlay-start display) 4 (% i 2)))
               (markdown-ts-appear-mathjax--post-command)
               (markdown-ts-appear-mathjax-kitty--repaint)))
            (goto-char (+ (overlay-start inline) 2))
            (markdown-ts-appear-mathjax--post-command)
            (kitty-bench--time
             'inline-source-motion 1000
             (lambda (i)
               (goto-char (+ (overlay-start inline) 2 (% i 2)))
               (markdown-ts-appear-mathjax--post-command)
               (markdown-ts-appear-mathjax-kitty--repaint)))
            (kitty-bench--time
             'cached-reveal 40
             (lambda (i)
               (goto-char (+ (overlay-start inline) (if (zerop (% i 2)) -1 1)))
               (markdown-ts-appear-mathjax--post-command)))
            (goto-char (1- (overlay-start inline)))
            (markdown-ts-appear-mathjax--post-command)
            (run-at-time
             0.2 nil
             (lambda ()
               ;; Give both checkouts the same GC starting point, independent
               ;; of allocations and subprocesses in the preceding loop cases.
               (garbage-collect)
               (clrhash kitty-bench--counts)
               (add-hook 'pre-command-hook #'kitty-bench--pre -100 t)
               (add-hook 'post-command-hook #'kitty-bench--post 100 t)
               (kitty-bench--input))))))
    (error (kitty-bench--finish (error-message-string err)))))

(setq markdown-ts-appear-mathjax-terminal-preview t markdown-ts-appear-mathjax-edit-preview t
      markdown-ts-appear-mathjax-cache-directory (make-temp-file "kitty-bench-cache-" t))
(kitty-graphics-mode 1)
(setq kitty-bench--buffer (generate-new-buffer "*Kitty benchmark*"))
(switch-to-buffer kitty-bench--buffer)
(insert "# Benchmark\n\nBefore $e^{i\\pi}+1=0$ after.\n\n$$\n\\frac{1}{1+e^{-x}}\n$$\n\nEnd\n")
(setq kitty-bench--source (buffer-string))
(markdown-ts-mode)
(goto-char (point-min))
(markdown-ts-appear-mathjax-mode 1)
(run-at-time 0.5 nil #'kitty-bench--measure)
(run-at-time 45 nil (lambda () (kitty-bench--finish "Timed out")))
