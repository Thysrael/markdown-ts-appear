;;; markdown-ts-appear-math.el --- Coordinate optional math previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; MathJax and latex-to-svg own detection, rendering and preview overlays.
;; Appear coordinates source reveal, fontification and table images, preserving
;; independently enabled modes when its integration is detached.

;;; Code:
(require 'seq)
(require 'cl-lib)
(require 'lisp-mnt)

(declare-function latex-to-svg-for-markdown-mode "latex-to-svg-for-markdown")
(declare-function latex-to-svg-frontend-span-at "latex-to-svg-frontend")
(declare-function latex-to-svg-frontend-spans "latex-to-svg-frontend")
(declare-function latex-to-svg-frontend-on-appearance-change "latex-to-svg-frontend")
(declare-function latex-to-svg-frontend-mode "latex-to-svg-frontend")
(declare-function markdown-ts-appear-mathjax-mode "markdown-ts-appear-mathjax")
(declare-function markdown-ts-appear-mathjax-spans "markdown-ts-appear-mathjax")
(declare-function markdown-ts-appear-mathjax-span-at "markdown-ts-appear-mathjax")
(declare-function markdown-ts-appear--active-p "markdown-ts-appear")
(declare-function markdown-ts-appear--tracking-p "markdown-ts-appear")
(declare-function markdown-ts-appear--update "markdown-ts-appear")
(declare-function markdown-ts-appear-table--math-changed "markdown-ts-appear-table")
(declare-function markdown-ts-appear-table--invalidate "markdown-ts-appear-table")
(declare-function markdown-ts-appear-table--schedule-render "markdown-ts-appear-table")

(defvar markdown-ts-appear-enable-math-preview)
(defvar markdown-ts-appear-math-renderer)
(defvar markdown-ts-appear--last-point)
(defvar latex-to-svg-frontend-reveal)
(defvar markdown-ts-appear-table--overlays)
(defvar markdown-ts-appear-mathjax--fontifying-spans)

(defvar-local markdown-ts-appear-math--owned nil
  "Non-nil when appear enabled the Markdown math adaptor.")
(defvar-local markdown-ts-appear-math--mathjax-owned nil
  "Non-nil when appear enabled the independent MathJax mode.")
(defvar-local markdown-ts-appear-math--saved-reveal nil
  "Previous (LOCAL-P VALUE) of the math source-reveal option.")

(defvar markdown-ts-appear-math--fontifying-spans nil
  "Current (BUFFER . FUNCTION) returning math spans for a fontification pass.")

(defconst markdown-ts-appear-math--requirements
  '((latex-to-svg-for-markdown "0.22.2" "20261017.1107")
    (latex-to-svg-frontend "0.22.2" "20261017.1107")
    (latex-to-svg-backend "0.15.0" "20261014.1334"))
  "Minimum stable and MELPA versions for each math dependency.")

(defun markdown-ts-appear-math--loaded-version (package)
  "Read PACKAGE's version from the library actually loaded in this session.
Do not use `load-path': an upgrade may have activated a newer directory
while the old library is still loaded.  Return nil if its source is unavailable."
  (when-let* ((file (car (seq-find
                         (lambda (entry) (member (cons 'provide package) (cdr entry)))
                         load-history)))
              (source (if (string-suffix-p ".elc" file) (substring file 0 -1) file))
              (source (cond ((file-readable-p source) source)
                            ((file-readable-p (concat source ".gz")) (concat source ".gz")))))
    (with-temp-buffer
      (insert-file-contents source nil 0 4096)
      (lm-package-version))))

(defun markdown-ts-appear-math--check-dependencies ()
  "Reject unsupported math dependencies before taking over their reveal policy."
  (dolist (requirement markdown-ts-appear-math--requirements)
    (let* ((package (car requirement))
           (version (markdown-ts-appear-math--loaded-version package))
           (parsed (and version (ignore-errors (version-to-list version))))
           (minimum (if (and parsed (>= (car parsed) 20000000))
                        (nth 2 requirement) (cadr requirement))))
      (unless (and parsed (version-list-<= (version-to-list minimum) parsed))
        (user-error
         "Math previews require %s %s+ (MELPA %s+); loaded %s. Upgrade the latex-to-svg packages and restart Emacs"
         package (cadr requirement) (nth 2 requirement) (or version "an unknown version")))))
  (unless (and (fboundp 'latex-to-svg-frontend-span-at)
               (fboundp 'latex-to-svg-frontend-spans))
    (user-error "Update latex-to-svg and restart Emacs: its span/reveal APIs are unavailable")))

(defun markdown-ts-appear-math--reveal-p (_kind)
  "Return non-nil while appear tracks source at point, for any math _KIND."
  (and (markdown-ts-appear--active-p) (markdown-ts-appear--tracking-p)))

(defun markdown-ts-appear-math--appearance-change ()
  "Refresh loaded math previews through the frontend's appearance handler."
  (when (featurep 'latex-to-svg-frontend)
    (latex-to-svg-frontend-on-appearance-change)))

(defun markdown-ts-appear-math--span-at (position)
  "Return the external math span at POSITION, or nil."
  (if (bound-and-true-p markdown-ts-appear-mathjax-mode)
      (markdown-ts-appear-mathjax-span-at position)
    (and (bound-and-true-p latex-to-svg-frontend-mode)
         (fboundp 'latex-to-svg-frontend-span-at)
         (latex-to-svg-frontend-span-at position))))

(defun markdown-ts-appear-math--spans (beg end)
  "Return the active provider's spans between BEG and END."
  (cond ((bound-and-true-p markdown-ts-appear-mathjax-mode)
         (markdown-ts-appear-mathjax-spans beg end))
        ((bound-and-true-p latex-to-svg-frontend-mode)
         (latex-to-svg-frontend-spans beg end))))

(defun markdown-ts-appear-math--contains-p (beg end)
  "Return non-nil when BEG..END is inside external math source."
  (seq-some
   (lambda (span)
     (and span (<= (nth 1 span) beg) (< beg (nth 2 span)) (<= end (nth 2 span))))
   (if (eq (car markdown-ts-appear-math--fontifying-spans) (current-buffer))
       (funcall (cdr markdown-ts-appear-math--fontifying-spans))
     (list (markdown-ts-appear-math--span-at beg)))))

(defun markdown-ts-appear-math--fontify (function beg end &rest arguments)
  "Call fontifier FUNCTION for BEG..END with ARGUMENTS, sharing math detection."
  (let* ((spans 'unknown)
         (markdown-ts-appear-math--fontifying-spans
          (cons (current-buffer)
                (lambda ()
                  (when (eq spans 'unknown)
                    (setq spans
                          (save-match-data (markdown-ts-appear-math--spans beg end))))
                  spans)))
         (markdown-ts-appear-mathjax--fontifying-spans markdown-ts-appear-math--fontifying-spans))
    (apply function beg end arguments)))

(defun markdown-ts-appear-math--revealed-region ()
  "Return math source bounds that table overlays must leave visible."
  (when (and (or (bound-and-true-p markdown-ts-appear-mathjax-mode)
                 (bound-and-true-p latex-to-svg-frontend-mode))
             (or (bound-and-true-p isearch-mode)
                 (markdown-ts-appear-math--reveal-p nil)))
    (when-let* ((span (markdown-ts-appear-math--span-at (point))))
      (cons (nth 1 span) (nth 2 span)))))

(defun markdown-ts-appear-math--table-spans (beg end)
  "Return inline (BEG END IMAGE OVERLAY) records for a table between BEG and END.
Use existing preview overlays; this never requests or compiles an image."
  (when (and (or (bound-and-true-p markdown-ts-appear-mathjax-mode)
                 (bound-and-true-p latex-to-svg-frontend-mode))
             (save-excursion (goto-char beg) (re-search-forward "[\\\\$]" end t)))
    (cl-loop for (kind start finish) in (markdown-ts-appear-math--spans beg end)
             when (and (eq kind 'inline) (<= beg start) (<= finish end))
             collect
             (let* ((overlay (seq-find
                              (lambda (ov)
                                (and (= start (overlay-start ov)) (= finish (overlay-end ov))
                                     (eq (car-safe (overlay-get ov 'display)) 'image)))
                              (overlays-at start)))
                    (image (and overlay (overlay-get overlay 'display))))
               (list start finish image overlay)))))

(defun markdown-ts-appear-math--image-shown (overlay image)
  "Notify the owning table when OVERLAY shows a new IMAGE."
  (when-let* ((buffer (overlay-buffer overlay)))
    (with-current-buffer buffer
      (when (markdown-ts-appear--active-p)
        (markdown-ts-appear-table--math-changed overlay image)))))

(defun markdown-ts-appear-math--watch-images ()
  "Observe image updates only while a buffer uses appear's math integration."
  ;; The frontend has no public preview-change hook yet.  Keep this single
  ;; notification adapter here; tables read only ordinary overlay displays.
  (if (seq-some (lambda (buffer)
                  (buffer-local-value 'markdown-ts-appear-math--saved-reveal buffer))
                (buffer-list))
      (advice-add 'latex-to-svg-frontend--show-image :after #'markdown-ts-appear-math--image-shown)
    (advice-remove 'latex-to-svg-frontend--show-image #'markdown-ts-appear-math--image-shown)))

(defun markdown-ts-appear-math--invalidate-tables ()
  "Rebuild wrapped tables after a math provider starts or stops."
  (when (and (markdown-ts-appear--active-p) markdown-ts-appear-table--overlays)
    (markdown-ts-appear-table--invalidate (point-min) (point-max))
    (markdown-ts-appear-table--schedule-render)))

(defun markdown-ts-appear-math--restore-reveal ()
  "Restore the previous reveal option if it is still owned by appear."
  (when markdown-ts-appear-math--saved-reveal
    (when (eq latex-to-svg-frontend-reveal #'markdown-ts-appear-math--reveal-p)
      (if (car markdown-ts-appear-math--saved-reveal)
          (setq-local latex-to-svg-frontend-reveal
                      (cadr markdown-ts-appear-math--saved-reveal))
        (kill-local-variable 'latex-to-svg-frontend-reveal)))
    (setq markdown-ts-appear-math--saved-reveal nil)))

(defun markdown-ts-appear-math--sync ()
  "Coordinate source reveal when the external math mode is toggled."
  (if (and (markdown-ts-appear--active-p)
           (bound-and-true-p latex-to-svg-frontend-mode))
      (unless markdown-ts-appear-math--saved-reveal
        (markdown-ts-appear-math--check-dependencies)
        (setq markdown-ts-appear-math--saved-reveal
              (list (local-variable-p 'latex-to-svg-frontend-reveal)
                    latex-to-svg-frontend-reveal))
        (setq-local latex-to-svg-frontend-reveal #'markdown-ts-appear-math--reveal-p))
    (markdown-ts-appear-math--restore-reveal)
    (setq markdown-ts-appear-math--owned nil))
  (markdown-ts-appear-math--watch-images)
  (markdown-ts-appear-math--invalidate-tables)
  (setq markdown-ts-appear--last-point nil)
  (when (markdown-ts-appear--tracking-p)
    (markdown-ts-appear--update)))

(defun markdown-ts-appear-math--latex-setup ()
  "Attach the reveal policy and optionally enable latex-to-svg."
  (add-hook 'latex-to-svg-frontend-mode-hook #'markdown-ts-appear-math--sync nil t)
  (cond
   ((bound-and-true-p latex-to-svg-frontend-mode)
    (markdown-ts-appear-math--sync))
   (markdown-ts-appear-enable-math-preview
    (unless (require 'latex-to-svg-for-markdown nil t)
      (user-error "Install latex-to-svg-for-markdown 0.22.2+ and a rendering engine for math previews"))
    (markdown-ts-appear-math--check-dependencies)
    (setq markdown-ts-appear-math--owned t)
    (latex-to-svg-for-markdown-mode 1))))

(defun markdown-ts-appear-math--mathjax-sync ()
  "Coordinate an independently toggled MathJax mode with appear."
  (if (bound-and-true-p markdown-ts-appear-mathjax-mode)
      (add-hook 'markdown-ts-appear-mathjax-preview-functions
                #'markdown-ts-appear-math--image-shown nil t)
    (remove-hook 'markdown-ts-appear-mathjax-preview-functions
                 #'markdown-ts-appear-math--image-shown t)
    (setq markdown-ts-appear-math--mathjax-owned nil))
  (markdown-ts-appear-math--invalidate-tables))

(defun markdown-ts-appear-math--renderer ()
  "Return the selected renderer, resolving automatic availability checks."
  (if (eq markdown-ts-appear-math-renderer 'auto)
      (if (and (locate-library "mathjax") (executable-find "node"))
          'mathjax
        'latex-to-svg)
    markdown-ts-appear-math-renderer))

(defun markdown-ts-appear-math--setup ()
  "Adopt an active renderer, or start the selected provider when enabled."
  (add-function :around (local 'font-lock-fontify-region-function)
                #'markdown-ts-appear-math--fontify)
  (add-hook 'markdown-ts-appear-mathjax-mode-hook #'markdown-ts-appear-math--mathjax-sync nil t)
  (cond
   ((bound-and-true-p markdown-ts-appear-mathjax-mode)
    (markdown-ts-appear-math--mathjax-sync))
   ((bound-and-true-p latex-to-svg-frontend-mode)
    (markdown-ts-appear-math--latex-setup))
   ((and markdown-ts-appear-enable-math-preview
         (eq (markdown-ts-appear-math--renderer) 'mathjax))
    (require 'markdown-ts-appear-mathjax)
    (setq markdown-ts-appear-math--mathjax-owned t)
    (markdown-ts-appear-mathjax-mode 1))
   (t (markdown-ts-appear-math--latex-setup))))

(defun markdown-ts-appear-math--teardown ()
  "Detach integration, stopping only an adaptor enabled by appear."
  (remove-function (local 'font-lock-fontify-region-function)
                    #'markdown-ts-appear-math--fontify)
  (remove-hook 'markdown-ts-appear-mathjax-mode-hook #'markdown-ts-appear-math--mathjax-sync t)
  (remove-hook 'markdown-ts-appear-mathjax-preview-functions #'markdown-ts-appear-math--image-shown t)
  (when markdown-ts-appear-math--mathjax-owned
    (setq markdown-ts-appear-math--mathjax-owned nil)
    (markdown-ts-appear-mathjax-mode -1))
  (remove-hook 'latex-to-svg-frontend-mode-hook #'markdown-ts-appear-math--sync t)
  (when markdown-ts-appear-math--owned
    (setq markdown-ts-appear-math--owned nil)
    (latex-to-svg-for-markdown-mode -1))
  (markdown-ts-appear-math--restore-reveal)
  (markdown-ts-appear-math--watch-images))

(defun markdown-ts-appear-math--restart ()
  "Switch an active appear buffer to its explicitly selected math renderer."
  (markdown-ts-appear-math--teardown)
  (when (bound-and-true-p markdown-ts-appear-mathjax-mode)
    (markdown-ts-appear-mathjax-mode -1))
  (when (bound-and-true-p latex-to-svg-for-markdown-mode)
    (latex-to-svg-for-markdown-mode -1))
  (when (bound-and-true-p latex-to-svg-frontend-mode)
    (latex-to-svg-frontend-mode -1))
  (markdown-ts-appear-math--setup))

(defun markdown-ts-appear-math--detach ()
  "Detach appear's integration, leaving the clone's own math mode active."
  ;; Not owned in the clone, so the teardown leaves the adaptor on.
  (setq markdown-ts-appear-math--owned nil
        markdown-ts-appear-math--mathjax-owned nil)
  (markdown-ts-appear-math--teardown))

(provide 'markdown-ts-appear-math)
;;; markdown-ts-appear-math.el ends here
