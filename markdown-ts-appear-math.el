;;; markdown-ts-appear-math.el --- Coordinate latex-to-svg previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; The optional Markdown adaptor owns math detection, rendering and overlays.
;; Appear supplies its reveal policy and restores the user's setup on teardown.

;;; Code:
(require 'seq)

(declare-function latex-to-svg-for-markdown-mode "latex-to-svg-for-markdown")
(declare-function latex-to-svg-frontend-span-at "latex-to-svg-frontend")
(declare-function latex-to-svg-frontend-spans "latex-to-svg-frontend")
(declare-function latex-to-svg-frontend-on-appearance-change "latex-to-svg-frontend")
(declare-function markdown-ts-appear--active-p "markdown-ts-appear")
(declare-function markdown-ts-appear--tracking-p "markdown-ts-appear")
(declare-function markdown-ts-appear--update "markdown-ts-appear")

(defvar markdown-ts-appear-enable-math-preview)
(defvar markdown-ts-appear--last-point)
(defvar latex-to-svg-frontend-reveal)
(defvar latex-to-svg-frontend--last-point)
(defvar latex-to-svg-frontend--reconcile-timer)
(defvar latex-to-svg-frontend--initial-render-timer)
(defvar latex-to-svg-frontend--requests)

(defvar-local markdown-ts-appear-math--owned nil
  "Non-nil when appear enabled the Markdown math adaptor.")
(defvar-local markdown-ts-appear-math--saved-reveal nil
  "Previous (LOCAL-P VALUE) of the math source-reveal option.")

(defvar markdown-ts-appear-math--fontifying-spans nil
  "Current (BUFFER . FUNCTION) returning math spans for a fontification pass.")

(defun markdown-ts-appear-math--reveal-p (_kind)
  "Return non-nil while appear tracks source at point, for any math _KIND."
  (and (markdown-ts-appear--active-p) (markdown-ts-appear--tracking-p)))

(defun markdown-ts-appear-math--appearance-change ()
  "Refresh loaded math previews through the frontend's appearance handler."
  (when (featurep 'latex-to-svg-frontend)
    (latex-to-svg-frontend-on-appearance-change)))

(defun markdown-ts-appear-math--span-at (position)
  "Return the external math span at POSITION, or nil."
  (and (bound-and-true-p latex-to-svg-frontend-mode)
       (fboundp 'latex-to-svg-frontend-span-at)
       (latex-to-svg-frontend-span-at position)))

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
                          (when (bound-and-true-p latex-to-svg-frontend-mode)
                            (save-match-data (latex-to-svg-frontend-spans beg end)))))
                  spans))))
    (apply function beg end arguments)))

(defun markdown-ts-appear-math--revealed-region ()
  "Return math source bounds that table overlays must leave visible."
  (when (and (bound-and-true-p latex-to-svg-frontend-mode)
             (or (bound-and-true-p isearch-mode)
                 (markdown-ts-appear-math--reveal-p nil)))
    (when-let* ((span (markdown-ts-appear-math--span-at (point))))
      (cons (nth 1 span) (nth 2 span)))))

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
        (unless (and (fboundp 'latex-to-svg-frontend-span-at)
                     (fboundp 'latex-to-svg-frontend-spans))
          (user-error "Update latex-to-svg: this integration requires its span/reveal APIs"))
        (setq markdown-ts-appear-math--saved-reveal
              (list (local-variable-p 'latex-to-svg-frontend-reveal)
                    latex-to-svg-frontend-reveal))
        (setq-local latex-to-svg-frontend-reveal #'markdown-ts-appear-math--reveal-p))
    (markdown-ts-appear-math--restore-reveal)
    (setq markdown-ts-appear-math--owned nil))
  (setq markdown-ts-appear--last-point nil)
  (when (markdown-ts-appear--tracking-p)
    (markdown-ts-appear--update)))

(defun markdown-ts-appear-math--setup ()
  "Attach the reveal policy and optionally enable latex-to-svg."
  (add-function :around (local 'font-lock-fontify-region-function)
                #'markdown-ts-appear-math--fontify)
  (add-hook 'latex-to-svg-frontend-mode-hook #'markdown-ts-appear-math--sync nil t)
  (cond
   ((bound-and-true-p latex-to-svg-frontend-mode)
    (markdown-ts-appear-math--sync))
   (markdown-ts-appear-enable-math-preview
    (unless (require 'latex-to-svg-for-markdown nil t)
      (user-error "Install the optional latex-to-svg-for-markdown package to enable math previews"))
    (unless (and (fboundp 'latex-to-svg-frontend-span-at)
                 (fboundp 'latex-to-svg-frontend-spans))
      (user-error "Update latex-to-svg: this integration requires its span/reveal APIs"))
    (setq markdown-ts-appear-math--owned t)
    (latex-to-svg-for-markdown-mode 1))))

(defun markdown-ts-appear-math--teardown ()
  "Detach integration, stopping only an adaptor enabled by appear."
  (remove-function (local 'font-lock-fontify-region-function)
                   #'markdown-ts-appear-math--fontify)
  (remove-hook 'latex-to-svg-frontend-mode-hook #'markdown-ts-appear-math--sync t)
  (when markdown-ts-appear-math--owned
    (setq markdown-ts-appear-math--owned nil)
    (latex-to-svg-for-markdown-mode -1))
  (markdown-ts-appear-math--restore-reveal))

(defun markdown-ts-appear-math--detach ()
  "Detach inherited math state in an unsupported indirect clone."
  ;; The frontend has no clone-detach API. Drop the clone's references before
  ;; teardown so it does not release the base buffer's pending work.
  (when (bound-and-true-p latex-to-svg-frontend-mode)
    (setq-local latex-to-svg-frontend--last-point nil
                latex-to-svg-frontend--reconcile-timer nil
                latex-to-svg-frontend--initial-render-timer nil
                latex-to-svg-frontend--requests nil)
    (remove-hook 'latex-to-svg-frontend-mode-hook #'markdown-ts-appear-math--sync t)
    (latex-to-svg-for-markdown-mode -1))
  (setq markdown-ts-appear-math--owned nil)
  (markdown-ts-appear-math--teardown))

(provide 'markdown-ts-appear-math)
;;; markdown-ts-appear-math.el ends here
