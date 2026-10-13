;;; markdown-ts-appear-math.el --- Coordinate latex-to-svg previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The optional latex-to-svg Markdown adaptor owns math detection, rendering
;; and overlays.  This bridge supplies appear's source-reveal policy and
;; remembers whether appear or the user enabled the adaptor.

;;; Code:

(declare-function latex-to-svg-for-markdown-mode "latex-to-svg-for-markdown")
(declare-function latex-to-svg-frontend-span-at "latex-to-svg-frontend")
(declare-function markdown-ts-appear--active-p "markdown-ts-appear")
(declare-function markdown-ts-appear--update "markdown-ts-appear")

(defvar markdown-ts-appear-enable-math-preview)
(defvar markdown-ts-appear--last-point)
(defvar latex-to-svg-frontend-reveal)
(defvar latex-to-svg-frontend--last-point)
(defvar latex-to-svg-frontend--reconcile-timer)
(defvar latex-to-svg-frontend--initial-render-timer)

(defvar-local markdown-ts-appear-math--owned nil
  "Non-nil when appear enabled the Markdown math adaptor.")

(defvar-local markdown-ts-appear-math--saved-reveal nil
  "Previous (LOCAL-P VALUE) of the math source-reveal option.")

(defun markdown-ts-appear-math--reveal-p (_kind)
  "Return non-nil while appear tracks source at point, for any math _KIND."
  (and (markdown-ts-appear--active-p)
       (memq #'markdown-ts-appear--update post-command-hook)))

(defun markdown-ts-appear-math--span-at (position)
  "Return the math span owned by latex-to-svg at POSITION, or nil."
  (and (bound-and-true-p latex-to-svg-frontend-mode)
       (fboundp 'latex-to-svg-frontend-span-at)
       (latex-to-svg-frontend-span-at position)))

(defun markdown-ts-appear-math--contains-p (beg end)
  "Return non-nil when BEG..END is inside external math source."
  (when-let* ((span (markdown-ts-appear-math--span-at beg)))
    (and (< beg (nth 2 span)) (<= end (nth 2 span)))))

(defun markdown-ts-appear-math--revealed-region ()
  "Return the math source bounds that table overlays must leave visible."
  (when (or (bound-and-true-p isearch-mode)
            (markdown-ts-appear-math--reveal-p nil))
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
        (unless (fboundp 'latex-to-svg-frontend-span-at)
          (user-error "Update latex-to-svg: this integration requires its span/reveal APIs"))
        (setq markdown-ts-appear-math--saved-reveal
              (list (local-variable-p 'latex-to-svg-frontend-reveal)
                    latex-to-svg-frontend-reveal))
        (setq-local latex-to-svg-frontend-reveal
                    #'markdown-ts-appear-math--reveal-p))
    (markdown-ts-appear-math--restore-reveal)
    (setq markdown-ts-appear-math--owned nil))
  ;; The cursor need not have moved when responsibility changes.
  (setq markdown-ts-appear--last-point nil)
  (when (memq #'markdown-ts-appear--update post-command-hook)
    (markdown-ts-appear--update)))

(defun markdown-ts-appear-math--setup ()
  "Attach the math reveal policy and optionally enable latex-to-svg."
  (add-hook 'latex-to-svg-frontend-mode-hook #'markdown-ts-appear-math--sync nil t)
  (when markdown-ts-appear-enable-math-preview
    (unless (require 'latex-to-svg-for-markdown nil t)
      (user-error "Install the optional latex-to-svg-for-markdown package to enable math previews"))
    (unless (fboundp 'latex-to-svg-frontend-span-at)
      (user-error "Update latex-to-svg: this integration requires its span/reveal APIs"))
    (unless (bound-and-true-p latex-to-svg-frontend-mode)
      (latex-to-svg-for-markdown-mode 1)
      (setq markdown-ts-appear-math--owned t)))
  (markdown-ts-appear-math--sync))

(defun markdown-ts-appear-math--teardown ()
  "Detach math integration, stopping only an adaptor enabled by appear."
  (remove-hook 'latex-to-svg-frontend-mode-hook #'markdown-ts-appear-math--sync t)
  (when markdown-ts-appear-math--owned
    (setq markdown-ts-appear-math--owned nil)
    (latex-to-svg-for-markdown-mode -1))
  (markdown-ts-appear-math--restore-reveal))

(defun markdown-ts-appear-math--detach ()
  "Detach inherited math state in an unsupported indirect clone."
  ;; The frontend currently has no indirect-clone detach API.  Its teardown
  ;; releases these shared objects, so first drop only the clone's references.
  (when (bound-and-true-p latex-to-svg-frontend-mode)
    (setq-local latex-to-svg-frontend--last-point nil
                latex-to-svg-frontend--reconcile-timer nil
                latex-to-svg-frontend--initial-render-timer nil)
    (remove-hook 'latex-to-svg-frontend-mode-hook #'markdown-ts-appear-math--sync t)
    (latex-to-svg-for-markdown-mode -1))
  (setq markdown-ts-appear-math--owned nil)
  (markdown-ts-appear-math--teardown))

(provide 'markdown-ts-appear-math)
;;; markdown-ts-appear-math.el ends here
