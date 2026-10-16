;;; markdown-ts-appear-compat.el --- Native Markdown integration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Keep the private markdown-ts-mode dependencies here.  Fontifier advice is
;; necessary because the major mode has no source-reveal extension point.
;; The remaining helpers preserve its math validation, image-container and
;; link-opening semantics without copying their implementations.

;;; Code:

(require 'markdown-ts-mode)
(require 'seq)

(defconst markdown-ts-appear--markup-invisibility 'markdown-ts--markup
  "Native invisibility category for hidden Markdown markup.")

(defalias 'markdown-ts-appear--native-latex-valid-p #'markdown-ts--latex-block-valid-p)
(defalias 'markdown-ts-appear--native-image-alone-p #'markdown-ts--image-alone-on-line-p)
(defalias 'markdown-ts-appear--native-set-hide-markup #'markdown-ts--set-hide-markup)
(defalias 'markdown-ts-appear--native-link-button #'markdown-ts--make-link-button)

(defconst markdown-ts-appear--visible-fontifiers
  '(markdown-ts--fontify-atx-heading
    markdown-ts--fontify-setext-heading
    markdown-ts--fontify-link-ref-label
    markdown-ts--fontify-link-ref-destination
    markdown-ts--fontify-unordered-list-marker
    markdown-ts--fontify-checkbox
    markdown-ts--fontify-autolink
    markdown-ts--fontify-backslash-escape
    markdown-ts--fontify-entity
    markdown-ts--fontify-hard-line-break
    markdown-ts--fontify-thematic-break)
  "Markdown fontifiers that replace or hide source markup.")

(defconst markdown-ts-appear--advice-bindings
  (append
   '((markdown-ts--fontify-delimiter . markdown-ts-appear--fontify-delimiter)
     (markdown-ts--fontify-atx-delimiter . markdown-ts-appear--fontify-heading-marker)
     (markdown-ts--fontify-link-destination . markdown-ts-appear--fontify-link-destination)
     (markdown-ts--fontify-link-node . markdown-ts-appear--fontify-link)
     (markdown-ts--fontify-image . markdown-ts-appear-image--fontify)
     (markdown-ts--fontify-latex-block . markdown-ts-appear--fontify-node)
     (line-move . markdown-ts-appear-table--line-move))
   (mapcar (lambda (function)
             (cons function 'markdown-ts-appear--fontify-visible-markup))
           markdown-ts-appear--visible-fontifiers))
  "Fontification and table-motion functions with their package advice.")

(defconst markdown-ts-appear--required-private-functions
  '(markdown-ts--latex-block-valid-p
    markdown-ts--image-alone-on-line-p
    markdown-ts--make-link-button
    markdown-ts--set-hide-markup)
  "Private Markdown helpers required by Markdown TS Appear.")

(defun markdown-ts-appear--missing-private-functions ()
  "Return required private Markdown functions that are unavailable."
  (seq-remove
   #'fboundp
   (append (mapcar #'car markdown-ts-appear--advice-bindings)
           markdown-ts-appear--required-private-functions)))

(defun markdown-ts-appear--set-advice (install-p)
  "Install package advice when INSTALL-P is non-nil; otherwise remove it."
  (dolist (binding markdown-ts-appear--advice-bindings)
    (if install-p
        (advice-add (car binding) :around (cdr binding))
      (advice-remove (car binding) (cdr binding)))))

(defun markdown-ts-appear--install-advice ()
  "Check and install native Markdown integration."
  (when-let* ((missing (markdown-ts-appear--missing-private-functions)))
    (error "Required private markdown-ts-mode functions are unavailable: %S"
           missing))
  (markdown-ts-appear--set-advice t))

(provide 'markdown-ts-appear-compat)
;;; markdown-ts-appear-compat.el ends here
