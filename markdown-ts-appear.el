;;; markdown-ts-appear.el --- Reveal Markdown source at point -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; Modifications Copyright (C) 2026 Dzming Li

;; Author: Thysrael <thysrael@163.com>
;; Adapted-by: Dzming Li
;; Version: 0.2.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: text, convenience
;; URL: https://github.com/hyperZphere/markdown-ts-appear

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `markdown-ts-appear-mode' reveals the smallest semantic Markdown element at
;; point.  It is a focused adaptation of markdown-ts-appear commit 0138e04 by
;; Thysrael.  MathJax previews, icons, rendering overlays, asynchronous jobs,
;; and caches have deliberately been removed.
;;
;; The only host is `markdown-ts-mode'.  This package and
;; `markdown-ts-modern-mode' intentionally style disjoint syntax elements, so
;; they can be enabled together without a dependency or integration layer.

;;; Code:

(require 'cl-lib)
(require 'markdown-ts-mode)
(require 'seq)

(defvar evil-insert-state-entry-hook)
(defvar evil-insert-state-exit-hook)
(defvar evil-state)

(defgroup markdown-ts-appear nil
  "Reveal Markdown source at point."
  :group 'markdown-ts
  :prefix "markdown-ts-appear-")

(defcustom markdown-ts-appear-trigger 'always
  "When `markdown-ts-appear-mode' should reveal source.
With `always', track point whenever the mode is enabled.  With
`evil-insert', track point only while Evil is in insert state."
  :type '(choice (const :tag "Whenever the mode is enabled" always)
                 (const :tag "Only in Evil insert state" evil-insert)))

(defvar-local markdown-ts-appear-mode nil
  "Non-nil when Markdown TS Appear mode is enabled.")

(defvar-local markdown-ts-appear--region nil
  "Markers delimiting the semantic Markdown source currently visible.")

(defvar-local markdown-ts-appear--last-point nil
  "Position checked by the most recent appear update.")

(defvar-local markdown-ts-appear--last-tick nil
  "Buffer modification tick checked by the most recent appear update.")

(defvar-local markdown-ts-appear--setup-p nil
  "Non-nil when appear integration is active in the current buffer.")

(defvar-local markdown-ts-appear--saved-hide-markup nil)
(defvar-local markdown-ts-appear--saved-hide-markup-local-p nil)
(defvar-local markdown-ts-appear--saved-invisibility-spec nil)
(defvar-local markdown-ts-appear--base-owner nil)

(defvar markdown-ts-appear--active-buffers nil
  "Live buffers in which `markdown-ts-appear-mode' is active.")

(defvar markdown-ts-appear--advice-installed-p nil
  "Non-nil when the shared fontifier advice is installed.")

(defun markdown-ts-appear--active-p ()
  "Return non-nil when appear mode controls fontification here."
  (or (and markdown-ts-appear-mode markdown-ts-appear--setup-p)
      (and (buffer-live-p markdown-ts-appear--base-owner)
           (buffer-local-value
            'markdown-ts-appear-mode markdown-ts-appear--base-owner))))

(defun markdown-ts-appear--visible-region ()
  "Return the reveal region governing the current buffer's text."
  (if (and (buffer-live-p markdown-ts-appear--base-owner)
           (buffer-local-value
            'markdown-ts-appear-mode markdown-ts-appear--base-owner))
      (buffer-local-value
       'markdown-ts-appear--region markdown-ts-appear--base-owner)
    markdown-ts-appear--region))

(defun markdown-ts-appear--region-visible-p (beginning end)
  "Return non-nil when BEGINNING through END overlaps visible source."
  (when-let* ((region (markdown-ts-appear--visible-region))
              (visible-beginning (marker-position (car region)))
              (visible-end (marker-position (cdr region))))
    (and (< beginning visible-end) (> end visible-beginning))))

(defun markdown-ts-appear--node-visible-p (node)
  "Return non-nil when NODE overlaps visible semantic Markdown source."
  (markdown-ts-appear--region-visible-p
   (treesit-node-start node) (treesit-node-end node)))

(defun markdown-ts-appear--fenced-code-block-at (position)
  "Return the fenced code block containing POSITION, if any."
  (let ((node (treesit-node-at position 'markdown)) block)
    (while (and node (not block))
      (when (and (equal (treesit-node-type node) "fenced_code_block")
                 (<= (treesit-node-start node) position)
                 (< position (treesit-node-end node)))
        (setq block node))
      (setq node (treesit-node-parent node)))
    block))

(defun markdown-ts-appear--wikilink-bounds-at (position)
  "Return Wiki-link bounds containing POSITION on the current line."
  (save-excursion
    (goto-char position)
    (goto-char (line-beginning-position))
    (let ((line-end (line-end-position)) bounds)
      (while (and (not bounds)
                  (re-search-forward "\\[\\[[^]\n]+]]" line-end t))
        (when (and (<= (match-beginning 0) position)
                   (< position (match-end 0)))
          (setq bounds (cons (match-beginning 0) (match-end 0)))))
      bounds)))

(defun markdown-ts-appear--restore ()
  "Restore normal rendering in the previously revealed region."
  (when-let* ((region markdown-ts-appear--region)
              (beginning (marker-position (car region)))
              (end (marker-position (cdr region))))
    (set-marker (car region) nil)
    (set-marker (cdr region) nil)
    (setq markdown-ts-appear--region nil)
    (save-restriction
      (widen)
      (font-lock-flush beginning end)
      (font-lock-ensure beginning end))))

(defun markdown-ts-appear--node-contains-p (node position)
  "Return non-nil when NODE semantically contains POSITION."
  (let ((beginning (treesit-node-start node))
        (end (treesit-node-end node)))
    (and (<= beginning position)
         (or (< position end)
             (and (= position end)
                  (> position beginning)
                  (not (eq (char-before position) ?\n)))))))

(defun markdown-ts-appear--inline-node-at (position)
  "Return the smallest revealable inline node at POSITION."
  (let ((node (treesit-node-at position 'markdown-inline)) found)
    (while (and node (not found))
      (let ((type (treesit-node-type node)))
        (when (and
               (markdown-ts-appear--node-contains-p node position)
               (member type
                       '("emphasis" "strong_emphasis" "strikethrough"
                         "code_span" "inline_link" "full_reference_link"
                         "collapsed_reference_link" "shortcut_link" "image"
                         "uri_autolink" "email_autolink" "entity_reference"
                         "numeric_character_reference" "backslash_escape"
                         "hard_line_break")))
          (setq found node)))
      (setq node (treesit-node-parent node)))
    ;; The inline grammar represents ~~text~~ as nested strikethrough nodes.
    (when (and found (equal (treesit-node-type found) "strikethrough"))
      (let ((parent (treesit-node-parent found)))
        (while (and parent
                    (equal (treesit-node-type parent) "strikethrough"))
          (setq found parent
                parent (treesit-node-parent parent)))))
    found))

(defun markdown-ts-appear--inline-bounds-at (position)
  "Return revealable inline-element bounds at POSITION."
  (when-let* ((node (markdown-ts-appear--inline-node-at position)))
    (let ((beginning (treesit-node-start node))
          (end (treesit-node-end node)))
      ;; Wiki links are parsed as a shortcut link inside extra brackets.
      (when (and (equal (treesit-node-type node) "shortcut_link")
                 (> beginning (point-min))
                 (< end (point-max))
                 (eq (char-before beginning) ?\[)
                 (eq (char-after end) ?\]))
        (setq beginning (1- beginning)
              end (1+ end)))
      (cons beginning end))))

(defun markdown-ts-appear--fallback-bounds-at (position)
  "Return conservative token bounds for malformed markup at POSITION."
  (save-excursion
    (goto-char position)
    (let ((line-beginning (line-beginning-position))
          (line-end (line-end-position)))
      (skip-chars-backward "^ \t\n" line-beginning)
      (let ((beginning (point)))
        (goto-char position)
        (skip-chars-forward "^ \t\n" line-end)
        (let ((end (point)))
          (when (and (< beginning end)
                     (or (memq (char-after beginning)
                               '(?* ?_ ?~ ?` ?< ?\\ ?\[ ?!))
                         (string-match-p
                          "\\[" (buffer-substring-no-properties
                                 beginning end))))
            (cons beginning end)))))))

(defun markdown-ts-appear--bounds-at-point (position)
  "Return source bounds for the smallest rendered element at POSITION."
  (or (markdown-ts-appear--wikilink-bounds-at position)
      (markdown-ts-appear--inline-bounds-at position)
      (markdown-ts-appear--fallback-bounds-at position)))

(defun markdown-ts-appear--bounds ()
  "Return source bounds for the smallest rendered element at point."
  (unless (markdown-ts-appear--fenced-code-block-at (point))
    (font-lock-ensure (line-beginning-position)
                      (min (point-max) (line-beginning-position 2)))
    (markdown-ts-appear--bounds-at-point (point))))

(defun markdown-ts-appear--update ()
  "Reveal source for the rendered Markdown element at point."
  (let ((position (point))
        (tick (buffer-chars-modified-tick)))
    (unless (and (equal position markdown-ts-appear--last-point)
                 (equal tick markdown-ts-appear--last-tick))
      (let* ((region markdown-ts-appear--region)
             (old-beginning
              (and region (marker-position (car region))))
             (old-end (and region (marker-position (cdr region)))))
        (save-restriction
          (widen)
          (let* ((bounds (markdown-ts-appear--bounds))
                 (beginning (car-safe bounds))
                 (end (cdr-safe bounds)))
            (unless (if bounds
                        (and old-beginning old-end
                             (= beginning old-beginning) (= end old-end))
                      (null region))
              (markdown-ts-appear--restore)
              (when bounds
                (setq markdown-ts-appear--region
                      (cons (copy-marker beginning)
                            (copy-marker end t)))
                (font-lock-flush beginning end)
                (font-lock-ensure beginning end)))))
        (setq markdown-ts-appear--last-point position
              markdown-ts-appear--last-tick tick)))))

(defun markdown-ts-appear--start ()
  "Start tracking the semantic Markdown element at point."
  (setq markdown-ts-appear--last-point nil
        markdown-ts-appear--last-tick nil)
  (add-hook 'post-command-hook #'markdown-ts-appear--update nil t)
  (markdown-ts-appear--update))

(defun markdown-ts-appear--stop ()
  "Stop tracking point and restore hidden Markdown markup."
  (remove-hook 'post-command-hook #'markdown-ts-appear--update t)
  (setq markdown-ts-appear--last-point nil
        markdown-ts-appear--last-tick nil)
  (markdown-ts-appear--restore))

(defun markdown-ts-appear--fontify-node (function node &rest arguments)
  "Call FUNCTION for NODE with ARGUMENTS without concealing visible source."
  (if (not (markdown-ts-appear--active-p))
      (apply function node arguments)
    (let ((markdown-ts-hide-markup
           (and markdown-ts-hide-markup
                (not (markdown-ts-appear--node-visible-p node)))))
      (apply function node arguments))))

(defun markdown-ts-appear--fontify-image (function node &rest arguments)
  "Call FUNCTION for image NODE with ARGUMENTS without covering visible source."
  (if (not (markdown-ts-appear--active-p))
      (apply function node arguments)
    (let ((markdown-ts-inline-images
           (and markdown-ts-inline-images
                (not (markdown-ts-appear--node-visible-p node)))))
      (apply function node arguments))))

(defconst markdown-ts-appear--markdown-fontifiers
  '(markdown-ts--fontify-delimiter
    markdown-ts--fontify-link-ref-label
    markdown-ts--fontify-link-ref-destination
    markdown-ts--fontify-link-destination
    markdown-ts--fontify-autolink
    markdown-ts--fontify-backslash-escape
    markdown-ts--fontify-entity
    markdown-ts--fontify-hard-line-break)
  "Host fontifiers whose concealment is controlled by appear mode.")

(defun markdown-ts-appear--install-advice ()
  "Install shared fontifier advice."
  (unless markdown-ts-appear--advice-installed-p
    (dolist (function markdown-ts-appear--markdown-fontifiers)
      (advice-add function :around #'markdown-ts-appear--fontify-node))
    (advice-add 'markdown-ts--fontify-image
                :around #'markdown-ts-appear--fontify-image)
    (setq markdown-ts-appear--advice-installed-p t)))

(defun markdown-ts-appear--remove-advice ()
  "Remove shared fontifier advice."
  (when markdown-ts-appear--advice-installed-p
    (dolist (function markdown-ts-appear--markdown-fontifiers)
      (advice-remove function #'markdown-ts-appear--fontify-node))
    (advice-remove 'markdown-ts--fontify-image
                   #'markdown-ts-appear--fontify-image)
    (setq markdown-ts-appear--advice-installed-p nil)))

(defun markdown-ts-appear--register-buffer ()
  "Register the current buffer and install advice if necessary."
  (setq markdown-ts-appear--active-buffers
        (seq-filter #'buffer-live-p markdown-ts-appear--active-buffers))
  (cl-pushnew (current-buffer) markdown-ts-appear--active-buffers)
  (markdown-ts-appear--install-advice))

(defun markdown-ts-appear--unregister-buffer ()
  "Unregister the current buffer and remove unused advice."
  (setq markdown-ts-appear--active-buffers
        (seq-filter
         (lambda (buffer)
           (and (buffer-live-p buffer) (not (eq buffer (current-buffer)))))
         markdown-ts-appear--active-buffers))
  (unless markdown-ts-appear--active-buffers
    (markdown-ts-appear--remove-advice)))

(defun markdown-ts-appear--enable-trigger ()
  "Enable point tracking according to `markdown-ts-appear-trigger'."
  (pcase markdown-ts-appear-trigger
    ('always (markdown-ts-appear--start))
    ('evil-insert
     (add-hook 'evil-insert-state-entry-hook
               #'markdown-ts-appear--start nil t)
     (add-hook 'evil-insert-state-exit-hook
               #'markdown-ts-appear--stop nil t)
     (when (eq (bound-and-true-p evil-state) 'insert)
       (markdown-ts-appear--start)))))

(defun markdown-ts-appear--disable-trigger ()
  "Disable point tracking hooks in the current buffer."
  (remove-hook 'evil-insert-state-entry-hook
               #'markdown-ts-appear--start t)
  (remove-hook 'evil-insert-state-exit-hook
               #'markdown-ts-appear--stop t)
  (markdown-ts-appear--stop))

(defun markdown-ts-appear--detach-indirect-clone ()
  "Detach inherited appear state from a new indirect clone."
  (when-let* ((base (buffer-base-buffer)))
    (setq markdown-ts-appear-mode nil
          markdown-ts-appear--setup-p nil
          markdown-ts-appear--region nil
          markdown-ts-appear--base-owner base
          post-command-hook
          (remove #'markdown-ts-appear--update post-command-hook)
          change-major-mode-hook
          (remove #'markdown-ts-appear--buffer-teardown
                  change-major-mode-hook)
          kill-buffer-hook
          (remove #'markdown-ts-appear--buffer-teardown kill-buffer-hook)
          clone-indirect-buffer-hook
          (remove #'markdown-ts-appear--detach-indirect-clone
                  clone-indirect-buffer-hook)
          evil-insert-state-entry-hook
          (remove #'markdown-ts-appear--start
                  evil-insert-state-entry-hook)
          evil-insert-state-exit-hook
          (remove #'markdown-ts-appear--stop
                  evil-insert-state-exit-hook))
    (setq local-minor-modes
          (remove 'markdown-ts-appear-mode local-minor-modes))))

(defun markdown-ts-appear--buffer-teardown ()
  "Disable appear mode before replacing or killing its buffer."
  (when markdown-ts-appear--setup-p
    (markdown-ts-appear-mode -1)))

(defun markdown-ts-appear--enable ()
  "Enable appear behavior in the current buffer."
  (unless (derived-mode-p 'markdown-ts-mode)
    (user-error "Markdown-ts-appear-mode requires markdown-ts-mode"))
  (when (buffer-base-buffer)
    (user-error "Markdown-ts-appear-mode does not support indirect buffers"))
  (setq markdown-ts-appear--saved-hide-markup markdown-ts-hide-markup
        markdown-ts-appear--saved-hide-markup-local-p
        (local-variable-p 'markdown-ts-hide-markup)
        markdown-ts-appear--saved-invisibility-spec
        (copy-tree buffer-invisibility-spec)
        markdown-ts-appear--setup-p t)
  (unless markdown-ts-hide-markup
    (setq-local markdown-ts-hide-markup t)
    (markdown-ts--set-hide-markup t))
  (add-hook 'change-major-mode-hook
            #'markdown-ts-appear--buffer-teardown nil t)
  (add-hook 'kill-buffer-hook #'markdown-ts-appear--buffer-teardown nil t)
  (add-hook 'clone-indirect-buffer-hook
            #'markdown-ts-appear--detach-indirect-clone nil t)
  (markdown-ts-appear--register-buffer)
  (markdown-ts-appear--enable-trigger))

(defun markdown-ts-appear--disable ()
  "Disable appear behavior and restore the previous buffer state."
  (when markdown-ts-appear--setup-p
    (markdown-ts-appear--disable-trigger)
    (remove-hook 'change-major-mode-hook
                 #'markdown-ts-appear--buffer-teardown t)
    (remove-hook 'kill-buffer-hook #'markdown-ts-appear--buffer-teardown t)
    (remove-hook 'clone-indirect-buffer-hook
                 #'markdown-ts-appear--detach-indirect-clone t)
    (if markdown-ts-appear--saved-hide-markup-local-p
        (setq-local markdown-ts-hide-markup
                    markdown-ts-appear--saved-hide-markup)
      (kill-local-variable 'markdown-ts-hide-markup))
    (setq buffer-invisibility-spec
          markdown-ts-appear--saved-invisibility-spec)
    (setq markdown-ts-appear--setup-p nil
          markdown-ts-appear--saved-hide-markup nil
          markdown-ts-appear--saved-hide-markup-local-p nil
          markdown-ts-appear--saved-invisibility-spec nil)
    (font-lock-flush)
    (font-lock-ensure)
    (markdown-ts-appear--unregister-buffer)))

;;;###autoload
(define-minor-mode markdown-ts-appear-mode
  "Reveal the smallest rendered Markdown element at point."
  :lighter nil
  :group 'markdown-ts-appear
  (if markdown-ts-appear-mode
      (unless markdown-ts-appear--setup-p
        (condition-case error-data
            (markdown-ts-appear--enable)
          (error
           (setq markdown-ts-appear-mode nil)
           (when markdown-ts-appear--setup-p
             (ignore-errors (markdown-ts-appear--disable)))
           (signal (car error-data) (cdr error-data)))))
    (markdown-ts-appear--disable)))

(defun markdown-ts-appear-unload-function ()
  "Disable active buffers and remove shared advice before unloading."
  (dolist (buffer (copy-sequence markdown-ts-appear--active-buffers))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (markdown-ts-appear-mode -1))))
  (setq markdown-ts-appear--active-buffers nil)
  (markdown-ts-appear--remove-advice)
  nil)

(provide 'markdown-ts-appear)

;;; markdown-ts-appear.el ends here
