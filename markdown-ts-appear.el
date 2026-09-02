;;; markdown-ts-appear.el --- Reveal Markdown source and preview math -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael

;; Author: Thysrael
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1") (mathjax "0.1"))
;; Keywords: markdown, text, convenience
;; URL: https://github.com/Thysrael/markdown-ts-appear

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; `markdown-ts-appear-mode' hides rendered Markdown markup and reveals the
;; smallest semantic element at point.  It can also render LaTeX fragments
;; asynchronously with MathJax while they are not being edited.

;;; Code:

(require 'markdown-ts-mode)
(require 'seq)
(require 'svg)

(declare-function mathjax-available-p "mathjax")
(declare-function mathjax-render "mathjax")

(defvar evil-insert-state-entry-hook)
(defvar evil-insert-state-exit-hook)
(defvar evil-state)

(defgroup markdown-ts-appear nil
  "Reveal rendered Markdown source at point."
  :group 'markdown-ts)

(defcustom markdown-ts-appear-trigger 'always
  "When `markdown-ts-appear-mode' should reveal source.
With `always', track point whenever the mode is enabled.  With
`evil-insert', track point only while Evil is in insert state; Evil remains
an optional dependency."
  :type '(choice (const :tag "Whenever the mode is enabled" always)
                 (const :tag "Only in Evil insert state" evil-insert))
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-enable-math-preview t
  "Whether `markdown-ts-appear-mode' should preview LaTeX with MathJax."
  :type 'boolean
  :group 'markdown-ts-appear)

(defcustom markdown-ts-appear-math-timeout 10
  "Seconds to wait for a MathJax rendering result."
  :type 'number
  :group 'markdown-ts-appear)

(defvar-local markdown-ts-appear-mode nil
  "Non-nil when Markdown TS Appear mode is enabled.")

(defvar-local markdown-ts-appear-math-preview-mode nil
  "Non-nil when Markdown TS Appear math previews are enabled.")

(defvar-local markdown-ts-appear--region nil
  "Markers delimiting the semantic Markdown source currently visible.")

(defvar-local markdown-ts-appear--last-point nil
  "Buffer position checked by the most recent appear update.")

(defvar-local markdown-ts-appear--last-tick nil
  "Buffer modification tick checked by the most recent appear update.")

(defvar-local markdown-ts-appear--previous-hide-markup nil
  "Value of `markdown-ts-hide-markup' before appear mode was enabled.")

(defvar-local markdown-ts-appear--managed-line-height-p nil
  "Non-nil when appear mode added `line-height' to managed properties.")

(defvar-local markdown-ts-appear--math-filter-installed-p nil
  "Non-nil when the math preview copy filter is installed.")

(defvar markdown-ts-appear--math-cache (make-hash-table :test #'equal)
  "MathJax results keyed by formula text and display style.")

(defvar markdown-ts-appear--math-pending (make-hash-table :test #'equal)
  "Pending MathJax requests keyed by formula text and display style.")

(defconst markdown-ts-appear--math-cache-miss
  (make-symbol "markdown-ts-appear-math-cache-miss")
  "Sentinel used for missing MathJax cache entries.")

(defun markdown-ts-appear--region-visible-p (beg end)
  "Return non-nil when BEG through END overlaps visible Markdown source."
  (when-let* ((region markdown-ts-appear--region)
              (visible-beg (marker-position (car region)))
              (visible-end (marker-position (cdr region))))
    (and (< beg visible-end) (> end visible-beg))))

(defun markdown-ts-appear--node-visible-p (node)
  "Return non-nil when NODE overlaps visible semantic Markdown source."
  (markdown-ts-appear--region-visible-p
   (treesit-node-start node) (treesit-node-end node)))

(defun markdown-ts-appear--restore ()
  "Restore hidden markup in the previously revealed region."
  (when-let* ((region markdown-ts-appear--region)
              (beg (marker-position (car region)))
              (end (marker-position (cdr region))))
    (set-marker (car region) nil)
    (set-marker (cdr region) nil)
    (setq markdown-ts-appear--region nil)
    (save-restriction
      (widen)
      (font-lock-flush beg end)
      (font-lock-ensure beg end))))

(defun markdown-ts-appear--bounds ()
  "Return source bounds for the smallest rendered element at point."
  (font-lock-ensure (line-beginning-position)
                    (min (point-max) (line-beginning-position 2)))
  (let* ((pos (point))
         (line-beg (line-beginning-position))
         (line-end (line-end-position))
         (contains-p
          (lambda (node)
            (let ((beg (treesit-node-start node))
                  (end (treesit-node-end node)))
              (and (<= beg pos)
                   (or (< pos end)
                       (and (= pos end) (> pos beg)
                            (not (eq (char-before pos) ?\n))))))))
         inline-node)
    (let ((node (treesit-node-at pos 'markdown-inline)))
      (while (and node (not inline-node))
        (let ((type (treesit-node-type node)))
          (when (and
                 (funcall contains-p node)
                 (member type
                         '("emphasis" "strong_emphasis" "strikethrough"
                           "code_span" "inline_link" "full_reference_link"
                           "collapsed_reference_link" "shortcut_link"
                           "image" "uri_autolink" "email_autolink"
                           "entity_reference" "numeric_character_reference"
                           "backslash_escape" "hard_line_break"
                           "latex_block"))
                 (or (not (equal type "latex_block"))
                     (markdown-ts--latex-block-valid-p node)))
            (setq inline-node node)))
        (setq node (and node (treesit-node-parent node)))))
    ;; The inline grammar represents `~~text~~' as nested strikethroughs.
    (when (and inline-node
               (equal (treesit-node-type inline-node) "strikethrough"))
      (let ((parent (treesit-node-parent inline-node)))
        (while (and parent
                    (equal (treesit-node-type parent) "strikethrough"))
          (setq inline-node parent
                parent (treesit-node-parent parent)))))
    (if inline-node
        (let ((beg (treesit-node-start inline-node))
              (end (treesit-node-end inline-node)))
          ;; Wiki links are parsed as a shortcut link inside extra brackets.
          (when (and (equal (treesit-node-type inline-node) "shortcut_link")
                     (> beg (point-min)) (< end (point-max))
                     (eq (char-before beg) ?\[)
                     (eq (char-after end) ?\]))
            (setq beg (1- beg)
                  end (1+ end)))
          (cons beg end))
      (let (structural-node)
        (let ((node (treesit-node-at pos 'markdown)))
          (while (and node (not structural-node))
            (when (and
                   (funcall contains-p node)
                   (member (treesit-node-type node)
                           '("atx_heading" "setext_heading" "list_item"
                             "task_list_marker_unchecked"
                             "task_list_marker_checked"
                             "pipe_table_header" "pipe_table_row"
                             "pipe_table_delimiter_row" "thematic_break"
                             "link_reference_definition")))
              (setq structural-node node))
            (setq node (and node (treesit-node-parent node)))))
        (or
         (pcase (and structural-node (treesit-node-type structural-node))
           ("atx_heading"
            (when-let* ((marker (treesit-node-child structural-node 0 'named)))
              (cons (treesit-node-start marker)
                    (save-excursion
                      (goto-char (treesit-node-end marker))
                      (skip-chars-forward " \t" line-end)
                      (point)))))
           ("setext_heading"
            (when-let* ((underline
                         (treesit-search-subtree
                          structural-node "\\`setext_h[12]_underline\\'")))
              (cons (treesit-node-start underline)
                    (treesit-node-end underline))))
           ("list_item"
            (when-let* ((marker (treesit-node-child structural-node 0 'named))
                        ((string-prefix-p
                          "list_marker_" (treesit-node-type marker)))
                        ((= line-beg
                            (save-excursion
                              (goto-char (treesit-node-start marker))
                              (line-beginning-position)))))
              (cons (treesit-node-start marker)
                    (treesit-node-end marker))))
           ((or "task_list_marker_unchecked" "task_list_marker_checked"
                "pipe_table_header" "pipe_table_row"
                "pipe_table_delimiter_row" "thematic_break"
                "link_reference_definition")
            (cons (treesit-node-start structural-node)
                  (treesit-node-end structural-node))))
         ;; Keep malformed markup stable while its closing syntax is typed.
         (save-excursion
           (goto-char pos)
           (skip-chars-backward "^ \t\n" line-beg)
           (let ((beg (point)))
             (goto-char pos)
             (skip-chars-forward "^ \t\n" line-end)
             (let ((end (point)))
               (when (and (< beg end)
                          (or (memq (char-after beg)
                                    '(?* ?_ ?~ ?` ?$ ?< ?\\ ?\[ ?!))
                              (string-match-p
                               "\\[" (buffer-substring-no-properties
                                      beg end))))
                 (cons beg end))))))))))

(defun markdown-ts-appear--update ()
  "Reveal source for the rendered Markdown element at point."
  (let ((position (point))
        (tick (buffer-chars-modified-tick)))
    (unless (and (equal position markdown-ts-appear--last-point)
                 (equal tick markdown-ts-appear--last-tick))
      (save-restriction
        (widen)
        (let* ((bounds (unless (markdown-ts-at-code-block-p)
                         (markdown-ts-appear--bounds)))
               (beg (car-safe bounds))
               (end (cdr-safe bounds))
               (region markdown-ts-appear--region)
               (old-beg (and region (marker-position (car region))))
               (old-end (and region (marker-position (cdr region)))))
          (unless (if bounds
                      (and old-beg old-end (= beg old-beg) (= end old-end))
                    (null region))
            (markdown-ts-appear--restore)
            (when bounds
              (setq markdown-ts-appear--region
                    (cons (copy-marker beg) (copy-marker end t)))
              (font-lock-flush beg end)
              (font-lock-ensure beg end)))))
      (setq markdown-ts-appear--last-point position)
      (setq markdown-ts-appear--last-tick tick))))

(defun markdown-ts-appear--start ()
  "Start tracking the semantic Markdown element at point."
  (setq markdown-ts-appear--last-point nil)
  (setq markdown-ts-appear--last-tick nil)
  (add-hook 'post-command-hook #'markdown-ts-appear--update nil t)
  (markdown-ts-appear--update))

(defun markdown-ts-appear--stop ()
  "Stop tracking point and restore hidden Markdown markup."
  (remove-hook 'post-command-hook #'markdown-ts-appear--update t)
  (setq markdown-ts-appear--last-point nil)
  (setq markdown-ts-appear--last-tick nil)
  (markdown-ts-appear--restore))

(defun markdown-ts-appear--math-node-data (node)
  "Return (BEG END MATH DISPLAY-P) for a LaTeX block NODE."
  (let (opening closing)
    (dotimes (index (treesit-node-child-count node))
      (let ((child (treesit-node-child node index)))
        (when (equal (treesit-node-type child) "latex_span_delimiter")
          (unless opening
            (setq opening child))
          (setq closing child))))
    (when (and opening closing
               (< (treesit-node-start opening) (treesit-node-start closing)))
      (let ((opener (treesit-node-text opening t)))
        (list (treesit-node-start node)
              (treesit-node-end node)
              (buffer-substring-no-properties
               (treesit-node-end opening) (treesit-node-start closing))
              (and (member opener '("$$" "\\[")) t))))))

(defun markdown-ts-appear--math-node-at (position)
  "Return the valid Markdown LaTeX block at POSITION, if any."
  (when-let* ((node (treesit-node-at position 'markdown-inline))
              (math-node (treesit-parent-until node "\\`latex_block\\'" t))
              ((<= (treesit-node-start math-node) position))
              ((< position (treesit-node-end math-node)))
              ((markdown-ts--latex-block-valid-p math-node)))
    math-node))

(defun markdown-ts-appear--math-clear (beg end)
  "Remove a rendered formula between BEG and END."
  (with-silent-modifications
    (remove-text-properties
     beg end '(display nil markdown-ts-appear--math-state nil))))

(defun markdown-ts-appear--math-state (beg end)
  "Return the uniform math preview state between BEG and END."
  (let ((state (get-text-property beg 'markdown-ts-appear--math-state)))
    (when (and state
               (= (next-single-property-change
                   beg 'markdown-ts-appear--math-state nil end)
                  end))
      state)))

(defun markdown-ts-appear--math-image (svg)
  "Create an image from MathJax SVG with a suitable baseline."
  (let* ((height
          (and (string-match "height=\"\\([-.0-9]+\\)" svg)
               (string-to-number (match-string 1 svg))))
         (vertical-align
          (and (string-match "vertical-align: \\([-.0-9]+\\)" svg)
               (string-to-number (match-string 1 svg))))
         (ascent (if (and height vertical-align (> height 0))
                     (round (* 100 (/ (+ height vertical-align) height)))
                   100)))
    (svg-image svg :ascent (max 0 (min 100 ascent)))))

(defun markdown-ts-appear--math-display-result (request data)
  "Display MathJax DATA for a still-valid rendering REQUEST."
  (pcase-let ((`(,buffer ,beg-marker ,end-marker ,source ,key) request))
    (unwind-protect
        (when (and (buffer-live-p buffer)
                   (marker-position beg-marker)
                   (marker-position end-marker))
          (with-current-buffer buffer
            (save-restriction
              (widen)
              (let ((beg (marker-position beg-marker))
                    (end (marker-position end-marker)))
                (when (and markdown-ts-appear-math-preview-mode
                           (< beg end)
                           (equal source
                                  (buffer-substring-no-properties beg end))
                           (not (markdown-ts-appear--region-visible-p beg end)))
                  (when-let* ((node (markdown-ts-appear--math-node-at beg))
                              (node-data
                               (markdown-ts-appear--math-node-data node)))
                    (pcase-let ((`(,node-beg ,node-end ,math ,display-p)
                                 node-data))
                      (when (and (= beg node-beg) (= end node-end)
                                 (equal key (cons display-p math)))
                        (markdown-ts-appear--math-clear beg end)
                        (with-silent-modifications
                          (if-let* ((svg (alist-get 'svg data)))
                              (progn
                                (put-text-property
                                 beg end 'display
                                 (or (alist-get
                                      'markdown-ts-appear--math-image data)
                                     (markdown-ts-appear--math-image svg)))
                                (put-text-property
                                 beg end 'markdown-ts-appear--math-state
                                 (list 'rendered key)))
                            (unless (alist-get 'transient data)
                              (put-text-property
                               beg end 'markdown-ts-appear--math-state
                               (list 'error key)))))))))))))
      (set-marker beg-marker nil)
      (set-marker end-marker nil))))

(defun markdown-ts-appear--math-finish-render (key data)
  "Cache MathJax DATA for KEY and complete its waiting requests."
  (when-let* ((svg (alist-get 'svg data)))
    (push (cons 'markdown-ts-appear--math-image
                (markdown-ts-appear--math-image svg))
          data))
  (unless (alist-get 'transient data)
    (when (>= (hash-table-count markdown-ts-appear--math-cache) 512)
      (clrhash markdown-ts-appear--math-cache))
    (puthash key data markdown-ts-appear--math-cache))
  (when-let* ((pending (gethash key markdown-ts-appear--math-pending)))
    (remhash key markdown-ts-appear--math-pending)
    (cancel-timer (car pending))
    (maphash
     (lambda (_ requests)
       (dolist (request requests)
         (condition-case error
             (markdown-ts-appear--math-display-result request data)
           (error
            (message "Markdown math preview failed: %s"
                     (error-message-string error))))))
     (cdr pending))))

(defun markdown-ts-appear--math-render-timeout (key)
  "Release requests waiting too long for MathJax KEY."
  (markdown-ts-appear--math-finish-render
   key '((error . "MathJax rendering timed out") (transient . t))))

(defun markdown-ts-appear--math-request (beg end math display-p)
  "Render MATH asynchronously for the region from BEG to END."
  (let* ((key (cons display-p math))
         (state (markdown-ts-appear--math-state beg end))
         (cached (gethash key markdown-ts-appear--math-cache
                          markdown-ts-appear--math-cache-miss)))
    (unless (and (consp state) (equal (cadr state) key))
      (markdown-ts-appear--math-clear beg end)
      (with-silent-modifications
        (put-text-property beg end 'markdown-ts-appear--math-state
                           (list 'pending key)))
      (let* ((source (buffer-substring-no-properties beg end))
             (request-key (list (current-buffer) beg end source))
             (request (list (current-buffer)
                            (copy-marker beg t)
                            (copy-marker end)
                            source key)))
        (if (not (eq cached markdown-ts-appear--math-cache-miss))
            (markdown-ts-appear--math-display-result request cached)
          (let ((pending (gethash key markdown-ts-appear--math-pending)))
            (if pending
                (let* ((requests (cdr pending))
                       (bucket (gethash request-key requests)))
                  (unless (seq-some
                           (lambda (waiting)
                             (and (equal (marker-position (cadr waiting)) beg)
                                  (equal (marker-position (caddr waiting)) end)
                                  (equal (nth 3 waiting) source)))
                           bucket)
                    (puthash request-key (cons request bucket) requests)))
              (let ((requests (make-hash-table :test #'equal)))
                (puthash request-key (list request) requests)
                (setq pending
                      (cons
                       (run-at-time
                        markdown-ts-appear-math-timeout nil
                        #'markdown-ts-appear--math-render-timeout key)
                       requests)))
              (puthash key pending markdown-ts-appear--math-pending)
              (condition-case error
                  (mathjax-render
                   (lambda (data)
                     (markdown-ts-appear--math-finish-render key data))
                   math :options (list :display display-p))
                (error
                 (markdown-ts-appear--math-finish-render
                  key `((error . ,(error-message-string error))
                        (transient . t))))))))))))

(defun markdown-ts-appear--math-preview-node (node)
  "Render the valid Markdown LaTeX block NODE when it is not being edited."
  (when (and markdown-ts-appear-math-preview-mode
             (markdown-ts--latex-block-valid-p node))
    (when-let* ((data (markdown-ts-appear--math-node-data node)))
      (pcase-let ((`(,beg ,end ,math ,display-p) data))
        (if (markdown-ts-appear--region-visible-p beg end)
            (markdown-ts-appear--math-clear beg end)
          (unless (markdown-ts--outline-invisible-p beg)
            (markdown-ts-appear--math-request beg end math display-p)))))))

(defun markdown-ts-appear--math-preview-window (window)
  "Enable math preview when WINDOW shows this buffer graphically."
  (when (and (window-live-p window)
             (eq (window-buffer window) (current-buffer))
             (display-graphic-p (window-frame window))
             (not markdown-ts-appear-math-preview-mode))
    (markdown-ts-appear-math-preview-mode 1)))

(defun markdown-ts-appear--math-graphic-window ()
  "Return a graphical window displaying the current buffer."
  (seq-find
   (lambda (window)
     (display-graphic-p (window-frame window)))
   (get-buffer-window-list nil nil t)))

(defun markdown-ts-appear--math-filter-copied-text (text)
  "Remove math preview properties from copied Markdown TEXT."
  (remove-text-properties
   0 (length text) '(display nil markdown-ts-appear--math-state nil) text)
  text)

(defun markdown-ts-appear--math-setup ()
  "Enable math preview now or when this buffer reaches a graphical frame."
  (unless markdown-ts-appear--math-filter-installed-p
    (add-function :filter-return (local 'filter-buffer-substring-function)
                  #'markdown-ts-appear--math-filter-copied-text)
    (setq markdown-ts-appear--math-filter-installed-p t))
  (add-hook 'window-buffer-change-functions
            #'markdown-ts-appear--math-preview-window nil t)
  (when-let* ((window (markdown-ts-appear--math-graphic-window)))
    (markdown-ts-appear--math-preview-window window)))

(defun markdown-ts-appear--math-teardown ()
  "Disable math preview and remove its buffer-local integration."
  (remove-hook 'window-buffer-change-functions
               #'markdown-ts-appear--math-preview-window t)
  (when markdown-ts-appear--math-filter-installed-p
    (remove-function (local 'filter-buffer-substring-function)
                     #'markdown-ts-appear--math-filter-copied-text)
    (setq markdown-ts-appear--math-filter-installed-p nil))
  (when markdown-ts-appear-math-preview-mode
    (markdown-ts-appear-math-preview-mode -1)))

(define-minor-mode markdown-ts-appear-math-preview-mode
  "Render Markdown LaTeX fragments asynchronously with MathJax."
  :lighter nil
  (if markdown-ts-appear-math-preview-mode
      (if (and (or (display-graphic-p)
                   (markdown-ts-appear--math-graphic-window))
               (image-type-available-p 'svg)
               (require 'mathjax nil t)
               (mathjax-available-p))
          (progn
            (remove-hook 'window-buffer-change-functions
                         #'markdown-ts-appear--math-preview-window t)
            (add-to-list 'font-lock-extra-managed-props
                         'markdown-ts-appear--math-state)
            (font-lock-flush))
        (setq markdown-ts-appear-math-preview-mode nil))
    (save-restriction
      (widen)
      (let ((pos (point-min)))
        (while (< pos (point-max))
          (let ((next (next-single-property-change
                       pos 'markdown-ts-appear--math-state nil (point-max))))
            (when (get-text-property pos 'markdown-ts-appear--math-state)
              (markdown-ts-appear--math-clear pos next))
            (setq pos next))))
      (setq font-lock-extra-managed-props
            (delq 'markdown-ts-appear--math-state
                  font-lock-extra-managed-props))
      (font-lock-flush))))

(defun markdown-ts-appear--fontify-node (function node &rest arguments)
  "Call FUNCTION with NODE and ARGUMENTS without covering visible source."
  (let ((markdown-ts-hide-markup
         (and markdown-ts-hide-markup
              (not (and markdown-ts-appear-mode
                        (markdown-ts-appear--node-visible-p node))))))
    (apply function node arguments)))

(defun markdown-ts-appear--fontify-visible-markup
    (function node &rest arguments)
  "Call FUNCTION with NODE and ARGUMENTS without covering visible markup."
  (let* ((type (treesit-node-type node))
         (markup-node
          (pcase type
            ("atx_heading" (treesit-node-child node 0 'named))
            ("setext_heading"
             (treesit-search-subtree node "\\`setext_h[12]_underline\\'"))
            (_ node)))
         (markdown-ts-hide-markup
          (and markdown-ts-hide-markup
               (not (and markdown-ts-appear-mode markup-node
                         (markdown-ts-appear--node-visible-p markup-node))))))
    (apply function node arguments)))

(defun markdown-ts-appear--fontify-image (function node &rest arguments)
  "Call FUNCTION with NODE and ARGUMENTS without covering image source."
  (let ((markdown-ts-inline-images
         (and markdown-ts-inline-images
              (not (and markdown-ts-appear-mode
                        (markdown-ts-appear--node-visible-p node))))))
    (apply function node arguments)))

(defun markdown-ts-appear--fontify-link (function node &rest arguments)
  "Call FUNCTION with NODE and ARGUMENTS, hiding unedited Wiki link source."
  (apply function node arguments)
  (when markdown-ts-appear-mode
    (let* ((parent (treesit-node-parent node))
           (parent-beg (treesit-node-start parent))
           (parent-end (treesit-node-end parent))
           (wikilink-p
            (and (equal (treesit-node-type parent) "shortcut_link")
                 (> parent-beg (point-min))
                 (< parent-end (point-max))
                 (eq (char-before parent-beg) ?\[)
                 (eq (char-after parent-end) ?\])))
           (beg (treesit-node-start node))
           (end (treesit-node-end node))
           (alias-beg
            (and wikilink-p
                 (save-excursion
                   (goto-char beg)
                   (search-forward "|" end t))))
           (visible-p
            (and wikilink-p
                 (markdown-ts-appear--region-visible-p
                  (1- parent-beg) (1+ parent-end)))))
      (when (and markdown-ts-hide-markup wikilink-p (not visible-p))
        (with-silent-modifications
          (put-text-property (1- parent-beg) parent-beg
                             'invisible 'markdown-ts--markup)
          (put-text-property parent-end (1+ parent-end)
                             'invisible 'markdown-ts--markup)
          (when alias-beg
            (put-text-property beg alias-beg
                               'invisible 'markdown-ts--markup)))))))

(defun markdown-ts-appear--fontify-math (function node &rest arguments)
  "Call FUNCTION with NODE and ARGUMENTS, then render the LaTeX block."
  (let ((markdown-ts-hide-markup
         (and markdown-ts-hide-markup
              (not (and markdown-ts-appear-mode
                        (markdown-ts-appear--node-visible-p node))))))
    (apply function node arguments))
  (markdown-ts-appear--math-preview-node node))

(defun markdown-ts-appear--enable-trigger ()
  "Enable point tracking according to `markdown-ts-appear-trigger'."
  (pcase markdown-ts-appear-trigger
    ('always
     (markdown-ts-appear--start))
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

;;;###autoload
(define-minor-mode markdown-ts-appear-mode
  "Reveal rendered Markdown source at point and preview unedited math."
  :lighter nil
  (if markdown-ts-appear-mode
      (progn
        (unless (derived-mode-p 'markdown-ts-mode)
          (setq markdown-ts-appear-mode nil)
          (user-error "Markdown TS Appear mode requires markdown-ts-mode"))
        (setq markdown-ts-appear--previous-hide-markup
              markdown-ts-hide-markup)
        (unless (memq 'line-height font-lock-extra-managed-props)
          (setq markdown-ts-appear--managed-line-height-p t)
          (add-to-list 'font-lock-extra-managed-props 'line-height))
        (unless markdown-ts-hide-markup
          (markdown-ts-toggle-hide-markup))
        (markdown-ts-appear--enable-trigger)
        (when markdown-ts-appear-enable-math-preview
          (markdown-ts-appear--math-setup)))
    (markdown-ts-appear--disable-trigger)
    (markdown-ts-appear--math-teardown)
    (unless markdown-ts-appear--previous-hide-markup
      (when markdown-ts-hide-markup
        (markdown-ts-toggle-hide-markup)))
    (font-lock-flush)
    (when markdown-ts-appear--managed-line-height-p
      (setq font-lock-extra-managed-props
            (delq 'line-height font-lock-extra-managed-props))
      (setq markdown-ts-appear--managed-line-height-p nil))))

(defconst markdown-ts-appear--visible-fontifiers
  '(markdown-ts--fontify-atx-heading
    markdown-ts--fontify-setext-heading
    markdown-ts--fontify-link-destination
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

(defun markdown-ts-appear--add-advice (symbol function)
  "Add FUNCTION around SYMBOL unless it is already present."
  (unless (advice-member-p function symbol)
    (advice-add symbol :around function)))

(defun markdown-ts-appear--install-advice ()
  "Install Markdown fontification advice."
  (markdown-ts-appear--add-advice
   'markdown-ts--fontify-delimiter #'markdown-ts-appear--fontify-node)
  (markdown-ts-appear--add-advice
   'markdown-ts--fontify-atx-delimiter #'markdown-ts-appear--fontify-node)
  (dolist (function markdown-ts-appear--visible-fontifiers)
    (markdown-ts-appear--add-advice
     function #'markdown-ts-appear--fontify-visible-markup))
  (markdown-ts-appear--add-advice
   'markdown-ts--fontify-link-node #'markdown-ts-appear--fontify-link)
  (markdown-ts-appear--add-advice
   'markdown-ts--fontify-image #'markdown-ts-appear--fontify-image)
  (markdown-ts-appear--add-advice
   'markdown-ts--fontify-latex-block #'markdown-ts-appear--fontify-math))

(defun markdown-ts-appear--remove-advice ()
  "Remove Markdown fontification advice."
  (advice-remove 'markdown-ts--fontify-delimiter
                 #'markdown-ts-appear--fontify-node)
  (advice-remove 'markdown-ts--fontify-atx-delimiter
                 #'markdown-ts-appear--fontify-node)
  (dolist (function markdown-ts-appear--visible-fontifiers)
    (advice-remove function #'markdown-ts-appear--fontify-visible-markup))
  (advice-remove 'markdown-ts--fontify-link-node
                 #'markdown-ts-appear--fontify-link)
  (advice-remove 'markdown-ts--fontify-image
                 #'markdown-ts-appear--fontify-image)
  (advice-remove 'markdown-ts--fontify-latex-block
                 #'markdown-ts-appear--fontify-math))

(defun markdown-ts-appear-unload-function ()
  "Remove global integration before unloading Markdown TS Appear."
  (markdown-ts-appear--remove-advice)
  nil)

(markdown-ts-appear--install-advice)

(provide 'markdown-ts-appear)

;;; markdown-ts-appear.el ends here
