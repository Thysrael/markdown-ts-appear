;;; markdown-ts-appear-mermaid.el --- Asynchronous Mermaid previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael

;; Author: Thysrael <thysrael@163.com>
;; Assisted-by: OpenCode:gpt-6-astra
;; Maintainer: Thysrael <thysrael@163.com>
;; Keywords: text, convenience
;; URL: https://github.com/Thysrael/markdown-ts-appear

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

;; Preview fenced Mermaid blocks through the optional mmdc executable.
;; A shared queue runs one renderer at a time and coalesces identical jobs.
;; Source overlays retain images while their source is revealed for editing.

;;; Code:

(require 'markdown-ts-mode)
(require 'outline)
(require 'image)
(require 'seq)
(require 'subr-x)

(declare-function markdown-ts-appear--active-p "markdown-ts-appear")
(declare-function markdown-ts-appear--tracking-p "markdown-ts-appear")
(declare-function markdown-ts-appear--node-ancestor "markdown-ts-appear")
(declare-function markdown-ts-appear--first-direct-child-of-type "markdown-ts-appear")
(declare-function markdown-ts-appear--direct-children-of-type "markdown-ts-appear")
(declare-function markdown-ts-appear--add-outline-hook "markdown-ts-appear")
(declare-function markdown-ts-appear--remove-outline-hook "markdown-ts-appear")

(defvar markdown-ts-appear-enable-mermaid-preview)
(defvar markdown-ts-appear-mermaid-command)
(defvar markdown-ts-appear-mermaid-size)
(defvar markdown-ts-appear-code-fence-style)

(defvar markdown-ts-appear-mermaid--cache (make-hash-table :test #'equal :weakness 'value)
  "Images keyed by (COMMAND THEME BODY), shared while previews retain them.")
(defvar markdown-ts-appear-mermaid--queue nil
  "Pending jobs, each a (KEY . SOURCE-OVERLAYS) pair.")
(defvar markdown-ts-appear-mermaid--job nil
  "Job currently being rendered.")
(defvar markdown-ts-appear-mermaid--process nil
  "Current asynchronous mmdc process.")
(defvar markdown-ts-appear-mermaid--query nil
  "Compiled fenced-block query.")
(defvar markdown-ts-appear-mermaid--refreshing nil
  "Non-nil while measuring or updating Mermaid views.")
(defvar-local markdown-ts-appear-mermaid--objects nil
  "Source overlays holding diagram keys, images and window-specific views.")
(defvar-local markdown-ts-appear-mermaid--scan-state nil
  "Last command and theme scanned, or nil when sources need rescanning.")
(defvar-local markdown-ts-appear-mermaid--view-state nil
  "Last source, point and window state displayed.")

(defun markdown-ts-appear-mermaid--delete (source)
  "Delete SOURCE and its views."
  (mapc #'delete-overlay (overlay-get source 'markdown-ts-appear-mermaid--views))
  (delete-overlay source))

(defun markdown-ts-appear-mermaid--body (node)
  "Return closed block NODE's code without Markdown container prefixes."
  (when-let* ((body (markdown-ts-appear--first-direct-child-of-type node "code_fence_content")))
    (let ((position (treesit-node-start body))
          (delimiters (markdown-ts-appear--direct-children-of-type node "fenced_code_block_delimiter"))
          parts)
      (dolist (child (markdown-ts-appear--direct-children-of-type body "block_continuation"))
        (push (buffer-substring-no-properties position (treesit-node-start child)) parts)
        (setq position (treesit-node-end child)))
      (push (buffer-substring-no-properties position (treesit-node-end body)) parts)
      (let ((code (apply #'concat (nreverse parts))))
        (if (length= delimiters 2) code
          ;; At EOF the grammar can include the closing fence in the content.
          (when (= (treesit-node-end node) (point-max))
            (save-match-data
              (let ((opening (treesit-node-text (car delimiters) t)))
                (when (string-match
                       (format "\n[ \t]\\{0,3\\}%s\\{%d,\\}[ \t]*\\'"
                               (regexp-quote (substring opening 0 1)) (length opening)) code)
                  (substring code 0 (1+ (match-beginning 0))))))))))))

(defun markdown-ts-appear-mermaid--end (node)
  "Return NODE's end, preserving its final newline outside the preview."
  (let ((end (treesit-node-end node)))
    (if (eq (char-before end) ?\n) (1- end) end)))

(defun markdown-ts-appear-mermaid--scan (theme)
  "Reconcile Mermaid sources with the current syntax and THEME."
  (with-memoization markdown-ts-appear-mermaid--query
    (treesit-query-compile 'markdown '((fenced_code_block) @block)))
  (let ((existing (make-hash-table :test #'eql)) current)
    (dolist (source markdown-ts-appear-mermaid--objects)
      (if (overlay-buffer source)
          (puthash (overlay-start source) source existing)
        (markdown-ts-appear-mermaid--delete source)))
    (dolist (node (treesit-query-capture (treesit-buffer-root-node 'markdown)
                                        markdown-ts-appear-mermaid--query nil nil t))
      (when-let* ((language (treesit-search-subtree node "\\`language\\'"))
                  ((string= (downcase (treesit-node-text language t)) "mermaid"))
                  (body (markdown-ts-appear-mermaid--body node)))
        (let* ((beg (treesit-node-start node))
               (end (markdown-ts-appear-mermaid--end node))
               (source (or (gethash beg existing) (make-overlay beg end nil t nil)))
               (text (buffer-substring-no-properties beg end))
               (key (list markdown-ts-appear-mermaid-command theme body)))
          (remhash beg existing)
          (unless (= (overlay-end source) end) (move-overlay source beg end))
          (unless (equal text (overlay-get source 'markdown-ts-appear-mermaid--source))
            (overlay-put source 'markdown-ts-appear-mermaid--source text))
          (unless (equal key (overlay-get source 'markdown-ts-appear-mermaid--key))
            (overlay-put source 'markdown-ts-appear-mermaid--key key)
            (overlay-put source 'markdown-ts-appear-mermaid--image
                         (gethash key markdown-ts-appear-mermaid--cache))
            (overlay-put source 'markdown-ts-appear-mermaid--error nil))
          (push source current))))
    (maphash (lambda (_ source) (markdown-ts-appear-mermaid--delete source)) existing)
    (setq markdown-ts-appear-mermaid--objects (nreverse current))))

(defun markdown-ts-appear-mermaid--windows (&optional update)
  "Return (WINDOW WIDTH LAYOUT START END) for windows showing this buffer.
LAYOUT describes source font metrics and wrapping, or nil for width-only sizing.
UPDATE is t to recompute all window ends, or a window to update only its end."
  (let* ((source-height (eq markdown-ts-appear-mermaid-size 'source))
         (max-width (if source-height markdown-ts-image-max-width
                      markdown-ts-appear-mermaid-size)))
    (mapcar (lambda (window)
              (let ((width (window-body-width window t)))
                (list window
                      (if (eq max-width 'window) width max-width)
                      (when source-height
                        (list width (window-default-line-height window)
                              (window-font-height window 'markdown-ts-code-block)
                              (window-font-width window 'markdown-ts-code-block)
                              tab-width truncate-lines truncate-partial-width-windows word-wrap
                              (window-hscroll window)
                              (and display-line-numbers
                                   (with-selected-window window (line-number-display-width t)))
                              markdown-ts-appear-code-fence-style))
                      (window-start window)
                      (window-end window (or (eq update t) (eq update window))))))
            (get-buffer-window-list (current-buffer) nil t))))

(defun markdown-ts-appear-mermaid--editing-p (source)
  "Return non-nil when SOURCE is being edited with source tracking active."
  (and (markdown-ts-appear--tracking-p)
       (<= (overlay-start source) (point)) (<= (point) (overlay-end source))))

(defun markdown-ts-appear-mermaid--editing-source ()
  "Return the source being edited, without visiting other diagrams."
  (when (markdown-ts-appear--tracking-p)
    (seq-find (lambda (source)
                (and (overlay-get source 'markdown-ts-appear-mermaid--source)
                     (markdown-ts-appear-mermaid--editing-p source)))
              (append (overlays-at (point))
                      (overlays-at (max (point-min) (1- (point))))))))

(defun markdown-ts-appear-mermaid--eligible-p (source windows)
  "Return non-nil when SOURCE needs a preview visible in WINDOWS."
  (and (not (markdown-ts-appear-mermaid--editing-p source))
       (not (outline-invisible-p (overlay-start source)))
       (seq-some (lambda (entry)
                   (and (display-images-p (window-frame (car entry)))
                        (nth 4 entry)
                        (< (overlay-start source) (nth 4 entry))
                        (> (overlay-end source) (nth 3 entry))))
                 windows)))

(defun markdown-ts-appear-mermaid--source-height (source window views)
  "Measure SOURCE in WINDOW with the replacement images in VIEWS hidden."
  (let ((images (seq-keep (lambda (view)
                            (when-let* ((image (overlay-get view 'display)))
                              (cons view image)))
                          (seq-uniq views #'eq))))
    (unwind-protect
        (progn
          (dolist (entry images) (overlay-put (car entry) 'display nil))
          (when (text-property-not-all (overlay-start source) (overlay-end source) 'fontified t)
            (font-lock-ensure (overlay-start source) (overlay-end source)))
          (max 1 (cdr (window-text-pixel-size
                       window (overlay-start source) (overlay-end source)))))
      (dolist (entry images) (overlay-put (car entry) 'display (cdr entry))))))

(defun markdown-ts-appear-mermaid--display (source windows)
  "Synchronize SOURCE's image or editable code with WINDOWS."
  (let ((image (overlay-get source 'markdown-ts-appear-mermaid--image))
        (text (overlay-get source 'markdown-ts-appear-mermaid--source))
        (error (overlay-get source 'markdown-ts-appear-mermaid--error))
        (editing (markdown-ts-appear-mermaid--editing-p source))
        (hidden (outline-invisible-p (overlay-start source)))
        (old (overlay-get source 'markdown-ts-appear-mermaid--views)) views)
    (pcase-dolist (`(,window ,width ,layout . ,_) windows)
      (let* ((beg (overlay-start source)) (end (overlay-end source))
             (view (or (seq-find (lambda (view) (eq (overlay-get view 'window) window)) old)
                       (let ((view (make-overlay beg end nil t nil)))
                         (overlay-put view 'window window)
                         (overlay-put view 'priority 200)
                         view)))
             (previous (overlay-get view 'markdown-ts-appear-mermaid--state))
             (state (list image text width layout editing hidden error)))
        (unless (and (= beg (overlay-start view)) (= end (overlay-end view)))
          (move-overlay view beg end))
        (unless (equal state previous)
          (overlay-put view 'markdown-ts-appear-mermaid--state state)
          (overlay-put view 'display nil)
          (overlay-put view 'after-string nil)
          (overlay-put view 'help-echo error)
          (when (and image (not editing) (not hidden) (display-images-p (window-frame window)))
            (when layout
              (let ((size-key (cons text layout)))
                (unless (equal size-key (overlay-get view 'markdown-ts-appear-mermaid--size-key))
                  ;; Other windows' image overlays can affect pixel measurement too.
                  (overlay-put view 'markdown-ts-appear-mermaid--source-height
                               (markdown-ts-appear-mermaid--source-height
                                source window (append old views)))
                  (overlay-put view 'markdown-ts-appear-mermaid--size-key size-key))))
            (let* ((height (and layout (overlay-get view 'markdown-ts-appear-mermaid--source-height)))
                   (key (list image width height))
                   (spec (or (and (equal key (overlay-get view 'markdown-ts-appear-mermaid--image-key))
                                  (overlay-get view 'markdown-ts-appear-mermaid--image))
                             (let ((spec (copy-sequence image)))
                               (setf (image-property spec :max-width) width)
                               (when height (setf (image-property spec :max-height) height))
                               spec))))
              (overlay-put view 'display spec)
              (unless (eq spec (overlay-get view 'markdown-ts-appear-mermaid--image))
                (overlay-put view 'markdown-ts-appear-mermaid--image spec)
                (overlay-put view 'markdown-ts-appear-mermaid--image-key key)))))
        (push view views)))
    (dolist (view old) (unless (memq view views) (delete-overlay view)))
    (setq views (nreverse views))
    (unless (equal views old)
      (overlay-put source 'markdown-ts-appear-mermaid--views views))))

(defun markdown-ts-appear-mermaid--current-p (source key)
  "Return non-nil if SOURCE still represents a live Mermaid block with KEY."
  (when-let* ((buffer (overlay-buffer source)))
    (with-current-buffer buffer
      (and markdown-ts-appear-enable-mermaid-preview (markdown-ts-appear--active-p)
           (equal key (overlay-get source 'markdown-ts-appear-mermaid--key))
           (without-restriction
             (let* ((beg (overlay-start source)) (end (overlay-end source))
                    (node (markdown-ts-appear--node-ancestor
                           (treesit-node-at beg 'markdown) "fenced_code_block")))
               (and node (= beg (treesit-node-start node))
                    (= end (markdown-ts-appear-mermaid--end node))
                    (equal (buffer-substring-no-properties beg end)
                           (overlay-get source 'markdown-ts-appear-mermaid--source)))))))))

(defun markdown-ts-appear-mermaid--complete (job image error)
  "Finish JOB with IMAGE or ERROR, discarding results for obsolete sources."
  (when (eq job markdown-ts-appear-mermaid--job)
    (setq markdown-ts-appear-mermaid--job nil markdown-ts-appear-mermaid--process nil)
    (when image (puthash (car job) image markdown-ts-appear-mermaid--cache))
    (let (buffers)
      (dolist (source (cdr job))
        (when (markdown-ts-appear-mermaid--current-p source (car job))
          (overlay-put source 'markdown-ts-appear-mermaid--image image)
          (overlay-put source 'markdown-ts-appear-mermaid--error error)
          (push (overlay-buffer source) buffers)))
      (dolist (buffer (seq-uniq buffers))
        (with-current-buffer buffer
          (setq markdown-ts-appear-mermaid--view-state nil)
          (markdown-ts-appear-mermaid--refresh t)
          ;; A shorter preview can expose another diagram below the old viewport.
          (when image
            (run-at-time 0 nil #'markdown-ts-appear-mermaid--refresh-buffer buffer t))))
      (when (and error buffers)
        (message "Mermaid preview failed; hover the source block for details")))
    (markdown-ts-appear-mermaid--next t)))

(defun markdown-ts-appear-mermaid--render (job)
  "Start an asynchronous mmdc process for JOB."
  (let* ((directory (make-temp-file "markdown-ts-appear-mermaid-" t))
         (default-directory (file-name-as-directory directory))
         (input (expand-file-name "diagram.mmd" directory))
         (output (expand-file-name "diagram.svg" directory))
         (config (expand-file-name "config.json" directory))
         (log (generate-new-buffer " *markdown-ts-appear-mermaid*"))
         (key (car job)))
    (condition-case err
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file input (insert (nth 2 key)))
            ;; SVG shadows can stall Emacs's synchronous image rasterization.
            (with-temp-file config
              (insert "{\"htmlLabels\":false,\"themeVariables\":"
                      "{\"dropShadow\":\"none\",\"nodeShadow\":false}}")))
          (setq markdown-ts-appear-mermaid--process
                (make-process
                 :name "markdown-ts-appear-mermaid" :buffer log :noquery t
                 :connection-type 'pipe :coding 'utf-8-unix
                 :command (list (car key) "-i" input "-o" output "-c" config
                                "-b" "transparent" "-t" (cadr key))
                 :sentinel
                 (lambda (process _event)
                   (when (memq (process-status process) '(exit signal))
                     (set-process-sentinel process #'ignore)
                     (unwind-protect
                         (let (image error)
                           (condition-case err
                               (if (and (zerop (process-exit-status process))
                                        (file-readable-p output))
                                   (with-temp-buffer
                                     (insert-file-contents output)
                                     (setq image (create-image (buffer-string) 'svg t :scale 1)))
                                 (setq error (with-current-buffer log (string-trim (buffer-string)))))
                             (error (setq error (error-message-string err))))
                            (markdown-ts-appear-mermaid--complete
                             job image (and (not image)
                                            (if (or (null error) (string-empty-p error))
                                                "No SVG image was produced" error))))
                       (kill-buffer log)
                       (delete-directory directory t)))))))
      (error
       (kill-buffer log)
       (delete-directory directory t)
       (markdown-ts-appear-mermaid--complete job nil (error-message-string err))))))

(defun markdown-ts-appear-mermaid--next (&optional update)
  "Start the next job that still has a visible, unedited source.
UPDATE selects which window ends to recompute."
  (while (and (not markdown-ts-appear-mermaid--job) markdown-ts-appear-mermaid--queue)
    (let ((job (pop markdown-ts-appear-mermaid--queue)))
      (when (seq-some (lambda (source)
                        (and (markdown-ts-appear-mermaid--current-p source (car job))
                             (with-current-buffer (overlay-buffer source)
                               (markdown-ts-appear-mermaid--eligible-p
                                source (markdown-ts-appear-mermaid--windows update)))))
                      (cdr job))
        (setq markdown-ts-appear-mermaid--job job)
        (markdown-ts-appear-mermaid--render job)))))

(defun markdown-ts-appear-mermaid--request (source)
  "Queue SOURCE, sharing an existing request for the same diagram."
  (let* ((key (overlay-get source 'markdown-ts-appear-mermaid--key))
         (job (or (and (equal key (car markdown-ts-appear-mermaid--job))
                       markdown-ts-appear-mermaid--job)
                  (assoc key markdown-ts-appear-mermaid--queue))))
    (if job
        (unless (memq source (cdr job)) (setcdr job (cons source (cdr job))))
      (setq markdown-ts-appear-mermaid--queue
            (nconc markdown-ts-appear-mermaid--queue (list (list key source)))))))

(defun markdown-ts-appear-mermaid--refresh (&optional update)
  "Update changed sources and preview visibility without repeating renders.
UPDATE selects window ends to recompute, outside ordinary command hooks."
  (when (and (not markdown-ts-appear-mermaid--refreshing)
             (memq #'markdown-ts-appear-mermaid--refresh post-command-hook))
    (let ((markdown-ts-appear-mermaid--refreshing t))
      (if (not (and markdown-ts-appear-enable-mermaid-preview (markdown-ts-appear--active-p)))
          (progn
            (mapc #'markdown-ts-appear-mermaid--delete markdown-ts-appear-mermaid--objects)
            (setq markdown-ts-appear-mermaid--objects nil
                  markdown-ts-appear-mermaid--scan-state nil
                  markdown-ts-appear-mermaid--view-state nil))
        (without-restriction
          (let* ((theme (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "default"))
                 (scan (list markdown-ts-appear-mermaid-command theme)))
            (unless (equal scan markdown-ts-appear-mermaid--scan-state)
              (setq markdown-ts-appear-mermaid--scan-state scan
                    markdown-ts-appear-mermaid--view-state nil)
              (markdown-ts-appear-mermaid--scan theme))
            (when markdown-ts-appear-mermaid--objects
              (let* ((windows (markdown-ts-appear-mermaid--windows update))
                     (previous markdown-ts-appear-mermaid--view-state)
                     (view (list scan windows (markdown-ts-appear-mermaid--editing-source))))
                (unless (equal view previous)
                  (setq markdown-ts-appear-mermaid--view-state view)
                  (dolist (source (if (and (equal scan (car previous))
                                          (equal windows (cadr previous)))
                                     (delq nil (list (nth 2 previous) (nth 2 view)))
                                   markdown-ts-appear-mermaid--objects))
                    (unless (overlay-get source 'markdown-ts-appear-mermaid--image)
                      (when-let* ((image (gethash (overlay-get source 'markdown-ts-appear-mermaid--key)
                                                 markdown-ts-appear-mermaid--cache)))
                        (overlay-put source 'markdown-ts-appear-mermaid--image image)))
                    (markdown-ts-appear-mermaid--display source windows)
                    (when (and (not (overlay-get source 'markdown-ts-appear-mermaid--image))
                               (not (overlay-get source 'markdown-ts-appear-mermaid--error))
                               (markdown-ts-appear-mermaid--eligible-p source windows))
                      (markdown-ts-appear-mermaid--request source)))))))))
      (markdown-ts-appear-mermaid--next update))))

(defun markdown-ts-appear-mermaid--after-change (beg end &rest _)
  "Invalidate the scan after an edit to a fenced block between BEG and END."
  (when (or (seq-some (lambda (source)
                       (overlay-get source 'markdown-ts-appear-mermaid--source))
                     (overlays-in beg end))
            (markdown-ts-appear--node-ancestor
             (treesit-node-at beg 'markdown) "fenced_code_block"))
    (setq markdown-ts-appear-mermaid--scan-state nil)))

(defun markdown-ts-appear-mermaid--syntax-change (_beg _end)
  "Recheck blocks after structural changes between _BEG and _END."
  (setq markdown-ts-appear-mermaid--scan-state nil))

(defun markdown-ts-appear-mermaid--invalidate (&rest _)
  "Recheck previews after window or outline changes."
  (setq markdown-ts-appear-mermaid--view-state nil)
  (markdown-ts-appear-mermaid--refresh t))

(defun markdown-ts-appear-mermaid--scroll (window _start)
  "Refresh previews when WINDOW scrolls to START."
  (markdown-ts-appear-mermaid--refresh-buffer (window-buffer window) window))

(defun markdown-ts-appear-mermaid--refresh-buffer (buffer &optional update)
  "Refresh visible diagrams if BUFFER is still live.
UPDATE selects which window ends to recompute."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (markdown-ts-appear-mermaid--refresh update))))

(defun markdown-ts-appear-mermaid--setup ()
  "Install Mermaid previews in the current buffer."
  (unless (executable-find markdown-ts-appear-mermaid-command)
    (user-error "Mermaid previews require mmdc; install @mermaid-js/mermaid-cli"))
  (unless (image-type-available-p 'svg)
    (user-error "Mermaid previews require SVG image support"))
  (add-hook 'post-command-hook #'markdown-ts-appear-mermaid--refresh 90 t)
  (add-hook 'after-change-functions #'markdown-ts-appear-mermaid--after-change 90 t)
  (add-hook 'markdown-ts-appear--syntax-change-functions #'markdown-ts-appear-mermaid--syntax-change nil t)
  (add-hook 'markdown-ts-appear--tracking-change-hook #'markdown-ts-appear-mermaid--refresh 90 t)
  (add-hook 'window-configuration-change-hook #'markdown-ts-appear-mermaid--invalidate nil t)
  (add-hook 'window-scroll-functions #'markdown-ts-appear-mermaid--scroll nil t)
  (markdown-ts-appear--add-outline-hook #'markdown-ts-appear-mermaid--invalidate 90)
  (markdown-ts-appear-mermaid--refresh t))

(defun markdown-ts-appear-mermaid--detach ()
  "Detach Mermaid state without deleting previews inherited from a base buffer."
  (remove-hook 'post-command-hook #'markdown-ts-appear-mermaid--refresh t)
  (remove-hook 'after-change-functions #'markdown-ts-appear-mermaid--after-change t)
  (remove-hook 'markdown-ts-appear--syntax-change-functions #'markdown-ts-appear-mermaid--syntax-change t)
  (remove-hook 'markdown-ts-appear--tracking-change-hook #'markdown-ts-appear-mermaid--refresh t)
  (remove-hook 'window-configuration-change-hook #'markdown-ts-appear-mermaid--invalidate t)
  (remove-hook 'window-scroll-functions #'markdown-ts-appear-mermaid--scroll t)
  (markdown-ts-appear--remove-outline-hook #'markdown-ts-appear-mermaid--invalidate)
  (setq markdown-ts-appear-mermaid--objects nil
        markdown-ts-appear-mermaid--scan-state nil
        markdown-ts-appear-mermaid--view-state nil))

(defun markdown-ts-appear-mermaid--teardown ()
  "Remove Mermaid previews and cancel a render without remaining subscribers."
  (mapc #'markdown-ts-appear-mermaid--delete markdown-ts-appear-mermaid--objects)
  (markdown-ts-appear-mermaid--detach)
  (when (and (process-live-p markdown-ts-appear-mermaid--process)
             (not (seq-some #'overlay-buffer (cdr markdown-ts-appear-mermaid--job))))
    ;; Let Puppeteer close its browser instead of abruptly killing Node.
    (interrupt-process markdown-ts-appear-mermaid--process)))

(provide 'markdown-ts-appear-mermaid)
;;; markdown-ts-appear-mermaid.el ends here
