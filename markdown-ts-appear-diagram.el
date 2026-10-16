;;; markdown-ts-appear-diagram.el --- Shared diagram previews -*- lexical-binding: t; -*-

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

;; Registered fence languages share visibility, sizing, caching and a render
;; queue.  Renderers supply SVG text through a callback and may return a cancel
;; function.  The built-in mmdc renderer uses the same interface.

;;; Code:

(require 'markdown-ts-mode)
(require 'cl-lib)
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

(defvar markdown-ts-appear-diagram-renderers)
(defvar markdown-ts-appear-diagram-size)
(defvar markdown-ts-appear-code-fence-style)

(defun markdown-ts-appear-diagram--key-equal (a b)
  "Compare render keys A and B, using identity for renderer functions."
  (and (eq (car a) (car b)) (equal (cdr a) (cdr b))))

(defun markdown-ts-appear-diagram--key-hash (key)
  "Hash KEY without depending on mutable renderer closure state."
  (logxor (sxhash-eq (car key)) (sxhash-equal (cdr key))))

(define-hash-table-test 'markdown-ts-appear-diagram--key-equal
  #'markdown-ts-appear-diagram--key-equal #'markdown-ts-appear-diagram--key-hash)

(defvar markdown-ts-appear-diagram--cache
  (make-hash-table :test 'markdown-ts-appear-diagram--key-equal :weakness 'value)
  "Images keyed by (RENDERER THEME BODY LANGUAGE).")
(defvar markdown-ts-appear-diagram--queue nil
  "Pending jobs, each a (KEY . SOURCE-OVERLAYS) pair.")
(defvar markdown-ts-appear-diagram--job nil
  "Job currently being rendered.")
(defvar markdown-ts-appear-diagram--cancel nil
  "Optional cancellation function for the current job.")
(defvar markdown-ts-appear-diagram--advancing nil
  "Non-nil while draining the queue, including synchronous completions.")
(defvar markdown-ts-appear-diagram--query nil
  "Compiled fenced-block query.")
(defvar markdown-ts-appear-diagram--refreshing nil
  "Non-nil while updating diagram views.")
(defvar-local markdown-ts-appear-diagram--objects nil
  "Source overlays holding render keys, images and window-specific views.")
(defvar-local markdown-ts-appear-diagram--scan-state nil
  "Last renderer registry and theme scanned.")
(defvar-local markdown-ts-appear-diagram--view-state nil
  "Last source, point and window state displayed.")

(defun markdown-ts-appear-diagram--same-scan-p (a b)
  "Return non-nil when scan states A and B select the same renderer identities."
  (and (equal (cadr a) (cadr b))
       (or (eq (car a) (car b))
           (and (= (length (car a)) (length (car b)))
                (cl-every (lambda (x y)
                            (and (equal (car x) (car y)) (eq (cdr x) (cdr y))))
                          (car a) (car b))))))

(defun markdown-ts-appear-diagram--delete (source)
  "Delete SOURCE and its views."
  (mapc #'delete-overlay (overlay-get source 'markdown-ts-appear-diagram--views))
  (delete-overlay source))

(defun markdown-ts-appear-diagram--body (node)
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

(defun markdown-ts-appear-diagram--end (node)
  "Return NODE's end, preserving its final newline outside the preview."
  (let ((end (treesit-node-end node)))
    (if (eq (char-before end) ?\n) (1- end) end)))

(defun markdown-ts-appear-diagram--scan (theme)
  "Reconcile registered fenced blocks with the current syntax and THEME."
  (with-memoization markdown-ts-appear-diagram--query
    (treesit-query-compile 'markdown '((fenced_code_block) @block)))
  (let ((existing (make-hash-table :test #'eql)) current)
    (dolist (source markdown-ts-appear-diagram--objects)
      (if (overlay-buffer source)
          (puthash (overlay-start source) source existing)
        (markdown-ts-appear-diagram--delete source)))
    (dolist (node (treesit-query-capture (treesit-buffer-root-node 'markdown)
                                        markdown-ts-appear-diagram--query nil nil t))
      (when-let* ((language-node (treesit-search-subtree node "\\`language\\'"))
                  (language (downcase (treesit-node-text language-node t)))
                  (renderer (cdr (assoc-string language markdown-ts-appear-diagram-renderers t)))
                  (body (markdown-ts-appear-diagram--body node)))
        (let* ((beg (treesit-node-start node))
               (end (markdown-ts-appear-diagram--end node))
               (source (or (gethash beg existing) (make-overlay beg end nil t nil)))
               (text (buffer-substring-no-properties beg end))
               (key (list renderer theme body language)))
          (remhash beg existing)
          (unless (= (overlay-end source) end) (move-overlay source beg end))
          (unless (equal text (overlay-get source 'markdown-ts-appear-diagram--source))
            (overlay-put source 'markdown-ts-appear-diagram--source text))
          (unless (markdown-ts-appear-diagram--key-equal
                   key (overlay-get source 'markdown-ts-appear-diagram--key))
            (overlay-put source 'markdown-ts-appear-diagram--key key)
            (overlay-put source 'markdown-ts-appear-diagram--image
                         (gethash key markdown-ts-appear-diagram--cache))
            (overlay-put source 'markdown-ts-appear-diagram--error nil))
          (push source current))))
    (maphash (lambda (_ source) (markdown-ts-appear-diagram--delete source)) existing)
    (setq markdown-ts-appear-diagram--objects (nreverse current))
    (markdown-ts-appear-diagram--cancel-unused)))

(defun markdown-ts-appear-diagram--windows (&optional update)
  "Return (WINDOW WIDTH LAYOUT START END) for windows showing this buffer.
LAYOUT describes source font metrics and wrapping, or nil for width-only sizing.
UPDATE is t to recompute all window ends, or a window to update only its end."
  (let* ((source-height (eq markdown-ts-appear-diagram-size 'source))
         (max-width (if source-height markdown-ts-image-max-width
                      markdown-ts-appear-diagram-size)))
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

(defun markdown-ts-appear-diagram--editing-p (source)
  "Return non-nil when SOURCE is being edited with source tracking active."
  (and (markdown-ts-appear--tracking-p)
       (<= (overlay-start source) (point)) (<= (point) (overlay-end source))))

(defun markdown-ts-appear-diagram--editing-source ()
  "Return the source being edited, without visiting other diagrams."
  (when (markdown-ts-appear--tracking-p)
    (seq-find (lambda (source)
                (and (overlay-get source 'markdown-ts-appear-diagram--source)
                     (markdown-ts-appear-diagram--editing-p source)))
              (append (overlays-at (point))
                      (overlays-at (max (point-min) (1- (point))))))))

(defun markdown-ts-appear-diagram--eligible-p (source windows)
  "Return non-nil when SOURCE needs a preview visible in WINDOWS."
  (and (not (markdown-ts-appear-diagram--editing-p source))
       (not (outline-invisible-p (overlay-start source)))
       (seq-some (lambda (entry)
                   (and (display-images-p (window-frame (car entry)))
                        (nth 4 entry)
                        (< (overlay-start source) (nth 4 entry))
                        (> (overlay-end source) (nth 3 entry))))
                 windows)))

(defun markdown-ts-appear-diagram--source-height (source window views)
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

(defun markdown-ts-appear-diagram--display (source windows)
  "Synchronize SOURCE's image or editable code with WINDOWS."
  (let ((image (overlay-get source 'markdown-ts-appear-diagram--image))
        (text (overlay-get source 'markdown-ts-appear-diagram--source))
        (error (overlay-get source 'markdown-ts-appear-diagram--error))
        (editing (markdown-ts-appear-diagram--editing-p source))
        (hidden (outline-invisible-p (overlay-start source)))
        (old (overlay-get source 'markdown-ts-appear-diagram--views)) views)
    (pcase-dolist (`(,window ,width ,layout . ,_) windows)
      (let* ((beg (overlay-start source)) (end (overlay-end source))
             (view (or (seq-find (lambda (view) (eq (overlay-get view 'window) window)) old)
                       (let ((view (make-overlay beg end nil t nil)))
                         (overlay-put view 'window window)
                         (overlay-put view 'priority 200)
                         view)))
             (previous (overlay-get view 'markdown-ts-appear-diagram--state))
             (state (list image text width layout editing hidden error)))
        (unless (and (= beg (overlay-start view)) (= end (overlay-end view)))
          (move-overlay view beg end))
        (unless (equal state previous)
          (overlay-put view 'markdown-ts-appear-diagram--state state)
          (overlay-put view 'display nil)
          (overlay-put view 'after-string nil)
          (overlay-put view 'help-echo error)
          (when (and image (not editing) (not hidden) (display-images-p (window-frame window)))
            (when layout
              (let ((size-key (cons text layout)))
                (unless (equal size-key (overlay-get view 'markdown-ts-appear-diagram--size-key))
                  ;; Other windows' image overlays can affect pixel measurement too.
                  (overlay-put view 'markdown-ts-appear-diagram--source-height
                               (markdown-ts-appear-diagram--source-height
                                source window (append old views)))
                  (overlay-put view 'markdown-ts-appear-diagram--size-key size-key))))
            (let* ((height (and layout (overlay-get view 'markdown-ts-appear-diagram--source-height)))
                   (key (list image width height))
                   (spec (or (and (equal key (overlay-get view 'markdown-ts-appear-diagram--image-key))
                                  (overlay-get view 'markdown-ts-appear-diagram--image))
                             (let ((spec (copy-sequence image)))
                               (setf (image-property spec :max-width) width)
                               (when height (setf (image-property spec :max-height) height))
                               spec))))
              (overlay-put view 'display spec)
              (unless (eq spec (overlay-get view 'markdown-ts-appear-diagram--image))
                (overlay-put view 'markdown-ts-appear-diagram--image spec)
                (overlay-put view 'markdown-ts-appear-diagram--image-key key)))))
        (push view views)))
    (dolist (view old) (unless (memq view views) (delete-overlay view)))
    (setq views (nreverse views))
    (unless (equal views old)
      (overlay-put source 'markdown-ts-appear-diagram--views views))))

(defun markdown-ts-appear-diagram--current-p (source key)
  "Return non-nil if SOURCE still represents the registered block with KEY."
  (when-let* ((buffer (overlay-buffer source)))
    (with-current-buffer buffer
      (and (markdown-ts-appear--active-p)
           (markdown-ts-appear-diagram--key-equal
            key (overlay-get source 'markdown-ts-appear-diagram--key))
           (eq (car key) (cdr (assoc-string (nth 3 key) markdown-ts-appear-diagram-renderers t)))
           (without-restriction
             (let* ((beg (overlay-start source)) (end (overlay-end source))
                    (node (markdown-ts-appear--node-ancestor
                           (treesit-node-at beg 'markdown) "fenced_code_block")))
               (and node (= beg (treesit-node-start node))
                    (= end (markdown-ts-appear-diagram--end node))
                    (equal (buffer-substring-no-properties beg end)
                           (overlay-get source 'markdown-ts-appear-diagram--source)))))))))

(defun markdown-ts-appear-diagram--cancel-unused ()
  "Retire an active job with no current subscribers, then cancel its renderer."
  (when (and markdown-ts-appear-diagram--job
             (not (seq-some (lambda (source)
                              (markdown-ts-appear-diagram--current-p
                               source (car markdown-ts-appear-diagram--job)))
                            (cdr markdown-ts-appear-diagram--job))))
    (let ((cancel markdown-ts-appear-diagram--cancel))
      ;; A cancellation function may invoke the old callback synchronously.
      (setq markdown-ts-appear-diagram--job nil markdown-ts-appear-diagram--cancel nil)
      (when cancel
        (condition-case err (funcall cancel)
          (error (message "Diagram cancellation failed: %s" (error-message-string err))))))))

(defun markdown-ts-appear-diagram--complete (job image error)
  "Finish JOB with IMAGE or ERROR, discarding results for obsolete sources."
  (when (eq job markdown-ts-appear-diagram--job)
    (setq markdown-ts-appear-diagram--job nil markdown-ts-appear-diagram--cancel nil)
    (when image (puthash (car job) image markdown-ts-appear-diagram--cache))
    (let (buffers)
      (dolist (source (cdr job))
        (when (markdown-ts-appear-diagram--current-p source (car job))
          (overlay-put source 'markdown-ts-appear-diagram--image image)
          (overlay-put source 'markdown-ts-appear-diagram--error error)
          (push (overlay-buffer source) buffers)))
      (dolist (buffer (seq-uniq buffers))
        (with-current-buffer buffer
          (setq markdown-ts-appear-diagram--view-state nil)
          (markdown-ts-appear-diagram--refresh t)
          ;; A shorter preview can expose another diagram below the old viewport.
          (when image
            (run-at-time 0 nil #'markdown-ts-appear-diagram--refresh-buffer buffer t))))
      (when (and error buffers)
        (message "Diagram preview failed; hover the source block for details")))
    (markdown-ts-appear-diagram--next t)))

(defun markdown-ts-appear-diagram--render (job)
  "Invoke JOB's renderer, accepting a synchronous or asynchronous SVG result."
  (let* ((key (car job))
         finished
         (callback
          (lambda (svg &optional problem)
            (unless finished
              (setq finished t)
              (when (eq job markdown-ts-appear-diagram--job)
                (let (image error)
                  (condition-case err
                      (if svg
                          (setq image (create-image svg 'svg t :scale 1))
                        (setq error (if (or (null problem) (string-empty-p problem))
                                        "No SVG image was produced" problem)))
                    ((error quit) (setq error (error-message-string err))))
                  (markdown-ts-appear-diagram--complete job image error)))))))
    (condition-case err
        (let ((cancel (funcall (car key) (nth 3 key) (nth 2 key) (cadr key) callback)))
          (when (and (functionp cancel) (not finished))
            (if (eq job markdown-ts-appear-diagram--job)
                (setq markdown-ts-appear-diagram--cancel cancel)
              (funcall cancel))))
      ((error quit) (funcall callback nil (error-message-string err))))))

(defun markdown-ts-appear-diagram--next (&optional update)
  "Start the next visible job, forwarding UPDATE to window measurement."
  (unless (or markdown-ts-appear-diagram--advancing markdown-ts-appear-diagram--refreshing)
    (let ((markdown-ts-appear-diagram--advancing t))
      (while (and (not markdown-ts-appear-diagram--job) markdown-ts-appear-diagram--queue)
        (let ((job (pop markdown-ts-appear-diagram--queue)))
          (when-let* ((source
                       (seq-find (lambda (source)
                                   (and (markdown-ts-appear-diagram--current-p source (car job))
                                        (with-current-buffer (overlay-buffer source)
                                          (markdown-ts-appear-diagram--eligible-p
                                           source (markdown-ts-appear-diagram--windows update)))))
                                 (cdr job))))
            (setq markdown-ts-appear-diagram--job job)
            (with-current-buffer (overlay-buffer source)
              (markdown-ts-appear-diagram--render job))))))))

(defun markdown-ts-appear-diagram--request (source)
  "Queue SOURCE, sharing an existing request for the same diagram."
  (let* ((key (overlay-get source 'markdown-ts-appear-diagram--key))
         (job (or (and (markdown-ts-appear-diagram--key-equal
                        key (car markdown-ts-appear-diagram--job))
                       markdown-ts-appear-diagram--job)
                  (seq-find (lambda (job)
                              (markdown-ts-appear-diagram--key-equal key (car job)))
                            markdown-ts-appear-diagram--queue))))
    (if job
        (unless (memq source (cdr job)) (setcdr job (cons source (cdr job))))
      (setq markdown-ts-appear-diagram--queue
            (nconc markdown-ts-appear-diagram--queue (list (list key source)))))))

(defun markdown-ts-appear-diagram--refresh (&optional update)
  "Refresh diagrams, using cached window ends unless UPDATE requests otherwise."
  (when (and (not markdown-ts-appear-diagram--refreshing)
             (memq #'markdown-ts-appear-diagram--refresh post-command-hook))
    (let ((markdown-ts-appear-diagram--refreshing t))
      (if (not (and markdown-ts-appear-diagram-renderers (markdown-ts-appear--active-p)))
          (progn
            (mapc #'markdown-ts-appear-diagram--delete markdown-ts-appear-diagram--objects)
            (setq markdown-ts-appear-diagram--objects nil
                  markdown-ts-appear-diagram--scan-state nil
                  markdown-ts-appear-diagram--view-state nil)
            (markdown-ts-appear-diagram--cancel-unused))
        (without-restriction
          (let* ((theme (if (eq (frame-parameter nil 'background-mode) 'dark) "dark" "default"))
                 (scan (list markdown-ts-appear-diagram-renderers theme)))
            (unless (markdown-ts-appear-diagram--same-scan-p
                     scan markdown-ts-appear-diagram--scan-state)
              (setq markdown-ts-appear-diagram--scan-state scan
                    markdown-ts-appear-diagram--view-state nil)
              (markdown-ts-appear-diagram--scan theme))
            (when markdown-ts-appear-diagram--objects
              (let* ((windows (markdown-ts-appear-diagram--windows update))
                     (previous markdown-ts-appear-diagram--view-state)
                     (view (list scan windows (markdown-ts-appear-diagram--editing-source))))
                (unless (equal view previous)
                  (setq markdown-ts-appear-diagram--view-state view)
                  (dolist (source (if (and (equal scan (car previous))
                                          (equal windows (cadr previous)))
                                     (delq nil (list (nth 2 previous) (nth 2 view)))
                                   markdown-ts-appear-diagram--objects))
                    (unless (overlay-get source 'markdown-ts-appear-diagram--image)
                      (when-let* ((image (gethash (overlay-get source 'markdown-ts-appear-diagram--key)
                                                 markdown-ts-appear-diagram--cache)))
                        (overlay-put source 'markdown-ts-appear-diagram--image image)))
                    (markdown-ts-appear-diagram--display source windows)
                    (when (and (not (overlay-get source 'markdown-ts-appear-diagram--image))
                               (not (overlay-get source 'markdown-ts-appear-diagram--error))
                               (markdown-ts-appear-diagram--eligible-p source windows))
                      (markdown-ts-appear-diagram--request source))))))))))
    ;; Rendering starts after view updates, so synchronous callbacks can refresh.
    (markdown-ts-appear-diagram--next update)))

(defun markdown-ts-appear-diagram--after-change (beg end &rest _)
  "Invalidate changed fenced blocks between BEG and END."
  (when (or (seq-some (lambda (source)
                       (overlay-get source 'markdown-ts-appear-diagram--source))
                     (overlays-in beg end))
            (markdown-ts-appear--node-ancestor
             (treesit-node-at beg 'markdown) "fenced_code_block"))
    (setq markdown-ts-appear-diagram--scan-state nil)))

(defun markdown-ts-appear-diagram--syntax-change (_beg _end)
  "Recheck blocks after structural changes between _BEG and _END."
  (setq markdown-ts-appear-diagram--scan-state nil))

(defun markdown-ts-appear-diagram--invalidate (&rest _)
  "Recheck previews after window or outline changes."
  (setq markdown-ts-appear-diagram--view-state nil)
  (markdown-ts-appear-diagram--refresh t))

(defun markdown-ts-appear-diagram--scroll (window _start)
  "Refresh previews when WINDOW scrolls to _START."
  (markdown-ts-appear-diagram--refresh-buffer (window-buffer window) window))

(defun markdown-ts-appear-diagram--refresh-buffer (buffer &optional update)
  "Refresh live BUFFER, forwarding UPDATE to window measurement."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer (markdown-ts-appear-diagram--refresh update))))

(defun markdown-ts-appear-diagram--setup ()
  "Install previews for registered languages."
  (unless (image-type-available-p 'svg)
    (user-error "Diagram previews require SVG image support"))
  (add-hook 'post-command-hook #'markdown-ts-appear-diagram--refresh 90 t)
  (add-hook 'after-change-functions #'markdown-ts-appear-diagram--after-change 90 t)
  (add-hook 'markdown-ts-appear--syntax-change-functions #'markdown-ts-appear-diagram--syntax-change nil t)
  (add-hook 'markdown-ts-appear--tracking-change-hook #'markdown-ts-appear-diagram--refresh 90 t)
  (add-hook 'window-configuration-change-hook #'markdown-ts-appear-diagram--invalidate nil t)
  (add-hook 'window-scroll-functions #'markdown-ts-appear-diagram--scroll nil t)
  (markdown-ts-appear--add-outline-hook #'markdown-ts-appear-diagram--invalidate 90)
  (markdown-ts-appear-diagram--refresh t))

(defun markdown-ts-appear-diagram--detach ()
  "Detach inherited diagram state without deleting the base buffer's overlays."
  (remove-hook 'post-command-hook #'markdown-ts-appear-diagram--refresh t)
  (remove-hook 'after-change-functions #'markdown-ts-appear-diagram--after-change t)
  (remove-hook 'markdown-ts-appear--syntax-change-functions #'markdown-ts-appear-diagram--syntax-change t)
  (remove-hook 'markdown-ts-appear--tracking-change-hook #'markdown-ts-appear-diagram--refresh t)
  (remove-hook 'window-configuration-change-hook #'markdown-ts-appear-diagram--invalidate t)
  (remove-hook 'window-scroll-functions #'markdown-ts-appear-diagram--scroll t)
  (markdown-ts-appear--remove-outline-hook #'markdown-ts-appear-diagram--invalidate)
  (setq markdown-ts-appear-diagram--objects nil
        markdown-ts-appear-diagram--scan-state nil
        markdown-ts-appear-diagram--view-state nil))

(defun markdown-ts-appear-diagram--teardown ()
  "Remove previews and release renders with no remaining subscribers."
  (mapc #'markdown-ts-appear-diagram--delete markdown-ts-appear-diagram--objects)
  (markdown-ts-appear-diagram--detach)
  (markdown-ts-appear-diagram--cancel-unused)
  (markdown-ts-appear-diagram--next t))

;;;###autoload
(defun markdown-ts-appear-diagram-mmdc (_language code theme callback &optional command)
  "Render CODE with Mermaid CLI using THEME, then call CALLBACK with SVG or error.
_LANGUAGE is supplied by `markdown-ts-appear-diagram-renderers'.
COMMAND defaults to \"mmdc\".  Return a zero-argument cancellation function."
  (let* ((directory (make-temp-file "markdown-ts-appear-mmdc-" t))
         (default-directory (file-name-as-directory directory))
         (input (expand-file-name "diagram.mmd" directory))
         (output (expand-file-name "diagram.svg" directory))
         (config (expand-file-name "config.json" directory))
         (log (generate-new-buffer " *markdown-ts-appear-mmdc*")))
    (condition-case err
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file input (insert code))
            ;; SVG shadows can stall Emacs's synchronous image rasterization.
            (with-temp-file config
              (insert "{\"htmlLabels\":false,\"themeVariables\":"
                      "{\"dropShadow\":\"none\",\"nodeShadow\":false}}")))
          (let ((process
                 (make-process
                  :name "markdown-ts-appear-diagram-mmdc" :buffer log :noquery t
                  :connection-type 'pipe :coding 'utf-8-unix
                  :command (list (or command "mmdc") "-i" input "-o" output "-c" config
                                 "-b" "transparent" "-t" theme)
                  :sentinel
                  (lambda (process _event)
                    (when (memq (process-status process) '(exit signal))
                      (set-process-sentinel process #'ignore)
                      (unwind-protect
                          (let (svg error)
                            (condition-case err
                                (if (and (zerop (process-exit-status process))
                                         (file-readable-p output))
                                    (with-temp-buffer
                                      (insert-file-contents output)
                                      (setq svg (buffer-string)))
                                  (setq error (with-current-buffer log (string-trim (buffer-string)))))
                              (error (setq error (error-message-string err))))
                            (funcall callback svg error))
                        (kill-buffer log)
                        (delete-directory directory t)))))))
            (lambda ()
              ;; Let Puppeteer close its browser instead of abruptly killing Node.
              (when (process-live-p process) (interrupt-process process)))))
      (error
       (kill-buffer log)
       (delete-directory directory t)
       (funcall callback nil (error-message-string err))
       nil))))

(provide 'markdown-ts-appear-diagram)
;;; markdown-ts-appear-diagram.el ends here
