;;; markdown-ts-appear-mathjax.el --- Independent MathJax previews -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Optional standalone math renderer for markdown-ts-mode.  Requires only the
;; GNU ELPA mathjax package and Node.js, not a TeX distribution or another math
;; frontend/backend.  Detection, worker, cache, reveal and editing presentation
;; live in this file.  Manual \tag is supported; document-wide automatic
;; numbering and cross-formula references are deliberately not implemented.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'image)
(require 'button)
(require 'hl-line)
(require 'markdown-ts-mode)

(declare-function mathjax-available-p "mathjax")
(declare-function markdown-ts-appear--tracking-p "markdown-ts-appear")

(defgroup markdown-ts-appear-mathjax nil
  "Independent MathJax previews."
  :group 'markdown-ts
  :prefix "markdown-ts-appear-mathjax-")

(defvar markdown-ts-appear-mathjax-mode)

(defcustom markdown-ts-appear-mathjax-terminal-preview nil
  "Use experimental Kitty previews for display and inline formulas.
Requires `kitty-graphics-mode' in a terminal Emacs session.  Editing previews
currently require a blank line after the formula.  Enable before starting
`markdown-ts-appear-mathjax-mode'.  Mixed GUI/TTY views are not supported."
  :type 'boolean :group 'markdown-ts-appear-mathjax)

(declare-function markdown-ts-appear-mathjax-kitty--available-p "markdown-ts-appear-mathjax-kitty" (ov))
(declare-function markdown-ts-appear-mathjax-kitty--sync "markdown-ts-appear-mathjax-kitty" (ov))
(declare-function markdown-ts-appear-mathjax-kitty--clear "markdown-ts-appear-mathjax-kitty" (ov))
(declare-function markdown-ts-appear-mathjax-kitty--repaint "markdown-ts-appear-mathjax-kitty" ())

(defun markdown-ts-appear-mathjax--set-option (option value)
  "Set OPTION to VALUE and refresh buffers using its default."
  (set-default option value)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (bound-and-true-p markdown-ts-appear-mathjax-mode)
                 (not (local-variable-p option)))
        (if (eq option 'markdown-ts-appear-mathjax-macros)
            (markdown-ts-appear-mathjax-refresh)
          (markdown-ts-appear-mathjax--redisplay))))))

(defcustom markdown-ts-appear-mathjax-edit-preview nil
  "Show source above a framed preview while editing display math.
Nil shows plain source without a frame, and renders when point leaves or
source reveal stops.  Inline math always uses plain-source editing."
  :type 'boolean :safe #'booleanp :group 'markdown-ts-appear-mathjax
  :initialize #'custom-initialize-default :set #'markdown-ts-appear-mathjax--set-option)

(defcustom markdown-ts-appear-mathjax-delay 0.2
  "Idle seconds before reconciling edits and updating a live preview."
  :type 'number :group 'markdown-ts-appear-mathjax)

(defcustom markdown-ts-appear-mathjax-macros ""
  "MathJax macro definitions prepended independently to every formula."
  :type 'string :group 'markdown-ts-appear-mathjax
  :initialize #'custom-initialize-default :set #'markdown-ts-appear-mathjax--set-option)

(defcustom markdown-ts-appear-mathjax-scale 1.0
  "Scale of previews relative to the source font's x-height."
  :type 'number :group 'markdown-ts-appear-mathjax
  :initialize #'custom-initialize-default :set #'markdown-ts-appear-mathjax--set-option)

(defcustom markdown-ts-appear-mathjax-center-display t
  "Center display formulas in reading mode."
  :type 'boolean :group 'markdown-ts-appear-mathjax
  :initialize #'custom-initialize-default :set #'markdown-ts-appear-mathjax--set-option)

(defcustom markdown-ts-appear-mathjax-cache-directory
  (expand-file-name "emacs/markdown-ts-appear-mathjax/"
                    (or (getenv "XDG_CACHE_HOME") "~/.cache/"))
  "Directory containing color-independent MathJax results."
  :type 'directory :group 'markdown-ts-appear-mathjax)

(defcustom markdown-ts-appear-mathjax-timeout 30
  "Maximum seconds to wait for a worker response."
  :type 'number :group 'markdown-ts-appear-mathjax)

(defface markdown-ts-appear-mathjax-source
  '((t :inherit hl-line :extend t))
  "Background of source in a live editing preview."
  :group 'markdown-ts-appear-mathjax)

(defface markdown-ts-appear-mathjax-preview
  '((t :inherit default :extend t :box (:line-width (-1 . -1) :color "gray50")))
  "Border and background of a live editing preview."
  :group 'markdown-ts-appear-mathjax)

(defvar-local markdown-ts-appear-mathjax-preview-functions nil
  "Functions called with (OVERLAY IMAGE) when a preview changes.
IMAGE is nil on failure.  Consumers can reuse ordinary image display overlays.")

(defvar markdown-ts-appear-mathjax--process nil)
(defvar markdown-ts-appear-mathjax--cache (make-hash-table :test #'equal))
(defvar markdown-ts-appear-mathjax--images (make-hash-table :test #'equal :weakness 'value))
(defvar markdown-ts-appear-mathjax--pending (make-hash-table :test #'equal))
(defvar markdown-ts-appear-mathjax--fontifying-spans nil
  "Optional (BUFFER . FUNCTION) supplying spans shared with appear's fontifier.")
(defvar-local markdown-ts-appear-mathjax--objects nil)
(defvar-local markdown-ts-appear-mathjax--current nil)
(defvar-local markdown-ts-appear-mathjax--timer nil)
(defvar-local markdown-ts-appear-mathjax--dirty nil)
(defvar-local markdown-ts-appear-mathjax--managed nil)

;;; Worker and content cache

(defconst markdown-ts-appear-mathjax--javascript
  "'use strict';
const bundle = process.argv[1];
process.argv[2] = JSON.stringify({options: {enableAssistiveMml: false}});
const write = process.stdout.write.bind(process.stdout);
process.stdout.write = (chunk, ...args) => write(chunk + '\\n', ...args);
require(bundle);
MathJax.startup.promise.then(() => {
  const {startup, config} = MathJax;
  MathJax.tex2svg = (source, options) => {
    const document = MathJax._.mathjax.mathjax.document('', {
      ...config.options,
      InputJax: new startup.constructors.tex(config.tex),
      OutputJax: startup.output,
    });
    return document.convert(source, options);
  };
});"
  "Embedded worker shim: persistent code/fonts, isolated TeX state per request.")

(defun markdown-ts-appear-mathjax--stop (process message)
  "Stop PROCESS and fail outstanding callbacks with MESSAGE."
  (let ((queue (process-get process 'queue)) (buffer (process-buffer process)))
    (process-put process 'queue nil)
    (process-put process 'tail nil)
    (when (eq process markdown-ts-appear-mathjax--process)
      (setq markdown-ts-appear-mathjax--process nil))
    (set-process-sentinel process #'ignore)
    (when (process-live-p process) (delete-process process))
    (when (buffer-live-p buffer) (kill-buffer buffer))
    (dolist (callback queue) (funcall callback (list (cons 'error message)) t))))

(defun markdown-ts-appear-mathjax-stop ()
  "Stop the worker; the next uncached request starts it again."
  (interactive)
  (when markdown-ts-appear-mathjax--process
    (markdown-ts-appear-mathjax--stop markdown-ts-appear-mathjax--process "MathJax worker stopped")))

(defun markdown-ts-appear-mathjax--filter (process text)
  "Parse complete JSON lines from PROCESS's TEXT exactly once."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (save-excursion (insert text))
    (while-let (((process-live-p process)) ((search-forward "\n" nil t))
                (end (point))
                (data (condition-case err
                          (save-restriction
                            (narrow-to-region (point-min) (1- end))
                            (goto-char (point-min))
                            (json-parse-buffer :object-type 'alist))
                        (json-error (markdown-ts-appear-mathjax--stop process (error-message-string err)) nil))))
      (delete-region (point-min) end)
      (let ((queue (process-get process 'queue)))
        (process-put process 'queue (cdr queue))
        (unless (cdr queue) (process-put process 'tail nil))
        (funcall (car queue) data)))))

(defun markdown-ts-appear-mathjax--send (formula display callback)
  "Send FORMULA in DISPLAY style; call CALLBACK with data and optional setup flag."
  (unless (process-live-p markdown-ts-appear-mathjax--process)
    (unless (and (require 'mathjax nil t) (mathjax-available-p))
      (error "Install the GNU ELPA mathjax package and Node.js"))
    (let ((buffer (generate-new-buffer " *appear-mathjax*"))
          (bundle (expand-file-name "math2svg.js" (file-name-directory (symbol-file 'mathjax-render)))))
      (condition-case err
          (setq markdown-ts-appear-mathjax--process
                (make-process
                 :name "appear-mathjax" :buffer buffer :noquery t
                 :connection-type 'pipe :coding 'utf-8-unix
                 :command (list "node" "-e" markdown-ts-appear-mathjax--javascript bundle)
                 :filter #'markdown-ts-appear-mathjax--filter
                 :sentinel (lambda (process event)
                             (when (memq (process-status process) '(exit signal))
                               (markdown-ts-appear-mathjax--stop process event)))))
        (error (kill-buffer buffer) (signal (car err) (cdr err))))))
  (let* ((process markdown-ts-appear-mathjax--process) (cell (list callback))
         (tail (process-get process 'tail)))
    (if tail (setcdr tail cell) (process-put process 'queue cell))
    (process-put process 'tail cell)
    (condition-case err
        (process-send-string process
                             (concat (json-serialize (list :math formula :format "tex"
                                                           :display (if display t :false) :em 16 :ex 8)) "\n"))
      (error (markdown-ts-appear-mathjax--stop process (error-message-string err))))
    process))

(defun markdown-ts-appear-mathjax--attribute (tag name)
  "Return double-quoted attribute NAME from SVG TAG."
  (when (string-match (concat "[[:space:]]" name "=\"\\([^\"]*\\)\"") tag)
    (match-string 1 tag)))

(defun markdown-ts-appear-mathjax--normalize (svg display)
  "Return a color-independent cache entry for SVG in DISPLAY style."
  (unless (string-match "<svg\\(?:[[:space:]][^>]*\\)?>" svg)
    (error "MathJax returned no SVG"))
  (let* ((beg (match-beginning 0)) (end (match-end 0)) (tag (match-string 0 svg))
         (box (mapcar #'string-to-number (split-string (or (markdown-ts-appear-mathjax--attribute tag "viewBox") ""))))
         (height-ex (string-to-number (or (markdown-ts-appear-mathjax--attribute tag "height") "0")))
         (style (markdown-ts-appear-mathjax--attribute tag "style"))
         (wide (and (null box) style (string-match "min-width: *\\([0-9.]+\\)ex" style)
                    (string-to-number (match-string 1 style))))
         (align (and style (string-match "vertical-align: *\\(-?[0-9.]+\\)ex" style)
                     (string-to-number (match-string 1 style)))))
    (unless (and (> height-ex 0) (or (= (length box) 4) wide))
      (error "MathJax returned invalid SVG dimensions"))
    (let ((width (if wide (* wide 4.42) (/ (nth 2 box) 100.0)))
          (height (if wide (* height-ex 4.42) (/ (nth 3 box) 100.0))))
      (when wide
        (setq tag (concat (substring tag 0 -1)
                          (format " viewBox=\"0 0 %.6f %.6f\">" (* wide 8) (* height-ex 8)))))
      (dolist (pair `(("width" . ,width) ("height" . ,height)))
        (setq tag (replace-regexp-in-string
                   (concat "[[:space:]]" (car pair) "=\"[^\"]*\"")
                   (format " %s=\"%.6fpt\"" (car pair) (cdr pair)) tag t t)))
      (list :svg (concat (substring svg 0 beg) tag (substring svg end))
            :width width :ex (/ height height-ex)
            :ascent (if display -1 (max 0 (min 100 (round (* 100 (+ 1 (/ (or align 0) height-ex)))))))))))

(defun markdown-ts-appear-mathjax--key (body display)
  "Return the content key for BODY, DISPLAY and this buffer's macros."
  (secure-hash 'sha1 (format "standalone-v1\0%s\0%S\0%s"
                             body display markdown-ts-appear-mathjax-macros)))

(defun markdown-ts-appear-mathjax--entry (key)
  "Return KEY's cached result, reading disk once when available."
  (or (gethash key markdown-ts-appear-mathjax--cache)
      (let ((file (expand-file-name (concat key ".json") markdown-ts-appear-mathjax-cache-directory)))
        (when (file-readable-p file)
          (condition-case nil
              (with-temp-buffer
                (insert-file-contents file)
                (puthash key (json-parse-buffer :object-type 'plist) markdown-ts-appear-mathjax--cache))
            (json-error nil))))))

(defun markdown-ts-appear-mathjax-render (body display callback)
  "Return cached BODY in DISPLAY style, or nil and call CALLBACK when ready.
Results are plists containing SVG geometry or an :error string. Identical
pending requests share a compile. Setup failures are not cached."
  (let* ((key (markdown-ts-appear-mathjax--key body display))
         (entry (markdown-ts-appear-mathjax--entry key)))
    (if entry entry
      (let ((waiting (gethash key markdown-ts-appear-mathjax--pending)))
        (puthash key (cons callback waiting) markdown-ts-appear-mathjax--pending)
        (unless waiting
          (let ((directory markdown-ts-appear-mathjax-cache-directory)
                (formula (concat markdown-ts-appear-mathjax-macros "\n" body "\n"))
                done timer process)
            (cl-labels
                ((finish (data &optional setup)
                   (unless done
                     (setq done t)
                     (when timer (cancel-timer timer))
                     (let ((callbacks (gethash key markdown-ts-appear-mathjax--pending)) result)
                       (remhash key markdown-ts-appear-mathjax--pending)
                        (setq result
                              (condition-case err
                                  (if (alist-get 'error data)
                                      (list :error (alist-get 'error data))
                                    (markdown-ts-appear-mathjax--normalize (alist-get 'svg data) display))
                                (error (setq setup t) (list :error (error-message-string err)))))
                        (unless setup
                          (puthash key result markdown-ts-appear-mathjax--cache)
                          (condition-case err
                              (progn
                                (make-directory directory t)
                                (let ((coding-system-for-write 'utf-8-unix))
                                  (with-temp-file (expand-file-name (concat key ".json") directory)
                                    (insert (json-serialize result)))))
                            (file-error (message "MathJax cache: %s" (error-message-string err)))))
                       (dolist (ready callbacks)
                         (condition-case err (funcall ready result)
                           (error (message "MathJax callback: %s" (error-message-string err)))))))))
              (setq timer (run-at-time
                           markdown-ts-appear-mathjax-timeout nil
                           (lambda ()
                             (if (process-live-p process)
                                 (markdown-ts-appear-mathjax--stop process "MathJax worker timed out")
                               (finish '((error . "MathJax worker timed out")) t)))))
              (condition-case err
                  (setq process (markdown-ts-appear-mathjax--send formula display #'finish))
                (error (run-at-time 0 nil #'finish (list (cons 'error (error-message-string err))) t)))))))
      nil)))

;;; Detection (blank-line bounded, outside Markdown code)

(defconst markdown-ts-appear-mathjax--environments
  '("equation" "equation*" "align" "align*" "alignat" "alignat*" "gather" "gather*"
    "multline" "multline*" "flalign" "flalign*" "displaymath" "math")
  "Math environments recognized as complete preview blocks.")

(defun markdown-ts-appear-mathjax--escaped-p (position)
  "Return non-nil when POSITION follows an odd run of backslashes."
  (save-excursion
    (goto-char position)
    (while (eq (char-before) ?\\) (backward-char))
    (cl-oddp (- position (point)))))

(defun markdown-ts-appear-mathjax--code-end (position)
  "Return the end of a Markdown code block covering POSITION, or nil."
  (when (treesit-parser-p treesit-primary-parser)
    (let ((node (treesit-node-at position treesit-primary-parser)))
      (while (and node (not (member (treesit-node-type node) '("fenced_code_block" "indented_code_block"))))
        (setq node (treesit-node-parent node)))
      (and node (treesit-node-end node)))))

(defun markdown-ts-appear-mathjax--block (position)
  "Return the blank-line-delimited range around POSITION."
  (save-excursion
    (goto-char position)
    (cons (if (re-search-backward "\n[ \t]*\n" nil t) (match-end 0) (point-min))
          (progn (goto-char position)
                 (if (re-search-forward "\n[ \t]*\n" nil t) (match-beginning 0) (point-max))))))

(defun markdown-ts-appear-mathjax--close (delimiter limit inline)
  "Find unescaped DELIMITER before LIMIT, skipping TeX comments.
INLINE applies the dollar-closing whitespace/digit checks. Return its start."
  (catch 'found
    (while (re-search-forward (concat "%\\|" (regexp-quote delimiter)) limit t)
      (let ((beg (match-beginning 0)))
        (unless (markdown-ts-appear-mathjax--escaped-p beg)
          (if (eq (char-after beg) ?%)
              (goto-char (min limit (line-end-position)))
            (when (or (not inline)
                      (and (not (memq (char-before beg) '(?\s ?\t ?\n)))
                           (not (and (char-after) (<= ?0 (char-after) ?9)))))
              (throw 'found beg))))))))

(defun markdown-ts-appear-mathjax--scan (beg end)
  "Return (KIND BEG END BODY-BEG BODY-END) records overlapping BEG..END."
  (save-match-data
    (save-excursion
      (let ((start (car (markdown-ts-appear-mathjax--block beg)))
            (limit (cdr (markdown-ts-appear-mathjax--block end))) bound records)
        (goto-char start)
        (while (re-search-forward "`+\\|\\$\\$?\\|\\\\[([]\\|\\\\begin{\\([^}\n]+\\)}" limit t)
          (let* ((open (match-string-no-properties 0)) (from (match-beginning 0))
                 (body-start (point)) (env (match-string-no-properties 1))
                 (code-end (markdown-ts-appear-mathjax--code-end from)))
            (when (or (not bound) (>= from bound))
              (setq bound (min limit (cdr (markdown-ts-appear-mathjax--block from)))))
            (cond
             (code-end (goto-char (min limit code-end)))
             ((markdown-ts-appear-mathjax--escaped-p from))
             ((eq (aref open 0) ?`)
              (let (close)
                (while (and (not close) (re-search-forward "`+" bound t))
                  (when (= (- (match-end 0) (match-beginning 0)) (length open)) (setq close (point))))
                (unless close (goto-char body-start))))
             ((and env (not (member env markdown-ts-appear-mathjax--environments))))
             ((and (equal open "$") (memq (char-after) '(nil ?\s ?\t ?\n))))
             (t
              (let* ((kind (cond (env 'environment) ((member open '("$$" "\\[")) 'display) (t 'inline)))
                     (close (cond (env (concat "\\end{" env "}"))
                                  ((equal open "\\[") "\\]") ((equal open "\\(") "\\)") (t open)))
                     (to (markdown-ts-appear-mathjax--close close bound (equal open "$"))))
                (if (not to) (goto-char body-start)
                  (when (and (< from end) (> (point) beg))
                    (push (list kind from (point) (if env from body-start) (if env (point) to)) records))))))))
        (nreverse records)))))

(defun markdown-ts-appear-mathjax-spans (beg end)
  "Return (KIND BEG END) math spans overlapping BEG..END."
  (mapcar (lambda (record) (seq-take record 3)) (markdown-ts-appear-mathjax--scan beg end)))

(defun markdown-ts-appear-mathjax-span-at (position)
  "Return the math span containing POSITION, including its closing boundary."
  (seq-find (lambda (span) (<= (nth 1 span) position (nth 2 span)))
            (markdown-ts-appear-mathjax-spans (max (point-min) (1- position))
                                             (min (point-max) (1+ position)))))

(defun markdown-ts-appear-mathjax--fontify (limit)
  "Protect math source from Markdown hiding/emphasis up to LIMIT."
  (let ((beg (point)))
    (with-silent-modifications
      (dolist (span (if (eq (car markdown-ts-appear-mathjax--fontifying-spans) (current-buffer))
                       (funcall (cdr markdown-ts-appear-mathjax--fontifying-spans))
                     (markdown-ts-appear-mathjax-spans beg limit)))
        (let ((start (max beg (nth 1 span))) (end (min limit (nth 2 span))))
          (when (< start end)
            (remove-text-properties start end '(invisible nil display nil))
            (put-text-property start end 'face 'markdown-ts-latex))))))
  (goto-char limit)
  nil)

;;; Images and reveal

(defun markdown-ts-appear-mathjax--image (ov entry)
  "Return a theme/font matched image for OV's ready ENTRY."
  (when-let* ((svg (plist-get entry :svg))
              (window (get-buffer-window (current-buffer) t))
              ((display-images-p (window-frame window))))
    (let* ((position (overlay-start ov))
           (color (save-excursion
                    (goto-char position) (font-lock-ensure position (min (point-max) (1+ position)))
                    (apply #'format "#%02x%02x%02x"
                            (mapcar (lambda (v) (/ v 257))
                                    (color-values
                                     (or (face-foreground 'markdown-ts-latex (window-frame window) t)
                                         (face-foreground 'default (window-frame window) t)))))))
           (font (or (font-at position window) (font-at 0 window "x")))
           (height (if font (aref (aref (font-get-glyphs font 0 1 "x") 0) 7)
                     (/ (frame-char-height (window-frame window)) 2.0)))
           (style (list (overlay-get ov 'markdown-ts-appear-mathjax--key) color height markdown-ts-appear-mathjax-scale)))
      (or (gethash style markdown-ts-appear-mathjax--images)
          (puthash style
                   (create-image (replace-regexp-in-string "currentColor" color svg t t) 'svg t
                                 :scale 1.0
                                 :width (max 1 (round (/ (* (plist-get entry :width) height
                                                           markdown-ts-appear-mathjax-scale) (plist-get entry :ex))))
                                 :ascent (if (< (plist-get entry :ascent) 0) 'center (plist-get entry :ascent)))
                   markdown-ts-appear-mathjax--images)))))

(defun markdown-ts-appear-mathjax--editing-p (ov)
  "Return non-nil if OV's source is currently being revealed."
  (and (overlay-buffer ov) (<= (overlay-start ov) (point) (overlay-end ov))
       (or (bound-and-true-p isearch-mode)
           (if (bound-and-true-p markdown-ts-appear-mode)
               (markdown-ts-appear--tracking-p) (not buffer-read-only)))))

(defun markdown-ts-appear-mathjax--at-point ()
  "Return this mode's overlay at point or its closing boundary."
  (or (seq-find (lambda (ov) (overlay-get ov 'markdown-ts-appear-mathjax)) (overlays-at (point)))
      (and (> (point) (point-min))
           (seq-find (lambda (ov) (and (overlay-get ov 'markdown-ts-appear-mathjax)
                                       (= (overlay-end ov) (point)))) (overlays-at (1- (point)))))))

(defun markdown-ts-appear-mathjax-show-error ()
  "Show the current formula and its complete MathJax diagnostic."
  (interactive)
  (let* ((ov (markdown-ts-appear-mathjax--at-point))
         (error (and ov (plist-get (overlay-get ov 'markdown-ts-appear-mathjax--entry) :error))))
    (unless error (user-error "No MathJax error at point"))
    (with-help-window "*MathJax error*"
      (princ error) (princ "\n\n") (princ (overlay-get ov 'markdown-ts-appear-mathjax--source)))))

(defun markdown-ts-appear-mathjax--sync (ov)
  "Synchronize OV's source/image presentation without compiling."
  (if (and markdown-ts-appear-mathjax-terminal-preview (not (display-graphic-p)))
      (markdown-ts-appear-mathjax-kitty--sync ov)
    (markdown-ts-appear-mathjax--sync-image ov)))

(defun markdown-ts-appear-mathjax--sync-image (ov)
  "Synchronize OV using the native image display."
  (when (overlay-buffer ov)
    (let* ((editing (markdown-ts-appear-mathjax--editing-p ov))
           (modified (overlay-get ov 'markdown-ts-appear-mathjax--modified))
           (image (overlay-get ov 'markdown-ts-appear-mathjax--image))
           (error (plist-get (overlay-get ov 'markdown-ts-appear-mathjax--entry) :error))
           (display (and (not editing) (not modified) image))
           (old (overlay-get ov 'display)))
      (overlay-put ov 'display display)
      (overlay-put ov 'before-string
                   (cond
                    ((and display markdown-ts-appear-mathjax-center-display
                          (not (eq (overlay-get ov 'markdown-ts-appear-mathjax--kind) 'inline)))
                     (propertize " " 'display `(space :align-to (- center (0.5 . ,image)))))
                    ((and (not editing) error (not modified)) (propertize "⚠ " 'face 'error 'help-echo error))))
      (if (and editing markdown-ts-appear-mathjax-edit-preview
               (not (eq (overlay-get ov 'markdown-ts-appear-mathjax--kind) 'inline))
               (save-excursion (goto-char (overlay-start ov)) (skip-chars-backward " \t")
                               (and (bolp) (progn (goto-char (overlay-end ov)) (skip-chars-forward " \t") (eolp)))))
          (markdown-ts-appear-mathjax--show-panel ov)
        (markdown-ts-appear-mathjax--clear-panel ov))
      (when (and display (not (eq display old)))
        (run-hook-with-args 'markdown-ts-appear-mathjax-preview-functions ov display)))))

(defun markdown-ts-appear-mathjax--request (ov)
  "Request OV's current formula, respecting source-only editing."
  (when (and (overlay-buffer ov) (not (overlay-get ov 'markdown-ts-appear-mathjax--pending))
             (get-buffer-window (current-buffer) t)
             (or (display-images-p)
                 (and markdown-ts-appear-mathjax-terminal-preview
                      (markdown-ts-appear-mathjax-kitty--available-p ov)))
             (or (not (markdown-ts-appear-mathjax--editing-p ov)) markdown-ts-appear-mathjax-edit-preview))
    (let ((buffer (current-buffer)) (token (list t))
          (source (overlay-get ov 'markdown-ts-appear-mathjax--source)))
      (overlay-put ov 'markdown-ts-appear-mathjax--pending token)
      (cl-labels
          ((ready (entry)
             (when (and (buffer-live-p buffer) (eq (overlay-buffer ov) buffer))
               (with-current-buffer buffer
                 (when (and markdown-ts-appear-mathjax-mode
                            (eq token (overlay-get ov 'markdown-ts-appear-mathjax--pending))
                            (equal source (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))
                   (overlay-put ov 'markdown-ts-appear-mathjax--pending nil)
                   (overlay-put ov 'markdown-ts-appear-mathjax--modified nil)
                   (overlay-put ov 'markdown-ts-appear-mathjax--entry entry)
                   (overlay-put ov 'markdown-ts-appear-mathjax--image (markdown-ts-appear-mathjax--image ov entry))
                   (overlay-put ov 'help-echo (or (plist-get entry :error) source))
                   (markdown-ts-appear-mathjax--sync ov)
                   (unless (overlay-get ov 'markdown-ts-appear-mathjax--image)
                     (run-hook-with-args 'markdown-ts-appear-mathjax-preview-functions ov nil)))))))
        (when-let* ((entry (markdown-ts-appear-mathjax-render
                           (overlay-get ov 'markdown-ts-appear-mathjax--body)
                           (not (eq (overlay-get ov 'markdown-ts-appear-mathjax--kind) 'inline)) #'ready)))
          (ready entry))))))

(defun markdown-ts-appear-mathjax--ensure (record &optional existing)
  "Update EXISTING or create a preview for RECORD, returning its overlay."
  (pcase-let* ((`(,kind ,beg ,end ,body-beg ,body-end) record)
               (body (buffer-substring-no-properties body-beg body-end))
               (source (buffer-substring-no-properties beg end))
               (key (markdown-ts-appear-mathjax--key body (not (eq kind 'inline))))
               (ov (or existing
                       (seq-find (lambda (ov) (and (overlay-get ov 'markdown-ts-appear-mathjax)
                                                   (= (overlay-start ov) beg))) (overlays-at beg))
                       (let ((new (make-overlay beg end nil t nil)))
                         (overlay-put new 'markdown-ts-appear-mathjax t)
                         (overlay-put new 'evaporate t)
                         (overlay-put new 'priority 1)
                         (overlay-put new 'face '(:strike-through nil :underline nil :overline nil :weight normal :slant normal :box nil))
                         (overlay-put new 'modification-hooks '(markdown-ts-appear-mathjax--modified))
                         (push new markdown-ts-appear-mathjax--objects) new))))
    (unless (and (= end (overlay-end ov)) (equal key (overlay-get ov 'markdown-ts-appear-mathjax--key)))
      (move-overlay ov beg end)
      (overlay-put ov 'markdown-ts-appear-mathjax--pending nil)
      (overlay-put ov 'markdown-ts-appear-mathjax--modified t)
      (overlay-put ov 'markdown-ts-appear-mathjax--entry nil))
    (overlay-put ov 'markdown-ts-appear-mathjax--source source)
    (overlay-put ov 'markdown-ts-appear-mathjax--body body)
    (overlay-put ov 'markdown-ts-appear-mathjax--kind kind)
    (overlay-put ov 'markdown-ts-appear-mathjax--key key)
    ov))

(defun markdown-ts-appear-mathjax--delete (ov)
  "Delete OV and its editing decoration."
  (when (overlay-get ov 'markdown-ts-appear-mathjax-kitty--view)
    (markdown-ts-appear-mathjax-kitty--clear ov))
  (markdown-ts-appear-mathjax--clear-panel ov)
  (overlay-put ov 'markdown-ts-appear-mathjax--pending nil)
  (delete-overlay ov))

(defun markdown-ts-appear-mathjax--modified (ov after &optional beg end &rest _)
  "Flag OV as edited; BEG..END and AFTER identify structural line changes."
  (when (and (overlay-get ov 'markdown-ts-appear-mathjax--rows) beg end)
    (cond
     ((and (not after) (<= beg (overlay-start ov)) (>= end (overlay-end ov)))
      (markdown-ts-appear-mathjax--clear-panel ov))
     ((save-excursion (goto-char beg) (search-forward "\n" end t))
      (overlay-put ov 'markdown-ts-appear-mathjax--rebuild t))))
  (when (and after (overlay-buffer ov))
    (overlay-put ov 'markdown-ts-appear-mathjax--modified t)
    (overlay-put ov 'markdown-ts-appear-mathjax--pending nil)
    (overlay-put ov 'markdown-ts-appear-mathjax--entry nil)
    (markdown-ts-appear-mathjax--sync ov)))

(defun markdown-ts-appear-mathjax--reconcile (&optional full recompile)
  "Reconcile changed paragraphs, or the FULL buffer, optionally forcing RECOMPILE."
  (when (and markdown-ts-appear-mathjax-mode (or full markdown-ts-appear-mathjax--dirty))
    ;; Parse first so distant fence/container changes join the pending range.
    (treesit-parser-root-node treesit-primary-parser)
    (when markdown-ts-appear-mathjax--timer (cancel-timer markdown-ts-appear-mathjax--timer))
    (setq markdown-ts-appear-mathjax--timer nil)
    (without-restriction
      (let* ((dirty markdown-ts-appear-mathjax--dirty)
             (beg (if full (point-min) (car (markdown-ts-appear-mathjax--block (car dirty)))))
             (end (if full (point-max) (cdr (markdown-ts-appear-mathjax--block (cdr dirty)))))
             (existing (make-hash-table :test #'eql)) objects)
        (when dirty (set-marker (car dirty) nil) (set-marker (cdr dirty) nil))
        (setq markdown-ts-appear-mathjax--dirty nil)
        ;; A newly inserted blank line may split an old formula across paragraphs.
        (dolist (ov (overlays-in beg end))
          (when (overlay-get ov 'markdown-ts-appear-mathjax)
            (setq beg (min beg (overlay-start ov)) end (max end (overlay-end ov)))))
        (setq beg (car (markdown-ts-appear-mathjax--block beg))
              end (cdr (markdown-ts-appear-mathjax--block end)))
        (dolist (ov markdown-ts-appear-mathjax--objects)
          (cond
           ((not (overlay-buffer ov)) (markdown-ts-appear-mathjax--delete ov))
           ((and (<= (overlay-start ov) end) (>= (overlay-end ov) beg))
            (puthash (overlay-start ov) ov existing))
           (t (push ov objects))))
        (when recompile (clrhash markdown-ts-appear-mathjax--images))
        (dolist (record (markdown-ts-appear-mathjax--scan beg end))
          (let* ((ov (markdown-ts-appear-mathjax--ensure record (gethash (nth 1 record) existing)))
                 (key (overlay-get ov 'markdown-ts-appear-mathjax--key)))
            (remhash (nth 1 record) existing)
            (push ov objects)
            (when recompile
              (remhash key markdown-ts-appear-mathjax--cache)
              (let ((file (expand-file-name (concat key ".json") markdown-ts-appear-mathjax-cache-directory)))
                (when (file-exists-p file) (delete-file file)))
              (overlay-put ov 'markdown-ts-appear-mathjax--entry nil)
              (overlay-put ov 'markdown-ts-appear-mathjax--pending nil))
            (if-let* ((entry (overlay-get ov 'markdown-ts-appear-mathjax--entry)))
                (overlay-put ov 'markdown-ts-appear-mathjax--image (markdown-ts-appear-mathjax--image ov entry))
              (markdown-ts-appear-mathjax--request ov))
            (markdown-ts-appear-mathjax--sync ov)))
        (maphash (lambda (_ ov)
                   (if (and (markdown-ts-appear-mathjax--editing-p ov)
                            (overlay-get ov 'markdown-ts-appear-mathjax--modified))
                       (push ov objects)
                     (markdown-ts-appear-mathjax--delete ov))) existing)
        (setq markdown-ts-appear-mathjax--objects (nreverse objects))))))

(defun markdown-ts-appear-mathjax-refresh (&optional recompile)
  "Reconcile all math previews in the current buffer.
With prefix RECOMPILE, invalidate this buffer's cached formulas first."
  (interactive "P")
  (markdown-ts-appear-mathjax--reconcile t recompile))

(defun markdown-ts-appear-mathjax--redisplay ()
  "Update preview presentation without rescanning unchanged source."
  (when markdown-ts-appear-mathjax-mode
    (markdown-ts-appear-mathjax--reconcile)
    (without-restriction
      (dolist (ov markdown-ts-appear-mathjax--objects)
        (if-let* ((entry (overlay-get ov 'markdown-ts-appear-mathjax--entry)))
            (overlay-put ov 'markdown-ts-appear-mathjax--image (markdown-ts-appear-mathjax--image ov entry))
          (markdown-ts-appear-mathjax--request ov))
        (markdown-ts-appear-mathjax--sync ov)))))

(defun markdown-ts-appear-mathjax--post-command ()
  "Reveal at point and render the formula just left, without full-buffer scans."
  (when markdown-ts-appear-mathjax-mode
    (let ((old markdown-ts-appear-mathjax--current)
          (current (markdown-ts-appear-mathjax--at-point)))
      (when (and (not current) markdown-ts-appear-mathjax--dirty)
        (when-let* ((record (seq-find (lambda (record) (<= (nth 1 record) (point) (nth 2 record)))
                                     (markdown-ts-appear-mathjax--scan
                                      (max (point-min) (1- (point))) (min (point-max) (1+ (point)))))))
          (setq current (markdown-ts-appear-mathjax--ensure record))))
      (dolist (ov (delete-dups (delq nil (list old current))))
        (when (overlay-buffer ov)
          (when (and (not (markdown-ts-appear-mathjax--editing-p ov))
                     (or (overlay-get ov 'markdown-ts-appear-mathjax--modified)
                         (not (overlay-get ov 'markdown-ts-appear-mathjax--entry))))
            (let ((record (seq-find (lambda (record) (= (nth 1 record) (overlay-start ov)))
                                    (markdown-ts-appear-mathjax--scan (overlay-start ov) (overlay-end ov)))))
              (if record (markdown-ts-appear-mathjax--request (markdown-ts-appear-mathjax--ensure record))
                (markdown-ts-appear-mathjax--delete ov))))
          (markdown-ts-appear-mathjax--sync ov)))
      (setq markdown-ts-appear-mathjax--current current))))

(defun markdown-ts-appear-mathjax--changed (beg end &rest _)
  "Queue source changes between BEG and END for idle reconciliation."
  (setq beg (max (point-min) (1- beg)) end (min (point-max) (1+ end)))
  (if (consp markdown-ts-appear-mathjax--dirty)
      (progn
        (set-marker (car markdown-ts-appear-mathjax--dirty) (min beg (car markdown-ts-appear-mathjax--dirty)))
        (set-marker (cdr markdown-ts-appear-mathjax--dirty) (max end (cdr markdown-ts-appear-mathjax--dirty))))
    (setq markdown-ts-appear-mathjax--dirty (cons (copy-marker beg) (copy-marker end t))))
  (when markdown-ts-appear-mathjax--timer (cancel-timer markdown-ts-appear-mathjax--timer))
  (let ((buffer (current-buffer)))
    (setq markdown-ts-appear-mathjax--timer
          (run-with-idle-timer markdown-ts-appear-mathjax-delay nil
                              (lambda () (when (buffer-live-p buffer)
                                            (with-current-buffer buffer (markdown-ts-appear-mathjax--reconcile))))))))

(defun markdown-ts-appear-mathjax--syntax-changed (ranges _parser)
  "Include RANGES changed by Markdown structure in the next reconciliation."
  (when markdown-ts-appear-mathjax-mode
    (without-restriction
      (dolist (range ranges)
        (markdown-ts-appear-mathjax--changed (car range) (cdr range))))))

(defun markdown-ts-appear-mathjax--appearance (&rest _)
  "Refresh active buffers from cached SVG after theme or font changes."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when markdown-ts-appear-mathjax-mode
        (mapc #'markdown-ts-appear-mathjax--clear-panel markdown-ts-appear-mathjax--objects)
        (markdown-ts-appear-mathjax--redisplay)))))

(defun markdown-ts-appear-mathjax--window (window)
  "Refresh a buffer newly displayed in WINDOW."
  (with-current-buffer (window-buffer window)
    (when markdown-ts-appear-mathjax-mode (markdown-ts-appear-mathjax--redisplay))))

;;; Optional source/preview frame

(defun markdown-ts-appear-mathjax--clear-panel (ov)
  "Remove the frame owned by OV."
  (when (overlay-get ov 'markdown-ts-appear-mathjax--rows)
    (mapc #'delete-overlay (overlay-get ov 'markdown-ts-appear-mathjax--rows))
    (dolist (property '(markdown-ts-appear-mathjax--rows markdown-ts-appear-mathjax--last-row
                       markdown-ts-appear-mathjax--preview-body markdown-ts-appear-mathjax--status
                       markdown-ts-appear-mathjax--shown-image markdown-ts-appear-mathjax--rebuild))
      (overlay-put ov property nil))))

(defun markdown-ts-appear-mathjax--source-rows (ov)
  "Create OV's source edges only initially or after newline changes."
  (when (or (not (overlay-get ov 'markdown-ts-appear-mathjax--rows))
            (overlay-get ov 'markdown-ts-appear-mathjax--rebuild))
    (mapc #'delete-overlay (overlay-get ov 'markdown-ts-appear-mathjax--rows))
    (let* ((box (face-attribute 'markdown-ts-appear-mathjax-preview :box nil t))
           (color (or (and (listp box) (plist-get box :color)) (and (stringp box) box)
                      (face-foreground 'shadow nil t)))
           (bar (propertize " " 'display '(space :width (1)) 'face `(:background ,color)))
           (tail (concat (propertize " " 'cursor t)
                         (propertize " " 'display '(space :align-to (- right (1)))) bar))
           (end (overlay-end ov)) rows)
      (save-excursion
        (goto-char (overlay-start ov)) (beginning-of-line)
        (let ((first t) (more t))
          (while more
            (let* ((beg (point)) (eol (line-end-position)) (row (make-overlay beg eol nil nil t))
                   (face (if first `((:overline ,color) markdown-ts-appear-mathjax-source)
                           'markdown-ts-appear-mathjax-source))
                   (prefix (or (get-char-property beg 'line-prefix) line-prefix))
                   (wrap (or (get-char-property beg 'wrap-prefix) wrap-prefix))
                   (border (concat bar " ")))
              (overlay-put row 'markdown-ts-appear-mathjax--row t)
              (overlay-put row 'priority 2) (overlay-put row 'face face)
              (add-face-text-property 0 (length border) face t border)
              (dolist (pair `((line-prefix . ,prefix) (wrap-prefix . ,wrap)))
                (overlay-put row (car pair)
                             (if (not (cdr pair)) border
                               (concat (if (stringp (cdr pair)) (cdr pair)
                                         (propertize " " 'display (cdr pair))) border))))
              (let ((edge (if (= beg eol) (propertize (concat bar " " tail) 'line-prefix "" 'wrap-prefix "")
                            (copy-sequence tail))))
                (add-face-text-property 0 (length edge) face t edge)
                (overlay-put row 'markdown-ts-appear-mathjax--tail edge)
                (overlay-put row 'after-string edge))
              (push row rows) (setq first nil)
              (if (>= eol end) (setq more nil) (forward-line 1))))))
      (overlay-put ov 'markdown-ts-appear-mathjax--last-row (car rows))
      (overlay-put ov 'markdown-ts-appear-mathjax--rows (nreverse rows))
      (overlay-put ov 'markdown-ts-appear-mathjax--rebuild nil))))

(defun markdown-ts-appear-mathjax--show-panel (ov)
  "Update OV's frame only when its image or status changes."
  (markdown-ts-appear-mathjax--source-rows ov)
  (let* ((image (overlay-get ov 'markdown-ts-appear-mathjax--image))
         (error (and (not (overlay-get ov 'markdown-ts-appear-mathjax--modified))
                     (plist-get (overlay-get ov 'markdown-ts-appear-mathjax--entry) :error)))
         (status (cond (image 'image) (error 'error) (t 'pending))))
    (unless (and (eq image (overlay-get ov 'markdown-ts-appear-mathjax--shown-image))
                 (eq status (overlay-get ov 'markdown-ts-appear-mathjax--status)))
      (let* ((content
              (cond
               (image (let ((padded (copy-sequence image)))
                        (setf (image-property padded :margin) '(0 . 8))
                        (concat (propertize " " 'display `(space :align-to (- center (0.5 . ,padded))))
                                (propertize " " 'display padded))))
               (error (concat (propertize (truncate-string-to-width
                                          (replace-regexp-in-string "[\n\r\t]+" " " error) 72 nil nil "…") 'face 'error)
                              "  " (propertize
                                     (buttonize "Details" (lambda (_)
                                                           (when-let* ((buffer (overlay-buffer ov)))
                                                             (with-current-buffer buffer
                                                               (save-excursion (goto-char (overlay-start ov))
                                                                               (markdown-ts-appear-mathjax-show-error))))))
                                     'face 'link)))
               (t (propertize "Waiting for a renderable formula…" 'face 'shadow))))
             (line (concat " " content (propertize " " 'display '(space :align-to (- right 1))) " ")))
        (add-face-text-property 0 (length line) 'markdown-ts-appear-mathjax-preview t line)
        (overlay-put ov 'markdown-ts-appear-mathjax--preview-body
                     (propertize
                      (concat (propertize "\n" 'face '(:inherit default :box nil :extend t)) line
                              (and (= (save-excursion (goto-char (overlay-end ov)) (line-end-position)) (point-max)) "\n"))
                      'line-prefix "" 'wrap-prefix ""
                      'help-echo (lambda (_window _object _position)
                                   (or error (and image (overlay-get ov 'markdown-ts-appear-mathjax--modified)
                                                  "Previous preview; waiting for the edited formula"))))))
      (overlay-put ov 'markdown-ts-appear-mathjax--status status)
      (overlay-put ov 'markdown-ts-appear-mathjax--shown-image image)))
  (let* ((row (overlay-get ov 'markdown-ts-appear-mathjax--last-row))
         (body (overlay-get ov 'markdown-ts-appear-mathjax--preview-body)))
    (unless (eq body (overlay-get row 'markdown-ts-appear-mathjax--preview-body))
      (overlay-put row 'after-string (propertize (concat (overlay-get row 'markdown-ts-appear-mathjax--tail) body)
                                               'help-echo (get-text-property 0 'help-echo body)))
      (overlay-put row 'markdown-ts-appear-mathjax--preview-body body))))

;;; Mode lifecycle

(defun markdown-ts-appear-mathjax--clone ()
  "Detach inherited preview state without deleting the base buffer's objects."
  (setq markdown-ts-appear-mathjax--objects nil markdown-ts-appear-mathjax--current nil
        markdown-ts-appear-mathjax--timer nil markdown-ts-appear-mathjax--dirty nil)
  (dolist (ov (overlays-in (point-min) (point-max)))
    (when (or (overlay-get ov 'markdown-ts-appear-mathjax) (overlay-get ov 'markdown-ts-appear-mathjax--row))
      (delete-overlay ov)))
  (markdown-ts-appear-mathjax-refresh))

;;;###autoload
(define-minor-mode markdown-ts-appear-mathjax-mode
  "Preview Markdown math with MathJax, independently of other math packages."
  :lighter nil
  :group 'markdown-ts-appear-mathjax
  (if markdown-ts-appear-mathjax-mode
      (progn
        (unless (and (derived-mode-p 'markdown-ts-mode)
                     (require 'mathjax nil t) (mathjax-available-p))
          (setq markdown-ts-appear-mathjax-mode nil)
          (user-error "MathJax previews require markdown-ts-mode, the mathjax package and Node.js"))
        (when markdown-ts-appear-mathjax-terminal-preview
          (require 'markdown-ts-appear-mathjax-kitty))
        (font-lock-add-keywords nil '((markdown-ts-appear-mathjax--fontify)) t)
        (dolist (property '(invisible display))
          (unless (memq property font-lock-extra-managed-props)
            (push property font-lock-extra-managed-props)
            (push property markdown-ts-appear-mathjax--managed)))
        (add-hook 'post-command-hook #'markdown-ts-appear-mathjax--post-command 90 t)
        (add-hook 'markdown-ts-appear--tracking-change-hook #'markdown-ts-appear-mathjax--post-command 90 t)
        (add-hook 'after-change-functions #'markdown-ts-appear-mathjax--changed 90 t)
        (when (eq (treesit-parser-buffer treesit-primary-parser) (current-buffer))
          (treesit-parser-add-notifier treesit-primary-parser #'markdown-ts-appear-mathjax--syntax-changed))
        (add-hook 'text-scale-mode-hook #'markdown-ts-appear-mathjax--redisplay nil t)
        (add-hook 'window-buffer-change-functions #'markdown-ts-appear-mathjax--window nil t)
        (add-hook 'clone-indirect-buffer-hook #'markdown-ts-appear-mathjax--clone -90 t)
        (add-hook 'change-major-mode-hook #'markdown-ts-appear-mathjax--disable nil t)
        (add-hook 'kill-buffer-hook #'markdown-ts-appear-mathjax--disable nil t)
        (dolist (hook '(enable-theme-functions disable-theme-functions after-setting-font-hook))
          (add-hook hook #'markdown-ts-appear-mathjax--appearance))
        (font-lock-flush)
        (markdown-ts-appear-mathjax-refresh)
        (markdown-ts-appear-mathjax--post-command))
    (when markdown-ts-appear-mathjax--timer (cancel-timer markdown-ts-appear-mathjax--timer))
    (mapc #'markdown-ts-appear-mathjax--delete markdown-ts-appear-mathjax--objects)
    (setq markdown-ts-appear-mathjax--objects nil markdown-ts-appear-mathjax--current nil
          markdown-ts-appear-mathjax--timer nil)
    (when (consp markdown-ts-appear-mathjax--dirty)
      (set-marker (car markdown-ts-appear-mathjax--dirty) nil)
      (set-marker (cdr markdown-ts-appear-mathjax--dirty) nil))
    (setq markdown-ts-appear-mathjax--dirty nil)
    (font-lock-remove-keywords nil '((markdown-ts-appear-mathjax--fontify)))
    (dolist (property markdown-ts-appear-mathjax--managed)
      (setq font-lock-extra-managed-props (delq property font-lock-extra-managed-props)))
    (setq markdown-ts-appear-mathjax--managed nil)
    (remove-hook 'post-command-hook #'markdown-ts-appear-mathjax--post-command t)
    (remove-hook 'post-command-hook #'markdown-ts-appear-mathjax-kitty--repaint t)
    (remove-hook 'markdown-ts-appear--tracking-change-hook #'markdown-ts-appear-mathjax--post-command t)
    (remove-hook 'after-change-functions #'markdown-ts-appear-mathjax--changed t)
    (when (and (treesit-parser-p treesit-primary-parser)
               (eq (treesit-parser-buffer treesit-primary-parser) (current-buffer)))
      (treesit-parser-remove-notifier treesit-primary-parser #'markdown-ts-appear-mathjax--syntax-changed))
    (remove-hook 'text-scale-mode-hook #'markdown-ts-appear-mathjax--redisplay t)
    (remove-hook 'window-buffer-change-functions #'markdown-ts-appear-mathjax--window t)
    (remove-hook 'clone-indirect-buffer-hook #'markdown-ts-appear-mathjax--clone t)
    (remove-hook 'change-major-mode-hook #'markdown-ts-appear-mathjax--disable t)
    (remove-hook 'kill-buffer-hook #'markdown-ts-appear-mathjax--disable t)
    (unless (seq-some (lambda (buffer) (buffer-local-value 'markdown-ts-appear-mathjax-mode buffer)) (buffer-list))
      (dolist (hook '(enable-theme-functions disable-theme-functions after-setting-font-hook))
        (remove-hook hook #'markdown-ts-appear-mathjax--appearance)))
    (font-lock-flush)))

(defun markdown-ts-appear-mathjax--disable ()
  "Disable previews before this buffer is discarded or changes major mode."
  (markdown-ts-appear-mathjax-mode -1))

(provide 'markdown-ts-appear-mathjax)
;;; markdown-ts-appear-mathjax.el ends here
