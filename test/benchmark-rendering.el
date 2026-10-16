;;; benchmark-rendering.el --- Source tracking and invalidation costs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Initialize package.el, then select the checkout with -L as in
;; benchmark-table.el. Use equally byte-compiled checkouts for comparisons.
;; Timings include garbage collection. The math case isolates overlay cleanup;
;; it does not invoke MathJax. The image case uses the bundled local SVG.
;; Diagram discovery requires mmdc and SVG support but does not run the CLI.

;;; Code:
(require 'benchmark)
(require 'cl-lib)
(require 'markdown-ts-appear)
(setq default-directory (file-name-directory (locate-library "markdown-ts-appear")))
(princ (format "Library: %s\n" (symbol-file 'markdown-ts-appear-mode)))

(with-temp-buffer
  (insert "Ordinary prose with [**a nested link**](target) and some more text.\n")
  (markdown-ts-mode)
  (goto-char (point-max))
  (markdown-ts-appear-mode 1)
  (font-lock-ensure)
  (unwind-protect
      (let ((original (symbol-function 'treesit-node-at)) (calls 0))
        (garbage-collect)
        (princ (format "prose-motion/10000=%S\n"
                       (benchmark-run 1
                         (dotimes (n 10000)
                           (goto-char (+ 2 (% n 12)))
                           (markdown-ts-appear--update)))))
        (cl-letf (((symbol-function 'treesit-node-at)
                   (lambda (&rest args) (cl-incf calls) (apply original args))))
          (dotimes (n 100)
            (goto-char (+ 2 (% n 12)))
            (markdown-ts-appear--update)))
        (princ (format "node-lookups/100-commands=%d\n" calls)))
    (markdown-ts-appear-mode -1)))

(dolist (n '(10 100))
  (with-temp-buffer
    (insert (make-string (+ 10 (* 4 n)) ?x))
    (dotimes (i n)
      (let ((ov (make-overlay (+ 2 (* 4 i)) (+ 5 (* 4 i)))))
        (overlay-put ov 'markdown-ts-appear-math--source "$x$")
        (push ov markdown-ts-appear-math--objects)))
    (garbage-collect)
    (princ (format "math=%d unrelated-edit/1000=%S\n" n
                   (benchmark-run 1000
                     (markdown-ts-appear-math--clear (point-max) (point-max)))))
    (princ (format "math=%d teardown=%S\n" n
                   (benchmark-run 1 (markdown-ts-appear-math--clear))))))

(dolist (n '(10 100))
  (with-temp-buffer
    (insert "Ordinary prose.\n\n")
    (dotimes (_ n) (insert "![demo](examples/demo.svg)\n\n"))
    (let ((markdown-ts-inline-images t)
          (markdown-ts-appear-enable-math-preview nil))
      (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t)))
        (markdown-ts-mode)
        (goto-char (point-min))
        (markdown-ts-appear-mode 1)
        (font-lock-ensure)
        (unwind-protect
            (let ((checks 0) (original (symbol-function 'file-attributes)))
              (cl-letf (((symbol-function 'file-attributes)
                         (lambda (&rest args) (cl-incf checks) (apply original args))))
                (princ (format "images=%d prose-edit/10=%S\n" n
                               (benchmark-run 10
                                 (goto-char 3) (insert "x")
                                 (run-hooks 'post-command-hook)))))
              (princ (format "images=%d file-checks=%d\n" n checks)))
          (markdown-ts-appear-mode -1))))))

;; Buried buffers exercise diagram discovery without starting the CLI.
(dolist (n '(10 100))
  (with-temp-buffer
    (insert "Ordinary prose.\n\n")
    (dotimes (_ n) (insert "```mermaid\ngraph TD; A-->B\n```\n\n"))
    (let ((markdown-ts-appear-diagram-renderers '(("mermaid" . markdown-ts-appear-diagram-mmdc)))
          (scans 0) (scan (symbol-function 'markdown-ts-appear-diagram--scan)))
      (markdown-ts-mode)
      (goto-char (point-min))
      (markdown-ts-appear-mode 1)
      (unwind-protect
          (cl-letf (((symbol-function 'markdown-ts-appear-diagram--scan)
                     (lambda (&rest args) (cl-incf scans) (apply scan args))))
            (princ (format "diagrams=%d prose-edit/10=%S " n
                           (benchmark-run 10
                             (goto-char 3) (insert "x")
                             (run-hooks 'post-command-hook))))
            (princ (format "scans=%d\n" scans)))
        (markdown-ts-appear-mode -1)))))

;;; benchmark-rendering.el ends here
