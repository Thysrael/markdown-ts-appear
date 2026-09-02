;;; markdown-ts-appear-test.el --- Tests for markdown-ts-appear -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (file-name-directory
              (directory-file-name
               (file-name-directory (or load-file-name buffer-file-name)))))
(require 'markdown-ts-appear)

(defmacro markdown-ts-appear-test--with-buffer (content &rest body)
  "Create a Markdown buffer containing CONTENT and evaluate BODY."
  (declare (indent 1) (debug t))
  `(progn
     (skip-unless (treesit-ready-p '(markdown markdown-inline)))
     (with-temp-buffer
       (insert ,content)
       (markdown-ts-mode)
       (let ((markdown-ts-appear-enable-math-preview nil)
             (markdown-ts-appear-trigger 'always))
         (markdown-ts-appear-mode 1)
         (font-lock-ensure)
         ,@body))))

(ert-deftest markdown-ts-appear-test-reveal-and-restore ()
  (markdown-ts-appear-test--with-buffer "**bold** plain\n"
    (goto-char 3)
    (markdown-ts-appear--update)
    (should-not (get-text-property (point-min) 'invisible))
    (goto-char 12)
    (markdown-ts-appear--update)
    (should (get-text-property (point-min) 'invisible))))

(ert-deftest markdown-ts-appear-test-update-cache ()
  (markdown-ts-appear-test--with-buffer "**bold**\n"
    (goto-char 3)
    (setq markdown-ts-appear--last-point nil)
    (setq markdown-ts-appear--last-tick nil)
    (let ((calls 0)
          (original (symbol-function 'markdown-ts-appear--bounds)))
      (cl-letf (((symbol-function 'markdown-ts-appear--bounds)
                 (lambda ()
                   (setq calls (1+ calls))
                   (funcall original))))
        (markdown-ts-appear--update)
        (markdown-ts-appear--update)
        (should (= calls 1))
        (insert "x")
        (markdown-ts-appear--update)
        (should (= calls 2))))))

(ert-deftest markdown-ts-appear-test-skips-code-blocks ()
  (markdown-ts-appear-test--with-buffer "```elisp\n(message \"hi\")\n```\n"
    (goto-char (point-min))
    (search-forward "message")
    (should (markdown-ts-at-code-block-p))
    (setq markdown-ts-appear--last-point nil)
    (setq markdown-ts-appear--last-tick nil)
    (let ((calls 0))
      (cl-letf (((symbol-function 'markdown-ts-appear--bounds)
                 (lambda ()
                   (setq calls (1+ calls))
                   nil)))
        (markdown-ts-appear--update)
        (should (zerop calls))))))

(ert-deftest markdown-ts-appear-test-restores-hide-markup-setting ()
  (markdown-ts-appear-test--with-buffer "**bold**\n"
    (should markdown-ts-hide-markup)
    (markdown-ts-appear-mode -1)
    (should-not markdown-ts-hide-markup)))

(ert-deftest markdown-ts-appear-test-caches-math-image ()
  (clrhash markdown-ts-appear--math-cache)
  (let ((calls 0))
    (cl-letf (((symbol-function 'markdown-ts-appear--math-image)
               (lambda (_svg)
                 (setq calls (1+ calls))
                 (list 'image calls))))
      (markdown-ts-appear--math-finish-render
       'test-key '((svg . "<svg height=\"1\"></svg>")))
      (let ((data (gethash 'test-key markdown-ts-appear--math-cache)))
        (should (= calls 1))
        (should (equal (alist-get 'markdown-ts-appear--math-image data)
                       '(image 1)))))))

(provide 'markdown-ts-appear-test)

;;; markdown-ts-appear-test.el ends here
