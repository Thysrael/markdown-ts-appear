;;; try-kitty-math.el --- Interactive terminal math demo -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Thysrael
;; SPDX-License-Identifier: GPL-3.0-or-later
;; In Kitty: emacs -nw -Q -L /path/to/kitty-graphics.el -l test/try-kitty-math.el

(setq load-prefer-newer t native-comp-jit-compilation nil)
(require 'package)
(package-initialize)
(add-to-list 'load-path (expand-file-name ".." (file-name-directory load-file-name)))
(require 'markdown-ts-appear)
(require 'markdown-ts-appear-mathjax-kitty)
(kitty-graphics-mode 1)
(setopt markdown-ts-appear-enable-math-preview t
        markdown-ts-appear-math-renderer 'mathjax
        markdown-ts-appear-mathjax-terminal-preview t
        markdown-ts-appear-mathjax-edit-preview t)
(switch-to-buffer (generate-new-buffer "*Kitty math demo*"))
(insert "# Kitty math preview\n\n"
        "Move into a formula to edit; keep its following blank line.\n\n"
        "$$\n\\sigma(x) = \\frac{1}{1+e^{-x}}\n$$\n\n"
        "$$\n\\int_0^1 x^2\\,dx = \\frac{1}{3}\n$$\n\n"
        "Inline $e^{i\\pi}+1=0$ and $a^2+b^2=c^2$ should stay on this line.\n\n")
(dotimes (i 50) (insert (format "Scroll test line %d\n" (1+ i))))
(markdown-ts-mode)
(goto-char (point-min))
(markdown-ts-appear-mode 1)
(set-buffer-modified-p nil)
