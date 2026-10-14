# markdown-ts-appear

<p align="center"><strong>Rendered while reading. Source when editing.</strong></p>

<p align="center">
<a href="https://melpa.org/#/markdown-ts-appear">
  <img alt="MELPA" src="https://img.shields.io/badge/dynamic/xml?url=https%3A%2F%2Fmelpa.org%2Fpackages%2Fmarkdown-ts-appear-badge.svg&amp;query=%2F%2F%2A%5Blocal-name()%3D%27text%27%5D%5Blast()%5D&amp;label=MELPA&amp;style=for-the-badge&amp;logo=gnuemacs&amp;logoColor=white&amp;color=922793"/>
</a>
<a href="https://github.com/Thysrael/markdown-ts-appear/actions/workflows/ci.yml"><img src="https://img.shields.io/github/actions/workflow/status/Thysrael/markdown-ts-appear/ci.yml?branch=main&amp;style=for-the-badge&amp;logo=githubactions&amp;logoColor=white&amp;label=build" alt="Build status"></a>
<a href="https://github.com/Thysrael/markdown-ts-appear/releases/latest"><img src="https://img.shields.io/github/v/release/Thysrael/markdown-ts-appear?display_name=tag&amp;sort=semver&amp;style=for-the-badge&amp;logo=github&amp;logoColor=white" alt="Latest release"></a>
<a href="https://www.gnu.org/software/emacs/"><img src="https://img.shields.io/badge/Emacs-31.1%2B-7F5AB6?style=for-the-badge&amp;logo=gnuemacs&amp;logoColor=white" alt="Emacs 31.1+"></a>
<a href="COPYING"><img src="https://img.shields.io/badge/license-GPL--3.0-green.svg?style=for-the-badge&amp;logo=opensourceinitiative&amp;logoColor=white" alt="GPL-3.0 license"></a>
</p>

![Markdown TS Appear demonstration](examples/appear.gif)

Can plain text keep its source while feeling almost rendered?

Reveal the Markdown source of the smallest element at point, then restore its
rendered appearance when point leaves.  This minor mode extends the built-in
`markdown-ts-mode` without changing buffer text.

Optional features include link icons, code fences, quote markers, callout
labels, width-aware table views, and LaTeX and Mermaid previews.

## Requirements

- Emacs 31.1+ with Tree-sitter support.
- The `markdown` and `markdown-inline` grammars.
- Math (optional): `latex-to-svg-for-markdown` from MELPA, SVG support, and a
  [supported rendering engine](https://github.com/alberti42/latex-to-svg#engine).
- Mermaid (optional): [Mermaid CLI](https://github.com/mermaid-js/mermaid-cli)
  (`mmdc`) with its browser runtime, and SVG image support.

## Installation

`markdown-ts-appear` is available from [MELPA](https://melpa.org/#/markdown-ts-appear).
After [configuring the archive](https://melpa.org/#/getting-started), use:

```emacs-lisp
(use-package markdown-ts-appear
  :ensure t
  :hook (markdown-ts-mode . markdown-ts-appear-mode))
```

Or install the latest Git revision:

```emacs-lisp
(use-package markdown-ts-appear
  :vc (markdown-ts-appear
       :url "https://github.com/Thysrael/markdown-ts-appear"
       :rev :newest)
  :hook (markdown-ts-mode . markdown-ts-appear-mode))
```

## Optional previews

Math and Mermaid previews are disabled by default. Both require an Emacs build
with SVG image support.

### Math

Install the Markdown adaptor from MELPA, then enable previews:

```emacs-lisp
(use-package latex-to-svg-for-markdown
  :ensure t
  :defer t
  :custom
  (markdown-ts-appear-enable-math-preview t)
  (latex-to-svg-frontend-center-display-math t))
```

Install the selected engine separately: LaTeX + dvisvgm (default), RaTeX, or
texres + pdftocairo. See [upstream setup](https://github.com/alberti42/latex-to-svg#engine).
MELPA Stable's v0.21.0 lacks the required integration APIs.

### Mermaid

Install [Mermaid CLI](https://github.com/mermaid-js/mermaid-cli) and its browser
runtime, then enable:

```emacs-lisp
(setopt markdown-ts-appear-enable-mermaid-preview t)
```

Ensure `mmdc` can render a diagram on its own.  Puppeteer's
`PUPPETEER_EXECUTABLE_PATH` environment variable can select an existing Chrome
installation.  No additional Emacs package is required.

## Configuration

Use `setopt` or Customize for options, and `custom-set-faces` or Customize for
faces.  See `C-h v` and `M-x describe-face` for full documentation.

### Options

| Option | Default | Values and effect |
| --- | --- | --- |
| `markdown-ts-appear-link-icon` | `""` | String before ordinary links; empty disables it. |
| `markdown-ts-appear-image-icon` | `""` | String before image links; empty disables it. |
| `markdown-ts-appear-wikilink-icon` | `"◆"` | String before Wiki links; empty disables it. |
| `markdown-ts-appear-code-fence-style` | `connected` | `raw` or `connected` fences with a language label. |
| `markdown-ts-appear-block-quote-marker` | `"▎"` | Replacement string; `nil` keeps the original marker. |
| `markdown-ts-appear-render-callouts` | `t` | Show callout labels and faint backgrounds on the header line only. |
| `markdown-ts-appear-table-style` | `wrapped` | `raw`, `unicode`, or width-aware `wrapped` tables. |
| `markdown-ts-appear-table-zebra-stripe` | `t` | Shade every second data row in `wrapped` tables. |
| `markdown-ts-appear-table-wrap-resize-delay` | `0.2` | Seconds to debounce `wrapped` table resize rendering. |
| `markdown-ts-appear-enable-math-preview` | `nil` | `t` enables latex-to-svg math previews. |
| `latex-to-svg-frontend-engine` | `latex` | Upstream engine choice: `latex`, `ratex`, or `texres`. |
| `markdown-ts-appear-enable-mermaid-preview` | `nil` | `t` previews unedited `mermaid` fenced blocks. |
| `markdown-ts-appear-mermaid-command` | `"mmdc"` | Mermaid CLI executable. |
| `markdown-ts-appear-mermaid-size` | `source` | `source`: fit source height; positive integer: limit only width, in pixels. |

Visual options applied through `setopt` or Customize update active buffers.
Re-enable `markdown-ts-appear-mode` after changing either preview-enable option.

### Faces

| Face | Purpose |
| --- | --- |
| `markdown-ts-appear-code-fence-marker` | Code block borders and language markers. |
| `markdown-ts-appear-label` | Inverse-video style for code language and callout labels. |
| `markdown-ts-appear-block-quote-marker` | Block quote markers. |
| `markdown-ts-appear-table` | Base font and appearance of wrapped tables; set its font for monospaced alignment. |
| `markdown-ts-appear-table-header` | Wrapped table header cells; bold by default. |
| `markdown-ts-appear-table-border` | Wrapped table borders, independent of row shading. |
| `markdown-ts-appear-table-zebra` | Alternating data-cell backgrounds in wrapped tables. |

Other colors and fonts follow `markdown-ts-mode` and the current theme.

## Modal Editing

By default, source is revealed whenever point enters an element.  To reveal
only in insert state, use one of these configurations.  State hooks are local
to each buffer and are removed when appear mode is disabled.

### Evil

```emacs-lisp
(defun my/markdown-appear-evil ()
  "Reveal Markdown source only in Evil insert state."
  (if markdown-ts-appear-mode
      (progn
        (add-hook 'evil-insert-state-entry-hook #'markdown-ts-appear-start nil t)
        (add-hook 'evil-insert-state-exit-hook #'markdown-ts-appear-stop nil t)
        (unless (eq (bound-and-true-p evil-state) 'insert)
          (markdown-ts-appear-stop)))
    (remove-hook 'evil-insert-state-entry-hook #'markdown-ts-appear-start t)
    (remove-hook 'evil-insert-state-exit-hook #'markdown-ts-appear-stop t)))

(add-hook 'markdown-ts-appear-mode-hook #'my/markdown-appear-evil)
```

### Meow

```emacs-lisp
(defun my/markdown-appear-meow ()
  "Reveal Markdown source only in Meow insert state."
  (if markdown-ts-appear-mode
      (progn
        (add-hook 'meow-insert-enter-hook #'markdown-ts-appear-start nil t)
        (add-hook 'meow-insert-exit-hook #'markdown-ts-appear-stop nil t)
        (unless (bound-and-true-p meow-insert-mode)
          (markdown-ts-appear-stop)))
    (remove-hook 'meow-insert-enter-hook #'markdown-ts-appear-start t)
    (remove-hook 'meow-insert-exit-hook #'markdown-ts-appear-stop t)))

(add-hook 'markdown-ts-appear-mode-hook #'my/markdown-appear-meow)
```

## Behavior Notes

- Enabling appear mode turns on `markdown-ts-hide-markup`.  Disabling it
  removes the buffer-local override and uses the current default value.
- Indirect buffers are unsupported; clones are detached from active tracking.
- Wrapped-table navigation skips hidden source characters. Revealing the row
  restores access to all of its source.
- Math detection, rendering, numbering, references and caching belong to
  `latex-to-svg`. Appear coordinates source reveal and restores the previous
  reveal setting on disable; independently enabled adaptors stay running.
- Mermaid renders visible, unedited blocks asynchronously, one job at a time.
  Identical diagrams share cached images; failed diagrams keep their source,
  with details available on hover.  Diagrams are only scaled down, preserving
  their aspect ratio, to fit the code block's displayed height by default, or
  the configured maximum width.  Editing shows the code at its natural height
  without padding.
- If preview setup fails, disable appear mode, fix the missing dependency, then
  re-enable it.

## Development

```sh
MARKDOWN_TS_APPEAR_REQUIRE_GRAMMARS=1 \
emacs --batch -Q \
  -l test/run-tests.el
```

Add `MARKDOWN_TS_APPEAR_REQUIRE_LATEX_TO_SVG=1` to require the optional math
packages. The real math rendering test needs SVG support and LaTeX + dvisvgm. Use
`MARKDOWN_TS_APPEAR_REQUIRE_MERMAID=1` to require the Mermaid CLI tests.
Without the corresponding
environment variable, tests with unavailable dependencies are skipped.

CI uses batch tests and real SVG rendering. Omit `--batch` to include graphical
interaction tests locally.

`test/latex-to-svg-upstream-test.el` contains separate upstream reproducers,
including the known failed-edit issue tracked in [#7](https://github.com/Thysrael/markdown-ts-appear/issues/7).
These diagnostics are not part of the required package CI suite.

Run `python3 test/table-redisplay.py` to check actual terminal cursor positions
and Evil insert transitions.  This uses a real PTY and the supplied three-column
CJK regression table; it requires Python 3, Evil, and the Markdown grammars.

`test/benchmark-table.el` measures cold row entry, cached motion, and unchanged
window notifications.  Initialize packages and put the library to measure first
on `load-path`, then load that file; timings include garbage collection.

`test/benchmark-editing.el` measures typing, deletion, style-boundary movement
and table navigation, including redraw time. In a separate graphical Emacs:

```sh
APPEAR_BENCH_MODE=math emacs -Q \
  --eval '(progn (require (quote package)) (package-initialize))' \
  -L . -l test/benchmark-editing.el
```

Compare `native`, `appear` and `math` modes. The benchmark reports latency, GC
time and fontification/scan counts, then exits.

## License

GNU GPL version 3 or later.  See [COPYING](COPYING).
