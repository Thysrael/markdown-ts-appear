# Markdown TS Appear

Move point into each rendered element to reveal its Markdown source.

## Inline markup

This line has *emphasis*, **strong text**, and ~~strikethrough~~.

Edit `inline code`, an escaped \*asterisk\*, and an entity: &copy; &#9731;.

## Links

Visit [the project](https://github.com/Thysrael/markdown-ts-appear),
<https://www.gnu.org/software/emacs/>, or <demo@example.com>.

Try a [full reference][project], a [collapsed reference][], and a
[shortcut reference].

Wiki-style links are supported too: [[Emacs]] and [[Markdown|an alias]].

![A local image](demo.svg)

An image without alt text still shows its file name: ![](demo.svg)

[project]: https://github.com/Thysrael/markdown-ts-appear "Project home"
[collapsed reference]: https://www.gnu.org/software/emacs/
[shortcut reference]: https://tree-sitter.github.io/tree-sitter/

Setext Heading
==============

### Lists and tasks

- Unordered item
- Nested markup with **bold text** and `code`
  - Nested item

1. Ordered item
2. Another ordered item

- [ ] Record the demo
- [x] Reveal the source

> Block quote markers remain visible and can contain *rendered markup*.
>
> The original marker remains visible across multiple lines.

### Table

| Element | Rendered form | Status |
| :------ | :------------ | -----: |
| Heading | ATX           | Ready  |
| Link    | Inline        | Ready  |
| Formula | Display       | Ready  |

### MathJax preview

Inline math: $e^{i\pi} + 1 = 0$ and $a^2 + b^2 = c^2$.

$$
\int_{-\infty}^{\infty} e^{-x^2}\,dx = \sqrt{\pi}
$$

$$
\mathbf{F}(\omega) = \int_{-\infty}^{\infty} f(t)e^{-i\omega t}\,dt
$$

---

The thematic break above and this hard line break  
also reveal their source when point enters them.

### Code block

Fenced code blocks keep their original delimiters and language visible:

```emacs-lisp
(use-package markdown-ts-appear
  :hook (markdown-ts-mode . markdown-ts-appear-mode))
```
