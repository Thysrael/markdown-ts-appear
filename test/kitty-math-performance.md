# Kitty math measurements

Measured on macOS with Emacs 32.0.50 and kitty-graphics
`13666d4eb2ef4eeed24697c0326368eff3667dce`, against baseline `7cb028c`.
Five alternating before/after runs used the same 112-column, 53-row window
and default 800,000-byte GC threshold. Values below are medians across runs.

| Warm-cache operation | Before | After |
| --- | ---: | ---: |
| Display-source motion, 1,000 handler calls | 29.39 ms | 18.83 ms |
| Inline-source motion, 1,000 handler calls | 21.94 ms | 12.89 ms |
| Reveal/re-hide, 40 handler calls | 117.12 ms | 0.40 ms |
| Actual command latency, median (200 commands/run) | 10.75 ms | 2.24 ms |
| Actual command latency, p95 | 19.24 ms | 4.42 ms |

Handler loops exclude idle waiting. Actual commands include forced Emacs
redisplay, but exclude the asynchronous Kitty repaint after the command.
Neither measures first-render latency or time until pixels reach the screen.
GC remains enabled: occasional command pauses still reach 25–39 ms after the
change. Each case starts after a collection to avoid inherited GC debt.

For each 200-command run:

- ImageMagick dimension queries: **100 → 0**.
- Geometry calculations: **200 → 0**.
- MathJax compilations and PNG conversions: **0 in both versions**.
- Kitty placement emissions: **600 in both versions**; redraw recovery remains
  active, using existing images.

The adapter keeps one prepared geometry/file tuple per formula. Editing state
only changes placement. Color, scale, terminal cell size, window width and
formula content invalidate preparation; PNG tinting runs only on cache misses.

Run inside a fresh Kitty terminal, with the dependencies installed:

```sh
TERM=xterm-256color KITTY_BENCH_ROOT=/path/to/checkout \
  KITTY_BENCH_LOG=/path/to/result.el \
  emacs -nw -Q -l /path/to/current/checkout/test/benchmark-kitty-math.el
```

Use the same benchmark script for both checkouts. The result records individual
command times, GC time and rendering call counts, and verifies source integrity.
