# Full latex-to-svg migration experiment

This branch replaces the internal MathJax preview pipeline with the complete
`latex-to-svg-for-markdown` adaptor. It is an experiment, not a mainline change.

## Revisions

- MathJax baseline: markdown-ts-appear `e5f50dc` (separate, detached worktree).
- Frontend: alberti42/latex-to-svg `b9cf5af2471f06e24fb14ca2fefc41fb3a404b5d`.
- Backend: alberti42/latex-to-svg-backend `a5c886daff1c16686038e2915b472022138e73d6`.
- MathJax Emacs package 0.1; Node.js 26.10.0.
- TeX Live 2026/Homebrew (pdfTeX 1.40.29), dvisvgm 3.6.1.
- RaTeX 0.1.14, native arm64 executable. texres was not tested.
- Local machine: Apple M4 Pro, macOS, Emacs 32.0.50 with SVG support.

## Ownership

The external adaptor owns math detection, rendering, overlays, cache, numbering,
references, and appearance refresh. Appear supplies a buffer-local reveal
predicate through the public `latex-to-svg-frontend-reveal` option, and uses
`latex-to-svg-frontend-span-at` to leave its spans alone. There is no MathJax
fallback and no advice around the external frontend.

`markdown-ts-appear-enable-math-preview` still defaults to nil. When enabled,
appear starts the adaptor and stops it on teardown. An adaptor already enabled
by the user is left running; either mode may be enabled first. The prior local
reveal option is restored unless the user has replaced it in the meantime.

Ordinary source tracking and Evil/Meow start/stop transitions apply to math on
the command hook. Math delimiter visibility is supplied by the frontend's
font-lock keywords. Appear avoids decorating Markdown-looking links inside
math and exposes wrapped table rows when their math source is edited.

Indirect buffers remain unsupported. Clone detachment currently needs to clear
three inherited private frontend state variables before disabling its hooks,
so it does not cancel the base buffer's timers or detach its cursor marker.
An upstream clone-detach API would remove this small private dependency.

## Known limitations and upstream failures

The standalone `test/latex-to-svg-upstream-test.el` loads **no appear code**.
Its three expected-behavior assertions fail on the pinned frontend:

1. An older render callback can overwrite a newer equation preview.
2. A callback can cover currently revealed source; a command without cursor
   movement does not necessarily repair it.
3. A callback can recreate a preview after the adaptor has been disabled.

These are controlled-backend lifecycle tests, not TeX process or screenshot
tests. They are intentionally kept outside the regular integration suite:

```sh
emacs --batch -Q -L /path/to/latex-to-svg-backend -L /path/to/latex-to-svg \
  -l test/latex-to-svg-upstream-test.el -f ert-run-tests-batch-and-exit
```

The initial report was submitted as
[latex-to-svg#5](https://github.com/alberti42/latex-to-svg/issues/5). Integration
and performance discussion continues in
[markdown-ts-appear#7](https://github.com/Thysrael/markdown-ts-appear/issues/7).
The external dependency is unpatched in this experiment, so these failures
remain blockers to a mainline migration.

Wrapped tables use replacement strings for entire rows: showing formula images
inside those strings is not implemented. Math source is editable by revealing
the row. Use `markdown-ts-appear-table-style` = `raw` for math previews in table
source; this experiment does not claim pixel-aware mathematical table layout.

Traditional LaTeX is not input-compatible with every MathJax expression. Check
existing Unicode/Chinese text, package-specific macros, nested environments and
preambles before migrating a document. RaTeX implements a different subset.

## Running the experiment

Keep the frontend and backend checkouts on `load-path`, ahead of older installed
versions, and put this checkout ahead of an installed markdown-ts-appear. With
the dependencies available, enable:

```elisp
(setq markdown-ts-appear-enable-math-preview t)
(setq latex-to-svg-frontend-center-display-math t)
;; Optional: choose RaTeX instead of traditional LaTeX.
;; (setq latex-to-svg-frontend-engine 'ratex
;;       latex-to-svg-backend-ratex-program "/path/to/render-svg"
;;       latex-to-svg-frontend-fallback nil)
```

The integration needs the APIs added after v0.21.0. The exact revision is pinned in
CI. The normal project suite now tests the external frontend rather than the
deleted MathJax staging/overlay implementation.

For a fresh graphical Emacs test session:

```sh
MATH_GUI_ENGINE=latex MATH_GUI_OUTPUT=/tmp/math-gui.txt \
  emacs -Q -L . -L /path/to/latex-to-svg-backend -L /path/to/latex-to-svg \
  -l test/run-math-gui.el
```

Use `MATH_GUI_ENGINE=ratex` and `MATH_BENCH_RATEX=/path/to/render-svg` for RaTeX.
The launcher uses a fresh temporary cache and the checkout's test file.

## Validation

- Regular ERT suite: **154 passed, 15 skipped, zero unexpected failures**.
  Skips are GUI-only image/Mermaid tests and the real-engine math test in batch.
  The real Mermaid CLI checks passed with an explicit Chrome executable.
- Separate graphical math suite: **14/14 passed with LaTeX**, and **14/14
  passed with RaTeX**, including a real SVG render, image dimensions and reveal.
  The other tests use the real frontend with a controlled rendering backend.
- Byte-compilation of all production libraries with warnings treated as errors:
  passed. Checkdoc on changed production libraries: passed.
- Python tests: 4 passed. Real PTY table/Evil checks: 435 cursor checkpoints passed.
- Standalone upstream asynchronous regressions: **3 failed**, as documented
  above. They are not disguised as passing integration tests.

The local runtime is Emacs 32.0.50. The updated Emacs 31.1/snapshot CI matrix
has not been run remotely. texres, large-document scaling and all existing
user preambles have not been validated.

## Performance methodology

`test/benchmark-math.el` measures actual rendering pipelines, not a stub.
The backend comparison calls `mathjax-display` (including its staging overlay)
versus `latex-to-svg-backend` (including its cache and image construction).
These are application-facing latency measurements, not bare CLI engine timings.
`test/run-math-benchmark.py` starts a fresh Emacs per engine and sample,
sequentially, with rotating engine order. Every sample has a fresh latex-to-svg
cache. LaTeX precompilation and worker count retain upstream defaults.
For the published comparison, both appear checkouts and the external
frontend/backend were byte-compiled beforehand; MathJax is the installed
compiled package.
Native JIT compilation is disabled during measurement. The recorded library
paths and an explicit checkout assertion guard against accidentally measuring
the installed appear package instead of the selected checkout.

Measurements distinguish:

- First formula, including runtime startup or cold preamble precompilation.
- Twenty distinct uncached formulas after startup.
- The same twenty formulas again (latex-to-svg cache hits; MathJax re-renders).
- A burst of thirty new formulas, using each implementation's normal queue.
- Full frontend opening/reopening a thirty-equation Markdown buffer.
- One thousand cached cursor-entry/exit commands, without forced redisplay.

The corpus alternates inline/display math and includes polynomials, fractions,
integrals, matrices and sums. The full frontend corpus uses only `$`/`$$`,
supported by both detectors. Graphical runs force `image-size` to verify real
SVG rasterization. Frontend open includes one forced redisplay. Timings do not
include launching Emacs or installing/loading packages. Frontend open happens
after the backend benchmark, so its process/preamble is warm but its formulas
are uncached. No assertion of identical typography is made.

```sh
python3 test/run-math-benchmark.py \
  --emacs /path/to/graphical/emacs \
  --baseline /path/to/e5f50dc-worktree \
  --frontend /path/to/latex-to-svg \
  --backend /path/to/latex-to-svg-backend \
  --ratex /path/to/render-svg \
  --output-dir /tmp/math-comparison --rounds 3
```

Each sample's raw JSON is retained next to `summary.json`. `--batch` runs only
backend image-spec completion, without GUI rasterization or frontend timings.
For matching compilation state, byte-compile the `markdown-ts-appear*.el`
libraries in each checkout and `latex-to-svg-frontend.el` /
`latex-to-svg-for-markdown.el` in the external frontend checkout first, using
the same Emacs and with the required dependencies on `load-path`.

The baseline can be prepared from this repository with:

```sh
git worktree add --detach ../markdown-ts-appear-mathjax e5f50dc
```

Install the optional `mathjax` Emacs package and Node.js for the baseline, and
use the external revisions listed above for the comparison. Both checkouts
also need `markdown-table-wrap` and the Markdown Tree-sitter grammars. Run the
Python launcher from this experimental checkout. On macOS, `--emacs` should
point to the GUI executable inside Emacs.app rather than a terminal wrapper.

## Measured results (2026-10-13)

Final compiled-code comparison, three independent processes per provider.
Times below are milliseconds; lower is faster. Single uncached/repeated
formula medians pool 60 requests (20 per process). Other medians summarize
the three runs. Each motion run executes 1,000 command hooks.

| Scenario | MathJax | LaTeX + dvisvgm | RaTeX |
|---|---:|---:|---:|
| First formula, cold runtime/cache | 158.66 | 387.05 | **19.54** |
| New formula after startup | **1.78** | 204.33 | 13.90 |
| Repeat already-rendered formula through backend API | 1.316 | **0.025** | 0.026 |
| Burst of 30 new formulas, backend | **34.43** | 869.46 | 284.23 |
| Open 30-equation document, uncached formulas / warm runtime | **79.42** | 968.53 | 322.75 |
| Reopen same document in another buffer, same process | 63.96 | **37.53** | 38.69 |
| Cached cursor enter/leave, per command | **0.346** | 1.125 | 1.127 |
| Synchronous blocking in first backend request | **0.48** | 171.89 | 1.00 |

Machine-readable medians: [math-benchmark-summary.json](math-benchmark-summary.json).
The runner saves individual measurements and `summary.json` in its output
directory. The published medians were checked against the original nine runs.
Earlier source-code runs are exploratory and are not mixed into this table.

### Interpretation

- **For sustained rendering of new formulas, MathJax wins this corpus.** It
  keeps a Node process alive; RaTeX starts an external process for each uncached
  formula. Warm single-request latency here is roughly 7.8 times lower than
  RaTeX's and 115 times lower than traditional LaTeX's.
- **RaTeX has the best cold first result.** No Node startup and no LaTeX
  preamble dump. A first preview and a stream of subsequent new previews are
  different workloads, with different winners.
- **latex-to-svg's cache is valuable.** Repeated backend requests use already
  compiled images. MathJax's backend has no equivalent content cache, but the
  old appear frontend DOES reuse overlays for unchanged formulas. Accordingly,
  API cache-hit timings are not a proxy for cursor-movement latency.
- **The full external frontend costs more on cached cursor motion** in this
  30-equation document: about 1.13 ms per command versus 0.35 ms. This includes
  actual reveal/font-lock hooks but no forced redisplay for each movement;
  it is not a keystroke-to-screen or large-document benchmark.
- **Traditional LaTeX is not completely nonblocking on first use:** roughly
  172 ms of the first request ran synchronously, primarily the default preamble
  precompilation. The subsequent equation jobs are asynchronous. This is a
  separate observation from the asynchronous overlay failures.

The burst uses upstream defaults: latex-to-svg allows **12 concurrent jobs**
on this machine; MathJax uses its persistent worker. Reopen/repeat tests are
warm within the same Emacs process; restarting Emacs with a populated disk
cache was not separately measured. Typography, unsupported syntax, equation
numbering and package support are not scored by this performance corpus.

## Current conclusion

Full delegation is implemented and normal integration checks pass. It reduces
our math-specific lifecycle code and adds upstream math capabilities, but it
does **not** establish a blanket speed improvement. RaTeX is the most responsive
cold-start choice here; MathJax remains faster for new-formula throughput and
the tested cached navigation workload. LaTeX is the capability-oriented option.

Keep this branch experimental until the three asynchronous lifecycle failures
are fixed upstream and verified in the combined editor. The benchmark scripts,
integration tests and isolated upstream regressions are retained for that next
iteration. No changes have been merged into main.
