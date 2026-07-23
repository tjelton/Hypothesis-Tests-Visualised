# The Box Model Part 2 (Central Limit Theorem) — pure JavaScript port

A dependency-light static-page rewrite of the Shiny "Box Model Part 2" lesson
(`R/fundamentals_box_model_part_2_*.R`). No webR/shinylive: plain HTML + JS.

A **simulation-based explainer** of the Central Limit Theorem, with three
interactive activities:

1. **n = 25 box (`1,0,0,0`):** repeatedly draw samples (Repeat 1/10/25/100) and
   watch the histogram of sample means approach a normal shape.
2. **n = 5 box:** the same, showing that too few draws leaves the distribution
   non-normal (right tail + discrete jumps).
3. **Custom box:** pick a preset box (or enter your own tickets) and simulate
   10,000 samples at each of n = 5, 25, 50, 100 — comparing how large n must be
   for the CLT to "kick in", by sum or by mean.

Draws are random (`Math.random`); there is no R seed to match — the point is the
emergent *shape* of the sample-statistic distribution.

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/box-model-part-2/
```

## Layout

The numeric engine, plots, box-model helpers and CSS live in the sibling
**`../shared/`** directory.

| File | Purpose |
|---|---|
| `index.html` | All static teaching content (ported verbatim from the R UI file) |
| `js/app.js` | The three CLT activities + histogram wiring |
| `../shared/js/plots.js` | `densityHistogramSVG` (R `hist(freq = FALSE)`) |
| `../shared/js/boxmodel.js`, `../shared/js/stats.js`, `../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3.

## Shared-code additions made for this lesson

* `plots.js` gained `densityHistogramSVG` — a density histogram (bar area sums
  to 1) supporting a bin COUNT (equal-width bins over the data range) or R's
  `"sturges"` default, plus the empty-axes placeholder the R module draws before
  any data exists. The other lessons are unaffected.

## Fidelity to the Shiny version

* Binning matches the R module's intent: the n = 25 demo uses ~`unique/3` bins
  (capped at 15), the n = 5 demo uses `unique − 1`, and the custom histograms use
  Sturges breaks. Since the data are random, exact bin edges are not reproduced —
  the shape is what matters, as the lesson itself notes.
* Teaching text is verbatim, including the R source's typos ("Empiricial",
  "ticekts", "sufficiently larger", the trailing "..").

## Tests

```sh
osascript -l JavaScript box-model-part-2/tests/smoke_jxa.js
```

Checks the density histogram (empty placeholder + populated), that the simulated
sample-mean distribution centres on the box mean, and that `app.js` loads cleanly
against a DOM stub.

## Deploying

Per the migration plan in `CLAUDE.md`: copy `box-model-part-2/` to the
**`gh-pages` branch as a top-level dir**, alongside `shared/` and the other
lessons.
