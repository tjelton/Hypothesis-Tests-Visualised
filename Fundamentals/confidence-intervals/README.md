# Confidence Intervals — pure JavaScript port

A dependency-light static-page rewrite of the Shiny "Confidence Intervals"
lesson (`R/fundamentals_confidence_interval_*.R`). No webR/shinylive: plain
HTML + JS.

A conceptual explainer that builds intuition for what a confidence interval
represents, via two simulations that each generate 100 intervals and colour
them by whether they contain the true mean, with a running coverage tally:

1. **Fixed demo** — 100 intervals from `N(100, 15²)` samples of size 30 at 95%.
   About 95% come out green (contain the true mean of 100).
2. **Interactive demo** — sliders for confidence level, sample size, and the
   population mean/SD, showing how each affects interval width and coverage.

Sampling is random (`Stats.rnorm`); only the interval arithmetic (qnorm-based
margins) is exact.

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/Fundamentals/confidence-intervals/
```

## Layout

The numeric engine, plots and CSS live in **`../../shared/`**.

| File | Purpose |
|---|---|
| `index.html` | Static teaching content (ported verbatim from the R UI file) |
| `js/app.js` | The two CI simulations + running-history wiring |
| `../../shared/js/{stats,plots}.js`, `../../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3.

## Shared-code additions made for this lesson

* `stats.js` gained `rnorm` (Box–Muller normal draws — random, no R-seed
  fidelity, as this is a simulation).
* `plots.js` gained `ciPlotSVG` (horizontal confidence intervals coloured by
  coverage, with a dashed true-mean line). The other lessons are unaffected.

## Fidelity to the Shiny version

* The interval method matches R: margin = `qnorm((1+conf/100)/2) · sd(sample)/√n`.
  Empirically a 95% interval covers the true mean ~95% of the time (verified over
  10,000 trials in the smoke test). Demo 2 fixes the x-axis to the widest
  possible margin (99% level, n = 5), and every slider resets the running
  history, matching the R observers.
* Teaching text is verbatim, including the stray trailing "U" after the Demo 2
  note in the R source.

## Tests

```sh
osascript -l JavaScript Fundamentals/confidence-intervals/tests/smoke_jxa.js
```

Verifies `rnorm`'s mean/sd, the exact z-multiplier, empirical 95% coverage over
many trials, that the CI plot renders green/red intervals with a dashed mean
line, and that `app.js` loads against a DOM stub.

## Deploying

Per `CLAUDE.md`: copy `Fundamentals/confidence-intervals/` to the **`gh-pages` branch as a
top-level dir**, alongside `shared/` and the other lessons.
