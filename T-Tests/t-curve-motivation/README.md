# Introducing the t-Distribution (Motivation) — pure JavaScript port

A dependency-light static-page rewrite of the Shiny "Introducing the
t-Distribution (Motivation)" lesson (`R/ttest_t_curve_motivation_*.R`). No
webR/shinylive: the page is plain HTML + hand-written JS.

Unlike the t-test lessons, this one has **no data upload and no hypothesis-test
chain** — it is a conceptual explainer built around two independent interactive
demos:

* **Demo 1 (overlay):** a degrees-of-freedom slider (1–25) and a "display
  normal curve" checkbox drive a t-density curve, with an optional standard
  normal overlay. Matching the R *code* (not its prose), the t-curve is drawn
  as a black solid line and the normal as a red dashed line.
* **Demo 2 (p-values):** a test-statistic input and a df slider (1–50) drive two
  side-by-side two-sided shaded plots — a standard normal (z-test) and a t-curve
  (t-test) — with each distribution's p-value shown below. This shows the
  t-curve's larger p-value at low df converging toward the normal as df grows.

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/T-Tests/t-curve-motivation/
```

## Layout

The numeric engine, plots and CSS live in the sibling **`../../shared/`** directory
(see `../../shared/README.md`).

| File | Purpose |
|---|---|
| `index.html` | All static teaching content (ported verbatim from the R UI file) |
| `js/app.js` | Wiring for the two demos (no data, no test chain) |
| `../../shared/js/stats.js` | `pnorm`/`dnorm`/`pt`/`dt`, etc. |
| `../../shared/js/plots.js` | `densityOverlaySVG` (Demo 1) and `shadedNormalCurveSVG`/`shadedTCurveSVG` (Demo 2) |
| `../../shared/css/style.css` | Shared theme |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3. This
lesson's MathJax config additionally loads the `[tex]/color` extension, because
its formulas use `\color{red}{…}` to highlight σ vs s.

## Shared-code additions made for this lesson

* `stats.js` gained `dnorm(x)` (standard normal density, exact closed form).
* `plots.js` gained `densityOverlaySVG` and `shadedNormalCurveSVG`, and its
  existing shaded-curve logic was generalised into `shadedCurveSVG(density, …)`
  (with `shadedTCurveSVG` now a thin wrapper — the t-test lessons are
  unaffected). The renderer also now draws just the curve, with no shading, for
  a blank/NaN test statistic (matching the R app).

## Fidelity to the Shiny version

* Demo 2 p-values follow R exactly: `2 * (1 - pnorm(|ts|))` and
  `2 * (1 - pt(|ts|, df))`, displayed as `as.character(round(p, 5))`.
* Teaching text is verbatim, including the R prose's quirk of calling the
  t-curve "red" and the normal "dashed" while the code actually draws the
  t-curve black and the normal red-dashed. Fix here and in the R source
  together, so the two versions stay in sync.

## Tests

```sh
# Demo 2 p-values vs R + plot generation + app.js load (stock macOS, no Node):
osascript -l JavaScript T-Tests/t-curve-motivation/tests/smoke_jxa.js
```

`tests/expected_pipeline.json` is R-computed ground truth for a spread of
`(test statistic, df)` combinations (`tools/generate_expected_pipeline.R`). The
shared `stats.js` accuracy (including the new `dnorm`) is verified by
`../../shared/tests/`.

## Deploying

Per the migration plan in `CLAUDE.md`: copy `T-Tests/t-curve-motivation/` to the
**`gh-pages` branch as a top-level dir**, alongside `shared/` and the other
lessons, served at
`https://tjelton.github.io/Hypothesis-Tests-Visualised/T-Tests/t-curve-motivation/`.
