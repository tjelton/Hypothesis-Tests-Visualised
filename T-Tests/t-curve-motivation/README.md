# Introducing the t-Distribution (Motivation)

Unlike the t-test lessons, this one has **no data upload and no hypothesis-test
chain** — it is a conceptual explainer built around two independent interactive
demos:

* **Demo 1 (overlay):** a degrees-of-freedom slider (1–25) and a "display
  normal curve" checkbox drive a t-density curve, with an optional standard
  normal overlay. The t-curve is drawn as a black solid line and the normal as a
  red dashed line.
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
| `index.html` | All static teaching content |
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
  unaffected). The renderer draws just the curve, with no shading, for a
  blank/NaN test statistic.

## Behaviour notes

Demo 2 p-values are `2 * (1 - pnorm(|ts|))` and `2 * (1 - pt(|ts|, df))`,
displayed rounded to 5 dp, and confirmed against R.

## Known issues

Worth prioritising: `../t-test-1-sample/` opens by telling students it is "highly
recommended" to read this page first, so both of these are hit early.

* **The prose describes the wrong curve colours.** The "T-Distribution and
  P-Values" section twice refers to "the red t-curve" above "the dashed normal
  curve", but Demo 1 draws the t-curve **black solid** and the normal **red
  dashed**. A student told to look at the red curve is looking at the normal.
* The prose in "When we don't know the population sd!" has its central sentence
  inverted both ways: it says we "substitute the population standard deviation
  **for** the sample standard deviation" (it is the other way round) and that we
  "now write SE instead of ŜE" (again reversed — the estimate is ŜE). The
  displayed formulas either side are correct.

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

Per `CLAUDE.md`: copy `T-Tests/t-curve-motivation/` to the
**`gh-pages` branch as a top-level dir**, alongside `shared/` and the other
lessons, served at
`https://tjelton.github.io/Hypothesis-Tests-Visualised/T-Tests/t-curve-motivation/`.
