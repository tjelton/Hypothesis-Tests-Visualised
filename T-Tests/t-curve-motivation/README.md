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

Demo 2's two plots share an x-range but are each scaled to their own curve's
peak density, so the curves cannot be compared by height across the pair — only
the printed p-values (and each plot's own shaded fraction) are comparable. The
prose says so.

Demo 1's y-range is fixed by the taller of the two densities whether or not the
normal overlay is shown, so the t-curve does not jump scale when the checkbox is
toggled.

## Content notes

The lesson deliberately leaves two things for later exercises (the conclusion
says so): how the degrees-of-freedom value is chosen (`df = n - 1` for the
1-sample case), and the normality assumption under which the test statistic
follows a t-distribution exactly.

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

The live site is served from the `gh-pages` branch, which mirrors `main` exactly
— every lesson keeps the same path it has here. `tools/deploy_gh_pages.sh`
publishes `origin/main` verbatim (plus a `.nojekyll` marker), so there is nothing
to copy per lesson and no separate step for `../../shared/` or the root
`index.html`:

```sh
bash tools/deploy_gh_pages.sh
```

Served at `https://tjelton.github.io/Hypothesis-Tests-Visualised/T-Tests/t-curve-motivation/`.
