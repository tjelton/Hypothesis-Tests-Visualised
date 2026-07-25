# Regression t-Test — pure JavaScript port

A dependency-light static-page rewrite of the Shiny "Regression t-Test" lesson
(`R/ttest_regression_*.R` and `R/utility_load_data_regression_2_variable_*.R`).
No webR/shinylive: the page is plain HTML + hand-written JS.

The lesson fits a simple linear regression `y ~ x` (OLS) and tests
**H₀: β₁ = 0** (no linear relationship). All quantities come from
`Stats.linreg` (matching R's `lm()` / `summary()`): the slope, the residual
standard error `s` (= `summary$sigma`), `SE(β₁) = s / √Sxx`, the test statistic
`t = β₁ / SE(β₁)`, and `df = n − 2`.

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/T-Tests/t-test-regression/
```

## Layout

The numeric engine, plots and CSS live in the sibling **`../../shared/`** directory
(see `../../shared/README.md`).

| File | Purpose |
|---|---|
| `index.html` | All static teaching content (ported verbatim from the R UI file) |
| `js/datasets.js` | Generated — the 6 datasets the loader offers (`tools/generate_datasets.R`) |
| `js/app.js` | Reactive wiring: loader, OLS fit, four residual plots, test stat, p-value, conclusion, CI |
| `../../shared/js/{stats,plots}.js`, `../../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3.

## Datasets

The synthetic `study_data` (`set.seed(1)`; `Minutes_Studied ~ U(0,600)`,
`Test_Score = 50 + 0.075·Minutes + N(0,5)`), plus `iris`, `mtcars`, `trees`,
`airquality`, `pressure` from R's `datasets` package. The loader picks an x and
a y numeric column, with an optional split by a categorical variable (only
`iris` has one). `airquality` contains `NA`s (emitted as `null`); incomplete
`(x, y)` pairs are dropped before fitting, as R's `lm()` does via `na.omit`.

## Shared-code additions made for this lesson

* `stats.js` gained `linreg(x, y)` — OLS fit returning slope, intercept,
  residuals, RSS, residual standard error, and SE of the slope (verified
  against R's `lm()`/`summary()` in `../../shared/tests/`).
* `plots.js` gained `scatterSVG`, which (via options) covers the scatter plot,
  the residual plots (`hline` at 0), the ordered-residual line plot
  (`connect`), and the intro horizontal-line plot (`xlim`/`ylim`).

## Fidelity to the Shiny version

* Default view (study_data, Minutes_Studied → Test_Score, two-sided):
  β̂₁ = 0.07, s = 3.411, SE = 0.004, TS = 17.877, df = 23, p ≈ 0,
  CI = (0.0623, 0.0786) — confirmed against R. The iris Sepal.Length →
  Sepal.Width fit gives TS = −1.44 overall but 7.681 within setosa (a nice
  Simpson's-paradox illustration), both matching R.
* The displayed test statistic is `round(t, 3)`; the p-value uses that rounded
  value with `df = n − 2`, while the CI uses the full-precision slope and SE —
  matching the R module's exact chain.

### Reproduced R quirks (documented so both versions can be fixed together)

* The p-value **t-curve plot** is drawn with `df − 1` (the R code passes
  `df = df() - 1` to the plotting helper, while the p-value uses the full `df`).
  Visual only.
* Teaching text is verbatim, including "constant varaince" and the Assumption 4
  text mislabelling itself "The second assumption".

### Deliberate deviation

* Requires ≥ 3 complete `(x, y)` pairs and non-constant `x` before showing the
  analysis (a degenerate fit would otherwise produce `NaN`). The R app does not
  guard this.

## Tests

```sh
osascript -l JavaScript T-Tests/t-test-regression/tests/smoke_jxa.js
```

`tests/expected_pipeline.json` is R-computed ground truth for the default
selection across all three alternatives (`tools/generate_expected_pipeline.R`).
The shared `stats.js` (including `linreg`) is verified by `../../shared/tests/`.

## Deploying

Per the migration plan in `CLAUDE.md`: copy `T-Tests/t-test-regression/` to the
**`gh-pages` branch as a top-level dir**, alongside `shared/` and the other
lessons, served at
`https://tjelton.github.io/Hypothesis-Tests-Visualised/T-Tests/t-test-regression/`.
