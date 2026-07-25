# Chi-Square Goodness of Fit Test — pure JavaScript lesson

A dependency-light static-page lesson in the same style as the other JS ports
(no webR/shinylive; plain HTML + JS). There is no Shiny predecessor — this
lesson is new.

Tests whether the counts observed across the categories of a single qualitative
variable are consistent with a hypothesised set of proportions. Inputs are
entered manually (no dataset):

* the **number of categories** `k` (≥ 2),
* an **observed count** per category — the sample size `n` is their sum,
* a **null proportion** per category (must sum to 1; defaults to `1/k` each).

The test is always right-tailed:

```
E_i = n · p_i                      (expected count per category)
χ² = Σ (O_i − E_i)² / E_i          (test statistic)
df = k − 1
p  = P(χ²_{k−1} > χ²)              (upper-tail chi-square probability)
```

The worked example throughout (and the default inputs) is **rolling a die**:
6 categories, observed `8, 12, 9, 11, 15, 5` (n = 60), null `1/6` each. This
gives expected counts of 10, χ² = 6, df = 5, p ≈ 0.30622 — the die appears fair,
so we fail to reject.

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/chi-square-goodness-of-fit/
```

## Layout

The numeric engine, plots, box-model helper and CSS live in **`../shared/`**.

| File | Purpose |
|---|---|
| `index.html` | Static teaching content + the intro (die) modal |
| `js/app.js` | Dynamic per-category inputs, box model, Cochran table, test-statistic table, chi-square p-value |
| `../shared/js/{stats,plots,boxmodel}.js`, `../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3.

### New shared code added for this lesson

* `stats.js`: `dchisq`, `pchisq` (lower tail, via the lower incomplete gamma
  `gammp` so the far tail doesn't cancel against 1) and `pchisqUpper` (the
  upper-tail p-value). Verified against R in `../shared/tests/`.
* `plots.js`: `shadedChiSquareCurveSVG(df, testStat)` — a right-skewed chi-square
  density with the upper tail above the test statistic shaded and marked.

## Behaviour notes

* **Sections**: title + "What is a chi-square goodness of fit test?" modal →
  Input Data (categories + observed counts) → Null Hypothesis (per-category null
  proportions + box) → Alternate Hypothesis (fixed: "at least one equality does
  not hold") → Assumptions (independence paragraph + Cochran's-rule expected-count
  table) → Test Statistic (per-category table with a bold column-sum row) →
  p-value (prelude + shaded curve) → Conclusion (p-value).
* **Box tickets** are derived smartly from the null proportions: it tries to
  express them as small whole-number ticket counts (fair die `1/6` each →
  `1, 2, 3, 4, 5, 6`; `0.45/0.45/0.05/0.05` → nine `1`s, nine `2`s, one `3`, one
  `4`). If that needs more than 24 tickets (e.g. 50 equal categories at 2%) it
  falls back to a percentage form (`"1" x 2%, "2" x 2%, …`).
* **Cochran's rule** holds when no expected count is 0 and no more than 20% of
  the expected counts are below 5. The verdict is green when it holds and red
  when it fails (listing which condition failed); cells below 5 are flagged red
  in the table.
* The null proportions are stored at full precision (`1/k` exactly for the
  untouched default) and only overwritten when a cell is edited, so the default
  die yields clean expected counts of 10 and χ² = 6 rather than 9.9999-style
  artefacts from rounded input display.

## Tests

```sh
osascript -l JavaScript chi-square-goodness-of-fit/tests/smoke_jxa.js
```

`tests/expected_pipeline.json` is R-computed ground truth
(`tools/generate_expected_pipeline.R`) for two scenarios: the default die
(fail-to-reject, Cochran holds) and an unequal-null example (reject, Cochran
fails). The shared `stats.js`/`plots.js` additions are covered by
`../shared/tests/` (chi-square reference values from
`../shared/tools/generate_reference_values.R`).

## Deploying

Per `CLAUDE.md`: copy `chi-square-goodness-of-fit/` to the **`gh-pages` branch as
a top-level dir**, alongside `shared/` and the other lessons. The edits to
`../shared/` (new `stats.js`/`plots.js`/`navbar.js` code and CSS) and the root
`index.html` quick-link must be published too.
