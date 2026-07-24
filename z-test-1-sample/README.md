# 1-Sample z-Test — pure JavaScript port

A dependency-light static-page rewrite of the Shiny "1-Sample z-test" lesson
(`R/ztest_1_sample_*.R`). No webR/shinylive: plain HTML + JS.

The z-test assumes a **known population standard deviation** (σ) and uses the
**standard normal** distribution (no degrees of freedom):

```
SE = σ / √n;   z = (OV − EV) / SE;   p-value from pnorm
```

Its data loader and datasets are **identical to the 1-sample t-test's**
(`load_1_sample_data`), so it offers the same "Mr. Han's Math Class" seeded
sample plus the R `datasets` sets. The extra control is the population σ, which
defaults to the sample SD (with a "Set Population SD to be Sample SD" button)
and resets to the sample SD whenever the data changes or an invalid (≤ 0) value
is entered.

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/z-test-1-sample/
```

## Layout

The numeric engine, plots, box-model helper and CSS live in **`../shared/`**.

| File | Purpose |
|---|---|
| `index.html` | Static teaching content (ported verbatim from the R UI file) |
| `js/datasets.js` | Generated — identical to the 1-sample t-test's datasets (`tools/generate_datasets.R`) |
| `js/app.js` | Loader + σ handling + z-test chain (SE, z, p, CI) |
| `../shared/js/{stats,plots,boxmodel}.js`, `../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3. No new
shared code was needed — `shadedNormalCurveSVG` (added for the t-curve lesson)
draws the p-value curve.

## Fidelity to the Shiny version

* Default view (Mr. Han's data, null 140, σ = sample SD = 4.751): TS = 2.9926,
  p (two-sided) = 0.00277, confirmed against R. Note the same data gives the
  t-test's p = 0.00874 — the z-test's smaller p reflects the normal's thinner
  tails versus the t-distribution.
* Displayed numbers follow the same round-then-parse chain as the R module.
* Teaching text is verbatim, including the R source's typos ("proivde",
  "deata", "becuase", and the modal writing "denoted by \(\mu\)" where it means
  σ).

### Reproduced R quirk (documented so both versions can be fixed together)

The confidence interval is centred on the **expected value (the null mean)**
rather than the observed sample mean — the R source sets
`xbar = as.numeric(EV_string)`. Because the null value is then the centre of the
interval, it always lies inside, so the CI conclusion is always "fail to
reject" (even when the p-value rejects). This mirrors the Shiny app; the fix
(centre on the observed mean) should be made in both versions together.

## Tests

```sh
osascript -l JavaScript z-test-1-sample/tests/smoke_jxa.js
```

`tests/expected_pipeline.json` is R-computed ground truth for Mr. Han's data
under all three alternatives (`tools/generate_expected_pipeline.R`). The shared
`stats.js` accuracy is verified by `../shared/tests/`.

## Deploying

Per `CLAUDE.md`: copy `z-test-1-sample/` to the **`gh-pages` branch as a
top-level dir**, alongside `shared/` and the other lessons.
