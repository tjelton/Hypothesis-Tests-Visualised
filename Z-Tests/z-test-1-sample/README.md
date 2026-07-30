# 1-Sample z-Test

The z-test assumes a **known population standard deviation** (σ) and uses the
**standard normal** distribution (no degrees of freedom):

```
SE = σ / √n;   z = (OV − EV) / SE;   p-value from pnorm
```

Its data loader and datasets are **identical to the 1-sample t-test's**, so it
offers the same "Mr. Han's Exam Marks" seeded sample plus the standard R
`datasets` sets. The extra control is the population σ:

* For **Mr. Han's Exam Marks** it defaults to **7.5**, the value the intro
  modal's case study establishes as known — so the page opens reproducing the
  worked example exactly (TS = 1.8956, one-sided greater, p = 0.02901).
* For every other data set no known σ exists, so it falls back to the **sample
  SD**. That violates the test's own third assumption; the page says so
  explicitly and points the reader at the 1-sample t-test.
* The "Set Population SD to be Sample SD" button always substitutes the sample
  SD, and σ resets to its default whenever the data changes or an invalid (≤ 0)
  value is entered. `KNOWN_SIGMA` in `js/app.js` is the lookup.

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/Z-Tests/z-test-1-sample/
```

## Layout

The numeric engine, plots, box-model helper and CSS live in **`../../shared/`**.

| File | Purpose |
|---|---|
| `index.html` | Static teaching content |
| `js/datasets.js` | Generated — identical to the 1-sample t-test's datasets (`tools/generate_datasets.R`) |
| `js/app.js` | Loader + σ handling + z-test chain (SE, z, p, CI) |
| `../../shared/js/{stats,plots,boxmodel}.js`, `../../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3. No new
shared code was needed — `shadedNormalCurveSVG` (added for the t-curve lesson)
draws the p-value curve.

## Design notes

Things a future edit could easily undo by accident:

* **The confidence interval is centred on the observed sample mean**, and its
  level is **derived as 1 − α** rather than taken as a separate input. Those two
  choices are what make the CI and p-value sections always agree; the CI
  section explains the link, so there is deliberately no confidence-level box to
  fill in. `tests/smoke_jxa.js` asserts the agreement for all three
  alternatives, so breaking it fails the suite.
* **σ defaults to 7.5 for Mr. Han's data** via `KNOWN_SIGMA` in `js/app.js`,
  because the case study establishes that value as known. The default alternate
  hypothesis is one-sided greater for the same reason. Together these make the
  page open reproducing its own intro modal.

## The case study

Mr. Han is a teacher who **also marks exams for the education board**. Marking
takes weeks, so the board gives him a random sample of 25 papers from across the
country for an early read on whether the cohort met the standard of 140. μ is
the mean mark of every student who sat the exam: real, fixed, and genuinely
unobservable, since only 25 of many thousands of papers are marked.

The scenario is built so the assumptions actually hold — the sample is random by
construction (Assumption 1), σ = 7.5 is known from prior years for exactly that
population (Assumption 3), and 140 is a cohort-level target rather than a
threshold meant for individuals. `T-Tests/t-test-1-sample/` quotes this
scenario, so the two lessons must be edited together.

The data set is labelled **"Mr. Han's Exam Marks"**; its values are
`set.seed(1); rnorm(25, 142, 5)`.

## Reference values

For Mr. Han's data (α = 0.05, null 140), confirmed against R:

| σ | TS | p (one-sided >) | 95% CI (one-sided) | verdict |
|---|---|---|---|---|
| 7.5 (known — the default) | 1.8956 | 0.02901 | (140.376, ∞) | reject |
| 4.751 (sample SD) | 2.9926 | 0.00138 | (141.2805, ∞) | reject |

Switching to two-sided under the known σ gives p = 0.05801 and a CI of
(139.9034, 145.7833) — a useful borderline case, where both routes fail to
reject. Substituting the sample SD gives two-sided p = 0.00277, where the
1-sample t-test on the same data gives 0.00632: more than double, which is the
concrete cost of violating Assumption 3. The page does not quote those two
numbers; Step 1 just says σ is usually unknown and sends the reader to the
t-test.

## Tests

```sh
osascript -l JavaScript Z-Tests/z-test-1-sample/tests/smoke_jxa.js
```

`tests/expected_pipeline.json` is R-computed ground truth for Mr. Han's data
under all three alternatives and both σ scenarios (known 7.5 and sample SD) —
regenerate with `Rscript tools/generate_expected_pipeline.R` from the repo root.
The smoke test also drives `app.js` through a DOM stub, so it pins the wiring
(default σ, the displayed TS/p/CI, and the CI/p-value agreement) rather than only
checking that the file parses. The shared `stats.js` accuracy is verified by
`../../shared/tests/`.

## Deploying

Per `CLAUDE.md`: copy `Z-Tests/z-test-1-sample/` to the **`gh-pages` branch as a
top-level dir**, alongside `shared/` and the other lessons.
