# 1-Sample z-Test — pure JavaScript port

A dependency-light static-page rewrite of the Shiny "1-Sample z-test" lesson
(`R/ztest_1_sample_*.R`). No webR/shinylive: plain HTML + JS.

The z-test assumes a **known population standard deviation** (σ) and uses the
**standard normal** distribution (no degrees of freedom):

```
SE = σ / √n;   z = (OV − EV) / SE;   p-value from pnorm
```

Its data loader and datasets are **identical to the 1-sample t-test's**
(`load_1_sample_data`), so it offers the same "Mr. Han's Exam Marks" seeded
sample plus the R `datasets` sets. The extra control is the population σ:

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
| `index.html` | Static teaching content (ported verbatim from the R UI file) |
| `js/datasets.js` | Generated — identical to the 1-sample t-test's datasets (`tools/generate_datasets.R`) |
| `js/app.js` | Loader + σ handling + z-test chain (SE, z, p, CI) |
| `../../shared/js/{stats,plots,boxmodel}.js`, `../../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3. No new
shared code was needed — `shadedNormalCurveSVG` (added for the t-curve lesson)
draws the p-value curve.

## Relationship to the Shiny version

Displayed numbers still follow the same round-then-parse chain as the R module,
and `tests/expected_pipeline.json` is R-computed ground truth. The following
**deliberate divergences** fix teaching bugs in the original rather than port
them, so the R module (`R/ztest_1_sample_srv.R`) needs the same changes if it is
ever revived:

1. **The CI is centred on the observed sample mean.** The R source set
   `xbar = as.numeric(EV_string)`, i.e. the null mean, which put the null value
   at the *centre* of the interval — so it was always inside and the CI section
   always concluded "fail to reject", contradicting the p-value section on the
   same page.
2. **The confidence level is derived as 1 − α** rather than being a second free
   input. With two independent inputs a student could set α = 0.05 and a 0.80
   level and get two contradicting conclusions with no explanation. The CI is
   also matched to the alternate hypothesis' sidedness. `tests/smoke_jxa.js`
   asserts the resulting CI/p-value duality for all three alternatives.
3. **σ defaults to the case study's known 7.5** for Mr. Han's data (see above),
   so the page agrees with its own intro modal.
4. **The default alternate hypothesis is one-sided greater**, matching the case
   study's \(H_1: \mu > 140\).
5. **"Accept the null hypothesis" → "fail to reject the null hypothesis"**, and
   the p-value definition now includes *"assuming the null hypothesis is
   true"*. Note the other lessons in this repo still say "accept".
6. **The case study is a different scenario.** The original had Mr. Han testing
   whether "the average exam grade for the 25 students in his class" exceeded
   140, with μ defined as "the average of his class". Under that wording μ was
   fully observed — it is just the mean of the 25 marks, 142.843 — so there was
   nothing to infer and the test was vacuous. It also made Assumptions 1 and 3
   both dubious: one class is not a random sample, and σ = 7.5 was measured on a
   different population from the one his class represented.

   Mr. Han is now a teacher who **also marks exams for the education board**.
   Marking takes weeks, so the board gives him a random sample of 25 papers from
   across the country for an early read on whether the cohort met the standard.
   μ is the mean mark of every student who sat the exam: real, fixed, and
   genuinely unobservable (only 25 of many thousands of papers are marked). The
   sample is random by construction, so Assumption 1 is *satisfied*; σ = 7.5 is
   known from prior years for exactly that population, so Assumption 3 is too;
   and 140 is a cohort-level target, so the test is no longer comparing a mean
   against a threshold meant for individuals. `T-Tests/t-test-1-sample/` quotes
   this scenario and has been updated to match (its red-text "remove σ" device
   still works unchanged).

   Because the marks are no longer one teacher's class, the data set label is
   now **"Mr. Han's Exam Marks"** (was "Mr. Han's Math Class") in both lessons.
   The values themselves are untouched: `set.seed(1); rnorm(25, 142, 5)`.
7. **Teaching-text corrections**: the R source's typos are fixed rather than
   reproduced ("proivde", "deata", "becuase", "exerice", "distribued", the
   duplicated CLT paragraph in Assumption 2, the "(SE)" that should read "(EV)"
   in the Test Statistic heading, and the modal writing \(\mu\) where it means
   σ). Assumption 1's example, which discussed a *proportion* test, now
   discusses this test.

Reference values for Mr. Han's data (α = 0.05, null 140), all confirmed
against R:

| σ | TS | p (one-sided >) | 95% CI (one-sided) | verdict |
|---|---|---|---|---|
| 7.5 (known — the default) | 1.8956 | 0.02901 | (140.376, ∞) | reject |
| 4.751 (sample SD) | 2.9926 | 0.00138 | (141.2805, ∞) | reject |

Switching to two-sided under the known σ gives p = 0.05801 and a CI of
(139.9034, 145.7833) — a useful borderline case, where both routes fail to
reject. Substituting the sample SD gives two-sided p = 0.00277 where the
1-sample t-test on the same data gives 0.00874 — roughly three times larger,
which is the concrete cost of violating Assumption 3. The page deliberately does
*not* quote these two numbers; Step 1 just says σ is unknown in practice and
sends the reader to the t-test.

## Tests

```sh
osascript -l JavaScript Z-Tests/z-test-1-sample/tests/smoke_jxa.js
```

`tests/expected_pipeline.json` is R-computed ground truth for Mr. Han's data
under all three alternatives and both σ scenarios (known 7.5 and sample SD) —
regenerate with `Rscript tools/generate_expected_pipeline.R` from the repo root.
The smoke test also asserts that each CI reaches the same verdict as its
p-value, which is the invariant the corrected CI restores. The shared `stats.js`
accuracy is verified by `../../shared/tests/`.

## Deploying

Per `CLAUDE.md`: copy `Z-Tests/z-test-1-sample/` to the **`gh-pages` branch as a
top-level dir**, alongside `shared/` and the other lessons.
