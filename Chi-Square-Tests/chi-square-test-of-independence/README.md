# Chi-Square Test of Independence

A dependency-light static-page lesson in the same style as the chi-square
goodness-of-fit lesson (plain HTML + JS).

Tests whether **two** qualitative variables are independent or associated,
using a contingency table of observed counts. Inputs are entered manually
(no dataset):

* a **name** and a **category count** for each variable — X (rows) and Y
  (columns) — plus a **name for every category level**;
* an **observed count** for every cell of the contingency table.

Under the null hypothesis the two variables are independent, so:

```
E_ij = (row i total) x (column j total) / grand total     (expected count)
chi^2 = sum over cells of (O - E)^2 / E                    (test statistic)
df = (rows - 1) x (columns - 1)
p  = P(chi^2_df > chi^2)                                   (upper-tail)
```

The worked example throughout (and the default table) is **coffee drinking vs.
being a night owl** (2×2): observed `50, 30 / 20, 40` (n = 140). Expected counts
are `40, 40, 30, 30`, χ² = 11.6667, df = 1, p ≈ 0.00064 — strong evidence the
variables are associated, so we reject independence.

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/Chi-Square-Tests/chi-square-test-of-independence/
```

## Layout

The numeric engine, plots, box-model helper and CSS live in **`../../shared/`**.
This lesson needed **no new shared code** — the chi-square functions
(`dchisq`, `pchisq`, `pchisqUpper`) and the shaded-curve plot
(`shadedChiSquareCurveSVG`) added for the goodness-of-fit lesson are reused
directly.

| File | Purpose |
|---|---|
| `index.html` | Static teaching content + the intro (coffee) modal |
| `js/app.js` | Variable/level naming, dynamic contingency-table inputs, expected frequencies, Cochran table, two-tab test statistic, chi-square p-value |
| `../../shared/js/{stats,plots,boxmodel}.js`, `../../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3.

## Behaviour notes

* **Sections**: title + "What is a chi-square independence test?" modal → Input
  Data (name the two variables, their categories, and the observed contingency
  table) → Null Hypothesis (X and Y independent) → Alternate Hypothesis (X and Y
  associated) → **Expected Frequencies** (explanation + \(E_{ij}\) table) →
  Assumptions → Test Statistic (two tabs) → p-value → Conclusion.
* **Variable / level names** are typed by the student and reflected in every
  table header and in the hypotheses. Blank names fall back to "X"/"Y" and
  "Category n". Names are HTML-escaped before insertion.
* **Independence assumption** (Assumption 1) is explained as *independence of the
  observations* — each individual contributes to exactly one cell — with an
  explicit callout that this is **not** the same as the independence between the
  two variables that the test itself is investigating.
* **Test Statistic** is built over two Bootstrap tabs: Step 1 shows, per cell,
  <span style="color:blue">observed</span> − <span style="color:red">expected</span>
  = difference (observed in blue, expected in red); Step 2 shows
  \((O-E)^2/E\) per cell and the summed test statistic in blue italic text.
* **Expected counts** are rounded to 2 dp and every downstream cell (O − E,
  \((O-E)^2/E\), the summed statistic) is computed from that rounded value, so
  the displayed tables are self-consistent (matches the goodness-of-fit lesson).
* **Cochran's rule** holds when no expected count is 0 and no more than 20% of
  the expected counts are below 5; the verdict is green when it holds, red when
  it fails, and cells below 5 are flagged red in the table.

## Tests

```sh
osascript -l JavaScript Chi-Square-Tests/chi-square-test-of-independence/tests/smoke_jxa.js
```

`tests/expected_pipeline.json` is R-computed ground truth
(`tools/generate_expected_pipeline.R`) for two scenarios: the default 2×2 coffee
example (reject; Cochran holds) and a 2×3 table with small counts (fail to
reject; Cochran fails). The shared `stats.js`/`plots.js` chi-square code is
covered by `../../shared/tests/`.

## Deploying

Per `CLAUDE.md`: copy `Chi-Square-Tests/chi-square-test-of-independence/` to the **`gh-pages`
branch as a top-level dir**, alongside `shared/` and the other lessons. The
edits to `../../shared/js/navbar.js` and the root `index.html` quick-link must be
published too.
