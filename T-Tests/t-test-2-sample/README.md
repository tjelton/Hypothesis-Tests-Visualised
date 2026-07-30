# 2-Sample t-Test

The test compares two independent samples. A "Same Spread" toggle switches
between a **pooled-variance** 2-sample t-test and a **Welch** t-test, which
changes both the standard error and the degrees of freedom:

```
pooled:  SE = sp·sqrt(1/n1 + 1/n2),  sp = sqrt(((n1-1)s1² + (n2-1)s2²)/(n1+n2-2)),  df = n1+n2-2
Welch:   SE = sqrt(s1²/n1 + s2²/n2),  df = Welch–Satterthwaite formula
```

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/T-Tests/t-test-2-sample/
```

## Layout

The numeric engine, plots and CSS live in the sibling **`../../shared/`** directory
(see `../../shared/README.md`).

| File | Purpose |
|---|---|
| `index.html` | All static teaching content |
| `js/datasets.js` | Generated — the 6 datasets the loader offers (`tools/generate_datasets.R`) |
| `js/app.js` | Reactive wiring: the data loader, both box models, pooled/Welch chain, p-value, conclusion, CI |
| `../../shared/js/{stats,plots,boxmodel}.js`, `../../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3.

## Datasets

The synthetic `blood_pressure` set (`set.seed(1)`, two `rnorm` groups of 50),
plus `iris`, `InsectSprays`, `CO2`, `ToothGrowth`, `PlantGrowth` from R's
`datasets` package. The loader splits a chosen numeric (dependent) variable by a
chosen categorical variable into two samples. `tools/generate_datasets.R`
reproduces the exact values.

## Shared-code additions made for this lesson

* `plots.js` gained `boxplotPairSVG` (two-group side-by-side horizontal
  boxplot) and a `main` option on `qqPlotSVG`; its QQ clip-path id is now
  unique per SVG (this page shows two QQ plots at once). The existing
  horizontal-box drawing was factored into a shared helper — the other lessons
  are unaffected.

## Reference values

Default view (blood_pressure, Drug_A vs Drug_B, equal variance, two-sided):
TS = 2.6759, df = 98, p = 0.00874, CI = (1.2483, 8.4141) — confirmed against R.
(Because n1 = n2 = 50 the pooled and Welch SEs coincide, but the df and hence the
p-values differ slightly, as expected.)

## Behaviour notes

The test statistic uses the **full-precision** SE and means, rounded only at the
final step to 4 dp; the p-value then uses that rounded TS string with the
**full-precision** df.

## Known issues

* **The p-value t-curve plot is drawn with `df − 1`** while the p-value itself
  uses the full `df`, so the shaded curve does not quite match the number beside
  it. Visual only, but it is the plot students read.
* **The intro modal's Sample 2 box mislabels its stats with subscript 1**
  (s₁/OV₁/n₁). The main-page Sample 2 box is correctly subscripted.
* The conclusion says "accept the null hypothesis" rather than "fail to reject".
* The p-value section's last paragraph says the test statistic falls on a
  "standard normal curve" rather than a t-curve.
* Typos: "eqaul variance" in the spread decision message, "vlaues" and
  "obervation" in the assumption text.

## Tests

```sh
# End-to-end calc chain (pooled AND Welch) vs R + plot generation + app.js load:
osascript -l JavaScript T-Tests/t-test-2-sample/tests/smoke_jxa.js
```

`tests/expected_pipeline.json` is R-computed ground truth for the default
selection under both spread choices and all three alternatives
(`tools/generate_expected_pipeline.R`). The shared `stats.js` accuracy is
verified by `../../shared/tests/`.

## Deploying

Per `CLAUDE.md`: copy `T-Tests/t-test-2-sample/` to the
**`gh-pages` branch as a top-level dir**, alongside `shared/` and the other
lessons, served at
`https://tjelton.github.io/Hypothesis-Tests-Visualised/T-Tests/t-test-2-sample/`.
