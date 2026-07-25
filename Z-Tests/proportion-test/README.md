# Proportion Test (z-test) — pure JavaScript port

A dependency-light static-page rewrite of the Shiny "Proportion Test (z-test)"
lesson (`R/ztest_proportion_test_*.R`). No webR/shinylive: plain HTML + JS.

Tests whether an observed proportion is consistent with a hypothesised
proportion `p0`. Inputs are entered manually (no dataset): the null proportion,
the sample size `n`, and the observed value `OV`. The box holds "1"/"0" tickets
in the null proportion, and the test uses the standard normal distribution:

```
sum:  EV = n·p0,  SE = √n·√(p0(1−p0))
mean: EV = p0,    SE = √(p0(1−p0))/√n
z = (OV − EV) / SE;  p-value from pnorm;  CI = Wilson score interval
```

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/Z-Tests/proportion-test/
```

## Layout

The numeric engine, plots, box-model helper and CSS live in **`../../shared/`**.

| File | Purpose |
|---|---|
| `index.html` | Static teaching content (ported verbatim from the R UI file) |
| `js/app.js` | Inputs + proportion box model + normal chain + Wilson CI |
| `../../shared/js/{stats,plots,boxmodel}.js`, `../../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3. No new
shared code was needed — `shadedNormalCurveSVG` and `densityHistogramSVG` (with
its normal-curve overlay) already existed.

## Behaviour notes

* The box tickets are simplified from `p0` via the GCD: e.g. `p0 = 0.7` →
  "1, 1, 1, 1, 1, 1, 1, 0, 0, 0"; `p0 = 0.5` → "1, 0"; `p0 = 0.4` →
  "1, 1, 0, 0, 0". When it won't simplify (GCD < 5, e.g. `p0 = 0.73`) it falls
  back to a percentage form: "1 x 73%, 0 x 27%".
* Assumption 3 shows a 10,000-sample empirical distribution (random) with the
  fitted normal curve overlaid — demonstrating the CLT for the chosen box.

## Fidelity to the Shiny version

* Default inputs (p0 = 0.7, n = 30, OV = 0.73, mean): EV = 0.7, SE = 0.08367,
  TS = 0.3586, p (two-sided) = 0.71989, Wilson CI = (0.5521, 0.8557) — confirmed
  against R. Since 0.7 lies inside the CI and p > 0.05, both conclude "fail to
  reject", consistent with each other.
* The confidence interval is the **Wilson score interval** (not the naïve
  Wald interval), matching the R source.
* Teaching text is verbatim, including the R source's typos ("Hypotheis",
  "porportion" in input ids, and the "Model Using Sum or Sample" heading).

### Faithful quirk

The observed-value input is bounded to `[0, 1]` even in "sum" mode (where a sum
could exceed 1), exactly as the R app constrains it. In sum mode the test
statistic is therefore only meaningful for small `EV`; the default "mean" mode
is the sensible one.

## Tests

```sh
osascript -l JavaScript Z-Tests/proportion-test/tests/smoke_jxa.js
```

`tests/expected_pipeline.json` is R-computed ground truth for the default inputs
under both box representations and the Wilson CI
(`tools/generate_expected_pipeline.R`). Shared `stats.js`/`plots.js` are covered
by `../../shared/tests/`.

## Deploying

Per `CLAUDE.md`: copy `Z-Tests/proportion-test/` to the **`gh-pages` branch as a
top-level dir**, alongside `shared/` and the other lessons.
