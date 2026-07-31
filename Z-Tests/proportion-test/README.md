# Proportion Test (z-test)

Tests whether an observed proportion is consistent with a hypothesised
proportion `p0`. Inputs are entered manually (no dataset): the null proportion,
the sample size `n`, and the observed **count** `x`. The box holds "1"/"0"
tickets in the null proportion, and the test uses the standard normal
distribution:

```
OV = x / n
EV = p0,  SE = √(p0(1−p0)) / √n
z  = (OV − EV) / SE;  p-value from pnorm;  CI = Wilson score interval
```

The box is always modelled by the sample **mean**. For 0/1 tickets the mean of
the draws *is* the observed proportion, which is what the hypotheses are stated
in, so there is nothing for a "sum" representation to add.

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/Z-Tests/proportion-test/
```

## Layout

The numeric engine, plots, box-model helper and CSS live in **`../../shared/`**.

| File | Purpose |
|---|---|
| `index.html` | Static teaching content |
| `js/app.js` | Inputs + proportion box model + normal chain + Wilson CI |
| `../../shared/js/{stats,plots,boxmodel}.js`, `../../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3. No new
shared code was needed — `shadedNormalCurveSVG` and `densityHistogramSVG` (with
its normal-curve overlay) already existed.

## Behaviour notes

* The observed value is entered as a **count**, and `OV = x/n` is computed
  exactly. Entering a pre-rounded proportion used to matter: for 22/30, "0.73"
  versus 0.7333 moved the two-sided p-value from 0.6903 to 0.7199.
* The box tickets are simplified from `p0` via the GCD: e.g. `p0 = 0.7` →
  "1, 1, 1, 1, 1, 1, 1, 0, 0, 0"; `p0 = 0.5` → "1, 0"; `p0 = 0.4` →
  "1, 1, 0, 0, 0". When it won't simplify (GCD < 5, e.g. `p0 = 0.73`) it falls
  back to a percentage form: "1 x 73%, 0 x 27%".
* Assumption 3 shows a 10,000-sample empirical distribution (random) with the
  fitted normal curve overlaid, plus a live check of `n·p ≥ 10` and
  `n·(1−p) ≥ 10` against the current inputs. The page presents these as a
  general rule and does not give them a name.
* **The histogram bins are aligned to the lattice.** A sample mean of 0/1
  tickets can only take the values `s/n`, so bin edges sit at `(s−0.5)/n` and
  every bin is an exact integer number of lattice steps wide. Equal-width bins
  across the observed range do *not* work: at `n = 50` that gives a width of
  0.024 against a lattice spacing of 0.02, so every fifth bin collects two
  attainable values and the rest one. The resulting comb made a bell-shaped
  distribution read as bimodal — the opposite of what the plot is for. Bins are
  grouped (`step > 1`) once the range exceeds 40 lattice points, keeping the
  count sane for large `n`.
* `σ = √(p0(1−p0))` is derived on the page rather than appearing as a bare
  number. The point made to students is that the null hypothesis *fixes* the
  spread of the box — it is not estimated from the sample.
* The confidence level is **derived as `1 − α`**, not entered separately, so the
  p-value and confidence-interval conclusions cannot describe different tests.
* Invalid input keeps the last good value rather than snapping to the page
  default, so a half-typed number does not silently change the maths.

## Reference values

Default inputs (`p0 = 0.7`, `n = 50`, `x = 37`, so `OV = 0.74`):

| Quantity | Value |
|---|---|
| `n·p0`, `n·(1−p0)` | 35, 15 — both ≥ 10 |
| `σ` | 0.45826 |
| EV, SE | 0.7, 0.06481 |
| TS | 0.6172 |
| p (two-sided) | 0.5371 |
| Wilson CI (95%) | (0.6045, 0.8413) |

Confirmed against R, including `prop.test(37, 50, p = 0.7, correct = FALSE)`,
which reproduces both the p-value (0.53709 unrounded) and the interval exactly.
Since 0.7 lies inside the CI and p > 0.05, both routes conclude "fail to
reject".

The example was chosen so that both normal-approximation inequalities are
comfortably satisfied. The previous default (`n = 30`, `p0 = 0.7`) gave
`n·(1−p0) = 9`, sitting just under the threshold the page now states.

The confidence interval is the **Wilson score interval**, not the naïve Wald
interval. It is not named in the student-facing text — the page says only that
this interval stays inside 0 to 1 and agrees with the p-value, and leaves the
algebra alone. Two reasons it is the right choice here:

* It inverts the *same* score test the page computes the TS from, so the two
  conclusions agree exactly. Wald does not: at the observed value where the
  two-sided p-value is exactly 0.05, the Wilson bound lands on `p0` while the
  Wald bound is off by 0.04.
* It cannot leave `[0, 1]`. At `OV = 0.95`, `n = 30`, Wald's upper bound is
  1.028 — an impossible proportion.

One-sided intervals are reported as `(lower, 1]` and `[0, upper)` rather than
running off to ±∞.

## Known issues

* The empirical-distribution histogram is re-simulated on every input change, so
  the CLT picture jumps around while typing.
* The lesson does not apply a continuity correction, matching
  `prop.test(correct = FALSE)` and the usual textbook z-test. At small `n` this
  makes the test somewhat anti-conservative: for 22/30 against `p0 = 0.7` the
  uncorrected p-value is 0.6903 where the exact binomial gives 0.8427 (the
  corrected normal approximation gives 0.8421). The distribution's *shape* is
  fine — this is discreteness, not a CLT failure.
* The worksheet at `../../worksheets/Z-Tests/proportion-test/` is still an
  unwritten skeleton.

## Tests

```sh
osascript -l JavaScript Z-Tests/proportion-test/tests/smoke_jxa.js
```

`tests/expected_pipeline.json` is R-computed ground truth for the default inputs
and the Wilson CI (`tools/generate_expected_pipeline.R`, run from the repo root).
Beyond matching those strings, the smoke test sweeps every count `0..n` at four
significance levels across all three alternatives and asserts the p-value and
confidence-interval conclusions never disagree — the equivalence the page now
claims to students. Shared `stats.js`/`plots.js` are covered by
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

Served at `https://tjelton.github.io/Hypothesis-Tests-Visualised/Z-Tests/proportion-test/`.
