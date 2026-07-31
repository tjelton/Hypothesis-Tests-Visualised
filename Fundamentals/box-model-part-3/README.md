# The Box Model Part 3 (Modelling Using a Normal Distribution)

A **box-model playground**: configure tickets, number of draws and sum/mean,
then work through the pipeline that turns a box into probability answers:

1. **Box parameters** → the box model plus its mean `μ` and population SD `σ`.
2. **Central Limit Theorem** — build a growing histogram of sample sums/means to
   check the CLT applies at the chosen `n` (Repeat 1/10/25/100, Reset).
3. **Modelling normal** — specify `N(EV, SE²)` (EV/SE derived by sum or mean),
   with a 10,000-sample histogram and the fitted normal curve overlaid.
4. **Finding probabilities** — a shaded `N(EV, SE²)` curve and the area between a
   lower/upper boundary (each optionally ±∞).

Simulation draws are random (`Math.random`); the EV/SE and probability
arithmetic is deterministic and matches R.

```
sum:  EV = n·mean(tickets),  SE = √n·popsd(tickets)
mean: EV = mean(tickets),    SE = popsd(tickets)/√n
```

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/Fundamentals/box-model-part-3/
```

## Layout

The numeric engine, plots, box-model helper and CSS live in **`../../shared/`**.

| File | Purpose |
|---|---|
| `index.html` | Static teaching content |
| `js/app.js` | Box playground + CLT + normal model + probability wiring |
| `../../shared/js/{stats,plots,boxmodel}.js`, `../../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3.

## Shared-code additions made for this lesson

* `stats.js` gained `popsd` (population SD, denominator n).
* `plots.js` gained `shadedNormalRegionSVG` (a general `N(ev, se²)` curve with a
  shaded region and EV±k·SE ticks) and a `curve: {ev, se}` overlay option on
  `densityHistogramSVG`. The other lessons are unaffected.

## Reference values

The deterministic pipeline is confirmed against R: the coin box (`1,0`, n = 100,
sum) gives EV = 50, SE = 5, P(sum ≥ 60) = 0.02275 and P(sum ≥ 70) = 3e-05 — the
answers the lesson text quotes.

## Known issues

Unfixed typos in the teaching text: "Empiricial", "wthin", "seperated", and
"3e-05" written inline rather than as a rounded probability.

## Tests

```sh
osascript -l JavaScript Fundamentals/box-model-part-3/tests/smoke_jxa.js
```

`tests/expected_pipeline.json` is R-computed ground truth for the EV/SE and
several tail/interval probabilities (`tools/generate_expected_pipeline.R`); the
histograms themselves are random and not asserted. Shared `stats.js`/`plots.js`
are covered by `../../shared/tests/`.

## Deploying

The live site is served from the `gh-pages` branch, which mirrors `main` exactly
— every lesson keeps the same path it has here. `tools/deploy_gh_pages.sh`
publishes `origin/main` verbatim (plus a `.nojekyll` marker), so there is nothing
to copy per lesson and no separate step for `../../shared/` or the root
`index.html`:

```sh
bash tools/deploy_gh_pages.sh
```

Served at `https://tjelton.github.io/Hypothesis-Tests-Visualised/Fundamentals/box-model-part-3/`.
