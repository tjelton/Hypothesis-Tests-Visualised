# The Box Model Part 3 (Modelling Using a Normal Distribution) — pure JavaScript port

A dependency-light static-page rewrite of the Shiny "Box Model Part 3" lesson
(`R/fundamentals_box_model_part_3_*.R`). No webR/shinylive: plain HTML + JS.

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
| `index.html` | Static teaching content (ported verbatim from the R UI file) |
| `js/app.js` | Box playground + CLT + normal model + probability wiring |
| `../../shared/js/{stats,plots,boxmodel}.js`, `../../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3.

## Shared-code additions made for this lesson

* `stats.js` gained `popsd` (population SD, denominator n).
* `plots.js` gained `shadedNormalRegionSVG` (a general `N(ev, se²)` curve with a
  shaded region and EV±k·SE ticks) and a `curve: {ev, se}` overlay option on
  `densityHistogramSVG`. The other lessons are unaffected.

## Fidelity to the Shiny version

* The deterministic pipeline matches R exactly: e.g. the coin box (`1,0`,
  n = 100, sum) gives EV = 50, SE = 5, and P(sum ≥ 60) = 0.02275, P(sum ≥ 70) =
  3e-05 — the answers the lesson text quotes.
* Teaching text is verbatim, including the R source's typos ("Empiricial",
  "wthin", "seperated", "3e-05").

## Tests

```sh
osascript -l JavaScript Fundamentals/box-model-part-3/tests/smoke_jxa.js
```

`tests/expected_pipeline.json` is R-computed ground truth for the EV/SE and
several tail/interval probabilities (`tools/generate_expected_pipeline.R`); the
histograms themselves are random and not asserted. Shared `stats.js`/`plots.js`
are covered by `../../shared/tests/`.

## Deploying

Per `CLAUDE.md`: copy `Fundamentals/box-model-part-3/` to the **`gh-pages` branch as a
top-level dir**, alongside `shared/` and the other lessons.
