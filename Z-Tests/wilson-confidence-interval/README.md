# Misc: Understanding the Wilson's Confidence Interval

A conceptual explainer, not a hypothesis test: there is **no null hypothesis, no
test statistic and no p-value** on this page. It exists to answer one question —
where do the two endpoints of a Wilson confidence interval come from?

The interval is presented by its defining property. For a candidate proportion
`p`, the C% prediction interval for the sample proportion is

```
p ± z · √(p(1−p)/n),    z = qnorm(1 − alpha/2),  alpha = 1 − C/100
```

A candidate is *plausible* when that interval contains the observed `p̂ = x/n`,
so the confidence interval is the set of plausible candidates and its endpoints
are the two candidates whose prediction interval only just reaches `p̂`:

```
lower endpoint:  p + z·√(p(1−p)/n) = p̂     (its UPPER limit lands on p̂)
upper endpoint:  p − z·√(p(1−p)/n) = p̂     (its LOWER limit lands on p̂)
```

Squaring either condition gives the same quadratic `(p̂ − p)² = z²p(1−p)/n`,
whose two roots are the closed-form Wilson bounds. Students find those two
candidates by hand with the sliders; the closed form is then revealed as the
algebra that skips the searching.

Note that the standard error uses the **candidate** `p`, not `p̂`. That is the
whole difference from the Wald interval `p̂ ± z√(p̂(1−p̂)/n)`, and it is why the
result is asymmetric about `p̂` — a candidate nearer 0.5 has a wider prediction
interval, so it can sit further from `p̂` and still reach it.

## The interaction

`x`, `n` and the confidence level are free inputs (defaults 9, 30, 95%, giving
`p̂ = 0.30` and the interval (0.1666, 0.4788)).

**Why those defaults.** The two prediction intervals differ in width by exactly
the factor by which the interval is off-centre — each endpoint sits `z·SE` from
`p̂`, so the SE ratio *is* the asymmetry ratio, and the two cannot be tuned
separately. A `p̂` near 0 or 1 makes one curve enormously fatter than the other
(at 3/20 the right one is 2.16× the left) and the plot reads as lopsided; a `p̂`
near 0.5 balances the curves but flattens the asymmetry that distinguishes
Wilson from Wald. At 9/30 the ratio is 1.34: the curves look comparable in
heft, while the interval is still plainly not centred on `p̂` (0.133 to the
left, 0.179 to the right).

One plot carries a single proportion axis, with `p̂` drawn as a ★ star on it and
a normal prediction curve for each of the two candidates. Two sliders of 10
increments each move the candidates, and each prediction interval is shaded by a
traffic light:

| Colour | Meaning | Where the candidate sits |
|---|---|---|
| **Red** | the prediction interval does not reach the star | outside the confidence interval |
| **Yellow** | it covers the star, but the star is not at its end | inside the interval, but not an endpoint |
| **Green** | the star sits exactly on the relevant end | *is* an endpoint |

Both sliders are green only at **tick 5**, at which point the answer box
appears. It opens with a sketch — the two green curves with the finished
interval drawn beneath them, a dashed riser tying each endpoint to the centre
of the curve it came from — then says why those endpoints are the edge, and
offers two collapsed boxes for getting the same numbers without the searching:
**Method 1** solves the quadratic to the closed form, **Method 2** runs
`binom::binom.confint(..., methods = "wilson")` (with `prop.test(...,
correct = FALSE)` as the base-R equivalent).

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/Z-Tests/wilson-confidence-interval/
```

## Layout

The numeric engine, plots and CSS live in **`../../shared/`**.

| File | Purpose |
|---|---|
| `index.html` | Static teaching content: the logic, the inputs, the task and colour key |
| `js/app.js` | Slider geometry, the traffic light, and the revealed answer |
| `tools/generate_expected_pipeline.R` | Regenerates `tests/expected_pipeline.json` |
| `tests/expected_pipeline.json` | R's values for the default example |
| `tests/smoke_jxa.js` | JXA smoke test (see below) |
| `../../shared/js/{stats,plots}.js`, `../../shared/css/style.css` | Shared engine |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3.

## Shared-code additions made for this lesson

`plots.js` gained `wilsonPredictionSVG(pHat, curves, xdomain, opts)`: one
proportion axis carrying the star, plus a shaded normal prediction curve per
candidate, coloured by its traffic-light state. Curves are clipped to the frame
and an optional `ymax` lets the caller hold the vertical scale fixed.

## Behaviour notes

* **Slider geometry.** Each slider is split at tick 5, so tick 5 is the exact
  Wilson endpoint whatever the ends are: ticks 0–5 interpolate from the outer
  end to the endpoint, ticks 5–10 from the endpoint inward to `p̂`. Both sliders
  run left-to-right in the same direction as the plot's axis, so moving a handle
  right moves its candidate right.
* **The outer ends** are the endpoints mirrored through `p̂`. For a `p̂` near 0
  or 1 the mirror falls outside `[0, 1]`, so it is pulled back to halfway
  between the endpoint and the boundary. At exactly 0 or 1 the SD is 0 and there
  is no curve to draw, so the outer end must stay a genuine proportion — which
  is why the left slider's lower half can be compressed. The smoke test sweeps
  `n` from 5 to 2000 and confidence levels 50–99% to confirm the outer ends stay
  inside `(0, 1)` and the colours still read red → green → yellow.
* **The axis and the vertical scale are computed over every slider position, not
  just the current one**, so neither rescales while a slider is dragged. A
  moving axis would make "is the end on the star?" impossible to judge, which is
  the only thing the student is being asked to do.
* **`x` must be strictly inside `(0, n)`.** At `x = 0` or `x = n` the observed
  proportion sits on a boundary, one endpoint collapses onto it, that side's
  curve has zero spread, and there is nothing left to line up. Rejected inputs
  are rolled back with a message rather than silently clamped.
* **Changing any input resets both sliders** to their outer ends, so the new
  endpoints have to be found rather than handed over.
* A prediction interval can extend past 0 or 1 — not on the defaults, but
  readily: at 3/20 the lower endpoint's prediction interval runs to −0.0453.
  That is the normal approximation showing its limits; the page says so and it
  does not affect the endpoints being sought.
* Green is decided by comparing the aimed-at prediction limit against `p̂` with
  a tolerance of `1e-9`, not by testing for tick 5. At tick 5 the candidate *is*
  the closed-form root, so the two agree to floating-point noise, and nothing
  else on either slider comes close.

## Tests

```sh
Rscript Z-Tests/wilson-confidence-interval/tools/generate_expected_pipeline.R   # regenerate fixture
osascript -l JavaScript Z-Tests/wilson-confidence-interval/tests/smoke_jxa.js   # run smoke test
```

Regenerating needs the `binom` package (`install.packages("binom")`) — the
fixture records `binom::binom.confint(method = "wilson")` because the answer
box's "Method 2" tells students to run it, so the numbers the page quotes are
checked against the function it names. The smoke test itself needs nothing
beyond stock macOS.

The smoke test checks the endpoints and the defining prediction-limit identity
to 10 dp against R (and against R's own `prop.test(x, n, correct = FALSE)`),
pins all 22 slider tick values, sweeps the traffic light over a wide range of
`x`, `n` and confidence levels, exercises the plot generator, and drives
`app.js` against a DOM stub — including that the answer box stays hidden until
both sliders are green, hides again when one leaves, and tracks changed inputs.
