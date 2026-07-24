# shared — common engine for the JavaScript ports

Code shared by every JS-port lesson (`t-test-1-sample/`, `t-test-paired/`, …).
Each lesson is deployed as its own top-level directory on the `gh-pages`
branch; `shared/` sits alongside them, so pages reference it with `../shared/…`
(which resolves both locally under `python3 -m http.server` from the repo root
and on GitHub Pages).

## Contents

| File | Purpose |
|---|---|
| `js/stats.js` | R-equivalent numerics: `pt`/`qt`/`dt`/`qnorm`, `sd`, quantile type 7, `fivenum`, `ppoints`, R-style `round()`/`as.character()`. Verified against R to ~1e-15. |
| `js/plots.js` | SVG re-implementations of the base-R plots (histogram, boxplot, QQ plot, shaded t-curve). Depends on the global `Stats`. |
| `js/boxmodel.js` | Port of the Shiny `box_model_html()` helper (blue box → arrow → yellow oval). |
| `css/style.css` | `tight_card()` look, body zoom, wide modal — mirrors the Shiny bslib theme. |
| `tests/stats.checks.js` | Runner-agnostic assertion suite for `stats.js`. |
| `tests/reference_values.json` | R-computed ground truth (generated). Also embeds the seeded sample fixture (`sample_stats.han_input`) so the suite needs no lesson data. |
| `tests/run_jxa.js` | Runs the suite on stock macOS (no Node). |
| `tests/stats.test.mjs` | Runs the suite under Node. |
| `tools/generate_reference_values.R` | Regenerates `tests/reference_values.json` from R. |

## How a lesson uses it

In the lesson's `index.html`:

```html
<link rel="stylesheet" href="../shared/css/style.css">
...
<script src="../shared/js/stats.js"></script>
<script src="../shared/js/boxmodel.js"></script>
<script src="../shared/js/plots.js"></script>
<script src="js/datasets.js"></script>   <!-- lesson-specific -->
<script src="js/app.js"></script>          <!-- lesson-specific -->
```

A lesson supplies only its own `datasets.js` (its data), `app.js` (its reactive
wiring), teaching content (`index.html`), and a lesson-chain smoke test.

## Tests

```sh
# Stock macOS (no Node):
osascript -l JavaScript shared/tests/run_jxa.js
# With Node:
node shared/tests/stats.test.mjs
```

Because `stats.js` is shared, its accuracy is tested **once** here rather than
per lesson. Each lesson's own `tests/smoke_jxa.js` covers that lesson's
calculation chain against R.
