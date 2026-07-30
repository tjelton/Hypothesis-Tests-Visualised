# The Box Model Part 1

This is a **conceptual, simulation-based explainer** (no hypothesis-test chain).
It introduces the box model through coin flips, with three interactive demos:

1. **Coin flips (words):** simulate 5 flips, shown as `H/T` sequences.
2. **Numeric tickets:** the same box with `1`/`0` tickets, summarised by sum or mean.
3. **Your own box:** enter arbitrary comma-separated tickets and a number of
   draws, then simulate sums/means.

Each demo keeps a rolling grid of the last 10 results: the first 10 simulations
append, and subsequent ones overwrite the oldest slot. The draws are random
(`Math.random`), so results differ from run to run.

## Running locally

```sh
python3 -m http.server 8000
# then open http://localhost:8000/Fundamentals/box-model-part-1/
```

## Layout

The box-model helpers and CSS live in the sibling **`../../shared/`** directory.

| File | Purpose |
|---|---|
| `index.html` | All static teaching content |
| `js/app.js` | The three simulation demos + rolling-grid logic |
| `../../shared/js/boxmodel.js` | `boxModelHTML`, `sampleCellHTML`, `sampleGridHTML` |
| `../../shared/css/style.css` | Shared theme |

External assets (CDN): Bootstrap 5 (Bootswatch Lumen) and MathJax 3.

## Shared-code additions made for this lesson

* `boxmodel.js` gained `sampleCellHTML` (one yellow rounded cell) and
  `sampleGridHTML` (a 2-column grid of cells). The other lessons are unaffected.

## Known issues

Unfixed typos in the teaching text: "ticekts", "seperated".

## Tests

```sh
osascript -l JavaScript Fundamentals/box-model-part-1/tests/smoke_jxa.js
```

Checks the box-model helpers render as expected and that `app.js` loads cleanly
against a DOM stub (there is no numeric ground truth for a random simulation).

## Deploying

Per `CLAUDE.md`: copy `Fundamentals/box-model-part-1/` to the
**`gh-pages` branch as a top-level dir**, alongside `shared/` and the other
lessons.
