# Hypothesis Tests Visualised Project (HTVP)

The purpose of HTVP is to create bite-sized, interactive experiences that enable students to build their conceptual understanding of hypothesis tests and to see hypothesis testing in action.
These webpages are NOT designed to be a replacement for traditional forms of learning (e.g. lectures, textbooks, lab tutorials); instead, they are designed to enhance existing modes of teaching
by facilitating active learning.

## This branch: the JavaScript rewrite

This branch (`Javascript-Convert`) is a full rewrite of HTVP as a **dependency-light static website** — plain HTML + hand-written JavaScript, with no webR/shinylive and no build step. Each lesson loads like any normal web page.

The original **R Shiny / shinylive** implementation is preserved on the [`main`](../../tree/main) and [`shiny_live_version`](../../tree/shiny_live_version) branches (and in git history).

### Why the rewrite?

The shinylive build had a slow cold start (~30s), most of which is the webR/WASM runtime booting R in the browser on every visit. A plain static site removes that floor entirely — the pages load instantly, with no interpreter to boot.

## Running locally

The site is fully static, so any static file server works. From the repo root:

```sh
python3 -m http.server 8000
# then open http://localhost:8000/
```

The home page (`index.html`) links to every lesson. (Opening files via `file://` also works, but the CDN assets — Bootstrap and MathJax — need network access either way.)

## Structure

```
/                     -> home page (index.html) + shared site navbar
/shared/              -> common engine used by every lesson
    js/stats.js         R-equivalent numerics (pt/qt/dt/pnorm/qnorm, linreg, ...)
    js/plots.js         SVG re-implementations of the base-R plots
    js/boxmodel.js      the box-model / sample-cell helpers
    js/navbar.js        the shared navigation bar (injected on every page)
    css/style.css       shared theme (Bootswatch Lumen)
    tests/              stats-accuracy suite (verified against R)
/t-test-1-sample/     -> one directory per lesson, each with its own
/t-test-paired/          index.html, js/app.js, js/datasets.js (where needed),
/box-model-part-1/       and a lesson-chain smoke test in tests/
...
```

Every lesson references the shared engine with `../shared/…` relative paths, so the same files work both locally and when deployed under a subpath. Edits to `/shared/` affect all lessons at once.

The eleven lessons: box model parts 1–3 and confidence intervals (Fundamentals); 1-sample and proportion z-tests (Z-Tests); t-curve motivation, 1-sample, paired, 2-sample and regression t-tests (T-Tests).

## Fidelity and testing

The JavaScript reproduces R's statistical functions closely (the distribution/quantile routines match R to ~1e-10 or better), and every lesson's displayed numbers were checked against values computed in R.

The `tools/generate_*.R` scripts in `/shared/` and each lesson regenerate the R-computed ground truth used by the tests; they are development tooling and are not needed to run the site. Tests run on stock macOS (no Node required):

```sh
# shared numeric-accuracy suite
osascript -l JavaScript shared/tests/run_jxa.js
# a lesson's end-to-end calculation chain (one per lesson)
osascript -l JavaScript t-test-1-sample/tests/smoke_jxa.js
```

## Found an Issue?

Did you find a bug? Is one of the statistical explanations incorrect, or is the math not quite what you would expect?

I'd apprecaite you letting me know! Please create a Github issue.

## Contributing

Thank you for your interest! At this time, this project is not accepting contributions.  Please feel free to open issues for bug reports or feature requests. Contributions may be considered in the future.

## AI Disclaimer

The project was created with the assistance of generative AI tools. This includes coding and content writing.
