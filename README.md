# Hypothesis Tests Visualised Project (HTVP)

The purpose of HTVP is to create bite-sized, interactive experiences that enable students to build their conceptual understanding of hypothesis tests and to see hypothesis testing in action.
These webpages are NOT designed to be a replacement for traditional forms of learning (e.g. lectures, textbooks, lab tutorials); instead, they are designed to enhance existing modes of teaching
by facilitating active learning.

## The site

HTVP is a **dependency-light static website** — plain HTML + hand-written JavaScript, with no build step. Each lesson loads like any normal web page.

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
    js/stats.js         Numerics matching R (pt/qt/dt/pnorm/qnorm, linreg, ...)
    js/plots.js         SVG implementations of the plots
    js/boxmodel.js      the box-model / sample-cell helpers
    js/navbar.js        the shared navigation bar (injected on every page)
    css/style.css       shared theme (Bootswatch Lumen)
    tests/              stats-accuracy suite (verified against R)
/Fundamentals/        -> one directory per category, and inside it one
/Z-Tests/                directory per lesson (e.g. /T-Tests/t-test-paired/),
/T-Tests/                each with its own index.html, js/app.js,
/Chi-Square-Tests/       js/datasets.js (where needed), and a lesson-chain
                         smoke test in tests/
/worksheets/          -> the guiding-questions pages, mirroring that structure
```

Every lesson references the shared engine with `../../shared/…` relative paths, so the same files work both locally and when deployed under a subpath. Edits to `/shared/` affect all lessons at once.

The thirteen lessons: box model parts 1–3 and confidence intervals (Fundamentals); 1-sample and proportion z-tests (Z-Tests); t-curve motivation, 1-sample, paired, 2-sample and regression t-tests (T-Tests); goodness of fit and test of independence (Chi-Square-Tests).

## Testing

R is the ground truth for every number the site displays. The distribution and quantile routines in `/shared/js/stats.js` match R to ~1e-10 or better, and each lesson's displayed values are checked against values computed in R.

The `tools/generate_*.R` scripts in `/shared/` and each lesson regenerate that ground truth; they are development tooling and are not needed to run the site. Tests run on stock macOS (no Node required):

```sh
# shared numeric-accuracy suite
osascript -l JavaScript shared/tests/run_jxa.js
# a lesson's end-to-end calculation chain (one per lesson)
osascript -l JavaScript T-Tests/t-test-1-sample/tests/smoke_jxa.js
```

## Deploying

The live site is served from the `gh-pages` branch, which mirrors `main` exactly. To publish the current `origin/main` (plus a `.nojekyll` marker), from the repo root:

```sh
bash tools/deploy_gh_pages.sh
```

The script builds the deploy commit with git plumbing, so it never switches branches or touches your working tree, and it fast-forwards `gh-pages` rather than force-pushing.

## Found an Issue?

Did you find a bug? Is one of the statistical explanations incorrect, or is the math not quite what you would expect?

I'd apprecaite you letting me know! Please create a Github issue.

## Contributing

Thank you for your interest! At this time, this project is not accepting contributions.  Please feel free to open issues for bug reports or feature requests. Contributions may be considered in the future.

## AI Disclaimer

The project was created with the assistance of generative AI tools. This includes coding and content writing.
