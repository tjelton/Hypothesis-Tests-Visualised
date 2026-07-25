# Computes R ground truth for the t-curve motivation lesson's Demo 2 p-values,
# exactly as R/ttest_t_curve_motivation_srv.R does:
#   normal p = 2 * (1 - pnorm(|ts|))
#   t-curve p = 2 * (1 - pt(|ts|, df))
# both displayed as as.character(round(p, 5)). Output feeds tests/smoke_jxa.js.
# Run from the repo root: Rscript t-curve-motivation/tools/generate_expected_pipeline.R

library(jsonlite)

# A spread of (test statistic, df) combinations, including the app defaults
# (ts = 1, df = 1) and larger df where the t-curve approaches the normal.
cases <- list(
  list(ts = 1,    df = 1),
  list(ts = 1,    df = 10),
  list(ts = 2.5,  df = 5),
  list(ts = -1.96, df = 30),
  list(ts = 3.2,  df = 50)
)

out <- lapply(cases, function(c) {
  list(
    ts = c$ts,
    df = c$df,
    p_normal = as.character(round(2 * (1 - pnorm(abs(c$ts))), 5)),
    p_t = as.character(round(2 * (1 - pt(abs(c$ts), df = c$df)), 5))
  )
})

write(toJSON(out, digits = NA, auto_unbox = TRUE, pretty = TRUE),
      "T-Tests/t-curve-motivation/tests/expected_pipeline.json")
cat("Wrote t-curve-motivation/tests/expected_pipeline.json\n")
