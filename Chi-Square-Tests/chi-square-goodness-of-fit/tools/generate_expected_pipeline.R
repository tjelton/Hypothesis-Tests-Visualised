# Computes the chi-square goodness-of-fit calculation chain in R for the two
# scenarios exercised by tests/smoke_jxa.js:
#   * "die"    -- the lesson's default (6 fair-die faces, observed 8/12/9/11/15/5)
#   * "reject" -- an unequal-null example whose expected counts fail Cochran's
#                 rule and whose p-value rejects, exercising the other branches.
# The rounding chain matches app.js: TS = as.character(round(sum contributions,
# 4)), p = as.character(round(upper-tail chi-square at as.numeric(TS), 5)).
# Run from the repo root:
#   Rscript chi-square-goodness-of-fit/tools/generate_expected_pipeline.R

library(jsonlite)

chain <- function(observed, null_props) {
  k <- length(observed)
  n <- sum(observed)
  # Expected counts rounded to 2 dp, so the test-statistic table is
  # self-consistent (every O - E and (O - E)^2 / E follows from the shown E).
  expected <- round(n * null_props, 2)
  diff <- observed - expected
  contrib <- diff^2 / expected
  ts_string <- as.character(round(sum(contrib), 4))
  df <- k - 1
  p_string <- as.character(round(pchisq(as.numeric(ts_string), df, lower.tail = FALSE), 5))
  num_lt5 <- sum(expected < 5)
  cochran_holds <- all(expected > 0) && (num_lt5 <= 0.2 * k)
  list(
    k = k, n = n,
    observed = observed,
    null_props = as.character(round(null_props, 5)),
    expected = as.character(round(expected, 4)),
    diff = as.character(round(diff, 4)),
    contrib = as.character(round(contrib, 4)),
    ts = ts_string,
    df = df,
    p = p_string,
    num_lt5 = num_lt5,
    cochran_holds = cochran_holds
  )
}

out <- list(
  die = chain(c(8, 12, 9, 11, 15, 5), rep(1 / 6, 6)),
  reject = chain(c(2, 3, 40, 5), c(0.45, 0.45, 0.05, 0.05))
)

write(toJSON(out, digits = NA, auto_unbox = TRUE, pretty = TRUE),
      "Chi-Square-Tests/chi-square-goodness-of-fit/tests/expected_pipeline.json")
cat("Wrote chi-square-goodness-of-fit/tests/expected_pipeline.json\n")
