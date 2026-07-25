# Computes the chi-square test-of-independence calculation chain in R for the
# two scenarios exercised by tests/smoke_jxa.js:
#   * "coffee" -- the lesson's 2x2 default (reject; Cochran holds)
#   * "small"  -- a 2x3 table whose expected counts fail Cochran's rule and
#                 whose p-value fails to reject (exercises the other branches).
# The rounding chain matches app.js: expected counts rounded to 2 dp, every
# downstream cell computed from that rounded E, TS = round(sum, 4), and
# p = round(upper-tail chi-square at as.numeric(TS), 5).
# Run from the repo root:
#   Rscript chi-square-test-of-independence/tools/generate_expected_pipeline.R

library(jsonlite)

# Flatten a matrix in row-major order (the order app.js iterates cells).
rowmajor <- function(m) as.vector(t(m))

chain <- function(obs) {
  rt <- rowSums(obs); ct <- colSums(obs); g <- sum(obs)
  E <- round(outer(rt, ct) / g, 2)            # expected counts, 2 dp
  diff <- obs - E
  contrib <- diff^2 / E
  ts_string <- as.character(round(sum(contrib), 4))
  df <- (nrow(obs) - 1) * (ncol(obs) - 1)
  p_string <- as.character(round(pchisq(as.numeric(ts_string), df, lower.tail = FALSE), 5))
  n_cells <- length(E)
  num_lt5 <- sum(E < 5)
  holds <- all(E > 0) && (num_lt5 <= 0.2 * n_cells)
  list(
    rows = nrow(obs), cols = ncol(obs), grand = g,
    observed = rowmajor(obs),
    expected = as.character(round(rowmajor(E), 2)),
    diff = as.character(round(rowmajor(diff), 2)),
    contrib = as.character(round(rowmajor(contrib), 4)),
    ts = ts_string, df = df, p = p_string,
    num_lt5 = num_lt5, cochran_holds = holds
  )
}

out <- list(
  coffee = chain(matrix(c(50, 30, 20, 40), nrow = 2, byrow = TRUE)),
  small  = chain(matrix(c(2, 3, 1, 4, 3, 2), nrow = 2, byrow = TRUE))
)

write(toJSON(out, digits = NA, auto_unbox = TRUE, pretty = TRUE),
      "Chi-Square-Tests/chi-square-test-of-independence/tests/expected_pipeline.json")
cat("Wrote chi-square-test-of-independence/tests/expected_pipeline.json\n")
