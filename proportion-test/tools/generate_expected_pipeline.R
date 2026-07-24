# Computes the proportion-test (z-test) calculation chain in R exactly as
# R/ztest_proportion_test_srv.R does, for the default inputs: null proportion
# p0 = 0.7, sample size n = 30, observed value OV = 0.73. Covers both the
# sum and mean box representations, all three alternatives, and the Wilson
# score confidence interval. Output feeds tests/smoke_jxa.js.
# Run from the repo root: Rscript proportion-test/tools/generate_expected_pipeline.R

library(jsonlite)

p0 <- 0.7; n <- 30; ov <- 0.73
sd_ <- sqrt(p0 * (1 - p0))

chain <- function(mode) {                       # mode: "sum" | "mean"
  if (mode == "mean") { EV <- p0; SE <- sd_ / sqrt(n) }
  else { EV <- n * p0; SE <- sqrt(n) * sd_ }
  EV_string <- as.character(round(EV, 5))
  SE_string <- as.character(round(SE, 5))
  ts <- as.numeric(as.character(round((ov - as.numeric(EV_string)) / as.numeric(SE_string), 4)))
  list(
    EV = EV_string, SE = SE_string,
    ts = as.character(round((ov - as.numeric(EV_string)) / as.numeric(SE_string), 4)),
    p_two = as.character(round(2 * (1 - pnorm(abs(ts))), 5)),
    p_greater = as.character(round(1 - pnorm(ts), 5)),
    p_less = as.character(round(pnorm(ts), 5))
  )
}

wilson <- function(p_hat, n, z) {
  denom <- 1 + z^2 / n
  margin <- z * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))
  list(lower = (p_hat + z^2 / (2 * n) - margin) / denom,
       upper = (p_hat + z^2 / (2 * n) + margin) / denom)
}
z2 <- qnorm(1 - 0.05 / 2); z1 <- qnorm(1 - 0.05)
ci_two <- wilson(ov, n, z2)
ci_one <- wilson(ov, n, z1)

out <- list(
  n = n, p0 = "0.7", ov = "0.73",
  mean = chain("mean"), sum = chain("sum"),
  ci_two_lower_4 = as.character(round(ci_two$lower, 4)),
  ci_two_upper_4 = as.character(round(ci_two$upper, 4)),
  ci_greater_lower_4 = as.character(round(ci_one$lower, 4)),
  ci_less_upper_4 = as.character(round(ci_one$upper, 4))
)

write(toJSON(out, digits = NA, auto_unbox = TRUE, pretty = TRUE),
      "proportion-test/tests/expected_pipeline.json")
cat("Wrote proportion-test/tests/expected_pipeline.json\n")
