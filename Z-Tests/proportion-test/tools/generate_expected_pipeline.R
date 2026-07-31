# Computes the proportion-test (z-test) calculation chain in R for the default
# inputs: null proportion p0 = 0.7, sample size n = 50, observed count x = 37
# (so OV = 37/50 = 0.74). Covers all three alternatives and the Wilson score
# confidence interval. Output feeds tests/smoke_jxa.js.
#
# The box is always modelled by the sample MEAN: for 0/1 tickets the mean of the
# draws IS the observed proportion, which is what the hypotheses are stated in.
# (The old "sum" representation has been removed from the lesson.)
#
# The confidence level is derived as 1 - alpha, so the CI and the p-value always
# describe the same test.
#
# Run from the repo root: Rscript Z-Tests/proportion-test/tools/generate_expected_pipeline.R

library(jsonlite)

p0 <- 0.7; n <- 50; x <- 37; alpha <- 0.05
ov <- x / n
sd_ <- sqrt(p0 * (1 - p0))

EV <- p0
SE <- sd_ / sqrt(n)
EV_string <- as.character(round(EV, 5))
SE_string <- as.character(round(SE, 5))
ts_num <- as.numeric(as.character(round((ov - as.numeric(EV_string)) / as.numeric(SE_string), 4)))

wilson <- function(p_hat, n, z) {
  denom <- 1 + z^2 / n
  margin <- z * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))
  list(lower = (p_hat + z^2 / (2 * n) - margin) / denom,
       upper = (p_hat + z^2 / (2 * n) + margin) / denom)
}
ci_two <- wilson(ov, n, qnorm(1 - alpha / 2))
ci_one <- wilson(ov, n, qnorm(1 - alpha))

out <- list(
  n = n, p0 = "0.7", x = x, ov = as.character(round(ov, 5)), alpha = alpha,
  sigma = as.character(round(sd_, 5)),
  np0 = n * p0, nq0 = n * (1 - p0),
  EV = EV_string, SE = SE_string,
  ts = as.character(round((ov - as.numeric(EV_string)) / as.numeric(SE_string), 4)),
  p_two = as.character(round(2 * (1 - pnorm(abs(ts_num))), 5)),
  p_greater = as.character(round(1 - pnorm(ts_num), 5)),
  p_less = as.character(round(pnorm(ts_num), 5)),
  ci_two_lower_4 = as.character(round(ci_two$lower, 4)),
  ci_two_upper_4 = as.character(round(ci_two$upper, 4)),
  ci_greater_lower_4 = as.character(round(ci_one$lower, 4)),
  ci_less_upper_4 = as.character(round(ci_one$upper, 4)),
  # Cross-check: R's own prop.test (uncorrected) must agree with both the
  # p-value and the two-sided Wilson interval we compute by hand.
  prop_test_p = as.character(round(prop.test(x, n, p = p0, correct = FALSE)$p.value, 5)),
  prop_test_ci = as.character(round(prop.test(x, n, p = p0, correct = FALSE)$conf.int, 4))
)

write(toJSON(out, digits = NA, auto_unbox = TRUE, pretty = TRUE),
      "Z-Tests/proportion-test/tests/expected_pipeline.json")
cat("Wrote Z-Tests/proportion-test/tests/expected_pipeline.json\n")
