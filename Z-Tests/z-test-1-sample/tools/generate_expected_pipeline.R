# Computes the full 1-sample z-test calculation chain in R (including the
# round-then-parse steps the page uses when displaying numbers) for Mr. Han's
# data. Output feeds tests/smoke_jxa.js.
# Run from the repo root: Rscript Z-Tests/z-test-1-sample/tools/generate_expected_pipeline.R
#
# Two sigma scenarios are covered, because the page offers both:
#   * known   -- sigma = 7.5, the case study's known population SD (the default)
#   * sample  -- sigma = sd(x), what the "Set Population SD to be Sample SD"
#                button produces (and what the other data sets fall back to)
#
# Unlike the Shiny original, the CI is centred on the OBSERVED mean and its
# level is 1 - alpha, so the CI and p-value verdicts always agree.

library(jsonlite)

set.seed(1)
x <- rnorm(25, mean = 142, sd = 5)
n <- length(x)

null_mu <- 140
alpha <- 0.05

EV <- null_mu
EV_string <- as.character(round(EV, 5))

chain <- function(sigma) {
  SE <- sigma / sqrt(n)
  SE_string <- as.character(round(SE, 5))
  ts_string <- as.character(round((mean(x) - as.numeric(EV_string)) / as.numeric(SE_string), 4))
  ts <- as.numeric(ts_string)

  z_two <- qnorm(1 - alpha / 2)
  z_one <- qnorm(1 - alpha)
  xbar <- mean(x)                      # CI is centred on the observed mean

  list(
    box_sigma = as.character(round(sigma, 3)),
    sigma_5 = as.character(round(sigma, 5)),
    SE_string = SE_string,
    ts_string = ts_string,
    p_two_sided_5 = as.character(round(2 * (1 - pnorm(abs(ts))), 5)),
    p_greater_5 = as.character(round(1 - pnorm(ts), 5)),
    p_less_5 = as.character(round(pnorm(ts), 5)),
    ci_two_lower_4 = as.character(round(xbar - z_two * SE, 4)),
    ci_two_upper_4 = as.character(round(xbar + z_two * SE, 4)),
    ci_greater_lower_4 = as.character(round(xbar - z_one * SE, 4)),
    ci_less_upper_4 = as.character(round(xbar + z_one * SE, 4))
  )
}

out <- list(
  n = n,
  alpha = alpha,
  box_mu = as.character(round(null_mu, 3)),
  box_ov = as.character(round(mean(x), 3)),
  EV_string = EV_string,
  ov_5 = as.character(round(mean(x), 5)),
  known = chain(7.5),
  sample = chain(sd(x))
)

write(toJSON(out, digits = NA, auto_unbox = TRUE, pretty = TRUE),
      "Z-Tests/z-test-1-sample/tests/expected_pipeline.json")
cat("Wrote Z-Tests/z-test-1-sample/tests/expected_pipeline.json\n")
