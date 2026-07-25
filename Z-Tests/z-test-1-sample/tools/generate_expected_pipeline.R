# Computes the full 1-sample z-test calculation chain in R, exactly as
# R/ztest_1_sample_srv.R does (including its round-then-parse steps), for
# Mr. Han's data with the default inputs: null mu = 140, sigma = sample SD.
# Covers each alternate hypothesis. Output feeds tests/smoke_jxa.js.
# Run from the repo root: Rscript z-test-1-sample/tools/generate_expected_pipeline.R

library(jsonlite)

set.seed(1)
x <- rnorm(25, mean = 142, sd = 5)
n <- length(x)

null_mu <- 140
sigma <- sd(x)                       # population SD defaults to the sample SD

EV <- null_mu
EV_string <- as.character(round(EV, 5))
SE <- sigma / sqrt(n)
SE_string <- as.character(round(SE, 5))
ts_string <- as.character(round((mean(x) - as.numeric(EV_string)) / as.numeric(SE_string), 4))
ts <- as.numeric(ts_string)

p1 <- 2 * (1 - pnorm(abs(ts)))
p2 <- 1 - pnorm(ts)
p3 <- pnorm(ts)

# CI (conf 0.95). NOTE: the R source centres the CI on EV (the null mean), not
# the observed mean -- reproduced here so the JS matches the Shiny app.
xbar <- as.numeric(EV_string)
alpha <- 1 - 0.95
z_two <- qnorm(1 - alpha / 2)
z_one <- qnorm(1 - alpha)

out <- list(
  n = n,
  box_mu = as.character(round(null_mu, 3)),
  box_sigma = as.character(round(sigma, 3)),
  box_ov = as.character(round(mean(x), 3)),
  EV_string = EV_string,
  SE_string = SE_string,
  sigma_5 = as.character(round(sigma, 5)),
  ov_5 = as.character(round(mean(x), 5)),
  ts_string = ts_string,
  p_two_sided_5 = as.character(round(p1, 5)),
  p_greater_5 = as.character(round(p2, 5)),
  p_less_5 = as.character(round(p3, 5)),
  ci_two_lower_4 = as.character(round(xbar - z_two * SE, 4)),
  ci_two_upper_4 = as.character(round(xbar + z_two * SE, 4)),
  ci_greater_lower_4 = as.character(round(xbar - z_one * SE, 4)),
  ci_less_upper_4 = as.character(round(xbar + z_one * SE, 4))
)

write(toJSON(out, digits = NA, auto_unbox = TRUE, pretty = TRUE),
      "Z-Tests/z-test-1-sample/tests/expected_pipeline.json")
cat("Wrote z-test-1-sample/tests/expected_pipeline.json\n")
