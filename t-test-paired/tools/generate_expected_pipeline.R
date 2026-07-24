# Computes the full paired-t-test lesson calculation chain in R, exactly as
# the Shiny modules do (including their round-then-parse steps), for the
# default selections: BloodPressureStudy with condition 1 = Before_Treatment,
# condition 2 = After_DrugA, so the sample is the paired difference
# (After_DrugA - Before_Treatment), null mu_d = 0. Output feeds
# tests/smoke_jxa.js.
# Run from the repo root: Rscript t-test-paired/tools/generate_expected_pipeline.R

library(jsonlite)

# Rebuild BloodPressureStudy exactly as the data loader does (single seed).
set.seed(1)
BloodPressureStudy <- data.frame(
  Before_Treatment = round(rnorm(30, 140, 15), 1),
  After_DrugA = round(rnorm(30, 135, 14), 1),
  After_DrugB = round(rnorm(30, 133, 13), 1)
)

# Paired difference = condition 2 - condition 1 (the actual sample analysed).
x <- BloodPressureStudy$After_DrugA - BloodPressureStudy$Before_Treatment
n <- length(x)
df <- n - 1

null_mu <- 0
null_mean_string <- as.character(round(null_mu, 3))

# Test-statistic module chain
EV <- as.numeric(null_mean_string)
EV_string <- as.character(round(EV, 5))
SE <- sd(x) / sqrt(n)
SE_string <- as.character(round(SE, 5))
ts_string <- as.character(round((mean(x) - as.numeric(EV_string)) / as.numeric(SE_string), 4))
ts <- as.numeric(ts_string)

# p-values per alternate hypothesis choice
p1 <- 2 * (1 - pt(abs(ts), df))
p2 <- 1 - pt(ts, df)
p3 <- pt(ts, df)

# Confidence intervals (conf level 0.95), full-precision xbar/se as in R module
xbar <- mean(x)
alpha <- 1 - 0.95
ci_two <- c(xbar - qt(1 - alpha / 2, df) * SE, xbar + qt(1 - alpha / 2, df) * SE)
ci_greater_lower <- xbar - qt(1 - alpha, df) * SE
ci_less_upper <- xbar + qt(1 - alpha, df) * SE

out <- list(
  n = n, df = df,
  box_mu = as.character(round(null_mu, 3)),
  box_s = as.character(round(sd(x), 3)),
  box_ov = as.character(round(mean(x), 3)),
  EV_string = EV_string,
  SE_string = SE_string,
  sd_5 = as.character(round(sd(x), 5)),
  ov_5 = as.character(round(mean(x), 5)),
  ts_string = ts_string,
  p_two_sided = p1, p_greater = p2, p_less = p3,
  p_two_sided_5 = as.character(round(p1, 5)),
  p_greater_5 = as.character(round(p2, 5)),
  p_less_5 = as.character(round(p3, 5)),
  ci_two_lower_4 = as.character(round(ci_two[1], 4)),
  ci_two_upper_4 = as.character(round(ci_two[2], 4)),
  ci_greater_lower_4 = as.character(round(ci_greater_lower, 4)),
  ci_less_upper_4 = as.character(round(ci_less_upper, 4))
)

write(toJSON(out, digits = NA, auto_unbox = TRUE, pretty = TRUE),
      "t-test-paired/tests/expected_pipeline.json")
cat("Wrote t-test-paired/tests/expected_pipeline.json\n")
