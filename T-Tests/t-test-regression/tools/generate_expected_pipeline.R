# Computes the full regression-t-test calculation chain in R, exactly as
# R/ttest_regression_srv.R does, for the default selection: study_data,
# x = Minutes_Studied, y = Test_Score. Covers all three alternate hypotheses.
# Output feeds tests/smoke_jxa.js.
# Run from the repo root: Rscript t-test-regression/tools/generate_expected_pipeline.R

library(jsonlite)

set.seed(1)
n <- 25
Minutes_Studied <- round(runif(n, 0, 600), 0)
Test_Score <- round(50 + 0.075 * Minutes_Studied + rnorm(n, 0, 5), 1)
x <- Minutes_Studied; y <- Test_Score

model <- lm(y ~ x); sm <- summary(model)
slope <- coef(model)[2]
se <- sm$coefficients[2, 2]
s <- sm$sigma
rss <- sum(residuals(model)^2)
sxx <- sum((x - mean(x))^2)
df <- n - 2
ts <- as.numeric(as.character(round(slope / se, 3)))   # displayed TS (3 dp)

p1 <- 2 * (1 - pt(abs(ts), df))
p2 <- 1 - pt(ts, df)
p3 <- pt(ts, df)

# CI (two-sided, conf 0.95) uses full-precision slope/se.
alpha <- 0.05
tval <- qt(1 - alpha / 2, df)

out <- list(
  n = n, df = df,
  slope_3 = as.character(round(slope, 3)),
  s_3 = as.character(round(s, 3)),
  rss_3 = as.character(round(rss, 3)),
  sxx_3 = as.character(round(sxx, 3)),
  se_3 = as.character(round(se, 3)),
  ts_3 = as.character(round(slope / se, 3)),
  p_two = as.character(round(p1, 5)),
  p_greater = as.character(round(p2, 5)),
  p_less = as.character(round(p3, 5)),
  ci_two_lower_4 = as.character(round(slope - tval * se, 4)),
  ci_two_upper_4 = as.character(round(slope + tval * se, 4))
)

write(toJSON(out, digits = NA, auto_unbox = TRUE, pretty = TRUE),
      "T-Tests/t-test-regression/tests/expected_pipeline.json")
cat("Wrote t-test-regression/tests/expected_pipeline.json\n")
