# Computes the full 2-sample-t-test calculation chain in R, exactly as
# R/ttest_2_sample_srv.R does (including its round-then-parse steps), for the
# default selection: blood_pressure, sample 1 = Drug_A, sample 2 = Drug_B,
# dependent variable = blood_pressure. Covers both the equal-variance (pooled)
# and Welch cases, and all three alternate hypotheses. Output feeds
# tests/smoke_jxa.js.
# Run from the repo root: Rscript t-test-2-sample/tools/generate_expected_pipeline.R

library(jsonlite)

set.seed(1)
n <- 50
blood_pressure <- data.frame(
  drug = rep(c("Drug_A", "Drug_B"), each = n),
  blood_pressure = c(rnorm(n, 120, 10), rnorm(n, 115, 10))
)
x1 <- blood_pressure$blood_pressure[blood_pressure$drug == "Drug_A"]
x2 <- blood_pressure$blood_pressure[blood_pressure$drug == "Drug_B"]

n1 <- length(x1); n2 <- length(x2)
mean1 <- mean(x1); mean2 <- mean(x2)
sd1 <- sd(x1); sd2 <- sd(x2)
pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))

# --- per-spread chain ---
chain <- function(equal_var) {
  if (equal_var) {
    se <- pooled_sd * sqrt(1 / n1 + 1 / n2)
    df <- n1 + n2 - 2
  } else {
    se <- sqrt(sd1^2 / n1 + sd2^2 / n2)
    df <- ((sd1^2 / n1 + sd2^2 / n2)^2) /
      (((sd1^2 / n1)^2) / (n1 - 1) + ((sd2^2 / n2)^2) / (n2 - 1))
  }
  ts <- as.numeric(as.character(round((mean1 - mean2) / se, 4)))
  p1 <- 2 * (1 - pt(abs(ts), df))
  p2 <- 1 - pt(ts, df)
  p3 <- pt(ts, df)

  # CI (two-sided, conf 0.95) uses var() = sd()^2; equals the SE/df above.
  alpha <- 0.05
  diff <- mean1 - mean2
  tval <- qt(1 - alpha / 2, df)
  ci_lower <- diff - tval * se
  ci_upper <- diff + tval * se

  list(
    se_5 = as.character(round(se, 5)),
    ts_4 = as.character(round((mean1 - mean2) / se, 4)),
    df_display = if (equal_var) as.character(df) else as.character(round(df, 3)),
    p_two = as.character(round(p1, 5)),
    p_greater = as.character(round(p2, 5)),
    p_less = as.character(round(p3, 5)),
    ci_two_lower_4 = as.character(round(ci_lower, 4)),
    ci_two_upper_4 = as.character(round(ci_upper, 4))
  )
}

out <- list(
  n1 = n1, n2 = n2,
  box_s1 = as.character(round(sd1, 3)), box_ov1 = as.character(round(mean1, 3)),
  box_s2 = as.character(round(sd2, 3)), box_ov2 = as.character(round(mean2, 3)),
  pooled_sd_3 = as.character(round(pooled_sd, 3)),
  equal = chain(TRUE),
  welch = chain(FALSE)
)

write(toJSON(out, digits = NA, auto_unbox = TRUE, pretty = TRUE),
      "T-Tests/t-test-2-sample/tests/expected_pipeline.json")
cat("Wrote t-test-2-sample/tests/expected_pipeline.json\n")
