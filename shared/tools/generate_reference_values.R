# Generates shared/tests/reference_values.json: R-computed values of
# pt/qt/dt/qnorm used by the shared stats suite (stats.test.mjs / run_jxa.js)
# to verify the accuracy of the JS distribution functions (this app is a
# teaching tool, so tail/quantile accuracy matters). The seeded sample used
# for the sample-statistic checks is embedded here too (han_input), so the
# suite is self-contained and does not depend on any lesson's datasets.js.
# Run from the repo root: Rscript shared/tools/generate_reference_values.R

library(jsonlite)

t_vals <- c(-20, -8, -3.5, -2.992, -1.5, -1, -0.5, -0.001, 0, 0.001, 0.5, 1,
            1.5, 1.96, 2.5, 2.992, 3.5, 8, 20)
p_vals <- c(1e-6, 1e-4, 0.001, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5,
            0.75, 0.9, 0.95, 0.975, 0.99, 0.999, 0.9999, 1 - 1e-6)
dfs <- c(1, 2, 3, 4, 5, 9, 10, 24, 29, 30, 49, 59, 70, 100, 149, 577)

pt_cases <- do.call(rbind, lapply(dfs, function(df) {
  data.frame(t = t_vals, df = df, value = pt(t_vals, df))
}))
qt_cases <- do.call(rbind, lapply(dfs, function(df) {
  data.frame(p = p_vals, df = df, value = qt(p_vals, df))
}))
dt_cases <- do.call(rbind, lapply(dfs, function(df) {
  data.frame(x = t_vals, df = df, value = dt(t_vals, df))
}))
qnorm_cases <- data.frame(p = p_vals, value = qnorm(p_vals))

# Reference sample statistics computed the way the Shiny modules do.
set.seed(1)
han <- rnorm(25, mean = 142, sd = 5)
sample_stats <- list(
  han_input = han,
  han_mean = mean(han),
  han_sd = sd(han),
  han_n = length(han),
  # quantile type 7 used by qqline / quantile() defaults
  han_q25 = unname(quantile(han, 0.25)),
  han_q75 = unname(quantile(han, 0.75)),
  # fivenum hinges used by boxplot.stats
  han_fivenum = fivenum(han)
)

out <- list(
  pt = pt_cases, qt = qt_cases, dt = dt_cases, qnorm = qnorm_cases,
  sample_stats = sample_stats
)
write(toJSON(out, digits = NA, dataframe = "columns", auto_unbox = TRUE),
      "shared/tests/reference_values.json")
cat("Wrote shared/tests/reference_values.json\n")
