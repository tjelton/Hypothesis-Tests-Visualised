# Computes the deterministic parts of the box-model-part-3 lesson in R exactly
# as R/fundamentals_box_model_part_3_srv.R does: the EV/SE of the modelling
# normal (by sum or mean) and the tail/interval probabilities. The simulation
# (histograms) is random and not checked. Output feeds tests/smoke_jxa.js.
# Run from the repo root: Rscript box-model-part-3/tools/generate_expected_pipeline.R

library(jsonlite)
popsd <- function(x) sqrt(mean((x - mean(x))^2))

evse <- function(box, n, mode) {              # mode: "sum" | "mean"
  if (mode == "mean") list(ev = mean(box), se = popsd(box) / sqrt(n))
  else list(ev = n * mean(box), se = sqrt(n) * popsd(box))
}
# Probability over [lower, upper] with NA = the corresponding infinity.
prob <- function(ev, se, lower, upper) {
  if (is.na(lower) && is.na(upper)) return(1)
  if (is.na(lower)) return(pnorm(upper, ev, se))
  if (is.na(upper)) return(pnorm(lower, ev, se, lower.tail = FALSE))
  pnorm(upper, ev, se) - pnorm(lower, ev, se)
}

coin <- c(1, 0); dice <- 1:6
cs <- evse(coin, 100, "sum")     # coin flips, n = 100, sum
cm <- evse(coin, 100, "mean")    # coin flips, n = 100, mean
ds <- evse(dice, 50, "sum")      # dice, n = 50, sum

out <- list(
  coin_sum_ev = as.character(round(cs$ev, 5)), coin_sum_se = as.character(round(cs$se, 5)),
  coin_mean_ev = as.character(round(cm$ev, 5)), coin_mean_se = as.character(round(cm$se, 5)),
  dice_sum_ev = as.character(round(ds$ev, 5)), dice_sum_se = as.character(round(ds$se, 5)),
  p_coin_ge_60 = as.character(round(prob(cs$ev, cs$se, 60, NA), 5)),
  p_coin_ge_70 = as.character(round(prob(cs$ev, cs$se, 70, NA), 5)),
  p_coin_40_70 = as.character(round(prob(cs$ev, cs$se, 40, 70), 5)),
  p_dice_ge_150 = as.character(round(prob(ds$ev, ds$se, 150, NA), 5)),
  p_coin_all = as.character(round(prob(cs$ev, cs$se, NA, NA), 5))
)

write(toJSON(out, digits = NA, auto_unbox = TRUE, pretty = TRUE),
      "Fundamentals/box-model-part-3/tests/expected_pipeline.json")
cat("Wrote box-model-part-3/tests/expected_pipeline.json\n")
