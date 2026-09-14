# Computes the Wilson-interval-by-prediction-intervals chain in R for the
# default inputs: x = 9 successes out of n = 30, at a 95% confidence level
# (so p-hat = 0.30). Output feeds tests/smoke_jxa.js.
#
# Those defaults are a deliberate compromise. The two prediction intervals
# differ in width by exactly the factor by which the interval is off-centre --
# each endpoint sits z*SE from p-hat, so the SE ratio IS the asymmetry ratio.
# A p-hat near 0 makes the right-hand curve enormously fatter (at 3/20 it is
# 2.16x), which reads as a lopsided plot; a p-hat near 0.5 balances the curves
# but hides the asymmetry that distinguishes Wilson from Wald. At 9/30 the
# ratio is 1.34: the curves look comparable, yet the interval is still plainly
# not centred on p-hat.
#
# The lesson's whole claim is the defining property of the Wilson interval:
# an endpoint p of the interval is a proportion whose own C% prediction
# interval for p-hat, namely
#
#     p +/- z * sqrt(p(1-p)/n),
#
# just reaches the observed p-hat. So the lower endpoint's UPPER prediction
# limit equals p-hat, and the upper endpoint's LOWER prediction limit equals
# p-hat. This script pins down both, because that identity -- not the closed
# form -- is what the page asks students to discover.
#
# It also records the 11 values behind each slider. Both sliders run in the
# same direction as the plot's proportion axis (tick 0 leftmost), and both are
# split at tick 5 so that tick 5 is the exact Wilson endpoint: the left slider
# runs far-end -> lower endpoint -> p-hat, the right slider p-hat -> upper
# endpoint -> far end.
#
# The far end is the endpoint mirrored through p-hat, which for a p-hat near 0
# or 1 lands outside [0, 1]. It is therefore pulled back to halfway between the
# endpoint and the boundary, so the far end is always a genuine proportion with
# a positive SD -- at exactly 0 or 1 the SD is 0 and there is no curve to draw.
# That is why the left slider's lower half can be compressed.
#
# Needs the `binom` package for the binom.confint cross-check shown in the
# lesson's "Method 2" box: install.packages("binom").
#
# Run from the repo root: Rscript Z-Tests/wilson-confidence-interval/tools/generate_expected_pipeline.R

library(jsonlite)
library(binom)

x <- 9; n <- 30; conf <- 95
alpha <- 1 - conf / 100
z <- qnorm(1 - alpha / 2)
p_hat <- x / n

# Closed form: the two roots of (p_hat - p)^2 = z^2 p(1-p)/n.
wilson <- function(p_hat, n, z) {
  denom <- 1 + z^2 / n
  margin <- z * sqrt(p_hat * (1 - p_hat) / n + z^2 / (4 * n^2))
  list(lower = (p_hat + z^2 / (2 * n) - margin) / denom,
       upper = (p_hat + z^2 / (2 * n) + margin) / denom)
}
ci <- wilson(p_hat, n, z)

se <- function(p) sqrt(p * (1 - p) / n)
pi_lo <- function(p) p - z * se(p)
pi_hi <- function(p) p + z * se(p)

# Slider tick -> candidate proportion. `a` is tick 0, `mid` the Wilson endpoint
# at tick 5, `b` tick 10. The halves are interpolated separately so tick 5 stays
# exact even when one end is clamped at 0 or 1.
ticks <- function(a, mid, b) {
  sapply(0:10, function(t) if (t <= 5) a + (mid - a) * t / 5
                           else        mid + (b - mid) * (t - 5) / 5)
}
left_far  <- max(ci$lower - (p_hat - ci$lower), ci$lower / 2)
right_far <- min(ci$upper + (ci$upper - p_hat), 1 - (1 - ci$upper) / 2)
left_ticks  <- ticks(left_far, ci$lower, p_hat)
right_ticks <- ticks(p_hat, ci$upper, right_far)

bc <- binom.confint(x, n, conf.level = conf / 100, methods = "wilson")

out <- list(
  x = x, n = n, conf = conf, alpha = alpha,
  p_hat = as.character(round(p_hat, 5)),
  z = as.character(round(z, 5)),
  ci_lower_4 = as.character(round(ci$lower, 4)),
  ci_upper_4 = as.character(round(ci$upper, 4)),
  ci_lower_10 = as.character(round(ci$lower, 10)),
  ci_upper_10 = as.character(round(ci$upper, 10)),
  # The defining property, to 10 dp: each endpoint's prediction interval just
  # touches p-hat from its own side.
  lower_pi_lo_4 = as.character(round(pi_lo(ci$lower), 4)),
  lower_pi_hi_10 = as.character(round(pi_hi(ci$lower), 10)),
  upper_pi_lo_10 = as.character(round(pi_lo(ci$upper), 10)),
  upper_pi_hi_4 = as.character(round(pi_hi(ci$upper), 4)),
  se_lower_4 = as.character(round(se(ci$lower), 4)),
  se_upper_4 = as.character(round(se(ci$upper), 4)),
  # Slider geometry.
  left_far_10 = as.character(round(left_far, 10)),
  right_far_10 = as.character(round(right_far, 10)),
  left_ticks_6 = as.character(round(left_ticks, 6)),
  right_ticks_6 = as.character(round(right_ticks, 6)),
  # Cross-check: the endpoints are exactly R's own uncorrected prop.test
  # interval, i.e. the standard Wilson score interval.
  prop_test_ci_4 = as.character(round(prop.test(x, n, correct = FALSE)$conf.int, 4)),
  # ...and exactly binom::binom.confint(method = "wilson"), which the lesson
  # tells students to use. Recorded at the 7 significant digits R prints by
  # default, because the page quotes those digits back.
  binom_confint_lower_7 = as.character(round(bc$lower, 7)),
  binom_confint_upper_7 = as.character(round(bc$upper, 7)),
  binom_confint_lower_10 = as.character(round(bc$lower, 10)),
  binom_confint_upper_10 = as.character(round(bc$upper, 10))
)

write(toJSON(out, digits = NA, auto_unbox = TRUE, pretty = TRUE),
      "Z-Tests/wilson-confidence-interval/tests/expected_pipeline.json")
cat("Wrote Z-Tests/wilson-confidence-interval/tests/expected_pipeline.json\n")
