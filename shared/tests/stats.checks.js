// Shared assertion suite for the stats layer. Runner-agnostic plain JS:
// driven by stats.test.mjs (Node) or run_jxa.js (macOS osascript).
// `ref` is tests/reference_values.json (R-computed ground truth).

function runStatsChecks(Stats, ref, hanValues, log) {
  let failures = 0;
  let checks = 0;

  function assertClose(actual, expected, relTol, label) {
    checks++;
    const denom = Math.max(Math.abs(expected), 1e-300);
    const relErr = Math.abs(actual - expected) / denom;
    if (!(relErr <= relTol) && Math.abs(actual - expected) > 1e-300) {
      failures++;
      log("FAIL " + label + ": got " + actual + ", expected " + expected +
          " (rel err " + relErr.toExponential(2) + ")");
    }
  }

  function assertEqual(actual, expected, label) {
    checks++;
    if (actual !== expected) {
      failures++;
      log("FAIL " + label + ": got " + JSON.stringify(actual) +
          ", expected " + JSON.stringify(expected));
    }
  }

  // Tolerances: the app displays values rounded to 4-5 decimal places, so
  // 1e-10 relative agreement with R is far beyond visible precision. In
  // practice the implementation agrees with R to ~1e-12 or better almost
  // everywhere (worst case ~8e-12 for pt near t=0 at large df).

  // pt: CDF must match R (p-values shown to students), including far tails.
  for (let i = 0; i < ref.pt.t.length; i++) {
    assertClose(Stats.pt(ref.pt.t[i], ref.pt.df[i]), ref.pt.value[i], 1e-10,
                "pt(" + ref.pt.t[i] + ", df=" + ref.pt.df[i] + ")");
  }

  // qt: quantiles (used for confidence intervals), including far tails.
  for (let i = 0; i < ref.qt.p.length; i++) {
    assertClose(Stats.qt(ref.qt.p[i], ref.qt.df[i]), ref.qt.value[i], 1e-10,
                "qt(" + ref.qt.p[i] + ", df=" + ref.qt.df[i] + ")");
  }

  // dt: density used for the plotted t-curve.
  for (let i = 0; i < ref.dt.x.length; i++) {
    assertClose(Stats.dt(ref.dt.x[i], ref.dt.df[i]), ref.dt.value[i], 1e-12,
                "dt(" + ref.dt.x[i] + ", df=" + ref.dt.df[i] + ")");
  }

  // qnorm: used for QQ-plot theoretical quantiles and the qqline.
  for (let i = 0; i < ref.qnorm.p.length; i++) {
    assertClose(Stats.qnorm(ref.qnorm.p[i]), ref.qnorm.value[i], 1e-10,
                "qnorm(" + ref.qnorm.p[i] + ")");
  }

  // dnorm: standard-normal density (exact closed form) used to draw the normal
  // reference curve in the t-curve lesson. Spot-checked against R's dnorm().
  assertClose(Stats.dnorm(0), 0.3989422804014327, 1e-14, "dnorm(0)");
  assertClose(Stats.dnorm(1), 0.24197072451914337, 1e-14, "dnorm(1)");
  assertClose(Stats.dnorm(-1.96), 0.058440944333451469, 1e-14, "dnorm(-1.96)");
  assertClose(Stats.dnorm(3.5), 0.00087268269504576015, 1e-14, "dnorm(3.5)");

  // Sample statistics on the seeded Mr. Han data (must equal what the Shiny
  // app computes for the same data).
  const han = ref.sample_stats;
  assertEqual(hanValues.length, han.han_n, "han n");
  assertClose(Stats.mean(hanValues), han.han_mean, 1e-13, "han mean");
  assertClose(Stats.sd(hanValues), han.han_sd, 1e-13, "han sd");
  assertClose(Stats.quantileType7(hanValues, 0.25), han.han_q25, 1e-13, "han q25");
  assertClose(Stats.quantileType7(hanValues, 0.75), han.han_q75, 1e-13, "han q75");
  const fn = Stats.fivenum(hanValues);
  for (let i = 0; i < 5; i++) {
    assertClose(fn[i], han.han_fivenum[i], 1e-13, "han fivenum[" + i + "]");
  }

  // linreg: OLS fit must match R's lm()/summary() (slope, intercept, residual
  // standard error sigma, SE of slope). Fixed vector checked against R.
  {
    const lx = [1, 2, 3, 4, 5, 6, 7, 8];
    const ly = [2.1, 3.9, 6.2, 7.8, 10.1, 11.7, 14.3, 15.8];
    const fit = Stats.linreg(lx, ly);
    assertClose(fit.slope, 1.98452380952381, 1e-12, "linreg slope");
    assertClose(fit.intercept, 0.0571428571428555, 1e-11, "linreg intercept");
    assertClose(fit.s, 0.230467089548767, 1e-12, "linreg sigma");
    assertClose(fit.seSlope, 0.0355618439733882, 1e-12, "linreg SE(slope)");
    assertClose(fit.slope / fit.seSlope, 55.804862397149, 1e-11, "linreg t");
  }

  // R-style rounding (round half to even).
  assertEqual(Stats.roundR(0.5), 0, "roundR(0.5)");
  assertEqual(Stats.roundR(1.5), 2, "roundR(1.5)");
  assertEqual(Stats.roundR(2.5), 2, "roundR(2.5)");
  assertEqual(Stats.roundR(-1.5), -2, "roundR(-1.5)");
  assertEqual(Stats.roundR(-2.5), -2, "roundR(-2.5)");
  assertEqual(Stats.roundR(2.567, 2), 2.57, "roundR(2.567, 2)");
  assertEqual(Stats.roundR(142.84371282471591, 3), 142.844, "roundR(142.843..., 3)");
  assertEqual(Stats.roundR(0.125, 2), 0.12, "roundR(0.125, 2) half-even");

  // formatR / roundStr: mimics as.character(round(x, d)).
  assertEqual(Stats.formatR(NaN), "NA", "formatR(NaN)");
  assertEqual(Stats.formatR(0.05), "0.05", "formatR(0.05)");
  assertEqual(Stats.formatR(142.844), "142.844", "formatR(142.844)");
  assertEqual(Stats.formatR(0.00001), "1e-05", "formatR(1e-5)");
  assertEqual(Stats.formatR(0.001), "0.001", "formatR(0.001)");
  assertEqual(Stats.formatR(-3.25), "-3.25", "formatR(-3.25)");
  assertEqual(Stats.roundStr(2.9915564, 4), "2.9916", "roundStr TS");

  // ppoints matches R's definition.
  const pp5 = Stats.ppoints(5);
  assertClose(pp5[0], (1 - 3 / 8) / (5 + 1 - 2 * (3 / 8)), 1e-15, "ppoints(5)[0]");
  const pp25 = Stats.ppoints(25);
  assertClose(pp25[0], 0.5 / 25, 1e-15, "ppoints(25)[0]");

  return { checks: checks, failures: failures };
}

if (typeof module !== "undefined" && module.exports) module.exports = runStatsChecks;
