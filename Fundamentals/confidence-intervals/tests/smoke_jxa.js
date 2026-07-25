// Smoke test for macOS JavaScriptCore (no browser/Node needed). This lesson is
// a random simulation, so it verifies the CI *method* statistically (a 95% CI
// covers the true mean ~95% of the time over many draws) plus the deterministic
// margin arithmetic, exercises the CI plot, and loads app.js against a DOM stub.
// Run from the repo root:
//   osascript -l JavaScript confidence-intervals/tests/smoke_jxa.js
ObjC.import("Foundation");

function readFile(path) {
  return ObjC.unwrap($.NSString.stringWithContentsOfFileEncodingError(
    path, $.NSUTF8StringEncoding, null));
}

function run(argv) {
  const shared = "shared";
  const Stats = eval(readFile(shared + "/js/stats.js") + "\n;Stats");
  const Plots = (function () {
    const src = readFile(shared + "/js/plots.js");
    return new Function("Stats", src + "\n;return Plots;")(Stats);
  })();

  const lines = [];
  let failures = 0, checks = 0;
  function assertTrue(c, label) { checks++; if (!c) { failures++; lines.push("FAIL " + label); } }

  // ---- rnorm sanity: large sample mean/sd approximate the parameters ----
  const big = Stats.rnorm(20000, 5, 2);
  assertTrue(Math.abs(Stats.mean(big) - 5) < 0.15, "rnorm mean approximates target");
  assertTrue(Math.abs(Stats.sd(big) - 2) < 0.15, "rnorm sd approximates target");

  // ---- margin arithmetic is exact (95% z-multiplier) ----
  assertTrue(Math.abs(Stats.qnorm(0.975) - 1.959963984540054) < 1e-9, "qnorm(0.975) matches R");

  // ---- coverage: a 95% CI should contain the true mean ~95% of 10,000 trials ----
  const trueMean = 100, popSd = 15, n = 30, z = Stats.qnorm(0.975);
  let covered = 0, T = 10000;
  for (let t = 0; t < T; t++) {
    const s = Stats.rnorm(n, trueMean, popSd);
    const m = Stats.mean(s), margin = z * Stats.sd(s) / Math.sqrt(n);
    if (m - margin <= trueMean && m + margin >= trueMean) covered++;
  }
  const rate = covered / T;
  // t-interval coverage of a z-CI is a bit below 0.95 at n=30; allow a band.
  assertTrue(rate > 0.90 && rate < 0.97, "95% CI empirical coverage in [0.90, 0.97] (got " + rate.toFixed(3) + ")");

  // ---- CI plot renders (green + red segments, dashed mean line) ----
  const lowers = [98, 101], uppers = [102, 105], contains = [true, false];
  const plot = Plots.ciPlotSVG(lowers, uppers, contains, 100, { main: "Simulated 95% Confidence Intervals" });
  assertTrue(plot.indexOf("<svg") === 0 && plot.indexOf("NaN") === -1, "CI plot is clean SVG");
  assertTrue(plot.indexOf('stroke="green"') !== -1 && plot.indexOf('stroke="red"') !== -1, "CI plot has green + red intervals");
  assertTrue(plot.indexOf("stroke-dasharray") !== -1, "CI plot has dashed true-mean line");
  // empty state (no data yet) is still clean
  assertTrue(Plots.ciPlotSVG([], [], [], 100, {}).indexOf("NaN") === -1, "empty CI plot is clean");

  // ---- app.js against DOM stub ----
  try {
    const appSrc = readFile("Fundamentals/confidence-intervals/js/app.js");
    new Function("document", "Stats", "Plots", appSrc)(
      { addEventListener: function () {}, getElementById: function () { return null; }, querySelector: function () { return null; }, querySelectorAll: function () { return []; } },
      Stats, Plots
    );
    checks++;
  } catch (e) { checks++; failures++; lines.push("FAIL app.js evaluation: " + e.message); }

  lines.push((checks - failures) + "/" + checks + " checks passed");
  if (failures > 0) lines.push("SMOKE TESTS FAILED");
  return lines.join("\n");
}
