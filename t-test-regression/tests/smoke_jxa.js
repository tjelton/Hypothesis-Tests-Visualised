// End-to-end smoke test for macOS JavaScriptCore (no browser/Node needed):
//   * replicates the regression lesson's calculation chain via Stats.linreg
//     and compares every displayed string against R (tests/expected_pipeline.json);
//   * exercises the plot generators (scatter, residual, ordered residuals, QQ,
//     shaded t-curve), including the intro horizontal-line plot;
//   * checks incomplete-pair dropping on airquality (which contains NA);
//   * syntax-checks app.js against a minimal DOM stub.
// Run from the repo root:
//   osascript -l JavaScript t-test-regression/tests/smoke_jxa.js
ObjC.import("Foundation");

function readFile(path) {
  return ObjC.unwrap($.NSString.stringWithContentsOfFileEncodingError(
    path, $.NSUTF8StringEncoding, null));
}

function run(argv) {
  const base = "t-test-regression";
  const shared = "shared";
  const Stats = eval(readFile(shared + "/js/stats.js") + "\n;Stats");
  const DATASETS = eval(readFile(base + "/js/datasets.js") + "\n;DATASETS");
  const Plots = (function () {
    const src = readFile(shared + "/js/plots.js");
    return new Function("Stats", src + "\n;return Plots;")(Stats);
  })();
  const expected = JSON.parse(readFile(base + "/tests/expected_pipeline.json"));

  const lines = [];
  let failures = 0, checks = 0;
  function assertEqual(a, e, label) { checks++; if (a !== e) { failures++; lines.push("FAIL " + label + ": got " + JSON.stringify(a) + ", expected " + JSON.stringify(e)); } }
  function assertTrue(c, label) { checks++; if (!c) { failures++; lines.push("FAIL " + label); } }

  // ---- default selection: study_data, x = Minutes_Studied, y = Test_Score ----
  const cols = DATASETS["study_data"].columns;
  const x = cols.find(c => c.name === "Minutes_Studied").values;
  const y = cols.find(c => c.name === "Test_Score").values;
  const fit = Stats.linreg(x, y);
  const df = fit.df;

  assertEqual(fit.n, expected.n, "n");
  assertEqual(df, expected.df, "df");
  assertEqual(Stats.roundStr(fit.slope, 3), expected.slope_3, "slope");
  assertEqual(Stats.roundStr(fit.s, 3), expected.s_3, "residual sd s");
  assertEqual(Stats.roundStr(fit.rss, 3), expected.rss_3, "RSS");
  assertEqual(Stats.roundStr(fit.sxx, 3), expected.sxx_3, "Sxx");
  assertEqual(Stats.roundStr(fit.seSlope, 3), expected.se_3, "SE(slope)");

  const tsStr = Stats.roundStr(fit.slope / fit.seSlope, 3);
  assertEqual(tsStr, expected.ts_3, "test statistic");
  const ts = Number(tsStr);
  assertEqual(Stats.roundStr(2 * (1 - Stats.pt(Math.abs(ts), df)), 5), expected.p_two, "p two-sided");
  assertEqual(Stats.roundStr(1 - Stats.pt(ts, df), 5), expected.p_greater, "p greater");
  assertEqual(Stats.roundStr(Stats.pt(ts, df), 5), expected.p_less, "p less");

  const alpha = 0.05, tv = Stats.qt(1 - alpha / 2, df);
  assertEqual(Stats.roundStr(fit.slope - tv * fit.seSlope, 4), expected.ci_two_lower_4, "CI lower");
  assertEqual(Stats.roundStr(fit.slope + tv * fit.seSlope, 4), expected.ci_two_upper_4, "CI upper");

  // ---- incomplete-pair dropping (airquality Ozone/Solar.R have NA) ----
  const aq = DATASETS["airquality"].columns;
  const ozone = aq.find(c => c.name === "Ozone").values;
  const solar = aq.find(c => c.name === "Solar.R").values;
  const ax = [], ay = [];
  for (let i = 0; i < ozone.length; i++) {
    if (typeof ozone[i] === "number" && typeof solar[i] === "number") { ax.push(ozone[i]); ay.push(solar[i]); }
  }
  assertTrue(ax.length > 0 && ax.length < ozone.length, "airquality drops NA pairs");
  const aqFit = Stats.linreg(ax, ay);
  assertTrue(isFinite(aqFit.slope) && isFinite(aqFit.seSlope), "airquality fit is finite");

  // ---- plots generate valid SVG ----
  const res = fit.residuals;
  const order = res.map((_, i) => i + 1);
  const plotOutputs = [
    Plots.scatterSVG(x, y, { main: "Scatter Plot", xlab: "X", ylab: "Y" }),
    Plots.scatterSVG(x, res, { main: "Residual Plot", xlab: "X", ylab: "Residuals", hline: 0 }),
    Plots.scatterSVG(order, res, { main: "Residuals vs Observation Order", connect: true, hline: 0 }),
    Plots.qqPlotSVG(res, {}),
    Plots.scatterSVG([], [], { xlim: [0, 600], ylim: [0, 100], hline: 50, main: "Hypothetical Horizontal Regression Line" }),
    Plots.shadedTCurveSVG(df - 1, ts, 1, {}),
    Plots.shadedTCurveSVG(df - 1, ts, 2, {}),
    Plots.shadedTCurveSVG(df - 1, ts, 3, {})
  ];
  plotOutputs.forEach(function (svg, i) {
    assertTrue(typeof svg === "string" && svg.indexOf("<svg") === 0, "plot " + i + " is SVG");
    assertTrue(svg.indexOf("NaN") === -1 && svg.indexOf("Infinity") === -1, "plot " + i + " has no NaN/Infinity");
  });

  // ---- app.js evaluates cleanly against a DOM stub ----
  try {
    const appSrc = readFile(base + "/js/app.js");
    new Function("document", "window", "DATASETS", "Stats", "Plots", appSrc)(
      { addEventListener: function () {}, getElementById: function () { return null; }, querySelectorAll: function () { return []; } },
      {}, DATASETS, Stats, Plots
    );
    checks++;
  } catch (e) {
    checks++; failures++;
    lines.push("FAIL app.js evaluation: " + e.message);
  }

  lines.push((checks - failures) + "/" + checks + " checks passed");
  if (failures > 0) lines.push("SMOKE TESTS FAILED");
  return lines.join("\n");
}
