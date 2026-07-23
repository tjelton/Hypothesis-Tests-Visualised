// End-to-end smoke test for macOS JavaScriptCore (no browser/Node needed):
//   * replicates the paired lesson's calculation chain (on the paired
//     difference) via Stats and compares every displayed string against R's
//     output (tests/expected_pipeline.json);
//   * exercises every SVG plot generator on real data;
//   * syntax-checks app.js by evaluating it against a minimal DOM stub.
// Run from the repo root:
//   osascript -l JavaScript t-test-paired/tests/smoke_jxa.js
ObjC.import("Foundation");

function readFile(path) {
  return ObjC.unwrap($.NSString.stringWithContentsOfFileEncodingError(
    path, $.NSUTF8StringEncoding, null));
}

function run(argv) {
  const base = "t-test-paired";
  const shared = "shared";
  const Stats = eval(readFile(shared + "/js/stats.js") + "\n;Stats");
  const DATASETS = eval(readFile(base + "/js/datasets.js") + "\n;DATASETS");
  const Plots = (function () {
    const src = readFile(shared + "/js/plots.js");
    return new Function("Stats", src + "\n;return Plots;")(Stats);
  })();
  const boxModelHTML = eval(readFile(shared + "/js/boxmodel.js") + "\n;boxModelHTML");
  const expected = JSON.parse(readFile(base + "/tests/expected_pipeline.json"));

  const lines = [];
  let failures = 0, checks = 0;
  function assertEqual(actual, exp, label) {
    checks++;
    if (actual !== exp) {
      failures++;
      lines.push("FAIL " + label + ": got " + JSON.stringify(actual) + ", expected " + JSON.stringify(exp));
    }
  }
  function assertTrue(cond, label) {
    checks++;
    if (!cond) { failures++; lines.push("FAIL " + label); }
  }

  // ---- reconstruct the default selection: BloodPressureStudy, condition 1 =
  // Before_Treatment, condition 2 = After_DrugA -> paired difference ----
  const cols = DATASETS["BloodPressureStudy"].columns;
  const before = cols.find(c => c.name === "Before_Treatment").values;
  const afterA = cols.find(c => c.name === "After_DrugA").values;
  const x = afterA.map((v, i) => v - before[i]);   // condition 2 - condition 1
  const n = x.length, df = n - 1;
  const xbar = Stats.mean(x), s = Stats.sd(x);
  const nullMuStr = Stats.roundStr(0, 3);

  assertEqual(n, expected.n, "n");
  assertEqual(nullMuStr, expected.box_mu, "box mu string");
  assertEqual(Stats.roundStr(s, 3), expected.box_s, "box s string");
  assertEqual(Stats.roundStr(xbar, 3), expected.box_ov, "box OV string");

  const EVStr = Stats.roundStr(Number(nullMuStr), 5);
  const SE = s / Math.sqrt(n);
  const SEStr = Stats.roundStr(SE, 5);
  assertEqual(EVStr, expected.EV_string, "EV string");
  assertEqual(SEStr, expected.SE_string, "SE string");
  assertEqual(Stats.roundStr(s, 5), expected.sd_5, "sd (5dp) string");
  assertEqual(Stats.roundStr(xbar, 5), expected.ov_5, "OV (5dp) string");

  const tsStr = Stats.roundStr((xbar - Number(EVStr)) / Number(SEStr), 4);
  assertEqual(tsStr, expected.ts_string, "test statistic string");

  const ts = Number(tsStr);
  const p1 = 2 * (1 - Stats.pt(Math.abs(ts), df));
  const p2 = 1 - Stats.pt(ts, df);
  const p3 = Stats.pt(ts, df);
  assertEqual(Stats.roundStr(p1, 5), expected.p_two_sided_5, "p two-sided (5dp)");
  assertEqual(Stats.roundStr(p2, 5), expected.p_greater_5, "p greater (5dp)");
  assertEqual(Stats.roundStr(p3, 5), expected.p_less_5, "p less (5dp)");

  const alpha = 1 - 0.95;
  assertEqual(Stats.roundStr(xbar - Stats.qt(1 - alpha / 2, df) * SE, 4), expected.ci_two_lower_4, "CI two-sided lower");
  assertEqual(Stats.roundStr(xbar + Stats.qt(1 - alpha / 2, df) * SE, 4), expected.ci_two_upper_4, "CI two-sided upper");
  assertEqual(Stats.roundStr(xbar - Stats.qt(1 - alpha, df) * SE, 4), expected.ci_greater_lower_4, "CI greater lower");
  assertEqual(Stats.roundStr(xbar + Stats.qt(1 - alpha, df) * SE, 4), expected.ci_less_upper_4, "CI less upper");

  // ---- plots generate valid-looking SVG on the three series ----
  const plotOutputs = [
    Plots.histogramSVG(x, { main: "Histogram", col: "blue", breaks: 30 }),
    Plots.histogramSVG(before, { breaks: 30 }),
    Plots.boxplotSVG(x, { horizontal: true, col: "blue" }),
    Plots.boxplotSVG(afterA, { main: "Boxplot of Sample Data" }),
    Plots.qqPlotSVG(x, {}),
    Plots.shadedTCurveSVG(df, ts, 1, {}),
    Plots.shadedTCurveSVG(df, ts, 2, {}),
    Plots.shadedTCurveSVG(df, -ts, 3, {})
  ];
  plotOutputs.forEach(function (svg, i) {
    assertTrue(typeof svg === "string" && svg.indexOf("<svg") === 0, "plot " + i + " is SVG");
    assertTrue(svg.indexOf("NaN") === -1 && svg.indexOf("Infinity") === -1, "plot " + i + " has no NaN/Infinity");
  });

  // ---- box model HTML ----
  const bm = boxModelHTML("&mu; = 0; s = 4.751", "OV = -4.373", "n = 30");
  assertTrue(bm.indexOf("&mu; = 0; s = 4.751") !== -1, "box model contains box label");
  assertTrue(bm.indexOf("OV = -4.373") !== -1, "box model contains sample label");
  assertTrue(bm.indexOf("n = 30") !== -1, "box model contains n label");

  // ---- app.js evaluates cleanly against a DOM stub ----
  try {
    const appSrc = readFile(base + "/js/app.js");
    new Function("document", "window", "DATASETS", "Stats", "Plots", "boxModelHTML", appSrc)(
      { addEventListener: function () {}, getElementById: function () { return null; }, querySelectorAll: function () { return []; } },
      {}, DATASETS, Stats, Plots, boxModelHTML
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
