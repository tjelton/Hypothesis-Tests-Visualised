// End-to-end smoke test for macOS JavaScriptCore (no browser/Node needed):
//   * replicates the 2-sample lesson's calculation chain (pooled AND Welch)
//     via Stats and compares every displayed string against R's output
//     (tests/expected_pipeline.json);
//   * exercises every SVG plot generator (incl. the side-by-side boxplot);
//   * syntax-checks app.js by evaluating it against a minimal DOM stub.
// Run from the repo root:
//   osascript -l JavaScript t-test-2-sample/tests/smoke_jxa.js
ObjC.import("Foundation");

function readFile(path) {
  return ObjC.unwrap($.NSString.stringWithContentsOfFileEncodingError(
    path, $.NSUTF8StringEncoding, null));
}

function run(argv) {
  const base = "T-Tests/t-test-2-sample";
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
    if (actual !== exp) { failures++; lines.push("FAIL " + label + ": got " + JSON.stringify(actual) + ", expected " + JSON.stringify(exp)); }
  }
  function assertTrue(cond, label) { checks++; if (!cond) { failures++; lines.push("FAIL " + label); } }

  // ---- reconstruct default selection: blood_pressure, Drug_A vs Drug_B ----
  const cols = DATASETS["blood_pressure"].columns;
  const drug = cols.find(c => c.name === "drug").values;
  const bp = cols.find(c => c.name === "blood_pressure").values;
  const x1 = [], x2 = [];
  for (let i = 0; i < drug.length; i++) (drug[i] === "Drug_A" ? x1 : x2).push(bp[i]);

  const n1 = x1.length, n2 = x2.length;
  const mean1 = Stats.mean(x1), mean2 = Stats.mean(x2);
  const sd1 = Stats.sd(x1), sd2 = Stats.sd(x2);
  const pooledSd = Math.sqrt(((n1 - 1) * sd1 * sd1 + (n2 - 1) * sd2 * sd2) / (n1 + n2 - 2));

  assertEqual(n1, expected.n1, "n1");
  assertEqual(n2, expected.n2, "n2");
  assertEqual(Stats.roundStr(sd1, 3), expected.box_s1, "box s1");
  assertEqual(Stats.roundStr(mean1, 3), expected.box_ov1, "box OV1");
  assertEqual(Stats.roundStr(sd2, 3), expected.box_s2, "box s2");
  assertEqual(Stats.roundStr(mean2, 3), expected.box_ov2, "box OV2");
  assertEqual(Stats.roundStr(pooledSd, 3), expected.pooled_sd_3, "pooled sd");

  function checkChain(equalVar, exp) {
    let se, df;
    if (equalVar) {
      se = pooledSd * Math.sqrt(1 / n1 + 1 / n2);
      df = n1 + n2 - 2;
    } else {
      se = Math.sqrt(sd1 * sd1 / n1 + sd2 * sd2 / n2);
      const num = Math.pow(sd1 * sd1 / n1 + sd2 * sd2 / n2, 2);
      const den = Math.pow(sd1 * sd1 / n1, 2) / (n1 - 1) + Math.pow(sd2 * sd2 / n2, 2) / (n2 - 1);
      df = num / den;
    }
    const tag = equalVar ? "equal" : "welch";
    assertEqual(Stats.roundStr(se, 5), exp.se_5, tag + " SE");
    const tsStr = Stats.roundStr((mean1 - mean2) / se, 4);
    assertEqual(tsStr, exp.ts_4, tag + " TS");
    assertEqual(equalVar ? String(df) : Stats.roundStr(df, 3), exp.df_display, tag + " df");

    const ts = Number(tsStr);
    assertEqual(Stats.roundStr(2 * (1 - Stats.pt(Math.abs(ts), df)), 5), exp.p_two, tag + " p two-sided");
    assertEqual(Stats.roundStr(1 - Stats.pt(ts, df), 5), exp.p_greater, tag + " p greater");
    assertEqual(Stats.roundStr(Stats.pt(ts, df), 5), exp.p_less, tag + " p less");

    const alpha = 0.05, diff = mean1 - mean2, tv = Stats.qt(1 - alpha / 2, df);
    assertEqual(Stats.roundStr(diff - tv * se, 4), exp.ci_two_lower_4, tag + " CI lower");
    assertEqual(Stats.roundStr(diff + tv * se, 4), exp.ci_two_upper_4, tag + " CI upper");
  }
  checkChain(true, expected.equal);
  checkChain(false, expected.welch);

  // ---- plots generate valid SVG ----
  const ts = Number(Stats.roundStr((mean1 - mean2) / (pooledSd * Math.sqrt(1 / n1 + 1 / n2)), 4));
  const df = n1 + n2 - 2;
  const plotOutputs = [
    Plots.boxplotPairSVG(x1, x2, { main: "Side-by-side Boxplots" }),
    Plots.histogramSVG(x1, { col: "blue", breaks: 30 }),
    Plots.histogramSVG(x2, { col: "red", breaks: 30 }),
    Plots.qqPlotSVG(x1, { main: "Sample 1: QQ Plot" }),
    Plots.qqPlotSVG(x2, { main: "Sample 2: QQ Plot" }),
    Plots.shadedTCurveSVG(df - 1, ts, 1, {}),
    Plots.shadedTCurveSVG(df - 1, ts, 2, {}),
    Plots.shadedTCurveSVG(df - 1, ts, 3, {})
  ];
  plotOutputs.forEach(function (svg, i) {
    assertTrue(typeof svg === "string" && svg.indexOf("<svg") === 0, "plot " + i + " is SVG");
    assertTrue(svg.indexOf("NaN") === -1 && svg.indexOf("Infinity") === -1, "plot " + i + " has no NaN/Infinity");
  });
  // Two QQ plots on one page must use distinct clip ids.
  const q1 = Plots.qqPlotSVG(x1, {}), q2 = Plots.qqPlotSVG(x2, {});
  const id1 = (q1.match(/id="(qqclip\d+)"/) || [])[1];
  const id2 = (q2.match(/id="(qqclip\d+)"/) || [])[1];
  assertTrue(id1 && id2 && id1 !== id2, "QQ clip ids are unique");

  // ---- box model HTML with unicode subscripts ----
  const bm = boxModelHTML("μ₁ = μ₂ ; s₁ = " + Stats.roundStr(sd1, 3), "OV₁ = " + Stats.roundStr(mean1, 3), "n₁ = " + n1);
  assertTrue(bm.indexOf("OV₁ = " + Stats.roundStr(mean1, 3)) !== -1, "box model contains sample label");

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
