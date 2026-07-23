// End-to-end smoke test for macOS JavaScriptCore (no browser/Node needed):
//   * checks the Demo 2 p-value strings (normal + t) against R ground truth
//     (tests/expected_pipeline.json);
//   * exercises the new plot generators (density overlay, shaded normal/t);
//   * syntax-checks app.js by evaluating it against a minimal DOM stub.
// Run from the repo root:
//   osascript -l JavaScript t-curve-motivation/tests/smoke_jxa.js
ObjC.import("Foundation");

function readFile(path) {
  return ObjC.unwrap($.NSString.stringWithContentsOfFileEncodingError(
    path, $.NSUTF8StringEncoding, null));
}

function run(argv) {
  const base = "t-curve-motivation";
  const shared = "shared";
  const Stats = eval(readFile(shared + "/js/stats.js") + "\n;Stats");
  const Plots = (function () {
    const src = readFile(shared + "/js/plots.js");
    return new Function("Stats", src + "\n;return Plots;")(Stats);
  })();
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

  // ---- Demo 2 p-values vs R ----
  expected.forEach(function (c) {
    const pNorm = Stats.roundStr(2 * (1 - Stats.pnorm(Math.abs(c.ts))), 5);
    const pT = Stats.roundStr(2 * (1 - Stats.pt(Math.abs(c.ts), c.df)), 5);
    assertEqual(pNorm, c.p_normal, "p_normal(ts=" + c.ts + ")");
    assertEqual(pT, c.p_t, "p_t(ts=" + c.ts + ", df=" + c.df + ")");
  });

  // ---- plots generate valid SVG across the control ranges ----
  const plotOutputs = [
    Plots.densityOverlaySVG(1, true, {}),
    Plots.densityOverlaySVG(25, true, {}),
    Plots.densityOverlaySVG(10, false, {}),     // normal curve hidden
    Plots.shadedNormalCurveSVG(1, 1, {}),
    Plots.shadedNormalCurveSVG(-1.96, 1, {}),
    Plots.shadedTCurveSVG(1, 1, 1, {}),
    Plots.shadedTCurveSVG(50, 3.2, 1, {}),
    Plots.shadedNormalCurveSVG(5, 1, {})        // beyond +/-3.5, axis widens
  ];
  plotOutputs.forEach(function (svg, i) {
    assertTrue(typeof svg === "string" && svg.indexOf("<svg") === 0, "plot " + i + " is SVG");
    assertTrue(svg.indexOf("NaN") === -1 && svg.indexOf("Infinity") === -1, "plot " + i + " has no NaN/Infinity");
  });

  // ---- blank test statistic (NaN): curve draws, no shading, p = NA ----
  const blank = Plots.shadedNormalCurveSVG(NaN, 1, {});
  assertTrue(blank.indexOf("<svg") === 0 && blank.indexOf("NaN") === -1, "blank ts -> clean curve SVG");
  assertEqual(Stats.roundStr(2 * (1 - Stats.pnorm(Math.abs(NaN))), 5), "NA", "blank ts -> p = NA");

  // ---- app.js evaluates cleanly against a DOM stub ----
  try {
    const appSrc = readFile(base + "/js/app.js");
    new Function("document", "window", "Stats", "Plots", appSrc)(
      { addEventListener: function () {}, getElementById: function () { return null; }, querySelectorAll: function () { return []; } },
      {}, Stats, Plots
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
