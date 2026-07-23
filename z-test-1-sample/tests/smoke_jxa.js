// End-to-end smoke test for macOS JavaScriptCore (no browser/Node needed):
//   * replicates the z-test calculation chain (sigma = sample SD) via Stats and
//     compares every displayed string against R (tests/expected_pipeline.json);
//   * exercises the plot generators (boxplot, histogram, QQ, shaded normal);
//   * syntax-checks app.js against a minimal DOM stub.
// Run from the repo root:
//   osascript -l JavaScript z-test-1-sample/tests/smoke_jxa.js
ObjC.import("Foundation");

function readFile(path) {
  return ObjC.unwrap($.NSString.stringWithContentsOfFileEncodingError(
    path, $.NSUTF8StringEncoding, null));
}

function run(argv) {
  const base = "z-test-1-sample";
  const shared = "shared";
  const Stats = eval(readFile(shared + "/js/stats.js") + "\n;Stats");
  const DATASETS = eval(readFile(base + "/js/datasets.js") + "\n;DATASETS");
  const Plots = (function () {
    const src = readFile(shared + "/js/plots.js");
    return new Function("Stats", src + "\n;return Plots;")(Stats);
  })();
  const boxModelHTML = (function () {
    const src = readFile(shared + "/js/boxmodel.js");
    return new Function(src + "\n;return boxModelHTML;")();
  })();
  const expected = JSON.parse(readFile(base + "/tests/expected_pipeline.json"));

  const lines = [];
  let failures = 0, checks = 0;
  function assertEqual(a, e, label) { checks++; if (a !== e) { failures++; lines.push("FAIL " + label + ": got " + JSON.stringify(a) + ", expected " + JSON.stringify(e)); } }
  function assertTrue(c, label) { checks++; if (!c) { failures++; lines.push("FAIL " + label); } }

  // ---- Mr. Han's data, default inputs (null 140, sigma = sample SD) ----
  const x = DATASETS["Mr. Han's Math Class"].columns[0].values;
  const n = x.length, xbar = Stats.mean(x), sigma = Stats.sd(x);
  const nullMuStr = Stats.roundStr(140, 3);

  assertEqual(n, expected.n, "n");
  assertEqual(nullMuStr, expected.box_mu, "box mu");
  assertEqual(Stats.roundStr(sigma, 3), expected.box_sigma, "box sigma");
  assertEqual(Stats.roundStr(xbar, 3), expected.box_ov, "box OV");

  const EVStr = Stats.roundStr(140, 5);
  const SE = sigma / Math.sqrt(n);
  const SEStr = Stats.roundStr(SE, 5);
  assertEqual(EVStr, expected.EV_string, "EV string");
  assertEqual(SEStr, expected.SE_string, "SE string");
  assertEqual(Stats.roundStr(sigma, 5), expected.sigma_5, "sigma (5dp)");
  assertEqual(Stats.roundStr(xbar, 5), expected.ov_5, "OV (5dp)");

  const tsStr = Stats.roundStr((xbar - Number(EVStr)) / Number(SEStr), 4);
  assertEqual(tsStr, expected.ts_string, "test statistic");

  const z = Number(tsStr);
  assertEqual(Stats.roundStr(2 * (1 - Stats.pnorm(Math.abs(z))), 5), expected.p_two_sided_5, "p two-sided");
  assertEqual(Stats.roundStr(1 - Stats.pnorm(z), 5), expected.p_greater_5, "p greater");
  assertEqual(Stats.roundStr(Stats.pnorm(z), 5), expected.p_less_5, "p less");

  // CI centred on EV (the null mean), matching the R source.
  const ci_xbar = Number(EVStr), alpha = 1 - 0.95;
  assertEqual(Stats.roundStr(ci_xbar - Stats.qnorm(1 - alpha / 2) * SE, 4), expected.ci_two_lower_4, "CI two lower");
  assertEqual(Stats.roundStr(ci_xbar + Stats.qnorm(1 - alpha / 2) * SE, 4), expected.ci_two_upper_4, "CI two upper");
  assertEqual(Stats.roundStr(ci_xbar - Stats.qnorm(1 - alpha) * SE, 4), expected.ci_greater_lower_4, "CI greater lower");
  assertEqual(Stats.roundStr(ci_xbar + Stats.qnorm(1 - alpha) * SE, 4), expected.ci_less_upper_4, "CI less upper");

  // ---- plots ----
  const plots = [
    Plots.boxplotSVG(x, { horizontal: true, col: "blue" }),
    Plots.histogramSVG(x, { breaks: 30 }),
    Plots.qqPlotSVG(x, {}),
    Plots.shadedNormalCurveSVG(z, 1, {}),
    Plots.shadedNormalCurveSVG(z, 2, {}),
    Plots.shadedNormalCurveSVG(z, 3, {})
  ];
  plots.forEach(function (svg, i) {
    assertTrue(typeof svg === "string" && svg.indexOf("<svg") === 0, "plot " + i + " is SVG");
    assertTrue(svg.indexOf("NaN") === -1 && svg.indexOf("Infinity") === -1, "plot " + i + " has no NaN/Infinity");
  });
  const bm = boxModelHTML("&mu; = 140; &sigma; = 7.5", "OV = 142.843", "n = 25");
  assertTrue(bm.indexOf("&sigma; = 7.5") !== -1, "box model contains sigma label");

  // ---- app.js against DOM stub ----
  try {
    const appSrc = readFile(base + "/js/app.js");
    new Function("document", "window", "DATASETS", "Stats", "Plots", "boxModelHTML", appSrc)(
      { addEventListener: function () {}, getElementById: function () { return null; }, querySelectorAll: function () { return []; } },
      {}, DATASETS, Stats, Plots, boxModelHTML
    );
    checks++;
  } catch (e) { checks++; failures++; lines.push("FAIL app.js evaluation: " + e.message); }

  lines.push((checks - failures) + "/" + checks + " checks passed");
  if (failures > 0) lines.push("SMOKE TESTS FAILED");
  return lines.join("\n");
}
