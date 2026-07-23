// End-to-end smoke test for macOS JavaScriptCore (no browser/Node needed):
//   * replicates the proportion-test chain (sum AND mean) via Stats and
//     compares every displayed string against R (tests/expected_pipeline.json),
//     including the Wilson score CI;
//   * exercises the plot generators (shaded normal, density histogram w/ curve);
//   * syntax-checks app.js against a minimal DOM stub.
// Run from the repo root:
//   osascript -l JavaScript proportion-test/tests/smoke_jxa.js
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
  const boxModelHTML = (function () {
    const src = readFile(shared + "/js/boxmodel.js");
    return new Function(src + "\n;return boxModelHTML;")();
  })();
  const expected = JSON.parse(readFile("proportion-test/tests/expected_pipeline.json"));

  const lines = [];
  let failures = 0, checks = 0;
  function assertEqual(a, e, label) { checks++; if (a !== e) { failures++; lines.push("FAIL " + label + ": got " + JSON.stringify(a) + ", expected " + JSON.stringify(e)); } }
  function assertTrue(c, label) { checks++; if (!c) { failures++; lines.push("FAIL " + label); } }

  const p0 = 0.7, n = 30, ov = 0.73, sd = Math.sqrt(p0 * (1 - p0));

  function checkChain(mode, exp) {
    const EV = mode === "mean" ? p0 : n * p0;
    const SE = mode === "mean" ? sd / Math.sqrt(n) : Math.sqrt(n) * sd;
    const EVStr = Stats.roundStr(EV, 5), SEStr = Stats.roundStr(SE, 5);
    assertEqual(EVStr, exp.EV, mode + " EV");
    assertEqual(SEStr, exp.SE, mode + " SE");
    const tsStr = Stats.roundStr((ov - Number(EVStr)) / Number(SEStr), 4);
    assertEqual(tsStr, exp.ts, mode + " TS");
    const z = Number(tsStr);
    assertEqual(Stats.roundStr(2 * (1 - Stats.pnorm(Math.abs(z))), 5), exp.p_two, mode + " p two-sided");
    assertEqual(Stats.roundStr(1 - Stats.pnorm(z), 5), exp.p_greater, mode + " p greater");
    assertEqual(Stats.roundStr(Stats.pnorm(z), 5), exp.p_less, mode + " p less");
  }
  checkChain("mean", expected.mean);
  checkChain("sum", expected.sum);

  // Wilson score CI (two-sided at 0.95, one-sided at 0.95).
  function wilson(pHat, nn, z) {
    const denom = 1 + z * z / nn;
    const margin = z * Math.sqrt(pHat * (1 - pHat) / nn + z * z / (4 * nn * nn));
    return { lower: (pHat + z * z / (2 * nn) - margin) / denom, upper: (pHat + z * z / (2 * nn) + margin) / denom };
  }
  const ci2 = wilson(ov, n, Stats.qnorm(1 - 0.05 / 2));
  const ci1 = wilson(ov, n, Stats.qnorm(1 - 0.05));
  assertEqual(Stats.roundStr(ci2.lower, 4), expected.ci_two_lower_4, "CI two lower");
  assertEqual(Stats.roundStr(ci2.upper, 4), expected.ci_two_upper_4, "CI two upper");
  assertEqual(Stats.roundStr(ci1.lower, 4), expected.ci_greater_lower_4, "CI greater lower");
  assertEqual(Stats.roundStr(ci1.upper, 4), expected.ci_less_upper_4, "CI less upper");

  // ---- plots ----
  const z = Number(expected.mean.ts);
  assertTrue(Plots.shadedNormalCurveSVG(z, 1, {}).indexOf("<svg") === 0, "shaded normal curve renders");
  const data = []; for (let i = 0; i < 800; i++) { let s = 0; for (let k = 0; k < 30; k++) s += Math.random() < 0.7 ? 1 : 0; data.push(s / 30); }
  const hist = Plots.densityHistogramSVG(data, { breaks: 15, curve: { ev: 0.7, se: sd / Math.sqrt(30) } });
  assertTrue(hist.indexOf("<rect") !== -1 && hist.indexOf('stroke="red"') !== -1 && hist.indexOf("NaN") === -1, "empirical histogram has bars + red curve");
  assertTrue(boxModelHTML("1, 1, 1, 1, 1, 1, 1, 0, 0, 0", "Data Science Class", "n = 30").indexOf("Data Science Class") !== -1, "box model renders");

  // ---- app.js against DOM stub ----
  try {
    const appSrc = readFile("proportion-test/js/app.js");
    new Function("document", "window", "Stats", "Plots", "boxModelHTML", appSrc)(
      { addEventListener: function () {}, getElementById: function () { return null; }, querySelector: function () { return null; }, querySelectorAll: function () { return []; } },
      {}, Stats, Plots, boxModelHTML
    );
    checks++;
  } catch (e) { checks++; failures++; lines.push("FAIL app.js evaluation: " + e.message); }

  lines.push((checks - failures) + "/" + checks + " checks passed");
  if (failures > 0) lines.push("SMOKE TESTS FAILED");
  return lines.join("\n");
}
