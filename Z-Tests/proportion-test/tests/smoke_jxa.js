// End-to-end smoke test for macOS JavaScriptCore (no browser/Node needed):
//   * replicates the proportion-test chain via Stats and compares every
//     displayed string against R (tests/expected_pipeline.json), including the
//     Wilson score CI;
//   * checks the normal-approximation inequalities and that the p-value and CI
//     conclusions agree once the confidence level is derived as 1 - alpha;
//   * exercises the plot generators (shaded normal, density histogram w/ curve);
//   * syntax-checks app.js against a minimal DOM stub.
// Run from the repo root:
//   osascript -l JavaScript Z-Tests/proportion-test/tests/smoke_jxa.js
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
  const expected = JSON.parse(readFile("Z-Tests/proportion-test/tests/expected_pipeline.json"));

  const lines = [];
  let failures = 0, checks = 0;
  function assertEqual(a, e, label) { checks++; if (a !== e) { failures++; lines.push("FAIL " + label + ": got " + JSON.stringify(a) + ", expected " + JSON.stringify(e)); } }
  function assertTrue(c, label) { checks++; if (!c) { failures++; lines.push("FAIL " + label); } }

  const p0 = 0.7, n = 50, x = 37, alpha = 0.05;
  const sd = Math.sqrt(p0 * (1 - p0));
  const ov = x / n;

  // The observed value comes from the count, exactly -- no pre-rounded input.
  assertEqual(Stats.roundStr(ov, 5), expected.ov, "OV = x/n");
  assertEqual(Stats.roundStr(sd, 5), expected.sigma, "sigma = sqrt(p0(1-p0))");

  // Normal-approximation inequalities: both must clear 10 for the default
  // example, so the lesson never ships a default that fails its own check.
  // Rounded: 50 * (1 - 0.7) is 15.000000000000002 in binary floating point, so
  // the page renders these through roundStr rather than raw.
  assertEqual(Stats.roundStr(n * p0, 4), String(expected.np0), "n*p0");
  assertEqual(Stats.roundStr(n * (1 - p0), 4), String(expected.nq0), "n*(1-p0)");
  assertTrue(n * p0 >= 10 && n * (1 - p0) >= 10, "default example satisfies both inequalities");

  const EVStr = Stats.roundStr(p0, 5), SEStr = Stats.roundStr(sd / Math.sqrt(n), 5);
  assertEqual(EVStr, expected.EV, "EV");
  assertEqual(SEStr, expected.SE, "SE");
  const tsStr = Stats.roundStr((ov - Number(EVStr)) / Number(SEStr), 4);
  assertEqual(tsStr, expected.ts, "TS");
  const z = Number(tsStr);
  assertEqual(Stats.roundStr(2 * (1 - Stats.pnorm(Math.abs(z))), 5), expected.p_two, "p two-sided");
  assertEqual(Stats.roundStr(1 - Stats.pnorm(z), 5), expected.p_greater, "p greater");
  assertEqual(Stats.roundStr(Stats.pnorm(z), 5), expected.p_less, "p less");

  // Wilson score CI at the derived confidence level 1 - alpha.
  function wilson(pHat, nn, zz) {
    const denom = 1 + zz * zz / nn;
    const margin = zz * Math.sqrt(pHat * (1 - pHat) / nn + zz * zz / (4 * nn * nn));
    return { lower: (pHat + zz * zz / (2 * nn) - margin) / denom, upper: (pHat + zz * zz / (2 * nn) + margin) / denom };
  }
  const ci2 = wilson(ov, n, Stats.qnorm(1 - alpha / 2));
  const ci1 = wilson(ov, n, Stats.qnorm(1 - alpha));
  assertEqual(Stats.roundStr(ci2.lower, 4), expected.ci_two_lower_4, "CI two lower");
  assertEqual(Stats.roundStr(ci2.upper, 4), expected.ci_two_upper_4, "CI two upper");
  assertEqual(Stats.roundStr(ci1.lower, 4), expected.ci_greater_lower_4, "CI greater lower");
  assertEqual(Stats.roundStr(ci1.upper, 4), expected.ci_less_upper_4, "CI less upper");

  // The two-sided interval must match R's own prop.test(correct = FALSE).
  assertEqual(Stats.roundStr(ci2.lower, 4), expected.prop_test_ci[0], "CI matches prop.test lower");
  assertEqual(Stats.roundStr(ci2.upper, 4), expected.prop_test_ci[1], "CI matches prop.test upper");

  // The whole point of deriving the confidence level from alpha: the p-value and
  // the CI can never disagree. The Wilson interval inverts this exact test, so
  // p0 sits inside the interval precisely when p > alpha. Sweep every possible
  // count, at several alphas, for all three alternatives -- the page now tells
  // students the two routes always match, so that claim is worth pinning down.
  for (const a of [0.01, 0.05, 0.1, 0.2]) {
    for (let xi = 0; xi <= n; xi++) {
      const ph = xi / n;
      const zz = Number(Stats.roundStr((ph - Number(EVStr)) / Number(SEStr), 4));
      // two-sided
      let ci = wilson(ph, n, Stats.qnorm(1 - a / 2));
      assertTrue((2 * (1 - Stats.pnorm(Math.abs(zz))) <= a) === (p0 < ci.lower || p0 > ci.upper),
        "two-sided routes agree at x=" + xi + ", alpha=" + a);
      // one-sided
      ci = wilson(ph, n, Stats.qnorm(1 - a));
      assertTrue(((1 - Stats.pnorm(zz)) <= a) === (p0 < ci.lower),
        "greater routes agree at x=" + xi + ", alpha=" + a);
      assertTrue((Stats.pnorm(zz) <= a) === (p0 > ci.upper),
        "less routes agree at x=" + xi + ", alpha=" + a);
    }
  }

  // ---- assumption-3 histogram binning ----
  // The sample mean only lands on the lattice s/n, so bins must be an exact
  // integer number of lattice steps wide and edges must sit midway between
  // attainable values. Otherwise some bins swallow two lattice points and others
  // one, and the comb pattern makes a bell-shaped distribution look bimodal.
  // Mirrors the rule in app.js renderEmpirical().
  function binEdges(sMin, sMax, nn) {
    const step = Math.max(1, Math.ceil((sMax - sMin + 1) / 40));
    const bks = [];
    for (let s = sMin; s <= sMax + step; s += step) bks.push((s - 0.5) / nn);
    return { bks: bks, step: step };
  }
  for (const cfg of [[50, 22, 46], [30, 12, 29], [100, 35, 65], [1000, 645, 755], [7, 0, 7], [2, 0, 2]]) {
    const nn = cfg[0], r = binEdges(cfg[1], cfg[2], nn);
    const w0 = r.bks[1] - r.bks[0];
    let uniform = true, lattice = true;
    for (let i = 1; i < r.bks.length - 1; i++) {
      if (Math.abs((r.bks[i + 1] - r.bks[i]) - w0) > 1e-9) uniform = false;
    }
    // width must be step/n exactly, and every edge an odd half-multiple of 1/n
    if (Math.abs(w0 - r.step / nn) > 1e-9) lattice = false;
    for (const b of r.bks) {
      if (Math.abs((b * nn + 0.5) - Math.round(b * nn + 0.5)) > 1e-9) lattice = false;
    }
    assertTrue(uniform, "n=" + nn + ": histogram bins are all the same width");
    assertTrue(lattice, "n=" + nn + ": bin width is an exact multiple of 1/n, edges between lattice points");
    assertTrue(r.bks[0] < cfg[1] / nn && r.bks[r.bks.length - 1] > cfg[2] / nn,
      "n=" + nn + ": bins span every observed value");
  }

  // ---- plots ----
  assertTrue(Plots.shadedNormalCurveSVG(z, 1, {}).indexOf("<svg") === 0, "shaded normal curve renders");
  const data = []; for (let i = 0; i < 800; i++) { let s = 0; for (let k = 0; k < n; k++) s += Math.random() < p0 ? 1 : 0; data.push(s / n); }
  const hist = Plots.densityHistogramSVG(data, { breaks: 15, curve: { ev: p0, se: sd / Math.sqrt(n) } });
  assertTrue(hist.indexOf("<rect") !== -1 && hist.indexOf('stroke="red"') !== -1 && hist.indexOf("NaN") === -1, "empirical histogram has bars + red curve");
  assertTrue(boxModelHTML("1, 1, 1, 1, 1, 1, 1, 0, 0, 0", "Sample Mean", "n = 50").indexOf("Sample Mean") !== -1, "box model renders");

  // ---- app.js against DOM stub ----
  try {
    const appSrc = readFile("Z-Tests/proportion-test/js/app.js");
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
