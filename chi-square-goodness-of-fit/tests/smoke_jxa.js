// End-to-end smoke test for macOS JavaScriptCore (no browser/Node needed):
//   * replicates the chi-square goodness-of-fit chain for both scenarios via
//     Stats and compares every displayed string against R
//     (tests/expected_pipeline.json): expected counts, O - E, (O-E)^2/E, the
//     summed test statistic, the upper-tail p-value, and the Cochran verdict;
//   * checks the smart box-ticket logic (fair die -> "1, 2, 3, 4, 5, 6");
//   * exercises the shaded chi-square curve plot generator;
//   * syntax-checks app.js against a minimal DOM stub.
// Run from the repo root:
//   osascript -l JavaScript chi-square-goodness-of-fit/tests/smoke_jxa.js
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
  const expected = JSON.parse(readFile("chi-square-goodness-of-fit/tests/expected_pipeline.json"));

  const lines = [];
  let failures = 0, checks = 0;
  function assertEqual(a, e, label) { checks++; if (a !== e) { failures++; lines.push("FAIL " + label + ": got " + JSON.stringify(a) + ", expected " + JSON.stringify(e)); } }
  function assertTrue(c, label) { checks++; if (!c) { failures++; lines.push("FAIL " + label); } }

  // Replicate app.js's calculation chain for a scenario.
  function checkChain(name, observed, nullProps, exp) {
    const k = observed.length;
    const n = observed.reduce((a, b) => a + b, 0);
    assertEqual(n, exp.n, name + " n");
    // Expected counts rounded to 2 dp, matching app.js.
    const expectedCounts = nullProps.map(p => Stats.roundR(n * p, 2));
    const df = k - 1;
    assertEqual(df, exp.df, name + " df");

    let sumContrib = 0, numLt5 = 0, anyZero = false;
    for (let i = 0; i < k; i++) {
      const O = observed[i], E = expectedCounts[i], diff = O - E, contrib = (diff * diff) / E;
      sumContrib += contrib;
      if (E < 5) numLt5++;
      if (E <= 0) anyZero = true;
      assertEqual(Stats.roundStr(E, 4), exp.expected[i], name + " E[" + i + "]");
      assertEqual(Stats.roundStr(diff, 4), exp.diff[i], name + " diff[" + i + "]");
      assertEqual(Stats.roundStr(contrib, 4), exp.contrib[i], name + " contrib[" + i + "]");
    }
    const tsStr = Stats.roundStr(sumContrib, 4);
    assertEqual(tsStr, exp.ts, name + " TS");

    const pStr = Stats.roundStr(Stats.pchisqUpper(Number(tsStr), df), 5);
    assertEqual(pStr, exp.p, name + " p");

    // Cochran verdict.
    assertEqual(numLt5, exp.num_lt5, name + " num_lt5");
    const holds = !anyZero && (numLt5 <= 0.2 * k + 1e-9);
    assertEqual(holds, exp.cochran_holds, name + " cochran holds");
  }

  checkChain("die", [8, 12, 9, 11, 15, 5], [1 / 6, 1 / 6, 1 / 6, 1 / 6, 1 / 6, 1 / 6], expected.die);
  checkChain("reject", [2, 3, 40, 5], [0.45, 0.45, 0.05, 0.05], expected.reject);

  // ---- smart box tickets (mirrors ticketsString in app.js) ----
  function ticketsString(props) {
    const k = props.length;
    const clean = props.map(p => (Number.isFinite(p) && p > 0) ? p : 0);
    let m = 0;
    for (let cand = 1; cand <= 100; cand++) {
      if (clean.every(p => Math.abs(p * cand - Math.round(p * cand)) < 5e-3)) { m = cand; break; }
    }
    if (m > 0) {
      const counts = clean.map(p => Math.round(p * m));
      const total = counts.reduce((a, b) => a + b, 0);
      if (total >= 1 && total <= 24) {
        const parts = [];
        for (let i = 0; i < k; i++) for (let j = 0; j < counts[i]; j++) parts.push(String(i + 1));
        return parts.join(", ");
      }
    }
    return clean.map((p, i) => '"' + (i + 1) + '" x ' + Stats.formatR(Stats.roundR(p * 100, 2)) + "%").join(", ");
  }
  // Fair die: one ticket per face, from rounded inputs (0.16667 each).
  assertEqual(ticketsString(Array(6).fill(Number("0.16667"))), "1, 2, 3, 4, 5, 6", "tickets fair die");
  // Unequal small integers: 0.5/0.3/0.2 -> 5+3+2 tickets.
  assertEqual(ticketsString([0.5, 0.3, 0.2]), "1, 1, 1, 1, 1, 2, 2, 2, 3, 3", "tickets 0.5/0.3/0.2");
  // Many equal categories fall back to the percentage form.
  assertTrue(ticketsString(Array(50).fill(0.02)).indexOf('"1" x 2%') === 0, "tickets 50-category percentage form");
  assertTrue(boxModelHTML("1, 2, 3, 4, 5, 6", null, null, 20).indexOf("1, 2, 3, 4, 5, 6") !== -1, "box model renders");

  // ---- plot ----
  const svg = Plots.shadedChiSquareCurveSVG(5, 6, { width: 560, height: 325 });
  assertTrue(svg.indexOf("<svg") === 0, "chi-square curve renders");
  assertTrue(svg.indexOf("rgba(255,0,0,0.5)") !== -1, "chi-square curve shades upper tail");
  assertTrue(svg.indexOf('stroke="blue"') !== -1, "chi-square curve marks test statistic");
  assertTrue(svg.indexOf("NaN") === -1, "chi-square curve has no NaN");

  // ---- app.js against DOM stub (syntax / top-level evaluation) ----
  try {
    const appSrc = readFile("chi-square-goodness-of-fit/js/app.js");
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
