// End-to-end smoke test for macOS JavaScriptCore (no browser/Node needed):
//   * replicates the test-of-independence chain for both scenarios via Stats and
//     compares every displayed string against R (tests/expected_pipeline.json):
//     expected counts E_ij, O - E, (O-E)^2/E, the summed test statistic, the
//     upper-tail p-value, and the Cochran verdict;
//   * exercises the shaded chi-square curve plot generator;
//   * syntax-checks app.js against a minimal DOM stub.
// Run from the repo root:
//   osascript -l JavaScript chi-square-test-of-independence/tests/smoke_jxa.js
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
  const expected = JSON.parse(readFile("Chi-Square-Tests/chi-square-test-of-independence/tests/expected_pipeline.json"));

  const lines = [];
  let failures = 0, checks = 0;
  function assertEqual(a, e, label) { checks++; if (a !== e) { failures++; lines.push("FAIL " + label + ": got " + JSON.stringify(a) + ", expected " + JSON.stringify(e)); } }
  function assertTrue(c, label) { checks++; if (!c) { failures++; lines.push("FAIL " + label); } }

  // Replicate app.js's calculation chain for a contingency table.
  function checkChain(name, obs, exp) {
    const rows = obs.length, cols = obs[0].length;
    assertEqual(rows, exp.rows, name + " rows");
    assertEqual(cols, exp.cols, name + " cols");
    const rowTot = obs.map(r => r.reduce((a, b) => a + b, 0));
    const colTot = [];
    for (let j = 0; j < cols; j++) { let s = 0; for (let i = 0; i < rows; i++) s += obs[i][j]; colTot.push(s); }
    const grand = rowTot.reduce((a, b) => a + b, 0);
    assertEqual(grand, exp.grand, name + " grand");

    const E = obs.map((r, i) => r.map((_, j) => Stats.roundR(rowTot[i] * colTot[j] / grand, 2)));
    let sum = 0, idx = 0, numLt5 = 0, anyZero = false;
    for (let i = 0; i < rows; i++) {
      for (let j = 0; j < cols; j++) {
        const O = obs[i][j], Eij = E[i][j], diff = O - Eij;
        assertEqual(Stats.roundStr(Eij, 2), exp.expected[idx], name + " E[" + idx + "]");
        assertEqual(Stats.roundStr(diff, 2), exp.diff[idx], name + " diff[" + idx + "]");
        const contrib = Eij > 0 ? (diff * diff) / Eij : 0;
        assertEqual(Stats.roundStr(contrib, 4), exp.contrib[idx], name + " contrib[" + idx + "]");
        sum += contrib;
        if (Eij < 5) numLt5++;
        if (Eij <= 0) anyZero = true;
        idx++;
      }
    }
    const tsStr = Stats.roundStr(sum, 4);
    assertEqual(tsStr, exp.ts, name + " TS");
    const df = (rows - 1) * (cols - 1);
    assertEqual(df, exp.df, name + " df");
    assertEqual(Stats.roundStr(Stats.pchisqUpper(Number(tsStr), df), 5), exp.p, name + " p");

    assertEqual(numLt5, exp.num_lt5, name + " num_lt5");
    const nCells = rows * cols;
    const holds = !anyZero && (numLt5 <= 0.2 * nCells + 1e-9);
    assertEqual(holds, exp.cochran_holds, name + " cochran holds");
  }

  checkChain("coffee", [[50, 30], [20, 40]], expected.coffee);
  checkChain("small", [[2, 3, 1], [4, 3, 2]], expected.small);

  // ---- plot ----
  const svg = Plots.shadedChiSquareCurveSVG(1, 11.6667, { width: 560, height: 325 });
  assertTrue(svg.indexOf("<svg") === 0, "chi-square curve renders");
  assertTrue(svg.indexOf("rgba(255,0,0,0.5)") !== -1, "chi-square curve shades upper tail");
  assertTrue(svg.indexOf('stroke="blue"') !== -1, "chi-square curve marks test statistic");
  assertTrue(svg.indexOf("NaN") === -1, "chi-square curve has no NaN");

  // ---- app.js against DOM stub (syntax / top-level evaluation) ----
  try {
    const appSrc = readFile("Chi-Square-Tests/chi-square-test-of-independence/js/app.js");
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
