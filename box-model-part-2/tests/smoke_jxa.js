// Smoke test for macOS JavaScriptCore (no browser/Node needed). This lesson is
// a pure simulation (CLT) explainer with no R ground truth, so the test checks
// that the density histogram renders (empty placeholder + populated), that the
// simulated sample-mean distribution behaves sanely, and that app.js loads
// against a minimal DOM stub. Run from the repo root:
//   osascript -l JavaScript box-model-part-2/tests/smoke_jxa.js
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

  const lines = [];
  let failures = 0, checks = 0;
  function assertTrue(c, label) { checks++; if (!c) { failures++; lines.push("FAIL " + label); } }

  // ---- empty placeholder histogram ----
  const empty = Plots.densityHistogramSVG([], { main: "n = 5", xlab: "Values" });
  assertTrue(empty.indexOf("<svg") === 0 && empty.indexOf("NaN") === -1, "empty histogram is clean SVG");

  // ---- populated density histogram (bars) ----
  const data = [];
  for (let i = 0; i < 500; i++) {
    let s = 0; for (let k = 0; k < 25; k++) s += Math.random() < 0.25 ? 1 : 0;  // sum of 25 Bernoulli(0.25)
    data.push(s / 25);
  }
  const hist = Plots.densityHistogramSVG(data, { breaks: 12, col: "lightgreen" });
  assertTrue(hist.indexOf("<rect") !== -1 && hist.indexOf("NaN") === -1, "populated histogram has bars, no NaN");
  const sturges = Plots.densityHistogramSVG(data, { breaks: "sturges" });
  assertTrue(sturges.indexOf("<rect") !== -1 && sturges.indexOf("NaN") === -1, "sturges histogram has bars, no NaN");

  // ---- sanity: mean of sample means approximates the box mean (0.25) ----
  const mm = data.reduce((a, b) => a + b, 0) / data.length;
  assertTrue(Math.abs(mm - 0.25) < 0.05, "sample-mean distribution centres near the box mean");

  // ---- box model helper ----
  assertTrue(boxModelHTML("1, 0, 0, 0", "Sample Mean", "n = 25").indexOf("Sample Mean") !== -1, "box model renders");

  // ---- app.js evaluates cleanly against a DOM stub ----
  try {
    const appSrc = readFile("box-model-part-2/js/app.js");
    new Function("document", "Stats", "Plots", "boxModelHTML", appSrc)(
      { addEventListener: function () {}, getElementById: function () { return null; }, querySelector: function () { return null; }, querySelectorAll: function () { return []; } },
      Stats, Plots, boxModelHTML
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
