// Smoke test for macOS JavaScriptCore (no browser/Node needed). This lesson is
// a pure simulation explainer with no R ground truth, so the test checks that
// the shared box-model helpers render as expected and that app.js loads without
// error against a minimal DOM stub. Run from the repo root:
//   osascript -l JavaScript box-model-part-1/tests/smoke_jxa.js
ObjC.import("Foundation");

function readFile(path) {
  return ObjC.unwrap($.NSString.stringWithContentsOfFileEncodingError(
    path, $.NSUTF8StringEncoding, null));
}

function run(argv) {
  const shared = "shared";
  const bm = (function () {
    const src = readFile(shared + "/js/boxmodel.js");
    return new Function(src + "\n;return { boxModelHTML: boxModelHTML, sampleCellHTML: sampleCellHTML, sampleGridHTML: sampleGridHTML };")();
  })();

  const lines = [];
  let failures = 0, checks = 0;
  function assertTrue(c, label) { checks++; if (!c) { failures++; lines.push("FAIL " + label); } }

  // ---- box-model helpers ----
  const box = bm.boxModelHTML("Head (H), Tail (T)", "Sample", "n = 5");
  assertTrue(box.indexOf("Head (H), Tail (T)") !== -1, "box label present");
  assertTrue(box.indexOf("Sample") !== -1 && box.indexOf("n = 5") !== -1, "sample + n labels present");

  const cell = bm.sampleCellHTML("H T T H T");
  assertTrue(cell.indexOf("H T T H T") !== -1 && cell.indexOf("#f9ffbd") !== -1, "sample cell renders");

  assertTrue(bm.sampleGridHTML([]) === "", "empty grid -> empty string");
  const grid = bm.sampleGridHTML(["1", "2", "3"]);
  assertTrue((grid.match(/#f9ffbd/g) || []).length === 3, "grid has one cell per label");

  // ---- app.js evaluates cleanly against a DOM stub ----
  try {
    const appSrc = readFile("box-model-part-1/js/app.js");
    new Function("document", "boxModelHTML", "sampleCellHTML", "sampleGridHTML", appSrc)(
      { addEventListener: function () {}, getElementById: function () { return null; }, querySelector: function () { return null; }, querySelectorAll: function () { return []; } },
      bm.boxModelHTML, bm.sampleCellHTML, bm.sampleGridHTML
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
