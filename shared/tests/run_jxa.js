// Stats-accuracy test runner for macOS's built-in JavaScriptCore (no Node).
// Verifies the shared js/stats.js against R-computed ground truth. Run from
// the repo root:
//   osascript -l JavaScript shared/tests/run_jxa.js
ObjC.import("Foundation");

function readFile(path) {
  return ObjC.unwrap($.NSString.stringWithContentsOfFileEncodingError(
    path, $.NSUTF8StringEncoding, null));
}

function run(argv) {
  const base = "shared";
  const statsSrc = readFile(base + "/js/stats.js");
  const checksSrc = readFile(base + "/tests/stats.checks.js");
  const ref = JSON.parse(readFile(base + "/tests/reference_values.json"));

  // eval returns its last expression; the sources are strict-mode so their
  // const bindings do not leak, hence the trailing expression to extract them.
  const Stats = eval(statsSrc + "\n;Stats");
  const runStatsChecks = eval(checksSrc + "\n;runStatsChecks");

  // The sample fixture is embedded in the reference file, so this suite does
  // not depend on any lesson's datasets.js.
  const hanValues = ref.sample_stats.han_input;

  const lines = [];
  const result = runStatsChecks(Stats, ref, hanValues, function (m) { lines.push(m); });
  lines.push((result.checks - result.failures) + "/" + result.checks + " checks passed");
  if (result.failures > 0) lines.push("TESTS FAILED");
  return lines.join("\n");
}
