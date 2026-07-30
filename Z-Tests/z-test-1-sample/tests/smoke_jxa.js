// End-to-end smoke test for macOS JavaScriptCore (no browser/Node needed):
//   * replicates the z-test calculation chain via Stats and compares every
//     displayed string against R (tests/expected_pipeline.json), for both sigma
//     scenarios the page offers (the case study's known 7.5, and the sample SD
//     produced by the "Set Population SD to be Sample SD" button);
//   * checks the CI/p-value duality that the corrected CI now guarantees;
//   * exercises the plot generators (boxplot, histogram, QQ, shaded normal);
//   * syntax-checks app.js against a minimal DOM stub.
// Run from the repo root:
//   osascript -l JavaScript Z-Tests/z-test-1-sample/tests/smoke_jxa.js
ObjC.import("Foundation");

function readFile(path) {
  return ObjC.unwrap($.NSString.stringWithContentsOfFileEncodingError(
    path, $.NSUTF8StringEncoding, null));
}

function run(argv) {
  const base = "Z-Tests/z-test-1-sample";
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

  // ---- Mr. Han's data, null mu = 140 ----
  const x = DATASETS["Mr. Han's Exam Marks"].columns[0].values;
  const n = x.length, xbar = Stats.mean(x);
  const alpha = expected.alpha;

  assertEqual(n, expected.n, "n");
  assertEqual(Stats.roundStr(140, 3), expected.box_mu, "box mu");
  assertEqual(Stats.roundStr(xbar, 3), expected.box_ov, "box OV");

  const EVStr = Stats.roundStr(140, 5);
  assertEqual(EVStr, expected.EV_string, "EV string");
  assertEqual(Stats.roundStr(xbar, 5), expected.ov_5, "OV (5dp)");

  // The known-sigma scenario is the page's default (Mr. Han's case study gives
  // sigma = 7.5); the sample-SD scenario is what the button produces.
  const scenarios = [
    { key: "known", sigma: 7.5, label: "known sigma" },
    { key: "sample", sigma: Stats.sd(x), label: "sample SD" }
  ];

  scenarios.forEach(function (sc) {
    const exp = expected[sc.key], tag = " [" + sc.label + "]";
    const SE = sc.sigma / Math.sqrt(n);
    const SEStr = Stats.roundStr(SE, 5);

    assertEqual(Stats.roundStr(sc.sigma, 3), exp.box_sigma, "box sigma" + tag);
    assertEqual(Stats.roundStr(sc.sigma, 5), exp.sigma_5, "sigma (5dp)" + tag);
    assertEqual(SEStr, exp.SE_string, "SE string" + tag);

    const tsStr = Stats.roundStr((xbar - Number(EVStr)) / Number(SEStr), 4);
    assertEqual(tsStr, exp.ts_string, "test statistic" + tag);

    const z = Number(tsStr);
    const pTwo = 2 * (1 - Stats.pnorm(Math.abs(z)));
    const pGreater = 1 - Stats.pnorm(z);
    const pLess = Stats.pnorm(z);
    assertEqual(Stats.roundStr(pTwo, 5), exp.p_two_sided_5, "p two-sided" + tag);
    assertEqual(Stats.roundStr(pGreater, 5), exp.p_greater_5, "p greater" + tag);
    assertEqual(Stats.roundStr(pLess, 5), exp.p_less_5, "p less" + tag);

    // CI centred on the OBSERVED mean, at confidence level 1 - alpha.
    const zTwo = Stats.qnorm(1 - alpha / 2), zOne = Stats.qnorm(1 - alpha);
    const twoLower = xbar - zTwo * SE, twoUpper = xbar + zTwo * SE;
    const greaterLower = xbar - zOne * SE, lessUpper = xbar + zOne * SE;
    assertEqual(Stats.roundStr(twoLower, 4), exp.ci_two_lower_4, "CI two lower" + tag);
    assertEqual(Stats.roundStr(twoUpper, 4), exp.ci_two_upper_4, "CI two upper" + tag);
    assertEqual(Stats.roundStr(greaterLower, 4), exp.ci_greater_lower_4, "CI greater lower" + tag);
    assertEqual(Stats.roundStr(lessUpper, 4), exp.ci_less_upper_4, "CI less upper" + tag);

    // The point of centring on the observed mean and deriving the level from
    // alpha: each interval must reach the same verdict as its p-value.
    const mu0 = 140;
    assertTrue((mu0 < twoLower || mu0 > twoUpper) === (pTwo <= alpha), "CI/p agree, two-sided" + tag);
    assertTrue((mu0 < greaterLower) === (pGreater <= alpha), "CI/p agree, greater" + tag);
    assertTrue((mu0 > lessUpper) === (pLess <= alpha), "CI/p agree, less" + tag);
  });

  // ---- plots ----
  const zDefault = Number(expected.known.ts_string);
  const plots = [
    Plots.boxplotSVG(x, { horizontal: true, col: "blue" }),
    Plots.histogramSVG(x, { breaks: 30 }),
    Plots.qqPlotSVG(x, {}),
    Plots.shadedNormalCurveSVG(zDefault, 1, {}),
    Plots.shadedNormalCurveSVG(zDefault, 2, {}),
    Plots.shadedNormalCurveSVG(zDefault, 3, {})
  ];
  plots.forEach(function (svg, i) {
    assertTrue(typeof svg === "string" && svg.indexOf("<svg") === 0, "plot " + i + " is SVG");
    assertTrue(svg.indexOf("NaN") === -1 && svg.indexOf("Infinity") === -1, "plot " + i + " has no NaN/Infinity");
  });
  const bm = boxModelHTML("&mu; = 140; &sigma; = 7.5", "OV = 142.843", "n = 25");
  assertTrue(bm.indexOf("&sigma; = 7.5") !== -1, "box model contains sigma label");

  // ---- app.js driven through a DOM stub ----
  // Runs the real module, fires DOMContentLoaded, and selects "Pre-uploaded
  // Data" -- so this asserts the *wiring*, not just that the file parses. In
  // particular it pins the default the case study depends on: Mr. Han's data
  // must load sigma = 7.5, not the sample SD.
  try {
    const els = {};
    function makeEl(id) {
      return {
        id: id, innerHTML: "", value: "", textContent: "", selected: false,
        classList: { add: function () {}, remove: function () {}, toggle: function () {} },
        addEventListener: function (ev, fn) { (this._h = this._h || {})[ev] = fn; },
        appendChild: function (opt) { if (opt.selected) this.value = opt.value; }
      };
    }
    const radios = [];
    const doc = {
      _dom: null,
      getElementById: function (id) { return els[id] || (els[id] = makeEl(id)); },
      createElement: function () { return makeEl("opt"); },
      addEventListener: function (ev, fn) { if (ev === "DOMContentLoaded") this._dom = fn; },
      querySelectorAll: function (sel) {
        if (sel.indexOf("data_upload_choice") !== -1) {
          if (radios.length === 0) {
            ["pre_uploaded", "manually_specified"].forEach(function (v) {
              const r = makeEl("radio-" + v); r.value = v; radios.push(r);
            });
          }
          return radios;
        }
        return [];   // alternate-hypothesis radios: leave at the state default
      }
    };

    new Function("document", "window", "DATASETS", "Stats", "Plots", "boxModelHTML",
      readFile(base + "/js/app.js"))(doc, {}, DATASETS, Stats, Plots, boxModelHTML);

    doc._dom();
    radios[0]._h.change({ target: { value: "pre_uploaded" } });

    assertEqual(els["dataset-select"].value, "Mr. Han's Exam Marks", "default data set");
    assertEqual(String(els["pop-sd"].value), "7.5", "default sigma is the case study's known 7.5");
    assertTrue(els["box-model"].innerHTML.indexOf("&sigma; = 7.5") !== -1, "box model shows sigma 7.5");
    assertTrue(els["box-model"].innerHTML.indexOf("OV = " + expected.box_ov) !== -1, "box model shows OV");

    // Defaults are the one-sided greater test from the case study.
    const tsShown = els["ts-out"].innerHTML.match(/&= (-?[\d.]+)\\end\{align\*\}/);
    assertEqual(tsShown && tsShown[1], expected.known.ts_string, "displayed TS matches R");
    const pShown = els["p-value-prelude"].innerHTML.match(/p = ([\d.]+)/);
    assertEqual(pShown && pShown[1], expected.known.p_greater_5, "displayed p matches R (one-sided greater)");
    assertTrue(els["p-value-prelude"].innerHTML.indexOf("assuming the null hypothesis is true") !== -1,
      "p-value definition states the null assumption");

    // CI centred on the observed mean, so it rejects here in step with the p-value.
    assertTrue(els["ci-out"].innerHTML.indexOf(expected.known.ci_greater_lower_4) !== -1, "displayed CI bound matches R");
    assertTrue(els["ci-out"].innerHTML.indexOf("reject the null hypothesis") !== -1, "CI reaches a verdict");
    assertTrue(els["conclusion-out"].innerHTML.indexOf("accept the null") === -1, "no 'accept the null' wording");
    assertTrue(els["conf-level-out"].innerHTML.indexOf("0.95") !== -1, "confidence level derived from alpha");
  } catch (e) { checks++; failures++; lines.push("FAIL app.js wiring: " + e.message); }

  lines.push((checks - failures) + "/" + checks + " checks passed");
  if (failures > 0) lines.push("SMOKE TESTS FAILED");
  return lines.join("\n");
}
