// End-to-end smoke test for macOS JavaScriptCore (no browser/Node needed):
//   * replicates the lesson's calculation chain via Stats and compares every
//     displayed string against R's output (tests/expected_pipeline.json);
//   * exercises every SVG plot generator on real data;
//   * syntax-checks app.js by evaluating it against a minimal DOM stub.
// Run from the repo root:
//   osascript -l JavaScript t-test-1-sample/tests/smoke_jxa.js
ObjC.import("Foundation");

function readFile(path) {
  return ObjC.unwrap($.NSString.stringWithContentsOfFileEncodingError(
    path, $.NSUTF8StringEncoding, null));
}

function run(argv) {
  const base = "T-Tests/t-test-1-sample";
  const shared = "shared";
  const Stats = eval(readFile(shared + "/js/stats.js") + "\n;Stats");
  const DATASETS = eval(readFile(base + "/js/datasets.js") + "\n;DATASETS");
  const Plots = (function () {
    // plots.js references the global `Stats`; provide it.
    const src = readFile(shared + "/js/plots.js");
    return new Function("Stats", src + "\n;return Plots;")(Stats);
  })();
  const boxModelHTML = eval(readFile(shared + "/js/boxmodel.js") + "\n;boxModelHTML");
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

  // ---- calculation chain on Mr. Han's data, default inputs ----
  const x = DATASETS["Mr. Han's Exam Marks"].columns[0].values;
  const n = x.length, df = n - 1;
  const xbar = Stats.mean(x), s = Stats.sd(x);
  const nullMuStr = Stats.roundStr(140, 3);

  assertEqual(n, expected.n, "n");
  assertEqual(nullMuStr, expected.box_mu, "box mu string");
  assertEqual(Stats.roundStr(s, 3), expected.box_s, "box s string");
  assertEqual(Stats.roundStr(xbar, 3), expected.box_ov, "box OV string");

  const EVStr = Stats.roundStr(Number(nullMuStr), 5);
  const SE = s / Math.sqrt(n);
  const SEStr = Stats.roundStr(SE, 5);
  assertEqual(EVStr, expected.EV_string, "EV string");
  assertEqual(SEStr, expected.SE_string, "SE string");
  assertEqual(Stats.roundStr(s, 5), expected.sd_5, "sd (5dp) string");
  assertEqual(Stats.roundStr(xbar, 5), expected.ov_5, "OV (5dp) string");

  const tsStr = Stats.roundStr((xbar - Number(EVStr)) / Number(SEStr), 4);
  assertEqual(tsStr, expected.ts_string, "test statistic string");

  const ts = Number(tsStr);
  const p1 = 2 * (1 - Stats.pt(Math.abs(ts), df));
  const p2 = 1 - Stats.pt(ts, df);
  const p3 = Stats.pt(ts, df);
  assertEqual(Stats.roundStr(p1, 5), expected.p_two_sided_5, "p two-sided (5dp)");
  assertEqual(Stats.roundStr(p2, 5), expected.p_greater_5, "p greater (5dp)");
  assertEqual(Stats.roundStr(p3, 5), expected.p_less_5, "p less (5dp)");

  // The confidence level is derived as 1 - alpha, so alpha = 0.05 <-> 95%.
  const alpha = 0.05;
  const twoLower = xbar - Stats.qt(1 - alpha / 2, df) * SE;
  const twoUpper = xbar + Stats.qt(1 - alpha / 2, df) * SE;
  const greaterLower = xbar - Stats.qt(1 - alpha, df) * SE;
  const lessUpper = xbar + Stats.qt(1 - alpha, df) * SE;
  assertEqual(Stats.roundStr(twoLower, 4), expected.ci_two_lower_4, "CI two-sided lower");
  assertEqual(Stats.roundStr(twoUpper, 4), expected.ci_two_upper_4, "CI two-sided upper");
  assertEqual(Stats.roundStr(greaterLower, 4), expected.ci_greater_lower_4, "CI greater lower");
  assertEqual(Stats.roundStr(lessUpper, 4), expected.ci_less_upper_4, "CI less upper");

  // Deriving the level from alpha is what guarantees the two routes agree.
  const mu0 = 140;
  assertTrue((mu0 < twoLower || mu0 > twoUpper) === (p1 <= alpha), "CI/p agree, two-sided");
  assertTrue((mu0 < greaterLower) === (p2 <= alpha), "CI/p agree, greater");
  assertTrue((mu0 > lessUpper) === (p3 <= alpha), "CI/p agree, less");

  // ---- plots generate valid-looking SVG on several datasets ----
  const irisSepal = DATASETS["iris"].columns[0].values;
  const chick = DATASETS["ChickWeight"].columns[0].values;
  const plotOutputs = [
    Plots.histogramSVG(x, { main: "Histogram", col: "blue", breaks: 30 }),
    Plots.histogramSVG(chick, { breaks: 30 }),
    Plots.boxplotSVG(x, { horizontal: true, col: "blue" }),
    Plots.boxplotSVG(irisSepal, { main: "Boxplot of Sample Data" }),
    Plots.qqPlotSVG(x, {}),
    Plots.qqPlotSVG(irisSepal, {}),
    Plots.shadedTCurveSVG(24, ts, 1, {}),
    Plots.shadedTCurveSVG(24, ts, 2, {}),
    Plots.shadedTCurveSVG(24, -ts, 3, {}),
    Plots.shadedTCurveSVG(24, 8.2, 1, {}),   // test stat beyond +/-3.5 widens the axis
    Plots.shadedTCurveSVG(24, -8.2, 3, {})
  ];
  plotOutputs.forEach(function (svg, i) {
    assertTrue(typeof svg === "string" && svg.indexOf("<svg") === 0, "plot " + i + " is SVG");
    assertTrue(svg.indexOf("NaN") === -1 && svg.indexOf("Infinity") === -1, "plot " + i + " has no NaN/Infinity");
  });

  // Boxplot stats agree with R's fivenum-based hinges on the Han data.
  const bs = Plots.boxplotStats(x);
  assertTrue(bs.lower < bs.median && bs.median < bs.upper, "boxplot hinge ordering");

  // ---- box model HTML ----
  const bm = boxModelHTML("&mu; = 140; &sigma; &asymp; s = 4.751", "OV = 142.843", "n = 25");
  assertTrue(bm.indexOf("&sigma; &asymp; s = 4.751") !== -1, "box model contains box label");
  assertTrue(bm.indexOf("OV = 142.843") !== -1, "box model contains sample label");
  assertTrue(bm.indexOf("n = 25") !== -1, "box model contains n label");

  // ---- app.js driven through a DOM stub ----
  // Runs the real module, fires DOMContentLoaded and selects "Pre-uploaded
  // Data", so this asserts the wiring rather than only that the file parses.
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

    // The box must say sigma ~= s, never "sigma = s": s is an estimate.
    assertTrue(els["box-model"].innerHTML.indexOf("&sigma; &asymp; s = " + expected.box_s) !== -1,
      "box model labels sigma as approximately s");
    assertTrue(els["box-model"].innerHTML.indexOf("&sigma; = s") === -1, "box model does not assert sigma = s");

    const tsShown = els["ts-out"].innerHTML.match(/&= (-?[\d.]+)\\end\{align\*\}/);
    assertEqual(tsShown && tsShown[1], expected.ts_string, "displayed TS matches R");
    const pShown = els["p-value-prelude"].innerHTML.match(/p = ([\d.]+)/);
    assertEqual(pShown && pShown[1], expected.p_two_sided_5, "displayed p matches R (two-sided default)");
    assertTrue(els["p-value-prelude"].innerHTML.indexOf("assuming the null hypothesis is true") !== -1,
      "p-value definition states the null assumption");
    assertTrue(els["p-value-prelude"].innerHTML.indexOf("falls on that t-curve") !== -1,
      "p-value section says t-curve, not normal curve");
    assertTrue(els["p-value-prelude"].innerHTML.indexOf(" = " + expected.df + "\\)") !== -1,
      "p-value section shows the degrees of freedom");

    assertTrue(els["ci-out"].innerHTML.indexOf(expected.ci_two_lower_4) !== -1, "displayed CI bound matches R");
    assertTrue(els["ci-out"].innerHTML.indexOf("reject the null hypothesis") !== -1, "CI reaches a verdict");
    assertTrue(els["conclusion-out"].innerHTML.indexOf("accept the null") === -1, "no 'accept the null' wording");
    assertTrue(els["conf-level-out"].innerHTML.indexOf("0.95") !== -1, "confidence level derived from alpha");
  } catch (e) {
    checks++; failures++;
    lines.push("FAIL app.js wiring: " + e.message);
  }

  lines.push((checks - failures) + "/" + checks + " checks passed");
  if (failures > 0) lines.push("SMOKE TESTS FAILED");
  return lines.join("\n");
}
