// Smoke test for macOS JavaScriptCore (no browser/Node needed). Checks the
// deterministic EV/SE + probability arithmetic against R
// (tests/expected_pipeline.json), exercises the plot generators, and loads
// app.js against a DOM stub. Run from the repo root:
//   osascript -l JavaScript box-model-part-3/tests/smoke_jxa.js
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
  const expected = JSON.parse(readFile("box-model-part-3/tests/expected_pipeline.json"));

  const lines = [];
  let failures = 0, checks = 0;
  function assertEqual(a, e, label) { checks++; if (a !== e) { failures++; lines.push("FAIL " + label + ": got " + JSON.stringify(a) + ", expected " + JSON.stringify(e)); } }
  function assertTrue(c, label) { checks++; if (!c) { failures++; lines.push("FAIL " + label); } }

  const pnormGen = (x, ev, se) => Stats.pnorm((x - ev) / se);
  function evse(box, n, mode) {
    const mu = Stats.mean(box), sigma = Stats.popsd(box);
    return mode === "mean" ? { ev: mu, se: sigma / Math.sqrt(n) } : { ev: n * mu, se: Math.sqrt(n) * sigma };
  }

  const coin = [1, 0], dice = [1, 2, 3, 4, 5, 6];
  const cs = evse(coin, 100, "sum"), cm = evse(coin, 100, "mean"), ds = evse(dice, 50, "sum");

  assertEqual(Stats.roundStr(cs.ev, 5), expected.coin_sum_ev, "coin sum EV");
  assertEqual(Stats.roundStr(cs.se, 5), expected.coin_sum_se, "coin sum SE");
  assertEqual(Stats.roundStr(cm.ev, 5), expected.coin_mean_ev, "coin mean EV");
  assertEqual(Stats.roundStr(cm.se, 5), expected.coin_mean_se, "coin mean SE");
  assertEqual(Stats.roundStr(ds.ev, 5), expected.dice_sum_ev, "dice sum EV");
  assertEqual(Stats.roundStr(ds.se, 5), expected.dice_sum_se, "dice sum SE");

  assertEqual(Stats.roundStr(1 - pnormGen(60, cs.ev, cs.se), 5), expected.p_coin_ge_60, "P(coin sum >= 60)");
  assertEqual(Stats.roundStr(1 - pnormGen(70, cs.ev, cs.se), 5), expected.p_coin_ge_70, "P(coin sum >= 70)");
  assertEqual(Stats.roundStr(pnormGen(70, cs.ev, cs.se) - pnormGen(40, cs.ev, cs.se), 5), expected.p_coin_40_70, "P(40 <= coin sum <= 70)");
  assertEqual(Stats.roundStr(1 - pnormGen(150, ds.ev, ds.se), 5), expected.p_dice_ge_150, "P(dice sum >= 150)");

  // ---- plots ----
  const data = []; for (let i = 0; i < 800; i++) { let s = 0; for (let k = 0; k < 100; k++) s += Math.random() < 0.5 ? 1 : 0; data.push(s); }
  const model = Plots.densityHistogramSVG(data, { breaks: 20, curve: { ev: 50, se: 5 } });
  assertTrue(model.indexOf("<rect") !== -1 && model.indexOf("stroke=\"red\"") !== -1 && model.indexOf("NaN") === -1, "normal-model histogram has bars + red curve, no NaN");
  const shaded = Plots.shadedNormalRegionSVG(50, 5, 60, null, {});
  assertTrue(shaded.indexOf("<svg") === 0 && shaded.indexOf("rgba(255,0,0,0.5)") !== -1 && shaded.indexOf("NaN") === -1, "shaded normal region renders");
  const shadedBoth = Plots.shadedNormalRegionSVG(50, 5, null, null, {});
  assertTrue(shadedBoth.indexOf("NaN") === -1, "shaded region with both infinities is clean");
  assertTrue(boxModelHTML("1, 0", "Sample Sum", "n = 100").indexOf("Sample Sum") !== -1, "box model renders");

  // ---- app.js against DOM stub ----
  try {
    const appSrc = readFile("box-model-part-3/js/app.js");
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
