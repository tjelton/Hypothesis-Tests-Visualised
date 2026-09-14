// End-to-end smoke test for macOS JavaScriptCore (no browser/Node needed):
//   * checks the defining property the lesson teaches -- that each Wilson
//     endpoint is the candidate proportion whose own prediction interval just
//     reaches p-hat -- against R (tests/expected_pipeline.json);
//   * pins the slider geometry, so tick 5 really is the exact endpoint on both
//     sliders and no other tick is;
//   * sweeps a wide range of x, n and confidence levels to confirm the traffic
//     light always reads red -> green -> yellow going inwards;
//   * exercises the plot generator;
//   * drives app.js against a DOM stub, including the reveal of the answer box.
// Run from the repo root:
//   osascript -l JavaScript Z-Tests/wilson-confidence-interval/tests/smoke_jxa.js
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
  const expected = JSON.parse(readFile("Z-Tests/wilson-confidence-interval/tests/expected_pipeline.json"));

  const lines = [];
  let failures = 0, checks = 0;
  function assertEqual(a, e, label) { checks++; if (a !== e) { failures++; lines.push("FAIL " + label + ": got " + JSON.stringify(a) + ", expected " + JSON.stringify(e)); } }
  function assertTrue(c, label) { checks++; if (!c) { failures++; lines.push("FAIL " + label); } }

  // ---- the model, mirroring js/app.js ----
  function wilson(pHat, n, z) {
    const denom = 1 + z * z / n;
    const margin = z * Math.sqrt(pHat * (1 - pHat) / n + z * z / (4 * n * n));
    return { lower: (pHat + z * z / (2 * n) - margin) / denom, upper: (pHat + z * z / (2 * n) + margin) / denom };
  }
  const seOf = (p, n) => Math.sqrt(p * (1 - p) / n);
  function geometry(x, n, conf) {
    const z = Stats.qnorm(1 - (1 - conf / 100) / 2), ph = x / n;
    const ci = wilson(ph, n, z);
    return {
      ph: ph, z: z, ci: ci,
      left: { a: Math.max(ci.lower - (ph - ci.lower), ci.lower / 2), mid: ci.lower, b: ph },
      right: { a: ph, mid: ci.upper, b: Math.min(ci.upper + (ci.upper - ph), 1 - (1 - ci.upper) / 2) }
    };
  }
  const tickValue = (g, t) => t <= 5 ? g.a + (g.mid - g.a) * t / 5 : g.mid + (g.b - g.mid) * (t - 5) / 5;
  const TOL = 1e-9;
  function lightFor(p, n, z, ph, side) {
    const s = seOf(p, n), lo = p - z * s, hi = p + z * s;
    const aimed = side === "left" ? hi : lo;
    if (Math.abs(aimed - ph) <= TOL) return "green";
    return (lo <= ph && ph <= hi) ? "yellow" : "red";
  }

  // ---- default example, against R ----
  const x = expected.x, n = expected.n, conf = expected.conf;
  const g = geometry(x, n, conf);
  assertEqual(Stats.roundStr(g.ph, 5), expected.p_hat, "p-hat = x/n");
  assertEqual(Stats.roundStr(g.z, 5), expected.z, "z critical value");
  assertEqual(Stats.roundStr(g.ci.lower, 4), expected.ci_lower_4, "Wilson lower (4dp)");
  assertEqual(Stats.roundStr(g.ci.upper, 4), expected.ci_upper_4, "Wilson upper (4dp)");
  assertEqual(Stats.roundStr(g.ci.lower, 10), expected.ci_lower_10, "Wilson lower (10dp)");
  assertEqual(Stats.roundStr(g.ci.upper, 10), expected.ci_upper_10, "Wilson upper (10dp)");

  // The identity the whole lesson rests on, to 10dp: the lower endpoint's UPPER
  // prediction limit is p-hat, and the upper endpoint's LOWER limit is p-hat.
  const piHi = (p) => p + g.z * seOf(p, n), piLo = (p) => p - g.z * seOf(p, n);
  assertEqual(Stats.roundStr(piHi(g.ci.lower), 10), expected.lower_pi_hi_10, "lower endpoint's upper limit sits on p-hat");
  assertEqual(Stats.roundStr(piLo(g.ci.upper), 10), expected.upper_pi_lo_10, "upper endpoint's lower limit sits on p-hat");
  assertEqual(Stats.roundStr(piHi(g.ci.lower), 10), Stats.roundStr(g.ph, 10), "...and that limit equals p-hat exactly");
  assertEqual(Stats.roundStr(piLo(g.ci.upper), 10), Stats.roundStr(g.ph, 10), "...on the other side too");
  assertEqual(Stats.roundStr(piLo(g.ci.lower), 4), expected.lower_pi_lo_4, "lower endpoint's lower limit");
  assertEqual(Stats.roundStr(piHi(g.ci.upper), 4), expected.upper_pi_hi_4, "upper endpoint's upper limit");
  assertEqual(Stats.roundStr(seOf(g.ci.lower, n), 4), expected.se_lower_4, "SE at lower endpoint");
  assertEqual(Stats.roundStr(seOf(g.ci.upper, n), 4), expected.se_upper_4, "SE at upper endpoint");

  // The interval is exactly R's uncorrected prop.test interval.
  assertEqual(Stats.roundStr(g.ci.lower, 4), expected.prop_test_ci_4[0], "matches prop.test lower");
  assertEqual(Stats.roundStr(g.ci.upper, 4), expected.prop_test_ci_4[1], "matches prop.test upper");

  // ...and binom::binom.confint(method = "wilson"), which the answer box's
  // "Method 2" tells students to run. The page quotes R's 7 significant
  // digits, so that rounding is pinned too.
  assertEqual(Stats.roundStr(g.ci.lower, 10), expected.binom_confint_lower_10, "matches binom.confint lower");
  assertEqual(Stats.roundStr(g.ci.upper, 10), expected.binom_confint_upper_10, "matches binom.confint upper");
  assertEqual(Stats.roundStr(g.ci.lower, 7), expected.binom_confint_lower_7, "quotes binom.confint lower at R's printed precision");
  assertEqual(Stats.roundStr(g.ci.upper, 7), expected.binom_confint_upper_7, "quotes binom.confint upper at R's printed precision");

  // ---- slider geometry, against R ----
  assertEqual(Stats.roundStr(g.left.a, 10), expected.left_far_10, "left slider outer end");
  assertEqual(Stats.roundStr(g.right.b, 10), expected.right_far_10, "right slider outer end");
  for (let t = 0; t <= 10; t++) {
    assertEqual(Stats.roundStr(tickValue(g.left, t), 6), expected.left_ticks_6[t], "left slider tick " + t);
    assertEqual(Stats.roundStr(tickValue(g.right, t), 6), expected.right_ticks_6[t], "right slider tick " + t);
  }
  // Both sliders run in the same direction as the plot's axis.
  for (let t = 1; t <= 10; t++) {
    assertTrue(tickValue(g.left, t) > tickValue(g.left, t - 1), "left slider increases with tick " + t);
    assertTrue(tickValue(g.right, t) > tickValue(g.right, t - 1), "right slider increases with tick " + t);
  }

  // ---- the traffic light, swept ----
  // Every combination below must read red -> green at 5 -> yellow on the left,
  // and yellow -> green at 5 -> red on the right. That is what the "Your task"
  // box promises students, so it is worth pinning down rather than assuming.
  const NS = [5, 8, 20, 37, 50, 100, 500, 2000];
  const CONFS = [50, 80, 90, 95, 99];
  for (const nn of NS) {
    for (const cf of CONFS) {
      for (let xi = 1; xi < nn; xi++) {
        // Sweeping every count for the large n would take minutes; a spread of
        // counts across the whole range exercises the same code paths.
        if (nn > 50 && xi % Math.ceil(nn / 25) !== 0) continue;
        const gg = geometry(xi, nn, cf);
        assertTrue(gg.left.a > 0 && gg.left.a < 1 && gg.right.b > 0 && gg.right.b < 1,
          "outer ends are real proportions at x=" + xi + ", n=" + nn + ", conf=" + cf);
        assertTrue(gg.ci.lower > 0 && gg.ci.upper < 1 && gg.ci.lower < gg.ph && gg.ph < gg.ci.upper,
          "endpoints straddle p-hat at x=" + xi + ", n=" + nn + ", conf=" + cf);
        let greensL = 0, greensR = 0, bad = "";
        for (let t = 0; t <= 10; t++) {
          const l = lightFor(tickValue(gg.left, t), nn, gg.z, gg.ph, "left");
          const r = lightFor(tickValue(gg.right, t), nn, gg.z, gg.ph, "right");
          const wantL = t < 5 ? "red" : (t === 5 ? "green" : "yellow");
          const wantR = t < 5 ? "yellow" : (t === 5 ? "green" : "red");
          if (l !== wantL) bad += " left@" + t + "=" + l;
          if (r !== wantR) bad += " right@" + t + "=" + r;
          if (l === "green") greensL++;
          if (r === "green") greensR++;
        }
        assertTrue(bad === "", "traffic light at x=" + xi + ", n=" + nn + ", conf=" + cf + ":" + bad);
        assertTrue(greensL === 1 && greensR === 1,
          "exactly one green per slider at x=" + xi + ", n=" + nn + ", conf=" + cf);
      }
    }
  }

  // ---- plot ----
  const curves = [
    { p: g.ci.lower, se: seOf(g.ci.lower, n), lo: piLo(g.ci.lower), hi: piHi(g.ci.lower), state: "green", side: "left" },
    { p: g.ci.upper, se: seOf(g.ci.upper, n), lo: piLo(g.ci.upper), hi: piHi(g.ci.upper), state: "green", side: "right" }
  ];
  const svg = Plots.wilsonPredictionSVG(g.ph, curves, [-0.2, 0.8], { ymax: 12 });
  assertTrue(svg.indexOf("<svg") === 0, "plot renders an svg");
  assertTrue(svg.indexOf("NaN") === -1, "plot has no NaN coordinates");
  assertTrue(svg.indexOf("rgba(40,167,69") !== -1, "green interval is shaded green");
  assertTrue((svg.match(/<polygon/g) || []).length >= 3, "two shaded intervals plus the star");
  assertTrue(svg.indexOf('stroke-dasharray="6,4"') !== -1, "left curve is dashed");
  assertTrue(svg.indexOf('stroke-dasharray="10,4,2,4"') !== -1, "right curve uses a different dash");
  assertTrue(svg.indexOf('stroke="#555"') === -1, "no vertical guide line at p-hat");
  // The answer box's sketch: same curves, plus the finished interval drawn
  // underneath with a riser at each endpoint.
  const sketch = Plots.wilsonPredictionSVG(g.ph, curves, [-0.2, 0.8],
    { ciBar: { lower: g.ci.lower, upper: g.ci.upper } });
  assertTrue(sketch.indexOf("confidence interval") !== -1, "sketch labels the interval");
  assertTrue(sketch.indexOf(Stats.roundStr(g.ci.lower, 4)) !== -1, "sketch labels the lower endpoint");
  assertTrue(sketch.indexOf(Stats.roundStr(g.ci.upper, 4)) !== -1, "sketch labels the upper endpoint");
  assertTrue(sketch.indexOf("NaN") === -1, "sketch has no NaN coordinates");
  assertTrue(sketch.length > svg.length, "sketch draws more than the plain plot");

  const red = Plots.wilsonPredictionSVG(g.ph, [Object.assign({}, curves[0], { state: "red" })], [-0.2, 0.8], {});
  assertTrue(red.indexOf("rgba(220,53,69") !== -1, "red interval is shaded red");
  // A zero-SD candidate has no curve to draw, but must not break the plot.
  assertTrue(Plots.wilsonPredictionSVG(g.ph, [{ p: 0, se: 0, lo: 0, hi: 0, state: "red", side: "left" }],
    [-0.2, 0.8], {}).indexOf("NaN") === -1, "degenerate candidate does not render NaN");

  // ---- app.js against a DOM stub ----
  // The stub is live enough to actually run the page: values go in, listeners
  // fire, innerHTML comes back out.
  function makeDom(initial) {
    const els = {}, listeners = {};
    function el(id) {
      if (!els[id]) {
        const classes = {};
        els[id] = {
          value: (id in initial) ? initial[id] : "",
          innerHTML: "",
          classList: {
            add: function (c) { classes[c] = true; },
            remove: function (c) { delete classes[c]; },
            toggle: function (c, on) { if (on) classes[c] = true; else delete classes[c]; },
            contains: function (c) { return !!classes[c]; }
          },
          addEventListener: function (ev, fn) { (listeners[id] = listeners[id] || []).push(fn); }
        };
      }
      return els[id];
    }
    return {
      doc: { getElementById: el, addEventListener: function () {}, querySelector: function () { return null; }, querySelectorAll: function () { return []; } },
      el: el,
      fire: function (id) { (listeners[id] || []).forEach(function (f) { f({ target: el(id) }); }); }
    };
  }

  try {
    const appSrc = readFile("Z-Tests/wilson-confidence-interval/js/app.js");
    const dom = makeDom({ "x-input": "9", "n-input": "30", "conf-input": "95", "left-slider": "0", "right-slider": "10" });
    new Function("document", "window", "Stats", "Plots", appSrc)(dom.doc, {}, Stats, Plots);
    checks++;

    const answer = dom.el("answer-box");
    assertTrue(answer.classList.contains("d-none"), "answer box hidden on load");
    assertTrue(dom.el("wilson-plot").innerHTML.indexOf("<svg") === 0, "plot drawn on load");
    assertTrue(dom.el("left-status").innerHTML.indexOf("RED") !== -1, "left slider starts red");
    assertTrue(dom.el("right-status").innerHTML.indexOf("RED") !== -1, "right slider starts red");

    // One slider green is not enough to reveal the interval.
    dom.el("left-slider").value = "5"; dom.fire("left-slider");
    assertTrue(dom.el("left-status").innerHTML.indexOf("GREEN") !== -1, "left slider green at tick 5");
    assertTrue(answer.classList.contains("d-none"), "answer box still hidden with one slider green");

    dom.el("right-slider").value = "5"; dom.fire("right-slider");
    assertTrue(!answer.classList.contains("d-none"), "answer box revealed once both are green");
    assertTrue(answer.innerHTML.indexOf(expected.ci_lower_4) !== -1, "answer box shows the R-checked lower endpoint");
    assertTrue(answer.innerHTML.indexOf(expected.ci_upper_4) !== -1, "answer box shows the R-checked upper endpoint");
    assertTrue(answer.innerHTML.indexOf("<svg") !== -1, "answer box opens with the sketch");
    // The interval's LaTeX spacer is written "\\\\;" in app.js so that JS hands
    // MathJax a real "\\;". Written with one backslash, JS silently collapses it to a
    // bare ";" and the interval renders as "(0.1666, ; 0.4788)".
    assertTrue(answer.innerHTML.indexOf(", ; ") === -1, "no stray semicolon inside the interval");
    assertTrue(answer.innerHTML.indexOf(", \\; ") !== -1, "interval uses a LaTeX thin space");
    assertTrue(answer.innerHTML.indexOf("confidence interval</text>") !== -1, "sketch draws the interval bar");
    assertTrue(answer.innerHTML.indexOf("answer-acc-1") !== -1, "Method 1 is a collapsible box");
    assertTrue(answer.innerHTML.indexOf("answer-acc-2") !== -1, "Method 2 is a collapsible box");
    assertTrue((answer.innerHTML.match(/accordion-button collapsed/g) || []).length === 2, "both methods start collapsed");
    assertTrue(answer.innerHTML.indexOf('binom.confint(9, 30, conf.level = 0.95, methods = "wilson")') !== -1, "Method 2 shows the binom.confint call");
    assertTrue(answer.innerHTML.indexOf(expected.binom_confint_lower_7) !== -1, "Method 2 quotes binom.confint's lower bound");
    assertTrue(answer.innerHTML.indexOf(expected.binom_confint_upper_7) !== -1, "Method 2 quotes binom.confint's upper bound");
    assertTrue(answer.innerHTML.indexOf("prop.test") === -1, "Method 2 no longer mentions base R");

    // Moving off the answer hides it again.
    dom.el("left-slider").value = "6"; dom.fire("left-slider");
    assertTrue(answer.classList.contains("d-none"), "answer box hidden again once a slider leaves green");

    // Changing the data resets both sliders, so the new endpoints have to be
    // found rather than handed over.
    dom.el("left-slider").value = "5"; dom.fire("left-slider");
    dom.el("right-slider").value = "5"; dom.fire("right-slider");
    assertTrue(!answer.classList.contains("d-none"), "answer box back after re-finding both");
    dom.el("n-input").value = "40"; dom.fire("n-input");
    assertTrue(answer.classList.contains("d-none"), "answer box hidden after an input change");
    assertEqual(dom.el("left-slider").value, 0, "left slider reset by an input change");
    assertEqual(dom.el("right-slider").value, 10, "right slider reset by an input change");

    // Validation: x must stay strictly inside (0, n), and a rejected value is
    // rolled back rather than left to produce a degenerate curve.
    dom.el("x-input").value = "40"; dom.fire("x-input");
    assertTrue(!dom.el("x-warning").classList.contains("d-none"), "x = n is rejected");
    assertEqual(dom.el("x-input").value, 9, "rejected x rolled back");
    dom.el("x-input").value = "0"; dom.fire("x-input");
    assertTrue(!dom.el("x-warning").classList.contains("d-none"), "x = 0 is rejected");
    dom.el("x-input").value = "10"; dom.fire("x-input");
    assertTrue(dom.el("x-warning").classList.contains("d-none"), "valid x accepted");
    dom.el("conf-input").value = "120"; dom.fire("conf-input");
    assertTrue(!dom.el("conf-warning").classList.contains("d-none"), "confidence level above 99 rejected");
    dom.el("conf-input").value = "99"; dom.fire("conf-input");
    assertTrue(dom.el("conf-warning").classList.contains("d-none"), "valid confidence level accepted");

    // And the endpoints still line up after all that input churn: n = 40,
    // x = 10, 99% -- tick 5 must still be the green one on both sides.
    dom.el("left-slider").value = "5"; dom.fire("left-slider");
    dom.el("right-slider").value = "5"; dom.fire("right-slider");
    const g2 = geometry(10, 40, 99);
    assertTrue(dom.el("answer-box").innerHTML.indexOf(Stats.roundStr(g2.ci.lower, 4)) !== -1,
      "answer box tracks new inputs (lower)");
    assertTrue(dom.el("answer-box").innerHTML.indexOf(Stats.roundStr(g2.ci.upper, 4)) !== -1,
      "answer box tracks new inputs (upper)");
  } catch (e) { checks++; failures++; lines.push("FAIL app.js evaluation: " + e.message); }

  lines.push((checks - failures) + "/" + checks + " checks passed");
  if (failures > 0) lines.push("SMOKE TESTS FAILED");
  return lines.join("\n");
}
