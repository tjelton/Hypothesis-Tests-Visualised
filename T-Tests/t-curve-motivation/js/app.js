// app.js -- interactive wiring for the "Introducing the t-Distribution
// (Motivation)" lesson. Ports R/ttest_t_curve_motivation_srv.R. This lesson
// has no data upload or hypothesis-test chain; it is a conceptual explainer
// with two independent demos:
//
//   Demo 1 (overlay): a df slider (1-25) and a "display normal curve" checkbox
//     drive a t-density curve (black) with an optional dashed standard-normal
//     overlay (red). Ports the `changing_df_graph` renderPlot.
//
//   Demo 2 (p-values): a test-statistic input and a df slider (1-50) drive two
//     side-by-side two-sided shaded plots -- a standard normal (z-test) and a
//     t-curve (t-test) -- with the corresponding p-values shown below each.
//     Ports the `test_stat_*_plot` / `p_value_*` outputs.
//
// Note: matching the R code (not its prose), Demo 1 draws the t-curve as a
// black solid line and the normal as a red dashed line.

"use strict";

(function () {

  const $ = id => document.getElementById(id);

  function typeset(el) {
    if (!window.MathJax) return;
    const nodes = el ? [el] : undefined;
    const run = () => window.MathJax.typesetPromise(nodes).catch(() => {});
    // MathJax loads async (deferred script): on first render it may not be ready
    // yet, so chain onto its startup promise to typeset once it has initialised.
    if (window.MathJax.startup && window.MathJax.startup.promise) {
      window.MathJax.startup.promise = window.MathJax.startup.promise.then(run);
    } else if (window.MathJax.typesetPromise) {
      run();
    }
  }

  // ---------- Demo 1: t-curve vs normal overlay ----------
  function renderOverlay() {
    const df = Number($("df-slider-1").value);
    const showNormal = $("display-normal").checked;
    $("df-slider-1-value").textContent = df;
    $("overlay-plot").innerHTML = Plots.densityOverlaySVG(df, showNormal, { width: 500, height: 300 });
  }

  // ---------- Demo 2: normal vs t p-value comparison ----------
  function renderPValueDemo() {
    const tsRaw = $("ts-input").value;
    const ts = tsRaw === "" ? NaN : Number(tsRaw);
    const df = Number($("df-slider-2").value);
    $("df-slider-2-value").textContent = df;

    // Two-sided (altChoice 1) shaded plots for each distribution.
    $("normal-plot").innerHTML = Plots.shadedNormalCurveSVG(ts, 1, { width: 360, height: 250 });
    $("t-plot").innerHTML = Plots.shadedTCurveSVG(df, ts, 1, { width: 360, height: 250 });

    // p-values: 2 * upper-tail probability of |ts| (NaN -> "NA" via formatR).
    const pNorm = 2 * (1 - Stats.pnorm(Math.abs(ts)));
    const pT = 2 * (1 - Stats.pt(Math.abs(ts), df));
    $("p-value-normal").innerHTML =
      "<p style='font-size: 16px; text-align: center;'>\\( p = " + Stats.roundStr(pNorm, 5) + " \\)</p>";
    $("p-value-t").innerHTML =
      "<p style='font-size: 16px; text-align: center;'>\\( p = " + Stats.roundStr(pT, 5) + " \\)</p>";
    typeset($("p-value-normal"));
    typeset($("p-value-t"));
  }

  // ---------- event bindings ----------
  document.addEventListener("DOMContentLoaded", function () {
    $("df-slider-1").addEventListener("input", renderOverlay);
    $("display-normal").addEventListener("change", renderOverlay);

    $("ts-input").addEventListener("input", renderPValueDemo);
    $("df-slider-2").addEventListener("input", renderPValueDemo);

    // Initial render with the default inputs.
    renderOverlay();
    renderPValueDemo();
  });

})();
