// app.js -- interactive wiring for the "Confidence Intervals" lesson. Ports
// R/fundamentals_confidence_interval_srv.R. Two simulations, each generating
// 100 confidence intervals from normal samples and colouring them by whether
// they contain the true mean, with a running coverage tally. Sampling is random
// (Stats.rnorm) -- only the CI arithmetic (qnorm-based margins) is exact.

"use strict";

(function () {

  const $ = id => document.getElementById(id);
  const S = Stats;

  // Build 100 confidence intervals from N(trueMean, popSd) samples of size n at
  // the given confidence level. Returns bounds, coverage flags, and the miss count.
  function simulateCIs(trueMean, popSd, n, confLevel) {
    const percentile = (1 + confLevel / 100) / 2;
    const z = S.qnorm(percentile);
    const lowers = [], uppers = [], contains = [];
    let misses = 0;
    for (let i = 0; i < 100; i++) {
      const samp = S.rnorm(n, trueMean, popSd);
      const m = S.mean(samp);
      const margin = z * S.sd(samp) / Math.sqrt(n);
      const lo = m - margin, hi = m + margin;
      const covered = lo <= trueMean && hi >= trueMean;
      lowers.push(lo); uppers.push(hi); contains.push(covered);
      if (!covered) misses++;
    }
    return { lowers, uppers, contains, misses };
  }

  function historyHTML(total, misses) {
    const pct = total === 0 ? 0 : 100 * (total - misses) / total;
    return "<b>Running History:</b><br><ul>" +
      "<li>Total number of intervals generated so far: " + total + "</li>" +
      "<li>Total number of intervals NOT containing the population mean: " + misses + "</li>" +
      "<li>Percentage of intervals containing the population mean: " + S.roundR(pct, 2) + "%</li></ul>";
  }

  document.addEventListener("DOMContentLoaded", function () {

    // ---------- Demo 1: fixed N(100, 15), n = 30, 95% ----------
    const demo1 = { total: 0, misses: 0, ci: null };
    function renderDemo1() {
      const ci = demo1.ci;
      $("ci-plot-1").innerHTML = Plots.ciPlotSVG(
        ci ? ci.lowers : [], ci ? ci.uppers : [], ci ? ci.contains : [], 100,
        { width: 620, height: 430, main: "Simulated 95% Confidence Intervals" });
      $("history-1").innerHTML = historyHTML(demo1.total, demo1.misses);
    }
    $("simulate-1").addEventListener("click", function () {
      demo1.ci = simulateCIs(100, 15, 30, 95);
      demo1.total += 100; demo1.misses += demo1.ci.misses;
      renderDemo1();
    });
    $("reset-1").addEventListener("click", function () { demo1.total = 0; demo1.misses = 0; renderDemo1(); });
    renderDemo1();

    // ---------- Demo 2: user-controlled distribution + confidence/sample size ----------
    const demo2 = { total: 0, misses: 0, ci: null };
    const val = (id) => Number($(id).value);
    function syncLabels() {
      $("conf-level-val").textContent = val("conf-level");
      $("sample-size-val").textContent = val("sample-size");
      $("dist-mean-val").textContent = val("dist-mean");
      $("dist-sd-val").textContent = val("dist-sd");
    }
    function renderDemo2() {
      const mean = val("dist-mean"), sd = Math.max(val("dist-sd"), 1e-6), conf = val("conf-level");
      // Fixed axis: widest possible margin (max confidence 99, smallest n = 5).
      const marginMax = S.qnorm((1 + 99 / 100) / 2) * sd / Math.sqrt(5);
      const xlim = [mean - marginMax * 1.2, mean + marginMax * 1.2];
      const ci = demo2.ci;
      $("ci-plot-2").innerHTML = Plots.ciPlotSVG(
        ci ? ci.lowers : [], ci ? ci.uppers : [], ci ? ci.contains : [], mean,
        { width: 620, height: 430, main: "Simulated " + conf + "% Confidence Intervals", xlim });
      $("history-2").innerHTML = historyHTML(demo2.total, demo2.misses);
    }
    function resetDemo2History() { demo2.total = 0; demo2.misses = 0; demo2.ci = null; }

    $("simulate-2").addEventListener("click", function () {
      demo2.ci = simulateCIs(val("dist-mean"), Math.max(val("dist-sd"), 1e-6), val("sample-size"), val("conf-level"));
      demo2.total += 100; demo2.misses += demo2.ci.misses;
      renderDemo2();
    });
    $("reset-2").addEventListener("click", function () { resetDemo2History(); renderDemo2(); });
    // Changing any slider resets the running history (matching the R observers).
    for (const id of ["conf-level", "sample-size", "dist-mean", "dist-sd"]) {
      $(id).addEventListener("input", function () { syncLabels(); resetDemo2History(); renderDemo2(); });
    }
    syncLabels();
    renderDemo2();
  });

})();
