// app.js -- interactive wiring for "The Box Model Part 3 - Modelling Using a
// Normal Distribution". Ports R/fundamentals_box_model_part_3_srv.R.
//
// The box playground lets the user configure tickets, number of draws, and
// sum/mean, then: (1) empirically test the CLT with a growing histogram,
// (2) specify the modelling normal N(EV, SE^2), and (3) find probabilities as
// areas under that normal curve. Draws are random (Math.random); only the
// EV/SE/probability arithmetic is deterministic and matches R.
//
//   sum:  EV = n * mean(tickets),   SE = sqrt(n) * popsd(tickets)
//   mean: EV = mean(tickets),       SE = popsd(tickets) / sqrt(n)
// Probabilities use the normal CDF with that mean/SE.

"use strict";

(function () {

  const $ = id => document.getElementById(id);
  const S = Stats;

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

  function drawWithReplacement(box, size) {
    const out = new Array(size);
    for (let i = 0; i < size; i++) out[i] = box[Math.floor(Math.random() * box.length)];
    return out;
  }
  function sum(a) { let s = 0; for (const v of a) s += v; return s; }
  function mean(a) { return sum(a) / a.length; }
  function simulateBox(mode, n, box) { const d = drawWithReplacement(box, n); return mode === "2" ? mean(d) : sum(d); }
  // Normal CDF with arbitrary mean/sd.
  function pnormGen(x, ev, se) { return S.pnorm((x - ev) / se); }

  const state = { tickets: [1, 0], draws: 100, mode: "1", empirical: [] };

  function modeWord() { return state.mode === "2" ? "mean" : "sum"; }
  function sampleLabel() { return state.mode === "2" ? "Sample Mean" : "Sample Sum"; }

  // EV/SE for the current box + sum/mean choice.
  function evse() {
    const mu = mean(state.tickets), sigma = S.popsd(state.tickets), n = state.draws;
    let ev, se;
    if (state.mode === "2") { ev = mu; se = sigma / Math.sqrt(n); }
    else { ev = n * mu; se = Math.sqrt(n) * sigma; }
    return { ev, se, mu, sigma, n };
  }

  function ticketsString() {
    let s = "", count = 0;
    for (const v of state.tickets) {
      count++;
      if (count === 15) { s += "\n" + v; count = 0; }
      else { s += ", " + v; }
    }
    return s.replace(/^, /, "");
  }

  document.addEventListener("DOMContentLoaded", function () {
    $("example-box").innerHTML = boxModelHTML("1, 0", "Sample Sum", "n = 100");

    // ---------- box parameters ----------
    function renderBox() {
      $("box-model").innerHTML = boxModelHTML(ticketsString(), sampleLabel(), "n = " + state.draws);
      const { mu, sigma } = evse();
      $("box-stats").innerHTML =
        "<p><b>Mean of the box:</b> $$ \\mu = " + S.roundStr(mu, 5) + "$$</p>" +
        "<p><b>Population SD of the box: </b> $$ \\sigma = " + S.roundStr(sigma, 5) + "$$</p>";
    }

    function renderCLTInstructions() {
      const w = modeWord();
      $("clt-instructions").innerHTML =
        "<p>Recall that the central limit theorem tells us that if we take a <b>sufficiently large number of draws</b> " +
        "from the box, then the <b>sample " + w + "s will follow an approximately normal distribution</b>.<br><br>Now we will empirically " +
        "test whether n = " + state.draws + " is a sufficient number of draws for the central limit theorem to " +
        "apply.<br><br>To do this, press the button below to repeat the process of drawing from the box " + state.draws +
        " and finding the " + w + ". These will be added to the histogram. If we have taken enough draws from the box, then the " +
        "histogram should look normally distributed.</p>";
      $("clt-satisfied").innerHTML =
        "<p>Does the data in the histogram above look normally distributed? Ensure that you have repeated the process of " +
        "drawing from the box, and finding the sample " + w + " many times. If it does not, scroll back above and update the " +
        "number of draws in step 2. If it does, continue below!</p>";
    }

    function renderCLTHist() {
      const isMean = state.mode === "2";
      const title = "Empiricial Distribution of Sample " + (isMean ? "Means" : "Sums") + " (n = " + state.empirical.length + ")";
      const xlab = isMean ? "Sample Mean Value" : "Sample Sum Value";
      const opts = { width: 540, height: 430, main: title, xlab, ylab: "Density", col: "lightgreen" };
      if (state.empirical.length === 0) { $("hist-clt").innerHTML = Plots.densityHistogramSVG([], opts); return; }
      opts.breaks = Math.min(20, new Set(state.empirical).size);
      $("hist-clt").innerHTML = Plots.densityHistogramSVG(state.empirical, opts);
    }

    // 10,000-sample histogram with the overlaid normal curve.
    function renderNormalModel() {
      const isMean = state.mode === "2";
      const { ev, se } = evse();
      const data = new Array(10000);
      for (let i = 0; i < 10000; i++) data[i] = simulateBox(state.mode, state.draws, state.tickets);
      const bins = Math.min(50, Math.round(new Set(data).size * 1.3));
      const title = "Empirical Distribution of 10000 Sample " + (isMean ? "Means" : "Sums") + " with\nOverlaid Normal Curve";
      $("normal-curve-plot").innerHTML = Plots.densityHistogramSVG(data, {
        width: 540, height: 430, main: title, xlab: isMean ? "Sample Mean Value" : "Sample Sum Value",
        ylab: "Density", col: "lightgreen", breaks: Math.max(1, bins), curve: { ev, se }
      });
    }

    function renderNormalText() {
      const w = modeWord();
      const { ev, se, mu, sigma, n } = evse();
      const evStr = S.roundStr(ev, 5), seStr = S.roundStr(se, 5);
      let evBlock, seBlock;
      if (state.mode === "2") {
        evBlock = "$$\\begin{align*} \\text{EV} &= \\mu \\\\ &=" + evStr + "\\end{align*}$$";
        seBlock = "$$\\begin{align*} \\text{SE} &= \\frac{\\sigma}{\\sqrt{n}} \\\\ &= \\frac{" + S.roundStr(sigma, 5) + "}{\\sqrt{" + n + "}}\\\\ &= " + seStr + "\\end{align*}$$";
      } else {
        evBlock = "$$\\begin{align*} \\text{EV} &= n \\times \\mu \\\\ &=" + n + "\\times" + S.roundStr(mu, 5) + "\\\\ &= " + evStr + "\\end{align*}$$";
        seBlock = "$$\\begin{align*} \\text{SE} &= \\sqrt{n} \\times \\sigma \\\\ &= \\sqrt{" + n + "} \\times" + S.roundStr(sigma, 5) + "\\\\ &= " + seStr + "\\end{align*}$$";
      }
      $("normal-text").innerHTML =
        "<p>Now that we have confirmed that we are taking a sufficient number of draws for the sample " + w + "s to follow a " +
        "normal distribution, we want to specify this general normal curve. We will set the mean to " +
        "be equal to the <b>sample " + w + "'s</b> expected value, and the standard deviation equal to its standard error:</p>" +
        "<p><b>Expected Value:</b></p>" + evBlock +
        "<p><b>Standard Error:</b></p>" + seBlock +
        "<p>Having found the expected value and standard error, we can model the distribution of the sample " + w + "s using the " +
        "following <b>general normal curve:</b></p>" +
        "$$\\begin{align*} \\text{Sample Sum} &\\sim N(\\text{EV}, \\text{SE}^2) \\\\ &= N(" + evStr + ", " + seStr + "^2) \\end{align*}$$";
    }

    function renderProbText() {
      const w = modeWord();
      const { ev, se } = evse();
      $("prob-text").innerHTML =
        "<p>Now that we are modelling the sample " + w + "s using a normal curve with mean " + S.roundStr(ev, 5) + " and standard deviation " +
        S.roundStr(se, 5) + ", we can start to ask probability based questions like, <br>" +
        "<ul><li>What is the chance that we see a value greater than \\(x\\)?</li>" +
        "<li>What is the chance that we see a value between \\(y\\) and \\(z\\)?</li></ul>" +
        "Use the controls below to find the the probabilities that values lie within the ranges you set.</p>";
    }

    // ---------- finding probabilities ----------
    function renderProbability() {
      const { ev, se } = evse();
      const lowerInf = $("lower-inf").checked;
      const upperInf = $("upper-inf").checked;
      const lv = $("lower-num").value === "" ? NaN : Number($("lower-num").value);
      const uv = $("upper-num").value === "" ? NaN : Number($("upper-num").value);
      const lower = (lowerInf || Number.isNaN(lv)) ? null : lv;   // null = -infinity
      const upper = (upperInf || Number.isNaN(uv)) ? null : uv;   // null = +infinity

      if (lower !== null && upper !== null && lower > upper) {
        $("interval-error").innerHTML = "<p style='color: red;'>ERROR: The lower interval cannot be greater than the upper interval.</p>";
        $("shaded-plot").innerHTML = "";
        $("prob-answer").innerHTML = "";
        return;
      }
      $("interval-error").innerHTML = "";

      let area;
      if (lower === null && upper === null) area = 1;
      else if (lower === null) area = pnormGen(upper, ev, se);
      else if (upper === null) area = 1 - pnormGen(lower, ev, se);
      else area = pnormGen(upper, ev, se) - pnormGen(lower, ev, se);

      const lowerStr = lower === null ? "-\\infty" : String(lower);
      const upperStr = upper === null ? "\\infty" : String(upper);
      $("shaded-plot").innerHTML = Plots.shadedNormalRegionSVG(ev, se, lower, upper, { width: 480, height: 340 });
      $("prob-answer").innerHTML =
        "<p>The probability that a value lies wthin the range \\([" + lowerStr + "," + upperStr + "]\\) is " + S.roundStr(area, 5) + ".</p>";
      typeset($("prob-answer"));
      typeset($("shaded-plot"));
    }

    // ---------- full re-render on box-parameter change ----------
    function renderAll() {
      state.empirical = [];
      renderBox();
      renderCLTInstructions();
      renderCLTHist();
      renderNormalModel();
      renderNormalText();
      renderProbText();
      renderProbability();
      // Typeset the whole document (no arg): MathJax's startup pass already
      // processed the outer container, so re-typesetting that same ancestor node
      // finds no math — a full-document pass reliably picks up the injected LaTeX.
      typeset();
    }

    // ---------- events ----------
    $("submit-tickets").addEventListener("click", function () {
      const nums = $("box-tickets-entry").value.split(",").map(s => Number(s.trim()));
      if (nums.some(Number.isNaN) || nums.length <= 1) { $("tickets-error").classList.remove("d-none"); state.tickets = [1, 0, 0, 0]; }
      else { $("tickets-error").classList.add("d-none"); state.tickets = nums; }
      renderAll();
    });
    $("number-of-draws").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (!Number.isNaN(v) && v >= 1) { state.draws = Math.ceil(v); $("draws-error").classList.add("d-none"); }
      else { state.draws = 25; $("draws-error").classList.remove("d-none"); }
      renderAll();
    });
    for (const r of document.querySelectorAll('input[name="sum_or_mean"]')) {
      r.addEventListener("change", function () { state.mode = this.value; renderAll(); });
    }

    $("rep1").addEventListener("click", () => { state.empirical.push(simulateBox(state.mode, state.draws, state.tickets)); renderCLTHist(); });
    $("rep10").addEventListener("click", () => { for (let i = 0; i < 10; i++) state.empirical.push(simulateBox(state.mode, state.draws, state.tickets)); renderCLTHist(); });
    $("rep25").addEventListener("click", () => { for (let i = 0; i < 25; i++) state.empirical.push(simulateBox(state.mode, state.draws, state.tickets)); renderCLTHist(); });
    $("rep100").addEventListener("click", () => { for (let i = 0; i < 100; i++) state.empirical.push(simulateBox(state.mode, state.draws, state.tickets)); renderCLTHist(); });
    $("reset-clt").addEventListener("click", () => { state.empirical = []; renderCLTHist(); });

    for (const id of ["lower-num", "upper-num"]) $(id).addEventListener("input", renderProbability);
    for (const id of ["lower-inf", "upper-inf"]) $(id).addEventListener("change", renderProbability);

    renderAll();
  });

})();
