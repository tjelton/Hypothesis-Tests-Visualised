// app.js -- interactive wiring for the 1-sample t-test lesson. Ports the
// reactive logic of R/ttest_1_sample_srv.R and its sub-modules
// (utility_load_data_1_sample_srv.R, ttest_1_sample_generic_*.R).
//
// Calculation parity notes (matching the Shiny code's rounding chain):
//   null_mean_string = as.character(round(null_mu, 3))
//   EV_string = as.character(round(as.numeric(null_mean_string), 5))
//   SE_string = as.character(round(sd/sqrt(n), 5))
//   TS = as.character(round((mean - as.numeric(EV_string)) / as.numeric(SE_string), 4))
//   p-value computed from as.numeric(TS), df = n - 1
//
// Notes on behaviour that is easy to undo by accident:
//   * Manually entered values that fail to parse are dropped, with a warning
//     naming how many were unusable.
//   * The confidence level is DERIVED as 1 - alpha rather than being a second
//     free input, so the CI and p-value conclusions cannot disagree. This
//     matches the 1-sample z-test lesson.
//   * The box is labelled "sigma ~= s": sigma is unknown and s stands in for
//     it. Do not write "sigma = s" -- that asserts the very thing this lesson
//     exists to correct.

"use strict";

(function () {

  const state = {
    dataChoice: null,          // null | "pre_uploaded" | "manually_specified"
    dataset: "Mr. Han's Exam Marks",
    column: null,
    factorCol: "(None)",
    factorLevel: null,
    data: null,                // array of numbers, or null until specified
    manualNAs: 0,
    manualUniqueErr: false,
    nullMu: 140,
    altChoice: 1,              // 1 two-sided, 2 greater, 3 less
    alpha: 0.05, alphaWarn: false,
    // strings produced by the test-statistic section, consumed downstream
    tsStr: "", pVal: 0
  };

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

  function setOptions(select, options, selected) {
    select.innerHTML = "";
    for (const o of options) {
      const opt = document.createElement("option");
      opt.value = o;
      opt.textContent = o;
      if (o === selected) opt.selected = true;
      select.appendChild(opt);
    }
  }

  // ---------- dataset helpers ----------
  function columns(dsName) { return DATASETS[dsName].columns; }
  function numericCols(dsName) {
    return columns(dsName).filter(c => c.type === "numeric").map(c => c.name);
  }
  function factorCols(dsName) {
    return columns(dsName).filter(c => c.type === "factor").map(c => c.name);
  }
  function getCol(dsName, colName) {
    return columns(dsName).find(c => c.name === colName);
  }

  // Current data under dataset/column/factor-filter selections.
  function computePreUploadedData() {
    const col = getCol(state.dataset, state.column);
    if (!col) return null;
    let values = col.values;
    if (state.factorCol !== "(None)" && state.factorLevel !== null) {
      const fcol = getCol(state.dataset, state.factorCol);
      values = values.filter((_, i) => fcol.values[i] === state.factorLevel);
    }
    return values.slice();
  }

  // ---------- data source UI flow ----------
  function onDataChoiceChange(choice) {
    state.dataChoice = choice;
    $("pre-uploaded-block").classList.toggle("d-none", choice !== "pre_uploaded");
    $("manual-block").classList.toggle("d-none", choice !== "manually_specified");

    if (choice === "pre_uploaded") {
      // Shiny re-renders the selectInput each switch, resetting to the first
      // dataset; the observers then repopulate the data immediately.
      state.dataset = "Mr. Han's Exam Marks";
      setOptions($("dataset-select"), Object.keys(DATASETS), state.dataset);
      onDatasetChange();
    } else {
      // Manual mode: like the Shiny app, the textarea is recreated empty and
      // no data exists until 'Upload' is pressed.
      $("manual-textarea").value = "";
      $("manual-unique-warning").classList.add("d-none");
      $("manual-missing-warning").classList.add("d-none");
      setData(null);
    }
  }

  function onDatasetChange() {
    state.dataset = $("dataset-select").value;
    const numCols = numericCols(state.dataset);
    state.column = numCols[0];
    setOptions($("column-select"), numCols, state.column);

    const fCols = factorCols(state.dataset);
    if (fCols.length > 0) {
      state.factorCol = "(None)";
      state.factorLevel = null;
      setOptions($("factor-select"), ["(None)"].concat(fCols), "(None)");
      $("factor-block").classList.remove("d-none");
    } else {
      state.factorCol = "(None)";
      state.factorLevel = null;
      $("factor-block").classList.add("d-none");
    }
    $("category-block").classList.add("d-none");
    setData(computePreUploadedData());
  }

  function onColumnChange() {
    state.column = $("column-select").value;
    setData(computePreUploadedData());
  }

  function onFactorChange() {
    state.factorCol = $("factor-select").value;
    if (state.factorCol === "(None)") {
      state.factorLevel = null;
      $("category-block").classList.add("d-none");
    } else {
      const levels = getCol(state.dataset, state.factorCol).levels;
      state.factorLevel = levels[0];
      setOptions($("category-select"), levels, state.factorLevel);
      $("category-block").classList.remove("d-none");
    }
    setData(computePreUploadedData());
  }

  function onCategoryChange() {
    state.factorLevel = $("category-select").value;
    setData(computePreUploadedData());
  }

  function onManualUpload() {
    const text = $("manual-textarea").value;
    if (!text) return;

    // Split at commas and newlines, trim, drop empties (as in the R module).
    const splits = text.split(/[,\n]/).map(s => s.trim()).filter(s => s !== "");
    const numeric = splits.map(s => (s === "" || isNaN(Number(s))) ? NaN : Number(s));
    const valid = numeric.filter(v => !Number.isNaN(v));

    // At least two unique interpretable values, otherwise sd = 0/NA and the
    // test breaks.
    const uniques = new Set(valid);
    if (uniques.size <= 1) {
      state.manualNAs = 0;
      state.manualUniqueErr = true;
      $("manual-unique-warning").classList.remove("d-none");
      $("manual-missing-warning").classList.add("d-none");
      setData(null);
      return;
    }
    state.manualUniqueErr = false;
    $("manual-unique-warning").classList.add("d-none");

    state.manualNAs = numeric.length - valid.length;
    if (state.manualNAs > 0) {
      $("manual-missing-warning-text").innerHTML =
        "Warning: From the data that you uploaded, " + state.manualNAs +
        " of the values could not be interpreted. This could be because these values were not numeric, " +
        "or because you did not specify the data into the required format.";
      $("manual-missing-warning").classList.remove("d-none");
    } else {
      $("manual-missing-warning").classList.add("d-none");
    }

    setData(valid);
  }

  // ---------- rendering ----------
  function setData(values) {
    state.data = (values && values.length > 0) ? values : null;
    renderPreview();
    $("rest-of-exercise").classList.toggle("d-none", state.data === null);
    if (state.data !== null) {
      renderAssumptionPlots();
      renderStats();
    }
  }

  function renderPreview() {
    const el = $("data-preview");
    if (state.data === null) {
      el.innerHTML = '<span style="color: blue;"><p>In order to proceed, you must select some data to act as your sample.</p></span>';
      return;
    }
    el.innerHTML =
      Plots.boxplotSVG(state.data, { width: 520, height: 220, main: "Boxplot", ylab: "Values", col: "blue", horizontal: true }) +
      Plots.histogramSVG(state.data, { width: 520, height: 220, main: "Histogram", xlab: "Values", ylab: "Frequency", col: "blue", breaks: 30 });
  }

  function renderAssumptionPlots() {
    $("qq-plot").innerHTML = Plots.qqPlotSVG(state.data, { width: 400, height: 400 });
    $("assump-boxplot").innerHTML = Plots.boxplotSVG(state.data, { width: 400, height: 400, main: "Boxplot of Sample Data" });
    $("assump-hist").innerHTML = Plots.histogramSVG(state.data, { width: 400, height: 400, main: "Histogram of Sample Data", xlab: "", ylab: "Frequency", breaks: 30 });
  }

  // Recomputes every stats-driven section, honouring the same
  // round-then-parse chain as the Shiny modules.
  function renderStats() {
    const data = state.data;
    const n = data.length;
    const xbar = Stats.mean(data);
    const s = Stats.sd(data);
    const df = n - 1;

    const nullMuStr = Stats.roundStr(state.nullMu, 3);   // null_mean_string()

    // --- box model ---
    // "sigma ~= s": the box's true spread is unknown; s stands in for it.
    $("box-model").innerHTML = boxModelHTML(
      "&mu; = " + nullMuStr + "; &sigma; &asymp; s = " + Stats.roundStr(s, 3),
      "OV = " + Stats.roundStr(xbar, 3),
      "n = " + n
    );

    // --- hypotheses ---
    $("null-hyp-out").innerHTML =
      "<center><p style='font-size: 16px;'>\\( H_0: \\) \\( \\mu = " + nullMuStr + " \\)</p></center>";
    const altSign = state.altChoice === 1 ? "\\neq" : (state.altChoice === 2 ? ">" : "<");
    $("alt-hyp-out").innerHTML =
      "<center><p style='font-size: 16px;'>\\( H_1: \\) \\(\\mu " + altSign + " " + nullMuStr + " \\)</p></center>";

    // --- test statistic ---
    const EV = Number(nullMuStr);
    const EVStr = Stats.roundStr(EV, 5);
    const SE = s / Math.sqrt(n);
    const SEStr = Stats.roundStr(SE, 5);

    $("ev-se-out").innerHTML =
      "<p>Expected Value:</p>" +
      "$$\\begin{align*} \\text{EV} &= \\mu\\\\ &=" + EVStr + "\\end{align*}$$" +
      "<p>Standard Error:</p>" +
      "$$\\begin{align*} \\text{SE} &= \\frac{s}{\\sqrt{n}} \\\\ &= \\frac{" + Stats.roundStr(s, 5) +
        "}{\\sqrt{" + n + "}}\\\\ &= " + SEStr + "\\end{align*}$$" +
      "(Note that we use the sample standard deviation [\\(s \\)] rather than the population standard deviation [\\(\\sigma \\)])";

    const tsNum = (xbar - Number(EVStr)) / Number(SEStr);
    state.tsStr = Stats.roundStr(tsNum, 4);
    $("ts-out").innerHTML =
      "$$\\begin{align*} \\text{TS} &= \\frac{\\text{OV} - \\text{EV}}{\\text{SE}} \\\\ &= \\frac{" +
        Stats.roundStr(xbar, 5) + " - " + EVStr + "}{" + SEStr + "} \\\\ &= " + state.tsStr + "\\end{align*}$$" +
      "<p style='text-align: left;'><span style='color: blue;'><i>The value for the test-statistic is " + state.tsStr + ". </i></span></p>";

    // --- p-value ---
    const ts = Number(state.tsStr);
    let pVal = 0;
    if (state.altChoice === 1) pVal = 2 * (1 - Stats.pt(Math.abs(ts), df));
    else if (state.altChoice === 2) pVal = 1 - Stats.pt(ts, df);
    else pVal = Stats.pt(ts, df);
    state.pVal = pVal;

    let thirdString = "<p>Our test statistic falls on that t-curve. ";
    if (state.altChoice === 1) {
      thirdString += "As we are doing a two-sided alternate hypothesis, we are interested in finding the <b>area below " +
        Stats.formatR(-Math.abs(ts)) + " and above " + Stats.formatR(Math.abs(ts)) + ".</p></b>";
    } else if (state.altChoice === 2) {
      thirdString += "As we are doing a one-sided greater than alternate hypothesis, we are interested in finding the <b>area above " +
        state.tsStr + ".</p></b>";
    } else {
      thirdString += "As we are doing a one-sided less than alternate hypothesis, we are interested in finding the <b>area below " +
        state.tsStr + ".</p></b>";
    }

    $("p-value-prelude").innerHTML =
      "<p>The p-value is the probability of observing a test-statistic <b>more extreme than our test statistic of " + state.tsStr +
        "</b>, <b>assuming the null hypothesis is true</b>. That last part is essential: the p-value is calculated in a world where \\( \\mu = " + nullMuStr +
        " \\), and it measures how unusual our sample would be in that world. It is not the probability that the null hypothesis is true.</p>" +
      "<p>Unlike in a z-test where the test statistics fall on a standard normal curve, in a t-test, the test statistics fall on a t-curve/distribution. " +
      "If you recall from the \"T-Curve Motivation\" exercise, to specify a t-distribution, you need to specify the degree of freedom, which adjusts " +
      "the 'fatness' of the t-curve's tails.</p>" +
      "<p><b>For a 1-sample t-test, we set the degrees of freedom equal to the sample size - 1 (that is, \\(n - 1\\)).</b></p>" +
      "<p>In this case, the degree of freedom is equal to \\(" + n + " - 1 = " + df + "\\).</p>" +
      thirdString +
      "<p style='font-size: 16px; text-align: center;'>\\( p = " + Stats.roundStr(pVal, 5) + " \\)</p>";

    $("t-curve-plot").innerHTML = Plots.shadedTCurveSVG(df, ts, state.altChoice, { width: 560, height: 325 });

    renderConclusion();
    renderCI(xbar, SE, df, Number(nullMuStr));
    typeset($("rest-of-exercise"));
  }

  function renderConclusion() {
    const p = state.pVal;
    const alpha = state.alpha;
    let mathLine, conclusionLine;
    // Three displays but two verdicts: p exactly equal to alpha rejects (the
    // rejection region is p <= alpha), and must not be shown as "alpha > p".
    if (p > alpha) {
      mathLine = "$$\\begin{align*} \\alpha &< p \\\\" + Stats.formatR(alpha) + " &< " + Stats.roundStr(p, 4) + "\\end{align*}$$";
      conclusionLine = "<span style='color: blue;'><p>As the p value is greater than our significance level, we <b>fail to reject the null hypothesis</b>.</p></span>";
    } else {
      const sign = p === alpha ? "=" : ">";
      mathLine = "$$\\begin{align*} \\alpha &" + sign + " p \\\\" + Stats.formatR(alpha) + " &" + sign + " " + Stats.roundStr(p, 4) + "\\end{align*}$$";
      conclusionLine = "<span style='color: blue;'><p>As the p value is " + (p === alpha ? "equal to" : "less than") +
        " our significance level, we <b>reject the null hypothesis</b>.</p></span>";
    }
    $("conclusion-out").innerHTML = mathLine + conclusionLine;
  }

  // Level derived from alpha, with the interval's sidedness matched to the
  // alternate hypothesis -- together these make this agree with the p-value.
  function renderCI(xbar, se, df, mu0) {
    const alpha = state.alpha;
    const conf = 1 - alpha;
    let formulaLine, substitutionLine, answerLine, conclusionText;

    $("conf-level-out").innerHTML =
      "<p style='font-size: 16px; text-align: center;'>\\( \\text{confidence level} = 1 - " + Stats.formatR(alpha) +
        " = " + Stats.formatR(conf) + " \\)</p>";

    if (state.altChoice === 1) {
      const tVal = Stats.qt(1 - alpha / 2, df);
      const lower = xbar - tVal * se;
      const upper = xbar + tVal * se;
      formulaLine = "$$CI = \\bar{x} \\pm t_{\\alpha/2, df} \\cdot SE$$";
      substitutionLine = "$$CI = " + Stats.roundStr(xbar, 4) +
        " \\pm t_{" + Stats.roundStr(alpha / 2, 4) + "," + df + "} \\times " + Stats.roundStr(se, 4) + "$$";
      answerLine = "$$CI = (" + Stats.roundStr(lower, 4) + ", " + Stats.roundStr(upper, 4) + ")$$";
      conclusionText = (mu0 < lower || mu0 > upper)
        ? "As the null hypothesis value is outside the confidence interval, we <b>reject the null hypothesis</b>."
        : "As the null hypothesis value is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
    } else if (state.altChoice === 2) {
      const tVal = Stats.qt(1 - alpha, df);
      const lower = xbar - tVal * se;
      formulaLine = "$$CI = (\\bar{x} - t_{\\alpha, df} \\cdot SE, \\infty)$$";
      substitutionLine = "$$CI = (" + Stats.roundStr(xbar, 4) +
        " - t_{" + Stats.roundStr(alpha, 4) + "," + df + "} \\times " + Stats.roundStr(se, 4) + ", \\infty)$$";
      answerLine = "$$CI = (" + Stats.roundStr(lower, 4) + ", \\infty)$$";
      conclusionText = (mu0 < lower)
        ? "As the null hypothesis value is below the confidence interval, we <b>reject the null hypothesis</b>."
        : "As the null hypothesis value is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
    } else {
      const tVal = Stats.qt(1 - alpha, df);
      const upper = xbar + tVal * se;
      formulaLine = "$$CI = (-\\infty, \\bar{x} + t_{\\alpha, df} \\cdot SE)$$";
      substitutionLine = "$$CI = (-\\infty, " + Stats.roundStr(xbar, 4) +
        " + t_{" + Stats.roundStr(alpha, 4) + "," + df + "} \\times " + Stats.roundStr(se, 4) + ")$$";
      answerLine = "$$CI = (-\\infty, " + Stats.roundStr(upper, 4) + ")$$";
      conclusionText = (mu0 > upper)
        ? "As the null hypothesis value is above the confidence interval, we <b>reject the null hypothesis</b>."
        : "As the null hypothesis value is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
    }

    $("ci-out").innerHTML = formulaLine + substitutionLine + answerLine +
      "<span style='color: blue;'><p>" + conclusionText + "</p></span>" +
      "<p><i>This is the same verdict the p-value reached above, as it must be — the interval is built from the same observed mean and the same standard error, " +
        "at the matching level of " + Stats.formatR(conf) + ".</i></p>";
  }

  // ---------- event bindings ----------
  document.addEventListener("DOMContentLoaded", function () {

    // Static example box model in the intro modal (matches the R modal).
    $("intro-example-box-model").innerHTML = boxModelHTML(
      "&mu; = 140; &sigma; &asymp; s = 4.751",
      "OV = 142.843",
      "n = 25"
    );

    for (const radio of document.querySelectorAll('input[name="data_upload_choice"]')) {
      radio.addEventListener("change", e => onDataChoiceChange(e.target.value));
    }
    $("dataset-select").addEventListener("change", onDatasetChange);
    $("column-select").addEventListener("change", onColumnChange);
    $("factor-select").addEventListener("change", onFactorChange);
    $("category-select").addEventListener("change", onCategoryChange);
    $("upload-btn").addEventListener("click", onManualUpload);

    // An empty or non-numeric box leaves the last valid value in place, rather
    // than propagating NaN through every displayed quantity.
    $("null-mu").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (!Number.isFinite(v)) return;
      state.nullMu = v;
      if (state.data !== null) renderStats();
    });

    for (const radio of document.querySelectorAll('input[name="alternate_hypothesis_choice"]')) {
      radio.addEventListener("change", function () {
        state.altChoice = Number(this.value);
        if (state.data !== null) renderStats();
      });
    }

    // alpha drives both conclusions: the p-value comparison and, via the
    // derived 1 - alpha confidence level, the interval.
    $("alpha-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (!Number.isFinite(v) || v <= 0 || v >= 1) {
        state.alpha = 0.05;
        state.alphaWarn = true;
      } else {
        state.alpha = v;
        state.alphaWarn = false;
      }
      $("alpha-warning").classList.toggle("d-none", !state.alphaWarn);
      if (state.data !== null) {
        const data = state.data;
        const n = data.length;
        renderConclusion();
        renderCI(Stats.mean(data), Stats.sd(data) / Math.sqrt(n), n - 1,
                 Number(Stats.roundStr(state.nullMu, 3)));
        typeset($("conclusion-out"));
        typeset($("ci-out"));
        typeset($("conf-level-out"));
      }
    });
  });

})();
