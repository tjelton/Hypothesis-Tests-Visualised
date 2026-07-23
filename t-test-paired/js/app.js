// app.js -- interactive wiring for the paired t-test lesson. Ports the
// reactive logic of R/ttest_paired_srv.R and its data loader
// (utility_load_data_paired_sample_srv.R). Everything downstream of the data
// loader is the 1-sample t-test chain applied to the PAIRED DIFFERENCE
// (condition 2 - condition 1), which is exactly how the Shiny app reuses its
// generic ttest_1_sample_generic_* modules here. The only display differences
// from the 1-sample lesson are the "d" subscript on mu (\mu_d) in the
// hypotheses and expected value, and a default null value of 0.
//
// Calculation parity (identical round-then-parse chain as the Shiny modules):
//   null_mean_string = as.character(round(null_mu, 3))
//   EV_string = as.character(round(as.numeric(null_mean_string), 5))
//   SE_string = as.character(round(sd(diff)/sqrt(n), 5))
//   TS = as.character(round((mean(diff) - as.numeric(EV_string)) / as.numeric(SE_string), 4))
//   p-value computed from as.numeric(TS), df = n - 1
//
// Deliberate deviation (an R-side quirk, matching the 1-sample port): manually
// entered values that fail to parse are dropped per condition before the
// equal-length check, exactly as the R module does with as.numeric()/is.na().

"use strict";

(function () {

  const state = {
    dataChoice: null,          // null | "pre_uploaded" | "manually_specified"
    dataset: "BloodPressureStudy",
    condition1: null,          // column name for condition 1
    condition2: null,          // column name for condition 2
    dataCond1: null,           // array of numbers (condition 1)
    dataCond2: null,           // array of numbers (condition 2)
    data: null,                // paired difference (cond2 - cond1), or null
    plotChoice: "Paired_Difference",
    nullMu: 0,
    altChoice: 1,              // 1 two-sided, 2 greater, 3 less
    alpha: 0.05, alphaWarn: false,
    conf: 0.95, confWarn: false,
    tsStr: "", pVal: 0
  };

  const $ = id => document.getElementById(id);

  function typeset(el) {
    if (window.MathJax && window.MathJax.typesetPromise) {
      window.MathJax.typesetPromise(el ? [el] : undefined).catch(() => {});
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
  function getCol(dsName, colName) {
    return columns(dsName).find(c => c.name === colName);
  }

  // ---------- data source UI flow ----------
  function onDataChoiceChange(choice) {
    state.dataChoice = choice;
    $("pre-uploaded-block").classList.toggle("d-none", choice !== "pre_uploaded");
    $("manual-block").classList.toggle("d-none", choice !== "manually_specified");

    if (choice === "pre_uploaded") {
      // Shiny re-renders the selectInput on each switch, resetting to the first
      // dataset; the observers then repopulate the data immediately.
      state.dataset = "BloodPressureStudy";
      setOptions($("dataset-select"), Object.keys(DATASETS), state.dataset);
      onDatasetChange();
    } else {
      // Manual mode: textareas start empty; no data until 'Upload' is pressed.
      $("manual-textarea-1").value = "";
      $("manual-textarea-2").value = "";
      $("manual-unequal-warning").classList.add("d-none");
      $("manual-unique-warning").classList.add("d-none");
      clearData();
    }
  }

  function onDatasetChange() {
    state.dataset = $("dataset-select").value;
    const numCols = numericCols(state.dataset);
    // Defaults mirror the R module: condition 1 = first numeric column,
    // condition 2 = second.
    state.condition1 = numCols[0];
    state.condition2 = numCols.length > 1 ? numCols[1] : numCols[0];
    setOptions($("condition-1-select"), numCols, state.condition1);
    setOptions($("condition-2-select"), numCols, state.condition2);
    recomputePreUploaded();
  }

  function onConditionChange() {
    state.condition1 = $("condition-1-select").value;
    state.condition2 = $("condition-2-select").value;
    recomputePreUploaded();
  }

  // Build the paired difference from the two selected columns, or show the
  // "conditions must differ" warning and clear the data.
  function recomputePreUploaded() {
    if (state.condition1 === state.condition2) {
      $("condition-same-warning").classList.remove("d-none");
      clearData();
      return;
    }
    $("condition-same-warning").classList.add("d-none");
    const c1 = getCol(state.dataset, state.condition1).values.slice();
    const c2 = getCol(state.dataset, state.condition2).values.slice();
    applyData(c1, c2);
  }

  function onManualUpload() {
    const t1 = $("manual-textarea-1").value;
    const t2 = $("manual-textarea-2").value;
    if (!t1 || !t2) return;

    // Reset warnings.
    $("manual-unequal-warning").classList.add("d-none");
    $("manual-unique-warning").classList.add("d-none");

    // Parse each condition independently: split on commas/newlines, trim, drop
    // empties, coerce to number, drop non-numeric (as the R module does).
    const parse = txt => txt.split(/[,\n]/).map(s => s.trim()).filter(s => s !== "")
      .map(s => Number(s)).filter(v => !Number.isNaN(v));
    const c1 = parse(t1);
    const c2 = parse(t2);

    // Paired data must have equal lengths after dropping non-numeric values.
    if (c1.length !== c2.length) {
      $("manual-unequal-warning-text").textContent =
        "Warning: The number of values in each condition is unequal. Condition 1 has " +
        c1.length + " values, and condition 2 has " + c2.length + " values.";
      $("manual-unequal-warning").classList.remove("d-none");
      clearData();
      return;
    }

    // The paired difference must have at least two unique values, otherwise
    // sd = 0 and the test breaks.
    const diff = c2.map((v, i) => v - c1[i]);
    if (new Set(diff).size <= 1) {
      $("manual-unique-warning").classList.remove("d-none");
      clearData();
      return;
    }

    applyData(c1, c2);
  }

  // ---------- data application / rendering ----------
  // Store the two conditions and their paired difference, then render.
  function applyData(cond1, cond2) {
    state.dataCond1 = cond1;
    state.dataCond2 = cond2;
    const diff = cond2.map((v, i) => v - cond1[i]);
    state.data = diff.length > 0 ? diff : null;
    afterDataChange();
  }

  function clearData() {
    state.dataCond1 = null;
    state.dataCond2 = null;
    state.data = null;
    afterDataChange();
  }

  function afterDataChange() {
    renderPreview();
    $("plot-choice-block").classList.toggle("d-none", state.data === null);
    $("rest-of-exercise").classList.toggle("d-none", state.data === null);
    if (state.data !== null) {
      renderAssumptionPlots();
      renderStats();
    }
  }

  // The series the preview plots depends on the radio choice (difference by
  // default), matching the R module's "data_to_plot" toggle.
  function currentPlotSeries() {
    if (state.plotChoice === "Condition_1") return state.dataCond1;
    if (state.plotChoice === "Condition_2") return state.dataCond2;
    return state.data; // Paired_Difference
  }

  function renderPreview() {
    const el = $("data-preview");
    if (state.data === null) {
      el.innerHTML = '<span style="color: blue;"><p>In order to proceed, you must select some data to act as your sample.</p></span>';
      return;
    }
    const series = currentPlotSeries();
    el.innerHTML =
      Plots.boxplotSVG(series, { width: 520, height: 220, main: "Boxplot", ylab: "Values", col: "blue", horizontal: true }) +
      Plots.histogramSVG(series, { width: 520, height: 220, main: "Histogram", xlab: "Values", ylab: "Frequency", col: "blue", breaks: 30 });
  }

  // Assumption plots always describe the paired difference (the actual sample).
  function renderAssumptionPlots() {
    $("qq-plot").innerHTML = Plots.qqPlotSVG(state.data, { width: 400, height: 400 });
    $("assump-boxplot").innerHTML = Plots.boxplotSVG(state.data, { width: 400, height: 400, main: "Boxplot of Sample Data" });
    $("assump-hist").innerHTML = Plots.histogramSVG(state.data, { width: 400, height: 400, main: "Histogram of Sample Data", xlab: "", ylab: "Frequency", breaks: 30 });
  }

  // Recomputes every stats-driven section, honouring the same round-then-parse
  // chain as the Shiny modules. `data` is the paired difference.
  function renderStats() {
    const data = state.data;
    const n = data.length;
    const xbar = Stats.mean(data);
    const s = Stats.sd(data);
    const df = n - 1;

    const nullMuStr = Stats.roundStr(state.nullMu, 3);   // null_mean_string()

    // --- box model (uses plain mu, matching the R box_model_html call) ---
    $("box-model").innerHTML = boxModelHTML(
      "&mu; = " + nullMuStr + "; s = " + Stats.roundStr(s, 3),
      "OV = " + Stats.roundStr(xbar, 3),
      "n = " + n
    );

    // --- hypotheses (mu carries the "d" subscript for the paired test) ---
    $("null-hyp-out").innerHTML =
      "<center><p style='font-size: 16px;'>\\( H_0: \\) \\( \\mu_d = " + nullMuStr + " \\)</p></center>";
    const altSign = state.altChoice === 1 ? "\\neq" : (state.altChoice === 2 ? ">" : "<");
    $("alt-hyp-out").innerHTML =
      "<center><p style='font-size: 16px;'>\\( H_1: \\) \\(\\mu_d " + altSign + " " + nullMuStr + " \\)</p></center>";

    // --- test statistic ---
    const EV = Number(nullMuStr);
    const EVStr = Stats.roundStr(EV, 5);
    const SE = s / Math.sqrt(n);
    const SEStr = Stats.roundStr(SE, 5);

    $("ev-se-out").innerHTML =
      "<p>Expected Value:</p>" +
      "$$\\begin{align*} \\text{EV} &= \\mu_d\\\\ &=" + EVStr + "\\end{align*}$$" +
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

    let thirdString = "<p>The test statistics fall on a standard normal curve. ";
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
      "<p>The p-value is the probability of observing a test-statistic <b>more extreme that our test statistic of " + state.tsStr + ".</b></p>" +
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
    if (p > alpha) {
      mathLine = "$$\\begin{align*} \\alpha &< p \\\\" + Stats.formatR(alpha) + " &< " + Stats.roundStr(p, 4) + "\\end{align*}$$";
      conclusionLine = "<span style='color: blue;'><p>As the p value is greater than our significance level, we <b>accept the null hypothesis</b>.</p></span>";
    } else {
      mathLine = "$$\\begin{align*} \\alpha &> p \\\\" + Stats.formatR(alpha) + " &> " + Stats.roundStr(p, 4) + "\\end{align*}$$";
      conclusionLine = "<span style='color: blue;'><p>As the p value is less than our significance level, we <b>reject the null hypothesis</b>.</p></span>";
    }
    $("conclusion-out").innerHTML = mathLine + conclusionLine;
  }

  function renderCI(xbar, se, df, mu0) {
    const conf = state.conf;
    const alpha = 1 - conf;
    let formulaLine, substitutionLine, answerLine, conclusionText;

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
      "<span style='color: blue;'><p>" + conclusionText + "</p></span>";
  }

  // ---------- event bindings ----------
  document.addEventListener("DOMContentLoaded", function () {

    // Static example box model in the intro modal (matches the R modal verbatim,
    // including the s = 4.751 value carried over from the 1-sample example).
    $("intro-example-box-model").innerHTML = boxModelHTML(
      "&mu; = 0; s = 4.751",
      "OV = -4.373",
      "n = 30"
    );

    for (const radio of document.querySelectorAll('input[name="data_upload_choice"]')) {
      radio.addEventListener("change", e => onDataChoiceChange(e.target.value));
    }
    $("dataset-select").addEventListener("change", onDatasetChange);
    $("condition-1-select").addEventListener("change", onConditionChange);
    $("condition-2-select").addEventListener("change", onConditionChange);
    $("upload-btn").addEventListener("click", onManualUpload);

    for (const radio of document.querySelectorAll('input[name="data_to_plot"]')) {
      radio.addEventListener("change", function () {
        state.plotChoice = this.value;
        if (state.data !== null) renderPreview();
      });
    }

    $("null-mu").addEventListener("input", function () {
      state.nullMu = this.value === "" ? NaN : Number(this.value);
      if (state.data !== null) renderStats();
    });

    for (const radio of document.querySelectorAll('input[name="alternate_hypothesis_choice"]')) {
      radio.addEventListener("change", function () {
        state.altChoice = Number(this.value);
        if (state.data !== null) renderStats();
      });
    }

    $("alpha-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v < 0 || v > 1) {
        state.alpha = 0.05;
        state.alphaWarn = true;
      } else {
        state.alpha = v;
        state.alphaWarn = false;
      }
      $("alpha-warning").classList.toggle("d-none", !state.alphaWarn);
      if (state.data !== null) { renderConclusion(); typeset($("conclusion-out")); }
    });

    $("conf-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v <= 0 || v >= 1) {
        state.conf = 0.95;
        state.confWarn = true;
      } else {
        state.conf = v;
        state.confWarn = false;
      }
      $("conf-warning").classList.toggle("d-none", !state.confWarn);
      if (state.data !== null) {
        const data = state.data;
        const n = data.length;
        renderCI(Stats.mean(data), Stats.sd(data) / Math.sqrt(n), n - 1,
                 Number(Stats.roundStr(state.nullMu, 3)));
        typeset($("ci-out"));
      }
    });
  });

})();
