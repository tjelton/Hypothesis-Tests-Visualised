// app.js -- interactive wiring for the 2-sample t-test lesson. Ports
// R/ttest_2_sample_srv.R and its data loader (utility_load_data_2_sample_srv.R).
//
// The test compares two independent samples. The "Same Spread" toggle selects
// between a pooled-variance 2-sample t-test and a Welch t-test, which changes
// the standard error AND the degrees of freedom:
//   pooled:  SE = sp * sqrt(1/n1 + 1/n2),  sp = sqrt(((n1-1)s1^2+(n2-1)s2^2)/(n1+n2-2)),  df = n1+n2-2
//   Welch:   SE = sqrt(s1^2/n1 + s2^2/n2),  df = Welch-Satterthwaite formula
//
// Calculation parity notes (matching the Shiny code):
//   * observed value = mean1 - mean2; test statistic = round(observed/SE, 4)
//     using the FULL-PRECISION SE (unlike the 1-sample lesson, which first
//     rounds EV/SE to strings). df is kept full-precision.
//   * the p-value is computed from the rounded test-statistic string and the
//     full-precision df.
//   * the CI recomputes SE/df from var() = sd()^2; these equal the values above.
//
// Reproduced R quirks (documented so both versions can be fixed together):
//   * the p-value t-curve plot is drawn with df - 1 (the R code passes
//     `df = df() - 1` to curve_shaded_test_stat, while the p-value itself uses
//     the full df). Visual only.
//   * the intro modal's Sample 2 box mislabels its stats with subscript 1
//     (s1/OV1/n1) -- a copy-paste from the Sample 1 box in the R source.

"use strict";

(function () {

  const state = {
    dataChoice: null,
    dataset: "blood_pressure",
    categorical: null,
    sample1: null, sample2: null,
    depvar: null,
    x1: null, x2: null,          // the two samples (arrays), or null
    plotType: "Box_plot",
    alt: 1,                       // 1 two-sided, 2 greater, 3 less
    spread: true,                 // true = equal variance, false = Welch
    alpha: 0.05, alphaWarn: false,
    conf: 0.95, confWarn: false,
    tsStr: "", pVal: 0
  };

  const $ = id => document.getElementById(id);
  const S = Stats;

  function typeset(el) {
    if (window.MathJax && window.MathJax.typesetPromise) {
      window.MathJax.typesetPromise(el ? [el] : undefined).catch(() => {});
    }
  }

  function setOptions(select, options, selected) {
    select.innerHTML = "";
    for (const o of options) {
      const opt = document.createElement("option");
      opt.value = o; opt.textContent = o;
      if (o === selected) opt.selected = true;
      select.appendChild(opt);
    }
  }

  // ---------- dataset helpers ----------
  function columns(ds) { return DATASETS[ds].columns; }
  function numericCols(ds) { return columns(ds).filter(c => c.type === "numeric").map(c => c.name); }
  function categoricalCols(ds) { return columns(ds).filter(c => c.type === "categorical").map(c => c.name); }
  function getCol(ds, name) { return columns(ds).find(c => c.name === name); }
  // Unique values in first-appearance order (R's unique()).
  function uniqueInOrder(arr) {
    const seen = new Set(), out = [];
    for (const v of arr) { if (!seen.has(v)) { seen.add(v); out.push(v); } }
    return out;
  }

  // ---------- data source UI flow ----------
  function onDataChoiceChange(choice) {
    state.dataChoice = choice;
    $("pre-uploaded-block").classList.toggle("d-none", choice !== "pre_uploaded");
    $("manual-block").classList.toggle("d-none", choice !== "manually_specified");
    if (choice === "pre_uploaded") {
      state.dataset = "blood_pressure";
      setOptions($("dataset-select"), Object.keys(DATASETS), state.dataset);
      onDatasetChange();
    } else {
      $("manual-textarea-1").value = "";
      $("manual-textarea-2").value = "";
      $("manual-unique-warning").classList.add("d-none");
      clearData();
    }
  }

  function onDatasetChange() {
    state.dataset = $("dataset-select").value;
    const catCols = categoricalCols(state.dataset);
    state.categorical = catCols[0];
    setOptions($("categorical-select"), catCols, state.categorical);
    populateSampleSelectors();
    const numCols = numericCols(state.dataset);
    state.depvar = numCols[0];
    setOptions($("depvar-select"), numCols, state.depvar);
    recomputePreUploaded();
  }

  function populateSampleSelectors() {
    const levels = uniqueInOrder(getCol(state.dataset, state.categorical).values);
    state.sample1 = levels[0];
    state.sample2 = levels.length > 1 ? levels[1] : levels[0];
    setOptions($("sample-1-select"), levels, state.sample1);
    setOptions($("sample-2-select"), levels, state.sample2);
  }

  function onCategoricalChange() {
    state.categorical = $("categorical-select").value;
    populateSampleSelectors();
    recomputePreUploaded();
  }

  function onSampleOrDepvarChange() {
    state.sample1 = $("sample-1-select").value;
    state.sample2 = $("sample-2-select").value;
    state.depvar = $("depvar-select").value;
    recomputePreUploaded();
  }

  // Split the dependent variable by the categorical column into two samples.
  function recomputePreUploaded() {
    if (state.sample1 === state.sample2) {
      $("sample-same-warning").classList.remove("d-none");
      clearData();
      return;
    }
    $("sample-same-warning").classList.add("d-none");
    const cat = getCol(state.dataset, state.categorical).values;
    const dep = getCol(state.dataset, state.depvar).values;
    const x1 = [], x2 = [];
    for (let i = 0; i < cat.length; i++) {
      if (cat[i] === state.sample1) x1.push(dep[i]);
      else if (cat[i] === state.sample2) x2.push(dep[i]);
    }
    applyData(x1, x2);
  }

  function onManualUpload() {
    const t1 = $("manual-textarea-1").value, t2 = $("manual-textarea-2").value;
    if (!t1 || !t2) return;
    $("manual-unique-warning").classList.add("d-none");
    const parse = txt => txt.split(/[,\n]/).map(s => s.trim()).filter(s => s !== "")
      .map(s => Number(s)).filter(v => !Number.isNaN(v));
    const x1 = parse(t1), x2 = parse(t2);
    // Each sample needs at least two unique values (else sd = 0 breaks the test).
    if (new Set(x1).size <= 1 || new Set(x2).size <= 1) {
      $("manual-unique-warning").classList.remove("d-none");
      clearData();
      return;
    }
    applyData(x1, x2);
  }

  // ---------- data application ----------
  function applyData(x1, x2) {
    state.x1 = (x1 && x1.length > 0) ? x1 : null;
    state.x2 = (x2 && x2.length > 0) ? x2 : null;
    afterDataChange();
  }
  function clearData() { state.x1 = null; state.x2 = null; afterDataChange(); }

  function afterDataChange() {
    const ready = state.x1 !== null && state.x2 !== null;
    renderPreview();
    $("plot-type-block").classList.toggle("d-none", !ready);
    $("rest-of-exercise").classList.toggle("d-none", !ready);
    if (ready) {
      renderAssumptions();
      renderStats();
    }
  }

  // ---------- preview + assumption plots ----------
  function renderPreview() {
    const el = $("data-preview");
    if (state.x1 === null || state.x2 === null) {
      el.innerHTML = '<span style="color: blue;"><p>In order to proceed, you must select some data to act as your sample.</p></span>';
      return;
    }
    if (state.plotType === "Box_plot") {
      el.innerHTML = Plots.boxplotPairSVG(state.x1, state.x2, { width: 520, height: 300, main: "Side-by-side Boxplots" });
    } else {
      el.innerHTML =
        Plots.histogramSVG(state.x1, { width: 520, height: 200, main: "Sample 1 Histogram", xlab: "Values", ylab: "Frequency", col: "blue", breaks: 30 }) +
        Plots.histogramSVG(state.x2, { width: 520, height: 200, main: "Sample 2 Histogram", xlab: "Values", ylab: "Frequency", col: "red", breaks: 30 });
    }
  }

  function renderAssumptions() {
    $("qq-1").innerHTML = Plots.qqPlotSVG(state.x1, { width: 400, height: 340, main: "Sample 1: QQ Plot" });
    $("qq-2").innerHTML = Plots.qqPlotSVG(state.x2, { width: 400, height: 340, main: "Sample 2: QQ Plot" });
    $("assump3-boxplot").innerHTML = Plots.boxplotPairSVG(state.x1, state.x2, { width: 640, height: 300, main: "Side-by-side Boxplots" });
    $("assump3-hist-1").innerHTML = Plots.histogramSVG(state.x1, { width: 400, height: 300, main: "Sample 1 Histogram", xlab: "Values", ylab: "Frequency", col: "blue", breaks: 30 });
    $("assump3-hist-2").innerHTML = Plots.histogramSVG(state.x2, { width: 400, height: 300, main: "Sample 2 Histogram", xlab: "Values", ylab: "Frequency", col: "red", breaks: 30 });
    $("assump3-sd-out").innerHTML =
      '<div style="text-align: right;"><p><b>Sample 1\'s standard deviation = ' + S.roundStr(S.sd(state.x1), 3) +
      "<br>Sample 2's standard deviation = " + S.roundStr(S.sd(state.x2), 3) + "</b></p></div>";
  }

  // ---------- core numeric quantities for the current data + spread choice ----------
  function core() {
    const x1 = state.x1, x2 = state.x2;
    const n1 = x1.length, n2 = x2.length;
    const mean1 = S.mean(x1), mean2 = S.mean(x2);
    const sd1 = S.sd(x1), sd2 = S.sd(x2);
    const pooledSd = Math.sqrt(((n1 - 1) * sd1 * sd1 + (n2 - 1) * sd2 * sd2) / (n1 + n2 - 2));
    let se, df;
    if (state.spread) {                    // equal variance (pooled)
      se = pooledSd * Math.sqrt(1 / n1 + 1 / n2);
      df = n1 + n2 - 2;
    } else {                               // Welch
      se = Math.sqrt(sd1 * sd1 / n1 + sd2 * sd2 / n2);
      const num = Math.pow(sd1 * sd1 / n1 + sd2 * sd2 / n2, 2);
      const den = Math.pow(sd1 * sd1 / n1, 2) / (n1 - 1) + Math.pow(sd2 * sd2 / n2, 2) / (n2 - 1);
      df = num / den;
    }
    return { n1, n2, mean1, mean2, sd1, sd2, pooledSd, se, df };
  }

  // ---------- main render of every stats-driven section ----------
  function renderStats() {
    const c = core();

    // --- spread decision text ---
    $("spread-decision").innerHTML = state.spread
      ? "<p>You have indicated that the <b>spread</b> of the 2 samples <b>is the same</b>. Hence, below we will do a <b>2-sample t-test with eqaul variance.</b></p>"
      : "<p>You have indicated that the <b>spread</b> of the 2 samples <b>is different</b>. Hence, below we will do a <b>Welch 2-sample t-test.</b></p>";

    // --- box models (Sample 1 uses subscript 1; Sample 2 subscript 2) ---
    $("box-model-1").innerHTML = boxModelHTML(
      "μ₁ = μ₂ ; s₁ = " + S.roundStr(c.sd1, 3), "OV₁ = " + S.roundStr(c.mean1, 3), "n₁ = " + c.n1);
    $("box-model-2").innerHTML = boxModelHTML(
      "μ₂ = μ₁ ; s₂ = " + S.roundStr(c.sd2, 3), "OV₂ = " + S.roundStr(c.mean2, 3), "n₂ = " + c.n2);

    // --- alternate hypothesis ---
    const altSign = state.alt === 1 ? "\\neq" : (state.alt === 2 ? ">" : "<");
    $("alt-hyp-out").innerHTML =
      "<center><p style='font-size: 16px;'>\\( H_1: \\) \\(\\mu_1 " + altSign + " \\mu_2 \\)</p></center>";

    // --- EV / SE ---
    const evStr = "$$\\begin{align*} \\text{EV} &= \\mu_2 - \\mu_1 \\\\ &=0\\end{align*}$$";
    let pooledStr = "";
    let seStr;
    if (state.spread) {
      pooledStr =
        "<p>Pooled standard deviation:</p>" +
        "$$\\begin{align*} \\widehat{\\sigma_p} &= \\sqrt{\\frac{ (n_1 - 1)\\widehat{\\sigma}_1^2 + (n_2 - 1)\\widehat{\\sigma}_2^2 }{n_1 + n_2 - 2}} \\\\" +
        " &= \\sqrt{\\frac{ (" + c.n1 + " - 1)" + S.roundStr(c.sd1, 3) + "^2 + (" + c.n2 + " - 1)" + S.roundStr(c.sd2, 3) + "^2 }{" + c.n1 + " + " + c.n2 + " - 2}} \\\\" +
        " &= \\sqrt{\\frac{ " + S.roundStr((c.n1 - 1) * c.sd1 * c.sd1, 3) + " + " + S.roundStr((c.n2 - 1) * c.sd2 * c.sd2, 3) + " }{" + (c.n1 + c.n2 - 2) + "}} \\\\" +
        " &= \\sqrt{" + S.roundStr(((c.n1 - 1) * c.sd1 * c.sd1 + (c.n2 - 1) * c.sd2 * c.sd2) / (c.n1 + c.n2 - 2), 3) + "} \\\\" +
        " &= " + S.roundStr(c.pooledSd, 3) + "\\end{align*}$$";
      seStr =
        "<p>Standard Error:</p>" +
        "$$\\begin{align*} \\text{SE} &= \\widehat{\\sigma_p}\\sqrt{\\frac{1}{n_1}+\\frac{1}{n_2}} \\\\" +
        " &= " + S.roundStr(c.pooledSd, 3) + "\\sqrt{\\frac{1}{" + c.n1 + "}+\\frac{1}{" + c.n2 + "}} \\\\" +
        " &= " + S.roundStr(c.se, 5) + "\\end{align*}$$";
    } else {
      seStr =
        "<p>Standard Error:</p>" +
        "$$\\begin{align*} \\text{SE} &= \\sqrt{\\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2}} \\\\" +
        " &= \\sqrt{\\frac{" + S.roundStr(c.sd1 * c.sd1, 3) + "}{" + c.n1 + "}+\\frac{" + S.roundStr(c.sd2 * c.sd2, 3) + "}{" + c.n2 + "}} \\\\" +
        " &= " + S.roundStr(c.se, 5) + "\\end{align*}$$";
    }
    $("ev-se-out").innerHTML = "<p>Expected Value:</p>" + evStr + pooledStr + seStr;

    // --- test statistic (full-precision SE and means; TS rounded to 4) ---
    const observed = c.mean1 - c.mean2;
    state.tsStr = S.roundStr(observed / c.se, 4);
    $("ts-out").innerHTML =
      "$$\\begin{align*} \\text{TS} &= \\frac{\\text{OV} - \\text{EV}}{\\text{SE}} \\\\" +
      " &= \\frac{\\mu_1 - \\mu_2 - 0}{\\text{SE}} \\\\" +
      " &= \\frac{" + S.roundStr(c.mean1, 3) + " - " + S.roundStr(c.mean2, 3) + " - 0}{" + S.roundStr(c.se, 3) + "} \\\\" +
      " &= " + state.tsStr + "\\end{align*}$$" +
      "<p style='text-align: left;'><span style='color: blue;'><i>The value for the test-statistic is " + state.tsStr + ". </i></span></p>";

    // --- p-value (uses rounded TS string + full-precision df) ---
    const ts = Number(state.tsStr);
    const df = c.df;
    let second;
    if (state.spread) {
      second =
        "<p>For a 2-sample t-test with equal variance, we set the degree of freedom equal to:</p>" +
        "$$\\begin{align*} \\text{df} &= n_1 + n_2 - 2\\\\ &= " + c.n1 + " + " + c.n2 + " - 2 \\\\ &= " + (c.n1 + c.n2 - 2) + "\\end{align*}$$";
    } else {
      const s1n = S.roundStr(c.sd1 * c.sd1, 3), s2n = S.roundStr(c.sd2 * c.sd2, 3);
      second =
        "<p>For a Welch 2-sample t-test, we set the degrees of freedom equal to:</p>" +
        "$$\\begin{align*} \\text{df} &= \\frac{\\left( \\frac{s_1^2}{n_1} + \\frac{s_2^2}{n_2} \\right)^2}{\\frac{\\left( \\frac{s_1^2}{n_1} \\right)^2}{n_1 - 1} + \\frac{\\left( \\frac{s_2^2}{n_2} \\right)^2}{n_2 - 1}} \\\\" +
        " &= \\frac{\\left( \\frac{" + s1n + "}{" + c.n1 + "} + \\frac{" + s2n + "}{" + c.n2 + "} \\right)^2}{\\frac{\\left( \\frac{" + s1n + "}{" + c.n1 + "} \\right)^2}{" + (c.n1 - 1) + "} + \\frac{\\left( \\frac{" + s2n + "}{" + c.n2 + "} \\right)^2}{" + (c.n2 - 1) + "}} \\\\" +
        " &= " + S.roundStr(df, 3) + "\\end{align*}$$";
    }

    let pVal;
    let third = "<p>The test statistics fall on a standard normal curve. ";
    if (state.alt === 1) {
      pVal = 2 * (1 - S.pt(Math.abs(ts), df));
      third += "As we are doing a two-sided alternate hypothesis, we are interested in finding the <b>area below " +
        S.formatR(-Math.abs(ts)) + " and above " + S.formatR(Math.abs(ts)) + ".</p></b>";
    } else if (state.alt === 2) {
      pVal = 1 - S.pt(ts, df);
      third += "As we are doing a one-sided greater than alternate hypothesis, we are interested in finding the <b>area above " + state.tsStr + ".</p></b>";
    } else {
      pVal = S.pt(ts, df);
      third += "As we are doing a one-sided less than alternate hypothesis, we are interested in finding the <b>area below " + state.tsStr + ".</p></b>";
    }
    state.pVal = pVal;

    $("p-value-prelude").innerHTML =
      "<p>The p-value is the probability of observing a test-statistic <b>more extreme that our test statistic of " + state.tsStr + ".</b></p>" +
      second + third +
      "<p style='font-size: 16px; text-align: center;'>\\( p = " + S.roundStr(pVal, 5) + " \\)</p>";

    // t-curve plot uses df - 1 (reproducing the R source's off-by-one).
    $("t-curve-plot").innerHTML = Plots.shadedTCurveSVG(df - 1, ts, state.alt, { width: 560, height: 325 });

    renderConclusion();
    renderCI();
    typeset($("rest-of-exercise"));
  }

  function renderConclusion() {
    const p = state.pVal, alpha = state.alpha;
    if (p > alpha) {
      $("conclusion-out").innerHTML =
        "$$\\begin{align*} \\alpha &< p \\\\" + S.formatR(alpha) + " &< " + S.roundStr(p, 4) + "\\end{align*}$$" +
        "<span style='color: blue;'><p>As the p value is greater than our significance level, we <b>accept the null hypothesis</b>.</p></span>";
    } else {
      $("conclusion-out").innerHTML =
        "$$\\begin{align*} \\alpha &> p \\\\" + S.formatR(alpha) + " &> " + S.roundStr(p, 4) + "\\end{align*}$$" +
        "<span style='color: blue;'><p>As the p value is less than our significance level, we <b>reject the null hypothesis</b>.</p></span>";
    }
  }

  function renderCI() {
    const c = core();
    const diff = c.mean1 - c.mean2;
    const alpha = 1 - state.conf;
    const se = c.se, df = c.df;
    let formula, sub, ans, concl;

    if (state.alt === 1) {
      const t = S.qt(1 - alpha / 2, df);
      const lower = diff - t * se, upper = diff + t * se;
      formula = "$$CI = (\\bar{x}_1 - \\bar{x}_2) \\pm t_{\\alpha/2, df} \\cdot SE$$";
      sub = "$$CI = " + S.roundStr(diff, 4) + " \\pm t_{" + S.roundStr(alpha / 2, 4) + ", " + S.roundStr(df, 2) + "} \\times " + S.roundStr(se, 4) + "$$";
      ans = "$$CI = (" + S.roundStr(lower, 4) + ", " + S.roundStr(upper, 4) + ")$$";
      concl = (0 < lower || 0 > upper)
        ? "As 0 (no difference) is outside the confidence interval, we <b>reject the null hypothesis</b>."
        : "As 0 (no difference) is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
    } else if (state.alt === 2) {
      const t = S.qt(1 - alpha, df);
      const lower = diff - t * se;
      formula = "$$CI = (\\bar{x}_1 - \\bar{x}_2 - t_{\\alpha, df} \\cdot SE, \\infty)$$";
      sub = "$$CI = (" + S.roundStr(diff, 4) + " - t_{" + S.roundStr(alpha, 4) + ", " + S.roundStr(df, 2) + "} \\times " + S.roundStr(se, 4) + ", \\infty)$$";
      ans = "$$CI = (" + S.roundStr(lower, 4) + ", \\infty)$$";
      concl = (0 < lower)
        ? "As 0 is below the confidence interval, we <b>reject the null hypothesis</b>."
        : "As 0 is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
    } else {
      const t = S.qt(1 - alpha, df);
      const upper = diff + t * se;
      formula = "$$CI = (-\\infty, \\bar{x}_1 - \\bar{x}_2 + t_{\\alpha, df} \\cdot SE)$$";
      sub = "$$CI = (-\\infty, " + S.roundStr(diff, 4) + " + t_{" + S.roundStr(alpha, 4) + ", " + S.roundStr(df, 2) + "} \\times " + S.roundStr(se, 4) + ")$$";
      ans = "$$CI = (-\\infty, " + S.roundStr(upper, 4) + ")$$";
      concl = (0 > upper)
        ? "As 0 is above the confidence interval, we <b>reject the null hypothesis</b>."
        : "As 0 is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
    }
    $("ci-out").innerHTML = formula + sub + ans + "<span style='color: blue;'><p>" + concl + "</p></span>";
  }

  // ---------- intro-modal box models (blood_pressure, Drug_A vs Drug_B) ----------
  function renderModalBoxes() {
    const cat = getCol("blood_pressure", "drug").values;
    const dep = getCol("blood_pressure", "blood_pressure").values;
    const a = [], b = [];
    for (let i = 0; i < cat.length; i++) (cat[i] === "Drug_A" ? a : b).push(dep[i]);
    $("box-model-modal-1").innerHTML = boxModelHTML(
      "μ₁ = μ₂ ; s₁ = " + S.roundStr(S.sd(a), 3), "OV₁ = " + S.roundStr(S.mean(a), 3), "n₁ = " + a.length);
    // The R source mislabels the Sample 2 modal box with subscript 1 -- reproduced.
    $("box-model-modal-2").innerHTML = boxModelHTML(
      "μ₁ = μ₂ ; s₁ = " + S.roundStr(S.sd(b), 3), "OV₁ = " + S.roundStr(S.mean(b), 3), "n₁ = " + b.length);
  }

  // ---------- event bindings ----------
  document.addEventListener("DOMContentLoaded", function () {
    renderModalBoxes();

    for (const radio of document.querySelectorAll('input[name="data_upload_choice"]')) {
      radio.addEventListener("change", e => onDataChoiceChange(e.target.value));
    }
    $("dataset-select").addEventListener("change", onDatasetChange);
    $("categorical-select").addEventListener("change", onCategoricalChange);
    $("sample-1-select").addEventListener("change", onSampleOrDepvarChange);
    $("sample-2-select").addEventListener("change", onSampleOrDepvarChange);
    $("depvar-select").addEventListener("change", onSampleOrDepvarChange);
    $("upload-btn").addEventListener("click", onManualUpload);

    for (const radio of document.querySelectorAll('input[name="plot_type"]')) {
      radio.addEventListener("change", function () { state.plotType = this.value; renderPreview(); });
    }

    for (const radio of document.querySelectorAll('input[name="alternate_hypothesis_choice"]')) {
      radio.addEventListener("change", function () {
        state.alt = Number(this.value);
        if (state.x1 !== null) renderStats();
      });
    }

    $("spread-toggle").addEventListener("change", function () {
      state.spread = this.checked;
      if (state.x1 !== null) renderStats();
    });

    $("alpha-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v < 0 || v > 1) { state.alpha = 0.05; state.alphaWarn = true; }
      else { state.alpha = v; state.alphaWarn = false; }
      $("alpha-warning").classList.toggle("d-none", !state.alphaWarn);
      if (state.x1 !== null) { renderConclusion(); typeset($("conclusion-out")); }
    });

    $("conf-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v <= 0 || v >= 1) { state.conf = 0.95; state.confWarn = true; }
      else { state.conf = v; state.confWarn = false; }
      $("conf-warning").classList.toggle("d-none", !state.confWarn);
      if (state.x1 !== null) { renderCI(); typeset($("ci-out")); }
    });
  });

})();
