// app.js -- interactive wiring for the regression t-test lesson. Ports
// R/ttest_regression_srv.R and its data loader
// (utility_load_data_regression_2_variable_srv.R).
//
// The test fits a simple linear regression y ~ x (OLS) and tests H0: slope = 0.
// All quantities come from Stats.linreg (matching R's lm()/summary()):
//   slope beta1, residual standard error s (= summary$sigma), SE(beta1) = s/sqrt(Sxx),
//   t = beta1 / SE(beta1), df = n - 2.
//
// Calculation parity notes (matching the Shiny code):
//   * the displayed test statistic is round(t, 3); the p-value uses that
//     rounded value with df = n - 2, while the CI uses the full-precision slope
//     and SE.
//   * incomplete (x, y) pairs are dropped before fitting, as R's lm() does via
//     na.omit (relevant for airquality, which contains NA).
//
// Reproduced R quirk: the p-value t-curve plot is drawn with df - 1 (the R code
// passes `df = df() - 1` to the plotting helper, while the p-value uses df).

"use strict";

(function () {

  const state = {
    dataChoice: null,
    dataset: "study_data",
    xcol: null, ycol: null,
    factorCol: "(None)", factorLevel: null,
    x: null, y: null,           // complete (x, y) pairs, or null
    alt: 1,
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
  function factorCols(ds) { return columns(ds).filter(c => c.type === "categorical").map(c => c.name); }
  function getCol(ds, name) { return columns(ds).find(c => c.name === name); }
  function uniqueInOrder(arr) {
    const seen = new Set(), out = [];
    for (const v of arr) if (!seen.has(v)) { seen.add(v); out.push(v); }
    return out;
  }

  // ---------- data source UI flow ----------
  function onDataChoiceChange(choice) {
    state.dataChoice = choice;
    $("pre-uploaded-block").classList.toggle("d-none", choice !== "pre_uploaded");
    $("manual-block").classList.toggle("d-none", choice !== "manually_specified");
    if (choice === "pre_uploaded") {
      state.dataset = "study_data";
      setOptions($("dataset-select"), Object.keys(DATASETS), state.dataset);
      onDatasetChange();
    } else {
      $("manual-textarea-x").value = "";
      $("manual-textarea-y").value = "";
      $("manual-unequal-warning").classList.add("d-none");
      $("manual-unique-warning").classList.add("d-none");
      clearData();
    }
  }

  function onDatasetChange() {
    state.dataset = $("dataset-select").value;
    const numCols = numericCols(state.dataset);
    state.xcol = numCols[0];
    state.ycol = numCols.length > 1 ? numCols[1] : numCols[0];
    setOptions($("x-select"), numCols, state.xcol);
    setOptions($("y-select"), numCols, state.ycol);

    const facs = factorCols(state.dataset);
    state.factorCol = "(None)"; state.factorLevel = null;
    if (facs.length > 0) {
      setOptions($("factor-select"), ["(None)"].concat(facs), "(None)");
      $("factor-block").classList.remove("d-none");
    } else {
      $("factor-block").classList.add("d-none");
    }
    $("category-block").classList.add("d-none");
    recomputePreUploaded();
  }

  function onColumnChange() {
    state.xcol = $("x-select").value;
    state.ycol = $("y-select").value;
    recomputePreUploaded();
  }

  function onFactorChange() {
    state.factorCol = $("factor-select").value;
    if (state.factorCol === "(None)") {
      state.factorLevel = null;
      $("category-block").classList.add("d-none");
    } else {
      const levels = uniqueInOrder(getCol(state.dataset, state.factorCol).values);
      state.factorLevel = levels[0];
      setOptions($("category-select"), levels, state.factorLevel);
      $("category-block").classList.remove("d-none");
    }
    recomputePreUploaded();
  }

  function onCategoryChange() {
    state.factorLevel = $("category-select").value;
    recomputePreUploaded();
  }

  function recomputePreUploaded() {
    let xs = getCol(state.dataset, state.xcol).values;
    let ys = getCol(state.dataset, state.ycol).values;
    let keep = xs.map((_, i) => true);
    if (state.factorCol !== "(None)" && state.factorLevel !== null) {
      const fc = getCol(state.dataset, state.factorCol).values;
      keep = keep.map((k, i) => fc[i] === state.factorLevel);
    }
    const x = [], y = [];
    for (let i = 0; i < xs.length; i++) {
      if (!keep[i]) continue;
      // Drop incomplete pairs (NA -> null), as R's lm() does via na.omit.
      if (typeof xs[i] === "number" && typeof ys[i] === "number") { x.push(xs[i]); y.push(ys[i]); }
    }
    applyData(x, y);
  }

  function onManualUpload() {
    const tx = $("manual-textarea-x").value, ty = $("manual-textarea-y").value;
    if (!tx || !ty) return;
    $("manual-unequal-warning").classList.add("d-none");
    $("manual-unique-warning").classList.add("d-none");
    const parse = t => t.split(/[,\n]/).map(s => s.trim()).filter(s => s !== "")
      .map(s => Number(s)).filter(v => !Number.isNaN(v));
    const x = parse(tx), y = parse(ty);
    if (x.length !== y.length) {
      $("manual-unequal-warning-text").textContent =
        "Warning: The number of values in each condition is unequal. Condition 1 has " +
        x.length + " values, and condition 2 has " + y.length + " values.";
      $("manual-unequal-warning").classList.remove("d-none");
      clearData();
      return;
    }
    // R requires only that ONE of the two has >= 2 unique values (uses ||).
    if (new Set(x).size <= 1 && new Set(y).size <= 1) {
      $("manual-unique-warning").classList.remove("d-none");
      clearData();
      return;
    }
    applyData(x, y);
  }

  // ---------- data application ----------
  function applyData(x, y) {
    // Need at least 3 complete points and non-constant x for a finite fit.
    let sxx = 0;
    if (x.length >= 3) {
      const mx = S.mean(x);
      for (const xi of x) sxx += (xi - mx) * (xi - mx);
    }
    if (x.length >= 3 && sxx > 0) { state.x = x; state.y = y; }
    else { state.x = null; state.y = null; }
    afterDataChange();
  }
  function clearData() { state.x = null; state.y = null; afterDataChange(); }

  function afterDataChange() {
    const ready = state.x !== null;
    renderPreview();
    $("rest-of-exercise").classList.toggle("d-none", !ready);
    if (ready) { renderAssumptions(); renderStats(); }
  }

  // ---------- plots ----------
  function renderPreview() {
    const el = $("data-preview");
    if (state.x === null) {
      el.innerHTML = '<span style="color: blue;"><p>In order to proceed, you must select some data to act as your sample.</p></span>';
      return;
    }
    el.innerHTML = Plots.scatterSVG(state.x, state.y, { width: 460, height: 360, xlab: "x-axis", ylab: "y-axis" });
  }

  function renderAssumptions() {
    const fit = S.linreg(state.x, state.y);
    const res = fit.residuals;
    $("lin-scatter").innerHTML = Plots.scatterSVG(state.x, state.y, { width: 400, height: 340, main: "Scatter Plot", xlab: "X", ylab: "Y" });
    const residualPlot = Plots.scatterSVG(state.x, res, { width: 400, height: 340, main: "Residual Plot", xlab: "X", ylab: "Residuals", hline: 0 });
    $("lin-residual").innerHTML = residualPlot;
    $("homo-residual").innerHTML = residualPlot;
    $("indep-residual").innerHTML = residualPlot;
    $("qq-resid").innerHTML = Plots.qqPlotSVG(res, { width: 400, height: 340 });
    const order = res.map((_, i) => i + 1);
    $("ordered-residuals").innerHTML = Plots.scatterSVG(order, res, { width: 400, height: 340, main: "Residuals vs Observation Order", xlab: "Observation Order", ylab: "Residuals", connect: true, hline: 0 });
  }

  // ---------- stats sections ----------
  function renderStats() {
    const fit = S.linreg(state.x, state.y);
    const n = fit.n, df = fit.df, slope = fit.slope, se = fit.seSlope, s = fit.s, rss = fit.rss, sxx = fit.sxx;

    // --- alternate hypothesis ---
    const altSign = state.alt === 1 ? "\\neq" : (state.alt === 2 ? ">" : "<");
    $("alt-hyp-out").innerHTML =
      "<center><p style='font-size: 16px;'>\\( H_1: \\) \\(\\beta_1 " + altSign + " 0 \\)</p></center>";

    // --- EV / SE ---
    $("ev-se-out").innerHTML =
      "<p>Expected value under \\( H_0 \\):</p>" +
      "$$\\begin{align*}\\mathbb{E}[\\widehat{\\beta}_1] &= 0 \\end{align*}$$" +
      "<p>Residual standard deviation:</p>" +
      "$$\\begin{align*}s &= \\sqrt{\\frac{1}{n - 2} \\bigl((y_1 - \\widehat{y}_1)^2 + \\cdots + (y_n - \\widehat{y}_n)^2\\bigr)} \\\\" +
      "&= \\sqrt{\\frac{1}{" + n + " - 2} \\times " + S.roundStr(rss, 3) + "} \\\\" +
      "&= \\sqrt{\\frac{" + S.roundStr(rss, 3) + "}{" + df + "}} \\\\" +
      "&= \\sqrt{" + S.roundStr(rss / df, 3) + "} \\\\" +
      "&= " + S.roundStr(s, 3) + "\\end{align*}$$" +
      "<p>Standard Error:</p>" +
      "$$\\begin{align*}SE(\\widehat{\\beta}_1) &= \\frac{s}{\\sqrt{(x_1 - \\bar{x})^2  + \\cdots + (x_n - \\bar{x})^2}} \\\\" +
      "&= \\frac{" + S.roundStr(s, 3) + "}{\\sqrt{" + S.roundStr(sxx, 3) + "}} \\\\" +
      "&= \\frac{" + S.roundStr(s, 3) + "}{" + S.roundStr(Math.sqrt(sxx), 3) + "} \\\\" +
      "&= " + S.roundStr(se, 3) + "\\end{align*}$$";

    // --- test statistic (displayed to 3 dp) ---
    const t = slope / se;
    state.tsStr = S.roundStr(t, 3);
    $("ts-out").innerHTML =
      "<p>When calculating the linear regression line for the sample data, $$\\widehat{\\beta}_1 = " + S.roundStr(slope, 3) + "$$.</p>" +
      "<p>Now to calculate the test statistic:</p>" +
      "$$\\begin{align*}t &= \\frac{\\widehat{\\beta}_1 - \\mathbb{E}[\\widehat{\\beta}_1]}{SE(\\widehat{\\beta}_1)} \\\\" +
      "&= \\frac{" + S.roundStr(slope, 3) + " - 0}{" + S.roundStr(se, 3) + "} \\\\" +
      "&= " + state.tsStr + "\\end{align*}$$" +
      "<p style='text-align: left;'><span style='color: blue;'><i>The value for the test-statistic is " + state.tsStr + ". </i></span></p>";

    // --- p-value (rounded TS + df = n - 2) ---
    const ts = Number(state.tsStr);
    let pVal, third = "<p>The test statistics fall on a standard normal curve. ";
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
      "<p>For a regression t-test, we set the degree of freedom equal to:</p>" +
      "$$\\begin{align*} \\text{df} &= n - 2\\\\ &= " + n + " - 2 \\\\ &= " + df + "\\end{align*}$$" +
      third +
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
    const fit = S.linreg(state.x, state.y);
    const slope = fit.slope, se = fit.seSlope, df = fit.df;
    const alpha = 1 - state.conf;
    let formula, sub, ans, concl;

    if (state.alt === 1) {
      const t = S.qt(1 - alpha / 2, df);
      const lower = slope - t * se, upper = slope + t * se;
      formula = "$$CI = (\\widehat{\\beta}_1) \\pm t_{\\alpha/2, df} \\cdot SE(\\widehat{\\beta}_1)$$";
      sub = "$$CI = " + S.roundStr(slope, 4) + " \\pm t_{" + S.roundStr(alpha / 2, 4) + ", " + S.roundStr(df, 2) + "} \\times " + S.roundStr(se, 4) + "$$";
      ans = "$$CI = (" + S.roundStr(lower, 4) + ", " + S.roundStr(upper, 4) + ")$$";
      concl = (0 < lower || 0 > upper)
        ? "As 0 (no linear association) is outside the confidence interval, we <b>reject the null hypothesis</b>."
        : "As 0 (no linear association) is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
    } else if (state.alt === 2) {
      const t = S.qt(1 - alpha, df);
      const lower = slope - t * se;
      formula = "$$CI = (\\widehat{\\beta}_1 - t_{\\alpha, df} \\cdot SE(\\widehat{\\beta}_1), \\infty)$$";
      sub = "$$CI = (" + S.roundStr(slope, 4) + " - t_{" + S.roundStr(alpha, 4) + ", " + S.roundStr(df, 2) + "} \\times " + S.roundStr(se, 4) + ", \\infty)$$";
      ans = "$$CI = (" + S.roundStr(lower, 4) + ", \\infty)$$";
      concl = (0 < lower)
        ? "As 0 is below the confidence interval, we <b>reject the null hypothesis</b>."
        : "As 0 is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
    } else {
      const t = S.qt(1 - alpha, df);
      const upper = slope + t * se;
      formula = "$$CI = (-\\infty, \\widehat{\\beta}_1 + t_{\\alpha, df} \\cdot SE(\\widehat{\\beta}_1))$$";
      sub = "$$CI = (-\\infty, " + S.roundStr(slope, 4) + " + t_{" + S.roundStr(alpha, 4) + ", " + S.roundStr(df, 2) + "} \\times " + S.roundStr(se, 4) + ")$$";
      ans = "$$CI = (-\\infty, " + S.roundStr(upper, 4) + ")$$";
      concl = (0 > upper)
        ? "As 0 is above the confidence interval, we <b>reject the null hypothesis</b>."
        : "As 0 is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
    }
    $("ci-out").innerHTML = formula + sub + ans + "<span style='color: blue;'><p>" + concl + "</p></span>";
  }

  // ---------- intro-modal plots (study_data) ----------
  function renderModalPlots() {
    const mins = getCol("study_data", "Minutes_Studied").values;
    const score = getCol("study_data", "Test_Score").values;
    $("intro-scatter").innerHTML = Plots.scatterSVG(mins, score, { width: 380, height: 320, main: "Study Data", xlab: "Minutes Studied", ylab: "Test Score" });
    // Hypothetical horizontal regression line at y = 50 (no points).
    $("intro-hline").innerHTML = Plots.scatterSVG([], [], { width: 380, height: 320, main: "Hypothetical Horizontal Regression Line", xlab: "Minutes Studied", ylab: "Test Score", xlim: [0, 600], ylim: [0, 100], hline: 50 });
  }

  // ---------- event bindings ----------
  document.addEventListener("DOMContentLoaded", function () {
    renderModalPlots();

    for (const radio of document.querySelectorAll('input[name="data_upload_choice"]')) {
      radio.addEventListener("change", e => onDataChoiceChange(e.target.value));
    }
    $("dataset-select").addEventListener("change", onDatasetChange);
    $("x-select").addEventListener("change", onColumnChange);
    $("y-select").addEventListener("change", onColumnChange);
    $("factor-select").addEventListener("change", onFactorChange);
    $("category-select").addEventListener("change", onCategoryChange);
    $("upload-btn").addEventListener("click", onManualUpload);

    for (const radio of document.querySelectorAll('input[name="alternate_hypothesis_choice"]')) {
      radio.addEventListener("change", function () {
        state.alt = Number(this.value);
        if (state.x !== null) renderStats();
      });
    }

    $("alpha-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v < 0 || v > 1) { state.alpha = 0.05; state.alphaWarn = true; }
      else { state.alpha = v; state.alphaWarn = false; }
      $("alpha-warning").classList.toggle("d-none", !state.alphaWarn);
      if (state.x !== null) { renderConclusion(); typeset($("conclusion-out")); }
    });

    $("conf-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v <= 0 || v >= 1) { state.conf = 0.95; state.confWarn = true; }
      else { state.conf = v; state.confWarn = false; }
      $("conf-warning").classList.toggle("d-none", !state.confWarn);
      if (state.x !== null) { renderCI(); typeset($("ci-out")); }
    });
  });

})();
