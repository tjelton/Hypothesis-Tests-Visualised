// app.js -- interactive wiring for the 1-sample z-test lesson. Ports
// R/ztest_1_sample_srv.R. The data loader (load_1_sample_data) and datasets are
// identical to the 1-sample t-test; the statistical difference is that the
// z-test assumes a KNOWN population standard deviation (sigma) and uses the
// standard normal distribution rather than the t-distribution:
//   SE = sigma / sqrt(n);  z = (OV - EV) / SE;  p-value from pnorm (no df).
//
// Calculation parity notes (matching the Shiny code's round-then-parse chain):
//   EV_string = as.character(round(null_mu, 5))
//   SE_string = as.character(round(sigma / sqrt(n), 5))
//   TS        = as.character(round((mean - as.numeric(EV_string)) / as.numeric(SE_string), 4))
//   p-value computed from as.numeric(TS) via pnorm.
//   sigma defaults to the sample SD and resets to it whenever the data changes
//   or an invalid (<= 0) value is entered.
//
// Reproduced R quirk (documented so both versions can be fixed together): the
// confidence interval is centred on the EXPECTED value (the null mean) rather
// than the observed sample mean -- the R source sets xbar = as.numeric(EV_string).
// Because the null value is then the centre of the interval, it is always inside,
// so the CI conclusion is always "fail to reject". This mirrors the Shiny app.

"use strict";

(function () {

  const state = {
    dataChoice: null,
    dataset: "Mr. Han's Math Class",
    column: null,
    factorCol: "(None)",
    factorLevel: null,
    data: null,
    manualNAs: 0,
    sigma: null,                 // population SD (defaults to the sample SD)
    nullMu: 140,
    altChoice: 1,
    alpha: 0.05, alphaWarn: false,
    conf: 0.95, confWarn: false,
    tsStr: "", pVal: 0
  };

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
  function setOptions(select, options, selected) {
    select.innerHTML = "";
    for (const o of options) {
      const opt = document.createElement("option");
      opt.value = o; opt.textContent = o;
      if (o === selected) opt.selected = true;
      select.appendChild(opt);
    }
  }

  // ---------- dataset helpers (identical to the 1-sample t-test loader) ----------
  function columns(ds) { return DATASETS[ds].columns; }
  function numericCols(ds) { return columns(ds).filter(c => c.type === "numeric").map(c => c.name); }
  function factorCols(ds) { return columns(ds).filter(c => c.type === "factor").map(c => c.name); }
  function getCol(ds, name) { return columns(ds).find(c => c.name === name); }

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

  function onDataChoiceChange(choice) {
    state.dataChoice = choice;
    $("pre-uploaded-block").classList.toggle("d-none", choice !== "pre_uploaded");
    $("manual-block").classList.toggle("d-none", choice !== "manually_specified");
    if (choice === "pre_uploaded") {
      state.dataset = "Mr. Han's Math Class";
      setOptions($("dataset-select"), Object.keys(DATASETS), state.dataset);
      onDatasetChange();
    } else {
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
    state.factorCol = "(None)"; state.factorLevel = null;
    if (fCols.length > 0) {
      setOptions($("factor-select"), ["(None)"].concat(fCols), "(None)");
      $("factor-block").classList.remove("d-none");
    } else {
      $("factor-block").classList.add("d-none");
    }
    $("category-block").classList.add("d-none");
    setData(computePreUploadedData());
  }

  function onColumnChange() { state.column = $("column-select").value; setData(computePreUploadedData()); }

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

  function onCategoryChange() { state.factorLevel = $("category-select").value; setData(computePreUploadedData()); }

  function onManualUpload() {
    const text = $("manual-textarea").value;
    if (!text) return;
    const splits = text.split(/[,\n]/).map(s => s.trim()).filter(s => s !== "");
    const numeric = splits.map(s => (s === "" || isNaN(Number(s))) ? NaN : Number(s));
    const valid = numeric.filter(v => !Number.isNaN(v));
    if (new Set(valid).size <= 1) {
      $("manual-unique-warning").classList.remove("d-none");
      $("manual-missing-warning").classList.add("d-none");
      setData(null);
      return;
    }
    $("manual-unique-warning").classList.add("d-none");
    state.manualNAs = numeric.length - valid.length;
    if (state.manualNAs > 0) {
      $("manual-missing-warning-text").innerHTML =
        "Warning: From the data that you uploaded, " + state.manualNAs +
        " of the values could not be interpreted. This could be becuase these values were not numeric, " +
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
      // sigma resets to the sample SD whenever the data changes (the R module
      // re-renders the numericInput with value = sample SD).
      state.sigma = S.sd(state.data);
      $("pop-sd").value = state.sigma;
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

  function renderStats() {
    const data = state.data;
    const n = data.length;
    const xbar = S.mean(data);
    const sigma = state.sigma;
    const nullMuStr = S.roundStr(state.nullMu, 3);

    // --- box model (uses sigma, not the sample SD) ---
    $("box-model").innerHTML = boxModelHTML(
      "&mu; = " + nullMuStr + "; &sigma; = " + S.roundStr(sigma, 3),
      "OV = " + S.roundStr(xbar, 3),
      "n = " + n
    );

    // --- hypotheses ---
    $("null-hyp-out").innerHTML =
      "<center><p style='font-size: 16px;'>\\( H_0: \\) \\( \\mu = " + nullMuStr + " \\)</p></center>";
    const altSign = state.altChoice === 1 ? "\\neq" : (state.altChoice === 2 ? ">" : "<");
    $("alt-hyp-out").innerHTML =
      "<center><p style='font-size: 16px;'>\\( H_1: \\) \\(\\mu " + altSign + " " + nullMuStr + " \\)</p></center>";

    // --- test statistic (EV = null mu; SE = sigma / sqrt(n)) ---
    const EV = state.nullMu;
    const EVStr = S.roundStr(EV, 5);
    const SE = sigma / Math.sqrt(n);
    const SEStr = S.roundStr(SE, 5);

    $("ev-se-out").innerHTML =
      "<p>Expected Value:</p>" +
      "$$\\begin{align*} \\text{EV} &= \\mu \\\\ &=" + EVStr + "\\end{align*}$$" +
      "<p>Standard Error:</p>" +
      "$$\\begin{align*} \\text{SE} &= \\frac{\\sigma}{\\sqrt{n}} \\\\ &= \\frac{" + S.roundStr(sigma, 5) +
        "}{\\sqrt{" + n + "}}\\\\ &= " + SEStr + "\\end{align*}$$";

    const tsNum = (xbar - Number(EVStr)) / Number(SEStr);
    state.tsStr = S.roundStr(tsNum, 4);
    $("ts-out").innerHTML =
      "$$\\begin{align*} \\text{TS} &= \\frac{\\text{OV} - \\text{EV}}{\\text{SE}} \\\\ &= \\frac{" +
        S.roundStr(xbar, 5) + " - " + EVStr + "}{" + SEStr + "} \\\\ &= " + state.tsStr + "\\end{align*}$$" +
      "<p style='text-align: left;'><span style='color: blue;'><i>The value for the test-statistic is " + state.tsStr + ". </i></span></p>";

    // --- p-value (standard normal; no degrees of freedom) ---
    const z = Number(state.tsStr);
    let pVal;
    let second = "<p>The test statistics fall on a standard normal curve. ";
    if (state.altChoice === 1) {
      pVal = 2 * (1 - S.pnorm(Math.abs(z)));
      second += "As we are doing a two-sided alternate hypothesis, we are interested in finding the <b>area below " +
        S.formatR(-Math.abs(z)) + " and above " + S.formatR(Math.abs(z)) + ".</p></b>";
    } else if (state.altChoice === 2) {
      pVal = 1 - S.pnorm(z);
      second += "As we are doing a one-sided greater than alternate hypothesis, we are interested in finding the <b>area above " + state.tsStr + ".</p></b>";
    } else {
      pVal = S.pnorm(z);
      second += "As we are doing a one-sided less than alternate hypothesis, we are interested in finding the <b>area below " + state.tsStr + ".</p></b>";
    }
    state.pVal = pVal;

    $("p-value-prelude").innerHTML =
      "<p>The p-value is the probability of observing a test-statistic <b>more extreme that our test statistic of " + state.tsStr + ".</b></p>" +
      second +
      "<p style='font-size: 16px; text-align: center;'>\\( p = " + S.roundStr(pVal, 5) + " \\)</p>";

    $("normal-plot").innerHTML = Plots.shadedNormalCurveSVG(z, state.altChoice, { width: 560, height: 325 });

    renderConclusion();
    renderCI(Number(EVStr), SE, Number(nullMuStr));
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

  // The R source centres the CI on `xbar = as.numeric(EV_string)` (the null
  // mean), not the observed mean -- reproduced here (see header note).
  function renderCI(xbar, se, mu0) {
    const conf = state.conf;
    const alpha = 1 - conf;
    let formula, sub, ans, concl;

    if (state.altChoice === 1) {
      const z = S.qnorm(1 - alpha / 2);
      const lower = xbar - z * se, upper = xbar + z * se;
      formula = "$$CI = \\bar{x} \\pm z_{\\alpha/2} \\cdot SE$$";
      sub = "$$CI = " + S.roundStr(xbar, 4) + " \\pm " + S.roundStr(z, 4) + " \\times " + S.roundStr(se, 4) + "$$";
      ans = "$$CI = (" + S.roundStr(lower, 4) + ", " + S.roundStr(upper, 4) + ")$$";
      concl = (mu0 < lower || mu0 > upper)
        ? "As the null hypothesis value is outside the confidence interval, we <b>reject the null hypothesis</b>."
        : "As the null hypothesis value is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
    } else if (state.altChoice === 2) {
      const z = S.qnorm(1 - alpha);
      const lower = xbar - z * se;
      formula = "$$CI = (\\bar{x} - z_{\\alpha} \\cdot SE, \\infty)$$";
      sub = "$$CI = (" + S.roundStr(xbar, 4) + " - " + S.roundStr(z, 4) + " \\times " + S.roundStr(se, 4) + ", \\infty)$$";
      ans = "$$CI = (" + S.roundStr(lower, 4) + ", \\infty)$$";
      concl = (mu0 < lower)
        ? "As the null hypothesis value is below the confidence interval, we <b>reject the null hypothesis</b>."
        : "As the null hypothesis value is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
    } else {
      const z = S.qnorm(1 - alpha);
      const upper = xbar + z * se;
      formula = "$$CI = (-\\infty, \\bar{x} + z_{\\alpha} \\cdot SE)$$";
      sub = "$$CI = (-\\infty, " + S.roundStr(xbar, 4) + " + " + S.roundStr(z, 4) + " \\times " + S.roundStr(se, 4) + ")$$";
      ans = "$$CI = (-\\infty, " + S.roundStr(upper, 4) + ")$$";
      concl = (mu0 > upper)
        ? "As the null hypothesis value is above the confidence interval, we <b>reject the null hypothesis</b>."
        : "As the null hypothesis value is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
    }
    $("ci-out").innerHTML = formula + sub + ans + "<span style='color: blue;'><p>" + concl + "</p></span>";
  }

  // ---------- event bindings ----------
  document.addEventListener("DOMContentLoaded", function () {
    $("intro-example-box-model").innerHTML = boxModelHTML("&mu; = 140; &sigma; = 7.5", "OV = 142.843", "n = 25");

    for (const radio of document.querySelectorAll('input[name="data_upload_choice"]')) {
      radio.addEventListener("change", e => onDataChoiceChange(e.target.value));
    }
    $("dataset-select").addEventListener("change", onDatasetChange);
    $("column-select").addEventListener("change", onColumnChange);
    $("factor-select").addEventListener("change", onFactorChange);
    $("category-select").addEventListener("change", onCategoryChange);
    $("upload-btn").addEventListener("click", onManualUpload);

    // population SD controls
    $("set-pop-sd").addEventListener("click", function () {
      if (state.data === null) return;
      state.sigma = S.sd(state.data);
      $("pop-sd").value = state.sigma;
      renderStats();
    });
    $("pop-sd").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (state.data === null) return;
      if (Number.isNaN(v) || v <= 0) {
        state.sigma = S.sd(state.data);   // invalid -> reset to sample SD
        this.value = state.sigma;
      } else {
        state.sigma = v;
      }
      renderStats();
    });

    $("null-mu").addEventListener("input", function () {
      state.nullMu = this.value === "" ? NaN : Number(this.value);
      if (state.data !== null) renderStats();
    });

    for (const radio of document.querySelectorAll('input[name="alternate_hypothesis_choice"]')) {
      radio.addEventListener("change", function () { state.altChoice = Number(this.value); if (state.data !== null) renderStats(); });
    }

    $("alpha-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v < 0 || v > 1) { state.alpha = 0.05; state.alphaWarn = true; }
      else { state.alpha = v; state.alphaWarn = false; }
      $("alpha-warning").classList.toggle("d-none", !state.alphaWarn);
      if (state.data !== null) { renderConclusion(); typeset($("conclusion-out")); }
    });

    $("conf-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v <= 0 || v >= 1) { state.conf = 0.95; state.confWarn = true; }
      else { state.conf = v; state.confWarn = false; }
      $("conf-warning").classList.toggle("d-none", !state.confWarn);
      if (state.data !== null) {
        const n = state.data.length;
        renderCI(Number(S.roundStr(state.nullMu, 5)), state.sigma / Math.sqrt(n), Number(S.roundStr(state.nullMu, 3)));
        typeset($("ci-out"));
      }
    });
  });

})();
