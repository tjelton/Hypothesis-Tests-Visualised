// app.js -- interactive wiring for the chi-square goodness of fit lesson.
//
// There is no Shiny predecessor for this lesson; it follows the same structure
// and conventions as the other JS lessons (proportion-test / t-test-1-sample).
//
// Inputs (entered manually, no dataset):
//   * number of categories k (>= 2)
//   * an observed COUNT per category; the sample size n is their sum
//   * a null PROPORTION per category (must sum to 1; defaults to 1/k each)
//
// The test is always right-tailed. For each category the expected count is
// E_i = n * p_i, the per-category contribution is (O_i - E_i)^2 / E_i, and the
// test statistic is their sum. Degrees of freedom = k - 1, and the p-value is
// the upper-tail chi-square probability P(chi^2_{k-1} > TS).
//
// Calculation parity notes (same round-then-parse chain the other lessons use):
//   TS_string = as.character(round(sum contributions, 4))
//   p-value computed from as.numeric(TS_string) via the upper-tail chi-square.

"use strict";

(function () {

  const $ = id => document.getElementById(id);
  const S = Stats;

  // Warn about a proportion sum being off by more than this. Loose enough that
  // rounding a repeating fraction (e.g. 1/6 -> 0.16667) still reads as summing
  // to 1, tight enough to catch a genuinely wrong set of proportions.
  const SUM_TOL = 1e-3;

  const state = {
    k: 6,
    observed: [8, 12, 9, 11, 15, 5],
    nullProps: equalProps(6),
    alpha: 0.05, alphaWarn: false,
    tsStr: "", pVal: 0
  };

  function equalProps(k) { return Array.from({ length: k }, () => 1 / k); }

  function typeset(el) {
    if (!window.MathJax) return;
    const nodes = el ? [el] : undefined;
    const run = () => window.MathJax.typesetPromise(nodes).catch(() => {});
    // MathJax loads async (deferred script): chain onto its startup promise so
    // the first render typesets once it has initialised.
    if (window.MathJax.startup && window.MathJax.startup.promise) {
      window.MathJax.startup.promise = window.MathJax.startup.promise.then(run);
    } else if (window.MathJax.typesetPromise) {
      run();
    }
  }

  // ---------- box tickets ----------
  // Smart ticket string for the box under the null hypothesis. Tries to express
  // the proportions as small whole-number ticket counts (so 1/6 each -> one
  // ticket per face "1, 2, 3, 4, 5, 6"); if that needs too many tickets (e.g.
  // 50 equal categories at 2% each) it falls back to a percentage form
  // ("1" x 2%, "2" x 2%, ...).
  function ticketsString(props) {
    const k = props.length;
    const clean = props.map(p => (Number.isFinite(p) && p > 0) ? p : 0);
    // Smallest multiplier making every proportion a near-whole ticket count.
    // Tolerance is generous because the proportions come from rounded inputs.
    let m = 0;
    for (let cand = 1; cand <= 100; cand++) {
      if (clean.every(p => Math.abs(p * cand - Math.round(p * cand)) < 5e-3)) { m = cand; break; }
    }
    if (m > 0) {
      const counts = clean.map(p => Math.round(p * m));
      const total = counts.reduce((a, b) => a + b, 0);
      if (total >= 1 && total <= 24) {
        const parts = [];
        for (let i = 0; i < k; i++) for (let j = 0; j < counts[i]; j++) parts.push(String(i + 1));
        return parts.join(", ");
      }
    }
    return clean.map((p, i) => '"' + (i + 1) + '" x ' + S.formatR(S.roundR(p * 100, 2)) + "%").join(", ");
  }

  // ---------- dynamic input builders ----------
  function buildObservedInputs() {
    let html = '<table class="table table-sm align-middle" style="max-width:360px;">' +
      '<thead><tr><th>Category</th><th>Observed count</th></tr></thead><tbody>';
    for (let i = 0; i < state.k; i++) {
      html += '<tr><td>' + (i + 1) + '</td><td>' +
        '<input type="number" class="form-control form-control-sm obs-input" id="obs-' + i +
        '" value="' + state.observed[i] + '" min="0" step="1" style="max-width:140px;"></td></tr>';
    }
    html += "</tbody></table>";
    $("observed-inputs").innerHTML = html;
    for (let i = 0; i < state.k; i++) $("obs-" + i).addEventListener("input", renderAll);
  }

  function buildNullInputs() {
    let html = '<table class="table table-sm align-middle" style="max-width:360px;">' +
      '<thead><tr><th>Category</th><th>Null proportion \\( p_i \\)</th></tr></thead><tbody>';
    for (let i = 0; i < state.k; i++) {
      html += '<tr><td>' + (i + 1) + '</td><td>' +
        '<input type="number" class="form-control form-control-sm null-input" id="null-' + i +
        '" value="' + S.roundStr(state.nullProps[i], 5) + '" min="0" max="1" step="any" style="max-width:140px;"></td></tr>';
    }
    html += "</tbody></table>";
    $("null-inputs").innerHTML = html;
    // state.nullProps is the source of truth, kept at full precision so an
    // untouched default (exactly 1/k) yields clean expected counts. The input
    // only DISPLAYS a rounded value; we overwrite the stored proportion when the
    // user actually edits that cell.
    for (let i = 0; i < state.k; i++) {
      const idx = i;
      $("null-" + i).addEventListener("input", function () {
        state.nullProps[idx] = this.value === "" ? NaN : Number(this.value);
        renderAll();
      });
    }
  }

  // Rebuild both input tables after the category count changes. Observed counts
  // keep their existing values where they overlap (new cells default to 0); the
  // null proportions reset to equal, since the old set no longer sums to 1.
  function rebuildInputs() {
    const oldObs = state.observed;
    state.observed = Array.from({ length: state.k }, (_, i) => (i < oldObs.length ? oldObs[i] : 0));
    state.nullProps = equalProps(state.k);
    buildObservedInputs();
    buildNullInputs();
  }

  // ---------- reading inputs ----------
  function readObserved() {
    const vals = [];
    let valid = true, badCells = 0;
    for (let i = 0; i < state.k; i++) {
      const raw = $("obs-" + i).value;
      const v = raw === "" ? NaN : Number(raw);
      if (!Number.isFinite(v) || v < 0 || !Number.isInteger(v)) { valid = false; badCells++; vals.push(NaN); }
      else vals.push(v);
    }
    return { vals, valid, badCells };
  }

  // ---------- main render ----------
  function renderAll() {
    const obs = readObserved();
    const n = obs.valid ? obs.vals.reduce((a, b) => a + b, 0) : 0;

    // Gate the rest of the exercise on valid observed data with n >= 1.
    if (!obs.valid || n < 1) {
      $("observed-warning-text").textContent = !obs.valid
        ? "Each observed count must be a whole number of 0 or greater."
        : "The total sample size must be at least 1 (enter some observed counts).";
      $("observed-warning").classList.remove("d-none");
      $("rest-of-exercise").classList.add("d-none");
      renderPreview(null, 0);
      return;
    }
    $("observed-warning").classList.add("d-none");
    $("rest-of-exercise").classList.remove("d-none");
    state.observed = obs.vals;

    // Null proportions (from state, kept even if they don't sum to 1, so the
    // student sees live feedback; a red warning flags the problem).
    const nullVals = state.nullProps;
    const inRange = v => Number.isFinite(v) && v >= 0 && v <= 1;
    const anyNullBad = nullVals.some(v => !inRange(v));
    const nullSum = nullVals.reduce((a, v) => a + (inRange(v) ? v : 0), 0);
    const props = nullVals.map(v => (inRange(v) ? v : 0));
    if (anyNullBad) {
      $("null-sum-warning-text").textContent =
        "Each null proportion must be a number between 0 and 1.";
      $("null-sum-warning").classList.remove("d-none");
    } else if (Math.abs(nullSum - 1) > SUM_TOL) {
      $("null-sum-warning-text").textContent =
        "The null proportions currently add up to " + S.roundStr(nullSum, 4) + ", but they must add up to 1.";
      $("null-sum-warning").classList.remove("d-none");
    } else {
      $("null-sum-warning").classList.add("d-none");
    }

    // Expected counts are rounded to 2 dp so the test-statistic table is
    // self-consistent: every O - E and (O - E)^2 / E a student reads off the
    // table is computed from the E shown, not from an unrounded value that would
    // make e.g. a "fair die" report 10.0002 and 0.4001 instead of 10 and 0.4.
    const expected = props.map(p => S.roundR(n * p, 2));
    const df = state.k - 1;

    renderPreview(obs.vals, n);
    renderBoxAndHyp(props, n);
    renderCochran(expected, n);
    renderTestStat(obs.vals, expected, n);

    // p-value: upper-tail chi-square evaluated at the rounded test statistic.
    const ts = Number(state.tsStr);
    const pVal = S.pchisqUpper(ts, df);
    state.pVal = pVal;
    renderPValue(ts, df, pVal);

    renderConclusion();
    typeset($("rest-of-exercise"));
    typeset($("data-preview"));
  }

  function renderPreview(obs, n) {
    const el = $("data-preview");
    if (obs === null) {
      el.innerHTML = '<span style="color: blue;"><p>Enter a valid set of observed counts to proceed.</p></span>';
      return;
    }
    let rows = "";
    for (let i = 0; i < state.k; i++) {
      rows += "<tr><td>" + (i + 1) + "</td><td>" + obs[i] + "</td><td>" + S.roundStr(obs[i] / n, 4) + "</td></tr>";
    }
    el.innerHTML =
      '<p style="font-size: 18px;"><b>Sample size:</b> \\( n = ' + n + ' \\)</p>' +
      '<p>Your observed data, with each category\'s observed proportion (count \\( \\div\\, n \\)):</p>' +
      '<table class="table table-sm"><thead><tr><th>Category</th><th>Observed</th><th>Observed prop.</th></tr></thead><tbody>' +
      rows + "</tbody></table>";
  }

  function renderBoxAndHyp(props, n) {
    const tickets = ticketsString(props);
    const font = tickets.length > 30 ? 15 : 20;
    // Box -> arrow (with n beside it) -> sample oval, mirroring the 1-sample
    // t-test box model; the oval says "Observed Sample" instead of an OV.
    $("box-model").innerHTML = boxModelHTML(tickets, "Observed Sample", "n = " + n, font);

    // Null hypothesis statement. If every proportion is equal we collapse to the
    // compact "= 1/k" chained form; otherwise we list each proportion.
    const allEqual = props.every(p => Math.abs(p - props[0]) < 1e-9);
    let nullTex;
    if (allEqual) {
      const chain = Array.from({ length: state.k }, (_, i) => "p_{" + (i + 1) + "}").join(" = ");
      nullTex = "H_0: " + chain + " = \\frac{1}{" + state.k + "}";
    } else {
      const parts = props.map((p, i) => "p_{" + (i + 1) + "} = " + S.formatR(S.roundR(p, 4)));
      nullTex = "H_0:\\ " + parts.join(",\\ ");
    }
    $("null-hyp-out").innerHTML = "<center><p style='font-size: 16px;'>\\( " + nullTex + " \\)</p></center>";
    $("alt-hyp-out").innerHTML =
      "<center><p style='font-size: 16px;'>\\( H_1: \\) at least one of the equalities in \\( H_0 \\) does not hold.</p></center>";
  }

  function renderCochran(expected, n) {
    let rows = "";
    let numLt5 = 0, anyZero = false;
    for (let i = 0; i < state.k; i++) {
      const e = expected[i];
      const lt5 = e < 5;
      if (lt5) numLt5++;
      if (e <= 0) anyZero = true;
      rows += "<tr><td>" + (i + 1) + "</td><td" + (lt5 ? " style='color:red;'" : "") + ">" +
        S.roundStr(e, 4) + (lt5 ? " (&lt; 5)" : "") + "</td></tr>";
    }
    $("cochran-table").innerHTML =
      '<table class="table table-sm table-bordered" style="max-width:360px;">' +
      "<thead><tr><th>Category</th><th>Expected count \\( E_i \\)</th></tr></thead><tbody>" +
      rows + "</tbody></table>";

    const pctLt5 = 100 * numLt5 / state.k;
    const cond20 = numLt5 <= 0.2 * state.k + 1e-9;
    const holds = !anyZero && cond20;

    let verdict;
    if (holds) {
      verdict = "<span style='color: green;'><p><b>The assumption holds.</b> None of the expected counts are 0, and " +
        numLt5 + " of the " + state.k + " expected counts (" + S.roundStr(pctLt5, 1) +
        "%) are below 5, which is within the 20% limit.</p></span>";
    } else {
      let reasons = [];
      if (anyZero) reasons.push("at least one expected count is 0");
      if (!cond20) reasons.push(numLt5 + " of the " + state.k + " expected counts (" + S.roundStr(pctLt5, 1) +
        "%) are below 5, which exceeds the 20% limit");
      verdict = "<span style='color: red;'><p><b>Warning: the assumption does NOT hold</b> because " +
        reasons.join(", and ") + ". The chi-square approximation may be unreliable; consider collecting more " +
        "data or combining categories.</p></span>";
    }
    $("cochran-verdict").innerHTML = verdict;
  }

  function renderTestStat(obs, expected, n) {
    let rows = "";
    let sumObs = 0, sumExp = 0, sumDiff = 0, sumContrib = 0;
    for (let i = 0; i < state.k; i++) {
      const O = obs[i], E = expected[i];
      const diff = O - E;
      const contrib = E > 0 ? (diff * diff) / E : NaN;
      sumObs += O; sumExp += E; sumDiff += diff;
      if (Number.isFinite(contrib)) sumContrib += contrib;
      rows += "<tr><td>" + (i + 1) + "</td><td>" + O + "</td><td>" + S.roundStr(E, 4) + "</td><td>" +
        S.roundStr(diff, 4) + "</td><td>" + (Number.isFinite(contrib) ? S.roundStr(contrib, 4) : "&mdash;") + "</td></tr>";
    }
    // Bottom (bold) row of column sums.
    rows += "<tr style='font-weight:bold;'><td>Sum</td><td>" + sumObs + "</td><td>" + S.roundStr(sumExp, 4) +
      "</td><td>" + S.roundStr(sumDiff, 4) + "</td><td>" + S.roundStr(sumContrib, 4) + "</td></tr>";

    $("ts-table").innerHTML =
      '<div style="overflow-x:auto;"><table class="table table-sm table-bordered" style="min-width:520px;">' +
      "<thead><tr><th>Category</th><th>Observed \\( (O) \\)</th><th>Expected \\( (E) \\)</th>" +
      "<th>\\( O - E \\)</th><th>\\( \\frac{(O - E)^2}{E} \\)</th></tr></thead><tbody>" +
      rows + "</tbody></table></div>";

    state.tsStr = S.roundStr(sumContrib, 4);
    $("ts-out").innerHTML =
      "<p style='text-align: left;'><span style='color: blue;'><i>The value for the test-statistic is " +
      state.tsStr + ". </i></span></p>";
  }

  function renderPValue(ts, df, pVal) {
    $("p-value-prelude").innerHTML =
      "<p>The p-value is the probability of observing a test statistic <b>at least as large as our test statistic of " +
      state.tsStr + "</b>, assuming the null hypothesis is true.</p>" +
      "<p>The test statistic follows a <b>chi-square distribution</b>. Its shape depends on a single parameter, the " +
      "<b>degrees of freedom</b>, which for a goodness of fit test equals the number of categories minus one " +
      "(\\( k - 1 \\)).</p>" +
      "<p>In this case, the degrees of freedom is \\( " + state.k + " - 1 = " + df + " \\).</p>" +
      "<p>Because a larger test statistic means the observed counts are further from what the null hypothesis predicts, " +
      "we are always interested in the <b>upper tail</b> &mdash; the area to the <b>right</b> of " + state.tsStr + ".</p>" +
      "<p style='font-size: 16px; text-align: center;'>\\( p = " + S.roundStr(pVal, 5) + " \\)</p>";
    $("chi-curve-plot").innerHTML = Plots.shadedChiSquareCurveSVG(df, ts, { width: 560, height: 325 });
  }

  function renderConclusion() {
    const p = state.pVal, alpha = state.alpha;
    let mathLine, conclusionLine;
    if (p > alpha) {
      mathLine = "$$\\begin{align*} \\alpha &< p \\\\" + S.formatR(alpha) + " &< " + S.roundStr(p, 4) + "\\end{align*}$$";
      conclusionLine = "<span style='color: blue;'><p>As the p value is greater than our significance level, we <b>accept the null hypothesis</b>.</p></span>";
    } else {
      mathLine = "$$\\begin{align*} \\alpha &> p \\\\" + S.formatR(alpha) + " &> " + S.roundStr(p, 4) + "\\end{align*}$$";
      conclusionLine = "<span style='color: blue;'><p>As the p value is less than our significance level, we <b>reject the null hypothesis</b>.</p></span>";
    }
    $("conclusion-out").innerHTML = mathLine + conclusionLine;
  }

  // ---------- event bindings ----------
  document.addEventListener("DOMContentLoaded", function () {
    // Static example box model in the intro modal (fair die: one ticket per
    // face), drawn with the sample oval and n like the other lessons.
    $("intro-example-box-model").innerHTML = boxModelHTML("1, 2, 3, 4, 5, 6", "Observed Sample", "n = 60", 20);

    buildObservedInputs();
    buildNullInputs();

    $("num-categories").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (!Number.isFinite(v) || v < 2 || !Number.isInteger(v)) {
        $("num-cat-warning").classList.remove("d-none");
        return;                       // keep the last valid category count
      }
      $("num-cat-warning").classList.add("d-none");
      state.k = v;
      rebuildInputs();
      renderAll();
    });

    $("alpha-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (!Number.isFinite(v) || v < 0 || v > 1) {
        state.alpha = 0.05;
        state.alphaWarn = true;
      } else {
        state.alpha = v;
        state.alphaWarn = false;
      }
      $("alpha-warning").classList.toggle("d-none", !state.alphaWarn);
      renderConclusion();
      typeset($("conclusion-out"));
    });

    renderAll();
  });

})();
