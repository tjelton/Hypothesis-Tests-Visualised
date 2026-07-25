// app.js -- interactive wiring for the chi-square test of independence lesson.
//
// There is no Shiny predecessor; it follows the same structure and conventions
// as the goodness-of-fit lesson and the other JS lessons.
//
// Inputs (entered manually, no dataset):
//   * a name and a category count for each of two qualitative variables X (rows)
//     and Y (columns), plus a name for every category level;
//   * an observed COUNT for every cell of the resulting contingency table.
//
// Under H0 the two variables are independent, so the expected count of a cell is
//   E_ij = (row i total) * (column j total) / grand total.
// The test is right-tailed: chi^2 = sum over cells of (O - E)^2 / E, with
// df = (rows - 1) * (columns - 1) and p = P(chi^2_df > chi^2).
//
// Expected counts are rounded to 2 dp and every downstream cell (O - E,
// (O-E)^2/E, the summed statistic) is computed from that rounded value, so the
// displayed tables are self-consistent (matches the goodness-of-fit lesson).

"use strict";

(function () {

  const $ = id => document.getElementById(id);
  const S = Stats;

  const state = {
    xName: "Coffee Drinker",
    yName: "Night Owl",
    rows: 2,
    cols: 2,
    xLevels: ["Drinks Coffee", "No Coffee"],
    yLevels: ["Night Owl", "Not a Night Owl"],
    observed: [[50, 30], [20, 40]],
    alpha: 0.05, alphaWarn: false,
    tsStr: "", pVal: 0
  };

  function esc(s) {
    return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;")
      .replace(/"/g, "&quot;").replace(/'/g, "&#39;");
  }

  function typeset(el) {
    if (!window.MathJax) return;
    const nodes = el ? [el] : undefined;
    const run = () => window.MathJax.typesetPromise(nodes).catch(() => {});
    if (window.MathJax.startup && window.MathJax.startup.promise) {
      window.MathJax.startup.promise = window.MathJax.startup.promise.then(run);
    } else if (window.MathJax.typesetPromise) {
      run();
    }
  }

  // ---------- labels (with fallbacks for blank inputs) ----------
  function varLabel(which) {
    const nm = which === "x" ? state.xName : state.yName;
    return (nm && nm.trim()) ? nm : (which === "x" ? "X" : "Y");
  }
  function levelLabel(which, i) {
    const arr = which === "x" ? state.xLevels : state.yLevels;
    const nm = arr[i];
    return (nm && nm.trim()) ? nm : "Category " + (i + 1);
  }

  // ---------- generic read-only grid (contingency-style table) ----------
  // cellHTML(i, j) -> inner HTML for the body cell. Optional row/column totals
  // and grand total (arrays / value of already-formatted strings).
  function buildGrid(cellHTML, opts = {}) {
    const withCol = opts.colTotals != null;
    const withRow = opts.rowTotals != null;
    const totalHeader = opts.totalHeader || "Total";
    const span = state.cols + (withCol ? 1 : 0);

    let head = '<tr><th></th><th class="text-center" colspan="' + span + '">' + esc(varLabel("y")) + "</th></tr>";
    head += '<tr><th>' + esc(varLabel("x")) + "</th>";
    for (let j = 0; j < state.cols; j++) head += '<th class="text-center">' + esc(levelLabel("y", j)) + "</th>";
    if (withCol) head += '<th class="text-center">' + totalHeader + "</th>";
    head += "</tr>";

    let body = "";
    for (let i = 0; i < state.rows; i++) {
      body += "<tr><th>" + esc(levelLabel("x", i)) + "</th>";
      for (let j = 0; j < state.cols; j++) body += '<td class="text-center">' + cellHTML(i, j) + "</td>";
      if (withRow) body += '<td class="text-center"><b>' + opts.rowTotals[i] + "</b></td>";
      body += "</tr>";
    }
    if (withCol) {
      body += "<tr><th>" + totalHeader + "</th>";
      for (let j = 0; j < state.cols; j++) body += '<td class="text-center"><b>' + opts.colTotals[j] + "</b></td>";
      if (opts.grand != null) body += '<td class="text-center"><b>' + opts.grand + "</b></td>";
      body += "</tr>";
    }
    const minW = Math.max(320, 120 * (state.cols + 1));
    return '<div style="overflow-x:auto;"><table class="table table-sm table-bordered align-middle" style="min-width:' +
      minW + 'px;"><thead>' + head + "</thead><tbody>" + body + "</tbody></table></div>";
  }

  // ---------- dynamic input builders ----------
  function buildLevelInputs(which) {
    const n = which === "x" ? state.rows : state.cols;
    const arr = which === "x" ? state.xLevels : state.yLevels;
    let html = '<p class="mb-1" style="font-size:0.9rem;">Category names:</p>';
    for (let i = 0; i < n; i++) {
      html += '<input type="text" class="form-control form-control-sm mb-1" id="' + which + "-level-" + i +
        '" value="' + esc(arr[i] || "") + '" placeholder="Category ' + (i + 1) + '">';
    }
    $(which + "-levels").innerHTML = html;
    for (let i = 0; i < n; i++) {
      const idx = i;
      $(which + "-level-" + i).addEventListener("input", function () {
        (which === "x" ? state.xLevels : state.yLevels)[idx] = this.value;
        renderAll();
      });
    }
  }

  function buildObservedTable() {
    let head = '<tr><th></th><th class="text-center" colspan="' + state.cols + '">' + esc(varLabel("y")) + "</th></tr>";
    head += '<tr><th>' + esc(varLabel("x")) + "</th>";
    for (let j = 0; j < state.cols; j++) head += '<th class="text-center">' + esc(levelLabel("y", j)) + "</th>";
    head += "</tr>";
    let body = "";
    for (let i = 0; i < state.rows; i++) {
      body += "<tr><th>" + esc(levelLabel("x", i)) + "</th>";
      for (let j = 0; j < state.cols; j++) {
        body += '<td><input type="number" class="form-control form-control-sm" id="obs-' + i + "-" + j +
          '" value="' + state.observed[i][j] + '" min="0" step="1" style="min-width:80px;"></td>';
      }
      body += "</tr>";
    }
    const minW = Math.max(320, 120 * (state.cols + 1));
    $("observed-table").innerHTML =
      '<div style="overflow-x:auto;"><table class="table table-sm table-bordered align-middle" style="min-width:' +
      minW + 'px;"><thead>' + head + "</thead><tbody>" + body + "</tbody></table></div>";
    for (let i = 0; i < state.rows; i++) {
      for (let j = 0; j < state.cols; j++) {
        const ri = i, cj = j;
        $("obs-" + i + "-" + j).addEventListener("input", function () {
          state.observed[ri][cj] = this.value === "" ? NaN : Number(this.value);
          renderAll();
        });
      }
    }
  }

  // Rebuild level-name inputs and the observed table after a category count
  // changes. Existing level names / observed counts are kept where they overlap;
  // new cells default to "" / 0.
  function resizeArrays() {
    const oldX = state.xLevels, oldY = state.yLevels, oldObs = state.observed;
    state.xLevels = Array.from({ length: state.rows }, (_, i) => (i < oldX.length ? oldX[i] : ""));
    state.yLevels = Array.from({ length: state.cols }, (_, j) => (j < oldY.length ? oldY[j] : ""));
    state.observed = Array.from({ length: state.rows }, (_, i) =>
      Array.from({ length: state.cols }, (_, j) =>
        (oldObs[i] && oldObs[i][j] != null) ? oldObs[i][j] : 0));
  }

  function rebuildInputs() {
    buildLevelInputs("x");
    buildLevelInputs("y");
    buildObservedTable();
  }

  // ---------- validation ----------
  function validateObserved() {
    let valid = true, grand = 0;
    for (let i = 0; i < state.rows; i++) {
      for (let j = 0; j < state.cols; j++) {
        const v = state.observed[i][j];
        if (!Number.isFinite(v) || v < 0 || !Number.isInteger(v)) valid = false;
        else grand += v;
      }
    }
    return { valid, grand };
  }

  // ---------- main render ----------
  function renderAll() {
    const { valid, grand } = validateObserved();
    if (!valid || grand < 1) {
      $("observed-warning-text").textContent = !valid
        ? "Each observed count must be a whole number of 0 or greater."
        : "The total sample size must be at least 1 (enter some observed counts).";
      $("observed-warning").classList.remove("d-none");
      $("rest-of-exercise").classList.add("d-none");
      renderPreview(null);
      return;
    }
    $("observed-warning").classList.add("d-none");
    $("rest-of-exercise").classList.remove("d-none");

    // Margins.
    const rowTot = state.observed.map(r => r.reduce((a, b) => a + b, 0));
    const colTot = [];
    for (let j = 0; j < state.cols; j++) { let s = 0; for (let i = 0; i < state.rows; i++) s += state.observed[i][j]; colTot.push(s); }

    // Expected counts, rounded to 2 dp (see file header).
    const exp = state.observed.map((r, i) => r.map((_, j) => S.roundR(rowTot[i] * colTot[j] / grand, 2)));
    const df = (state.rows - 1) * (state.cols - 1);

    renderPreview(rowTot, colTot, grand);
    renderHypotheses();
    renderExpectedTable(exp);
    renderCochran(exp);
    renderTestStat(exp, df);

    const ts = Number(state.tsStr);
    state.pVal = S.pchisqUpper(ts, df);
    renderPValue(ts, df, state.pVal);
    renderConclusion();

    typeset($("rest-of-exercise"));
    typeset($("data-preview"));
  }

  function renderPreview(rowTot, colTot, grand) {
    const el = $("data-preview");
    if (rowTot === null) {
      el.innerHTML = '<span style="color: blue;"><p>Enter a valid contingency table of observed counts to proceed.</p></span>';
      return;
    }
    el.innerHTML =
      '<p style="font-size: 18px;"><b>Sample size:</b> \\( n = ' + grand + ' \\) (the grand total)</p>' +
      "<p>Your observed counts, with the row, column and grand totals used to find the expected frequencies:</p>" +
      buildGrid((i, j) => "" + state.observed[i][j], { rowTotals: rowTot, colTotals: colTot, grand: grand });
  }

  function renderHypotheses() {
    const x = "<b>" + esc(varLabel("x")) + "</b>", y = "<b>" + esc(varLabel("y")) + "</b>";
    $("null-hyp-out").innerHTML =
      "<center><p style='font-size: 16px;'>\\( H_0: \\) " + x + " and " + y + " are <b>independent</b>.</p></center>";
    $("alt-hyp-out").innerHTML =
      "<center><p style='font-size: 16px;'>\\( H_1: \\) " + x + " and " + y + " are <b>associated</b>.</p></center>";
  }

  function renderExpectedTable(exp) {
    $("expected-table").innerHTML = buildGrid((i, j) => S.roundStr(exp[i][j], 2));
  }

  function renderCochran(exp) {
    const nCells = state.rows * state.cols;
    let numLt5 = 0, anyZero = false;
    for (let i = 0; i < state.rows; i++) {
      for (let j = 0; j < state.cols; j++) {
        if (exp[i][j] < 5) numLt5++;
        if (exp[i][j] <= 0) anyZero = true;
      }
    }
    $("cochran-table").innerHTML = buildGrid((i, j) => {
      const e = exp[i][j], lt5 = e < 5;
      return (lt5 ? '<span style="color:red;">' : "") + S.roundStr(e, 2) + (lt5 ? " (&lt; 5)</span>" : "");
    });

    const pctLt5 = 100 * numLt5 / nCells;
    const cond20 = numLt5 <= 0.2 * nCells + 1e-9;
    const holds = !anyZero && cond20;
    let verdict;
    if (holds) {
      verdict = "<span style='color: green;'><p><b>The assumption holds.</b> None of the expected counts are 0, and " +
        numLt5 + " of the " + nCells + " expected counts (" + S.roundStr(pctLt5, 1) +
        "%) are below 5, which is within the 20% limit.</p></span>";
    } else {
      const reasons = [];
      if (anyZero) reasons.push("at least one expected count is 0");
      if (!cond20) reasons.push(numLt5 + " of the " + nCells + " expected counts (" + S.roundStr(pctLt5, 1) +
        "%) are below 5, which exceeds the 20% limit");
      verdict = "<span style='color: red;'><p><b>Warning: the assumption does NOT hold</b> because " +
        reasons.join(", and ") + ". The chi-square approximation may be unreliable; consider collecting more " +
        "data or combining categories.</p></span>";
    }
    $("cochran-verdict").innerHTML = verdict;
  }

  function renderTestStat(exp, df) {
    // Tab 1: observed (blue) - expected (red) = difference, per cell.
    $("oe-table").innerHTML = buildGrid((i, j) => {
      const O = state.observed[i][j], E = exp[i][j], diff = O - E;
      return '<span style="color:blue;">' + O + '</span> &minus; <span style="color:red;">' + S.roundStr(E, 2) +
        "</span> = " + S.roundStr(diff, 2);
    });

    // Tab 2: (O - E)^2 / E per cell, then the summed test statistic.
    let sum = 0;
    $("contrib-table").innerHTML = buildGrid((i, j) => {
      const O = state.observed[i][j], E = exp[i][j];
      if (E <= 0) return "&mdash;";
      const contrib = (O - E) * (O - E) / E;
      sum += contrib;
      return S.roundStr(contrib, 4);
    });

    state.tsStr = S.roundStr(sum, 4);
    $("ts-out").innerHTML =
      "<p style='text-align: left;'><span style='color: blue;'><i>Adding \\( \\frac{(O - E)^2}{E} \\) over every cell, the value " +
      "for the test-statistic is " + state.tsStr + ". </i></span></p>";
  }

  function renderPValue(ts, df, pVal) {
    $("p-value-prelude").innerHTML =
      "<p>The p-value is the probability of observing a test statistic <b>at least as large as our test statistic of " +
      state.tsStr + "</b>, assuming the null hypothesis (independence) is true.</p>" +
      "<p>The test statistic follows a <b>chi-square distribution</b>. For a test of independence, the " +
      "<b>degrees of freedom</b> equal (number of rows &minus; 1) &times; (number of columns &minus; 1).</p>" +
      "<p>In this case, the degrees of freedom is \\( (" + state.rows + " - 1)(" + state.cols + " - 1) = " + df + " \\).</p>" +
      "<p>Because a larger test statistic means the observed counts are further from what independence predicts, " +
      "we are always interested in the <b>upper tail</b> &mdash; the area to the <b>right</b> of " + state.tsStr + ".</p>" +
      "<p style='font-size: 16px; text-align: center;'>\\( p = " + S.roundStr(pVal, 5) + " \\)</p>";
    $("chi-curve-plot").innerHTML = Plots.shadedChiSquareCurveSVG(df, ts, { width: 560, height: 325 });
  }

  function renderConclusion() {
    const p = state.pVal, alpha = state.alpha;
    let mathLine, conclusionLine;
    if (p > alpha) {
      mathLine = "$$\\begin{align*} \\alpha &< p \\\\" + S.formatR(alpha) + " &< " + S.roundStr(p, 4) + "\\end{align*}$$";
      conclusionLine = "<span style='color: blue;'><p>As the p value is greater than our significance level, we <b>accept the null hypothesis</b> (the variables appear independent).</p></span>";
    } else {
      mathLine = "$$\\begin{align*} \\alpha &> p \\\\" + S.formatR(alpha) + " &> " + S.roundStr(p, 4) + "\\end{align*}$$";
      conclusionLine = "<span style='color: blue;'><p>As the p value is less than our significance level, we <b>reject the null hypothesis</b> (the variables appear associated).</p></span>";
    }
    $("conclusion-out").innerHTML = mathLine + conclusionLine;
  }

  // ---------- event bindings ----------
  document.addEventListener("DOMContentLoaded", function () {
    buildLevelInputs("x");
    buildLevelInputs("y");
    buildObservedTable();

    $("x-name").addEventListener("input", function () { state.xName = this.value; renderAll(); });
    $("y-name").addEventListener("input", function () { state.yName = this.value; renderAll(); });

    function onCountChange(which) {
      const input = which === "x" ? $("x-count") : $("y-count");
      const v = input.value === "" ? NaN : Number(input.value);
      if (!Number.isFinite(v) || v < 2 || !Number.isInteger(v)) {
        $("count-warning").classList.remove("d-none");
        return;                       // keep the last valid count
      }
      $("count-warning").classList.add("d-none");
      if (which === "x") state.rows = v; else state.cols = v;
      resizeArrays();
      rebuildInputs();
      renderAll();
    }
    $("x-count").addEventListener("input", () => onCountChange("x"));
    $("y-count").addEventListener("input", () => onCountChange("y"));

    $("alpha-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (!Number.isFinite(v) || v < 0 || v > 1) { state.alpha = 0.05; state.alphaWarn = true; }
      else { state.alpha = v; state.alphaWarn = false; }
      $("alpha-warning").classList.toggle("d-none", !state.alphaWarn);
      renderConclusion();
      typeset($("conclusion-out"));
    });

    renderAll();
  });

})();
