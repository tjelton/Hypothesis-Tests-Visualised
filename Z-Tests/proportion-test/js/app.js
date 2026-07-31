// app.js -- interactive wiring for the proportion test (z-test). Inputs are
// entered manually (null proportion p0, sample size n, observed count x); there
// is no dataset. The box holds "1" and "0" tickets in the null proportion, and
// the test uses the standard normal distribution:
//   OV = x / n,  EV = p0,  SE = sqrt(p0(1-p0)) / sqrt(n)
//   z = (OV - EV) / SE;  p-value from pnorm;  CI is the Wilson score interval.
//
// Calculation parity notes:
//   EV_string = as.character(round(EV, 5)); SE_string = as.character(round(SE, 5));
//   TS = as.character(round((OV - as.numeric(EV_string)) / as.numeric(SE_string), 4));
//   p-value from as.numeric(TS) via pnorm. The empirical-distribution histogram
//   is a random simulation (Math.random).
//
// Deliberate divergences from the original Shiny app:
//   * The "sum" box representation has been REMOVED. It bounded the observed
//     value to [0, 1] while comparing it against EV = n*p0, so the test
//     statistic was nonsense (-8.08 on the defaults) and contradicted the
//     confidence-interval section on the same page. The mean of a 0/1 box IS
//     the proportion, which is what the hypotheses are stated in, so mean is
//     the only representation that makes sense here.
//   * The observed value is entered as a COUNT x, not a pre-rounded proportion.
//     OV = x/n is then exact; the old input invited "0.73" for 22/30, which
//     moved the p-value by 0.03.
//   * The confidence level is DERIVED as 1 - alpha rather than being a second
//     free input, so the CI and p-value conclusions cannot disagree.
//   * "accept the null hypothesis" is now "fail to reject the null hypothesis".
//   * p0 = 0/1 and n = 0 are now rejected by validation. They gave SE = 0 or
//     SE = Inf, rendering TS as "-Inf"/"NA" while the conclusion still claimed
//     "reject the null hypothesis".

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
  function gcd(a, b) { a = Math.round(a); b = Math.round(b); while (b) { [a, b] = [b, a % b]; } return a; }

  const state = { nullProp: 0.7, n: 50, count: 37, alt: 1, alpha: 0.05, tsStr: "", pVal: 0 };

  // The observed proportion, computed exactly from the count.
  function ov() { return state.count / state.n; }

  // EV/SE for the current p0 and n. The box is built assuming H0 is true, so
  // the ticket SD is fixed by p0 -- it is not estimated from the sample.
  function evse() {
    const p0 = state.nullProp, n = state.n;
    const sd = Math.sqrt(p0 * (1 - p0));
    return { EV: p0, SE: sd / Math.sqrt(n), sd };
  }

  // Ticket string for the box: "1" and "0" tickets in the null proportion,
  // simplified via the GCD (or a percentage form when it won't simplify).
  function ticketsString() {
    const propTemp = state.nullProp * 100;
    const isInt = Math.abs(propTemp - Math.round(propTemp)) < 1e-9;
    const g = isInt ? gcd(propTemp, 100 - propTemp) : 1;
    if (g < 5) {
      const comp = 100 - propTemp;
      return "1 x " + S.formatR(S.roundR(propTemp, 2)) + "%, 0 x " + S.formatR(S.roundR(comp, 2)) + "%";
    }
    const ones = propTemp / g, zeros = (100 - propTemp) / g;
    const parts = [];
    for (let i = 0; i < ones; i++) parts.push("1");
    for (let i = 0; i < zeros; i++) parts.push("0");
    return parts.join(", ");
  }

  document.addEventListener("DOMContentLoaded", function () {
    $("intro-example-box-model").innerHTML = boxModelHTML("1, 1, 1, 1, 1, 1, 1, 0, 0, 0", "Sample Mean", "n = 50");

    function renderBoxAndHyp() {
      $("box-model").innerHTML = boxModelHTML(ticketsString(), "Sample Mean", "n = " + state.n);
      const p0s = S.formatR(state.nullProp);
      $("null-hyp-out").innerHTML = "<center><p style='font-size: 16px;'>\\( H_0: \\) \\( p = " + p0s + " \\)</p></center>";
      const sign = state.alt === 1 ? "\\neq" : (state.alt === 2 ? ">" : "<");
      $("alt-hyp-out").innerHTML = "<center><p style='font-size: 16px;'>\\( H_1: \\) \\( p " + sign + " " + p0s + " \\)</p></center>";
    }

    function renderStats() {
      renderBoxAndHyp();
      const { EV, SE, sd } = evse();
      const EVStr = S.roundStr(EV, 5), SEStr = S.roundStr(SE, 5);
      const p0s = S.formatR(state.nullProp);

      $("assump3-text").innerHTML =
        "<p>The third assumption is that the sample means follow an approximate normal distribution.</p>" +
        "<p><span style='color: blue;'><b>How do we check?</b></span><br></p>" +
        "<ul><li>Recall that the central limit theorem tells us that if we take a sufficiently large number of draws from the box, " +
        "then the sample means will approximately follow a normal distribution. <i>If confused, please see the box model exercise</i>.</li>" +
        "<li>One way we can easily tell if the central limit theorem applies is to take many draws from the box, and see whether the values appear normally distributed. " +
        "The plot alongside shows the distribution of 10,000 simulated sample means.</li>" +
        "<li>As a general rule, the approximation is good enough when we expect at least 10 draws on each side — that is, at least 10 \"1\" tickets " +
        "and at least 10 \"0\" tickets in a typical sample:</li></ul>" +
        "$$n \\times p \\geq 10 \\quad \\text{and} \\quad n \\times (1 - p) \\geq 10$$" +
        "<p>The reason we need both is that a proportion close to 0 or 1 has little room to vary on one side, which makes the distribution " +
        "lopsided rather than bell-shaped. A larger \\( n \\) is needed to correct for that.</p>";

      // Live check of the two inequalities against the current inputs.
      const nP = state.n * state.nullProp, nQ = state.n * (1 - state.nullProp);
      const okP = nP >= 10, okQ = nQ >= 10;
      $("assump3-check").innerHTML =
        "$$n \\times p = " + state.n + " \\times " + p0s + " = " + S.roundStr(nP, 4) +
          (okP ? " \\geq 10 \\;\\checkmark" : " < 10 \\;\\times") + "$$" +
        "$$n \\times (1 - p) = " + state.n + " \\times " + S.formatR(S.roundR(1 - state.nullProp, 10)) + " = " + S.roundStr(nQ, 4) +
          (okQ ? " \\geq 10 \\;\\checkmark" : " < 10 \\;\\times") + "$$" +
        (okP && okQ
          ? "<span style='color: green;'><p><b>The assumption holds.</b> Both values are at least 10, so the normal approximation is reasonable here.</p></span>"
          : "<span style='color: #b8860b;'><p><b>Careful.</b> " + (okP ? "\\( n \\times (1 - p) \\)" : "\\( n \\times p \\)") +
            " is below 10, so the normal approximation may be unreliable. Increase \\( n \\), or move the null proportion away from " +
            (okP ? "1" : "0") + ".</p></span>");

      $("observed-value-text").innerHTML =
        "<p><i>How many of your \\( n = " + state.n + " \\) draws landed on the event of interest? Enter the count, and the observed " +
        "proportion is worked out for you.</i></p>";

      const OV = ov();
      $("observed-value-out").innerHTML =
        "$$\\begin{align*} \\text{OV} &= \\frac{x}{n} \\\\ &= \\frac{" + state.count + "}{" + state.n + "} \\\\ &= " +
          S.roundStr(OV, 5) + "\\end{align*}$$";

      // EV / SE. sigma is fixed by the null hypothesis, not estimated.
      $("ev-se-out").innerHTML =
        "<p>Because we built the box assuming the null hypothesis is true, the expected value of a draw is just the null proportion:</p>" +
        "$$\\begin{align*} \\text{EV} &= p \\\\ &= " + EVStr + "\\end{align*}$$" +
        "<p>The tickets are only \"1\"s and \"0\"s, so once we have fixed the proportion of \"1\"s at \\( p = " + p0s + " \\), the spread of the box " +
        "is fixed too. We do not estimate it from our sample — the null hypothesis hands it to us:</p>" +
        "$$\\begin{align*} \\sigma &= \\sqrt{p(1-p)} \\\\ &= \\sqrt{" + p0s + " \\times " + S.formatR(S.roundR(1 - state.nullProp, 10)) +
          "} \\\\ &= " + S.roundStr(sd, 5) + "\\end{align*}$$" +
        "<p>The standard error then scales that spread down by the number of draws:</p>" +
        "$$\\begin{align*} \\text{SE} &= \\frac{\\sigma}{\\sqrt{n}} \\\\ &= \\frac{" + S.roundStr(sd, 5) +
          "}{\\sqrt{" + state.n + "}}\\\\ &= " + SEStr + "\\end{align*}$$";

      // test statistic
      const tsNum = (OV - Number(EVStr)) / Number(SEStr);
      state.tsStr = S.roundStr(tsNum, 4);
      $("ts-out").innerHTML =
        "$$\\begin{align*} \\text{TS} &= \\frac{\\text{OV} - \\text{EV}}{\\text{SE}} \\\\ &= \\frac{" +
          S.roundStr(OV, 5) + " - " + EVStr + "}{" + SEStr + "} \\\\ &= " + state.tsStr + "\\end{align*}$$" +
        "<p style='text-align: left;'><span style='color: blue;'><i>The value for the test-statistic is " + state.tsStr + ". </i></span></p>";

      // p-value (standard normal)
      const z = Number(state.tsStr);
      let pVal, second = "<p>The test statistic falls on a standard normal curve. ";
      if (state.alt === 1) {
        pVal = 2 * (1 - S.pnorm(Math.abs(z)));
        second += "As we are doing a two-sided alternate hypothesis, we are interested in finding the <b>area below " +
          S.formatR(-Math.abs(z)) + " and above " + S.formatR(Math.abs(z)) + ".</b></p>";
      } else if (state.alt === 2) {
        pVal = 1 - S.pnorm(z);
        second += "As we are doing a one-sided greater than alternate hypothesis, we are interested in finding the <b>area above " + state.tsStr + ".</b></p>";
      } else {
        pVal = S.pnorm(z);
        second += "As we are doing a one-sided less than alternate hypothesis, we are interested in finding the <b>area below " + state.tsStr + ".</b></p>";
      }
      state.pVal = pVal;
      $("p-value-prelude").innerHTML =
        "<p>The p-value is the probability of observing a test-statistic <b>more extreme than our test statistic of " + state.tsStr +
          "</b>, <b>assuming the null hypothesis is true</b>. That last part is essential: the p-value is calculated in a world where \\( p = " + p0s +
          " \\), and it measures how unusual our sample would be in that world. It is not the probability that the null hypothesis is true.</p>" +
        second + "<p style='font-size: 16px; text-align: center;'>\\( p = " + S.roundStr(pVal, 5) + " \\)</p>";
      $("normal-plot").innerHTML = Plots.shadedNormalCurveSVG(z, state.alt, { width: 560, height: 325 });

      renderConclusion();
      renderCI();
      // Typeset the whole document (no arg): MathJax's startup pass already
      // processed the outer container, so re-typesetting that same ancestor node
      // finds no math — a full-document pass reliably picks up the injected LaTeX.
      typeset();
    }

    function renderConclusion() {
      const p = state.pVal, alpha = state.alpha;
      if (p > alpha) {
        $("conclusion-out").innerHTML =
          "$$\\begin{align*} \\alpha &< p \\\\" + S.formatR(alpha) + " &< " + S.roundStr(p, 5) + "\\end{align*}$$" +
          "<span style='color: blue;'><p>As the p value is greater than our significance level, we <b>fail to reject the null hypothesis</b>.</p></span>";
      } else {
        $("conclusion-out").innerHTML =
          "$$\\begin{align*} \\alpha &\\geq p \\\\" + S.formatR(alpha) + " &\\geq " + S.roundStr(p, 5) + "\\end{align*}$$" +
          "<span style='color: blue;'><p>As the p value is less than or equal to our significance level, we <b>reject the null hypothesis</b>.</p></span>";
      }
    }

    // Wilson score confidence interval for the proportion.
    function wilson(pHat, n, z) {
      const denom = 1 + z * z / n;
      const margin = z * Math.sqrt(pHat * (1 - pHat) / n + z * z / (4 * n * n));
      return { lower: (pHat + z * z / (2 * n) - margin) / denom, upper: (pHat + z * z / (2 * n) + margin) / denom };
    }
    // One substituted Wilson bound, as LaTeX. `sign` is "-" for the lower bound
    // and "+" for the upper. Both bounds are identical apart from that sign, so
    // building them here keeps the three alternate-hypothesis branches short.
    function boundTeX(ph, zr, zr2, n, sign) {
      return "\\frac{" + ph + " + " + zr2 + "/(2*" + n + ") " + sign + " " + zr +
        " \\times \\sqrt{(" + ph + "*(1-" + ph + ")/" + n + ") + (" + zr2 + "/(4*" + n + "^2))}}" +
        "{1 + " + zr2 + "/" + n + "}";
    }

    function renderCI() {
      const pHat = ov(), n = state.n, p0 = state.nullProp;
      const alpha = state.alpha;
      $("conf-level-out").innerHTML =
        "<p style='font-size: 16px; text-align: center;'>\\( \\text{confidence level} = 1 - " + S.formatR(alpha) +
        " = " + S.roundStr(1 - alpha, 5) + " \\)</p>";
      let formula, sub, ans, concl;
      if (state.alt === 1) {
        const z = S.qnorm(1 - alpha / 2), ci = wilson(pHat, n, z), zr = S.roundR(z, 4), zr2 = S.roundR(zr * zr, 4);
        formula = "$$CI = \\left( \\frac{\\widehat{p} + z^2/(2n) - z \\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n}, \\frac{\\widehat{p} + z^2/(2n) + z \\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n} \\right)$$";
        // Stacked over two lines: side by side, the substituted bounds overflow
        // the card. The \left(...\right) sizes to the whole block, so it still
        // reads as a single interval.
        sub = "$$CI = \\left( \\begin{aligned} &" + boundTeX(S.roundR(pHat, 4), zr, zr2, n, "-") + ", \\\\[6pt] &" +
          boundTeX(S.roundR(pHat, 4), zr, zr2, n, "+") + " \\end{aligned} \\right)$$";
        ans = "$$CI = (" + S.roundR(ci.lower, 4) + ", " + S.roundR(ci.upper, 4) + ")$$";
        concl = (p0 < ci.lower || p0 > ci.upper)
          ? "As the null proportion is outside the confidence interval, we <b>reject the null hypothesis</b>."
          : "As the null proportion is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
      } else if (state.alt === 2) {
        const z = S.qnorm(1 - alpha), ci = wilson(pHat, n, z), zr = S.roundR(z, 4), zr2 = S.roundR(zr * zr, 4);
        formula = "$$CI = \\left( \\frac{\\widehat{p} + z^2/(2n) - z \\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n}, \\; 1 \\right]$$";
        sub = "$$CI = \\left( " + boundTeX(S.roundR(pHat, 4), zr, zr2, n, "-") + ", \\; 1 \\right]$$";
        ans = "$$CI = (" + S.roundR(ci.lower, 4) + ", \\; 1]$$";
        concl = (p0 < ci.lower)
          ? "As the null proportion is below the confidence interval, we <b>reject the null hypothesis</b>."
          : "As the null proportion is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
      } else {
        const z = S.qnorm(1 - alpha), ci = wilson(pHat, n, z), zr = S.roundR(z, 4), zr2 = S.roundR(zr * zr, 4);
        formula = "$$CI = \\left[ 0, \\; \\frac{\\widehat{p} + z^2/(2n) + z \\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n} \\right)$$";
        sub = "$$CI = \\left[ 0, \\; " + boundTeX(S.roundR(pHat, 4), zr, zr2, n, "+") + " \\right)$$";
        ans = "$$CI = [0, \\; " + S.roundR(ci.upper, 4) + ")$$";
        concl = (p0 > ci.upper)
          ? "As the null proportion is above the confidence interval, we <b>reject the null hypothesis</b>."
          : "As the null proportion is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
      }
      $("ci-out").innerHTML = formula + sub + ans + "<span style='color: blue;'><p>" + concl + "</p></span>";
    }

    // 10,000-sample empirical distribution with the overlaid normal curve.
    function renderEmpirical() {
      const p0 = state.nullProp, n = state.n;
      const { EV, SE } = evse();
      const data = new Array(10000);
      let sMin = Infinity, sMax = -Infinity;
      for (let i = 0; i < 10000; i++) {
        let s = 0;
        for (let k = 0; k < n; k++) s += Math.random() < p0 ? 1 : 0;
        if (s < sMin) sMin = s;
        if (s > sMax) sMax = s;
        data[i] = s / n;
      }
      // A sample mean of 0/1 tickets can only land on the lattice s/n, so the bin
      // edges have to sit midway between attainable values and every bin must hold
      // the SAME number of them. Equal-width bins across the observed range give a
      // width that is not a multiple of 1/n -- at n = 50 it was 0.024 against a
      // spacing of 0.02, so every fifth bin swallowed two lattice points and the
      // rest one. That comb pattern made a perfectly bell-shaped distribution look
      // bimodal, which is the opposite of what this plot is here to show.
      const step = Math.max(1, Math.ceil((sMax - sMin + 1) / 40));
      const breaks = [];
      for (let s = sMin; s <= sMax + step; s += step) breaks.push((s - 0.5) / n);
      $("empirical-hist").innerHTML = Plots.densityHistogramSVG(data, {
        width: 500, height: 300,
        main: "Empirical Distribution of 10,000 Sample Means with\nOverlaid Normal Curve",
        xlab: "Sample Mean Value",
        ylab: "Density", col: "lightgreen", breaks: breaks, curve: { ev: EV, se: SE }
      });
    }

    function renderAll() { renderStats(); renderEmpirical(); }

    // ---------- inputs ----------
    // Invalid entries keep the last good value rather than snapping back to the
    // page default, so a half-typed number does not silently change the maths.
    // p0 = 0 and p0 = 1 are rejected: they give a zero-variance box, so SE = 0
    // and the test statistic is undefined.
    $("null-prop").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v <= 0 || v >= 1) { $("null-prop-warning").classList.remove("d-none"); return; }
      state.nullProp = v; $("null-prop-warning").classList.add("d-none");
      renderAll();
    });
    // n must be a whole number >= 1. n = 0 gave SE = Inf and a "NA" p-value.
    $("number-of-draws").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v < 1 || !Number.isInteger(v)) { $("n-warning").classList.remove("d-none"); return; }
      state.n = v; $("n-warning").classList.add("d-none");
      // A smaller n can leave the observed count above the new sample size.
      if (state.count > state.n) { state.count = state.n; $("observed-count").value = String(state.count); }
      renderAll();
    });
    $("observed-count").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v < 0 || v > state.n || !Number.isInteger(v)) {
        $("observed-count-warning").classList.remove("d-none"); return;
      }
      state.count = v; $("observed-count-warning").classList.add("d-none");
      renderStats();
    });
    for (const r of document.querySelectorAll('input[name="alternate_hypothesis_choice"]')) {
      r.addEventListener("change", function () { state.alt = Number(this.value); renderStats(); });
    }
    // alpha drives both the p-value conclusion and the confidence level, so a
    // change has to re-render the CI as well.
    $("alpha-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v <= 0 || v >= 1) { $("alpha-warning").classList.remove("d-none"); return; }
      state.alpha = v; $("alpha-warning").classList.add("d-none");
      renderConclusion(); renderCI();
      typeset($("conclusion-out")); typeset($("ci-out")); typeset($("conf-level-out"));
    });

    renderAll();
  });

})();
