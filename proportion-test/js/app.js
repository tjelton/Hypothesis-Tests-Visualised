// app.js -- interactive wiring for the proportion test (z-test). Ports
// R/ztest_proportion_test_srv.R. Inputs are entered manually (null proportion
// p0, sample size n, observed value OV); there is no dataset. The box holds "1"
// and "0" tickets in the null proportion, and the test uses the standard normal
// distribution:
//   sum:  EV = n*p0,  SE = sqrt(n)*sqrt(p0(1-p0))
//   mean: EV = p0,    SE = sqrt(p0(1-p0))/sqrt(n)
//   z = (OV - EV) / SE;  p-value from pnorm;  CI is the Wilson score interval.
//
// Calculation parity notes (matching the Shiny code):
//   EV_string = as.character(round(EV, 5)); SE_string = as.character(round(SE, 5));
//   TS = as.character(round((OV - as.numeric(EV_string)) / as.numeric(SE_string), 4));
//   p-value from as.numeric(TS) via pnorm. The empirical-distribution histogram
//   is a random simulation (Math.random). Sample sizes are ceiling()'d.
//
// Note: the observed-value input is constrained to [0, 1] even in "sum" mode
// (where the observed sum could exceed 1). This is faithful to the R app, which
// applies the same 0-1 bound; in sum mode the test statistic is therefore only
// meaningful for small EV.

"use strict";

(function () {

  const $ = id => document.getElementById(id);
  const S = Stats;

  function typeset(el) {
    if (window.MathJax && window.MathJax.typesetPromise) window.MathJax.typesetPromise(el ? [el] : undefined).catch(() => {});
  }
  function gcd(a, b) { a = Math.round(a); b = Math.round(b); while (b) { [a, b] = [b, a % b]; } return a; }

  const state = { nullProp: 0.7, n: 30, mode: "2", ov: 0.73, alt: 1, alpha: 0.05, conf: 0.95, tsStr: "", pVal: 0 };

  function modeWord() { return state.mode === "2" ? "mean" : "sum"; }

  // EV/SE for the current p0, n and sum/mean choice.
  function evse() {
    const p0 = state.nullProp, n = state.n;
    const sd = Math.sqrt(p0 * (1 - p0));
    if (state.mode === "2") return { EV: p0, SE: sd / Math.sqrt(n), sd };
    return { EV: n * p0, SE: Math.sqrt(n) * sd, sd };
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
    $("intro-example-box-model").innerHTML = boxModelHTML("1, 1, 1, 1, 1, 1, 1, 0, 0, 0", "Data Science Class", "n = 30");

    function renderBoxAndHyp() {
      const sampleLabel = state.mode === "2" ? "Sample Mean" : "Sample Sum";
      $("box-model").innerHTML = boxModelHTML(ticketsString(), sampleLabel, "n = " + state.n);
      const p0s = S.formatR(state.nullProp);
      $("null-hyp-out").innerHTML = "<center><p style='font-size: 16px;'>\\( H_0: \\) \\( p = " + p0s + " \\)</p></center>";
      const sign = state.alt === 1 ? "\\neq" : (state.alt === 2 ? ">" : "<");
      $("alt-hyp-out").innerHTML = "<center><p style='font-size: 16px;'>\\( H_1: \\) \\( p " + sign + " " + p0s + " \\)</p></center>";
    }

    function renderStats() {
      renderBoxAndHyp();
      const w = modeWord();
      const { EV, SE, sd } = evse();
      const EVStr = S.roundStr(EV, 5), SEStr = S.roundStr(SE, 5);

      // assumption 3 + observed-value prompt text depend on sum/mean
      $("assump3-text").innerHTML =
        "<p>The third assumption is that the sample " + w + "s follow an approximate normal distribution.</p>" +
        "<p><span style='color: blue;'><b>How do we check?</b></span><br></p>" +
        "<ul><li>Recall that the central limit theorem tells us that if we take a sufficiently large number of draws from the box, " +
        "then the sample " + w + "s will approximately follow a normal distribution. <i>If confused, please see the box model exercise</i>.</li>" +
        "<li>One way we can easily tell if the central limit theorem applies is to sample taking many draws from the box, and seeing whether the values appear normally distributed.</li>" +
        "<li>The plot to the left shows the distribution of 10000 simulated samples.</li>" +
        "<li>Additionally, if the distribution of tickets is symmetric and/or normally distributed, you will need to take fewer draws from the box for the sample " +
        w + "s to be normally distributed (i.e., a smaller n is needed for the central limit theorem to apply).</li></ul>";
      $("observed-value-text").innerHTML = "<p><i>What is the observed " + w + " that you saw from your sample?</i></p>";

      // EV / SE
      let evseHtml;
      if (state.mode === "2") {
        evseHtml =
          "<p>Expected Value:</p>$$\\begin{align*} \\text{EV} &= \\mu \\\\ &=" + EVStr + "\\end{align*}$$" +
          "<p>Standard Error:</p>$$\\begin{align*} \\text{SE} &= \\frac{\\sigma}{\\sqrt{n}} \\\\ &= \\frac{" + S.roundStr(sd, 5) +
            "}{\\sqrt{" + state.n + "}}\\\\ &= " + SEStr + "\\end{align*}$$";
      } else {
        evseHtml =
          "<p>Expected Value:</p>$$\\begin{align*} \\text{EV} &= n \\times \\mu \\\\ &=" + state.n + "\\times" + S.roundStr(state.nullProp, 5) + "\\\\ &= " + EVStr + "\\end{align*}$$" +
          "<p>Standard Error:</p>$$\\begin{align*} \\text{SE} &= \\sqrt{n} \\times \\sigma \\\\ &= \\sqrt{" + state.n + "} \\times" + S.roundStr(sd, 5) + "\\\\ &= " + SEStr + "\\end{align*}$$";
      }
      $("ev-se-out").innerHTML = evseHtml;

      // test statistic
      const tsNum = (state.ov - Number(EVStr)) / Number(SEStr);
      state.tsStr = S.roundStr(tsNum, 4);
      $("ts-out").innerHTML =
        "$$\\begin{align*} \\text{TS} &= \\frac{\\text{OV} - \\text{EV}}{\\text{SE}} \\\\ &= \\frac{" +
          S.roundStr(state.ov, 5) + " - " + EVStr + "}{" + SEStr + "} \\\\ &= " + state.tsStr + "\\end{align*}$$" +
        "<p style='text-align: left;'><span style='color: blue;'><i>The value for the test-statistic is " + state.tsStr + ". </i></span></p>";

      // p-value (standard normal)
      const z = Number(state.tsStr);
      let pVal, second = "<p>The test statistics fall on a standard normal curve. ";
      if (state.alt === 1) {
        pVal = 2 * (1 - S.pnorm(Math.abs(z)));
        second += "As we are doing a two-sided alternate hypothesis, we are interested in finding the <b>area below " +
          S.formatR(-Math.abs(z)) + " and above " + S.formatR(Math.abs(z)) + ".</p></b>";
      } else if (state.alt === 2) {
        pVal = 1 - S.pnorm(z);
        second += "As we are doing a one-sided greater than alternate hypothesis, we are interested in finding the <b>area above " + state.tsStr + ".</p></b>";
      } else {
        pVal = S.pnorm(z);
        second += "As we are doing a one-sided less than alternate hypothesis, we are interested in finding the <b>area below " + state.tsStr + ".</p></b>";
      }
      state.pVal = pVal;
      $("p-value-prelude").innerHTML =
        "<p>The p-value is the probability of observing a test-statistic <b>more extreme that our test statistic of " + state.tsStr + ".</b></p>" +
        second + "<p style='font-size: 16px; text-align: center;'>\\( p = " + S.roundStr(pVal, 5) + " \\)</p>";
      $("normal-plot").innerHTML = Plots.shadedNormalCurveSVG(z, state.alt, { width: 560, height: 325 });

      renderConclusion();
      renderCI();
      typeset(document.querySelector(".container-fluid"));
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

    // Wilson score confidence interval for the proportion.
    function wilson(pHat, n, z) {
      const denom = 1 + z * z / n;
      const margin = z * Math.sqrt(pHat * (1 - pHat) / n + z * z / (4 * n * n));
      return { lower: (pHat + z * z / (2 * n) - margin) / denom, upper: (pHat + z * z / (2 * n) + margin) / denom };
    }
    function renderCI() {
      const pHat = state.ov, n = state.n, p0 = state.nullProp;
      const alpha = 1 - state.conf;
      let formula, sub, ans, concl;
      if (state.alt === 1) {
        const z = S.qnorm(1 - alpha / 2), ci = wilson(pHat, n, z), zr = S.roundR(z, 4), zr2 = zr * zr;
        formula = "$$CI = \\left( \\frac{\\widehat{p} + z^2/(2n) - z \\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n}, \\frac{\\widehat{p} + z^2/(2n) + z \\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n} \\right)$$";
        sub = "$$CI = \\left( \\frac{" + S.roundR(pHat, 4) + " + " + zr2 + "/(2*" + n + ") - " + zr + " \\times \\sqrt{(" + S.roundR(pHat, 4) + "*(1-" + S.roundR(pHat, 4) + ")/" + n + ") + (" + zr2 + "/(4*" + n + "^2))}}{1 + " + zr2 + "/" + n + "}, " +
          "\\frac{" + S.roundR(pHat, 4) + " + " + zr2 + "/(2*" + n + ") + " + zr + " \\times \\sqrt{(" + S.roundR(pHat, 4) + "*(1-" + S.roundR(pHat, 4) + ")/" + n + ") + (" + zr2 + "/(4*" + n + "^2))}}{1 + " + zr2 + "/" + n + "} \\right)$$";
        ans = "$$CI = (" + S.roundR(ci.lower, 4) + ", " + S.roundR(ci.upper, 4) + ")$$";
        concl = (p0 < ci.lower || p0 > ci.upper)
          ? "As the null proportion is outside the confidence interval, we <b>reject the null hypothesis</b>."
          : "As the null proportion is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
      } else if (state.alt === 2) {
        const z = S.qnorm(1 - alpha), ci = wilson(pHat, n, z), zr = S.roundR(z, 4), zr2 = zr * zr;
        formula = "$$CI = \\left( \\frac{\\widehat{p} + z^2/(2n) - z \\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n}, \\infty \\right)$$";
        sub = "$$CI = \\left( \\frac{" + S.roundR(pHat, 4) + " + " + zr2 + "/(2*" + n + ") - " + zr + " \\times \\sqrt{" + S.roundR(pHat, 4) + "*(1-" + S.roundR(pHat, 4) + ")/" + n + " + " + zr2 + "/(4*" + n + "^2)}}{1 + " + zr2 + "/" + n + "}, \\infty \\right)$$";
        ans = "$$CI = (" + S.roundR(ci.lower, 4) + ", \\infty)$$";
        concl = (p0 < ci.lower)
          ? "As the null proportion is below the confidence interval, we <b>reject the null hypothesis</b>."
          : "As the null proportion is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
      } else {
        const z = S.qnorm(1 - alpha), ci = wilson(pHat, n, z), zr = S.roundR(z, 4), zr2 = zr * zr;
        formula = "$$CI = \\left( -\\infty, \\frac{\\widehat{p} + z^2/(2n) + z \\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n} \\right)$$";
        sub = "$$CI = \\left(-\\infty, \\frac{" + S.roundR(pHat, 4) + " + " + zr2 + "/(2*" + n + ") + " + zr + " \\times \\sqrt{(" + S.roundR(pHat, 4) + "*(1-" + S.roundR(pHat, 4) + ")/" + n + ") + (" + zr2 + "/(4*" + n + "^2))}}{1 + " + zr2 + "/" + n + "}\\right)$$";
        ans = "$$CI = (-\\infty, " + S.roundR(ci.upper, 4) + ")$$";
        concl = (p0 > ci.upper)
          ? "As the null proportion is above the confidence interval, we <b>reject the null hypothesis</b>."
          : "As the null proportion is inside the confidence interval, we <b>fail to reject the null hypothesis</b>.";
      }
      $("ci-out").innerHTML = formula + sub + ans + "<span style='color: blue;'><p>" + concl + "</p></span>";
    }

    // 10,000-sample empirical distribution with the overlaid normal curve.
    function renderEmpirical() {
      const p0 = state.nullProp, n = state.n, isMean = state.mode === "2";
      const { EV, SE } = evse();
      const data = new Array(10000);
      for (let i = 0; i < 10000; i++) {
        let s = 0;
        for (let k = 0; k < n; k++) s += Math.random() < p0 ? 1 : 0;
        data[i] = isMean ? s / n : s;
      }
      const bins = Math.min(new Set(data).size, 20);
      const title = "Empirical Distribution of 10000 Sample " + (isMean ? "Means" : "Sums") + " with\nOverlaid Normal Curve";
      $("empirical-hist").innerHTML = Plots.densityHistogramSVG(data, {
        width: 500, height: 300, main: title, xlab: isMean ? "Sample Mean Value" : "Sample Sum Value",
        ylab: "Density", col: "lightgreen", breaks: Math.max(1, bins), curve: { ev: EV, se: SE }
      });
    }

    function renderAll() { renderStats(); renderEmpirical(); }

    // ---------- inputs ----------
    $("null-prop").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v < 0 || v > 1) { state.nullProp = 0.7; $("null-prop-warning").classList.remove("d-none"); }
      else { state.nullProp = v; $("null-prop-warning").classList.add("d-none"); }
      renderAll();
    });
    $("number-of-draws").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v < 0) { state.n = 25; $("n-warning").classList.remove("d-none"); }
      else { state.n = Math.ceil(v); $("n-warning").classList.add("d-none"); }
      renderAll();
    });
    for (const r of document.querySelectorAll('input[name="sum_or_mean"]')) {
      r.addEventListener("change", function () { state.mode = this.value; renderAll(); });
    }
    $("observed-value").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v < 0 || v > 1) { state.ov = 0.73; $("observed-val-warning").classList.remove("d-none"); }
      else { state.ov = v; $("observed-val-warning").classList.add("d-none"); }
      renderStats();
    });
    for (const r of document.querySelectorAll('input[name="alternate_hypothesis_choice"]')) {
      r.addEventListener("change", function () { state.alt = Number(this.value); renderStats(); });
    }
    $("alpha-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v < 0 || v > 1) { state.alpha = 0.05; $("alpha-warning").classList.remove("d-none"); }
      else { state.alpha = v; $("alpha-warning").classList.add("d-none"); }
      renderConclusion(); typeset($("conclusion-out"));
    });
    $("conf-input").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (Number.isNaN(v) || v <= 0 || v >= 1) { state.conf = 0.95; $("conf-warning").classList.remove("d-none"); }
      else { state.conf = v; $("conf-warning").classList.add("d-none"); }
      renderCI(); typeset($("ci-out"));
    });

    renderAll();
  });

})();
