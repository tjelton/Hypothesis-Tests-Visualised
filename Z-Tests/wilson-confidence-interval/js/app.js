// app.js -- interactive wiring for "Misc: Understanding the Wilson's
// Confidence Interval". This page is NOT a hypothesis test: there is no null
// hypothesis, no test statistic and no p-value. It does one thing -- show where
// the two endpoints of a Wilson confidence interval come from.
//
// The idea being visualised. Pick a candidate proportion p. If p really were
// the population proportion, then a C% prediction interval for the sample
// proportion would be
//
//   p +/- z * sqrt(p(1-p)/n),        z = qnorm(1 - alpha/2),  alpha = 1 - C/100
//
// Note the SD uses the CANDIDATE p, not p-hat -- that is exactly what separates
// Wilson from Wald, and it is why the interval comes out asymmetric about p-hat.
// The Wilson interval is the set of candidates whose prediction interval still
// contains the observed p-hat, so its endpoints are the two candidates whose
// prediction interval just BARELY reaches p-hat:
//
//   lower endpoint: p + z*sqrt(p(1-p)/n) = p-hat   (upper limit lands on p-hat)
//   upper endpoint: p - z*sqrt(p(1-p)/n) = p-hat   (lower limit lands on p-hat)
//
// Squaring either equation gives the same quadratic (p-hat - p)^2 = z^2p(1-p)/n,
// whose two roots are the closed-form Wilson bounds. The student finds those two
// candidates by hand with the sliders; the closed form is then revealed as the
// algebra that skips the searching.

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

  // Sliders start at their outer ends, so both begin red and the answer is
  // never showing when the page loads or the inputs change.
  const LEFT_START = 0, RIGHT_START = 10;
  const state = { x: 9, n: 30, conf: 95, leftTick: LEFT_START, rightTick: RIGHT_START };

  // An endpoint counts as found when its prediction limit sits on p-hat. At
  // tick 5 the candidate IS the closed-form root, so the two agree to within
  // floating-point noise; nothing else on either slider comes close.
  const TOL = 1e-9;

  const pHat = () => state.x / state.n;
  const alpha = () => 1 - state.conf / 100;
  const zCrit = () => S.qnorm(1 - alpha() / 2);
  const se = p => Math.sqrt(p * (1 - p) / state.n);

  // Closed-form Wilson bounds: the two roots of (p-hat - p)^2 = z^2 p(1-p)/n.
  function wilson(p, n, z) {
    const denom = 1 + z * z / n;
    const margin = z * Math.sqrt(p * (1 - p) / n + z * z / (4 * n * n));
    return { lower: (p + z * z / (2 * n) - margin) / denom, upper: (p + z * z / (2 * n) + margin) / denom };
  }

  // Slider geometry. Each slider is split at tick 5 so tick 5 is the exact
  // endpoint whatever the ends are: ticks 0-5 interpolate from `a` to the
  // endpoint, ticks 5-10 from the endpoint to `b`.
  //
  // The outer end is the endpoint mirrored through p-hat. For a p-hat near 0 or
  // 1 that mirror falls outside [0, 1], so it is pulled back to halfway between
  // the endpoint and the boundary -- the outer end must stay a real proportion
  // with a positive SD, since at exactly 0 or 1 there is no curve to draw.
  function geometry() {
    const ph = pHat(), n = state.n, z = zCrit();
    const ci = wilson(ph, n, z);
    const leftFar = Math.max(ci.lower - (ph - ci.lower), ci.lower / 2);
    const rightFar = Math.min(ci.upper + (ci.upper - ph), 1 - (1 - ci.upper) / 2);
    return {
      ci, z, ph,
      left: { a: leftFar, mid: ci.lower, b: ph },
      right: { a: ph, mid: ci.upper, b: rightFar }
    };
  }

  function tickValue(g, t) {
    return t <= 5 ? g.a + (g.mid - g.a) * t / 5
                  : g.mid + (g.b - g.mid) * (t - 5) / 5;
  }

  // One candidate's prediction interval, plus its traffic-light state. `side`
  // says which limit is the one being aimed at p-hat.
  function curveFor(p, side) {
    const z = zCrit(), s = se(p);
    const lo = p - z * s, hi = p + z * s;
    const aimed = side === "left" ? hi : lo;
    let st;
    if (Math.abs(aimed - pHat()) <= TOL) st = "green";
    else if (lo <= pHat() && pHat() <= hi) st = "yellow";
    else st = "red";
    return { p, se: s, lo, hi, state: st, side };
  }

  // ---------- input validation ----------
  // x must be strictly inside (0, n): at x = 0 or x = n the observed proportion
  // sits on a boundary, one endpoint collapses onto it and that side's curve has
  // no spread, so there is nothing left to line up.
  function readInputs() {
    const nRaw = Number($("n-input").value);
    const xRaw = Number($("x-input").value);
    const cRaw = Number($("conf-input").value);
    let ok = true;

    const nBad = !Number.isInteger(nRaw) || nRaw < 2;
    $("n-warning").classList.toggle("d-none", !nBad);
    if (nBad) ok = false; else state.n = nRaw;

    const xBad = !Number.isInteger(xRaw) || xRaw <= 0 || xRaw >= state.n;
    $("x-warning").classList.toggle("d-none", !xBad);
    if (xBad) ok = false; else state.x = xRaw;

    const cBad = !isFinite(cRaw) || cRaw < 1 || cRaw > 99;
    $("conf-warning").classList.toggle("d-none", !cBad);
    if (cBad) ok = false; else state.conf = cRaw;

    if (!ok) {
      $("n-input").value = state.n;
      $("x-input").value = state.x;
      $("conf-input").value = state.conf;
    }
  }

  // ---------- rendering ----------
  const LABEL = {
    green: "GREEN — perfectly aligned",
    yellow: "YELLOW — covers the star, but not on its end",
    red: "RED — misses the star entirely"
  };
  const BADGE = { green: "success", yellow: "warning", red: "danger" };

  function statusHTML(c) {
    const limit = c.side === "left" ? "upper" : "lower";
    let msg;
    if (c.state === "green") {
      msg = "The " + limit + " end of this prediction interval lands exactly on the star. " +
            "This candidate is an endpoint of the confidence interval.";
    } else if (c.state === "yellow") {
      msg = "The star is inside this prediction interval, but not at its " + limit +
            " end — this candidate is still plausible, so it is inside the interval rather than on its edge.";
    } else {
      msg = "This prediction interval does not reach the star at all: if this were the true proportion, " +
            "the observed result would be a surprise. This candidate is outside the confidence interval.";
    }
    const badgeCls = c.state === "yellow" ? "bg-warning text-dark" : "bg-" + BADGE[c.state];
    return '<span class="badge ' + badgeCls + '">' + LABEL[c.state] + "</span>" +
      "<p class='mb-1 mt-2' style='font-size:15px;'>Candidate \\( p = " + S.roundStr(c.p, 4) + " \\)<br>" +
      "Prediction interval \\( = " + S.roundStr(c.p, 4) + " \\pm " + S.roundStr(zCrit(), 4) +
      " \\times " + S.roundStr(c.se, 4) + " = (" + S.roundStr(c.lo, 4) + ", " + S.roundStr(c.hi, 4) + ") \\)</p>" +
      "<p class='mb-0' style='font-size:15px;'>" + msg + "</p>";
  }

  function render() {
    const g = geometry();
    const ph = g.ph;
    const left = curveFor(tickValue(g.left, state.leftTick), "left");
    const right = curveFor(tickValue(g.right, state.rightTick), "right");

    // The axis and the vertical scale are computed over EVERY slider position,
    // not just the current one, so neither rescales while a slider is dragged --
    // a moving axis would make "is the end on the star?" impossible to judge.
    let xlo = ph, xhi = ph, peak = 0;
    for (const side of ["left", "right"]) {
      for (let t = 0; t <= 10; t++) {
        const c = curveFor(tickValue(g[side], t), side);
        xlo = Math.min(xlo, c.lo); xhi = Math.max(xhi, c.hi);
        if (c.se > 0) peak = Math.max(peak, S.dnorm(0) / c.se);
      }
    }
    const padded = 0.1 * (xhi - xlo);

    $("wilson-plot").innerHTML = Plots.wilsonPredictionSVG(ph, [left, right],
      [xlo - padded, xhi + padded], {
        main: "Prediction intervals for the sample proportion, at each candidate",
        xlab: "Proportion", ymax: peak, cssHeight: 420
      });

    $("left-status").innerHTML = statusHTML(left);
    $("right-status").innerHTML = statusHTML(right);
    $("p-hat-out").innerHTML = "\\( \\widehat{p} = " + state.x + "/" + state.n + " = " + S.roundStr(ph, 5) +
      " \\), \\( z = " + S.roundStr(g.z, 4) + " \\)";

    renderAnswer(left, right, g, [xlo - padded, xhi + padded], peak);
    typeset($("wilson-app"));
  }

  // The payoff box: only once BOTH endpoints have been found by hand does the
  // page show the interval. It opens with the two green curves and the finished
  // interval sketched beneath them, says why those endpoints are the edge, and
  // then offers two ways to get the same numbers without the searching.
  function renderAnswer(left, right, g, xdomain, ymax) {
    const box = $("answer-box");
    if (left.state !== "green" || right.state !== "green") {
      box.classList.add("d-none");
      box.innerHTML = "";
      return;
    }
    box.classList.remove("d-none");

    const n = state.n, ph = g.ph, z = g.z;
    const lo = S.roundStr(g.ci.lower, 4), hi = S.roundStr(g.ci.upper, 4);
    const zr = S.roundR(z, 4), zr2 = S.roundR(zr * zr, 4), phr = S.roundR(ph, 4);
    const bound = sign =>
      "\\frac{" + phr + " + " + zr2 + "/(2 \\times " + n + ") " + sign + " " + zr +
      " \\sqrt{" + phr + "(1-" + phr + ")/" + n + " + " + zr2 + "/(4 \\times " + n + "^2)}}" +
      "{1 + " + zr2 + "/" + n + "}";

    const sketch = Plots.wilsonPredictionSVG(ph, [left, right], xdomain, {
      main: "", xlab: "Proportion", ymax: ymax, cssHeight: 340,
      ciBar: { lower: g.ci.lower, upper: g.ci.upper }
    });

    // Method 2 quotes the numbers R prints, which is 7 significant digits.
    const conf = S.formatR(state.conf);
    const rCall = 'binom.confint(' + state.x + ', ' + n + ', conf.level = ' + S.formatR(state.conf / 100) + ', methods = "wilson")';

    box.innerHTML =
      '<div class="tight-card accent"><div class="tight-card-title">You found it — the ' + conf +
      '% Wilson confidence interval</div><div class="tight-card-body">' +

      '<div class="answer-sketch">' + sketch + "</div>" +

      "<p class='mt-2'>The interval runs from the centre of one green curve to the centre of the other:</p>" +
      "$$CI = (" + lo + ", \\; " + hi + ")$$" +
      "<p>Both curves stop <b>exactly</b> at the star — that is what made them green:</p>" +
      "<ul>" +
      "<li>the lower endpoint's prediction interval reaches <b>up</b> to \\( \\widehat{p} \\): \\( " +
      lo + " + " + S.roundStr(z, 4) + " \\times " + S.roundStr(left.se, 4) + " = " + S.roundStr(ph, 4) + " \\)</li>" +
      "<li>the upper endpoint's reaches <b>down</b> to \\( \\widehat{p} \\): \\( " +
      hi + " - " + S.roundStr(z, 4) + " \\times " + S.roundStr(right.se, 4) + " = " + S.roundStr(ph, 4) + " \\)</li>" +
      "</ul>" +
      "<p>Move either candidate any further from the star and its prediction interval stops reaching it. So these two are the " +
      "outermost proportions that still make your result plausible — the interval sits right on the edge of both prediction " +
      "intervals, which is exactly what a confidence interval is.</p>" +

      "<p class='mt-3 mb-2'><b>How you would work this out without the sliders</b></p>" +
      '<div class="accordion" id="answer-accordion">' +

      '<div class="accordion-item"><h2 class="accordion-header"><button class="accordion-button collapsed" type="button" ' +
      'data-bs-toggle="collapse" data-bs-target="#answer-acc-1"><b>Method 1 — in R</b></button></h2>' +
      '<div id="answer-acc-1" class="accordion-collapse collapse" data-bs-parent="#answer-accordion"><div class="accordion-body">' +
      "<p>The <code>binom</code> package computes it directly:</p>" +
      '<pre class="answer-code"><code>install.packages("binom")\nlibrary(binom)\n\n' + rCall + "</code></pre>" +
      "<p class='mb-0'>which returns <code>lower = " + S.roundStr(g.ci.lower, 7) + "</code> and <code>upper = " +
      S.roundStr(g.ci.upper, 7) + "</code>.</p>" +
      "</div></div></div>" +

      '<div class="accordion-item"><h2 class="accordion-header"><button class="accordion-button collapsed" type="button" ' +
      'data-bs-toggle="collapse" data-bs-target="#answer-acc-2"><b>Method 2 — solve the equation</b></button></h2>' +
      '<div id="answer-acc-2" class="accordion-collapse collapse" data-bs-parent="#answer-accordion"><div class="accordion-body">' +
      "<p>Solving that equation for \\( p \\) gives a closed form. In practice nobody evaluates this by hand, and in practice, " +
      "we use software libraries like in Method 1 to find the endpoints.</p>" +
      "$$CI = \\left( \\frac{\\widehat{p} + z^2/(2n) - z\\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n}, \\; " +
      "\\frac{\\widehat{p} + z^2/(2n) + z\\sqrt{\\widehat{p}(1-\\widehat{p})/n + z^2/(4n^2)}}{1 + z^2/n} \\right)$$" +
      "$$CI = \\left( \\begin{aligned} &" + bound("-") + ", \\\\[6pt] &" + bound("+") + " \\end{aligned} \\right)$$" +
      "$$CI = (" + lo + ", \\; " + hi + ")$$" +
      "</div></div></div>" +

      "</div></div></div>";
  }

  // ---------- wiring ----------
  function onInputChange() {
    readInputs();
    // Re-hide the answer: the endpoints move with the inputs, so the student
    // searches again rather than being handed the new ones.
    state.leftTick = LEFT_START;
    state.rightTick = RIGHT_START;
    $("left-slider").value = LEFT_START;
    $("right-slider").value = RIGHT_START;
    render();
  }

  for (const id of ["x-input", "n-input", "conf-input"]) {
    $(id).addEventListener("change", onInputChange);
  }
  $("left-slider").addEventListener("input", e => { state.leftTick = Number(e.target.value); render(); });
  $("right-slider").addEventListener("input", e => { state.rightTick = Number(e.target.value); render(); });

  render();

})();
