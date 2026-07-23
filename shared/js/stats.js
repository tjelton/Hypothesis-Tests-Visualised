// stats.js -- numerical routines for the 1-sample t-test lesson.
//
// These reimplement the R functions the Shiny version relies on (pt, qt, dt,
// qnorm, sd, quantile type 7, fivenum, ppoints, round). Accuracy of the
// distribution tails/quantiles is verified against R in tests/stats.test.mjs,
// since this is a teaching tool and the displayed p-values/CIs must match R.

"use strict";

const Stats = (() => {

  // ---------- log-gamma (Lanczos, ~15 significant digits) ----------
  const LANCZOS_G = 7;
  const LANCZOS_C = [
    0.99999999999980993, 676.5203681218851, -1259.1392167224028,
    771.32342877765313, -176.61502916214059, 12.507343278686905,
    -0.13857109526572012, 9.9843695780195716e-6, 1.5056327351493116e-7
  ];

  function lgamma(x) {
    if (x < 0.5) {
      // Reflection formula
      return Math.log(Math.PI / Math.abs(Math.sin(Math.PI * x))) - lgamma(1 - x);
    }
    x -= 1;
    let a = LANCZOS_C[0];
    const t = x + LANCZOS_G + 0.5;
    for (let i = 1; i < LANCZOS_C.length; i++) a += LANCZOS_C[i] / (x + i);
    return 0.5 * Math.log(2 * Math.PI) + (x + 0.5) * Math.log(t) - t + Math.log(a);
  }

  // ---------- regularized incomplete beta I_x(a, b) ----------
  function betacf(a, b, x) {
    const MAXIT = 300, EPS = 3e-16, FPMIN = 1e-300;
    const qab = a + b, qap = a + 1, qam = a - 1;
    let c = 1, d = 1 - qab * x / qap;
    if (Math.abs(d) < FPMIN) d = FPMIN;
    d = 1 / d;
    let h = d;
    for (let m = 1; m <= MAXIT; m++) {
      const m2 = 2 * m;
      let aa = m * (b - m) * x / ((qam + m2) * (a + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d;
      h *= d * c;
      aa = -(a + m) * (qab + m) * x / ((a + m2) * (qap + m2));
      d = 1 + aa * d; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = 1 + aa / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d;
      const del = d * c;
      h *= del;
      if (Math.abs(del - 1) < EPS) break;
    }
    return h;
  }

  function pbeta(x, a, b) {
    if (x <= 0) return 0;
    if (x >= 1) return 1;
    const bt = Math.exp(lgamma(a + b) - lgamma(a) - lgamma(b) +
                        a * Math.log(x) + b * Math.log(1 - x));
    if (x < (a + 1) / (a + b + 2)) return bt * betacf(a, b, x) / a;
    return 1 - bt * betacf(b, a, 1 - x) / b;
  }

  // ---------- incomplete gamma (for erfc, used by qnorm refinement) ----------
  function gser(a, x) {
    const ITMAX = 300, EPS = 3e-16;
    let ap = a, sum = 1 / a, del = sum;
    for (let n = 1; n <= ITMAX; n++) {
      ap += 1;
      del *= x / ap;
      sum += del;
      if (Math.abs(del) < Math.abs(sum) * EPS) break;
    }
    return sum * Math.exp(-x + a * Math.log(x) - lgamma(a));
  }

  function gcf(a, x) {
    const ITMAX = 300, EPS = 3e-16, FPMIN = 1e-300;
    let b = x + 1 - a, c = 1 / FPMIN, d = 1 / b, h = d;
    for (let i = 1; i <= ITMAX; i++) {
      const an = -i * (i - a);
      b += 2;
      d = an * d + b; if (Math.abs(d) < FPMIN) d = FPMIN;
      c = b + an / c; if (Math.abs(c) < FPMIN) c = FPMIN;
      d = 1 / d;
      const del = d * c;
      h *= del;
      if (Math.abs(del - 1) < EPS) break;
    }
    return Math.exp(-x + a * Math.log(x) - lgamma(a)) * h;
  }

  // Q(a, x) = 1 - P(a, x)
  function gammq(a, x) {
    if (x < 0) return NaN;
    if (x === 0) return 1;
    return (x < a + 1) ? 1 - gser(a, x) : gcf(a, x);
  }

  function erfc(x) {
    return x >= 0 ? gammq(0.5, x * x) : 2 - gammq(0.5, x * x);
  }

  // ---------- normal distribution ----------
  function pnorm(x) {
    return 0.5 * erfc(-x / Math.SQRT2);
  }

  // Standard normal density (R's dnorm(x, mean = 0, sd = 1)). Exact closed
  // form; used only to draw the normal reference curve in the t-curve lesson.
  function dnorm(x) {
    return Math.exp(-x * x / 2) / Math.sqrt(2 * Math.PI);
  }

  // Acklam's inverse-normal algorithm with one Halley refinement step
  // (brings the ~1e-9 relative error of the raw rational approximation down
  // to essentially machine precision).
  function qnorm(p) {
    if (p <= 0) return -Infinity;
    if (p >= 1) return Infinity;
    const a = [-3.969683028665376e+01, 2.209460984245205e+02, -2.759285104469687e+02,
               1.383577518672690e+02, -3.066479806614716e+01, 2.506628277459239e+00];
    const b = [-5.447609879822406e+01, 1.615858368580409e+02, -1.556989798598866e+02,
               6.680131188771972e+01, -1.328068155288572e+01];
    const c = [-7.784894002430293e-03, -3.223964580411365e-01, -2.400758277161838e+00,
               -2.549732539343734e+00, 4.374664141464968e+00, 2.938163982698783e+00];
    const d = [7.784695709041462e-03, 3.224671290700398e-01, 2.445134137142996e+00,
               3.754408661907416e+00];
    const pLow = 0.02425, pHigh = 1 - pLow;
    let x;
    if (p < pLow) {
      const q = Math.sqrt(-2 * Math.log(p));
      x = (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) /
          ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
    } else if (p <= pHigh) {
      const q = p - 0.5, r = q * q;
      x = (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q /
          (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1);
    } else {
      const q = Math.sqrt(-2 * Math.log(1 - p));
      x = -(((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) /
           ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
    }
    // Halley refinement
    const e = pnorm(x) - p;
    const u = e * Math.sqrt(2 * Math.PI) * Math.exp(x * x / 2);
    x = x - u / (1 + x * u / 2);
    return x;
  }

  // ---------- t distribution ----------
  function dt(x, df) {
    const norm = Math.exp(lgamma((df + 1) / 2) - lgamma(df / 2)) /
                 Math.sqrt(df * Math.PI);
    return norm * Math.pow(1 + x * x / df, -(df + 1) / 2);
  }

  function pt(t, df) {
    if (!isFinite(t)) return t > 0 ? 1 : 0;
    const x = df / (df + t * t);
    const p = 0.5 * pbeta(x, df / 2, 0.5);
    return t <= 0 ? p : 1 - p;
  }

  function qt(p, df) {
    if (p <= 0) return -Infinity;
    if (p >= 1) return Infinity;
    if (p === 0.5) return 0;
    // Exact closed forms for df = 1, 2 as starting points/answers
    if (df === 1) return refineQt(Math.tan(Math.PI * (p - 0.5)), p, df);
    if (df === 2) return refineQt((2 * p - 1) / Math.sqrt(2 * p * (1 - p)), p, df);
    // Cornish-Fisher expansion around the normal quantile
    const z = qnorm(p);
    const g1 = (Math.pow(z, 3) + z) / 4;
    const g2 = (5 * Math.pow(z, 5) + 16 * Math.pow(z, 3) + 3 * z) / 96;
    const g3 = (3 * Math.pow(z, 7) + 19 * Math.pow(z, 5) + 17 * Math.pow(z, 3) - 15 * z) / 384;
    const g4 = (79 * Math.pow(z, 9) + 776 * Math.pow(z, 7) + 1482 * Math.pow(z, 5) -
                1920 * Math.pow(z, 3) - 945 * z) / 92160;
    const guess = z + g1 / df + g2 / (df * df) + g3 / Math.pow(df, 3) + g4 / Math.pow(df, 4);
    return refineQt(guess, p, df);
  }

  // Bracket the root of pt(t) = p around `guess`, bisect, then polish with
  // Newton. Bulletproof (pt is monotone) and converges to machine precision.
  function refineQt(guess, p, df) {
    let lo = guess, hi = guess;
    let step = Math.max(1, Math.abs(guess) * 0.5);
    while (pt(lo, df) > p) { lo -= step; step *= 2; }
    step = Math.max(1, Math.abs(guess) * 0.5);
    while (pt(hi, df) < p) { hi += step; step *= 2; }
    for (let i = 0; i < 200; i++) {
      const mid = 0.5 * (lo + hi);
      if (mid === lo || mid === hi) break;
      if (pt(mid, df) < p) lo = mid; else hi = mid;
      if (hi - lo < 1e-15 * Math.max(1, Math.abs(mid))) break;
    }
    let t = 0.5 * (lo + hi);
    for (let i = 0; i < 3; i++) {
      const density = dt(t, df);
      if (density <= 0 || !isFinite(density)) break;
      const tNew = t - (pt(t, df) - p) / density;
      if (!isFinite(tNew)) break;
      t = tNew;
    }
    return t;
  }

  // ---------- basic sample statistics ----------
  function mean(x) {
    let s = 0;
    for (const v of x) s += v;
    return s / x.length;
  }

  // Sample standard deviation (denominator n - 1), matching R's sd().
  function sd(x) {
    const n = x.length;
    if (n < 2) return NaN;
    const m = mean(x);
    let ss = 0;
    for (const v of x) ss += (v - m) * (v - m);
    return Math.sqrt(ss / (n - 1));
  }

  // Population standard deviation (denominator n). R has no built-in for this;
  // the box-model lessons define popsd(x) = sqrt(mean((x - mean(x))^2)).
  function popsd(x) {
    const n = x.length;
    const m = mean(x);
    let ss = 0;
    for (const v of x) ss += (v - m) * (v - m);
    return Math.sqrt(ss / n);
  }

  // Random normal draws (R's rnorm) via Box-Muller. Used only by the simulation
  // lessons, so it draws from Math.random (no seed fidelity to R is required).
  function rnorm(n, mean = 0, sd = 1) {
    const out = new Array(n);
    for (let i = 0; i < n; i++) {
      let u = 0, v = 0;
      while (u === 0) u = Math.random();
      while (v === 0) v = Math.random();
      out[i] = mean + sd * Math.sqrt(-2 * Math.log(u)) * Math.cos(2 * Math.PI * v);
    }
    return out;
  }

  // Simple linear regression y ~ x (ordinary least squares), matching R's
  // lm(y ~ x) / summary(): slope, intercept, residuals, residual standard
  // error s (= summary(model)$sigma), and SE of the slope. Used by the
  // regression t-test lesson. Assumes complete (non-missing) x/y pairs.
  function linreg(x, y) {
    const n = x.length;
    const mx = mean(x), my = mean(y);
    let sxx = 0, sxy = 0;
    for (let i = 0; i < n; i++) {
      const dx = x[i] - mx;
      sxx += dx * dx;
      sxy += dx * (y[i] - my);
    }
    const slope = sxy / sxx;
    const intercept = my - slope * mx;
    const fitted = x.map(xi => intercept + slope * xi);
    const residuals = y.map((yi, i) => yi - fitted[i]);
    const rss = residuals.reduce((a, r) => a + r * r, 0);
    const df = n - 2;
    const s = Math.sqrt(rss / df);         // residual standard error
    const seSlope = s / Math.sqrt(sxx);    // SE of the slope estimate
    return { n, mx, my, sxx, sxy, slope, intercept, fitted, residuals, rss, df, s, seSlope };
  }

  // R quantile() type 7 (the default) for a single probability.
  function quantileType7(x, p) {
    const sorted = x.slice().sort((a, b) => a - b);
    const n = sorted.length;
    const h = (n - 1) * p;
    const lo = Math.floor(h);
    const hi = Math.min(lo + 1, n - 1);
    return sorted[lo] + (h - lo) * (sorted[hi] - sorted[lo]);
  }

  // R fivenum(): min, lower hinge, median, upper hinge, max.
  function fivenum(x) {
    const sorted = x.slice().sort((a, b) => a - b);
    const n = sorted.length;
    const n4 = Math.floor((n + 3) / 2) / 2;
    const d = [1, n4, (n + 1) / 2, n + 1 - n4, n];
    return d.map(v =>
      0.5 * (sorted[Math.floor(v) - 1] + sorted[Math.ceil(v) - 1])
    );
  }

  // R ppoints(): probability points for QQ plots.
  function ppoints(n) {
    const a = n <= 10 ? 3 / 8 : 0.5;
    const out = new Array(n);
    for (let i = 1; i <= n; i++) out[i - 1] = (i - a) / (n + 1 - 2 * a);
    return out;
  }

  // ---------- R-compatible rounding / formatting ----------

  // R's round(): round half to even (Math.round rounds half up/towards +Inf).
  function roundR(x, digits = 0) {
    if (!isFinite(x)) return x;
    const sign = x < 0 ? -1 : 1;
    const m = Math.pow(10, digits);
    const y = Math.abs(x) * m;
    let r;
    const frac = y - Math.floor(y);
    // Only treat as a tie when the scaled value is exactly representable at .5
    if (Math.abs(frac - 0.5) < Number.EPSILON * Math.max(1, y)) {
      r = 2 * Math.round(y / 2); // half to even
    } else {
      r = Math.round(y);
    }
    return sign * r / m;
  }

  // Approximates R's as.character() for numbers: shortest representation, and
  // scientific notation ("1e-05") when it is strictly shorter than fixed.
  // NaN maps to "NA" like R's NA propagation through round()/as.character().
  function formatR(x) {
    if (typeof x !== "number" || Number.isNaN(x)) return "NA";
    if (!isFinite(x)) return x > 0 ? "Inf" : "-Inf";
    if (x === 0) return "0";

    // Fixed notation, up to 15 significant digits, trailing zeros trimmed.
    let fixed = x.toPrecision(15);
    if (fixed.includes("e") || fixed.includes("E")) {
      fixed = Number(fixed).toFixed(Math.max(0, 15 - Math.floor(Math.log10(Math.abs(x))) - 1));
    }
    if (fixed.includes(".")) fixed = fixed.replace(/0+$/, "").replace(/\.$/, "");

    // Scientific notation with 2-digit exponent, e.g. "1e-05", "1.5e-05".
    let sci = Number(Number(x).toPrecision(15)).toExponential();
    sci = sci.replace(/e([+-])(\d)$/, "e$10$2");

    return sci.length < fixed.length ? sci : fixed;
  }

  // Equivalent of R's as.character(round(x, digits)).
  function roundStr(x, digits) {
    return formatR(roundR(x, digits));
  }

  // R pretty(range, n): "nice" break points covering [lo, hi]. Simplified
  // version of R's algorithm (1/2/5 x 10^k units with R's default biases);
  // used for histogram breaks and axis ticks.
  function prettyBreaks(lo, hi, n) {
    if (lo === hi) { lo -= 0.5; hi += 0.5; }
    const cell = (hi - lo) / n;
    const base = Math.pow(10, Math.floor(Math.log10(cell)));
    const ratio = cell / base;
    let unit;
    if (ratio < 1.5) unit = base;
    else if (ratio < 3) unit = 2 * base;
    else if (ratio < 7) unit = 5 * base;
    else unit = 10 * base;
    const start = Math.floor(lo / unit + 1e-10) * unit;
    const breaks = [];
    for (let v = start; v < hi + unit * (1 - 1e-10); v += unit) {
      // Snap to a clean decimal to avoid 0.30000000000000004-style labels.
      breaks.push(Number(v.toFixed(12)));
    }
    return breaks;
  }

  return {
    lgamma, pbeta, pnorm, dnorm, qnorm, erfc,
    dt, pt, qt,
    mean, sd, popsd, rnorm, linreg, quantileType7, fivenum, ppoints,
    roundR, formatR, roundStr, prettyBreaks
  };
})();

if (typeof module !== "undefined" && module.exports) module.exports = Stats;
