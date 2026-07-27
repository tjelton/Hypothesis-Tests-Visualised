// plots.js -- zero-dependency SVG re-implementations of the base-R plots the
// Shiny lesson draws: histogram, boxplot (boxplot.stats semantics), normal
// QQ plot with qqline, and the shaded t-curve from
// R/utility_generic_plotting_functions.R::curve_shaded_test_stat.

"use strict";

const Plots = (() => {

  const FONT = "font-family:sans-serif;";

  // Monotonic id source for per-SVG clipPath ids (several plots of the same
  // kind can appear on one page, and clip-path ids are document-global).
  let uid = 0;

  function esc(s) {
    return String(s).replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");
  }

  // Linear scale from data domain to pixel range.
  function scale(d0, d1, r0, r1) {
    const k = (r1 - r0) / (d1 - d0);
    return v => r0 + (v - d0) * k;
  }

  // R-style 4% padding on both ends of a data range (xaxs = "r").
  function padRange(lo, hi) {
    if (lo === hi) { lo -= 0.5; hi += 0.5; }
    const pad = 0.04 * (hi - lo);
    return [lo - pad, hi + pad];
  }

  function tickValues(lo, hi) {
    return Stats.prettyBreaks(lo, hi, 5).filter(v => v >= lo && v <= hi);
  }

  function fmtTick(v) {
    return Stats.formatR(Number(v.toFixed(10)));
  }

  // Shared frame: margins, axes, title, axis labels. Returns helpers.
  function makeFrame(width, height, opts) {
    const m = Object.assign({ top: 34, right: 14, bottom: 44, left: 52 }, opts.margins || {});
    const [xlo, xhi] = padRange(opts.xdomain[0], opts.xdomain[1]);
    const [ylo, yhi] = opts.padY === false
      ? [opts.ydomain[0], opts.ydomain[1]]
      : padRange(opts.ydomain[0], opts.ydomain[1]);
    const sx = scale(xlo, xhi, m.left, width - m.right);
    const sy = scale(ylo, yhi, height - m.bottom, m.top);
    let parts = [];

    if (opts.main) {
      parts.push(`<text x="${(m.left + width - m.right) / 2}" y="${m.top - 12}" text-anchor="middle" font-size="15" font-weight="bold" style="${FONT}">${esc(opts.main)}</text>`);
    }
    if (opts.xlab) {
      parts.push(`<text x="${(m.left + width - m.right) / 2}" y="${height - 8}" text-anchor="middle" font-size="13" style="${FONT}">${esc(opts.xlab)}</text>`);
    }
    if (opts.ylab) {
      const yMid = (m.top + height - m.bottom) / 2;
      parts.push(`<text x="14" y="${yMid}" text-anchor="middle" font-size="13" style="${FONT}" transform="rotate(-90 14 ${yMid})">${esc(opts.ylab)}</text>`);
    }
    if (opts.xAxis !== false) {
      const y0 = height - m.bottom;
      const ticks = tickValues(xlo, xhi);
      if (ticks.length >= 2) {
        parts.push(`<line x1="${sx(ticks[0])}" y1="${y0}" x2="${sx(ticks[ticks.length - 1])}" y2="${y0}" stroke="black"/>`);
      }
      for (const t of ticks) {
        parts.push(`<line x1="${sx(t)}" y1="${y0}" x2="${sx(t)}" y2="${y0 + 5}" stroke="black"/>`);
        parts.push(`<text x="${sx(t)}" y="${y0 + 18}" text-anchor="middle" font-size="12" style="${FONT}">${fmtTick(t)}</text>`);
      }
    }
    if (opts.yAxis !== false) {
      const x0 = m.left;
      const ticks = tickValues(ylo, yhi);
      if (ticks.length >= 2) {
        parts.push(`<line x1="${x0}" y1="${sy(ticks[0])}" x2="${x0}" y2="${sy(ticks[ticks.length - 1])}" stroke="black"/>`);
      }
      for (const t of ticks) {
        parts.push(`<line x1="${x0}" y1="${sy(t)}" x2="${x0 - 5}" y2="${sy(t)}" stroke="black"/>`);
        parts.push(`<text x="${x0 - 8}" y="${sy(t) + 4}" text-anchor="end" font-size="12" style="${FONT}">${fmtTick(t)}</text>`);
      }
    }

    return { m, sx, sy, xlo, xhi, ylo, yhi, parts };
  }

  function svgWrap(width, height, parts, cssHeight) {
    return `<svg viewBox="0 0 ${width} ${height}" width="100%" ${cssHeight ? `style="max-height:${cssHeight}px;"` : ""} xmlns="http://www.w3.org/2000/svg" role="img">` +
           parts.join("") + "</svg>";
  }

  // ---------- histogram (R hist(x, breaks = n)) ----------
  function histogramSVG(x, { width = 500, height = 400, main = "", xlab = "", ylab = "Frequency", col = "#d3d3d3", breaks = 30, cssHeight = null } = {}) {
    const lo = Math.min(...x), hi = Math.max(...x);
    const bks = Stats.prettyBreaks(lo, hi, breaks);
    const counts = new Array(bks.length - 1).fill(0);
    for (const v of x) {
      // R default right = TRUE: intervals (b[i], b[i+1]], first includes b[0].
      let idx = -1;
      for (let i = 0; i < bks.length - 1; i++) {
        if ((v > bks[i] || (i === 0 && v >= bks[0])) && v <= bks[i + 1]) { idx = i; break; }
      }
      if (idx >= 0) counts[idx]++;
    }
    const f = makeFrame(width, height, {
      xdomain: [bks[0], bks[bks.length - 1]],
      ydomain: [0, Math.max(...counts)],
      main, xlab, ylab
    });
    for (let i = 0; i < counts.length; i++) {
      const x0 = f.sx(bks[i]), x1 = f.sx(bks[i + 1]);
      const y0 = f.sy(0), y1 = f.sy(counts[i]);
      f.parts.push(`<rect x="${x0}" y="${y1}" width="${x1 - x0}" height="${y0 - y1}" fill="${col}" stroke="black"/>`);
    }
    return svgWrap(width, height, f.parts, cssHeight);
  }

  // ---------- density histogram (R hist(..., freq = FALSE)) ----------
  // Used by the CLT (box model part 2) lesson. `breaks` may be a bin COUNT
  // (equal-width bins over [min, max], as R's seq(range, length.out = n+1)) or
  // the string "sturges" (R's default nclass.Sturges -> pretty breaks). With no
  // data it draws the empty placeholder axes (ylim 0..0.25, xlim `emptyXDomain`,
  // which callers set to the range the data will actually occupy).
  function densityHistogramSVG(values, { width = 460, height = 430, main = "", xlab = "", ylab = "Density",
      col = "lightgreen", breaks = "sturges", curve = null, cssHeight = null,
      emptyXDomain = [0, 10] } = {}) {
    if (!values || values.length === 0) {
      const f = makeFrame(width, height, { xdomain: emptyXDomain, ydomain: [0, 0.25], main, xlab, ylab, padY: false });
      return svgWrap(width, height, f.parts, cssHeight);
    }
    const n = values.length;
    let lo = Math.min(...values), hi = Math.max(...values);
    if (hi <= lo) { lo -= 0.5; hi += 0.5; }               // degenerate: single value
    let bks;
    if (Array.isArray(breaks)) {
      bks = breaks;
    } else if (breaks === "sturges") {
      const nclass = Math.ceil(Math.log2(n) + 1);
      bks = Stats.prettyBreaks(lo, hi, nclass);
    } else {
      const nb = Math.max(1, Math.round(breaks));
      bks = [];
      for (let i = 0; i <= nb; i++) bks.push(lo + (hi - lo) * i / nb);
    }
    // Counts per bin, R default right = TRUE (first bin includes the left edge).
    const counts = new Array(bks.length - 1).fill(0);
    for (const v of values) {
      for (let i = 0; i < bks.length - 1; i++) {
        if ((v > bks[i] || (i === 0 && v >= bks[0])) && v <= bks[i + 1]) { counts[i]++; break; }
      }
    }
    // Density: bar area sums to 1, so height = count / (n * binWidth).
    const dens = counts.map((c, i) => c / (n * (bks[i + 1] - bks[i])));
    // Optional overlaid normal density curve N(curve.ev, curve.se) in red.
    const dnorm = (x, ev, se) => Stats.dnorm((x - ev) / se) / se;
    let ymax = Math.max(...dens);
    if (curve) ymax = Math.max(ymax, dnorm(curve.ev, curve.ev, curve.se));  // peak at the mean
    const f = makeFrame(width, height, {
      xdomain: [bks[0], bks[bks.length - 1]], ydomain: [0, ymax], main, xlab, ylab
    });
    for (let i = 0; i < dens.length; i++) {
      const x0 = f.sx(bks[i]), x1 = f.sx(bks[i + 1]);
      const y0 = f.sy(0), y1 = f.sy(dens[i]);
      f.parts.push(`<rect x="${x0}" y="${y1}" width="${x1 - x0}" height="${y0 - y1}" fill="${col}" stroke="black"/>`);
    }
    if (curve) {
      const N = 200, lo = bks[0], hi = bks[bks.length - 1], pts = [];
      for (let i = 0; i < N; i++) { const xv = lo + (hi - lo) * i / (N - 1); pts.push(`${f.sx(xv)},${f.sy(dnorm(xv, curve.ev, curve.se))}`); }
      f.parts.push(`<polyline points="${pts.join(" ")}" fill="none" stroke="red" stroke-width="2"/>`);
    }
    return svgWrap(width, height, f.parts, cssHeight);
  }

  // ---------- general normal curve with a shaded region [lower, upper] ----------
  // Ports the R "shaded_normal_curve" (box model part 3, finding probabilities):
  // a N(ev, se^2) curve over ev +/- 4 se, shading between the bounds (null = the
  // corresponding +/-infinity edge), with x ticks at ev + se*(-3..3).
  function shadedNormalRegionSVG(ev, se, lower, upper, { width = 480, height = 350, cssHeight = null } = {}) {
    const lo = ev - 4 * se, hi = ev + 4 * se;
    let a = (lower === null || !isFinite(lower)) ? lo : Math.max(lower, lo);
    let b = (upper === null || !isFinite(upper)) ? hi : Math.min(upper, hi);
    const dnorm = x => Stats.dnorm((x - ev) / se) / se;
    const N = 1000, xs = [], ys = [];
    for (let i = 0; i < N; i++) { const xv = lo + (hi - lo) * i / (N - 1); xs.push(xv); ys.push(dnorm(xv)); }
    const ticks = [];
    for (let k = -3; k <= 3; k++) ticks.push(ev + se * k);
    const f = makeFrame(width, height, {
      xdomain: [lo, hi], ydomain: [0, Math.max(...ys)],
      margins: { top: 10, right: 10, bottom: 40, left: 10 }, yAxis: false, xAxis: false
    });
    // shaded region
    const shade = [];
    for (let i = 0; i < N; i++) if (xs[i] >= a && xs[i] <= b) shade.push(`${f.sx(xs[i])},${f.sy(ys[i])}`);
    if (shade.length) {
      const first = shade[0].split(",")[0], last = shade[shade.length - 1].split(",")[0];
      f.parts.push(`<polygon points="${first},${f.sy(0)} ${shade.join(" ")} ${last},${f.sy(0)}" fill="rgba(255,0,0,0.5)" stroke="none"/>`);
    }
    f.parts.push(`<polyline points="${xs.map((xv, i) => `${f.sx(xv)},${f.sy(ys[i])}`).join(" ")}" fill="none" stroke="black" stroke-width="2"/>`);
    // custom x-axis
    const y0 = height - f.m.bottom;
    f.parts.push(`<line x1="${f.sx(ticks[0])}" y1="${y0}" x2="${f.sx(ticks[ticks.length - 1])}" y2="${y0}" stroke="black"/>`);
    for (const t of ticks) {
      f.parts.push(`<line x1="${f.sx(t)}" y1="${y0}" x2="${f.sx(t)}" y2="${y0 + 5}" stroke="black"/>`);
      f.parts.push(`<text x="${f.sx(t)}" y="${y0 + 18}" text-anchor="middle" font-size="11" style="${FONT}">${t.toFixed(2)}</text>`);
    }
    return svgWrap(width, height, f.parts, cssHeight);
  }

  // ---------- confidence-interval plot (horizontal CIs, coloured by coverage) ----------
  // Ports the R CI simulation plots: one horizontal segment per interval at
  // y = i, green if it contains trueMean else red, with a dashed vertical line
  // at trueMean. `xlim` fixes the axis (demo 2 uses a fixed axis).
  function ciPlotSVG(lowers, uppers, contains, trueMean, { width = 620, height = 430, main = "",
      xlab = "Value", ylab = "Sample Number", xlim = null, cssHeight = null } = {}) {
    const n = lowers.length;
    const xd = xlim || (n > 0
      ? [Math.min(...lowers), Math.max(...uppers)]
      : [trueMean - 10, trueMean + 10]);
    const f = makeFrame(width, height, { xdomain: xd, ydomain: [1, 100], main, xlab, ylab });
    f.parts.push(`<line x1="${f.sx(trueMean)}" y1="${f.sy(1)}" x2="${f.sx(trueMean)}" y2="${f.sy(100)}" stroke="black" stroke-dasharray="5,4"/>`);
    for (let i = 0; i < n; i++) {
      const y = f.sy(i + 1);
      const colr = contains[i] ? "green" : "red";
      f.parts.push(`<line x1="${f.sx(lowers[i])}" y1="${y}" x2="${f.sx(uppers[i])}" y2="${y}" stroke="${colr}" stroke-width="2"/>`);
    }
    return svgWrap(width, height, f.parts, cssHeight);
  }

  // ---------- boxplot (R boxplot.stats: fivenum hinges, 1.5 IQR whiskers) ----------
  function boxplotStats(x) {
    const fn = Stats.fivenum(x);
    const [min, lower, median, upper, max] = fn;
    const iqr = upper - lower;
    const inRange = x.filter(v => v >= lower - 1.5 * iqr && v <= upper + 1.5 * iqr);
    const whiskLo = Math.min(...inRange);
    const whiskHi = Math.max(...inRange);
    const outliers = x.filter(v => v < whiskLo || v > whiskHi);
    return { lower, median, upper, whiskLo, whiskHi, outliers };
  }

  // Parts for one horizontal box centred at pixel `cy` with half-height `halfH`.
  function horizontalBoxParts(f, st, cy, halfH, col) {
    const wcapH = halfH * 0.5;
    return [
      `<line x1="${f.sx(st.whiskLo)}" y1="${cy}" x2="${f.sx(st.lower)}" y2="${cy}" stroke="black" stroke-dasharray="5,4"/>`,
      `<line x1="${f.sx(st.upper)}" y1="${cy}" x2="${f.sx(st.whiskHi)}" y2="${cy}" stroke="black" stroke-dasharray="5,4"/>`,
      `<line x1="${f.sx(st.whiskLo)}" y1="${cy - wcapH}" x2="${f.sx(st.whiskLo)}" y2="${cy + wcapH}" stroke="black"/>`,
      `<line x1="${f.sx(st.whiskHi)}" y1="${cy - wcapH}" x2="${f.sx(st.whiskHi)}" y2="${cy + wcapH}" stroke="black"/>`,
      `<rect x="${f.sx(st.lower)}" y="${cy - halfH}" width="${f.sx(st.upper) - f.sx(st.lower)}" height="${2 * halfH}" fill="${col}" stroke="black"/>`,
      `<line x1="${f.sx(st.median)}" y1="${cy - halfH}" x2="${f.sx(st.median)}" y2="${cy + halfH}" stroke="black" stroke-width="2.5"/>`
    ].concat(st.outliers.map(o => `<circle cx="${f.sx(o)}" cy="${cy}" r="3.5" fill="none" stroke="black"/>`));
  }

  function boxplotSVG(x, { width = 500, height = 400, main = "", ylab = "", col = "white", horizontal = false, cssHeight = null } = {}) {
    const st = boxplotStats(x);
    const dataLo = Math.min(...x), dataHi = Math.max(...x);
    let f;
    const parts = [];
    if (horizontal) {
      f = makeFrame(width, height, { xdomain: [dataLo, dataHi], ydomain: [0, 1], main, ylab, yAxis: false, padY: false });
      const cy = (f.sy(0) + f.sy(1)) / 2;
      const halfH = (f.sy(0) - f.sy(1)) * 0.25;
      parts.push(...horizontalBoxParts(f, st, cy, halfH, col));
    } else {
      f = makeFrame(width, height, { xdomain: [0, 1], ydomain: [dataLo, dataHi], main, ylab, xAxis: false });
      const cx = (f.sx(0) + f.sx(1)) / 2;
      const halfW = (f.sx(1) - f.sx(0)) * 0.25;
      const wcapW = halfW * 0.5;
      parts.push(`<line x1="${cx}" y1="${f.sy(st.whiskLo)}" x2="${cx}" y2="${f.sy(st.lower)}" stroke="black" stroke-dasharray="5,4"/>`);
      parts.push(`<line x1="${cx}" y1="${f.sy(st.upper)}" x2="${cx}" y2="${f.sy(st.whiskHi)}" stroke="black" stroke-dasharray="5,4"/>`);
      parts.push(`<line x1="${cx - wcapW}" y1="${f.sy(st.whiskLo)}" x2="${cx + wcapW}" y2="${f.sy(st.whiskLo)}" stroke="black"/>`);
      parts.push(`<line x1="${cx - wcapW}" y1="${f.sy(st.whiskHi)}" x2="${cx + wcapW}" y2="${f.sy(st.whiskHi)}" stroke="black"/>`);
      parts.push(`<rect x="${cx - halfW}" y="${f.sy(st.upper)}" width="${2 * halfW}" height="${f.sy(st.lower) - f.sy(st.upper)}" fill="${col}" stroke="black"/>`);
      parts.push(`<line x1="${cx - halfW}" y1="${f.sy(st.median)}" x2="${cx + halfW}" y2="${f.sy(st.median)}" stroke="black" stroke-width="2.5"/>`);
      for (const o of st.outliers) {
        parts.push(`<circle cx="${cx}" cy="${f.sy(o)}" r="3.5" fill="none" stroke="black"/>`);
      }
    }
    f.parts.push(...parts);
    return svgWrap(width, height, f.parts, cssHeight);
  }

  // ---------- side-by-side horizontal boxplots (two groups) ----------
  // Ports R's boxplot(list(`Sample 1` = x1, `Sample 2` = x2), horizontal = TRUE,
  // col = c("blue", "red")). Sample 1 is drawn on top.
  function boxplotPairSVG(x1, x2, { width = 560, height = 300, main = "",
      labels = ["Sample 1", "Sample 2"], cols = ["blue", "red"], cssHeight = null } = {}) {
    const st1 = boxplotStats(x1), st2 = boxplotStats(x2);
    const all = x1.concat(x2);
    const f = makeFrame(width, height, {
      xdomain: [Math.min(...all), Math.max(...all)], ydomain: [0, 1],
      main, ylab: "", yAxis: false, padY: false, margins: { left: 78 }
    });
    // Two rows: sample 1 near the top (y = 0.72), sample 2 below (y = 0.28).
    const rows = [{ st: st1, yd: 0.72, col: cols[0], label: labels[0] },
                  { st: st2, yd: 0.28, col: cols[1], label: labels[1] }];
    const halfH = (f.sy(0) - f.sy(1)) * 0.16;
    for (const r of rows) {
      const cy = f.sy(r.yd);
      f.parts.push(...horizontalBoxParts(f, r.st, cy, halfH, r.col));
      f.parts.push(`<text x="${f.m.left - 10}" y="${cy + 4}" text-anchor="end" font-size="12" style="${FONT}">${esc(r.label)}</text>`);
    }
    return svgWrap(width, height, f.parts, cssHeight);
  }

  // ---------- scatter / residual / ordered-residual plot ----------
  // Ports R's plot(x, y) with open circles. Options:
  //   xlim/ylim  - override the data ranges (e.g. the intro horizontal-line plot)
  //   hline      - draw a dashed red horizontal line at this y (residual plots)
  //   connect    - join points in order with a line (type = "b"; ordered residuals)
  function scatterSVG(x, y, { width = 480, height = 360, main = "", xlab = "", ylab = "",
      xlim = null, ylim = null, hline = null, connect = false, cssHeight = null } = {}) {
    const xd = xlim || [Math.min(...x), Math.max(...x)];
    const yd = ylim || [Math.min(...y), Math.max(...y)];
    const f = makeFrame(width, height, { xdomain: xd, ydomain: yd, main, xlab, ylab });
    if (hline !== null) {
      f.parts.push(`<line x1="${f.sx(f.xlo)}" y1="${f.sy(hline)}" x2="${f.sx(f.xhi)}" y2="${f.sy(hline)}" stroke="red" stroke-width="2" stroke-dasharray="5,4"/>`);
    }
    if (connect && x.length > 1) {
      const pts = x.map((xv, i) => `${f.sx(xv)},${f.sy(y[i])}`).join(" ");
      f.parts.push(`<polyline points="${pts}" fill="none" stroke="black"/>`);
    }
    for (let i = 0; i < x.length; i++) {
      f.parts.push(`<circle cx="${f.sx(x[i])}" cy="${f.sy(y[i])}" r="3" fill="none" stroke="black"/>`);
    }
    return svgWrap(width, height, f.parts, cssHeight);
  }

  // ---------- normal QQ plot (qqnorm + qqline(col = "red")) ----------
  function qqPlotSVG(x, { width = 400, height = 400, main = "Normal Q-Q Plot", cssHeight = null } = {}) {
    const sorted = x.slice().sort((a, b) => a - b);
    const theo = Stats.ppoints(sorted.length).map(Stats.qnorm);
    const f = makeFrame(width, height, {
      xdomain: [Math.min(...theo), Math.max(...theo)],
      ydomain: [sorted[0], sorted[sorted.length - 1]],
      main, xlab: "Theoretical Quantiles", ylab: "Sample Quantiles"
    });
    // qqline: through the (25%, 75%) quantile pairs, clipped to the plot region.
    const qy1 = Stats.quantileType7(x, 0.25), qy2 = Stats.quantileType7(x, 0.75);
    const qx1 = Stats.qnorm(0.25), qx2 = Stats.qnorm(0.75);
    const slope = (qy2 - qy1) / (qx2 - qx1);
    const yAt = xv => qy1 + slope * (xv - qx1);
    const clip = "qqclip" + (uid++);
    f.parts.push(`<clipPath id="${clip}"><rect x="${f.m.left}" y="${f.m.top}" width="${width - f.m.left - f.m.right}" height="${height - f.m.top - f.m.bottom}"/></clipPath>`);
    f.parts.push(`<line x1="${f.sx(f.xlo)}" y1="${f.sy(yAt(f.xlo))}" x2="${f.sx(f.xhi)}" y2="${f.sy(yAt(f.xhi))}" stroke="red" stroke-width="1.5" clip-path="url(#${clip})"/>`);
    for (let i = 0; i < sorted.length; i++) {
      f.parts.push(`<circle cx="${f.sx(theo[i])}" cy="${f.sy(sorted[i])}" r="3" fill="none" stroke="black"/>`);
    }
    return svgWrap(width, height, f.parts, cssHeight);
  }

  // ---------- shaded curve (curve_shaded_test_stat) ----------
  // Generic renderer for R's curve_shaded_test_stat: draws a density curve and
  // shades the tail region(s) for a test statistic. `density` is x -> y (e.g.
  // t-density for a given df, or the standard normal). altChoice: 1 = two-sided,
  // 2 = greater-than (upper tail), 3 = less-than (lower tail).
  function shadedCurveSVG(density, testStat, altChoice, { width = 560, height = 325, cssHeight = null } = {}) {
    let lo = -3.5, hi = 3.5;
    if (altChoice === 1 && Math.abs(testStat) > hi) {
      lo = -Math.abs(testStat) - 1;
      hi = Math.abs(testStat) + 1;
    } else if (testStat < lo) {
      lo = testStat - 1;
    } else if (testStat > hi) {
      hi = testStat + 1;
    }

    const N = 1000;
    const xs = [], ys = [];
    for (let i = 0; i < N; i++) {
      const xv = lo + (hi - lo) * i / (N - 1);
      xs.push(xv);
      ys.push(density(xv));
    }
    const yMax = Math.max(...ys);

    // mar = c(4, 0.5, 0.5, 0.5): x-axis only, essentially no other margins.
    const f = makeFrame(width, height, {
      xdomain: [lo, hi],
      ydomain: [0, yMax],
      margins: { top: 8, right: 8, bottom: 40, left: 8 },
      yAxis: false, xlab: "", ylab: "", main: ""
    });

    function shadeRegion(from, to) {
      const pts = [];
      for (let i = 0; i < N; i++) {
        if (xs[i] >= from && xs[i] <= to) pts.push(`${f.sx(xs[i])},${f.sy(ys[i])}`);
      }
      if (pts.length === 0) return "";
      const first = pts[0].split(",")[0], last = pts[pts.length - 1].split(",")[0];
      return `<polygon points="${first},${f.sy(0)} ${pts.join(" ")} ${last},${f.sy(0)}" fill="rgba(255,0,0,0.5)" stroke="none"/>`;
    }

    function vline(atX) {
      return `<line x1="${f.sx(atX)}" y1="${f.sy(f.ylo)}" x2="${f.sx(atX)}" y2="${f.sy(f.yhi)}" stroke="blue" stroke-dasharray="6,5"/>`;
    }

    // Blue labels at y = 0.3 in data coordinates, left-aligned (adj = 0),
    // matching the R helper.
    function label(atX, value) {
      return `<text x="${f.sx(atX)}" y="${f.sy(0.3)}" font-size="13" fill="blue" style="${FONT}">${fmtTick(Stats.roundR(value, 2))}</text>`;
    }

    // Only shade/annotate for a finite test statistic; if the input is blank
    // (NaN) the R app silently draws just the curve.
    const parts = [];
    if (Number.isFinite(testStat)) {
      if (altChoice === 1) {
        const a = Math.abs(testStat);
        parts.push(shadeRegion(lo, -a));
        parts.push(shadeRegion(a, hi));
        parts.push(vline(-a), vline(a));
        parts.push(label(-a - 0.8, -a), label(a + 0.25, a));
      } else if (altChoice === 2) {
        parts.push(shadeRegion(testStat, hi));
        parts.push(vline(testStat));
        parts.push(label(testStat + 0.25, testStat));
      } else if (altChoice === 3) {
        parts.push(shadeRegion(lo, testStat));
        parts.push(vline(testStat));
        parts.push(label(testStat - 0.8, testStat));
      }
    }

    // Curve on top of the shading, like the R draw order visually implies.
    const curve = xs.map((xv, i) => `${f.sx(xv)},${f.sy(ys[i])}`).join(" ");
    parts.push(`<polyline points="${curve}" fill="none" stroke="black" stroke-width="2"/>`);

    f.parts.push(...parts);
    return svgWrap(width, height, f.parts, cssHeight);
  }

  // Shaded t-curve for a given df (the case the t-test lessons use).
  function shadedTCurveSVG(df, testStat, altChoice, opts = {}) {
    return shadedCurveSVG(x => Stats.dt(x, df), testStat, altChoice, opts);
  }

  // Shaded standard-normal curve (the z-test comparison in the t-curve lesson).
  function shadedNormalCurveSVG(testStat, altChoice, opts = {}) {
    return shadedCurveSVG(x => Stats.dnorm(x), testStat, altChoice, opts);
  }

  // ---------- shaded chi-square curve (goodness-of-fit p-value) ----------
  // A chi-square density on [0, hi] for the given df, shading the UPPER tail
  // above the test statistic (a GoF test is always right-tailed), with a blue
  // dashed marker + label at the test statistic. The x-range covers the bulk of
  // the distribution (mean +/- a few sd) and always the test statistic. The
  // curve is sampled away from x = 0 so the df <= 2 spike doesn't dominate the
  // vertical scale.
  function shadedChiSquareCurveSVG(df, testStat, { width = 560, height = 325, cssHeight = null } = {}) {
    const mean = df, sd = Math.sqrt(2 * df);
    let hi = Math.max(mean + 4 * sd, 10);
    if (Number.isFinite(testStat) && testStat > 0) hi = Math.max(hi, testStat * 1.15);

    const N = 1000, x0 = hi / N, xs = [], ys = [];
    for (let i = 0; i < N; i++) {
      const xv = x0 + (hi - x0) * i / (N - 1);
      xs.push(xv);
      ys.push(Stats.dchisq(xv, df));
    }
    const yMax = Math.max(...ys);

    const f = makeFrame(width, height, {
      xdomain: [0, hi], ydomain: [0, yMax],
      margins: { top: 10, right: 12, bottom: 40, left: 12 },
      yAxis: false, xlab: "", ylab: "", main: ""
    });

    const parts = [];
    if (Number.isFinite(testStat) && testStat > 0 && testStat < hi) {
      const pts = [];
      for (let i = 0; i < N; i++) if (xs[i] >= testStat) pts.push(`${f.sx(xs[i])},${f.sy(ys[i])}`);
      if (pts.length) {
        const first = pts[0].split(",")[0], last = pts[pts.length - 1].split(",")[0];
        parts.push(`<polygon points="${first},${f.sy(0)} ${pts.join(" ")} ${last},${f.sy(0)}" fill="rgba(255,0,0,0.5)" stroke="none"/>`);
      }
      parts.push(`<line x1="${f.sx(testStat)}" y1="${f.sy(0)}" x2="${f.sx(testStat)}" y2="${f.sy(yMax)}" stroke="blue" stroke-dasharray="6,5"/>`);
      parts.push(`<text x="${f.sx(testStat) + 4}" y="${f.sy(yMax * 0.6)}" font-size="13" fill="blue" style="${FONT}">${fmtTick(Stats.roundR(testStat, 2))}</text>`);
    }
    const curve = xs.map((xv, i) => `${f.sx(xv)},${f.sy(ys[i])}`).join(" ");
    parts.push(`<polyline points="${curve}" fill="none" stroke="black" stroke-width="2"/>`);

    f.parts.push(...parts);
    return svgWrap(width, height, f.parts, cssHeight);
  }

  // ---------- t-curve vs normal overlay (t-curve motivation, Demo 1) ----------
  // Ports the R renderPlot: x in [-4, 4], t-density (black solid), and the
  // standard normal (red dashed) when `showNormal`. Only an x-axis is drawn
  // (axes = FALSE; axis(1)), and the y-range always spans BOTH curves so the
  // t-curve keeps the same vertical scale whether or not the normal is shown
  // (R computes ylim = range(y_t, y_norm) before the checkbox branch).
  function densityOverlaySVG(df, showNormal, { width = 500, height = 300, cssHeight = null } = {}) {
    const N = 100;
    const xs = [], yt = [], yn = [];
    for (let i = 0; i < N; i++) {
      const xv = -4 + 8 * i / (N - 1);
      xs.push(xv);
      yt.push(Stats.dt(xv, df));
      yn.push(Stats.dnorm(xv));
    }
    const all = yt.concat(yn);
    const yMin = Math.min(...all), yMax = Math.max(...all);

    const f = makeFrame(width, height, {
      xdomain: [-4, 4],
      ydomain: [yMin, yMax],
      margins: { top: 8, right: 8, bottom: 40, left: 8 },
      yAxis: false, xlab: "", ylab: "", main: ""
    });

    const tCurve = xs.map((xv, i) => `${f.sx(xv)},${f.sy(yt[i])}`).join(" ");
    if (showNormal) {
      const nCurve = xs.map((xv, i) => `${f.sx(xv)},${f.sy(yn[i])}`).join(" ");
      f.parts.push(`<polyline points="${nCurve}" fill="none" stroke="red" stroke-width="1" stroke-dasharray="6,4"/>`);
    }
    f.parts.push(`<polyline points="${tCurve}" fill="none" stroke="black" stroke-width="1"/>`);
    return svgWrap(width, height, f.parts, cssHeight);
  }

  return { histogramSVG, densityHistogramSVG, boxplotSVG, boxplotPairSVG, scatterSVG, qqPlotSVG,
           shadedCurveSVG, shadedTCurveSVG, shadedNormalCurveSVG, shadedChiSquareCurveSVG,
           shadedNormalRegionSVG, densityOverlaySVG, ciPlotSVG, boxplotStats };
})();

if (typeof module !== "undefined" && module.exports) module.exports = Plots;
