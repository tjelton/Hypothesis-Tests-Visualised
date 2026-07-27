// app.js -- interactive wiring for "The Box Model Part 2 - Central Limit
// Theorem". Ports R/fundamentals_box_model_part_2_srv.R. Pure simulation (random
// draws), so Math.random is used directly and there is no R value fidelity to
// match; the point is the emergent SHAPE of the sample-mean/sum distribution.

"use strict";

(function () {

  const $ = id => document.getElementById(id);

  function drawWithReplacement(box, size) {
    const out = new Array(size);
    for (let i = 0; i < size; i++) out[i] = box[Math.floor(Math.random() * box.length)];
    return out;
  }
  function sum(a) { let s = 0; for (const v of a) s += v; return s; }
  function mean(a) { return sum(a) / a.length; }
  // One sample statistic: sum (mode 1) or mean (mode 2) of `n` draws from `box`.
  function sampleStat(box, n, mode) { const d = drawWithReplacement(box, n); return mode === "2" ? mean(d) : sum(d); }

  // ----- Sections 1 & 2: accumulate sample means, redraw the histogram -----
  // The SVG is drawn at half the previous viewBox size and its container is set
  // to 50% width in the HTML, so the plot occupies half the page width while the
  // labels keep roughly their old on-screen size.
  const MEAN_DEMO_BINS = 14;
  function makeMeanDemo(box, n, histId) {
    let data = [];
    function render() {
      const title = "Empirical Distribution of Sample Means (n = " + data.length + ")";
      // Sample means of a 0/1 box always sit in [0, 1], so the empty placeholder
      // axes use that range (ticks land on 0, 0.2, ..., 1) rather than 0..10.
      const opts = { width: 460, height: 360, main: title, xlab: "Sample Mean Value", ylab: "Density", col: "lightgreen", emptyXDomain: [0, 1] };
      if (data.length === 0) { $(histId).innerHTML = Plots.densityHistogramSVG([], opts); return; }
      opts.breaks = MEAN_DEMO_BINS;
      $(histId).innerHTML = Plots.densityHistogramSVG(data, opts);
    }
    function repeat(k) { for (let i = 0; i < k; i++) data.push(mean(drawWithReplacement(box, n))); render(); }
    function reset() { data = []; render(); }
    render();
    return { repeat, reset };
  }

  document.addEventListener("DOMContentLoaded", function () {
    $("box-model-n25").innerHTML = boxModelHTML("1, 0, 0, 0", "Sample Mean", "n = 25");
    $("box-model-n5").innerHTML = boxModelHTML("1, 0, 0, 0", "Sample Mean", "n = 5");

    const demo25 = makeMeanDemo([1, 0, 0, 0], 25, "hist-n25");
    const demo5 = makeMeanDemo([1, 0, 0, 0], 5, "hist-n5");

    $("rep1-n25").addEventListener("click", () => demo25.repeat(1));
    $("rep10-n25").addEventListener("click", () => demo25.repeat(10));
    $("rep25-n25").addEventListener("click", () => demo25.repeat(25));
    $("rep100-n25").addEventListener("click", () => demo25.repeat(100));
    $("reset-n25").addEventListener("click", () => demo25.reset());

    $("rep1-n5").addEventListener("click", () => demo5.repeat(1));
    $("rep10-n5").addEventListener("click", () => demo5.repeat(10));
    $("rep25-n5").addEventListener("click", () => demo5.repeat(25));
    $("rep100-n5").addEventListener("click", () => demo5.repeat(100));
    $("reset-n5").addEventListener("click", () => demo5.reset());

    // ----- Section 4: custom box, 4 sample sizes, 10,000 samples each -----
    const EXAMPLES = {
      "1": [1, 0],
      "2": [1, 0, 0, 0],
      "3": [1, 0, 0, 0, 0, 0],
      "4": [1, 2, 3, 4, 5, 6, 7, 8, 9],
      "5": [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 100]
    };
    const EXAMPLE_INFO = {
      "1": "When looking from the perspective of sample sums, we see that at a sample size of n = 25, the distribution of sample means appears normally distributed. Hence, for this box, values of n greater than or equal to 25 are sufficient. It is relatively unsurprising that we need such a small sample size here. The box is very symmetric (there is an even balance between the '0' and '1' tickets).",
      "2": "When looking from the perspective of sample sums, we see that at a sample size of n = 25, the distribution of sample means appears very close to being normally distributed. Hence, for this box, values of n greater than or equal to 25 are sufficient.",
      "3": "When looking from the perspective of sample sums, we see that at a sample size of n = 50, the distribution of sample means appears very close to being normally distributed. We need a larger sample size for this box, as there is a large imbalance between the '0' and '1' tickets.",
      "4": "When looking from the perspective of sample sums, we see that at a sample size of n = 5, the distribution of sample means appears very close to being normally distributed. A small sample size for this box is sufficient, as the tickets are nearly symmetric.",
      "5": "Even at n = 100, the sample sums or means do not appear to be normally distributed. This is because the tickets are incredibly asymmetric. This is a classic example where assuming that if n is large, the CLT must apply (such as n greater than 35 or 50) can be misleading."
    };

    let tickets = EXAMPLES["1"].slice();
    const custom = { 5: [], 25: [], 50: [], 100: [] };

    function exampleChoice() { return document.querySelector('input[name="example_choice"]:checked').value; }
    function customSumMean() { return document.querySelector('input[name="custom_sum_mean"]:checked').value; }

    // Tickets laid out comma-separated, wrapping every 15 tickets (matching R).
    function ticketsString() {
      let s = "", count = 0;
      for (const v of tickets) {
        count++;
        if (count === 15) { s += "\n" + v; count = 0; }
        else { s += ", " + v; }
      }
      return s.replace(/^, /, "");
    }
    function renderCustomBox() { $("custom-box-model").innerHTML = boxModelHTML(ticketsString()); }

    function clearCustom() { custom[5] = []; custom[25] = []; custom[50] = []; custom[100] = []; renderCustomHists(); }
    function renderCustomHists() {
      [[5, "hist-custom-n5"], [25, "hist-custom-n25"], [50, "hist-custom-n50"], [100, "hist-custom-n100"]].forEach(([n, id]) => {
        $(id).innerHTML = Plots.densityHistogramSVG(custom[n], { width: 380, height: 300, main: "n = " + n, xlab: "Values", ylab: "Density", col: "lightgreen", breaks: "sturges" });
      });
    }
    function renderInfo() {
      const c = exampleChoice();
      $("examples-info").innerHTML = EXAMPLE_INFO[c]
        ? "<p><span style='color: red;'>What do we see after pressing simulate? </span>" + EXAMPLE_INFO[c] + "</p>"
        : "";
    }

    function onExampleChange() {
      const c = exampleChoice();
      $("custom-input-block").classList.toggle("d-none", c !== "6");
      tickets = c === "6" ? [1, 0] : EXAMPLES[c].slice();
      renderCustomBox();
      clearCustom();
      renderInfo();
    }

    for (const r of document.querySelectorAll('input[name="example_choice"]')) r.addEventListener("change", onExampleChange);
    for (const r of document.querySelectorAll('input[name="custom_sum_mean"]')) r.addEventListener("change", clearCustom);

    $("submit-custom-tickets").addEventListener("click", function () {
      const nums = $("custom-tickets-entry").value.split(",").map(s => Number(s.trim()));
      if (nums.some(Number.isNaN) || nums.length <= 1) {
        $("custom-tickets-error").classList.remove("d-none");
        tickets = [1, 0];
      } else {
        $("custom-tickets-error").classList.add("d-none");
        tickets = nums;
      }
      renderCustomBox();
      clearCustom();
    });

    $("simulate-custom").addEventListener("click", function () {
      const mode = customSumMean();
      [5, 25, 50, 100].forEach(n => {
        const arr = new Array(10000);
        for (let i = 0; i < 10000; i++) arr[i] = sampleStat(tickets, n, mode);
        custom[n] = arr;
      });
      renderCustomHists();
    });

    // initial render
    renderCustomBox();
    clearCustom();
    renderInfo();
  });

})();
