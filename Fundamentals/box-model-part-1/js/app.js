// app.js -- interactive wiring for "The Box Model Part 1". Ports
// R/fundamentals_box_model_part_1_srv.R. This is a simulation explainer with
// three demos; the draws are random (no fidelity to any particular R seed is
// required), so Math.random is used directly.
//
// Each demo keeps a rolling grid of up to 10 results: the first 10 simulations
// append, and subsequent ones overwrite position ((count - 1) % 10), exactly
// as the R module cycles through its display slots.

"use strict";

(function () {

  const $ = id => document.getElementById(id);

  // Draw `size` items from `box` with replacement.
  function drawWithReplacement(box, size) {
    const out = [];
    for (let i = 0; i < size; i++) out.push(box[Math.floor(Math.random() * box.length)]);
    return out;
  }

  // A rolling buffer of up to 10 labels with the R module's overwrite cycle.
  function makeRoller() {
    return { labels: [], count: 0,
      add(label) {
        if (this.count < 10) { this.labels.push(label); this.count++; }
        else { this.count++; let pos = this.count % 10; if (pos === 0) pos = 10; this.labels[pos - 1] = label; }
      },
      reset() { this.labels = []; this.count = 0; }
    };
  }

  const coinBox = boxModelHTML("Head (H), Tail (T)", "Sample", "n = 5");

  document.addEventListener("DOMContentLoaded", function () {
    // ---- static box models ----
    $("example-coin-flip-1").innerHTML = coinBox;
    $("example-coin-flip-2").innerHTML = coinBox;
    $("single-sample-words").innerHTML = sampleCellHTML("H T T H T");
    $("single-sample-numbers").innerHTML = sampleCellHTML("1 0 0 1 0");

    // ---- Demo 1: coin flips shown as H/T sequences ----
    const roll1 = makeRoller();
    $("simulate-1").addEventListener("click", function () {
      roll1.add(drawWithReplacement(["H", "T"], 5).join(" "));
      $("sim-samples-1").innerHTML = sampleGridHTML(roll1.labels);
    });

    // ---- Demo 2: numeric tickets summarised by sum or mean ----
    const roll2 = makeRoller();
    function demoSumMean() { return document.querySelector('input[name="demo_sum_mean"]:checked').value; }
    function renderNumericBox() {
      $("coin-flip-numeric-box").innerHTML = boxModelHTML("1, 0", "Sample " + demoSumMean(), "n = 5");
    }
    renderNumericBox();
    for (const r of document.querySelectorAll('input[name="demo_sum_mean"]')) {
      r.addEventListener("change", function () { roll2.reset(); $("sim-samples-2").innerHTML = ""; renderNumericBox(); });
    }
    $("simulate-2").addEventListener("click", function () {
      const draw = drawWithReplacement([1, 0], 5);
      const summary = demoSumMean() === "Mean" ? mean(draw) : sum(draw);
      roll2.add(summary + " (" + draw.join(" ") + ")");
      $("sim-samples-2").innerHTML = sampleGridHTML(roll2.labels);
    });

    // ---- Demo 3: your own box (sum/mean of user-defined tickets) ----
    let tickets = [1, 0, 0, 0, 0, 0];
    let draws = 20;
    const roll3 = makeRoller();

    function boxSumMean() { return document.querySelector('input[name="box_sum_mean"]:checked').value; }
    // Tickets laid out comma-separated, wrapping to a new line every 15 tickets
    // (matching the R box_label construction).
    function ticketsString() {
      let s = "", count = 0;
      for (const v of tickets) {
        count++;
        if (count === 15) { s += "\n" + v; count = 0; }
        else { s += ", " + v; }
      }
      return s.replace(/^, /, "");
    }
    function renderOwnBox() {
      const sampleLabel = boxSumMean() === "2" ? "Sample Mean" : "Sample Sum";
      $("own-box-model").innerHTML = boxModelHTML(ticketsString(), sampleLabel, "n = " + draws);
      roll3.reset();
      $("sim-samples-own").innerHTML = "";
    }
    renderOwnBox();

    $("submit-tickets").addEventListener("click", function () {
      const parts = $("box-tickets-entry").value.split(",").map(s => s.trim());
      const nums = parts.map(Number);
      if (nums.some(Number.isNaN) || nums.length <= 1) {
        $("tickets-error").classList.remove("d-none");
        tickets = [1, 0, 0, 0, 0, 0];
      } else {
        $("tickets-error").classList.add("d-none");
        tickets = nums;
      }
      renderOwnBox();
    });

    $("number-of-draws").addEventListener("input", function () {
      const v = this.value === "" ? NaN : Number(this.value);
      if (!Number.isNaN(v) && v >= 1) { draws = Math.ceil(v); $("draws-error").classList.add("d-none"); }
      else { draws = 25; $("draws-error").classList.remove("d-none"); }
      renderOwnBox();
    });

    for (const r of document.querySelectorAll('input[name="box_sum_mean"]')) {
      r.addEventListener("change", renderOwnBox);
    }
    $("simulate-own").addEventListener("click", function () {
      const draw = drawWithReplacement(tickets, draws);
      const summary = boxSumMean() === "2" ? mean(draw) : sum(draw);
      roll3.add(String(summary));
      $("sim-samples-own").innerHTML = sampleGridHTML(roll3.labels);
    });
  });

  function sum(a) { return a.reduce((s, v) => s + v, 0); }
  function mean(a) { return sum(a) / a.length; }

})();
