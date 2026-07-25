// navbar.js -- the shared site navigation bar, injected on every page (home +
// all lessons). Mirrors the Shiny app's page_navbar: a brand, a Home link, and
// Fundamentals / Z-Tests / T-Tests / Chi-Square Tests dropdown menus.
//
// Lessons are organised into category folders that match the dropdown menus, so
// a lesson lives at "/<Category folder>/<slug>/" (e.g.
// /T-Tests/t-test-paired/). That means a lesson page is two directories deep,
// while the home page is the site root. Links therefore need a relative prefix:
// "../../" from inside a lesson, "" from the home page. The script detects which
// by checking whether the current path ends in a known "<folder>/<slug>" pair,
// so the same file works both locally (served from the repo root) and on GitHub
// Pages (served under /Hypothesis-Tests-Visualised/). The current lesson's menu
// item is marked active.

"use strict";

(function () {

  // [menu title, category folder, [[slug, label], ...]]
  const MENUS = [
    ["Fundamentals", "Fundamentals", [
      ["box-model-part-1", "Box Model - Part 1"],
      ["box-model-part-2", "Box Model - Part 2"],
      ["box-model-part-3", "Box Model - Part 3"],
      ["confidence-intervals", "Confidence Intervals"]
    ]],
    ["Z-Tests", "Z-Tests", [
      ["z-test-1-sample", "1-Sample Z-Test"],
      ["proportion-test", "Proportion (Z-test)"]
    ]],
    ["T-Tests", "T-Tests", [
      ["t-curve-motivation", "T-Curve Motivation"],
      ["t-test-1-sample", "1-Sample T-Test"],
      ["t-test-paired", "Paired T-Test"],
      ["t-test-2-sample", "2-Sample T-Test"],
      ["t-test-regression", "Regression T-Test"]
    ]],
    ["Chi-Square Tests", "Chi-Square-Tests", [
      ["chi-square-goodness-of-fit", "Goodness of Fit"],
      ["chi-square-test-of-independence", "Test of Independence"]
    ]]
  ];

  // Full path of every lesson = "<folder>/<slug>".
  const allPaths = MENUS.reduce((a, [, folder, items]) =>
    a.concat(items.map(([slug]) => folder + "/" + slug)), []);
  const path = location.pathname.replace(/index\.html$/, "").replace(/\/$/, "");
  const currentPath = allPaths.find(p => path.endsWith("/" + p)) || null;
  const prefix = currentPath ? "../../" : "";   // "../../" inside a lesson, "" at the home root
  const homeHref = prefix || "./";

  function menu(title, folder, items) {
    const parentActive = items.some(([slug]) => folder + "/" + slug === currentPath);
    const lis = items.map(([slug, label]) => {
      const full = folder + "/" + slug;
      return '<li><a class="dropdown-item' + (full === currentPath ? " active" : "") +
        '" href="' + prefix + full + '/">' + label + "</a></li>";
    }).join("");
    return '<li class="nav-item dropdown">' +
      '<a class="nav-link dropdown-toggle' + (parentActive ? " active" : "") + '" href="#" role="button" data-bs-toggle="dropdown" aria-expanded="false">' + title + "</a>" +
      '<ul class="dropdown-menu">' + lis + "</ul></li>";
  }

  const html =
    '<nav class="navbar navbar-expand-lg navbar-light bg-light border-bottom mb-3">' +
      '<div class="container-fluid">' +
        '<a class="navbar-brand" href="' + homeHref + '">Hypothesis Tests Visualised</a>' +
        '<button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#htvpNavCollapse" aria-controls="htvpNavCollapse" aria-expanded="false" aria-label="Toggle navigation"><span class="navbar-toggler-icon"></span></button>' +
        '<div class="collapse navbar-collapse" id="htvpNavCollapse"><ul class="navbar-nav">' +
          '<li class="nav-item"><a class="nav-link' + (currentPath ? "" : " active") + '" href="' + homeHref + '">Home</a></li>' +
          MENUS.map(([title, folder, items]) => menu(title, folder, items)).join("") +
        "</ul></div>" +
      "</div>" +
    "</nav>";

  function inject() {
    // Replace an optional <div id="htvp-nav"> placeholder, else prepend to body.
    const el = document.getElementById("htvp-nav");
    if (el) el.outerHTML = html;
    else document.body.insertAdjacentHTML("afterbegin", html);
  }

  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", inject);
  else inject();

})();
