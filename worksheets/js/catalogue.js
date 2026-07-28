// catalogue.js -- the single list of worksheets, used by both the home page
// (to build the card grid) and every worksheet page (to build the prev/next
// pager). Adding a worksheet means adding one entry here; nothing else needs
// to know about it.
//
// `folder` and `slug` must match the interactive lesson's directory on the main
// site, because both the worksheet's own URL and its "Open the interactive
// page" link are derived from them:
//
//     worksheet   ->  /worksheets/<folder>/<slug>/
//     lesson      ->  /<folder>/<slug>/
//
// `status` records whether the questions have actually been written yet
// ("skeleton") or not ("ready"). Nothing renders it -- the home page lists bare
// titles -- but it is the quickest way to see how much of the site is done.

"use strict";

window.WORKSHEETS = [
  {
    menu: "Fundamentals",
    folder: "Fundamentals",
    items: [
      { slug: "box-model-part-1",  title: "The Box Model Part 1 - What is the Box Model?",                 status: "ready" },
      { slug: "box-model-part-2",  title: "The Box Model Part 2 - Central Limit Theorem",                  status: "ready" },
      { slug: "box-model-part-3",  title: "The Box Model Part 3 - Modelling Using a Normal Distribution",  status: "ready" },
      { slug: "confidence-intervals", title: "Confidence Intervals",                                       status: "skeleton" }
    ]
  },
  {
    menu: "Z-Tests",
    folder: "Z-Tests",
    items: [
      { slug: "z-test-1-sample",  title: "1-Sample z-test",          status: "skeleton" },
      { slug: "proportion-test",  title: "Proportion Test (z-test)", status: "skeleton" }
    ]
  },
  {
    menu: "T-Tests",
    folder: "T-Tests",
    items: [
      { slug: "t-curve-motivation", title: "Introducing the t-Distribution (Motivation)", status: "skeleton" },
      { slug: "t-test-1-sample",    title: "1-Sample t-Test",                             status: "skeleton" },
      { slug: "t-test-paired",      title: "Paired t-Test",                               status: "skeleton" },
      { slug: "t-test-2-sample",    title: "2-Sample t-Test",                             status: "skeleton" },
      { slug: "t-test-regression",  title: "Regression t-Test",                           status: "skeleton" }
    ]
  },
  {
    menu: "Chi-Square Tests",
    folder: "Chi-Square-Tests",
    items: [
      { slug: "chi-square-goodness-of-fit",       title: "Chi-Square Goodness of Fit Test", status: "skeleton" },
      { slug: "chi-square-test-of-independence",  title: "Chi-Square Test of Independence", status: "skeleton" }
    ]
  }
];

// Flattened reading order, so the pager can walk straight through the site.
window.WORKSHEETS_FLAT = window.WORKSHEETS.reduce(function (all, group) {
  return all.concat(group.items.map(function (item) {
    return {
      menu: group.menu,
      folder: group.folder,
      slug: item.slug,
      title: item.title,
      status: item.status
    };
  }));
}, []);
