// worksheet.js -- shared behaviour for every page of the Guiding Questions site.
//
// Each page declares where it sits via attributes on <body>:
//
//     data-depth     how many directories below /worksheets/ the page lives
//                    (0 = the home page, 1 = /worksheets/style-guide/,
//                     2 = /worksheets/<folder>/<slug>/)
//     data-folder    the lesson's category folder  (worksheet pages only)
//     data-slug      the lesson's slug             (worksheet pages only)
//
// Relative links are built from data-depth, so the site works unchanged when
// served from the repo root locally and from /Hypothesis-Tests-Visualised/ on
// GitHub Pages. Nothing here hardcodes the deployed domain.
//
// Responsibilities: inject the top bar, build the prev/next pager, and render
// the home page card grid.

"use strict";

(function () {

  const body    = document.body;
  const depth   = parseInt(body.dataset.depth || "0", 10);
  const folder  = body.dataset.folder || null;
  const slug    = body.dataset.slug || null;

  const wsRoot   = depth === 0 ? "./" : "../".repeat(depth);  // -> /worksheets/
  const siteRoot = "../".repeat(depth + 1);                   // -> interactive site root

  const flat = window.WORKSHEETS_FLAT || [];
  const here = flat.findIndex(w => w.folder === folder && w.slug === slug);

  // ------------------------------------------------------------- top bar

  function buildTopbar() {
    const bar = document.createElement("header");
    bar.className = "topbar";
    bar.innerHTML =
      '<div class="topbar-inner">' +
        '<a class="brand" href="' + wsRoot + '">HTVP Guiding Questions</a>' +
        '<nav class="topbar-links">' +
          '<a href="' + wsRoot + '">All worksheets</a>' +
          // Opens in a new tab: the worksheets are meant to be read alongside
          // the interactive page, not instead of it.
          '<a class="live" href="' + siteRoot + '" target="_blank" rel="noopener">Interactive site ↗</a>' +
        '</nav>' +
      '</div>';
    body.insertBefore(bar, body.firstChild);
  }

  // -------------------------------------------------------- home page grid

  function buildHomeGrid() {
    const mount = document.getElementById("catalogue");
    if (!mount) return;

    (window.WORKSHEETS || []).forEach(function (group) {
      const head = document.createElement("div");
      head.className = "cat-head";
      head.innerHTML =
        "<h2>" + group.menu + "</h2>" +
        '<span class="count">' + group.items.length +
          " worksheet" + (group.items.length === 1 ? "" : "s") + "</span>";
      mount.appendChild(head);

      const grid = document.createElement("div");
      grid.className = "home-grid";
      group.items.forEach(function (item) {
        const card = document.createElement("a");
        card.className = "ws-card";
        card.href = wsRoot + group.folder + "/" + item.slug + "/";
        card.innerHTML = '<span class="ws-card-title">' + item.title + "</span>";
        grid.appendChild(card);
      });
      mount.appendChild(grid);
    });
  }

  // ------------------------------------------------------------- pager

  function buildPager() {
    if (here === -1) return;

    const prev = flat[here - 1];
    const next = flat[here + 1];
    if (!prev && !next) return;

    const pager = document.createElement("nav");
    pager.className = "pager";

    function link(item, dir, cls) {
      const a = document.createElement("a");
      a.className = cls;
      a.href = wsRoot + item.folder + "/" + item.slug + "/";
      a.innerHTML = '<span class="dir">' + dir + "</span>" + item.title;
      return a;
    }

    if (prev) pager.appendChild(link(prev, "← Previous", "prev"));
    if (next) pager.appendChild(link(next, "Next →", "next"));

    const main = document.querySelector("main.page") || body;
    main.appendChild(pager);
  }

  // ------------------------------------------------------------------ go

  buildTopbar();
  buildHomeGrid();
  buildPager();

})();
