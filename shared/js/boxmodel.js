// boxmodel.js -- JS port of box_model_html() from R/utility_helper_functions.R.
// Renders the blue tickets-box -> labelled down-arrow -> yellow sample oval
// as inline HTML/CSS. Labels may contain HTML entities (&mu;) and "\n" line
// breaks (rendered via white-space: pre-line).

"use strict";

function boxModelHTML(boxLabel, sampleLabel = null, nLabel = null, fontSize = 22) {
  const boxStyle =
    "background-color:#bdfeff;" +
    "border:1px solid black;" +
    "border-radius:0.15em;" +
    "padding:0.8em 1.3em;" +
    "min-width:13.5em;" +
    "max-width:100%;" +
    "text-align:center;" +
    "white-space:pre-line;" +
    "line-height:1.35;" +
    "box-sizing:border-box;";

  const ovalStyle =
    "background-color:#f9ffbd;" +
    "border:1px solid black;" +
    "border-radius:50%;" +
    "padding:1.05em 1.7em;" +
    "min-width:8.5em;" +
    "text-align:center;" +
    "white-space:pre-line;" +
    "line-height:1.35;" +
    "box-sizing:border-box;";

  let arrow = "";
  if (sampleLabel !== null) {
    const nLabelHTML = (nLabel !== null)
      ? '<div style="position:absolute; left:0.6em; top:50%; transform:translateY(-50%);' +
        ' white-space:nowrap; font-size:0.85em;">' + nLabel + "</div>"
      : "";
    arrow =
      '<div style="position:relative; width:2px; height:3em; background:#333333; margin:0.15em 0;">' +
        '<div style="position:absolute; bottom:-0.4em; left:50%; transform:translateX(-50%);' +
        ' width:0; height:0; border-left:0.4em solid transparent;' +
        ' border-right:0.4em solid transparent; border-top:0.55em solid #333333;"></div>' +
        nLabelHTML +
      "</div>";
  }

  return (
    '<div style="display:flex; flex-direction:column; align-items:center; padding:0.4em 0;' +
    " font-size:" + fontSize + 'px;">' +
      '<div style="' + boxStyle + '">' + boxLabel + "</div>" +
      arrow +
      (sampleLabel !== null ? '<div style="' + ovalStyle + '">' + sampleLabel + "</div>" : "") +
    "</div>"
  );
}

// One yellow rounded "sample" cell (port of sample_cell_html). The label may
// contain HTML.
function sampleCellHTML(label) {
  return '<div style="display:flex; align-items:center; justify-content:center; text-align:center;' +
    "width:120px; min-height:40px; padding:4px;" +
    "background-color:#f9ffbd; border:1px solid black; border-radius:8px;" +
    'font-size:12px; box-sizing:border-box;">' + label + "</div>";
}

// A 2-column grid of sample cells (port of sample_grid_html). Empty -> "".
function sampleGridHTML(labels) {
  if (!labels || labels.length === 0) return "";
  return '<div style="display:grid; grid-template-columns:repeat(2, max-content);' +
    'gap:12px; justify-content:center; padding:6px 0;">' +
    labels.map(sampleCellHTML).join("") + "</div>";
}

if (typeof module !== "undefined" && module.exports) {
  module.exports = { boxModelHTML, sampleCellHTML, sampleGridHTML };
}
