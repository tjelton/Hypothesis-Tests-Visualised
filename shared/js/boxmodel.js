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

if (typeof module !== "undefined" && module.exports) module.exports = boxModelHTML;
