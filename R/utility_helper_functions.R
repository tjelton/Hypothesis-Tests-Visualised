tight_card <- function(title, ..., content_style = NULL,  header_colour = NULL) {
  
  style_for_card = paste(
    "width: 100%;",
    "border-top: 4px solid ", header_colour, ";",
    "padding: 0;",
    "margin: 0;"
  )
  if (is.null(header_colour)) {
    style_for_card = NULL
  }
  
  card(
    full_screen = FALSE,
    style = style_for_card,
    tags$div(
      style = "margin: 0; padding: 0;",
      # Title area
      tags$div(
        title,
        style = "
          font-size: 1.25rem;
          font-weight: bold;
          padding: 0 0.25rem 0 0.25rem;
          margin: -2px 0 0 0;
          line-height: 1.2;
          text-decoration: underline;
        "
      ),
      # Flexible content area.
      tags$div(
        tagList(...),
        style = paste(
          "padding: 0.5rem 0.25rem 0 0.25rem; margin: 0;",
          content_style
        )
      )
    )
  )
}

primary_card <- function(title, ..., content_style = NULL, header_colour = "#FFFFFF", title_color = "#FFFFFF") {
  card(
    full_screen = FALSE,
    style = "width: 100%; padding: 0; margin: 0;",
    
    # Header with background color and white text,
    # but no horizontal padding here:
    card_header(
      tags$div(
        title,
        style = "padding-left: 0.25rem; padding-right: 0.25rem;"
      ),
      style = paste(
        "background-color:", header_colour, ";",
        "color:", title_color, ";",
        "font-size: 1.25rem;",
        "padding-top: 0.8rem; padding-bottom: 0.8rem;",  # vertical padding only here
        "line-height: 1.2;",
        "margin: 0;"
      )
    ),
    
    # Body with inner div padding:
    card_body(
      tags$div(
        tagList(...),
        style = "padding-left: 0.25rem; padding-right: 0.25rem;"
      ),
      class = "primary-card-content",
      style = paste(
        "padding-top: 0.5rem;",
        "padding-bottom: 0.5rem;",
        "margin: 0;",
        content_style
      )
    )
  )
}

# ---------------------------------------------------------------------------
# Box-model diagram helpers (zero-dependency replacements for DiagrammeR/grViz)
#
# These render the "box model" visuals used throughout the app as plain inline
# HTML/CSS via htmltools (already a Shiny dependency), avoiding the large
# DiagrammeR dependency tree that dominated the shinylive cold-start load time.
#
# Labels may contain HTML entities (e.g. "&mu;", "&sigma;") and "\n" newlines;
# both render correctly (newlines become line breaks via `white-space: pre-line`).
# ---------------------------------------------------------------------------

# Blue population/tickets box (top of the box model). Faithful to the old
# grViz `shape = box, fillcolor = "#bdfeff"` node.
#
# Every dimension is expressed in `em` and anchored to `font_size` (px) on the
# wrapper, so the whole diagram scales as a unit — matching the old grViz SVG,
# which grew with its container. Bump `font_size` to make a given diagram
# bigger/smaller.
box_model_html <- function(box_label, sample_label = NULL, n_label = NULL, font_size = 22) {

  box_style <- paste0(
    "background-color:#bdfeff;",
    "border:1px solid black;",
    "border-radius:0.15em;",
    "padding:0.8em 1.3em;",
    "min-width:13.5em;",
    "max-width:100%;",
    "text-align:center;",
    "white-space:pre-line;",
    "line-height:1.35;",
    "box-sizing:border-box;"
  )

  oval_style <- paste0(
    "background-color:#f9ffbd;",
    "border:1px solid black;",
    "border-radius:50%;",
    "padding:1.05em 1.7em;",
    "min-width:8.5em;",
    "text-align:center;",
    "white-space:pre-line;",
    "line-height:1.35;",
    "box-sizing:border-box;"
  )

  # Downward arrow (vertical stem + CSS triangle head) with the sample-size
  # label sitting to its right, mirroring the grViz edge `box->sample`.
  arrow <- tags$div(
    style = "position:relative; width:2px; height:3em; background:#333333; margin:0.15em 0;",
    tags$div(style = paste0(
      "position:absolute; bottom:-0.4em; left:50%; transform:translateX(-50%);",
      "width:0; height:0; border-left:0.4em solid transparent;",
      "border-right:0.4em solid transparent; border-top:0.55em solid #333333;"
    )),
    if (!is.null(n_label)) tags$div(
      HTML(as.character(n_label)),
      style = paste0(
        "position:absolute; left:0.6em; top:50%; transform:translateY(-50%);",
        "white-space:nowrap; font-size:0.85em;"
      )
    )
  )

  tags$div(
    style = paste0(
      "display:flex; flex-direction:column; align-items:center; padding:0.4em 0;",
      "font-size:", font_size, "px;"
    ),
    tags$div(HTML(as.character(box_label)), style = box_style),
    if (!is.null(sample_label)) arrow,
    if (!is.null(sample_label)) tags$div(HTML(as.character(sample_label)), style = oval_style)
  )
}

# A single yellow rounded "sample" cell (replaces the plaintext HTML-table
# cells that grViz rendered for individual samples).
sample_cell_html <- function(label) {
  tags$div(
    HTML(as.character(label)),
    style = paste0(
      "display:flex; align-items:center; justify-content:center; text-align:center;",
      "width:120px; min-height:40px; padding:4px;",
      "background-color:#f9ffbd; border:1px solid black; border-radius:8px;",
      "font-size:12px; box-sizing:border-box;"
    )
  )
}

# A grid of sample cells arranged in two columns (row-major, matching the old
# grViz 2-column tables of simulated samples).
sample_grid_html <- function(labels) {
  if (length(labels) == 0) return(NULL)
  tags$div(
    style = paste0(
      "display:grid; grid-template-columns:repeat(2, max-content);",
      "gap:12px; justify-content:center; padding:6px 0;"
    ),
    lapply(labels, sample_cell_html)
  )
}

popsd <- function(x) {
  # Remove NA values
  x <- na.omit(x)
  
  # Return NA if input is empty
  n <- length(x)
  if (n == 0) return(NA_real_)  
  
  mean_x <- mean(x)
  sqrt(sum((x - mean_x)^2) / n)
}

gcd <- function(a, b) {
  while (b != 0) {
    temp <- b
    b <- a %% b
    a <- temp
  }
  return(abs(a))
}

# We assume that if mean_or_sample_as_int = 1 then we are talking about sum, and mean_or_sample_as_int = 2 is mean.
simulate_box <- function(mean_or_sample_as_int, n, box) {
  value = sample(box, n, replace = TRUE)
  if (mean_or_sample_as_int == 2) {
    value = mean(value)
  } else {
    value = sum(value)
  }
  return(value)
}
