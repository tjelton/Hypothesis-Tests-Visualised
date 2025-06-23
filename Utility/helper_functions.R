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

primary_card <- function(title, ..., content_style = NULL, header_colour = "#FFFFFF") {
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
        "color: white;",
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
