#' Launch the Hypothesis Tests Visualised app
#' @export
run_HTPV_app <- function() {
  app_dir <- system.file("app", package = "HypothesisTestsVisualised")
  
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing the package.", call. = FALSE)
  }
  
  shiny::runApp(app_dir, display.mode = "normal")
}
