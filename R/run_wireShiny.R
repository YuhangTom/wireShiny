#' Run the Shiny app based on functionalities of `wire`
#'
#' @importFrom shiny runApp
#' @export
#' @examples
#' if (interactive()) {
#'   run_wireShiny()
#' }
#'
run_wireShiny <- function() {
  appDir <- system.file("wireShinyApp", package = "wireShiny")

  if (appDir == "") {
    stop("Could not find the Shiny app directory. Try re-installing `wireShiny`.", call. = FALSE)
  }

  runApp(appDir, display.mode = "normal")
}
