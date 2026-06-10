#' Run the ShinyGLM app
#'
#' @export

run_app <- function() {
  # need to call library on shinyBS directly otherwise package is not attached
  suppressPackageStartupMessages(library(shinyBS))

  shiny::addResourcePath("www", system.file("www", package = "ShinyGLM"))

  shiny::shinyApp(
    ui = app_ui(),
    server = app_server
  )
}
