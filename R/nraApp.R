#' Run the NRA Shiny Application
#'
#' @return An object representing the NRA app
#' @export

nraApp <- function(logAsJson = TRUE) {
  if (logAsJson) {
    rapbase::loggerSetup()
  }
  shiny::shinyApp(ui = appUi, server = appServer)
}
