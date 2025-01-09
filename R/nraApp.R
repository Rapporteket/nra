#' Run the NRA Shiny Application
#'
#' @return An object representing the NRA app
#' @export

nraApp <- function() {
  shiny::shinyApp(ui = appUi, server = appServer)
}
