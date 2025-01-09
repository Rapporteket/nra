#' Server logic for the norgast app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

appServer <- function(input, output, session) {

  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "nra",
    caller = "nra"
  )

  nraData <-  nra::lastShinyData()

  # rapbase::appLogger(session = session, msg = 'Starter NRA')

  ##############################################################################
  # Startside ##################################################################

  nra::startside_server("startside", usrRole = user$role())

  ##############################################################################


  ##############################################################################
  # Eksport  ###################################################################
  rapbase::exportUCServer("nraExport", "nra")

  ## veileding
  rapbase::exportGuideServer("nraExportGuide", "nra")

  ##############################################################################



}
