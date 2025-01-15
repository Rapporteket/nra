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
  RegData <- nraData$RegData

  BrValg <- BrValg(RegData=RegData)

  rapbase::appLogger(session = session, msg = 'Starter NRA')

  ##############################################################################
  # Startside ##################################################################

  nra::startside_server("startside", usrRole = user$role())

  ##############################################################################

  nra::fordelingsfig_server("fordelingsfig", reshID = user$org(),
                            RegData = req(RegData), hvd_session = session,
                            BrValg = req(BrValg))

  ##############################################################################

  nra::fordelingsfig_prepost_server("fordelingsfig_prepost_id",
                                    reshID = user$org(), RegData = req(RegData),
                                    hvd_session = session, BrValg = req(BrValg))

  ##############################################################################
  # Eksport  ###################################################################
  rapbase::exportUCServer("nraExport", "nra")

  ## veileding
  rapbase::exportGuideServer("nraExportGuide", "nra")

  ##############################################################################



}
