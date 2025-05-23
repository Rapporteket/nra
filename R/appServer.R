#' Server logic for the nra app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

appServer <- function(input, output, session) {
  rapbase::logShinyInputChanges(input)
  nraData <-  nra::lastShinyData()
  RegData <- nraData$RegData
  Skjemaoversikt <- nraData$Skjemaoversikt
  map_avdeling <- data.frame(
    UnitId = unique(RegData$AvdRESH),
    orgname = RegData$SenterKortNavn[match(unique(RegData$AvdRESH),
                                           RegData$AvdRESH)])

  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "nra",
    caller = "nra",
    map_orgname = shiny::req(map_avdeling)
  )

  BrValg <- BrValg(RegData=RegData)

  rapbase::appLogger(session = session, msg = 'Starter NRA')

  shiny::observeEvent(
    shiny::req(user$role()), {
      if (user$role() != 'SC') {
        # shiny::hideTab("nra_app_id", target = "Utsending")
        shiny::hideTab("nra_app_id", target = "Verktøy")
      } else {
        # shiny::showTab("nra_app_id", target = "Utsending")
        shiny::showTab("nra_app_id", target = "Verktøy")
      }
    })


  ##############################################################################
  # Innholdsmoduler ############################################################

  nra::startside_server(
    "startside", usrRole = user$role
  )
  nra::fordelingsfig_server(
    "fordelingsfig", reshID = user$org,
    RegData = req(RegData), hvd_session = session,
    BrValg = req(BrValg), userRole = user$role
  )
  nra::fordelingsfig_prepost_server(
    "fordelingsfig_prepost_id",
    reshID = user$org, RegData = req(RegData),
    hvd_session = session, userRole = user$role,
    BrValg = req(BrValg)
  )
  nra::andeler_tid_server(
    "andeler_tid_id", reshID = user$org,
    RegData = RegData, hvd_session = session,
    BrValg = req(BrValg), userRole = user$role
  )
  nra::gjsn_prepost_server(
    "gjsn_prepost_id", reshID = user$org,
    RegData = RegData, hvd_session = session
  )
  nra::indikatorfig_server(
    "indikator_id", RegData = RegData,
    hvd_session = session
  )
  nra::datadump_server(
    "datadump_id", reshID = user$org,
    userRole = user$role, hvd_session = session
  )
  nra::admtab_server("admtab_id", RegData = RegData,
                     hvd_session = session, skjemaoversikt=Skjemaoversikt
  )

  # Eksport  ###################################################################
  rapbase::exportUCServer("nraExport", "nra")

  ## veileding
  rapbase::exportGuideServer("nraExportGuide", "nra")

  ##############################################################################



}
