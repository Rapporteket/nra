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

  # Legg til verktøy-fanen for SC-brukere, og fjern den for andre roller
  tool_tabs_added <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(shiny::req(user$role()), {
    if (user$role() == "SC") {
      if (!tool_tabs_added()) {
        shiny::appendTab(
          inputId = "nra_app_id",
          tab = shiny::navbarMenu(
            "Verktøy",
            shiny::tabPanel(
              "Metadata",
              shiny::sidebarLayout(
                shiny::sidebarPanel(shiny::uiOutput("metaControl")),
                shiny::mainPanel(shiny::htmlOutput("metaData"))
              )
            ),
            shiny::tabPanel(
              "Eksport",
              shiny::sidebarLayout(
                shiny::sidebarPanel(
                  rapbase::exportUCInput("nraExport")
                ),
                shiny::mainPanel(
                  rapbase::exportGuideUI("nraExportGuide")
                )
              )
            ),
            shiny::tabPanel(
              "Bruksstatistikk",
              shiny::sidebarLayout(
                shiny::sidebarPanel(rapbase::statsInput("nraStats")),
                shiny::mainPanel(
                  rapbase::statsUI("nraStats"),
                  rapbase::statsGuideUI("nraStatsGuide")
                )
              )
            )
          )
        )
        tool_tabs_added(TRUE)
      }
    } else {
      if (tool_tabs_added()) {
        shiny::removeTab("nra_app_id", target = "Verktøy")
        tool_tabs_added(FALSE)
      }
    }
  })


  #################################################################
  # Innholdsmoduler ###############################################

  nra::startside_server(
    "startside", user = user
  )
  nra::fordelingsfig_server(
    "fordelingsfig",
    RegData = req(RegData),
    BrValg = req(BrValg), user = user
  )
  nra::fordelingsfig_prepost_server(
    "fordelingsfig_prepost_id",
    RegData = req(RegData),
    user = user,
    BrValg = req(BrValg)
  )
  nra::andeler_tid_server(
    "andeler_tid_id",
    RegData = RegData,
    BrValg = req(BrValg), user = user
  )
  nra::gjsn_prepost_server(
    "gjsn_prepost_id", user = user,
    RegData = RegData
  )
  nra::indikatorfig_server(
    "indikator_id", RegData = RegData
  )
  nra::datadump_server(
    "datadump_id", user = user
  )
  nra::admtab_server("admtab_id", RegData = RegData,
                     skjemaoversikt=Skjemaoversikt
  )

  ## Metadata
  meta <- shiny::reactive({
    rapbase::describeRegistryDb("data")
  })

  output$metaControl <- shiny::renderUI({
    tabs <- names(meta())
    selectInput("metaTab", "Velg tabell:", tabs)
  })


  output$metaDataTable <- DT::renderDataTable(
    meta()[[input$metaTab]],
    rownames = FALSE,
    options = list(lengthMenu = c(25, 50, 100, 200, 400))
  )

  output$metaData <- shiny::renderUI({
    DT::dataTableOutput("metaDataTable")
  })

  ## Stats
  observe(
    rapbase::statsServer("nraStats",
                         registryName = "nra",
                         app_id = Sys.getenv("FALK_APP_ID"),
                         eligible = (user$role() == "SC")
    )
  )
  rapbase::statsGuideServer("nraStatsGuide", registryName = "nra")



  # Eksport  ###################################################################
  rapbase::exportUCServer("nraExport", "nra")

  ## veileding
  rapbase::exportGuideServer("nraExportGuide", "nra")

  ##############################################################################



}
