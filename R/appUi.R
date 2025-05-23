#' Client (ui) for the nra app
#'
#' @return An shiny app ui object
#' @export

appUi <- function() {

  shiny::addResourcePath('rap', system.file('www', package='rapbase'))
  regTitle = "NRA"

  # Define UI for application
  ui <- shiny::navbarPage(
    shinyjs::useShinyjs(),
    id = "nra_app_id",
    title = shiny::div(shiny::a(
      shiny::includeHTML(
        system.file('www/logo.svg', package='rapbase')
      )
    ),
    regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    shiny::tabPanel(
      "Startside",
      rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),
      nra::startside_ui("startside")
    ),
    shiny::tabPanel(
      "Fordelingsfigurer",
      nra::fordelingsfig_ui("fordelingsfig")
    ),
    shiny::tabPanel(
      "Fordelingsfigurer - Før og etter",
      nra::fordelingsfig_prepost_ui(id = "fordelingsfig_prepost_id")
    ),
    shiny::tabPanel(
      "Tidsutvikling andeler",
             nra::andeler_tid_ui(id = "andeler_tid_id")
    ),
    tabPanel("Gjennomsnitt/andeler før og etter operasjon",
             gjsn_prepost_ui(id = "gjsn_prepost_id")
    ),
    tabPanel("Indikatorer",
             indikatorfig_ui(id = "indikator_id")
    ),
    tabPanel("Datadump",
             datadump_ui(id = "datadump_id")
    ),
    tabPanel("Administrative tabeller",
             admtab_ui(id = "admtab_id")
    ),

    shiny::navbarMenu(
      "Verktøy",

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

}
