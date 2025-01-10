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
