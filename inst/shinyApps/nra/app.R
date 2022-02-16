#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(nra)
library(tidyverse)
library(shinyalert)
library(kableExtra)
library(DT)
library(htmltools)
library(shinyjs)
library(lubridate)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle <-  "RAPPORTEKET NRA"
logo <- includeHTML(system.file('www/logo.svg', package='rapbase'))
logoCode <- paste0("var header = $('.navbar> .container-fluid');\n",
                   "header.append('<div class=\"navbar-brand\" style=\"float:left;font-size:75%\">",
                   logo,
                   "</div>');\n",
                   "console.log(header)")
logoWidget <- tags$script(shiny::HTML(logoCode))

source(system.file("shinyApps/nra/R/lastshinydata.R", package = "nra"), encoding = 'UTF-8')
source(system.file("shinyApps/nra/R/BrValg.R", package = "nra"), encoding = 'UTF-8')
source(system.file("shinyApps/nra/R/modul_fordelingsfig.R", package = "nra"), encoding = 'UTF-8')
source(system.file("shinyApps/nra/R/modul_gjsn_prepost.R", package = "nra"), encoding = 'UTF-8')
source(system.file("shinyApps/nra/R/modul_datadump.R", package = "nra"), encoding = 'UTF-8')
source(system.file("shinyApps/nra/R/modul_admtab.R", package = "nra"), encoding = 'UTF-8')
source(system.file("shinyApps/nra/R/modul_indikatorfig.R", package = "nra"), encoding = 'UTF-8')

AllData <- lastshinydata()
RegData <- AllData$RegData
Skjemaoversikt <- AllData$Skjemaoversikt
rm(AllData)
BrValg <- BrValg(RegData=RegData)

# Define UI for application
ui <- tagList(
  shinyalert::useShinyalert(),
  shinyjs::useShinyjs(),
  navbarPage(
    title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    tabPanel("Startside",
             rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                          organization = uiOutput("appOrgName"),
                                          addUserInfo = TRUE),
             startside_UI(id = "startside_id")
    ),
    tabPanel("Fordelingsfigurer",
             fordelingsfig_UI(id = "fordelingsfig_id", BrValg = BrValg)
    ),
    tabPanel("Gjennomsnitt/andeler før og etter operasjon",
             gjsn_prepost_UI(id = "gjsn_prepost_id")
    ),
    tabPanel("Indikatorer",
             indikatorfig_UI(id = "indikator_id")
    ),
    tabPanel("Datadump",
             datadump_UI(id = "datadump_id")
    ),
    tabPanel("Administrative tabeller",
             admtab_UI(id = "admtab_id")
    ),

    shiny::navbarMenu("Verktøy",
                      # shiny::tabPanel(
                      #   "Utsending",
                      #   shiny::sidebarLayout(
                      #     shiny::sidebarPanel(
                      #       rapbase::autoReportOrgInput("norgastDispatch"),
                      #       rapbase::autoReportInput("norgastDispatch")
                      #     ),
                      #     shiny::mainPanel(
                      #       rapbase::autoReportUI("norgastDispatch")
                      #     )
                      #   )
                      # ),

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
)

# Define server logic
server <- function(input, output, session) {

  if (rapbase::isRapContext()) {
    rapbase::appLogger(session = session, msg = 'Starter NRA')
    reshID <- rapbase::getUserReshId(session)
    userRole <- rapbase::getUserRole(session)
  } else {
    reshID <- 601225
    userRole <- 'SC'
  }

  # if (userRole != 'SC') {
  #   shiny::hideTab("norgast_app_id", target = "Sykehusvisning")
  # }

  # callModule(startside, "startside_id")
  callModule(fordelingsfig, "fordelingsfig_id", reshID = reshID, RegData = RegData, hvd_session = session)
  callModule(gjsn_prepost, "gjsn_prepost_id", reshID = reshID, RegData = RegData, hvd_session = session)
  callModule(indikatorfig, "indikator_id", RegData = RegData, hvd_session = session)
  callModule(datadump, "datadump_id", reshID = reshID, userRole = userRole, hvd_session = session)
  callModule(admtab, "admtab_id", reshID = reshID, RegData = RegData, userRole = userRole,
             hvd_session = session, skjemaoversikt=Skjemaoversikt)
  # Eksport  #
  rapbase::exportUCServer("nraExport", "nra")
  ## veileding
  rapbase::exportGuideServer("nraExportGuide", "nra")

  ## Stats
  rapbase::statsServer("nraStats", registryName = "nra",
                       eligible = (userRole == "SC"))
  rapbase::statsGuideServer("nraStatsGuide", registryName = "nra")

  #################################################################################################################################

  #Navbarwidget
  output$appUserName <- renderText(rapbase::getUserFullName(session))
  output$appOrgName <- renderText(rapbase::getUserReshId(session))

  # Brukerinformasjon
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = "Den er grei!")
  })


}

# Run the application
shinyApp(ui = ui, server = server)

