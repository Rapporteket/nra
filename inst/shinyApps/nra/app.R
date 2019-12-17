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

RegData <- lastshinydata()
BrValg <- BrValg(RegData=RegData)

# Define UI for application
ui <- tagList(
  shinyalert::useShinyalert(),
  navbarPage(
  title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
              regTitle),
  windowTitle = regTitle,
  theme = "rap/bootstrap.css",

  tabPanel("Fordelingsfigurer",
           fordelingsfig_UI(id = "fordelingsfig_id", BrValg = BrValg)
  )
)
)

# Define server logic
server <- function(input, output, session) {

  if (rapbase::isRapContext()) {
    raplog::appLogger(session = session, msg = 'Starter NRA')
    reshID <- rapbase::getUserReshId(session)
    userRole <- rapbase::getUserRole(session)
  } else {
    reshID <- 601225
    userRole <- 'SC'
  }

  # if (userRole != 'SC') {
  #   shiny::hideTab("norgast_app_id", target = "Sykehusvisning")
  # }


  callModule(fordelingsfig, "fordelingsfig_id", reshID = reshID, RegData = RegData)


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

