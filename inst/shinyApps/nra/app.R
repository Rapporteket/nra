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

AllData <- lastshinydata()
RegData <- AllData$RegData
Skjemaoversikt <- AllData$Skjemaoversikt
rm(AllData)
BrValg <- BrValg(RegData=RegData)

# Define UI for application
ui <- tagList(
  shinyalert::useShinyalert(),
  navbarPage(
    title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    tabPanel("Startside",
             mainPanel(
               shinyjs::useShinyjs(),
               shinyalert::useShinyalert(),
               rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                            organization = uiOutput("appOrgName"),
                                            addUserInfo = TRUE),

               h2('Velkommen til Rapporteket - NRA', align='center'),
               br(),
               # h4(tags$b('Her skal Tone og Stig formulere kloke og reflekterte meldinger til Rapportekets brukere. En foreløpig variant er gitt under:')),
               # br(),
               h4('Du er nå inne på Rapporteket for NRA, registerets resultattjeneste.
                Disse sidene inneholder en samling av figurer og tabeller som viser resultater fra registeret.
                På hver av sidene kan man gjøre utvalg i menyene til venstre. Alle resultater er basert
                på ferdigstilte registreringer. Merk at data er hentet direkte fra registerets database.
                Dette medfører at nyere data ikke er kvalitetssikret ennå.'),
               h4('Du kan se på resultater for eget sykehus, nasjonale data og eget sykehus sett opp mot landet for øvrig.
                Alle figurer og
                tabeller kan lastes ned.'),
               br(),
               h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
               h4(tags$b('Fordelinger '), 'viser fordelinger (figur/tabell) av ulike variabler.
                Man kan velge hvilken variabel man vil se på, og man kan gjøre ulike filtreringer.'),
               br(),
               h4(tags$b('Gjennomsnitt/andeler før og etter operasjon '), 'viser gjennomsnitt eller andel av en variabel. Kan vise enten kun pre-data,
                pre og 1-årsoppfølgingsdata, eller pre-og 1 og 5-årsoppfølgingsdata'),
               br(),
               h4(tags$b('Datadump '), 'gir mulighet til å laste ned din egen avdelings registreringer.'),
               br(),
               h4(tags$b('Administrative tabeller '), 'er en samling oversikter over antall registreringer.'),
               br(),
               # br(),
               # h3('HER KAN MAN F.EKS. VISE ANTALL REGISTRERINGER SISTE X MND.'),
               # br(),
               br(),
               h4('Oversikt over registerets kvalitetsindikatorer og resultater finner du på www.kvalitetsregistre.no:', #helpText
                  a("NRA", href="https://www.kvalitetsregistre.no/registers/541/resultater"),
                  target="_blank", align='center'),
               br(),
               h4('Mer informasjon om registeret finnes på NRA sin ',
                  a("hjemmeside", href="https://unn.no/fag-og-forskning/medisinske-kvalitetsregistre/nra-norsk-register-for-analinkontinens", target="_blank"),
                  align='center')
             )

    ),


    tabPanel("Fordelingsfigurer",
             fordelingsfig_UI(id = "fordelingsfig_id", BrValg = BrValg)
    ),
    tabPanel("Gjennomsnitt/andeler før og etter operasjon",
             gjsn_prepost_UI(id = "gjsn_prepost_id")
    ),
    tabPanel("Datadump",
             datadump_UI(id = "datadump_id")
    ),
    tabPanel("Administrative tabeller",
             admtab_UI(id = "admtab_id")
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
  callModule(gjsn_prepost, "gjsn_prepost_id", reshID = reshID, RegData = RegData)
  callModule(datadump, "datadump_id", reshID = reshID, userRole = userRole, hvd_session = session)
  callModule(admtab, "admtab_id", reshID = reshID, RegData = RegData, userRole = userRole, hvd_session = session, skjemaoversikt=Skjemaoversikt)

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

