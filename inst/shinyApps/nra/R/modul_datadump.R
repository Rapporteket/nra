# Modul for datadump-fane i NRA sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#

datadump_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      id = ns("id_dump_panel"),
      # uiOutput(outputId = ns('valgtevar_dump')),
      dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til", min = '2014-01-01', language = "nb",
                     max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), separator = " til "),
      selectInput(inputId = ns("dumptype"), label = "Velg type datadump",
                  choices = c('alleVar', 'alleVarNum', 'ForlopsOversikt', 'SkjemaOversikt')),
      # selectInput(inputId = ns("op_gruppe"), label = "Velg reseksjonsgruppe(r)",
      #             choices = BrValg$reseksjonsgrupper, multiple = TRUE),
      # uiOutput(outputId = ns('ncsp')),
      # selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
      #             choices = BrValg$sykehus, multiple = TRUE),
      tags$hr(),
      # actionButton(ns("reset_input"), "Nullstill valg"),
      downloadButton(ns("lastNed_dump"), "Last ned datadump")
    ),
    mainPanel(
      h2('Datadump - NRA', align='center'),
      br(),
      h4('Her kan du laste ned forskjellige varianter av datadump for NRA. Lokale brukere vil bare kunne laste ned data for egen avdeling.'),
      br(),
      h4(tags$b(tags$u('Forklaring til de ulike datadump-typene:'))),
      h4(tags$b('alleVar '), 'inneholder alle kliniske variabler i registeret og benytter etikettene til kategoriske variabler.'),
      h4(tags$b('alleVarNum '), 'inneholder alle kliniske variabler i registeret og benytter tallkodene til kategoriske variabler.'),
      h4(tags$b('ForlopsOversikt '), 'inneholder en del administrative data relevant for forløpene.'),
      h4(tags$b('SkjemaOversikt '), 'er en oversikt over status til alle registreringer i registreret, også uferdige.')
    )
  )
}


datadump <- function(input, output, session, reshID, userRole, hvd_session){

  # observeEvent(input$reset_input, {
  #   shinyjs::reset("id_dump_panel")
  # })

  # observe(
  #   if (userRole != 'SC') {
  #     shinyjs::hide(id = 'valgtShus')
  #   })


  output$lastNed_dump <- downloadHandler(
    filename = function(){
      paste0(input$dumptype, '_NRA', Sys.time(), '.csv')
    },
    content = function(file){
      if (rapbase::isRapContext()) {
        tmpData <- nraHentTabell(input$dumptype)
      } else {
        tmpData <- read.table(paste0('I:/nra/', input$dumptype, '2020-01-07.txt'), header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
      }
      dumpdata <- tmpData[as.Date(tmpData$HovedDato) >= input$datovalg[1] &
                            as.Date(tmpData$HovedDato) <= input$datovalg[2], ]
      if (userRole != 'SC') {
        dumpdata <- dumpdata[dumpdata$AvdRESH == reshID, ]
      }
      # } else {
      #   if (!is.null(input$valgtShus)) {dumpdata <- dumpdata[dumpdata$AvdRESH %in% as.numeric(input$valgtShus), ]}
      # }

      # if (!is.null(input$op_gruppe)) {dumpdata <- dumpdata[which(dumpdata$Op_gr %in% as.numeric(input$op_gruppe)), ]}
      # if (!is.null(input$ncsp_verdi)) {dumpdata <- dumpdata[which(substr(dumpdata$Hovedoperasjon, 1, 5) %in% ncsp_verdi), ]}
      # if (!is.null(input$valgtevar_dump_verdi)) {dumpdata <- dumpdata[, input$valgtevar_dump_verdi]}

      write.csv2(dumpdata, file, row.names = F, na = '')
    }
  )



}
