# Modul for fordelingsfigurer i NRA sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# @inheritParams nraFigAndeler
#
# @return Serverdelen av fordelingsfigur
#
#
gjsn_prepost_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      selectInput(inputId = ns("valgtVar"), label = "Velg variabel", choices =
                    c('St. Marks score'='StMarksTotalScore',
                      'Wexner score'='WexnerTotalScore',
                      'Inkontinensscore'='InkontinensScore',
                      'Generell livskvalitet'='GenQol',
                      'Påvirkning seksualliv'='QolSexualitet',
                      'Andel urininkontinente'='Urinlekkasje_v2',
                      'EQ5D Skore' = 'EQ5DSkore',
                      'EQ5D Helsetilstand' = 'EQ5DHelsetilstand')),
      dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til",
                     max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), language = "nb", separator = " til "),
      selectInput(inputId = ns("sammenlign"), label = "Sammenlign med oppfølging", choices =
                    c('Kun pre'=0, 'Pre og 1-årsoppfølging'=1,
                      'Pre 1- og 5-årsoppfølging'=2, 'Pre og 5-årsoppfølging'=3)),
      sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                  max = 130, value = c(0, 130)),
      selectInput(inputId = ns("erMann"), label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      selectInput(inputId = ns("forlopstype1"), label = "Velg operasjonstype",
                  choices = c('--'=99, 'Sfinkterplastikk'=1, 'SNM'=2)),
      uiOutput(outputId = ns('forlopstype2')),
      shiny::selectInput(inputId = ns("onestage"), label = "One stage",
                         choices = c('--'=99, 'Ja'=1, 'Nei'=0), selected = 99),
      selectInput(inputId = ns("gr_var"), label = "Grupperingsvariabel",
                  choices = c('SenterKortNavn', 'Aar')),
      selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = ns("tab"),
        tabPanel("Figur", value = "fig",
                 plotOutput(ns("Figur1"), height="auto"), downloadButton(ns("lastNedBilde"), "Last ned figur")),
        tabPanel("Tabell", value = "tab",
                 uiOutput(ns("utvalg")),
                 tableOutput(ns("Tabell1")), downloadButton(ns("lastNed"), "Last ned tabell")
        )
      )
    )
  )
}


gjsn_prepost <- function(input, output, session, reshID, RegData, hvd_session){

  output$forlopstype2 <- renderUI({
    ns <- session$ns
    if (as.numeric(input$forlopstype1)!=1) {
      selectInput(inputId = ns("forlopstype2_verdi"), label = "SNM-type",
                  choices = if (as.numeric(input$forlopstype1)!=1) {
                    c('Test usikker'=1, 'Test positiv'=2, 'Revisjon'=3, 'Eksplantasjon'=4, 'Test negativ'=5)
                  },  multiple = TRUE)
    }
  })


  output$Figur1 <- renderPlot({
    nraGjsnPrePost(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                   maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                   grvar=input$gr_var, outfile = '', preprosess=F, erMann = as.numeric(input$erMann), sammenlign=as.numeric(input$sammenlign),
                   reshID = reshID, hentData=F, forlopstype1=as.numeric(input$forlopstype1),
                   forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)} else {99},
                   onestage = if(!is.null(input$onestage)){as.numeric(input$onestage)} else {99})
  }, width = 700, height = 700)


  tabellReager <- reactive({
    TabellData <- nraGjsnPrePost(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                                 maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                 grvar='SenterKortNavn', outfile = '', preprosess=F, erMann = as.numeric(input$erMann), sammenlign=as.numeric(input$sammenlign),
                                 reshID = reshID, hentData=F, forlopstype1=as.numeric(input$forlopstype1),
                                 forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)} else {99},
                                 onestage = if(!is.null(input$onestage)){as.numeric(input$onestage)} else {99})
  })

  output$utvalg <- renderUI({
    TabellData <- tabellReager()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})



  output$Tabell1 <- function() {
    TabellData <- tabellReager()
    if (input$valgtVar == 'Urinlekkasje_v2') {
      if (as.numeric(input$sammenlign) == 0) {
        Tabell1 <- tibble(Sykehus = TabellData$grtxt, Antall = round(as.numeric(TabellData$PlotMatrise)*TabellData$Ngr/100),
                          'Andel (%)' = as.numeric(TabellData$PlotMatrise), N = TabellData$Ngr) %>%
          knitr::kable("html", digits = c(0,0,1,0)) %>%
          kable_styling("hover", full_width = F)
      } else {
        if (as.numeric(input$sammenlign) == 1) {
          Tabell1 <- tibble(Sykehus = TabellData$grtxt, Antall = round(as.numeric(TabellData$PlotMatrise[1,])*TabellData$Ngr/100),
                 'Andel (%)' = as.numeric(TabellData$PlotMatrise[1,]),
                 Antall = round(as.numeric(TabellData$PlotMatrise[2,])*TabellData$Ngr/100),
                 'Andel (%)' = as.numeric(TabellData$PlotMatrise[2,]),
                 N = TabellData$Ngr, .name_repair = "minimal") %>% knitr::kable("html", digits = c(0,0,1,0,1,0)) %>%
            kable_styling("hover", full_width = F) %>%
            add_header_above(c(" ", "Før intervensjon" = 2, "1. årskontroll" = 2, " "))
        } else {
          if (as.numeric(input$sammenlign) == 2) {
            Tabell1 <- tibble(Sykehus = TabellData$grtxt, Antall = round(as.numeric(TabellData$PlotMatrise[1,])*TabellData$Ngr/100),
                   'Andel (%)' = as.numeric(TabellData$PlotMatrise[1,]),
                   Antall = round(as.numeric(TabellData$PlotMatrise[2,])*TabellData$Ngr/100),
                   'Andel (%)' = as.numeric(TabellData$PlotMatrise[2,]),
                   Antall = round(as.numeric(TabellData$PlotMatrise[3,])*TabellData$Ngr/100),
                   'Andel (%)' = as.numeric(TabellData$PlotMatrise[3,]),
                   N = TabellData$Ngr, .name_repair = "minimal") %>% knitr::kable("html", digits = c(0,0,1,0,1,0,1,0)) %>%
              kable_styling("hover", full_width = F) %>%
              add_header_above(c(" ", "Før intervensjon" = 2, "1-årskontroll" = 2, "5-årskontroll" = 2, " "))
          }
        }
      }
    } else {
      if (as.numeric(input$sammenlign) == 0) {
        Tabell1 <- tibble(Sykehus = TabellData$grtxt, gj.sn = as.numeric(TabellData$PlotMatrise),
                          KI = paste0(sprintf("%.1f", as.numeric(TabellData$KIned)), '-', sprintf("%.1f", as.numeric(TabellData$KIopp))),
                          N = TabellData$Ngr) %>%
          knitr::kable("html", digits = c(0,1,0,0)) %>%
          kable_styling("hover", full_width = F)
      } else {
        if (as.numeric(input$sammenlign) == 1) {
          Tabell1 <- tibble(Sykehus = TabellData$grtxt, gj.sn. = as.numeric(TabellData$PlotMatrise[1,]),
                 KI = paste0(sprintf("%.1f", as.numeric(TabellData$KIned[1,])), '-', sprintf("%.1f", as.numeric(TabellData$KIopp[1,]))),
                 gj.sn. = as.numeric(TabellData$PlotMatrise[2,]),
                 KI = paste0(sprintf("%.1f", as.numeric(TabellData$KIned[2,])), '-', sprintf("%.1f", as.numeric(TabellData$KIopp[2,]))),
                 N = TabellData$Ngr, .name_repair = "minimal") %>% knitr::kable("html", digits = c(0,1,0,1,0,0)) %>%
            kable_styling("hover", full_width = F) %>%
            add_header_above(c(" ", "Før intervensjon" = 2, "1. årskontroll" = 2, " "))
        } else {
          if (as.numeric(input$sammenlign) == 2) {
            Tabell1 <- tibble(Sykehus = TabellData$grtxt, gj.sn. = as.numeric(TabellData$PlotMatrise[1,]),
                   KI = paste0(sprintf("%.1f", as.numeric(TabellData$KIned[1,])), '-', sprintf("%.1f", as.numeric(TabellData$KIopp[1,]))),
                   gj.sn. = as.numeric(TabellData$PlotMatrise[2,]),
                   KI = paste0(sprintf("%.1f", as.numeric(TabellData$KIned[2,])), '-', sprintf("%.1f", as.numeric(TabellData$KIopp[2,]))),
                   gj.sn. = as.numeric(TabellData$PlotMatrise[3,]),
                   KI = paste0(sprintf("%.1f", as.numeric(TabellData$KIned[3,])), '-', sprintf("%.1f", as.numeric(TabellData$KIopp[3,]))),
                   N = TabellData$Ngr, .name_repair = "minimal") %>% knitr::kable("html", digits = c(0,1,0,1,0,1,0,0)) %>%
              kable_styling("hover", full_width = F) %>%
              add_header_above(c(" ", "Før intervensjon" = 2, "1-årskontroll" = 2, "5-årskontroll" = 2, " "))
          }
        }
      }
    }
  }

  output$lastNed <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- tabellReager()
      if (input$valgtVar == 'Urinlekkasje') {
        if (as.numeric(input$sammenlign) == 0) {
          Tabell1 <- tibble(Sykehus = TabellData$grtxt, Antall = round(as.numeric(TabellData$PlotMatrise)*TabellData$Ngr/100),
                            'Andel (%)' = as.numeric(TabellData$PlotMatrise), N = TabellData$Ngr)
        } else {
          if (as.numeric(input$sammenlign) == 1) {
            Tabell1 <- tibble(Sykehus = TabellData$grtxt, Antall = round(as.numeric(TabellData$PlotMatrise[1,])*TabellData$Ngr/100),
                   'Andel (%)' = as.numeric(TabellData$PlotMatrise[1,]),
                   Antall = round(as.numeric(TabellData$PlotMatrise[2,])*TabellData$Ngr/100),
                   'Andel (%)' = as.numeric(TabellData$PlotMatrise[2,]),
                   N = TabellData$Ngr, .name_repair = "minimal")
          } else {
            if (as.numeric(input$sammenlign) == 2) {
              Tabell1 <- tibble(Sykehus = TabellData$grtxt, Antall = round(as.numeric(TabellData$PlotMatrise[1,])*TabellData$Ngr/100),
                     'Andel (%)' = as.numeric(TabellData$PlotMatrise[1,]),
                     Antall = round(as.numeric(TabellData$PlotMatrise[2,])*TabellData$Ngr/100),
                     'Andel (%)' = as.numeric(TabellData$PlotMatrise[2,]),
                     Antall = round(as.numeric(TabellData$PlotMatrise[3,])*TabellData$Ngr/100),
                     'Andel (%)' = as.numeric(TabellData$PlotMatrise[3,]),
                     N = TabellData$Ngr, .name_repair = "minimal")
            }
          }
        }
      } else {
        if (as.numeric(input$sammenlign) == 0) {
          Tabell1 <- tibble(Sykehus = TabellData$grtxt, gj.sn = as.numeric(TabellData$PlotMatrise),
                            KI = paste0(sprintf("%.1f", as.numeric(TabellData$KIned)), '-', sprintf("%.1f", as.numeric(TabellData$KIopp))),
                            N = TabellData$Ngr)
        } else {
          if (as.numeric(input$sammenlign) == 1) {
            Tabell1 <- tibble(Sykehus = TabellData$grtxt, gj.sn. = as.numeric(TabellData$PlotMatrise[1,]),
                   KI = paste0(sprintf("%.1f", as.numeric(TabellData$KIned[1,])), '-', sprintf("%.1f", as.numeric(TabellData$KIopp[1,]))),
                   gj.sn. = as.numeric(TabellData$PlotMatrise[2,]),
                   KI = paste0(sprintf("%.1f", as.numeric(TabellData$KIned[2,])), '-', sprintf("%.1f", as.numeric(TabellData$KIopp[2,]))),
                   N = TabellData$Ngr, .name_repair = "minimal")
          } else {
            if (as.numeric(input$sammenlign) == 2) {
              Tabell1 <- tibble(Sykehus = TabellData$grtxt, gj.sn. = as.numeric(TabellData$PlotMatrise[1,]),
                     KI = paste0(sprintf("%.1f", as.numeric(TabellData$KIned[1,])), '-', sprintf("%.1f", as.numeric(TabellData$KIopp[1,]))),
                     gj.sn. = as.numeric(TabellData$PlotMatrise[2,]),
                     KI = paste0(sprintf("%.1f", as.numeric(TabellData$KIned[2,])), '-', sprintf("%.1f", as.numeric(TabellData$KIopp[2,]))),
                     gj.sn. = as.numeric(TabellData$PlotMatrise[3,]),
                     KI = paste0(sprintf("%.1f", as.numeric(TabellData$KIned[3,])), '-', sprintf("%.1f", as.numeric(TabellData$KIopp[3,]))),
                     N = TabellData$Ngr, .name_repair = "minimal")
            }
          }
        }
      }
      write.csv2(Tabell1, file, row.names = F)
    }
  )

  output$lastNedBilde <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, Sys.time(), '.', input$bildeformat)
    },

    content = function(file){
      nra::nraGjsnPrePost(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                     maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                     grvar='SenterKortNavn', preprosess=F, erMann = as.numeric(input$erMann), sammenlign=as.numeric(input$sammenlign),
                     reshID = reshID, hentData=F, forlopstype1=as.numeric(input$forlopstype1),
                     forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)} else {99}, outfile = file,
                     onestage = if(!is.null(input$onestage)){as.numeric(input$onestage)} else {99})
    }
  )

  shiny::observe({
    if (rapbase::isRapContext()) {
      if (req(input$tab) == "fig") {
        mld_fordeling <- paste0(
          "NRA: Figur - gj.sn. pre-post, variabel - ",
          input$valgtVar)
      }
      if (req(input$tab) == "tab") {
        mld_fordeling <- paste(
          "NRA: tabell - gj.sn. pre-post, variabel - ",
          input$valgtVar)
      }
      rapbase::repLogger(
        session = hvd_session,
        msg = mld_fordeling
      )
      mldLastNedFig <- paste(
        "NRA: nedlasting figur - gj.sn. pre-post variabel -",
        input$valgtVar
      )
      mldLastNedTab <- paste(
        "NRA: nedlasting tabell - gj.sn. pre-post variabel -",
        input$valgtVar
      )
      shinyjs::onclick(
        "lastNedBilde",
        rapbase::repLogger(
          session = hvd_session,
          msg = mldLastNedFig
        )
      )
      shinyjs::onclick(
        "lastNed",
        rapbase::repLogger(
          session = hvd_session,
          msg = mldLastNedTab
        )
      )
    }
  })



}
