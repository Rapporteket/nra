# Modul for fordelingsfigurer i NRA sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# @inheritParams nraFigAndeler
#
# @return Serverdelen av fordelingsfigur
#
#
andeler_tid_ui <- function(id, BrValg){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      selectInput(inputId = ns("valgtVar"), label = "Velg variabel", choices = c("tidligereKonservativ")),
      dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til",
                     max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), language = "nb", separator = " til "),
      selectInput(inputId = ns("enhetsUtvalg"), label = "Lag figur for",
                  choices = c('Hele landet'=0, 'Egen avd. mot landet forøvrig'=1, 'Egen avd.'=2)),
      selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                  choices = BrValg$sykehus, multiple = TRUE),
      selectInput(inputId = ns("tidsenhet"),
                  label = "Velg tidsenhet",
                  choices = c('Aar', 'Mnd', 'Kvartal', 'Halvaar'),
                  selected = 'Kvartal'),
      selectInput(inputId = ns("inkl_konf"), label = "Inkluder konfidensintervall",
                  choices = c('Ja'=1, 'Nei'=0)),
      sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                  max = 130, value = c(0, 130)),
      selectInput(inputId = ns("erMann"), label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      selectInput(inputId = ns("forlopstype1"), label = "Velg operasjonstype",
                  choices = c('Sfinkterplastikk'=1, 'SNM'=2),
                  multiple = TRUE),
      uiOutput(outputId = ns('forlopstype2')),
      shiny::selectInput(inputId = ns("onestage"), label = "One stage",
                         choices = c('--'=99, 'Ja'=1, 'Nei'=0), selected = 99),
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
                           tableOutput(ns("Tabell_tid")), downloadButton(ns("lastNed"), "Last ned tabell")
                  )
      )
    )
  )
}


andeler_tid <- function(input, output, session, reshID, RegData, hvd_session){

  output$forlopstype2 <- renderUI({
    ns <- session$ns
    if (!is.null(input$forlopstype1)) {
      selectInput(inputId = ns("forlopstype2_verdi"), label = "SNM-type",
                  choices = if (2 %in% as.numeric(input$forlopstype1)) {
                    if (1 %in% as.numeric(input$forlopstype1)) {
                      c('Test usikker'=1, 'Test positiv'=2, 'Revisjon'=3,
                        'Eksplantasjon'=4, 'Test negativ'=5, 'Sfinkterplastikk'=NA)
                    } else {
                      c('Test usikker'=1, 'Test positiv'=2, 'Revisjon'=3,
                        'Eksplantasjon'=4, 'Test negativ'=5)
                    }
                  },
                  multiple = TRUE)
    }
  })


  output$Figur1 <- renderPlot({
    nra::nraFigAndelTid(RegData = RegData, valgtVar = input$valgtVar,
                        minald=as.numeric(input$alder[1]),
                        maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1],
                        datoTil = input$datovalg[2],
                        valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
                        outfile = '',
                        tidsenhet = if (!is.null(input$tidsenhet)) {input$tidsenhet} else {99},
                        inkl_konf = if (!is.null(input$inkl_konf)) {input$inkl_konf} else {99},
                        erMann = as.numeric(input$erMann), reshID = reshID,
                        enhetsUtvalg = input$enhetsUtvalg,
                        forlopstype1=if(!is.null(input$forlopstype1)){as.numeric(input$forlopstype1)} else {99},
                        forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)} else {99},
                        onestage = if(!is.null(input$onestage)){as.numeric(input$onestage)} else {99})
  }, width = 700, height = 700)


  tabellReagerTid <- reactive({
    TabellData <- nraFigAndelTid(RegData = RegData, valgtVar = input$valgtVar,
                                 minald=as.numeric(input$alder[1]),
                                 maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1],
                                 datoTil = input$datovalg[2],
                                 valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
                                 outfile = '',
                                 tidsenhet = if (!is.null(input$tidsenhet)) {input$tidsenhet} else {99},
                                 inkl_konf = if (!is.null(input$inkl_konf)) {input$inkl_konf} else {99},
                                 erMann = as.numeric(input$erMann), reshID = reshID,
                                 enhetsUtvalg = input$enhetsUtvalg,
                                 forlopstype1=if(!is.null(input$forlopstype1)){as.numeric(input$forlopstype1)} else {99},
                                 forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)} else {99},
                                 onestage = if(!is.null(input$onestage)){as.numeric(input$onestage)} else {99})
  })

  output$utvalg <- renderUI({
    TabellData <- tabellReagerTid()
    tagList(
      h3(HTML(paste0(TabellData$tittel, '<br />'))),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})



  output$Tabell_tid <- function() {

    utdata <- tabellReagerTid()
    if (input$enhetsUtvalg == 1) {
      Tabell_tid <- tibble(Tidsperiode = utdata$Tidtxt, Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                           N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                           Konf.int.ovre = utdata$KonfInt$Konf[2,], Antall2 = round(utdata$Andeler$AndelRest*utdata$NTid$NTidRest/100),
                           N2 = utdata$NTid$NTidRest, Andel2 = utdata$Andeler$AndelRest, Konf.int.nedre2 = utdata$KonfInt$KonfRest[1,],
                           Konf.int.ovre2 = utdata$KonfInt$KonfRest[2,])
      names(Tabell_tid) <- c('Tidsperiode', 'Antall', 'N', 'Andel (%)', 'KI_nedre', 'KI_ovre', 'Antall', 'N', 'Andel (%)',
                             'KI_nedre', 'KI_ovre')
      Tabell_tid %>% knitr::kable("html", digits = c(0,0,0,1,1,1,0,0,1,1,1)) %>%
        kable_styling("hover", full_width = F) %>%
        add_header_above(c(" ", "Din avdeling" = 5, "Landet forøvrig" = 5))
    } else {
      Tabell_tid <- tibble(Tidsperiode = utdata$Tidtxt,
                           Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                           N = utdata$NTid$NTidHoved, 'Andel (%)'= utdata$Andeler$AndelHoved, KI_nedre = utdata$KonfInt$Konf[1,],
                           KI_ovre = utdata$KonfInt$Konf[2,])
      Tabell_tid %>%
        knitr::kable("html", digits = c(0,0,0,1,1,1)) %>%
        kable_styling("hover", full_width = F)
    }
  }

  output$lastNed <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, '_tid', Sys.time(), '.csv')
    },
    content = function(file){
      utdata <- tabellReagerTid()
      if (input$enhetsUtvalg == 1) {
        Tabell_tid <- tibble(Tidsperiode = utdata$Tidtxt, Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                             N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                             Konf.int.ovre = utdata$KonfInt$Konf[2,], Antall2 = round(utdata$Andeler$AndelRest*utdata$NTid$NTidRest/100),
                             N2 = utdata$NTid$NTidRest, Andel2 = utdata$Andeler$AndelRest, Konf.int.nedre2 = utdata$KonfInt$KonfRest[1,],
                             Konf.int.ovre2 = utdata$KonfInt$KonfRest[2,])
      } else {
        Tabell_tid <- tibble(Tidsperiode = utdata$Tidtxt,
                             Antall = round(utdata$Andeler$AndelHoved*utdata$NTid$NTidHoved/100),
                             N = utdata$NTid$NTidHoved, Andel = utdata$Andeler$AndelHoved, Konf.int.nedre = utdata$KonfInt$Konf[1,],
                             Konf.int.ovre = utdata$KonfInt$Konf[2,])
      }
      write.csv2(Tabell_tid, file, row.names = F, fileEncoding = "Latin1")
    }
  )

  output$lastNedBilde <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, '_tid', Sys.time(), '.', input$bildeformat)
    },
    content = function(file){
      nra::nraFigAndelTid(RegData = RegData, valgtVar = input$valgtVar,
                          minald=as.numeric(input$alder[1]),
                          maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1],
                          datoTil = input$datovalg[2],
                          valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
                          outfile = file,
                          tidsenhet = if (!is.null(input$tidsenhet)) {input$tidsenhet} else {99},
                          inkl_konf = if (!is.null(input$inkl_konf)) {input$inkl_konf} else {99},
                          erMann = as.numeric(input$erMann), reshID = reshID,
                          enhetsUtvalg = input$enhetsUtvalg,
                          forlopstype1=if(!is.null(input$forlopstype1)){as.numeric(input$forlopstype1)} else {99},
                          forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)} else {99},
                          onestage = if(!is.null(input$onestage)){as.numeric(input$onestage)} else {99})
    }
  )

  shiny::observe({
    if (rapbase::isRapContext()) {
      if (req(input$tab) == "fig") {
        mld_fordeling <- paste0(
          "NRA: Figur - tidsvisning, variabel - ",
          input$valgtVar)
      }
      if (req(input$tab) == "tab") {
        mld_fordeling <- paste(
          "NRA: tabell - tidsvisning variabel - ",
          input$valgtVar)
      }
      rapbase::repLogger(
        session = hvd_session,
        msg = mld_fordeling
      )
      shinyjs::onclick(
        "lastNedBilde",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste(
            "NRA: nedlasting figur - tidsvisning, variabel -",
            input$valgtVar
          )
        )
      )
      shinyjs::onclick(
        "lastNed",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste(
            "NRA: nedlasting tabell - tidsvisning, variabel -",
            input$valgtVar
          )
        )
      )
    }
  })

}
