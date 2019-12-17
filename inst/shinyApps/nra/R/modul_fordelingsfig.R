# Modul for fordelingsfigurer i NRA sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
# @inheritParams nraFigAndeler
#
# @return Serverdelen av fordelingsfigur
#
#
fordelingsfig_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      selectInput(inputId = ns("valgtVar"), label = "Velg variabel", choices = BrValg$varvalg),
      dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til", min = '2014-01-01',
                     max = Sys.Date(), start  = '2014-01-01', end = Sys.Date(), language = "nb", separator = " til "),
      selectInput(inputId = ns("enhetsUtvalg"), label = "Lag figur for",
                  choices = c('Hele landet'=0, 'Egen avd. mot landet forøvrig'=1, 'Egen avd.'=2)),
      selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                  choices = BrValg$sykehus, multiple = TRUE),
      sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                  max = 130, value = c(0, 130)),
      selectInput(inputId = ns("erMann"), label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      selectInput(inputId = ns("forlopstype1"), label = "Velg operasjonstype",
                  choices = c('--'=99, 'Sfinkterplastikk'=1, 'SNM'=2)),
      uiOutput(outputId = ns('forlopstype2')),
      selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Figur",
                 plotOutput(ns("Figur1"), height="auto"), downloadButton("lastNedBilde", "Last ned figur")),
        tabPanel("Tabell",
                 uiOutput(ns("utvalg"))
                 # tableOutput("Tabell1"), downloadButton("lastNed", "Last ned tabell")
        )
      )
    )
  )
}


fordelingsfig <- function(input, output, session, reshID, RegData){

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
    nraFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                  maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                  valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''}, outfile = '', preprosess=F,
                  erMann = as.numeric(input$erMann), reshID = reshID, enhetsUtvalg = input$enhetsUtvalg, hentData=F,
                  forlopstype1=as.numeric(input$forlopstype1), forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)}
                  else {99})
  }, width = 700, height = 700)


  tabellReager <- reactive({
    TabellData <- nraFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                                maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                                valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''}, outfile = '', preprosess=F,
                                erMann = as.numeric(input$erMann), reshID = reshID, enhetsUtvalg = input$enhetsUtvalg, hentData=F,
                                forlopstype1=as.numeric(input$forlopstype1), forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)}
                                else {99})
  })

  output$utvalg <- renderUI({
    TabellData <- tabellReager()
    tagList(
      h3(TabellData$tittel),
      h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
    )})



  output$Tabell1 <- function() {

    TabellData <- tabellReager()
    if (input$enhetsUtvalg == 1) {

      Tabell1 <- tibble('Kategori'=colnames(TabellData$Antall),
                        'Antall'=TabellData$Antall[1,],
                        'Andel'=TabellData$Andeler[1,])

      Tabell1 <- TabellData$Antall %>%
        mutate(Kategori = rownames(.)) %>%
        select(Kategori, everything()) %>%
        mutate(AndelHoved = 100*AntHoved/NHoved) %>%
        mutate(AndelRest= 100*AntRest/Nrest)
      Tabell1 <- Tabell1[, c(1,2,4,6,3,5,7)]
      names(Tabell1) <- c('Kategori', 'Antall', 'N', 'Andel', 'Antall', 'N', 'Andel')
      Tabell1 %>% knitr::kable("html", digits = c(0,0,0,1,0,0,1)) %>%
        kable_styling("hover", full_width = F) %>%
        add_header_above(c(" ", "Din avdeling" = 3, "Landet forøvrig" = 3))
    } else {
      Tabell1 <- TabellData$Antall %>%
        mutate(Kategori = rownames(.)) %>%
        select(Kategori, everything()) %>%
        mutate(AndelHoved = 100*AntHoved/NHoved)
      names(Tabell1) <- c('Kategori', 'Antall', 'N', 'Andel')
      Tabell1 %>%
        knitr::kable("html", digits = c(0,0,0,1)) %>%
        kable_styling("hover", full_width = F)
    }

  }
  #
  # output$lastNed <- downloadHandler(
  #   filename = function(){
  #     paste0(input$valgtVar, Sys.time(), '.csv')
  #   },
  #
  #   content = function(file){
  #     TabellData <- tabellReager()
  #     if (input$enhetsUtvalg == 1) {
  #       Tabell1 <- TabellData$Antall %>%
  #         mutate(Kategori = rownames(.)) %>%
  #         select(Kategori, everything()) %>%
  #         mutate(AndelHoved = 100*AntHoved/NHoved) %>%
  #         mutate(AndelRest= 100*AntRest/Nrest)
  #       Tabell1 <- Tabell1[, c(1,2,4,6,3,5,7)]
  #     } else {
  #       Tabell1 <- TabellData$Antall %>%
  #         mutate(Kategori = rownames(.)) %>%
  #         select(Kategori, everything()) %>%
  #         mutate(AndelHoved = 100*AntHoved/NHoved)
  #     }
  #     write.csv2(Tabell1, file, row.names = F)
  #   }
  # )
  #
  # output$lastNedBilde <- downloadHandler(
  #   filename = function(){
  #     paste0(input$valgtVar, Sys.time(), '.', input$bildeformat)
  #   },
  #
  #   content = function(file){
  #     norgast::FigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
  #                         maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
  #                         valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''},
  #                         op_gruppe = if (!is.null(input$op_gruppe)) {input$op_gruppe} else {''},
  #                         ncsp = if (!is.null(input$ncsp_verdi)) {input$ncsp_verdi} else {''},
  #                         BMI = if (!is.null(input$BMI)) {input$BMI} else {''},
  #                         tilgang = if (!is.null(input$tilgang)) {input$tilgang} else {''},
  #                         minPRS = as.numeric(input$PRS[1]), maxPRS = as.numeric(input$PRS[2]),
  #                         ASA = if (!is.null(input$ASA)) {input$ASA} else {''},
  #                         whoEcog = if (!is.null(input$whoEcog)) {input$whoEcog} else {''},
  #                         forbehandling = if (!is.null(input$forbehandling)) {input$forbehandling} else {''},
  #                         malign = as.numeric(input$malign),
  #                         reshID = reshID(), enhetsUtvalg = input$enhetsUtvalg, erMann = as.numeric(input$erMann),
  #                         elektiv = as.numeric(input$elektiv), hastegrad = as.numeric(input$hastegrad), outfile = file)
  #   }
  # )



}
