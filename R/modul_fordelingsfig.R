#' Modul for fordelingsfigurer i NRA sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#
#' @inheritParams nraFigAndeler
#'
#' @return ui-del av fordelingsfigurmodul
#'
#' @export
#'
fordelingsfig_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      uiOutput(ns("valgtVar_ui")),
      dateRangeInput(inputId=ns("datovalg"),
                     label = "Dato fra og til",
                     max = Sys.Date(),
                     start  = '2014-01-01', end = Sys.Date(),
                     language = "nb", separator = " til "),
      selectInput(inputId = ns("enhetsUtvalg"),
                  label = "Lag figur for",
                  choices = c('Hele landet'=0,
                              'Egen avd. mot landet forøvrig'=1,
                              'Egen avd.'=2)),
      uiOutput(ns("valgtShus_ui")),
      sliderInput(inputId=ns("alder"),
                  label = "Alder", min = 0,
                  max = 130, value = c(0, 130)),
      selectInput(inputId = ns("erMann"),
                  label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      selectInput(inputId = ns("forlopstype1"),
                  label = "Velg operasjonstype",
                  choices = c('--'=99, 'Sfinkterplastikk'=1,
                              'SNM'=2, "1-års oppfølging"=3,
                              "5-års oppfølging"=4),
                  multiple = TRUE),
      uiOutput(outputId = ns('forlopstype2')),
      shiny::selectInput(inputId = ns("onestage"),
                         label = "One stage",
                         choices = c('--'=99, 'Ja'=1, 'Nei'=0),
                         selected = 99),
      selectInput(inputId = ns("bildeformat"),
                  label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = ns("tab"),
                  tabPanel("Figur", value = "fig",
                           plotOutput(ns("Figur1"), height="auto"),
                           downloadButton(ns("lastNedBilde"),
                                          "Last ned figur")),
                  tabPanel("Tabell", value = "tab",
                           uiOutput(ns("utvalg")),
                           tableOutput(ns("Tabell1")),
                           downloadButton(ns("lastNed"),
                                          "Last ned tabell")
                  )
      )
    )
  )
}

#' Modul for fordelingsfigurer i NRA sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#
#' @inheritParams nraFigAndeler
#'
#' @return Serverdelen av fordelingsfigurmodul
#'
#' @export
#'
fordelingsfig_server <- function(id, reshID, RegData, hvd_session,
                                 BrValg, userRole){
  moduleServer(
    id,
    function(input, output, session) {

      output$valgtShus_ui <- renderUI({
        ns <- session$ns
        if (userRole() == 'SC') {
          selectInput(inputId = ns("valgtShus"),
                      label = "Velg sykehus",
                      choices = BrValg$sykehus,
                      multiple = TRUE)
        }
      })

      output$valgtVar_ui <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("valgtVar"),
                    label = "Velg variabel",
                    choices = BrValg$varvalg)
      })

      output$forlopstype2 <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("forlopstype2_verdi"),
                    label = "SNM-type",
                    choices = c('Test usikker'=1,
                                'Test positiv'=2, 'Revisjon'=3,
                                'Eksplantasjon'=4, 'Test negativ'=5),
                    multiple = TRUE)
      })





      PlotParams <- reactive({
        nraBeregnAndeler(
          RegData = RegData, valgtVar = input$valgtVar,
          minald=as.numeric(input$alder[1]),
          maxald=as.numeric(input$alder[2]),
          datoFra = input$datovalg[1], datoTil = input$datovalg[2],
          valgtShus = if (!is.null(input$valgtShus) & userRole() == "SC") {
            input$valgtShus} else {''},
          erMann = as.numeric(input$erMann), reshID = reshID(),
          enhetsUtvalg = input$enhetsUtvalg,
          forlopstype1=if(!is.null(input$forlopstype1)){
            as.numeric(input$forlopstype1)} else {99},
          forlopstype2=if(!is.null(input$forlopstype2_verdi)){
            as.numeric(input$forlopstype2_verdi)} else {99},
          onestage = if(!is.null(input$onestage)){
            as.numeric(input$onestage)} else {99})
      })

      output$utvalg <- renderUI({
        TabellData <- PlotParams()
        tagList(
          h3(HTML(paste0(TabellData$tittel, '<br />'))),
          h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
        )})

      output$Figur1 <- renderPlot({
        nraPlotAndeler(PlotParams())
      }, width = 700, height = 700)



      output$Tabell1 <- function() {
        TabellData <- PlotParams()
        if (input$enhetsUtvalg == 1) {
          Tabell1 <- dplyr::as_tibble(TabellData$tabelldata) %>%
            dplyr::mutate('Kategori'=TabellData$grtxt,
                          'AndelHoved'=AntHoved/NHoved*100,
                          'AndelRest'=AntRest/NRest*100) %>%
            dplyr::select(Kategori, 1:2, AndelHoved, 3:4, AndelRest) %>%
            knitr::kable("html",
                         col.names = c("Kategori","Antall", "N", "Andel",
                                       "Antall", "N", "Andel"),
                         digits = c(0,0,0,1,0,0,1)) %>%
            kableExtra::kable_styling("hover", full_width = F) %>%
            kableExtra::add_header_above(c(" ", "Din avdeling" = 3,
                                           "Landet forøvrig" = 3))
        } else {
          Tabell1 <- dplyr::as_tibble(TabellData$tabelldata) %>%
            dplyr::mutate('Kategori'=TabellData$grtxt,
                          'Andel'=AntHoved/NHoved*100) %>%
            dplyr::select(Kategori, 1:2, Andel) %>%
            dplyr::rename(Antall=AntHoved, N=NHoved) %>%
            knitr::kable("html", digits = c(0,0,0,1)) %>%
            kableExtra::kable_styling("hover", full_width = F)
        }
      }

      output$lastNed <- downloadHandler(
        filename = function(){
          paste0(input$valgtVar, Sys.time(), '.csv')
        },

        content = function(file){
          TabellData <- PlotParams()
          if (input$enhetsUtvalg == 1) {
            Tabell1 <- dplyr::as_tibble(TabellData$tabelldata) %>%
              dplyr::mutate('Kategori'=TabellData$grtxt,
                            'AndelHoved'=AntHoved/NHoved*100,
                            'AndelRest'=AntRest/NRest*100) %>%
              dplyr::select(Kategori, 1:2, AndelHoved, 3:4, AndelRest)
          } else {
            Tabell1 <- dplyr::as_tibble(TabellData$tabelldata) %>%
              dplyr::mutate('Kategori'=TabellData$grtxt,
                            'Andel'=AntHoved/NHoved*100) %>%
              dplyr::select(Kategori, 1:2, Andel)
          }
          write.csv2(Tabell1, file, row.names = F)
        }
      )

      output$lastNedBilde <- downloadHandler(
        filename = function(){
          paste0(input$valgtVar, Sys.time(), '.', input$bildeformat)
        },

        content = function(file){
          nra::nraPlotAndeler(PlotParams(), outfile = file)
        }
      )


      shiny::observe({
        if (rapbase::isRapContext()) {
          if (req(input$tab) == "fig") {
            mld_fordeling <- paste0(
              "NRA: Figur - fordeling, variabel - ",
              input$valgtVar)
          }
          if (req(input$tab) == "tab") {
            mld_fordeling <- paste(
              "NRA: tabell - fordeling. variabel - ",
              input$valgtVar)
          }
          rapbase::repLogger(
            session = hvd_session,
            msg = mld_fordeling
          )
          mldLastNedFig <- paste(
            "NRA: nedlasting figur - fordeling. variabel -",
            input$valgtVar
          )
          mldLastNedTab <- paste(
            "NRA: nedlasting tabell - fordeling. variabel -",
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
  )
}
