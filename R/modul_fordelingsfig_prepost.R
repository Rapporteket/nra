#' Modul for før/etter-figur i NRA sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @inheritParams nraFigAndeler
#'
#' @return UI-del av før/etter-figur
#'
#' @export
#'
fordelingsfig_prepost_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      selectInput(inputId = ns("valgtVar"), label = "Velg variabel",
                  choices = "BegrensSeksLiv"),
      dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til",
                     max = Sys.Date(), start  = '2014-01-01',
                     end = Sys.Date(), language = "nb", separator = " til "),
      selectInput(inputId = ns("enhetsUtvalg"), label = "Lag figur for",
                  choices = c('Hele landet'=0,
                              'Egen avd. mot landet forøvrig'=1,
                              'Egen avd.'=2)),
      uiOutput(ns("valgtShus_ui")),
      sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                  max = 130, value = c(0, 130)),
      selectInput(inputId = ns("erMann"), label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      selectInput(inputId = ns("forlopstype1"),
                  label = "Velg operasjonstype",
                  choices = c('--'=99, 'Sfinkterplastikk'=1,
                              'SNM'=2, "1-års oppfølging"=3,
                              "5-års oppfølging"=4), multiple = TRUE),
      uiOutput(outputId = ns('forlopstype2')),
      shiny::selectInput(inputId = ns("onestage"), label = "One stage",
                         choices = c('--'=99, 'Ja'=1, 'Nei'=0), selected = 99),
      selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        id = ns("tab"),
        tabPanel("Figur", value = "fig",
                 plotOutput(ns("Figur1"), height="auto"),
                 downloadButton(ns("lastNedBilde"), "Last ned figur")) #,
        # tabPanel("Tabell", value = "tab",
        #          uiOutput(ns("utvalg")),
        #          tableOutput(ns("Tabell1")),
        #          downloadButton(ns("lastNed"), "Last ned tabell")
        # )
      )
    )
  )
}

#' Modul for før/etter-figur i NRA sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @inheritParams nraFigAndeler
#'
#' @return Serverdel av før/etter-figur
#'
#' @export
#'
fordelingsfig_prepost_server <- function(id, RegData, user, BrValg){
  moduleServer(
    id,
    function(input, output, session) {

      output$forlopstype2 <- renderUI({
        ns <- session$ns
        selectInput(inputId = ns("forlopstype2_verdi"), label = "SNM-type",
                    choices = c('Test usikker'=1, 'Test positiv'=2, 'Revisjon'=3,
                                'Eksplantasjon'=4, 'Test negativ'=5),
                    multiple = TRUE)
      })

      output$valgtShus_ui <- renderUI({
        ns <- session$ns
        if (user$role() == 'SC') {
          selectInput(inputId = ns("valgtShus"), label = "Velg sykehus",
                      choices = BrValg$sykehus, multiple = TRUE)
        }
      })


      output$Figur1 <- renderPlot({
        nraBeregnFordelingPrePost(
          RegData = RegData,
          valgtVar = input$valgtVar,
          minald=as.numeric(input$alder[1]),
          maxald=as.numeric(input$alder[2]),
          datoFra = input$datovalg[1],
          datoTil = input$datovalg[2],
          valgtShus = if (!is.null(input$valgtShus) & user$role() == "SC") {
            input$valgtShus} else {''},
          outfile = '',
          erMann = as.numeric(input$erMann), reshID = user$org(),
          enhetsUtvalg = input$enhetsUtvalg,
          forlopstype1=if(!is.null(input$forlopstype1)){
            as.numeric(input$forlopstype1)} else {99},
          forlopstype2=if(!is.null(input$forlopstype2_verdi)){
            as.numeric(input$forlopstype2_verdi)} else {99},
          onestage = if(!is.null(input$onestage)){
            as.numeric(input$onestage)} else {99},
          sammenlign=1)
      }, width = 700, height = 700)


      output$lastNedBilde <- downloadHandler(
        filename = function(){
          paste0(input$valgtVar, Sys.time(), '.', input$bildeformat)
        },

        content = function(file){
          nra::nraBeregnFordelingPrePost(
            RegData = RegData, valgtVar = input$valgtVar,
            minald=as.numeric(input$alder[1]),
            maxald=as.numeric(input$alder[2]),
            datoFra = input$datovalg[1],
            datoTil = input$datovalg[2],
            valgtShus = if (!is.null(input$valgtShus) & user$role() == "SC") {
              input$valgtShus} else {''},
            erMann = as.numeric(input$erMann),
            reshID = user$org(),
            enhetsUtvalg = input$enhetsUtvalg,
            forlopstype1=if(!is.null(input$forlopstype1)){
              as.numeric(input$forlopstype1)} else {99},
            forlopstype2=if(!is.null(input$forlopstype2_verdi)){
              as.numeric(input$forlopstype2_verdi)} else {99},
            outfile = file, onestage = if(!is.null(input$onestage)){
              as.numeric(input$onestage)} else {99},
            sammenlign=1)
        }
      )

    }
  )
}
