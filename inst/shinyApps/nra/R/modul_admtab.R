# Modul for Administrative tabeller-fane i NRA sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#

admtab_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      id = ns("id_adm_panel"),
      selectInput(inputId = ns("adm_tidsenhet"), label = "Velg tidsenhet",
                  choices = c('Måneder'=1, 'År'=2)),
      conditionalPanel(condition = paste0("input['", ns("adm_tidsenhet"), "'] == '1'"),
                       norgast::dateInput2(inputId=ns("datovalg_adm_tid_mnd"), label = "Vis til og med måned: ",
                                           max = Sys.Date(), value = Sys.Date(), minview = 'months', format = "MM yyyy", language="no"),
                       sliderInput(inputId=ns("ant_mnd"), label = "Antall måneder", min = 1, max = 24, value = 12, step = 1)),
      conditionalPanel(condition = paste0("input['", ns("adm_tidsenhet"), "'] == '2'"),
                       norgast::dateInput2(inputId=ns("datovalg_adm_tid_aar"), label = "Vis til og med år: ",
                                           max = Sys.Date(), value = Sys.Date(), minview = 'years', format = "yyyy", language="no"),
                       sliderInput(inputId= ns("ant_aar"), label = "Antall år", min = 1, max = 10, value = 5, step = 1)),
      sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                  max = 130, value = c(0, 130)),
      selectInput(inputId = ns("erMann"), label = "Kjønn",
                  choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
      selectInput(inputId = ns("forlopstype1"), label = "Velg forløpstype",
                  choices = c('--'=99, 'Sfinkterplastikk'=1, 'SNM'=2, 'Oppfølging 1 år'=3, 'Oppfølging 5 år'=4)),
      uiOutput(outputId = ns('forlopstype2')),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(tabsetPanel(id= ns("admtabeller"),
                          tabPanel("Antall skjema", value = "id_ant_skjema",
                                   h4('Her kan du velge om du vil se registreringer per måned eller per år og for hvor
                                             lang periode. For sfinkterplastikk og SNM gjøres datofiltreringen på prosedyredato,
                                             mens for oppfølginger er det dato for siste utfylling som brukes.'),
                                   DTOutput(ns("Tabell_adm")), downloadButton(ns("lastNed_adm"), "Last ned tabell"))
    )
    )

  )
}


admtab <- function(input, output, session, reshID, RegData, userRole, hvd_session, skjemaoversikt){

  observeEvent(input$reset_input, {
    shinyjs::reset("id_adm_panel")
  })

  output$forlopstype2 <- renderUI({
    ns <- session$ns
    if (as.numeric(input$forlopstype1)==2) {
      selectInput(inputId = ns("forlopstype2_verdi"), label = "SNM-type",
                  choices = if (as.numeric(input$forlopstype1)!=1) {
                    c('Test usikker'=1, 'Test positiv'=2, 'Revisjon'=3, 'Eksplantasjon'=4, 'Test negativ'=5)
                  },  multiple = TRUE)
    }
  })



  andre_adm_tab <- function() {

    aux <- nraUtvalg(RegData = RegData, minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                     datoFra = '2012-01-01', datoTil = '2100-01-01',
                     erMann = as.numeric(input$erMann),forlopstype1=as.numeric(input$forlopstype1),
                     forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)} else {99})
    aux <- aux$RegData

    if (input$adm_tidsenhet == 1) {

      tilDato <- as.Date(paste0(input$datovalg_adm_tid_mnd))
      fraDato <- tilDato %m-% months(as.numeric(input$ant_mnd)) %>% floor_date(unit="months")

      aux$mnd <- factor(format(aux$HovedDato, format='%b-%y'), levels = format(seq(fraDato, tilDato, by="month"), "%b-%y"))

      ant_skjema <-  as.data.frame.matrix(addmargins(table(aux[, c('SenterKortNavn', 'mnd')]))) %>% as_tibble(rownames = 'SenterKortNavn')
    }

    if (input$adm_tidsenhet == 2) {

      fraDato <- as.Date(input$datovalg_adm_tid_aar) %m-% years(input$ant_aar) %>% floor_date(unit="years")

      aux$mnd <- factor(format(aux$HovedDato, format='%Y'), levels = format(seq(as.Date(fraDato),as.Date(input$datovalg_adm_tid_aar), by="year"), "%Y"))

      ant_skjema <-  as.data.frame.matrix(addmargins(table(aux[, c('SenterKortNavn', 'mnd')]))) %>% as_tibble(rownames = 'SenterKortNavn')
    }

    sketch <- htmltools::withTags(table(
      tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)

  }

  output$Tabell_adm = renderDT(
    datatable(andre_adm_tab()$ant_skjema[-dim(andre_adm_tab()$ant_skjema)[1], ],
              container = andre_adm_tab()$sketch,
              rownames = F,
              options = list(pageLength = 40)
    )
  )

  output$lastNed_adm <- downloadHandler(
    filename = function(){
      paste0('Regoversikt_tid', Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- andre_adm_tab()$ant_skjema
      write.csv2(TabellData, file, row.names = F)
    }
  )


  shiny::observe({
    if (rapbase::isRapContext()) {
      raplog::repLogger(
        session = hvd_session,
        msg = 'NRA: Kjører administrativ rapport.'
      )

      shinyjs::onclick(
        "lastNed_adm",
        raplog::repLogger(
          session = hvd_session,
          msg = "NRA: nedlasting adm. tabell."
        )
      )
    }
  })





}
