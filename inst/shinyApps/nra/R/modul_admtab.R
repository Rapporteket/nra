# Modul for Administrative tabeller-fane i NRA sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#

admtab_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      id = ns("id_adm_panel"),
      shiny::uiOutput(outputId = ns('datovalg_ui')),
      shiny::uiOutput(outputId = ns('velg_datovar_ui')),
      shiny::uiOutput(outputId = ns("adm_tidsenhet_ui")),
      shiny::uiOutput(outputId = ns("tab_mnd")),
      shiny::uiOutput(outputId = ns("tab_aar")),
      shiny::uiOutput(outputId = ns("forlopstype1_ui")),
      shiny::uiOutput(outputId = ns('forlopstype2_ui')),
      shiny::uiOutput(outputId = ns('onestage_ui')),
      shiny::uiOutput(outputId = ns('regstatus_ui')),
      shiny::uiOutput(outputId = ns('skjemastatus_ui')),
      shiny::tags$hr(),
      shiny::actionButton(ns("reset_input"), "Nullstill valg")
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        id= ns("admtabeller"),
        shiny::tabPanel("Antall forløp",
                        value = "id_ant_forlop",
                        shiny::h4('Her kan få en oversikt over antall forløp i registeret basert på dato for prosedyre.
                                   Man kan velge type forløp og registreringsstatus for de ulike delene av forløpet.'),
                        DT::DTOutput(ns("Tabell_adm_forlop")),
                        downloadButton(ns("lastNed_adm_forlop"), "Last ned tabell")),
        shiny::tabPanel("Antall registreringer etter forløpstype", value = "id_ant_forlopstype",
                        shiny::h4('Her kan du velge om du vil se registreringer per måned eller per år og for hvor
                                             lang periode. For sfinkterplastikk og SNM gjøres datofiltreringen på prosedyredato,
                                             mens for oppfølginger er det dato for siste utfylling som brukes.'),
                        DT::DTOutput(ns("Tabell_adm")),
                        downloadButton(ns("lastNed_adm_forlopstype"), "Last ned tabell")),
        shiny::tabPanel("Antall skjema", value = "id_ant_skjema",
                        shiny::h2('Innregistreringer i NRA etter skjematype',
                                  align='center'),
                        shiny::br(),
                        shiny::br(),
                        DT::DTOutput(ns("Tabell_adm1")),
                        downloadButton(ns("lastNed_adm_skjematype"), "Last ned tabell")
        )
      )
    )

  )
}


admtab <- function(input, output, session, reshID, RegData, userRole, hvd_session, skjemaoversikt){

  observeEvent(input$reset_input, {
    shinyjs::reset("id_adm_panel")
  })

  output$forlopstype1_ui <- renderUI({
    ns <- session$ns
    if (input$admtabeller == "id_ant_forlopstype") {
      selectInput(inputId = ns("forlopstype1"), label = "Velg forløpstype",
                  choices = c('Sfinkterplastikk'=1, 'SNM'=2,
                              'Oppfølging 1 år'=3, 'Oppfølging 5 år'=4),
                  multiple = TRUE)}
    else {
      if (input$admtabeller == "id_ant_forlop") {
        selectInput(inputId = ns("forlopstype1"), label = "Velg forløpstype",
                    choices = c('Sfinkterplastikk'=1, 'SNM'=2),
                    multiple = TRUE)}
    }
  })

  output$adm_tidsenhet_ui <- renderUI({
    ns <- session$ns
    req(input$admtabeller %in% c("id_ant_forlop", "id_ant_forlopstype"))
    shiny::selectInput(inputId = ns("adm_tidsenhet"), label = "Velg tidsenhet",
                       choices = c('Måneder'=1, 'År'=2), selected = 2)
  })

  output$velg_datovar_ui <- renderUI({
    ns <- session$ns
    req(input$admtabeller %in% c("id_ant_skjema"))
    shiny::selectInput(inputId = ns("velg_datovar"), label = "Datovariabel",
                       choices = c('HovedDato', 'OpprettetDato', 'SistLagretDato'))
  })

  output$regstatus_ui <- renderUI({
    ns <- session$ns
    req(input$admtabeller == "id_ant_forlop")
    shiny::checkboxGroupInput(
      inputId=ns("regstatus"),
      label="Forløp med fullført:",
      choices = c("Basisregistrering"=1, "1-årsoppfølging"=2, "5-årsoppfølging"=3),
      selected = 1,
      inline = FALSE
    )
  })

  output$skjemastatus_ui <- renderUI({
    ns <- session$ns
    req(input$admtabeller == "id_ant_skjema")
    shiny::checkboxGroupInput(
      inputId=ns("skjemastatus"),
      label="Skjemastatus:",
      choices = c("Ferdigstilt"=1, "I kladd"=0, "Opprettet"=-1),
      selected = 1,
      inline = FALSE
    )
  })

  output$datovalg_ui <- renderUI({
    ns <- session$ns
    req(input$admtabeller == "id_ant_skjema")
    shiny::dateRangeInput(inputId=ns("datovalg"), label = "Dato fra og til",
                          min = '2015-01-01', language = "nb",
                          max = Sys.Date(), start  = Sys.Date() %m-% months(12),
                          end = Sys.Date(), separator = " til ")
  })


  output$forlopstype2_ui <- renderUI({
    ns <- session$ns
    if (2 %in% as.numeric(req(input$forlopstype1))) {
      selectInput(inputId = ns("forlopstype2"), label = "SNM-type",
                  choices = c('Test usikker'=1, 'Test positiv'=2, 'Revisjon'=3,
                              'Eksplantasjon'=4, 'Test negativ'=5, 'Ikke aktuelt'=NA),
                  multiple = TRUE)
    }
  })

  output$onestage_ui <- shiny::renderUI({
    ns <- session$ns
    req(input$admtabeller %in% c("id_ant_forlop", "id_ant_forlopstype"))
    if (2 %in% as.numeric(req(input$forlopstype1))) {
      shiny::selectInput(inputId = ns("onestage"), label = "One stage",
                         choices = c('--'=99, 'Ja'=1, 'Nei'=0), selected = 99)
    }
  })

  output$tab_mnd <- shiny::renderUI({
    ns <- session$ns
    req(input$admtabeller %in% c("id_ant_forlop", "id_ant_forlopstype"))
    req(input$adm_tidsenhet == '1')
    tagList(
      shinyWidgets::airDatepickerInput(inputId=ns("datovalg_adm_tid_mnd"), label = "Vis til og med måned: ", minDate = '2014-01-01',
                                       maxDate = Sys.Date(), value = Sys.Date(), view = "months", minView = 'months',
                                       dateFormat = "MM yyyy", language="da"),
      sliderInput(inputId=ns("ant_mnd"), label = "Antall måneder", min = 1, max = 24, value = 6, step = 1)
    )
  })

  output$tab_aar <- shiny::renderUI({
    ns <- session$ns
    req(input$admtabeller %in% c("id_ant_forlop", "id_ant_forlopstype"))
    req(input$adm_tidsenhet == '2')
    tagList(
      shinyWidgets::airDatepickerInput(inputId=ns("datovalg_adm_tid_aar"), label = "Vis til og med år: ", minDate = '2014-01-01',
                                       maxDate = Sys.Date(), value = Sys.Date(), view = "years", minView = 'years',
                                       dateFormat = "yyyy", language="da"),
      sliderInput(inputId= ns("ant_aar"), label = "Antall år", min = 1, max = 10, value = 5, step = 1)
    )
  })




  antskjema <- function() {
    ant_skjema <- skjemaoversikt %>%
      dplyr::mutate(SenterKortNavn = RegData$SenterKortNavn[match(AvdRESH, RegData$AvdRESH)]) %>%
      dplyr::mutate(Skjemanavn = factor(Skjemanavn, levels = Skjemanavn[match(sort(unique(SkjemaRekkeflg)), SkjemaRekkeflg)])) %>%
      dplyr::rename(Dato = req(input$velg_datovar)) %>%
      dplyr::filter(Dato >= input$datovalg[1] & Dato <= input$datovalg[2]) %>%
      dplyr::filter(SkjemaStatus %in% as.numeric(input$skjemastatus)) %>%
      dplyr::select("SenterKortNavn", "Skjemanavn") %>%
      table() %>%
      addmargins(1) %>%
      as.data.frame.matrix() %>%
      tidyr::as_tibble(rownames = "Sykehus")

    sketch <- htmltools::withTags(table(
      DT::tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      DT::tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)
  }

  output$Tabell_adm1 = DT::renderDT(
    DT::datatable(antskjema()$ant_skjema[-dim(antskjema()$ant_skjema)[1], ],
                  container = antskjema()$sketch,
                  rownames = F,
                  options = list(pageLength = 40)
    )
  )


  output$lastNed_adm_skjematype <- downloadHandler(
    filename = function(){
      paste0('Regoversikt_skjematype_', Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- antskjema()$ant_skjema
      write.csv2(TabellData, file, row.names = F, fileEncoding = "Latin1")
    }
  )

  andre_adm_tab <- function() {

    aux <- nraUtvalg(RegData = RegData, datoFra = '2012-01-01', datoTil = '2100-01-01',
                     forlopstype1=if(!is.null(input$forlopstype1)){as.numeric(input$forlopstype1)} else {99},
                     forlopstype2=if(!is.null(input$forlopstype2)){as.numeric(input$forlopstype2)} else {99},
                     onestage = if(!is.null(input$onestage)){as.numeric(input$onestage)} else {99})
    aux <- aux$RegData

    if (input$adm_tidsenhet == 1) {
      req(input$datovalg_adm_tid_mnd)
      tilDato <- as.Date(paste0(input$datovalg_adm_tid_mnd))
      fraDato <- tilDato %m-% months(as.numeric(input$ant_mnd)-1) %>% floor_date(unit="months")

      aux$mnd <- factor(format(aux$HovedDato, format='%b-%y'), levels = format(seq(fraDato, tilDato, by="month"), "%b-%y"))

      ant_skjema <-  as.data.frame.matrix(addmargins(table(aux[, c('SenterKortNavn', 'mnd')]))) %>% as_tibble(rownames = 'Sykehus')
    }

    if (input$adm_tidsenhet == 2) {
      req(input$datovalg_adm_tid_aar)
      fraDato <- as.Date(input$datovalg_adm_tid_aar) %m-% years(input$ant_aar-1) %>% floor_date(unit="years")

      aux$mnd <- factor(format(aux$HovedDato, format='%Y'), levels = format(seq(as.Date(fraDato),as.Date(input$datovalg_adm_tid_aar), by="year"), "%Y"))

      ant_skjema <-  as.data.frame.matrix(addmargins(table(aux[, c('SenterKortNavn', 'mnd')]))) %>% as_tibble(rownames = 'Sykehus')
    }

    sketch <- htmltools::withTags(table(
      tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)

  }

  output$Tabell_adm = DT::renderDT(
    DT::datatable(andre_adm_tab()$ant_skjema[-dim(andre_adm_tab()$ant_skjema)[1], ],
              container = andre_adm_tab()$sketch,
              rownames = F,
              options = list(pageLength = 40)
    )
  )

  output$lastNed_adm_forlopstype <- downloadHandler(
    filename = function(){
      paste0('Regoversikt_forlopstype_', Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- andre_adm_tab()$ant_skjema
      write.csv2(TabellData, file, row.names = F, fileEncoding = "Latin1")
    }
  )


  admtab_forlop <- function() {
    map_shnavn_kortnavn <- data.frame(shusnavn=unique(RegData$Sykehusnavn),
                                      kortnavn=RegData$SenterKortNavn[match(unique(RegData$Sykehusnavn),
                                                                            RegData$Sykehusnavn)])
    skjemaoversikt <- merge(skjemaoversikt, RegData[, c("ForlopsID", "KobletForlopsID")],
                            by = "ForlopsID", all.x = T)
    skjemaoversikt$Sykehusnavn <- map_shnavn_kortnavn$kortnavn[match(skjemaoversikt$Sykehusnavn,
                                                                     map_shnavn_kortnavn$shusnavn)]


    skjemaoversikt_forlop <-
      merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "1A Anamnese",
                           c("ForlopsID", "HovedDato", "Sykehusnavn", "AvdRESH", "SkjemaStatus")],
            skjemaoversikt[skjemaoversikt$Skjemanavn == "1B Symptom",
                           c("SkjemaStatus", "ForlopsID")],
            by = "ForlopsID", suffixes = c("_1A", "_1B"), all = T) %>%
      merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "2B Sfinkter",
                           c("SkjemaStatus", "ForlopsID")],
            by = "ForlopsID", suffixes = c("", "_2B"), all = T) %>%
      merge(skjemaoversikt[skjemaoversikt$Skjemanavn %in% c("2A SNM-1", "2A SNM-2", "2A SNM-3", "2A SNM-4", "2A SNM-5"),
                           c("SkjemaStatus", "ForlopsID")],
            by = "ForlopsID", suffixes = c("", "_2A"), all = T) %>%
      merge(skjemaoversikt[skjemaoversikt$Skjemanavn %in% c("2AT2 SNM-1", "2AT2 SNM-2", "2AT2 SNM-5"),
                           c("SkjemaStatus", "ForlopsID")],
            by = "ForlopsID", suffixes = c("", "_2AT2"), all = T) %>%
      merge(skjemaoversikt[skjemaoversikt$Skjemanavn %in% c("1B Oppfølging 1 år", "3A Oppfølging 1 år"),
                           c("SkjemaStatus", "KobletForlopsID")],
            by.x = "ForlopsID", by.y = "KobletForlopsID", suffixes = c("", "_oppf_1aar"), all = T) %>%
      merge(skjemaoversikt[skjemaoversikt$Skjemanavn %in% c("1B Oppfølging 5 år", "3A Oppfølging 5 år"),
                           c("SkjemaStatus", "KobletForlopsID")],
            by.x = "ForlopsID", by.y = "KobletForlopsID", suffixes = c("", "_oppf_5aar"), all = T)

    names(skjemaoversikt_forlop)[names(skjemaoversikt_forlop) == "SkjemaStatus"] <- "SkjemaStatus_2B"

    skjemaoversikt_forlop <- merge(skjemaoversikt_forlop,
                                   RegData[, c("ForlopsID", "ForlopsType1Num", "ForlopsType2Num", "PasientID", "Onestage")],
                                   by = "ForlopsID", all.x = T)


    skjemaoversikt_forlop$statusbasis <- 0
    skjemaoversikt_forlop$statusbasis[skjemaoversikt_forlop$SkjemaStatus_1A == 1 &
                                        skjemaoversikt_forlop$SkjemaStatus_1B == 1 &
                                        (skjemaoversikt_forlop$SkjemaStatus_2B == 1 |
                                           skjemaoversikt_forlop$SkjemaStatus_2A1 == 1 |
                                           skjemaoversikt_forlop$ForlopsType2 %in%
                                           c("Eksplantasjon", "Revisjon"))] <- 1

    if (!is.null(input$adm_tidsenhet)) {
      if (input$adm_tidsenhet == 1) {
        req(input$datovalg_adm_tid_mnd)
        tilDato <- as.Date(paste0(input$datovalg_adm_tid_mnd))
        fraDato <- tilDato %m-% months(as.numeric(input$ant_mnd)-1) %>% floor_date(unit="months")

        skjemaoversikt_forlop$tid <- factor(format(skjemaoversikt_forlop$HovedDato, format='%b-%y'),
                                            levels = format(seq(fraDato, tilDato, by="month"), "%b-%y"))
      }
      if (input$adm_tidsenhet == 2) {
        req(input$datovalg_adm_tid_aar)
        fraDato <- as.Date(input$datovalg_adm_tid_aar) %m-% years(input$ant_aar-1) %>%
          floor_date(unit="years")

        skjemaoversikt_forlop$tid <- factor(format(skjemaoversikt_forlop$HovedDato, format='%Y'),
                                            levels = format(seq(as.Date(fraDato),as.Date(input$datovalg_adm_tid_aar),
                                                                by="year"), "%Y"))
      }
    }

    skjemaoversikt_forlop$SkjemaStatus_oppf_1aar[is.na(skjemaoversikt_forlop$SkjemaStatus_oppf_1aar)] <- 0
    skjemaoversikt_forlop$SkjemaStatus_oppf_5aar[is.na(skjemaoversikt_forlop$SkjemaStatus_oppf_5aar)] <- 0

    if(!is.null(input$forlopstype1)){
      skjemaoversikt_forlop <- skjemaoversikt_forlop[skjemaoversikt_forlop$ForlopsType1Num %in%
                                                       as.numeric(input$forlopstype1), ]
    }

    if(!is.null(input$forlopstype2)){
      skjemaoversikt_forlop <- skjemaoversikt_forlop[skjemaoversikt_forlop$ForlopsType2Num %in%
                                                       as.numeric(input$forlopstype2), ]
    }
    if (!is.null(input$onestage)) {
      if (as.numeric(input$onestage) %in% c(0, 1)) {
        skjemaoversikt_forlop <- skjemaoversikt_forlop[skjemaoversikt_forlop$Onestage %in%
                                                         as.numeric(input$onestage), ]
      }
    }

    adm_tab <- skjemaoversikt_forlop %>% dplyr::group_by(Sykehusnavn, tid) %>%
      dplyr::summarise(antall = sum(statusbasis == as.numeric("1" %in% input$regstatus) &
                                      SkjemaStatus_oppf_1aar == as.numeric("2" %in% input$regstatus) &
                                      SkjemaStatus_oppf_5aar == as.numeric("3" %in% input$regstatus))
      ) %>% dplyr::rename(Sykehus = Sykehusnavn)

    adm_tab <- adm_tab[!is.na(adm_tab$tid), ] %>%
      tidyr::spread(value = "antall", key = "tid", fill = 0, drop = FALSE)

    adm_tab <- dplyr::bind_rows(adm_tab, as_tibble(as.list(c("Sykehus"="Sum", colSums(adm_tab[, -1])))) %>% mutate_at(vars(-Sykehus), as.numeric))

    sketch <- htmltools::withTags(table(
      tableHeader(adm_tab[-dim(adm_tab)[1], ]),
      tableFooter(c('Sum' , as.numeric(adm_tab[dim(adm_tab)[1], 2:dim(adm_tab)[2]])))))
    list(ant_skjema=adm_tab, sketch=sketch)

  }

  output$Tabell_adm_forlop = DT::renderDT(
    DT::datatable(admtab_forlop()$ant_skjema[-dim(admtab_forlop()$ant_skjema)[1], ],
              container = admtab_forlop()$sketch,
              rownames = F,
              options = list(pageLength = 40)
    )
  )

  output$lastNed_adm_forlop <- downloadHandler(
    filename = function(){
      paste0('Regoversikt_forlop_', Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- admtab_forlop()$ant_skjema
      write.csv2(TabellData, file, row.names = F, fileEncoding = "Latin1")
    }
  )



  shiny::observe({
    if (rapbase::isRapContext()) {
      rapbase::repLogger(
        session = hvd_session,
        msg = 'NRA: Kjører administrativ rapport.'
      )

      shinyjs::onclick(
        "lastNed_adm_forlopstype",
        rapbase::repLogger(
          session = hvd_session,
          msg = "NRA: nedlasting adm. tabell. forlopstype"
        )
      )

      shinyjs::onclick(
        "lastNed_adm_forlop",
        rapbase::repLogger(
          session = hvd_session,
          msg = "NRA: nedlasting adm. tabell. forlopsbasert"
        )
      )

      shinyjs::onclick(
        "lastNed_adm_skjematype",
        rapbase::repLogger(
          session = hvd_session,
          msg = "NRA: nedlasting adm. tabell. etter skjematype"
        )
      )
    }
  })



}
