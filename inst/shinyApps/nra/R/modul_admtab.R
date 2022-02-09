# Modul for Administrative tabeller-fane i NRA sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#

admtab_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      id = ns("id_adm_panel"),
      shiny::selectInput(inputId = ns("adm_tidsenhet"), label = "Velg tidsenhet",
                         choices = c('Måneder'=1, 'År'=2), selected = 2),
      shiny::uiOutput(ns("tab_mnd")),
      shiny::uiOutput(ns("tab_aar")),
      shiny::uiOutput(ns("forlopstype1_ui")),
      shiny::uiOutput(outputId = ns('forlopstype2')),
      shiny::selectInput(inputId = ns("onestage"), label = "One stage",
                         choices = c('--'=99, 'Ja'=1, 'Nei'=0), selected = 99),
      shiny::uiOutput(outputId = ns('regstatus_ui')),
      tags$hr(),
      actionButton(ns("reset_input"), "Nullstill valg")
    ),
    mainPanel(tabsetPanel(id= ns("admtabeller"),
                          tabPanel("Antall forløp", value = "id_ant_forlop",
                                   h4('Her kan få en oversikt over antall forløp i registeret basert på dato for prosedyre.
                                   Man kan velge type forløp og registreringsstatus for de ulike delene av forløpet.'),
                                   DTOutput(ns("Tabell_adm_forlop")), downloadButton(ns("lastNed_adm_forlop"), "Last ned tabell")),
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

  output$forlopstype1_ui <- renderUI({
    ns <- session$ns
    if (input$admtabeller == "id_ant_skjema") {
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

  output$forlopstype2 <- renderUI({
    ns <- session$ns
    if (2 %in% as.numeric(req(input$forlopstype1))) {
      selectInput(inputId = ns("forlopstype2_verdi"), label = "SNM-type",
                  choices = c('Test usikker'=1, 'Test positiv'=2, 'Revisjon'=3,
                              'Eksplantasjon'=4, 'Test negativ'=5, 'Ikke aktuelt'=NA),
                  multiple = TRUE)
    }
  })

  output$tab_mnd <- shiny::renderUI({
    ns <- session$ns
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
    req(input$adm_tidsenhet == '2')
    tagList(
      shinyWidgets::airDatepickerInput(inputId=ns("datovalg_adm_tid_aar"), label = "Vis til og med år: ", minDate = '2014-01-01',
                                       maxDate = Sys.Date(), value = Sys.Date(), view = "years", minView = 'years',
                                       dateFormat = "yyyy", language="da"),
      sliderInput(inputId= ns("ant_aar"), label = "Antall år", min = 1, max = 10, value = 5, step = 1)
    )
  })

  andre_adm_tab <- function() {

    aux <- nraUtvalg(RegData = RegData, datoFra = '2012-01-01', datoTil = '2100-01-01',
                     forlopstype1=if(!is.null(input$forlopstype1)){as.numeric(input$forlopstype1)} else {99},
                     forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)} else {99},
                     onestage = if(!is.null(input$onestage)){as.numeric(input$onestage)} else {99})
    aux <- aux$RegData

    if (input$adm_tidsenhet == 1) {
      req(input$datovalg_adm_tid_mnd)
      tilDato <- as.Date(paste0(input$datovalg_adm_tid_mnd))
      fraDato <- tilDato %m-% months(as.numeric(input$ant_mnd)-1) %>% floor_date(unit="months")

      aux$mnd <- factor(format(aux$HovedDato, format='%b-%y'), levels = format(seq(fraDato, tilDato, by="month"), "%b-%y"))

      ant_skjema <-  as.data.frame.matrix(addmargins(table(aux[, c('SenterKortNavn', 'mnd')]))) %>% as_tibble(rownames = 'SenterKortNavn')
    }

    if (input$adm_tidsenhet == 2) {
      req(input$datovalg_adm_tid_aar)
      fraDato <- as.Date(input$datovalg_adm_tid_aar) %m-% years(input$ant_aar-1) %>% floor_date(unit="years")

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
      rapbase::repLogger(
        session = hvd_session,
        msg = 'NRA: Kjører administrativ rapport.'
      )

      shinyjs::onclick(
        "lastNed_adm",
        rapbase::repLogger(
          session = hvd_session,
          msg = "NRA: nedlasting adm. tabell."
        )
      )
    }
  })


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
      merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "2A SNM-1",
                           c("SkjemaStatus", "ForlopsID")],
            by = "ForlopsID", suffixes = c("", "_2A1"), all = T) %>%
      merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "2A SNM-2",
                           c("SkjemaStatus", "ForlopsID")],
            by = "ForlopsID", suffixes = c("", "_2A2"), all = T) %>%
      merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "1B Oppfølging 1 år",
                           c("SkjemaStatus", "KobletForlopsID")],
            by.x = "ForlopsID", by.y = "KobletForlopsID", suffixes = c("", "_oppf_1aar"), all = T) %>%
      merge(skjemaoversikt[skjemaoversikt$Skjemanavn == "1B Oppfølging 5 år",
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

    skjemaoversikt_forlop$SkjemaStatus_oppf_1aar[is.na(skjemaoversikt_forlop$SkjemaStatus_oppf_1aar)] <- 0
    skjemaoversikt_forlop$SkjemaStatus_oppf_5aar[is.na(skjemaoversikt_forlop$SkjemaStatus_oppf_5aar)] <- 0

    if(!is.null(input$forlopstype1)){
      skjemaoversikt_forlop <- skjemaoversikt_forlop[skjemaoversikt_forlop$ForlopsType1Num %in%
                                                       as.numeric(input$forlopstype1), ]
    }
    # else {
    #   if(!is.null(input$forlopstype2_verdi)){
    #     skjemaoversikt_forlop <- skjemaoversikt_forlop[skjemaoversikt_forlop$ForlopsType2Num %in%
    #                                                      as.numeric(input$forlopstype2_verdi), ]
    #   }
    # }
    if(!is.null(input$forlopstype2_verdi)){
      skjemaoversikt_forlop <- skjemaoversikt_forlop[skjemaoversikt_forlop$ForlopsType2Num %in%
                                                       as.numeric(input$forlopstype2_verdi), ]
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
      )

    adm_tab <- adm_tab[!is.na(adm_tab$tid), ] %>%
      tidyr::spread(value = "antall", key = "tid", fill = 0, drop = FALSE)

    return(adm_tab)
  }

  output$Tabell_adm_forlop = renderDT(
    datatable(admtab_forlop(),
              rownames = F,
              options = list(pageLength = 40)
    )
  )

}
