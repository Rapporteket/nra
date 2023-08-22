# Modul for indikatorfigurer i NRA sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
#
indikatorfig_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = ns("valgtVar"),
        label = "Velg variabel",
        choices = c("Andel operert etter standardisert metode" = "Indikator_standardisert",
                    "Andel skjema levert innen 4mnd postoperativt" = "Indikator_aktualitet",
                    "Andel skjema levert innen 4mnd postoperativt - SNM" = "Indikator_aktualitet_snm",
                    "Andel skjema levert innen 4mnd postoperativt - Sfinkterplastikk" = "Indikator_aktualitet_sfinkt",
                    "Prosentvis reduksjon i lekkasjeepisoder >= 50%" = "Indikator1_lekk_red50",
                    "Utført ultralyd" = "Ultralyd",
                    "Utført ultralyd - SNM" = "Ultralyd_snm",
                    "Utført ultralyd - Sfinkterplastikk" = "Ultralyd_sfinkt",
                    "Tidligere konservativ behandling" = "tidl_konservativ",
                    "Tidligere konservativ behandling - SNM" = "tidl_konservativ_snm",
                    "Tidligere konservativ behandling - Sfinkterplastikk" = "tidl_konservativ_sfinkt",
                    "Bekreftet sårinfeksjon innen 30 dager etter implantasjon" = "saarinfeksjon",
                    "St. Mark’s Inkontinensskår <=9 1 år etter operasjon med SNM" = "stmarks_9_1aar_snm",
                    "St. Mark’s Inkontinensskår <=9 5 år etter operasjon med SNM" = "stmarks_9_5aar_snm",
                    "St. Mark’s Inkontinensskår <=12 1 år etter operasjon med SNM" = "stmarks_12_1aar_snm",
                    "St. Mark’s Inkontinensskår <=12 5 år etter operasjon med SNM" = "stmarks_12_5aar_snm",
                    "St. Mark’s Inkontinensskår <=9 1 år etter sfinkterplastikk" = "stmarks_9_1aar_sfinkt",
                    "St. Mark’s Inkontinensskår <=9 5 år etter sfinkterplastikk" = "stmarks_9_5aar_sfinkt",
                    "St. Mark’s Inkontinensskår <=12 1 år etter sfinkterplastikk" = "stmarks_12_1aar_sfinkt",
                    "St. Mark’s Inkontinensskår <=12 5 år etter sfinkterplastikk" = "stmarks_12_5aar_sfinkt",
                    "Wexnerskår <=9 1 år etter operasjon med SNM" = "wexner_9_1aar_snm",
                    "Wexnerskår <=12 1 år etter operasjon med SNM" = "wexner_12_1aar_snm",
                    "Wexnerskår <=9 5 år etter operasjon med SNM" = "wexner_9_5aar_snm",
                    "Wexnerskår <=12 5 år etter operasjon med SNM" = "wexner_12_5aar_snm",
                    "Inkontinensskår <=9 1 år etter operasjon med SNM" = "nra_inkontinensscore_9_1aar_snm",
                    "Inkontinensskår <=12 1 år etter operasjon med SNM" = "nra_inkontinensscore_12_1aar_snm",
                    "Inkontinensskår <=9 1 år etter sfinkterplastikk" = "nra_inkontinensscore_9_1aar_sfinkt",
                    "Inkontinensskår <=12 1 år etter sfinkterplastikk" = "nra_inkontinensscore_12_1aar_sfinkt",
                    "Andel informert om ett års oppfølging" = "andel_inform_oppf",
                    "Andel informert om ett års oppfølging - SNM" = "andel_inform_oppf_snm",
                    "Andel informert om ett års oppfølging - Sfinkterplastikk" = "andel_inform_oppf_sfinkt",
                    "Inkontinensskår <=9 5 år etter operasjon med SNM" = "nra_inkontinensscore_9_5aar_snm",
                    "Inkontinensskår <=12 5 år etter operasjon med SNM" = "nra_inkontinensscore_12_5aar_snm",
                    "Inkontinensskår <=9 5 år etter sfinkterplastikk" = "nra_inkontinensscore_9_5aar_sfinkt",
                    "Inkontinensskår <=12 5 år etter sfinkterplastikk" = "nra_inkontinensscore_12_5aar_sfinkt",
                    "nra_reduksjon_4_stmarks_1aar_sfinkt" = "nra_reduksjon_4_stmarks_1aar_sfinkt",
                    "nra_reduksjon_4_stmarks_5aar_sfinkt" = "nra_reduksjon_4_stmarks_5aar_sfinkt",
                    "nra_reduksjon_4_stmarks_1aar_snm" = "nra_reduksjon_4_stmarks_1aar_snm",
                    "nra_reduksjon_4_stmarks_5aar_snm" = "nra_reduksjon_4_stmarks_5aar_snm",
                    "nra_reduksjon_40pst_stmarks_1aar_sfinkt" = "nra_reduksjon_40pst_stmarks_1aar_sfinkt",
                    "nra_reduksjon_40pst_stmarks_5aar_sfinkt" = "nra_reduksjon_40pst_stmarks_5aar_sfinkt",
                    "nra_reduksjon_40pst_stmarks_1aar_snm" = "nra_reduksjon_40pst_stmarks_1aar_snm",
                    "nra_reduksjon_40pst_stmarks_5aar_snm" = "nra_reduksjon_40pst_stmarks_5aar_snm"),
        selected = 1
      ),
      uiOutput(outputId = ns('tilAar')),
      selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                  choices = c('pdf', 'svg'))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = ns("tab"),
                  tabPanel("Figur", value = "fig",
                           plotOutput(ns("Figur1"), height="auto"), downloadButton(ns("lastNedBilde"), "Last ned figur")),
                  tabPanel("Tabell", value = "tab",
                           tableOutput(ns("Tabell1")), downloadButton(ns("lastNed_tab"), "Last ned tabell")
                  )
      )
    )
  )
}


indikatorfig <- function(input, output, session, RegData, hvd_session){

  output$tilAar <- renderUI({
    ns <- session$ns
    selectInput(inputId = ns("tilAar_verdi"), label = "T.o.m. år",
                choices = rev((min(RegData$Aar, na.rm = T)+2):max(RegData$Aar, na.rm = T)))
  })



  indikatorData <- reactive({
    indikatordata <- nra::nraBeregnIndikator(RegData=RegData, valgtVar=input$valgtVar)
    TabellData <- indikatordata$indikator
    TabellData <- TabellData[which(TabellData$year <= as.numeric(req(input$tilAar_verdi))), ]
    indikatordata$indikator <- TabellData
    indikatordata
  })

  output$Figur1 <- renderPlot({
    indikator <- req(indikatorData()$indikator)
    plotdata <- indikator[, c('AvdRESH', 'year', 'var', "SenterKortNavn")]
    nra::nraFigIndikator_v4(plotdata, tittel = indikatorData()$tittel,
                            terskel = indikatorData()$terskel, maal = indikatorData()$maal,
                            minstekrav = indikatorData()$minstekrav,
                            maalretn = indikatorData()$maalRetn, xmax = indikatorData()$xmax,
                            decreasing =indikatorData()$decreasing, outfile='')
  }, width = 700, height = 700)


  output$lastNedBilde <- downloadHandler(
    filename = function(){
      paste0('fig_', input$valgtVar, Sys.time(), '.', input$bildeformat)
    },

    content = function(file){
      indikator <- req(indikatorData()$indikator)
      plotdata <- indikator[, c('AvdRESH', 'year', 'var', "SenterKortNavn")]
      nra::nraFigIndikator_v4(plotdata, tittel = indikatorData()$tittel,
                              terskel = indikatorData()$terskel, maal = indikatorData()$maal,
                              maalretn = indikatorData()$maalRetn, xmax = indikatorData()$xmax,
                              decreasing =indikatorData()$decreasing, outfile=file)
    }
  )



  indikatortab <- function(indikatordata) {
    Tabell <- req(indikatordata) %>%
      dplyr::filter(year <= as.numeric(input$tilAar_verdi)) %>%
      dplyr::filter(year >= (as.numeric(input$tilAar_verdi)-2)) %>%
      dplyr::group_by(SenterKortNavn, year) %>%
      dplyr::summarise(Antall = as.integer(sum(var)),
                       N = n(),
                       Andel = Antall/N*100)
    Tabell$Antall[Tabell$N < 5] <- NA
    Tabell$Andel[Tabell$N < 5] <- NA
    Tabell
  }

  output$Tabell1 <- renderTable({
    indikatortab(indikatorData()$indikator)
  }, digits = 0, na = "")


  output$lastNed_tab <- downloadHandler(
    filename = function(){
      paste0('indikator_', input$valgtVar, Sys.time(), '.csv')
    },
    content = function(file){
      TabellData <- indikatortab(indikatorData()$indikator)
      write.csv2(TabellData, file, row.names = F, na = "", fileEncoding = "Latin1")
    }
  )


  shiny::observe({
    if (rapbase::isRapContext()) {
      if (req(input$tab) == "fig") {
        mld_fordeling <- paste0(
          "NRA: indikatorfig. variabel - ",
          input$valgtVar)
      }
      if (req(input$tab) == "tab") {
        mld_fordeling <- paste(
          "NRA: indikatortab. variabel - ",
          input$valgtVar)
      }
      rapbase::repLogger(
        session = hvd_session,
        msg = mld_fordeling
      )
      mldLastNedFig <- paste(
        "NRA: nedlasting indikatorfig. variabel -",
        input$valgtVar
      )
      mldLastNedTab <- paste(
        "NRA: nedlasting indikatortab. variabel -",
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
        "lastNed_tab",
        rapbase::repLogger(
          session = hvd_session,
          msg = mldLastNedTab
        )
      )
    }
  })



}


