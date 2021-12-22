# Modul for indikatorfigurer i NRA sin shiny-app på Rapporteket
#
# Kun til bruk i Shiny
#
#
indikatorfig_UI <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(
      selectInput(inputId = ns("valgtVar"), label = "Velg variabel",
                  choices = c("Prosentvis reduksjon i lekkasjeepisoder >= 50%" = "Indikator1_lekk_red50",
                              "Utført ultralyd" = "Ultralyd",
                              "Tidligere konservativ behandling" = "tidl_konservativ",
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
                              "Wexnerskår <=12 1 år etter operasjon med SNM" = "wexner_12_1aar_snm")),
      uiOutput(outputId = ns('tilAar'))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(id = ns("tab"),
                  tabPanel("Figur", value = "fig",
                           plotOutput(ns("Figur1"), height="auto"), downloadButton(ns("lastNedBilde"), "Last ned figur")),
                  tabPanel("Tabell", value = "tab",
                           # uiOutput(ns("utvalg")),
                           tableOutput(ns("Tabell1")), downloadButton(ns("lastNed"), "Last ned tabell")
                  ),
                  tabPanel("DT_Tabell", value = "tab2",
                           # uiOutput(ns("utvalg")),
                           DTOutput(ns("Tabell2"))
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
    # names(plotdata) <- c('ReshId', 'Aar', 'Teller', "SenterKortNavn")
    nra::nraFigIndikator_v2(plotdata, tittel = indikatorData()$tittel,
                            terskel = indikatorData()$terskel, maal = indikatorData()$maal,
                    maalretn = indikatorData()$maalRetn, xmax = indikatorData()$xmax,
                    decreasing =indikatorData()$decreasing, outfile='')
  }, width = 700, height = 700)

  output$Tabell1 <- renderTable(

    Tabell <- req(indikatorData()$indikator) %>% dplyr::group_by(SenterKortNavn, year) %>%
      dplyr::summarise(Antall = as.integer(sum(var)),
                       N = n(),
                       Andel = Antall/N*100)
  )

  output$Tabell2 <- renderDT(

    Tabell <- req(indikatorData()$indikator) %>% dplyr::group_by(SenterKortNavn, year) %>%
      dplyr::summarise(Antall = as.integer(sum(var)),
                       N = n(),
                       Andel = round(Antall/N*100, 1))
  )



  # plotdata <- indikator[, c('AvdRESH', 'year', 'var')]
  # names(plotdata) <- c('ReshId', 'Aar', 'Teller')
  # plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]

  # output$Figur1 <- renderPlot({
  #   nraFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
  #                 maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
  #                 valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''}, outfile = '', preprosess=F,
  #                 erMann = as.numeric(input$erMann), reshID = reshID, enhetsUtvalg = input$enhetsUtvalg, hentData=F,
  #                 forlopstype1=as.numeric(input$forlopstype1), forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)}
  #                 else {99})
  # }, width = 700, height = 700)


  # tabellReager <- reactive({
  #   TabellData <- nraFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
  #                               maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
  #                               valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''}, outfile = '', preprosess=F,
  #                               erMann = as.numeric(input$erMann), reshID = reshID, enhetsUtvalg = input$enhetsUtvalg, hentData=F,
  #                               forlopstype1=as.numeric(input$forlopstype1), forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)}
  #                               else {99})
  # })
  #
  # output$utvalg <- renderUI({
  #   TabellData <- tabellReager()
  #   tagList(
  #     h3(HTML(paste0(TabellData$tittel, '<br />'))),
  #     h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
  #   )})



  # output$Tabell1 <- function() {
  #
  #   TabellData <- tabellReager()
  #   if (input$enhetsUtvalg == 1) {
  #     Tabell1 <- as_tibble(TabellData$TabellData) %>%
  #       mutate('Kategori'=TabellData$grtxt, 'AndelHoved'=AntHoved/NHoved*100, 'AndelRest'=AntRest/NRest*100) %>%
  #       select(Kategori, 1:2, AndelHoved, 3:4, AndelRest) %>%
  #       rename(Antall=AntHoved, N=NHoved, Andel=AndelHoved, Antall=AntRest, N=NRest, Andel=AndelRest) %>%
  #       knitr::kable("html", digits = c(0,0,0,1,0,0,1)) %>%
  #       kable_styling("hover", full_width = F) %>%
  #       add_header_above(c(" ", "Din avdeling" = 3, "Landet forøvrig" = 3))
  #   } else {
  #     Tabell1 <- as_tibble(TabellData$TabellData) %>%
  #       mutate('Kategori'=TabellData$grtxt, 'Andel'=AntHoved/NHoved*100) %>%
  #       select(Kategori, 1:2, Andel) %>%
  #       rename(Antall=AntHoved, N=NHoved) %>%
  #       knitr::kable("html", digits = c(0,0,0,1)) %>%
  #       kable_styling("hover", full_width = F)
  #   }
  # }

  # output$lastNed <- downloadHandler(
  #   filename = function(){
  #     paste0(input$valgtVar, Sys.time(), '.csv')
  #   },
  #
  #   content = function(file){
  #     TabellData <- tabellReager()
  #     if (input$enhetsUtvalg == 1) {
  #       Tabell1 <- as_tibble(TabellData$TabellData) %>%
  #         mutate('Kategori'=TabellData$grtxt, 'AndelHoved'=AntHoved/NHoved*100, 'AndelRest'=AntRest/NRest*100) %>%
  #         select(Kategori, 1:2, AndelHoved, 3:4, AndelRest)
  #     } else {
  #       Tabell1 <- as_tibble(TabellData$TabellData) %>%
  #         mutate('Kategori'=TabellData$grtxt, 'Andel'=AntHoved/NHoved*100) %>%
  #         select(Kategori, 1:2, Andel)
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
  #     nra::nraFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
  #                        maxald=as.numeric(input$alder[2]), datoFra = input$datovalg[1], datoTil = input$datovalg[2],
  #                        valgtShus = if (!is.null(input$valgtShus)) {input$valgtShus} else {''}, preprosess=F,
  #                        erMann = as.numeric(input$erMann), reshID = reshID, enhetsUtvalg = input$enhetsUtvalg, hentData=F,
  #                        forlopstype1=as.numeric(input$forlopstype1), forlopstype2=if(!is.null(input$forlopstype2_verdi)){as.numeric(input$forlopstype2_verdi)}
  #                        else {99}, outfile = file)
  #   }
  # )


  # shiny::observe({
  #   if (rapbase::isRapContext()) {
  #     if (req(input$tab) == "fig") {
  #       mld_fordeling <- paste0(
  #         "NRA: Figur - fordeling, variabel - ",
  #         input$valgtVar)
  #     }
  #     if (req(input$tab) == "tab") {
  #       mld_fordeling <- paste(
  #         "NRA: tabell - fordeling. variabel - ",
  #         input$valgtVar)
  #     }
  #     rapbase::repLogger(
  #       session = hvd_session,
  #       msg = mld_fordeling
  #     )
  #     mldLastNedFig <- paste(
  #       "NRA: nedlasting figur - fordeling. variabel -",
  #       input$valgtVar
  #     )
  #     mldLastNedTab <- paste(
  #       "NRA: nedlasting tabell - fordeling. variabel -",
  #       input$valgtVar
  #     )
  #     shinyjs::onclick(
  #       "lastNedBilde",
  #       rapbase::repLogger(
  #         session = hvd_session,
  #         msg = mldLastNedFig
  #       )
  #     )
  #     shinyjs::onclick(
  #       "lastNed",
  #       rapbase::repLogger(
  #         session = hvd_session,
  #         msg = mldLastNedTab
  #       )
  #     )
  #   }
  # })



}


