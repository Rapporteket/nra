startside_UI <- function(id){
  ns <- NS(id)
  shiny::bootstrapPage(
    div(class = "container",
        div(class = "panel panel-default",
            div(class = "panel-heading" , style = "background-color : #E0E0E0 ",
                h2('Velkommen til Rapporteket - NRA', align='center')),
            div(class = "panel-body",style = "background-color:#F0F0F0",
                div(class="panel-text",
                    br(),
                    h4('Du er nå inne på Rapporteket for NRA, registerets resultattjeneste.
                Disse sidene inneholder en samling av figurer og tabeller som viser resultater fra registeret.
                På hver av sidene kan man gjøre utvalg i menyene til venstre. Alle resultater er basert
                på ferdigstilte registreringer. Merk at data er hentet direkte fra registerets database.
                Dette medfører at nyere data ikke er kvalitetssikret ennå.'),
                    h4('Du kan se på resultater for eget sykehus, nasjonale data og eget sykehus sett opp mot landet for øvrig.
                       Hvis ikke annet oppgis så gjøres alle datovalg basert på operasjonsdato. Alle figurer og
                       tabeller kan lastes ned.'),
                    br(),

                    h4(tags$b(tags$u('Innhold i de ulike fanene:'))),
                    div(class = "container", style ="margin-right:(@gutter / 10)" ,
                        h4(tags$b('Fordelinger '), 'viser fordelinger (figur/tabell) av ulike variabler.
                Man kan velge hvilken variabel man vil se på, og man kan gjøre ulike filtreringer.'),
                        br(),
                        h4(tags$b('Gjennomsnitt/andeler før og etter operasjon '), 'viser gjennomsnitt eller andel av en variabel. Kan vise enten kun pre-data,
                pre og 1-årsoppfølgingsdata, eller pre-og 1 og 5-årsoppfølgingsdata'),
                        br(),
                        h4(tags$b('Indikatorer '), 'viser registerets kvalitetsindikatorer'),
                        br(),
                        h4(tags$b('Datadump '), 'gir mulighet til å laste ned din egen avdelings registreringer.'),
                        br(),
                        h4(tags$b('Administrative tabeller '), 'er en samling oversikter over antall registreringer.')
                    ),
                    br(),
                    br(),
                    div(class="container",
                        fixedRow(
                          column(width = 4, offset = 1,
                                 h4('Oversikt over registerets kvalitetsindikatorer og resultater finner du på www.kvalitetsregistre.no:', #helpText
                                    a("NRA", href="https://www.kvalitetsregistre.no/registers/541/resultater"),
                                    target="_blank", align='center')),
                          column(width = 4,offset = 2,
                                 h4('Mer informasjon om registeret finnes på NRA sin ',
                                    a("hjemmeside", href="https://unn.no/fag-og-forskning/medisinske-kvalitetsregistre/nra-norsk-register-for-analinkontinens", target="_blank"),
                                    align='center')
                          )
                        )
                    )
                )
            )
        ))
  )
}

startside <- function(input, output){
  # observe(
  #   if (usrRole != "SC") {
  #     shinyjs::hide("SC1")
  #   }
  # )
}
