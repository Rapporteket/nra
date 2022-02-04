#' Definer lister for brukerkontroller
#'
#' Denne funksjonen lager navnede vektorer til bruk i brukerkontroller i Shiny
#'
#' @inheritParams nraFigAndeler
#'
#' @return BrValg En liste med brukerkontrollvektorer
#'
#' @export
#'
BrValg <- function(RegData) {

  varvalg = c('Alder'= 'PasientAlder', 'Etiologi (gml)'= 'Etiologi', 'Etiologi (ny)'= 'Etiologi_v2',
              'Tidligere behandling'= 'TidlBeh_v3',
              'Symtomvarighet'= 'Symtomvarighet', 'Ultralydvurdering av sfinkterskade'= 'Sfinktervurdering',
              'Pasientens tilfredshet'= 'Tilfredshet', 'Komplikasjoner SNM test'= 'Komplikasjon',
              'Komplikasjoner SNM implantasjon'= 'KomplikasjonT2', 'Komplikasjoner SNM totalt'= 'KomplSNMtot',
              'Komplikasjoner ved sfinkterplastikk'= 'KomplSfinkter', 'SNM-dagbok'= 'SNMdagbok')

  sykehus <- RegData$AvdRESH[match(sort(unique(RegData$SenterKortNavn)), RegData$SenterKortNavn)]
  names(sykehus) <- sort(unique(RegData$SenterKortNavn))

  BrValg <- list(varvalg=varvalg, sykehus=sykehus)

}
