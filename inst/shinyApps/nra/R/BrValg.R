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

  varvalg = c('Alder'= 'PasientAlder',
              'Etiologi (gml)'= 'Etiologi',
              'Etiologi (ny)'= 'Etiologi_v2',
              'Tidligere behandling'= 'TidlBeh_v3',
              'Symtomvarighet'= 'Symtomvarighet',
              'Ultralydvurdering av sfinkterskade'= 'Sfinktervurdering',
              'Pasientens tilfredshet'= 'Tilfredshet',
              'Komplikasjoner SNM test (gml)'= 'Komplikasjon',
              'Komplikasjoner SNM implantasjon (gml)'= 'KomplikasjonT2',
              'Komplikasjoner SNM totalt (gml)'= 'KomplSNMtot',
              'Komplikasjoner SNM test (ny)'= 'Komplikasjon_ny',
              'Komplikasjoner SNM implantasjon (ny)'= 'KomplikasjonT2_ny',
              'Komplikasjoner SNM totalt (ny)'= 'KomplSNMtot_ny',
              'Komplikasjoner ved sfinkterplastikk'= 'KomplSfinkter',
              'SNM-dagbok (gml)'= 'SNMdagbok',
              'SNM-dagbok (ny)'= 'SNMdagbok_v2',
              "Begrensning av seksualliv" = "BegrensSeksLiv",
              "PGIC Endring" = "PGICEndring",
              "PGIC Endring lekkasje" = "PGICEndringLekkasje",
              "EQ5D: Angst og depresjon" = "EQ5DAngst",
              "EQ5D: Personlig stell" = "EQ5DPersonligStell",
              "EQ5D: Smerte og ubehag" = "EQ5DSmerte",
              "EQ5D: Gange" = "EQ5DGange",
              "EQ5D: Vanlige gjøremål" = "EQ5DVanligeGjoeremaal")

  sykehus <- RegData$AvdRESH[match(sort(unique(RegData$SenterKortNavn)), RegData$SenterKortNavn)]
  names(sykehus) <- sort(unique(RegData$SenterKortNavn))

  BrValg <- list(varvalg=varvalg, sykehus=sykehus)

}
