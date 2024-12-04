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
              'Pasientens tilfredshet etter 1 år'= 'Tilfredshet_1aar',
              'Pasientens tilfredshet etter 5 år'= 'Tilfredshet_5aar',
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
              "PGIC Endring etter 1 år" = "PGICEndring_1aar",
              "PGIC Endring lekkasje etter 1 år" = "PGICEndringLekkasje_1aar",
              "PGIC Endring etter 5 år" = "PGICEndring_5aar",
              "PGIC Endring lekkasje etter 5 år" = "PGICEndringLekkasje_5aar",
              "EQ5D: Angst og depresjon" = "EQ5DAngst",
              "EQ5D: Personlig stell" = "EQ5DPersonligStell",
              "EQ5D: Smerte og ubehag" = "EQ5DSmerte",
              "EQ5D: Gange" = "EQ5DGange",
              "EQ5D: Vanlige gjøremål" = "EQ5DVanligeGjoeremaal")

  sykehus <- RegData$AvdRESH[match(sort(unique(RegData$SenterKortNavn)), RegData$SenterKortNavn)]
  names(sykehus) <- sort(unique(RegData$SenterKortNavn))

  BrValg <- list(varvalg=varvalg, sykehus=sykehus)

}
