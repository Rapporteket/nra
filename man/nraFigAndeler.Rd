% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nraFigAndeler.R
\name{nraFigAndeler}
\alias{nraFigAndeler}
\title{Lag søylediagram eller stabelplott som viser andeler av ulike variabler}
\usage{
nraFigAndeler(
  RegData,
  valgtVar,
  datoFra = "2012-04-01",
  datoTil = "2050-12-31",
  tittelstr = 1.2,
  valgtShus = "",
  outfile = "",
  preprosess = TRUE,
  minald = 0,
  maxald = 130,
  erMann = 99,
  reshID,
  enhetsUtvalg = 0,
  hentData = F,
  forlopstype1 = 99,
  forlopstype2 = 99,
  onestage = 99
)
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}

\item{valgtVar}{Hvilken variabel skal plottes}

\item{datoFra}{Tidligste dato i utvalget (vises alltid i figuren).}

\item{datoTil}{Seneste dato i utvalget (vises alltid i figuren).}

\item{valgtShus}{Vektor med AvdResh over hvilke sykehus man genererer rapporten for.
Denne overstyrer reshID og er bare tilgjengelig for SC-bruker.}

\item{outfile}{Navn på fil figuren skrives til. Default: '' (Figur skrives
til systemets default output device (som regel skjerm))}

\item{preprosess}{Preprosesser data
FALSE: Nei (Default)
TRUE: Ja}

\item{minald}{Alder, fra og med (Default: 0)}

\item{maxald}{Alder, til og med (Default: 130)}

\item{erMann}{kjønn
1: menn
0: kvinner
99: begge (alt annet enn 0 og 1) (Default)}

\item{reshID}{Parameter følger fra innlogging helseregister.no og angir
hvilken enhet i spesialisthelsetjenesten brukeren tilhører}

\item{enhetsUtvalg}{Lag figur for
0: Hele landet
1: Egen enhet mot resten av landet (Default)
2: Egen enhet}

\item{hentData}{Gjør spørring mot database
FALSE: Nei, RegData gis som input til funksjonen (Default)
TRUE: Ja}

\item{forlopstype1}{Type forløp
1: Sfinkterplastikk
2: SNM
3: Oppfølging 1 år
4: Oppfølging 3 år}

\item{forlopstype2}{Type SNM-forløp (til dels) avhengig av testkonklusjon
1: Test - En test det ikke konkluderes på: Forløpet avsluttes. Ofte vil ny test
   foretas men ev. nytt forløp har ingen kobling til dette forløpet
2: Test og eventuell implantasjon - Permanent implantasjon tilbys
3: Revisjon - Et eksisterende implantat revideres, ingen kobling til ev. opprinnelig forløp
4: Eksplantasjon Et eksisterende implantat eksplanteres, ingen kobling til ev. opprinnelig forløp
5: Test eksplantasjon -}
}
\value{
En figur med søylediagram eller et stabelplot av ønsket variabel
}
\description{
Denne funksjonen lager et søylediagram eller stabelplot som viser andeler av valgt variabel
filtrert på de utvalg som er gjort.
}
