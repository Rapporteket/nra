#' Denne funksjonen definerer en del nye (sammensatte) variabler relevante for rapporter i NRA
#' og rensker opp i andre.
#'
#'
#' @inheritParams nraFigAndeler
#'
#' @return Data En list med det filtrerte datasettet og sykehusnavnet som tilsvarer reshID
#'
#' @export

nraPreprosess <- function(RegData)
{
  # RegData <- RegData[RegData$BasisRegStatus==1,]
  RegData$HovedDato <- as.Date(RegData$HovedDato, format="%Y-%m-%d")
  RegData$Aar <- as.numeric(format(RegData$HovedDato, format="%Y"))
  RegData$Mnd <- as.numeric(format(RegData$HovedDato, format="%m"))
  RegData$SenterKortNavn <- trimws(RegData$SenterKortNavn)
  RegData$SenterKortNavn[RegData$SenterKortNavn=="Helse Ber"] <- "Haukeland"

  RegData$Sfinktervurdering <- NA
  RegData$Sfinktervurdering[RegData$Ultralyd==1 & RegData$PartiellDefekt==0 &
                              RegData$FullveggsdefektYtreSfinkter==0 &
                              RegData$FullveggsdefektIndreSfinkter==0] <- 0
  RegData$Sfinktervurdering[RegData$Ultralyd==1 & RegData$PartiellDefekt==1 &
                              RegData$FullveggsdefektYtreSfinkter==0 &
                              RegData$FullveggsdefektIndreSfinkter==0] <- 1
  RegData$Sfinktervurdering[RegData$Ultralyd==1 & RegData$PartiellDefekt==1 &
                              RegData$FullveggsdefektYtreSfinkter==0 &
                              RegData$FullveggsdefektIndreSfinkter %in% 1:2] <- 2
  RegData$Sfinktervurdering[RegData$Ultralyd==1 & RegData$PartiellDefekt==0 &
                              RegData$FullveggsdefektYtreSfinkter %in% 1:2 &
                              RegData$FullveggsdefektIndreSfinkter==0] <- 3
  RegData$Sfinktervurdering[RegData$Ultralyd==1 & RegData$PartiellDefekt==0 &
                              RegData$FullveggsdefektYtreSfinkter %in% 1:2 &
                              RegData$FullveggsdefektIndreSfinkter %in% 1:2] <- 4
  RegData$Sfinktervurdering[RegData$Ultralyd==1 & RegData$PartiellDefekt==0 &
                              RegData$FullveggsdefektYtreSfinkter==0 &
                              RegData$FullveggsdefektIndreSfinkter %in% 1:2] <- 5
  RegData$Sfinktervurdering[RegData$Ultralyd==2] <- 9
  RegData$Sfinktervurdering[RegData$Ultralyd==0] <- 99

  # RegData$Onestage <- factor(RegData$Onestage, levels = c(0,1), labels = c("Midl. elektr.", "Permanent"))
  RegData$Testprosedyre <- factor(RegData$Testprosedyre, levels = c(1,2), labels = c("Midl. elektr.", "Permanent"))

  return(invisible(RegData))
}
