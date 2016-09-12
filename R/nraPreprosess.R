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
  RegData$HovedDato <- as.POSIXlt(RegData$HovedDato, format="%Y-%m-%d")
  RegData$Aar <- RegData$HovedDato$year+1900


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


  return(invisible(RegData))
}
