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






  return(invisible(RegData))
}
