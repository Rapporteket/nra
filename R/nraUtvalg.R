#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#'
#' @inheritParams nraFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

nraUtvalg <- function(RegData, datoFra, datoTil, valgtShus='', minald, maxald, erMann,
                             forlopstype1, forlopstype2, fargepalett='BlaaRapp')
{
  # Definerer intersect-operator
  "%i%" <- intersect

  Ninn <- dim(RegData)[1]
  indVarMed <- 1:Ninn
  indAld <- which(RegData$PasientAlder >= minald & RegData$PasientAlder <= maxald)
  indDato <- which(RegData$HovedDato >= as.POSIXlt(datoFra) & RegData$HovedDato <= as.POSIXlt(datoTil))
  indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
  indForlop1 <- if (forlopstype1[1] != '') {which(RegData$ForlopsType1Num %in% as.numeric(forlopstype1))} else {indForlop1 <- 1:Ninn}
  indForlop2 <- if (forlopstype2[1] != '') {which(RegData$ForlopsType2Num %in% as.numeric(forlopstype2))} else {indForlop2 <- 1:Ninn}

  indMed <- indAld %i% indDato %i% indKj %i% indVarMed %i% indForlop1 %i% indForlop2
  RegData <- RegData[indMed,]

  utvalgTxt <- c(paste('Dato: ',
                       min(RegData$HovedDato, na.rm=T), ' til ', max(RegData$HovedDato, na.rm=T), sep='' ),
                 if ((minald>0) | (maxald<130)) {
                   paste('Pasienter fra ', min(RegData$PasientAlder, na.rm=T), ' til ', max(RegData$PasientAlder, na.rm=T), ' år', sep='')},
                 if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
                 if (length(valgtShus)>1) {paste0('Valgte RESH: ', paste(as.character(valgtShus), collapse=','))},
                 if (forlopstype1[1] !='') {paste0('Hovedforløp: ', paste(as.character(RegData$ForlopsType1[
                   match(as.numeric(forlopstype1), RegData$ForlopsType1Num)]), collapse=', '))},
                 if (forlopstype2[1] !='') {paste0('Underforløp: ', paste(as.character(RegData$ForlopsType2[
                   match(as.numeric(forlopstype2), RegData$ForlopsType2Num)]), collapse=', '))}
  )


  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett)
  return(invisible(UtData))
}
