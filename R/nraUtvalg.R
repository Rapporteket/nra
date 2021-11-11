#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#'
#' @inheritParams nraFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

nraUtvalg <- function(RegData, datoFra="2011-01-01", datoTil = "2100-01-01", valgtShus='', minald=0, maxald=130, erMann=99,
                             forlopstype1=99, forlopstype2=99, onestage=99, fargepalett='BlaaRapp')
{
  # Definerer intersect-operator
  "%i%" <- intersect

  Ninn <- dim(RegData)[1]
  indVarMed <- 1:Ninn
  indAld <- which(RegData$PasientAlder >= minald & RegData$PasientAlder <= maxald)
  indDato <- which(RegData$HovedDato >= datoFra & RegData$HovedDato <= datoTil)
  indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
  indForlop1 <- if (forlopstype1[1] != 99) {which(RegData$ForlopsType1Num %in% as.numeric(forlopstype1))} else {indForlop1 <- 1:Ninn}
  indForlop2 <- if (forlopstype2[1] != 99) {which(RegData$ForlopsType2Num %in% as.numeric(forlopstype2))} else {indForlop2 <- 1:Ninn}
  indOnestage <- if (onestage[1] != 99) {which(RegData$Onestage %in% as.numeric(onestage))} else {indOnestage <- 1:Ninn}

  indMed <- indAld %i% indDato %i% indKj %i% indVarMed %i% indForlop1 %i% indForlop2 %i% indOnestage
  RegData <- RegData[indMed,]

  if (dim(RegData)[1] > 0) {
    utvalgTxt <- c(paste('Dato: ', min(RegData$HovedDato, na.rm = T), ' til ', max(RegData$HovedDato, na.rm = T), sep='' ),
                 if ((minald>0) | (maxald<130)) {
                   paste('Pasienter fra ', min(RegData$PasientAlder, na.rm=T), ' til ', max(RegData$PasientAlder, na.rm=T), ' år', sep='')},
                 if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
                 if (valgtShus[1] != '') {paste0('Avdeling: ', paste(unique(RegData$SenterKortNavn[RegData$AvdRESH %in% valgtShus]),
                                                                      collapse=', '))},
                 if (forlopstype1[1] !=99) {paste0('Hovedforløp: ', paste(as.character(RegData$ForlopsType1[
                   match(as.numeric(forlopstype1), RegData$ForlopsType1Num)]), collapse=', '))},
                 if (forlopstype2[1] !=99) {paste0('SNM-type: ', paste(as.character(RegData$ForlopsType2[
                   match(as.numeric(forlopstype2), RegData$ForlopsType2Num)]), collapse=', '))},
                 if (onestage[1] != 99) {paste0("Onestage: ", c("Nei", "Ja")[onestage+1])}
    )} else {
      utvalgTxt <- 'Ingen registreringer for dette utvalget'
  }


  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett)
  return(invisible(UtData))
}
