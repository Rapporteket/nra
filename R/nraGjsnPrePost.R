#' Denne funksjonen generer figur på gjennomsnitt før operasjon, etter 1 år og etter 5 år,
#' inkludert konfidensintervaller.
#'
#' Gjelder St. Marks, Generell livskvalitet og Seksualitet
#'
#' @inheritParams nraFigAndeler
#' @param Sammenlign
#'            0: Nei - Lag figur kun for pre
#'            1: Ja, 1 års - lag figur med pre og 1-års oppfølging
#'            2: Ja, alle år - lag figur med pre, 1-års og 5-års oppfølging
#'
#' @return En figur med gjennomsnitt før operasjon, etter 1 år og etter 5 år
#' @export

nraGjsnPrePost <- function(RegData, valgtVar, datoFra='2012-04-01', datoTil='2050-12-31',
                           valgtShus='', outfile = '', preprosess=TRUE, minald=0, maxald=130,
                           erMann='', reshID, enhetsUtvalg=0, hentData=F, forlopstype1='', forlopstype2='',
                           Sammenlign=0)
{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- nraHentRegData()
  }

  ## Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- nraPreprosess(RegData=RegData)
  }

  ## Fjerner reistreringer som mangler valgt variabel
  RegData$Variabel <- RegData[, valgtVar]
  RegData <- RegData[!is.na(RegData$Variabel), ]

  ## Gjør utvalg basert på brukervalg (LibUtvalg)

  if (valgtShus[1]!='') {
    valgtShus <- as.numeric(valgtShus)
    if (length(valgtShus)==1) {reshID<-valgtShus[1]}
  }

  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SenterKortNavn[match(reshID, RegData$AvdRESH)])
  }

  if (enhetsUtvalg!=0 & length(valgtShus)>1) {
    reshID <- 99
    RegData$AvdRESH[RegData$AvdRESH %in% valgtShus] <- reshID
    shtxt <- 'Ditt utvalg'
  }

  ## Skill ut oppfølginer
  Oppfolging1 <- RegData[RegData$ForlopsType1Num == 3, ]
  Oppfolging2 <- RegData[RegData$ForlopsType1Num == 4, ]

  RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ]

  if (enhetsUtvalg == 2) {RegData <- 	RegData[which(RegData$AvdRESH == reshID),]}

  nraUtvalg <- nraUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                         minald=minald, maxald=maxald, erMann=erMann, valgtShus=valgtShus,
                         forlopstype1=forlopstype1, forlopstype2=forlopstype2)
  RegData <- nraUtvalg$RegData
  utvalgTxt <- nraUtvalg$utvalgTxt


  if (Sammenlign == 0) {
    RegData1 <- RegData[,c("Variabel", "SenterKortNavn")]
    names(RegData)[names(RegData)=='Variabel'] <- 'VariabelPre'
  }

  if (Sammenlign == 1) {
    Oppfolging1 <- Oppfolging1[Oppfolging1$KobletForlopsID %in% RegData$ForlopsID, ]
    RegData <- RegData[RegData$ForlopsID %in% Oppfolging1$KobletForlopsID, ]
    RegData <- merge(RegData[,c("Variabel", "SenterKortNavn", "ForlopsID", "ForlopsType1Num")],
                      Oppfolging1[,c("Variabel", "KobletForlopsID", "ForlopsType1Num")], by.x = 'ForlopsID', by.y = 'KobletForlopsID',
                      suffixes = c('Pre', 'Post1'))
  }


  PrePost <- aggregate(RegData[, c('VariabelPre', "VariabelPost1")],
                           by=list(RegData$SenterKortNavn), mean, na.rm = TRUE)













}
