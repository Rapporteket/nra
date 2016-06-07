#' Denne funksjonen oppsummerer SNM-dagboken med gjennomsnitt før og under test.
#' Bør den inkludere konfidensintervaller?
#'
#' @inheritParams nraFigAndeler
#'
#' @return En figur med gjennomsnitt før operasjon, etter 1 år og etter 5 år
#' @export

nraSNMdagbok <- function(RegData, datoFra='2012-04-01', datoTil='2050-12-31', valgtShus='',
                           outfile = '', preprosess=TRUE, minald=0, maxald=130, enhetsUtvalg=0,
                           erMann='', reshID, hentData=F, forlopstype1='', forlopstype2='')

{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- nraHentRegData()
  }

  ## Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- nraPreprosess(RegData=RegData)
    print(dim(RegData))
  }

  RegData <- RegData[RegData$ForlopsType1Num == 2, ]
  RegData <- RegData[!is.na(RegData$UrgencyFoerTest), ]

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

  if (enhetsUtvalg == 2) {RegData <- 	RegData[which(RegData$AvdRESH == reshID),]}

  nraUtvalg <- nraUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                         minald=minald, maxald=maxald, erMann=erMann, valgtShus=valgtShus,
                         forlopstype1=forlopstype1, forlopstype2=forlopstype2)
  RegData <- nraUtvalg$RegData
  utvalgTxt <- nraUtvalg$utvalgTxt

  if (enhetsUtvalg %in% c(0,2)) {		#Ikke sammenlikning
    medSml <- 0
    indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasett
    indRest <- NULL
    smltxt <- NULL
  } else {						#Skal gjøre sammenlikning
    medSml <- 1
    if (enhetsUtvalg == 1) {
      indHoved <-which(as.numeric(RegData$AvdRESH)==reshID)
      smltxt <- 'Landet forøvrig'
      indRest <- which(as.numeric(RegData$AvdRESH) != reshID)
    }
  }

  PlotMatrise <- list(Hoved = 0, Rest =0)
  Nrest <- 0

  PreGjsn <- colMeans(RegData[indHoved, c("InkontinensFoerTest", "UrgencyFoerTest", "AvfoeringerFoerTest", "LekkasjedagerFoer")], na.rm = TRUE)
  PostGjsn <- colMeans(RegData[indHoved, c("InkontinensUnderTest", "UrgencyUnderTest", "AvfoeringerUnderTest", "LekkasjedagerUnder")], na.rm = TRUE)
  PlotMatrise$Hoved <- as.matrix(rbind(PreGjsn, PostGjsn))
  NHoved <- length(indHoved)
  if (medSml==1) {
    PreGjsn <- colMeans(RegData[indRest, c("InkontinensFoerTest", "UrgencyFoerTest", "AvfoeringerFoerTest", "LekkasjedagerFoer")], na.rm = TRUE)
    PostGjsn <- colMeans(RegData[indRest, c("InkontinensUnderTest", "UrgencyUnderTest", "AvfoeringerUnderTest", "LekkasjedagerUnder")], na.rm = TRUE)
    PlotMatrise$Rest <- as.matrix(rbind(PreGjsn, PostGjsn))
    Nrest <- length(indRest)
  }

  ##### Plot ####################

  cexgr <- 0.9
  retn <- 'H'
  txtretn <- 1
  grtxt <- ''
  grtxt2 <- ''
  subtxt <- ''
  tittel <- 'SNM-dagbok'

  grtxt <- c('Inkontinensepisoder', 'Urgencyepisoder', 'Avføringsepisoder', 'Dager med lekkasje')

  #Hvis for få observasjoner..
  #if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg == 1)) {
  if (NHoved < 10 | (medSml ==1 & Nrest<10)) {
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(main='SNM-dagbok')
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.65, 'Færre enn 10 registreringer i hoved-', cex=1.2)
    text(0.55, 0.6, 'eller sammenlikningsgruppe', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {
    # x11()
    #Plottspesifikke parametre:
    FigTypUt <- figtype(outfile, fargepalett=nraUtvalg$fargepalett)
    NutvTxt <- length(utvalgTxt)
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    #Ngr <- matrix(c(AntPre, AntPost), antGr, 2)
    lwdRest <- 3	#tykkelse på linja som repr. landet
    cexleg <- 0.9	#Størrelse på legendtekst
    cexpt <- 2	#Størrelse på punkter (resten av landet)


    ymax <- 2*antGr*1.6
    xmax <- max(c(PlotMatrise$Hoved, PlotMatrise$Rest),na.rm=T)*1.25

    pos <- barplot(PlotMatrise$Hoved[2:1,antGr:1], beside=TRUE, horiz=TRUE, main='', las=1,
                   col=farger[c(1,2)], border='white', font.main=1,  xlim=c(0,xmax), ylim=c(0.25, 3.3)*antGr,
                   names.arg=rev(grtxt), cex.names=cexgr, xlab="Hendelser pr uke")

    if (medSml == 1) {
      points(PlotMatrise$Rest[2:1,antGr:1], y=pos+0.1,  col=fargeRest,  cex=cexpt, pch=18) #c("p","b","o"),
      legend('topleft', c(paste0('Før, N=', NHoved, ' '), 'Under test', paste0(smltxt, ' N=', Nrest)),
             text.width = c(0.2,0.2,0.21)*xmax, bty='n', pch=c(15,15,18), pt.cex=cexpt, #lty=c(NA,NA,NA),
             col=farger[c(2,1,3)], border=farger[c(2,1,3)], ncol=3, cex=cexleg)
    } else {
      legend('top', c('Før test', 'Under test',paste('N=',NHoved,sep='')), bty='n',
             fill=farger[c(2,1,NA)], border=NA, ncol=3, cex=cexleg)
    }

    title(tittel, font.main=1)	#line=0.5,
    title(shtxt, font.main=1, line=0.5)
    #Tekst som angir hvilket utvalg som er gjort
#     avst <- 0.8
#     utvpos <- 3+length(tittel)-1	#Startlinje for teksten
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}


  }


}
