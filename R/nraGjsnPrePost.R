#' Denne funksjonen generer figur på gjennomsnitt før operasjon, etter 1 år og etter 5 år,
#' inkludert konfidensintervaller.
#'
#' Gjelder St. Marks, Generell livskvalitet og Seksualitet
#'
#' @inheritParams nraFigAndeler
#' @param sammenlign
#'            0: Nei - Lag figur kun for pre
#'            1: Ja, 1 års - lag figur med pre og 1-års oppfølging
#'            2: Ja, alle år - lag figur med pre, 1-års og 5-års oppfølging
#'
#' @return En figur med gjennomsnitt før operasjon, etter 1 år og etter 5 år
#' @export

nraGjsnPrePost <- function(RegData, valgtVar, datoFra='2012-04-01', datoTil='2050-12-31',
                           outfile = '', preprosess=TRUE, minald=0, maxald=130,
                           erMann='', reshID, hentData=F, forlopstype1='', forlopstype2='',
                           sammenlign=0)
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
  if (valgtVar=='QolSexualitet') {RegData <- RegData[RegData$Variabel!=99, ]}

  ## Skill ut oppfølginger
  Oppfolging1 <- RegData[RegData$ForlopsType1Num == 3, ]
  Oppfolging2 <- RegData[RegData$ForlopsType1Num == 4, ]

  RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ]

  nraUtvalg <- nraUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                         minald=minald, maxald=maxald, erMann=erMann,
                         forlopstype1=forlopstype1, forlopstype2=forlopstype2)
  RegData <- nraUtvalg$RegData
  utvalgTxt <- nraUtvalg$utvalgTxt

  if (dim(RegData)[1]>4) {
    if (sammenlign == 0) {
      RegData <- RegData[,c("Variabel", "SenterKortNavn")]
      names(RegData)[names(RegData)=='Variabel'] <- 'VariabelPre'
      Pre <- aggregate(RegData$VariabelPre, by=list(RegData$SenterKortNavn), mean, na.rm = TRUE)
      PlotMatrise <- as.matrix(t(Pre[,-1]))
      PlotMatrise <- cbind(PlotMatrise, mean(RegData[, c('VariabelPre')]))
      Ngr <- table(RegData$SenterKortNavn)  ######## Må forsikre at rekkefølgen av sykehus blir lik som i PlotMatrise
      Ngr <- c(Ngr, sum(Ngr))
      tittel2 <- 'før operasjon'
    }

    if (sammenlign == 1) {
      Oppfolging1 <- Oppfolging1[Oppfolging1$KobletForlopsID %in% RegData$ForlopsID, ]
      RegData <- RegData[RegData$ForlopsID %in% Oppfolging1$KobletForlopsID, ]
      RegData <- merge(RegData[,c("PasientID", "Variabel", "SenterKortNavn", "ForlopsID", "ForlopsType1Num")],
                       Oppfolging1[,c("Variabel", "KobletForlopsID", "ForlopsType1Num")], by.x = 'ForlopsID', by.y = 'KobletForlopsID',
                       suffixes = c('Pre', 'Post1'))
      PrePost <- aggregate(RegData[, c('VariabelPre', "VariabelPost1")],
                           by=list(RegData$SenterKortNavn), mean, na.rm = TRUE)
      PlotMatrise <- as.matrix(t(PrePost[,-1]))
      PlotMatrise <- cbind(PlotMatrise, colMeans(RegData[, c('VariabelPre', "VariabelPost1")]))
      Ngr <- table(RegData$SenterKortNavn)  ######## Må forsikre at rekkefølgen av sykehus blir lik som i PlotMatrise
      Ngr <- c(Ngr, sum(Ngr))
      tittel2 <- 'før og etter (12mnd) operasjon'
    }

    if (sammenlign == 2) {
      ## Må finne ut hvordan koblingen mellom ForlopsID og KobletForlopsID fungerer ved 5års-oppfølging
      tittel2 <- 'før og etter (1 og 5 år) operasjon'
    }

    ############## Lag figur  ###############################

    grtxt <- c(names(Ngr)[1:(length(Ngr)-1)], 'Nasjonalt')
    tittel <- switch (valgtVar,
                      'StMarksTotalScore' = paste0('St. Marks score ', tittel2),
                      'GenQol' = c(paste0('Generell livskvalitet ', tittel2), 'Skala fra 0=\'Verst tenkelig\' til 10=\'Best tenkelig\''),
                      'QolSexualitet' = c(paste0('Påvirkning av seksualliv ', tittel2), 'Skala fra 0=\'I svært liten grad\' til 10=\'I svært stor grad\'')
    )

    'St. Marks score før og etter (12mnd) operasjon'
    cexgr<-0.9
    cexleg <- 0.9	#Størrelse på legendtekst
    retn<-'V'
    txtretn<-1

    FigTypUt <- figtype(outfile, fargepalett='BlaaOff')
    NutvTxt <- length(utvalgTxt)
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    ymax <- max(PlotMatrise, na.rm=T)*1.25

    grtxt2 <-  paste0('(N=', Ngr, ')')
    pos <- barplot(PlotMatrise, beside=TRUE, las=txtretn, ylab="Gjennomsnittsscore",
                   col=farger[1:(sammenlign+1)], border='white', ylim=c(0, ymax))
    mtext(at=colMeans(pos), grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
    mtext(at=colMeans(pos), grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)

    title(tittel, line=1, font.main=1)
    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}

  } else {
    FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'Færre enn 5 registreringer', cex=1.2)
    if ( outfile != '') {dev.off()}
  }


}
