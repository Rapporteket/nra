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
                           outfile = '', preprosess=TRUE, minald=0, maxald=130, grvar='SenterKortNavn',
                           erMann=99, reshID, hentData=F, forlopstype1=99, forlopstype2=99,
                           sammenlign=0, inkl_konf=0, egen_mot_landet=F, valgtShus='', graa='')
{
  egetShus <- RegData$SenterKortNavn[match(reshID, RegData$AvdRESH)]
  RegData$Grvar <- RegData[, grvar]
  if (valgtShus != '') {RegData <- RegData[which(RegData$AvdRESH %in% valgtShus), ]}

  if (valgtVar %in% c('StMarksTotalScore', 'GenQol', 'QolSexualitet', 'WexnerTotalScore')) {
    inkl_konf <- 1
  }

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- nraHentRegData()
  }

  ## Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- nraPreprosess(RegData=RegData)
  }

  ## Fjerner registreringer som mangler valgt variabel
  RegData$Variabel <- RegData[, valgtVar]
  RegData <- RegData[!is.na(RegData$Variabel), ]
  if (valgtVar=='Urinlekkasje') {
    RegData <- RegData[RegData$Urinlekkasje != 9, ]
    RegData$Variabel <- 100*RegData$Variabel}

  ## Skill ut oppfølginger
  Oppfolging1 <- RegData[RegData$ForlopsType1Num == 3, ]
  Oppfolging2 <- RegData[RegData$ForlopsType1Num == 4, ]

  RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ]

  nraUtvalg <- nraUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                         minald=minald, maxald=maxald, erMann=erMann,
                         forlopstype1=forlopstype1, forlopstype2=forlopstype2, valgtShus=valgtShus)
  RegData <- nraUtvalg$RegData
  utvalgTxt <- nraUtvalg$utvalgTxt

  if (dim(RegData)[1]>4) {
    if (sammenlign == 0) {
      RegData <- RegData[,c("Variabel", "Grvar", "ForlopsID")]
      names(RegData)[names(RegData)=='Variabel'] <- 'VariabelPre'
      if (valgtVar=='QolSexualitet') {
        Nuaktuelt <- length(RegData$VariabelPre[RegData$VariabelPre==99])
        RegData <- RegData[RegData$VariabelPre!=99, ]
      }
      nraUtvalg <- nraUtvalg(RegData=nraUtvalg$RegData[nraUtvalg$RegData$ForlopsID %in% RegData$ForlopsID, ], # I tilfelle utvalget er endret
                             datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,   # ved fjerning av registreringer
                             forlopstype1=forlopstype1, forlopstype2=forlopstype2, valgtShus=valgtShus)
      utvalgTxt <- nraUtvalg$utvalgTxt
      Pre <- aggregate(RegData$VariabelPre, by=list(RegData$Grvar), mean, na.rm = TRUE)
      PrePostSD <- aggregate(RegData[, c('VariabelPre')],
                               by=list(RegData$Grvar), sd, na.rm = TRUE)
      PrePostSD <- cbind(as.matrix(t(PrePostSD[,-1])), sd(RegData[, c('VariabelPre')], na.rm=T))
      PlotMatrise <- as.matrix(t(Pre[,-1]))
      PlotMatrise <- cbind(PlotMatrise, mean(RegData[, c('VariabelPre')]))
      Ngr <- table(as.character(RegData$Grvar))  ######## Må forsikre at rekkefølgen av sykehus blir lik som i PlotMatrise
      Ngr <- c(Ngr, sum(Ngr))
      tittel2 <- 'før operasjon'
    }

    if (sammenlign == 1) {
      RegData <- RegData[which(RegData$OppflgRegStatus %in% 1:2), ]
      Oppfolging1 <- Oppfolging1[Oppfolging1$KobletForlopsID %in% RegData$ForlopsID, ]
      RegData <- RegData[RegData$ForlopsID %in% Oppfolging1$KobletForlopsID, ]
      RegData <- merge(RegData[,c("PasientID", "Variabel", "Grvar", "ForlopsID", "ForlopsType1Num")],
                       Oppfolging1[,c("Variabel", "KobletForlopsID", "ForlopsType1Num")], by.x = 'ForlopsID', by.y = 'KobletForlopsID',
                       suffixes = c('Pre', 'Post1'))
      if (valgtVar=='QolSexualitet') {
        Nuaktuelt <- length(RegData$VariabelPre[RegData$VariabelPre==99 | RegData$VariabelPost1==99])
        RegData <- RegData[RegData$VariabelPre!=99, ]
        RegData <- RegData[RegData$VariabelPost1!=99, ]
      }
      nraUtvalg <- nraUtvalg(RegData=nraUtvalg$RegData[nraUtvalg$RegData$ForlopsID %in% RegData$ForlopsID, ], # I tilfelle utvalget er endret
                             datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,   # ved fjerning av registreringer
                             forlopstype1=forlopstype1, forlopstype2=forlopstype2, valgtShus=valgtShus)
      utvalgTxt <- nraUtvalg$utvalgTxt
      PrePost <- aggregate(RegData[, c('VariabelPre', "VariabelPost1")],
                           by=list(RegData$Grvar), mean, na.rm = TRUE)
      PrePostSD <- aggregate(RegData[, c('VariabelPre', "VariabelPost1")],
                            by=list(RegData$Grvar), sd, na.rm = TRUE)
      PrePostSD <- cbind(as.matrix(t(PrePostSD[,-1])), apply(RegData[, c('VariabelPre', "VariabelPost1")], 2, sd, na.rm=T))
      PlotMatrise <- as.matrix(t(PrePost[,-1]))
      PlotMatrise <- cbind(PlotMatrise, colMeans(RegData[, c('VariabelPre', "VariabelPost1")]))
      Ngr <- table(as.character(RegData$Grvar)) ######## Må forsikre at rekkefølgen av sykehus blir lik som i PlotMatrise
      Ngr <- c(Ngr, sum(Ngr))



      tittel2 <- 'før og etter (12mnd) operasjon'
    }

    if (sammenlign == 2) {
      RegData <- RegData[which(RegData$OppflgRegStatus %in% 1:2), ]
      Oppfolging1 <- Oppfolging1[Oppfolging1$KobletForlopsID %in% RegData$ForlopsID, ]
      RegData <- RegData[RegData$ForlopsID %in% Oppfolging1$KobletForlopsID, ]
      RegData <- merge(RegData[,c("PasientID", "Variabel", "Grvar", "ForlopsID", "ForlopsType1Num")],
                       Oppfolging1[,c("Variabel", "KobletForlopsID", "ForlopsType1Num")], by.x = 'ForlopsID', by.y = 'KobletForlopsID',
                       suffixes = c('', 'Post1'))
      Oppfolging2 <- Oppfolging2[Oppfolging2$KobletForlopsID %in% RegData$ForlopsID, ]
      RegData <- RegData[RegData$ForlopsID %in% Oppfolging2$KobletForlopsID, ]
      RegData <- merge(RegData[,c("PasientID", "Variabel", "VariabelPost1", "Grvar", "ForlopsID", "ForlopsType1Num")],
                       Oppfolging2[,c("Variabel", "KobletForlopsID", "ForlopsType1Num")], by.x = 'ForlopsID', by.y = 'KobletForlopsID',
                       suffixes = c('', 'Post5'))
      if (valgtVar=='QolSexualitet') {
        Nuaktuelt <- length(RegData$Variabel[RegData$Variabel==99 | RegData$VariabelPost5==99 | RegData$Variabel==99])
        RegData <- RegData[RegData$Variabel!=99, ]
        RegData <- RegData[RegData$VariabelPost1!=99, ]
        RegData <- RegData[RegData$VariabelPost5!=99, ]
      }
      nraUtvalg <- nraUtvalg(RegData=nraUtvalg$RegData[nraUtvalg$RegData$ForlopsID %in% RegData$ForlopsID, ], # I tilfelle utvalget er endret
                             datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,   # ved fjerning av registreringer
                             forlopstype1=forlopstype1, forlopstype2=forlopstype2, valgtShus=valgtShus)
      utvalgTxt <- nraUtvalg$utvalgTxt
      PrePost <- aggregate(RegData[, c('Variabel', 'VariabelPost1', "VariabelPost5")],
                           by=list(RegData$Grvar), mean, na.rm = TRUE)
      PrePostSD <- aggregate(RegData[, c('Variabel', 'VariabelPost1', "VariabelPost5")],
                             by=list(RegData$Grvar), sd, na.rm = TRUE)
      PrePostSD <- cbind(as.matrix(t(PrePostSD[,-1])), apply(RegData[, c('Variabel', 'VariabelPost1', "VariabelPost5")], 2, sd, na.rm=T))
      PlotMatrise <- as.matrix(t(PrePost[,-1]))
      PlotMatrise <- cbind(PlotMatrise, colMeans(RegData[, c('Variabel', 'VariabelPost1', "VariabelPost5")]))
      Ngr <- table(as.character(RegData$Grvar))  ######## Må forsikre at rekkefølgen av sykehus blir lik som i PlotMatrise
      Ngr <- c(Ngr, sum(Ngr))
      tittel2 <- 'før og etter (1 og 5 år) operasjon'
    }

    ############## Lag figur  ###############################

    grtxt <- c(names(Ngr)[1:(length(Ngr)-1)], 'Samlet')
    tittel <- switch(valgtVar,
                     'StMarksTotalScore' = paste0('St. Marks score ', tittel2, ', inkl. 95 % konf.int.'),
                     'GenQol' = c(paste0('Generell livskvalitet ', tittel2, ', inkl. 95 % konf.int.'), 'Skala fra 0=\'Verst tenkelig\' til 10=\'Best tenkelig\''),
                     'QolSexualitet' = c(paste0('Påvirkning av seksualliv ', tittel2, ', inkl. 95 % konf.int.'),
                                         'Skala fra 0=\'I svært liten grad\' til 10=\'I svært stor grad\'',
                                         paste0('Spørsmålet uaktuelt i ', Nuaktuelt, ' forløp')),
                     'Urinlekkasje' = paste0('Andel med urinlekkasje ', tittel2),
                     'WexnerTotalScore' = paste0('Wexner ', tittel2, ', inkl. 95 % konf.int.')
    )

    ytekst <- switch(valgtVar,
                     'StMarksTotalScore' = 'Gjennomsnittsscore',
                     'GenQol' = 'Gjennomsnittsscore',
                     'QolSexualitet' = 'Gjennomsnittsscore',
                     'Urinlekkasje' = 'Andel i prosent',
                     'WexnerTotalScore' = 'Gjennomsnittsscore'
    )
    cexgr<-0.9
    cexleg <- 0.9	#Størrelse på legendtekst
    retn<-'V'
    txtretn<-1

    FigTypUt <- rapFigurer::figtype(outfile, fargepalett='BlaaOff')
    NutvTxt <- length(utvalgTxt)
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

    PlotMatrise[ , Ngr < 5] <- 0
    grtxt2 <-  paste0('(N=', Ngr, ')')
    grtxt2[Ngr<5] <- '(N<5)'

    farger <- FigTypUt$farger
    ymax <- max(PlotMatrise, na.rm=T)*1.25

    soyleTxt <- PlotMatrise
    soyleTxt[ , Ngr < 5] <- NA
    soyleTxt <- sprintf('%.1f', soyleTxt)


    N_matr <- t(matrix(Ngr, nrow=length(Ngr), ncol=sammenlign+1))
    KIned <- PlotMatrise - qt(.975, N_matr-1)*PrePostSD/sqrt(N_matr)
    KIopp <- PlotMatrise + qt(.975, N_matr-1)*PrePostSD/sqrt(N_matr)
    KIned <- PlotMatrise - qt(.975, N_matr-1)*PrePostSD/sqrt(N_matr)
    KIopp <- PlotMatrise + qt(.975, N_matr-1)*PrePostSD/sqrt(N_matr)

    KIned[ , Ngr < 5] <- 0
    KIopp[ , Ngr < 5] <- 0
    if(inkl_konf==1) {
      ymax <- max(KIopp, na.rm=T)*1.25
    }

    if (egen_mot_landet) {
      ind_med <- which(grtxt %in% c(egetShus, 'Samlet'))
      PlotMatrise <- PlotMatrise[, ind_med]
      grtxt <- grtxt[ind_med]
      grtxt2 <- grtxt2[ind_med]
      KIned <- KIned[, ind_med]
      KIopp <- KIopp[, ind_med]
      Ngr <- Ngr[ind_med]
      soyleTxt <- soyleTxt[c(2*ind_med[1]-1, 2*ind_med[1], 2*ind_med[2]-1, 2*ind_med[2])]
    }
soylefarger <- matrix(farger[1:(sammenlign+1)], nrow = sammenlign+1, ncol = length(grtxt))
soylefarger[, which(grtxt %in% graa)] <- c('gray40', 'gray70', 'gray80')[1:(sammenlign+1)]

    pos <- barplot(PlotMatrise, beside=TRUE, las=txtretn, ylab=ytekst,
                   col=soylefarger, border='white', ylim=c(0, ymax))
    mtext(at=colMeans(pos), grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
    mtext(at=colMeans(pos), grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
    # text(x=pos, y=PlotMatrise, sprintf('%.1f', PlotMatrise), pos=1, cex=cexgr, col=farger[4])
    text(x=pos, y=0, soyleTxt, pos=3, cex=cexgr, col=farger[4])

    title(tittel, line=1, font.main=1)
    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    if (sammenlign > 0){
      legend('top', c('Pre', 'Oppflg. 1 år', 'Oppflg. 5 år')[1:(sammenlign+1)],
             border=c(fargeHoved,NA), col=farger[1:(sammenlign+1)], bty='n', pch=c(15,15), pt.cex=2,
             lwd=3,	lty=NA, ncol=2, cex=cexleg)
    }

    if(inkl_konf==1) {
      arrows(pos[ , Ngr > 4], KIned[ , Ngr > 4], pos[ , Ngr > 4], KIopp[ , Ngr > 4], code=3, angle=90, lwd=1.5, col='gray', length=0.05)
    }


    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}

    utdata <- list(PlotMatrise=PlotMatrise, KIned=KIned, KIopp=KIopp, utvalgTxt=utvalgTxt, tittel=tittel, grtxt=grtxt, grtxt2=grtxt2, Ngr=Ngr)

  } else {
    FigTypUt <- rapFigurer::figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'Færre enn 5 registreringer', cex=1.2)
    if ( outfile != '') {dev.off()}

    utdata <- NA
  }

  return(invisible(utdata))

}
