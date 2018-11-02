#' Plot andeler/andeler i angitt format
#'
#' Denne funksjonen tar som input en dataramme med andeler over 3 år,
#' der radnavn angir grupperingsvariabel og kolonnenavn år. Funksjonen
#' returnerer et søyleplot hvor søylene representerer sist år, fyllt sirkel er året
#' før og åpen sirkel to år før
#'
#' @param andeler En dataramme med andeler/andeler i spesifisert form
#' @param outfile Angir filnavn og format på figuren som returneres,
#' @param N En vektor/matrise med N for ratene
#' @return Et plot av andeler over tre år
#'
#' @export
#'
indikatorFigRaterGrVar <- function(RegData, valgtVar='StMarksMindreEnn9', outfile='', width=600, height=600,
                                   decreasing=F, terskel=0, minstekrav = NA, maal = NA, xtekst ='Andel %',
                                   til100 = F, skriftStr=1.3, pktStr=1.5, datoFra='2016-01-01', datoTil='2050-12-31',
                                   hentData=F, preprosess=T, minald=0, maxald=130, erMann='',
                                   forlopstype1='', forlopstype2='')
  {

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- nraHentRegData()
  }

  ## Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- nraPreprosess(RegData=RegData)
  }

  if (valgtVar=='StMarksMindreEnn9') {
    tittel <- 'Andel med St. Marks mindre enn 9 etter 1 år'
    maal <- 30
    RegData$Indikator <- NA
    RegData$Indikator[which(RegData$StMarksTotalScore<9)] <- 1
    RegData$Indikator[which(RegData$StMarksTotalScore>=9)] <- 0
  }

  if (valgtVar=='StMarksMindreEnn12') {
    tittel <- 'Andel med St. Marks mindre enn 12 etter 1 år'
    maal <- 50
    RegData$Indikator <- NA
    RegData$Indikator[which(RegData$StMarksTotalScore<12)] <- 1
    RegData$Indikator[which(RegData$StMarksTotalScore>=12)] <- 0
  }

  if (valgtVar=='WexnerMindreEnn9') {
    tittel <- 'Andel med Wexner mindre enn 9 etter 1 år'
    maal <- 30
    RegData$Indikator <- NA
    RegData$Indikator[which(RegData$WexnerTotalScore<9)] <- 1
    RegData$Indikator[which(RegData$WexnerTotalScore>=9)] <- 0
  }

  if (valgtVar=='WexnerMindreEnn12') {
    tittel <- 'Andel med Wexner mindre enn 12 etter 1 år'
    maal <- 50
    RegData$Indikator <- NA
    RegData$Indikator[which(RegData$WexnerTotalScore<12)] <- 1
    RegData$Indikator[which(RegData$WexnerTotalScore>=12)] <- 0
  }

  ## Skill ut oppfølginger
  Oppfolging1 <- RegData[RegData$ForlopsType1Num == 3, ] # 1-årsoppfølging
  Oppfolging1 <- Oppfolging1[!is.na(Oppfolging1$Indikator), ]
  RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ] # Sfinkter og SNM basisregistreringer
  Oppfolging1 <- Oppfolging1[Oppfolging1$KobletForlopsID %in% RegData$ForlopsID, ] # Bare inkluder oppfølginger der det finnes basisreg
  RegData <- RegData[RegData$ForlopsID %in% Oppfolging1$KobletForlopsID, ]
  # RegData <- merge(RegData[,c("PasientID", "Variabel", "SenterKortNavn", "ForlopsID", "ForlopsType1Num")],
  #                  Oppfolging1[,c("Variabel", "KobletForlopsID", "ForlopsType1Num")], by.x = 'ForlopsID', by.y = 'KobletForlopsID',
  #                  suffixes = c('Pre', 'Post1'))
  # Oppfolging2 <- RegData[RegData$ForlopsType1Num == 4, ]
  #
  # RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ]

  ## Gjør utvalg basert på brukervalg
  nraUtvalg <- nraUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                         minald=minald, maxald=maxald, erMann=erMann,
                         forlopstype1=forlopstype1, forlopstype2=forlopstype2)

  RegData <- nraUtvalg$RegData
  Oppfolging1 <- Oppfolging1[Oppfolging1$KobletForlopsID %in% RegData$ForlopsID, ]

  andeler <- Oppfolging1 %>% group_by(SenterKortNavn) %>% summarise(n = sum(Indikator),
                                                         N = n())
  andeler$Andel <- andeler$n/andeler$N*100
  andeler <- rbind(andeler, tibble(SenterKortNavn = 'Nasjonalt', n=sum(andeler$n), N=sum(andeler$N), Andel=sum(andeler$n)/sum(andeler$N)*100))

  andeler$SenterKortNavn <- paste0(andeler$SenterKortNavn, ' (', andeler$N, ')')
  andeler$Andel[andeler$N < terskel] <- NA

  if (decreasing){
    rekkefolge <- order(andeler$Andel, decreasing = decreasing)
  } else {
    rekkefolge <- order(andeler$Andel, decreasing = decreasing, na.last = F)
  }
  andeler <- andeler[rekkefolge, ]
  andeler[andeler$N<terskel, -1] <- NA
  pst_txt <- sprintf('%.0f', andeler$Andel)
  pst_txt[is.na(andeler$Andel)] <- paste0('N<', terskel)
  pst_txt <- c(pst_txt, NA)

  andeler <- bind_rows(andeler, tibble(SenterKortNavn='(N)', n=NA, N=NA, Andel=NA))

  FigTypUt <- rapbase::figtype(outfile='', width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], dim(andeler)[1])
  soyleFarger[which(substr(andeler$SenterKortNavn, 1, 6)=='Nasjon')] <- farger[4]
  windows(width = width, height = height)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig

  cexgr <- skriftStr

  if (til100) {xmax <- 100
  } else {
    xmax <- max(c(andeler$Andel, maal), na.rm = T)*1.1
    # if (!is.na(maal)) {
    #   xmax <- max(max(andeler$Andel, na.rm = T), maal)*1.1
    # }
  }

  vmarg <- max(0, strwidth(andeler$SenterKortNavn, units='figure', cex=cexgr)*0.9)
  # par('fig'=c(vmarg, 1, 0, 1))
  NutvTxt <- length(nraUtvalg$utvalgTxt)
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))
  # par('mar'=c(5.1, 4.1, 4.1, 9.1))

  ypos <- barplot( t(andeler$Andel), beside=T, las=1,
                   # main = tittel,
                   font.main=1, cex.main=1.3,
                   # xlim=c(0,max(andeler, na.rm = T)*1.1),
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = xtekst,
                   ylim = c(0,dim(andeler)[1]*1.4)) # '#96BBE7'
  ypos <- as.vector(ypos)

  if (!is.na(maal)) {
    lines(x=rep(maal, 2), y=c(-1, max(ypos)+diff(ypos)[1]/2), col=farger[2], lwd=2)
    # barplot( t(andeler[,3]), beside=T, las=1,
    #          main = tittel, font.main=1, cex.main=1.3,
    #          # xlim=c(0,max(andeler, na.rm = T)*1.1),
    #          xlim=c(0,xmax),
    #          names.arg=rep('',dim(andeler)[1]),
    #          horiz=T, axes=F, space=c(0,0.3),
    #          col=soyleFarger, border=NA, xlab = 'Andel %', add=TRUE)
    par(xpd=TRUE)
    text(x=maal, y=max(ypos)+diff(ypos)[1]/2, labels = paste0('Mål=',maal,'%'), pos = 4, cex=0.7)
    par(xpd=FALSE)
  }
  axis(1,cex.axis=0.9)
  mtext(andeler$SenterKortNavn, side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  # mtext( c(N[,1], 2013), side=4, line=2.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  # mtext( c(N[,2], 2014), side=4, line=5.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  # mtext( c(N[,3], 2015), side=4, line=8.5, las=1, at=c(ypos, max(ypos)+diff(ypos)[1]), col=1, cex=cexgr, adj = 1)
  # mtext( 'Sykehus/HF', side=2, line=9.5, las=0, col=1, cex=cexgr)#, outer=TRUE)#, adj = 1)
   # mtext( 'N', side=4, line=5.5, las=1, at=max(ypos)+1.8*diff(ypos)[1], col=1, cex=cexgr, adj = 1)
  # points(y=ypos, x=andeler[,1],cex=pktStr) #'#4D4D4D'
  # points(y=ypos, x=andeler[,2],cex=pktStr,pch= 19)
  text(x=0, y=ypos, labels = pst_txt, cex=0.8,pos=4)
  # par(xpd=TRUE)
  # legend('top', inset=c(vmarg,-.03), cex=0.9, bty='n', # bg='white', box.col='white',
  #        lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
  #        legend=c('2013','2014', '2015'), ncol = 3)
  # par(xpd=FALSE)

#   legend(x=82, y=ypos[2]+1,xjust=0, cex=1.2, bty='o', bg='white', box.col='white',
#          lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1,1,2), col=c('black','black',farger[3]),
#          legend=c('2013','2014', '2015') )



  title(tittel, line=1, font.main=1, cex.main = 1.6)
  #Tekst som angir hvilket utvalg som er gjort
  mtext(nraUtvalg$utvalgTxt, side=3, las=1, cex.main=1.4, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


  # par('mar'= oldpar_mar)
  par('fig'= oldpar_fig)

  if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}


}
