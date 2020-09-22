#' Plot andeler/andeler i angitt format
#'
#' @return Et plot av andeler over tre år
#'
#' @export
#'
indikatorFigRaterGrVar_basis <- function(RegData, valgtVar='reduksjon_lekkasje50', outfile='', width=600, height=600,
                                   decreasing=F, terskel=0, minstekrav = NA, maal = NA, xtekst ='Andel %',
                                   til100 = F, skriftStr=1.3, pktStr=1.5, datoFra='2016-01-01', datoTil='2050-12-31',
                                   hentData=F, preprosess=T, minald=0, maxald=130, erMann='',
                                   forlopstype1=99, forlopstype2=99)
{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- nraHentRegData()
  }

  ## Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- nraPreprosess(RegData=RegData)
  }

  if (valgtVar=='reduksjon_lekkasje50') {
    tittel <- c('Andel med prosentvis reduksjon', 'i lekkasjeepisoper >= 50%')
    maal <- 50
    RegData <- RegData[-which(rowSums(is.na(RegData[, c("InkontinensFoerTest","InkontinensUnderTest")])) !=0 ), ]
    RegData$pst_endr <- (RegData$InkontinensFoerTest-RegData$InkontinensUnderTest)/RegData$InkontinensFoerTest*100
    RegData$pst_endr[is.nan(RegData$pst_endr)] <- 0
    RegData$Indikator <- NA
    RegData$Indikator[RegData$pst_endr >= 50] <- 1
    RegData$Indikator[RegData$pst_endr < 50] <- 0
  }

  if (valgtVar=='reduksjon_lekkasje75') {
    tittel <- c('Andel med prosentvis reduksjon', 'i lekkasjeepisoper >= 75%')
    maal <- 70
    RegData <- RegData[-which(rowSums(is.na(RegData[, c("InkontinensFoerTest","InkontinensUnderTest")])) !=0 ), ]
    RegData$pst_endr <- (RegData$InkontinensFoerTest-RegData$InkontinensUnderTest)/RegData$InkontinensFoerTest*100
    RegData$pst_endr[is.nan(RegData$pst_endr)] <- 0
    RegData$Indikator <- NA
    RegData$Indikator[RegData$pst_endr >= 75] <- 1
    RegData$Indikator[RegData$pst_endr < 75] <- 0
  }

  if (valgtVar=='utfort_ultralyd') {
    tittel <- c('Andel med utført ultralyd')
    maal <- 95
    RegData <- RegData[which(RegData$Ultralyd %in% 0:2), ]
    RegData$Indikator <- RegData$Sfinktervurdering
    RegData$Indikator <- as.numeric(RegData$Indikator != 99)
  }

  if (valgtVar == 'saarinfeksjon') {
    tittel <- c('Andel med sårinfeksjon')
    maal <- 4
    RegData$Variabel <- pmax(RegData$Komplikasjon, RegData$KomplikasjonT2, na.rm = T)
    RegData <- RegData[RegData$ForlopsType1Num == 2, ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData$Variabel[which(RegData$Variabel==9 & (RegData$Komplikasjon==2 | RegData$KomplikasjonT2==2))] <- 2   # Velg bekreftet eller mistenkt
    RegData$Variabel[which(RegData$Variabel==9 & (RegData$Komplikasjon==1 | RegData$KomplikasjonT2==1))] <- 1
    RegData$Variabel[which(RegData$Variabel==98 & (RegData$Komplikasjon==2 | RegData$KomplikasjonT2==2))] <- 2   # Velg bekreftet eller mistenkt
    RegData$Variabel[which(RegData$Variabel==98 & (RegData$Komplikasjon==1 | RegData$KomplikasjonT2==1))] <- 1
    RegData$Indikator <- as.numeric(RegData$Variabel == 2)
  }


  ## Gjør utvalg basert på brukervalg
  nraUtvalg <- nraUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                         minald=minald, maxald=maxald, erMann=erMann,
                         forlopstype1=forlopstype1, forlopstype2=forlopstype2)

  RegData <- nraUtvalg$RegData

  andeler <- RegData %>% group_by(SenterKortNavn) %>% summarise(n = sum(Indikator),
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

  # x11()

  FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], dim(andeler)[1])
  soyleFarger[which(substr(andeler$SenterKortNavn, 1, 6)=='Nasjon')] <- farger[4]
  # windows(width = width, height = height)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig

  cexgr <- skriftStr

  #Hvis for få observasjoner..
  if (max(!is.na(andeler$Andel))==0) {
    #-----------Figur---------------------------------------
    plot.new()
    text(0.5, 0.6, 'Færre enn 5 registreringer alle grupper', cex=cexgr)


  } else {

    if (til100) {xmax <- 100
    } else {
      xmax <- max(c(andeler$Andel, maal), na.rm = T)*1.1
      # if (!is.na(maal)) {
      #   xmax <- max(max(andeler$Andel, na.rm = T), maal)*1.1
      # }
    }

    vmarg <- max(0, strwidth(andeler$SenterKortNavn, units='figure', cex=1.2*cexgr)*0.9)
    # par('fig'=c(vmarg, 1, 0, 1))
    NutvTxt <- length(nraUtvalg$utvalgTxt)
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))
    # par('mar'=c(5.1, 4.1, 4.1, 9.1))

    ypos <- barplot( t(andeler$Andel), beside=T, las=1,
                     # main = tittel,
                     font.main=1, cex.main=cexgr, cex.lab=0.8*cexgr,
                     # xlim=c(0,max(andeler, na.rm = T)*1.1),
                     xlim=c(0,xmax), xlab = 'Andel (%)',
                     names.arg=rep('',dim(andeler)[1]),
                     horiz=T, axes=F, space=c(0,0.3),
                     col=soyleFarger, border=NA,
                     ylim = c(0,dim(andeler)[1]*1.4)) # '#96BBE7'
    ypos <- as.vector(ypos)

    if (!is.na(maal)) {
      lines(x=rep(maal, 2), y=c(-1, max(ypos)+diff(ypos)[1]/2), col=farger[2], lwd=2)
      par(xpd=TRUE)
      text(x=maal, y=max(ypos)+diff(ypos)[1]/2, labels = paste0('Mål=',maal,'%'), pos = 4, cex=0.7*cexgr)
      par(xpd=FALSE)
    }
    axis(1,cex.axis=0.8*cexgr)
    mtext(andeler$SenterKortNavn, side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
    text(x=0, y=ypos, labels = pst_txt, cex=cexgr*0.9,pos=4)

    title(tittel, line=1, font.main=1, cex.main = 0.9*cexgr)
    #Tekst som angir hvilket utvalg som er gjort
    mtext(nraUtvalg$utvalgTxt, side=3, las=1, cex=0.6*cexgr, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


    # par('mar'= oldpar_mar)
    par('fig'= oldpar_fig)
  }

  if (outfile != '') {dev.off()}


}
