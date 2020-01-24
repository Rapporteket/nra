#' Denne funksjonen generer figur på gjennomsnitt gruppert etter valgt grupperingsvariabel
#'
#' @inheritParams nraFigAndeler
#' @param valgtVar - tar verdien 'Tilfredshet'
#'
#' @return En figur med gjennomsnitt fordelt etter vagt grupperingsvariabel
#' @export

nraGjsnGrVar <- function(RegData, valgtVar='Tilfredshet', datoFra='2012-04-01', datoTil='2050-12-31',
                         outfile = '', preprosess=TRUE, minald=0, maxald=130, decreasing=F, egen_mot_landet=F,
                         erMann='', hentData=F, forlopstype1='', forlopstype2='', terskel=0, reshID=0,
                         inkl_konf=0, grvar='SenterKortNavn', width=600, height=600, xtekst='Gjennomsnitt',
                         graa = '')
{

  egetShus <- RegData$SenterKortNavn[match(reshID, RegData$AvdRESH)]

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- nraHentRegData()
  }

  ## Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- nraPreprosess(RegData=RegData)
  }

  RegData$grvar <- RegData[, grvar]


  if (valgtVar == 'Tilfredshet') {
    RegData <- merge(RegData[, -which(names(RegData)=="Tilfredshet")], RegData[,c("Tilfredshet", "KobletForlopsID")], by.x = 'ForlopsID', by.y = 'KobletForlopsID',
                     suffixes = c('', 'Post1'))
    RegData <- RegData[which(RegData$Tilfredshet %in% 0:10), ]
    tittel <- 'Tilfredshet med behandling'
  }

  ## Fjerner registreringer som mangler valgt variabel
  RegData$Variabel <- RegData[, valgtVar]
  RegData <- RegData[!is.na(RegData$Variabel), ]

  nraUtvalg <- nraUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                         minald=minald, maxald=maxald, erMann=erMann,
                         forlopstype1=forlopstype1, forlopstype2=forlopstype2)
  RegData <- nraUtvalg$RegData
  utvalgTxt <- nraUtvalg$utvalgTxt



  Tabell <- RegData %>% group_by(grvar) %>% summarise(summert = sum(Variabel),
                                                      N = n(),
                                                      gj.sn = mean(Variabel)) %>% ungroup()

  Tabell <- bind_rows(Tabell, tibble(grvar='Nasjonalt', summert=sum(Tabell$summert),
                                     N=sum(Tabell$N), gj.sn = sum(Tabell$summert)/sum(Tabell$N)))
  if (egen_mot_landet) {
    ind_med <- which(Tabell$grvar %in% c(egetShus, 'Nasjonalt'))
    Tabell <- Tabell[ind_med, ]
  }
  Tabell$grvar_ren <- Tabell$grvar
  Tabell$grvar <- paste0(Tabell$grvar, ' (', Tabell$N, ')')

  Tabell$gj.sn[Tabell$N < terskel] <- NA

  if (decreasing){
    rekkefolge <- order(Tabell$gj.sn, decreasing = decreasing)
  } else {
    rekkefolge <- order(Tabell$gj.sn, decreasing = decreasing, na.last = F)
  }
  Tabell <- Tabell[rekkefolge, ]
  # N <- N[rekkefolge, ]
  Tabell[Tabell$N<terskel, -1] <- NA

  col_txt <- sprintf('%.1f', Tabell$gj.sn)
  col_txt[is.na(Tabell$gj.sn)] <- paste0('N<', terskel)
  col_txt <- c(col_txt, NA)

  Tabell <- bind_rows(Tabell, tibble(grvar='(N)', summert=NA, N=NA, gj.sn=NA, grvar_ren=NA))

  FigTypUt <- rapbase::figtype(outfile='', width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], dim(Tabell)[1])
  soyleFarger[which(substr(Tabell$grvar, 1, 6)=='Nasjon')] <- farger[4]
  soyleFarger[which(Tabell$grvar_ren %in% graa)] <- 'gray88'
  windows(width = width, height = height)

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig


  cexgr <- 1.2
  xmax <- max(Tabell$gj.sn, na.rm = T)*1.1

  vmarg <- max(0, strwidth(Tabell$gj.sn, units='figure', cex=cexgr)*0.8)
  # par('fig'=c(vmarg, 1, 0, 1))
  NutvTxt <- length(nraUtvalg$utvalgTxt)
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))

  ypos <- barplot( t(Tabell$gj.sn), beside=T, las=1,
                   # main = tittel,
                   font.main=1, cex.main=1.3,
                   # xlim=c(0,max(andeler, na.rm = T)*1.1),
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(Tabell)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = xtekst)#,
  # ylim = c(0,dim(Tabell)[1]*1.4)) # '#96BBE7'
  ypos <- as.vector(ypos)

  axis(1,cex.axis=0.9)
  mtext(Tabell$grvar, side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  mtext( 'Sykehus/HF', side=2, line=9.5, las=0, col=1, cex=cexgr)
  text(x=0, y=ypos, labels = col_txt, cex=0.8,pos=4)


  title(tittel, line=1, font.main=1, cex=1.4)
  mtext(nraUtvalg$utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

  par('fig'= oldpar_fig)
  if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}

}






