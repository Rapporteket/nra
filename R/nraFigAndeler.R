#' Lag søylediagram eller stabelplott som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram eller stabelplot som viser andeler av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param valgtVar Hvilken variabel skal plottes
#' @param datoFra Tidligste dato i utvalget (vises alltid i figuren).
#' @param datoTil Seneste dato i utvalget (vises alltid i figuren).
#' @param minald Alder, fra og med (Default: 0)
#' @param maxald Alder, til og med (Default: 130)
#' @param erMann kjønn
#'                 1: menn
#'                 0: kvinner
#'                 99: begge (alt annet enn 0 og 1) (Default)
#' @param outfile Navn på fil figuren skrives til. Default: '' (Figur skrives
#'    til systemets default output device (som regel skjerm))
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param enhetsUtvalg Lag figur for
#'                 0: Hele landet
#'                 1: Egen enhet mot resten av landet (Default)
#'                 2: Egen enhet
#' @param preprosess Preprosesser data
#'                 FALSE: Nei (Default)
#'                 TRUE: Ja
#' @param hentData Gjør spørring mot database
#'                 FALSE: Nei, RegData gis som input til funksjonen (Default)
#'                 TRUE: Ja
#' @param valgtShus Vektor med AvdResh over hvilke sykehus man genererer rapporten for.
#'                  Denne overstyrer reshID og er bare tilgjengelig for SC-bruker.
#' @param forlopstype1 Type forløp
#'                  1: Sfinkterplastikk
#'                  2: SNM
#'                  3: Oppfølging 1 år
#'                  4: Oppfølging 3 år
#' @param forlopstype2 Type SNM-forløp (til dels) avhengig av testkonklusjon
#'                  1: Test - En test det ikke konkluderes på: Forløpet avsluttes. Ofte vil ny test
#'                     foretas men ev. nytt forløp har ingen kobling til dette forløpet
#'                  2: Test og eventuell implantasjon - Permanent implantasjon tilbys
#'                  3: Revisjon - Et eksisterende implantat revideres, ingen kobling til ev. opprinnelig forløp
#'                  4: Eksplantasjon Et eksisterende implantat eksplanteres, ingen kobling til ev. opprinnelig forløp
#'                  5: Test eksplantasjon -
#'
#' @return En figur med søylediagram eller et stabelplot av ønsket variabel
#'
#' @export


nraFigAndeler  <- function(RegData, valgtVar, datoFra='2012-04-01', datoTil='2050-12-31',
                           valgtShus='', outfile = '', preprosess=TRUE, minald=0, maxald=130,
                           erMann='', reshID, enhetsUtvalg=0, hentData=F, forlopstype1='', forlopstype2='')
{
  if (valgtVar == 'SNMdagbok') {
    nraSNMdagbok(RegData=RegData, datoFra=datoFra, datoTil=datoTil, enhetsUtvalg=enhetsUtvalg, valgtShus = valgtShus,
                 outfile = outfile, preprosess=preprosess, minald=minald, maxald=maxald,
                 erMann=erMann, reshID=reshID, hentData=hentData, forlopstype1=forlopstype1, forlopstype2=forlopstype2)
  } else {


    ## Hvis spørring skjer fra R på server. ######################
    if(hentData){
      RegData <- nraHentRegData()
    }

    ## Hvis RegData ikke har blitt preprosessert
    if (preprosess){
      RegData <- nraPreprosess(RegData=RegData)
    }

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
    if (valgtVar == 'Tilfredshet') {
      RegData <- merge(RegData, RegData[,c("Tilfredshet", "KobletForlopsID")], by.x = 'ForlopsID', by.y = 'KobletForlopsID',
                       suffixes = c('', 'Post1'), all.x = TRUE)
    }

    if (enhetsUtvalg == 2) {RegData <- 	RegData[which(RegData$AvdRESH == reshID),]}

    nraUtvalg <- nraUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                           minald=minald, maxald=maxald, erMann=erMann, valgtShus=valgtShus,
                           forlopstype1=forlopstype1, forlopstype2=forlopstype2)
    RegData <- nraUtvalg$RegData
    utvalgTxt <- nraUtvalg$utvalgTxt

    #Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
    Andeler <- list(Hoved = 0, Rest =0)
    NRest <- 0
    AntRest <- 0

    if (valgtVar %in% c('Etiologi', 'TidlBeh', 'KomplSfinkter')) {
      flerevar <- 1
    } else {
      flerevar <- 0
    }

    if (dim(RegData)[1] > 0) {
      if (flerevar == 0 ) {
        PlotParams <- nraPrepVar(RegData, valgtVar, enhetsUtvalg, reshID=reshID)
        RegData <- PlotParams$RegData; medSml <- PlotParams$medSml;
        indHoved <- PlotParams$indHoved; indRest <- PlotParams$indRest;
        PlotParams$RegData <- NA
        AntHoved <- table(RegData$VariabelGr[indHoved])
        NHoved <- sum(AntHoved)
        Andeler$Hoved <- 100*AntHoved/NHoved
        if (medSml==1) {
          AntRest <- table(RegData$VariabelGr[indRest])
          NRest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
          Andeler$Rest <- 100*AntRest/NRest
        }
      }

      #FIGURER SATT SAMMEN AV FLERE VARIABLE, ULIKT TOTALUTVALG
      if (flerevar == 1){
        utvalg <- c('Hoved', 'Rest')	#Hoved vil angi enhet, evt. hele landet hvis ikke gjøre sml, 'Rest' utgjør sammenligningsgruppa
        RegDataLand <- RegData

        if (enhetsUtvalg %in% c(0,2)) {
          indHoved <- 1:dim(RegData)[1]
          indRest <- NULL
          medSml <- 0
        } else {						#Skal gjøre sammenlikning
          medSml <- 1
          indHoved <-which(as.numeric(RegData$AvdRESH)==reshID)
          indRest <- which(as.numeric(RegData$AvdRESH) != reshID)
        }

        PlotParams <- nraPrepVar(RegData[indHoved, ], valgtVar, enhetsUtvalg, reshID=reshID)
        AntHoved <- PlotParams$AntVar
        NHoved <- max(PlotParams$NVar, na.rm=T)
        Andeler$Hoved <- 100*PlotParams$AntVar/PlotParams$NVar

        if (medSml == 1) {
          PlotParams2 <- nraPrepVar(RegData[indRest, ], valgtVar, enhetsUtvalg, reshID=reshID)
          AntRest <- PlotParams2$AntVar
          NRest <- max(PlotParams2$NVar,na.rm=T)	#length(indRest)- Kan inneholde NA
          Andeler$Rest <- 100*PlotParams2$AntVar/PlotParams2$NVar
          rm(PlotParams2)
        }

      }   #end sjekk om figuren inneholder flere variable



      ##-----------Figur---------------------------------------
      tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; grtxt2 <- PlotParams$grtxt2;
      stabel <- PlotParams$stabel; subtxt <- PlotParams$subtxt; incl_N <- PlotParams$incl_N;
      incl_pst <- PlotParams$incl_pst; retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;
      FigTypUt <- figtype(outfile=outfile, fargepalett=nraUtvalg$fargepalett, pointsizePDF=12);
      antDes <- PlotParams$antDes; smltxt <- PlotParams$smltxt;
    } else {
      NHoved <- 0
      NRest <- 0
      medSml <- 1
    }


    if ( NHoved < 5 | 	(medSml ==1 & NRest<5)) {	#(valgtVar=='Underkat' & all(hovedkat != c(1,2,5,7))) |
      FigTypUt <- figtype(outfile)
      farger <- FigTypUt$farger
      plot.new()
      # title(tittel)	#, line=-6)
      legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
      text(0.5, 0.6, 'Færre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex=1.2)
      if ( outfile != '') {dev.off()}

    } else {

      NutvTxt <- length(utvalgTxt)
      antDesTxt <- paste('%.', antDes, 'f', sep='')
      grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf(antDesTxt, Andeler$Hoved)), '%)', sep='')
      vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.75))
      par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

      farger <- FigTypUt$farger
      fargeHoved <- farger[1]
      fargeRest <- farger[3]
      antGr <- length(grtxt)
      lwdRest <- 3	#tykkelse på linja som repr. landet
      cexleg <- 1	#Størrelse på legendtekst

      #Horisontale søyler
      if (retn == 'H') {
        xmax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
        pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel,
                       col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
        if (NHoved>0) {mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)}

        if (medSml == 1) {
          points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
          legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''),
                          paste(smltxt, ' (N=', NRest,')', sep='')),
                 border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
                 lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
        } else {
          legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
                 border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
        }
      }

      if (retn == 'V' ) {
        #Vertikale søyler eller linje
        if (length(grtxt2) == 1) {grtxt2 <- paste('(', sprintf(antDesTxt, Andeler$Hoved), '%)', sep='')}
        ymax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
        pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",
                       xlab=subtxt, col=fargeHoved, border='white', ylim=c(0, ymax))	#sub=subtxt,
        mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
        mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
        if (medSml == 1) {
          points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
          legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')),
                 border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
                 lwd=lwdRest, ncol=2, cex=cexleg)
        } else {
          legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
                 border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
        }
      }

      # if (tittel==1) {title(Tittel, line=1, font.main=1)}
      title(tittel, line=1, font.main=1)
      #Tekst som angir hvilket utvalg som er gjort
      mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

      par('fig'=c(0, 1, 0, 1))
      if ( outfile != '') {dev.off()}
    }

    if (dim(RegData)[1] > 0) {
      #Beregninger som returneres fra funksjonen.
      AndelerUt <- rbind(Andeler$Hoved, Andeler$Rest)
      rownames(AndelerUt) <- c('Hoved', 'Rest')
      AntallUt <- rbind(AntHoved, AntRest)
      rownames(AntallUt) <- c('Hoved', 'Rest')

      UtData <- list(paste(toString(tittel),'.', sep=''), AndelerUt, AntallUt, grtxt )
      names(UtData) <- c('Tittel', 'Andeler', 'Antall', 'GruppeTekst')
      return(invisible(UtData))

    }
  }

}
