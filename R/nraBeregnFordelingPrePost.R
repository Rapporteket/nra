#' Beregn fordeling av valgt variabel før operasjon og ved oppfølging
#'
#' Denne funksjonen gjør utvalg og beregninger for presentasjon av foredelingsfigur
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
#' @param valgtShus Vektor med AvdResh over hvilke sykehus man genererer rapporten for.
#'                  Denne overstyrer reshID og er bare tilgjengelig for SC-bruker.
#' @param forlopstype1 Type forløp
#'                  1: Sfinkterplastikk
#'                  2: SNM
#'                  3: Oppfølging 1 år
#'                  4: Oppfølging 5 år
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

nraBeregnFordelingPrePost  <- function(RegData, valgtVar, datoFra='2012-04-01',
                                       datoTil='2050-12-31', valgtShus='',
                                       outfile = '', preprosess=TRUE, minald=0,
                                       maxald=130, erMann=99, reshID, enhetsUtvalg=0,
                                       forlopstype1=99, forlopstype2=99,
                                       onestage=99, sammenlign=1)

  # RegData=RegData_alt; valgtVar="BegrensSeksLiv"; datoFra='2012-04-01';
  # datoTil='2050-12-31'; valgtShus='';
  # outfile = ''; preprosess=TRUE; minald=0;
  # maxald=130; erMann=99; reshID=601225; enhetsUtvalg=0;
  # forlopstype1=99; forlopstype2=99;
  # onestage=99; sammenlign=1
  {
    # RegData_alt <- RegData
    # Definer variabel for plotting
    PlotParams <- nraPrepVar(RegData=RegData, valgtVar=valgtVar,
                           enhetsUtvalg = 0, reshID = 0)
    RegData <- PlotParams$RegData
    # Lag utflatet datasett
    RegData <- merge(RegData[RegData$ForlopsType1Num %in% 1:2, ],
                     RegData[RegData$ForlopsType1Num %in% 3, ], by.x = "ForlopsID",
                     by.y = "KobletForlopsID", suffixes = c("", "_post1"))
    if (sammenlign == 2) {
      RegData <- merge(RegData,
                       RegData_alt[RegData_alt$ForlopsType1Num %in% 4, ], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post5"))
    }

    ## Gjør utvalg basert på brukervalg

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
                           forlopstype1=forlopstype1, forlopstype2=forlopstype2, onestage=onestage)
    RegData <- nraUtvalg$RegData
    utvalgTxt <- nraUtvalg$utvalgTxt

    if (valgtVar == "BegrensSeksLiv") {
      if (sammenlign == 1) {
        tittel <- c("Begrenser du ditt seksualliv på grunn av mulige uhell/lekkasjer ",
                    paste0("med hensyn til avføring/lukt, ikke aktuelt for ",
                           sum(is.na(RegData$VariabelGr) | is.na(RegData$VariabelGr_post1)),
                           " pasienter"))
        RegData <- RegData[!is.na(RegData$VariabelGr) & !is.na(RegData$VariabelGr_post1), ]}
      if (sammenlign == 2) {
        tittel <- c("Begrenser du ditt seksualliv på grunn av mulige uhell/lekkasjer ",
                    paste0("med hensyn til avføring/lukt, ikke aktuelt for ",
                           sum(is.na(RegData$VariabelGr) | is.na(RegData$VariabelGr_post1) |
                                 is.na(RegData$VariabelGr_post5)),
                           " pasienter"))
        RegData <- RegData[!is.na(RegData$VariabelGr) &
                             !is.na(RegData$VariabelGr_post1) &
                                      !is.na(RegData$VariabelGr_post5), ]}
      PlotParams$tittel <- tittel
    } else {
      if (sammenlign == 1) {
        RegData <- RegData[!is.na(RegData$VariabelGr) & !is.na(RegData$VariabelGr_post1), ]}
      if (sammenlign == 2) {
        RegData <- RegData[!is.na(RegData$VariabelGr) &
                             !is.na(RegData$VariabelGr_post1) &
                             !is.na(RegData$VariabelGr_post5), ]}
    }

    tabell <- RegData %>% summarise(foer_operasjon= n(), .by = VariabelGr) %>%
      merge(RegData %>% summarise(etter1aar = n(), .by = VariabelGr_post1),
            by.x = "VariabelGr", by.y = "VariabelGr_post1")
    if (sammenlign == 2) {
      tabell <- tabell %>%
        merge(RegData %>% summarise(etter5aar = n(), .by = VariabelGr_post5),
              by.x = "VariabelGr", by.y = "VariabelGr_post5")
    }

    N = sum(tabell$foer_operasjon)
    tabell <- tabell[-1]/N*100
    tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; grtxt2 <- PlotParams$grtxt2;
    stabel <- PlotParams$stabel; subtxt <- PlotParams$subtxt; incl_N <- PlotParams$incl_N;
    incl_pst <- PlotParams$incl_pst; retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;
    FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett=nraUtvalg$fargepalett, pointsizePDF=12);
    antDes <- PlotParams$antDes; smltxt <- PlotParams$smltxt;

    NutvTxt <- length(utvalgTxt)
    retn <- "V"
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.7))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    fargeHoved <- farger[1]
    fargeRest <- farger[3]

    # if (retn == 'V' ) {
      #Vertikale søyler eller linje
      ymax <- min(max(tabell,na.rm=T)*1.25, 110)
      pos <- barplot(t(tabell), beside=TRUE, las=1, ylab="Andel pasienter (%)",
                     sub=subtxt, cex.axis=cexgr, cex.sub=cexgr,	cex.lab=cexgr, # ,	names.arg=grtxt, cex.names=cexgr,
                     col=farger[c(2,1)], border='white', ylim=c(0, ymax), xaxt='n')
      mtext(at=colMeans(pos), grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
      mtext(at=colMeans(pos), grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
      text(pos[1,], 0.1, labels = sprintf("%.1f", tabell[,1]), pos = 3, cex = 0.8, col = "white")
      text(pos[2,], 0.1, labels = sprintf("%.1f", tabell[,2]), pos = 3, cex = 0.8, col = "white")

      legend('top', c('Før', 'Etter', paste('N=', N, sep='')), bty='n',
             fill=farger[c(2,1,NA)], border=NA, ncol=3, cex=1)




      title(tittel, font.main=1)	#line=0.5,
      title(shtxt, font.main=1, line=0.5, cex.main=1)
      mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

      if ( outfile != '') {dev.off()}

  }
