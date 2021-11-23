#' Preparer variabler for plotting
#'
#' Denne funksjonen grupperer og klargjør variabler for andelsplot
#'
#' Her kan detaljer skrives
#'
#' @inheritParams nraFigAndeler
#'
#' @return PrepData En liste med plotrelevante størrelser
#'
#' @export
#'
nraPrepVar <- function(RegData, valgtVar, enhetsUtvalg, reshID)
{
  stabel=FALSE; incl_N=FALSE; incl_pst=FALSE; retn= 'V'; tittel <- ''; inkl_konf=0; antDes=1;
  cexgr <- 1.0; grtxt <- ''; grtxt2 <- ''; subtxt <- ''; VarTxt <- ''; AntVar=NA; NVar=NA;


  RegData$Variabel <- NA
  if (valgtVar == 'PasientAlder') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData$Variabel <- as.numeric(RegData$Variabel)
    RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = T), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- c(paste0('Aldersfordeling. Gjsn=', sprintf("%.1f", mean(RegData$Variabel, na.rm = T)), ', median=', round(median(RegData$Variabel, na.rm = T),1),'.'),
                paste0('Minimum=', round(min(RegData$Variabel, na.rm = T),1), ', maksimum=', round(max(RegData$Variabel, na.rm = T),1),'.'))
    gr <- c(0, seq(25, 85, 10), 130)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    subtxt <- 'Aldersgrupper'
    grtxt <- levels(RegData$VariabelGr)
    grtxt[1] <- paste0('<', as.character(gr[2]))
    grtxt[length(grtxt)] <- paste0('>', as.character(gr[length(gr)-1]))
  }


  if (valgtVar == 'Tilfredshet') {
    retn <- 'H'
    RegData$Variabel <- RegData$TilfredshetPost1
    # RegData <- RegData[RegData$ForlopsType1Num %in% 3:4, ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- 'Tilfredshet med behandlingstilbudet'
    gr <- rev(0:10)
    grtxt <- rev(c('0=Svært misfornøyd', as.character(1:9), '10=Svært fornøyd'))
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
    # subtxt <- 'Aldersgrupper'
  }

  if (valgtVar == 'Komplikasjon') {
    retn <- 'H'
    RegData$Variabel <- RegData$Komplikasjon
    RegData <- RegData[RegData$ForlopsType1Num == 2, ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[RegData$Onestage != 1, ]  # Fjerner Onestage
    tittel <- 'Komplikasjoner SNM test innen 30 dager'
    gr <- c(0,1,2,9)
    grtxt <- c('Ingen', 'Sårinfeksjon mistenkt', 'Sårinfeksjon bekreftet','Annet')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
    # subtxt <- 'Aldersgrupper'
  }

  if (valgtVar == 'KomplikasjonT2') {
    retn <- 'H'
    RegData$Variabel <- RegData$KomplikasjonT2
    RegData <- RegData[RegData$ForlopsType1Num == 2, ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- 'Komplikasjoner SNM implantasjon innen 30 dager'
    gr <- c(0,1,2,9)
    grtxt <- c('Ingen', 'Sårinfeksjon mistenkt', 'Sårinfeksjon bekreftet','Annet')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
    # subtxt <- 'Aldersgrupper'
  }

  if (valgtVar == 'KomplSNMtot') {
    retn <- 'H'
    RegData$Variabel <- pmax(RegData$Komplikasjon, RegData$KomplikasjonT2, na.rm = T)
    RegData <- RegData[RegData$ForlopsType1Num == 2, ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData$Variabel[which(RegData$Variabel==9 & (RegData$Komplikasjon==2 | RegData$KomplikasjonT2==2))] <- 2   # Velg bekreftet eller mistenkt
    RegData$Variabel[which(RegData$Variabel==9 & (RegData$Komplikasjon==1 | RegData$KomplikasjonT2==1))] <- 1   # sårinfeksjon fremfor annet
    RegData$Variabel[which(RegData$Variabel==98 & (RegData$Komplikasjon==2 | RegData$KomplikasjonT2==2))] <- 2   # Velg bekreftet eller mistenkt
    RegData$Variabel[which(RegData$Variabel==98 & (RegData$Komplikasjon==1 | RegData$KomplikasjonT2==1))] <- 1
    tittel <- 'Komplikasjoner SNM innen 30 dager'
    gr <- c(0,1,2,9,98)
    grtxt <- c('Ingen', 'Sårinfeksjon mistenkt', 'Sårinfeksjon bekreftet', 'Annet', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

  if (valgtVar == 'Etiologi') {
    retn <- 'H'
    Ukjente <- unique(RegData$PasientID[which(RegData$ForlopsType1Num %in% 1:2 & RegData$Ukjent==1)]) # Bør man sjekke om PasientID også finnes
    RegData <- RegData[RegData$ForlopsType1Num %in% 1:2 & RegData$Ukjent==0, ]                        # i RegData?
    N <- length(unique(union(Ukjente, unique(RegData$PasientID))))

    SamletPrPID <- aggregate(RegData[, c("AnnenBekkenKirurgi", 'AnnetTraume', 'Hemoroidereksjon', 'NevrologiskSykdom',
                                 'ObsteriskSkade','PeriferNervskade', 'PerinealAbscess', 'Rectumreseksjon', 'Sfinkterotomi', 'AnnetEtiologi')],
                     by=list(RegData$PasientID), max, na.rm = TRUE)
    SamletPrPID[SamletPrPID==-Inf] <- NA
    AntVar <- c(colSums(SamletPrPID[,-1], na.rm = T), Ukjent=length(Ukjente))
    NVar<-rep(N, length(AntVar))
    grtxt <- c('Annen bekkenkirurgi', 'Annet traume', 'Hemoroidekirurgi', 'Nevrologisk sykdom',
               'Obsterisk skade','Perifer nerveskade', 'Perineal abscess', 'Rectumreseksjon', 'Sfinkterotomi', 'Annet', 'Ukjent')
    tittel <- 'Etiologi'
  }

  if (valgtVar == 'TidlBeh') {
    retn <- 'H'
    RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ]
    N <- length(unique(RegData$PasientID))

    SamletPrPID <- aggregate(RegData[, c("Konservativ", "Irrigasjon", "Tibialisstimulering", "AnalInjection", "SNM", "Sfinkterplastikk",
                                         "Rectopexi", "KirurgiForRectumprolaps", "Gracilisplastikk", "Stomi", "AnnetTidligereBeh")],
                             by=list(RegData$PasientID), max, na.rm = TRUE)
    # RegData[, c("Konservativ", "Irrigasjon", "Tibialisstimulering", "AnalInjection", "SNM", "Sfinkterplastikk",
    #             "Rectopexi", "KirurgiForRectumprolaps", "Gracilisplastikk", "Stomi", "AnnetTidligereBeh", "PasientID")] %>% group_by(PasientID) %>%
    #   summarise_all(max, na.rm=T)
    SamletPrPID[SamletPrPID==-Inf] <- NA
    AntVar <- colSums(SamletPrPID[,-1], na.rm = T)
    NVar<-rep(N, length(AntVar))
    grtxt <- c("Konservativ", "Irrigasjon", "Tibialisstimulering", "Analinjeksjon", "SNM", "Sfinkterplastikk",
               "Rectopexi", "Kirurgi for Rektumprolaps", "Gracilisplastikk", "Stomi", "Annen")
    tittel <- 'Tidligere behandling'
  }

  if (valgtVar == 'TidlBeh_v2') {
    retn <- 'H'
    RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ]
    N <- length(unique(RegData$PasientID))

    SamletPrPID <- aggregate(RegData[, c("Konservativ_v2", "AnalInjection", "SNM", "Sfinkterplastikk",
                                         "Prolapskirurgi", "Stomi", "KunstigLukkMuskel", "AnnetTidligereBeh")],
                             by=list(RegData$PasientID), max, na.rm = TRUE)
    # RegData[, c("Konservativ", "Irrigasjon", "Tibialisstimulering", "AnalInjection", "SNM", "Sfinkterplastikk",
    #             "Rectopexi", "KirurgiForRectumprolaps", "Gracilisplastikk", "Stomi", "AnnetTidligereBeh", "PasientID")] %>% group_by(PasientID) %>%
    #   summarise_all(max, na.rm=T)
    SamletPrPID[SamletPrPID==-Inf] <- NA
    AntVar <- colSums(SamletPrPID[,-1], na.rm = T)
    NVar<-rep(N, length(AntVar))
    grtxt <- c("Konservativ behandling", "Anal injeksjon", "SNM", "Rekonstruksjon av lukkemuskelen",
               "Kirurgi for endetarmsprolaps", "Stomi", "Kunstig lukkemuskel", "Annen")
    tittel <- 'Tidligere behandling'
  }

  if (valgtVar == 'TidlBeh_v3') {
    retn <- 'H'
    RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ]
    N <- length(unique(RegData$PasientID))

    SamletPrPID <- aggregate(RegData[, c("Konservativ_v2", "AnalInjection", "SNM", "Sfinkterplastikk",
                                         "Prolapskirurgi", "Stomi", "KunstigLukkMuskel", "AnnetTidligereBeh")],
                             by=list(RegData$PasientID), max, na.rm = TRUE)
    # RegData[, c("Konservativ", "Irrigasjon", "Tibialisstimulering", "AnalInjection", "SNM", "Sfinkterplastikk",
    #             "Rectopexi", "KirurgiForRectumprolaps", "Gracilisplastikk", "Stomi", "AnnetTidligereBeh", "PasientID")] %>% group_by(PasientID) %>%
    #   summarise_all(max, na.rm=T)
    SamletPrPID[SamletPrPID==-Inf] <- NA
    AntVar <- colSums(SamletPrPID[,-1], na.rm = T)
    # NVar<-rep(N, length(AntVar))
    NVar<-colSums(!is.na(SamletPrPID[,-1]))
    grtxt <- c("Konservativ behandling", "Anal injeksjon", "SNM", "Rekonstruksjon av lukkemuskelen",
               "Kirurgi for endetarmsprolaps", "Stomi", "Kunstig lukkemuskel", "Annen")
    tittel <- 'Tidligere behandling'
  }

  if (valgtVar == 'KomplSfinkter') {
    retn <- 'H'
    RegData <- RegData[RegData$ForlopsType1Num == 1, ]
    N <- dim(RegData)[1]
    RegData$ikkepostopkompl <- !(RegData$PostopKomplikasjoner)
    AntVar <- colSums(RegData[, c("ikkepostopkompl", "PostopKomplikasjoner", "Bloedning", "Saarinfeksjon", "Saardehisens")], na.rm = TRUE)
    NVar<-rep(N, length(AntVar))
    grtxt <- c("Ingen", "Totalt", "Blødning", "Sårinfeksjon", "Sårdehisens")
    tittel <- 'Komplikasjoner ved sfinkterplastikk'
  }



  if (valgtVar == 'Symtomvarighet') {
    retn <- 'H'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = T), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- 1:4
    grtxt <- c('Mindre enn 1 år', 'Mellom 1 år og inntil 5 år', 'Mellom 5 år og inntil 10 år', 'Mer enn 10 år')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
    tittel <- 'Symptomvarighet'
  }

  if (valgtVar == 'Sfinktervurdering') {
    retn <- 'H'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[which(RegData$Ultralyd %in% 0:2), ]
#     RegData <- RegData[order(RegData$HovedDato, decreasing = T), ]
#     RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- c(0:5, 9, 99)
    grtxt <- c('Ingen skade', 'Partiell defekt ytre sfinkter', 'Partiell ytre og fullvegg indre',
               'Fullveggsdefekt ytre sfinkter', 'Fullvegg ytre og indre sfinkter', 'Fullveggsdefekt indre sfinkter',
               'Ukjent resultat', 'Ultralyd ikke utført')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
    tittel <- 'Ultralydvurdering av sfinkterskade'
  }


  #Generere hovedgruppe og sammenlikningsgruppe
  #Trenger indeksene før det genereres tall for figurer med flere variable med ulike utvalg

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


  PlotParams <- list(RegData=RegData, tittel=tittel, grtxt=grtxt, grtxt2=grtxt2, subtxt=subtxt,
                     incl_N=incl_N, incl_pst=incl_pst, retn=retn, cexgr=cexgr, VarTxt=VarTxt, inkl_konf=inkl_konf,
                     AntVar=AntVar, NVar=NVar, antDes=antDes, medSml=medSml, indHoved=indHoved, indRest=indRest, smltxt=smltxt)

  return(invisible(PlotParams))
}
