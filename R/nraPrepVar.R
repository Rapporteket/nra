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
nraPrepVar <- function(RegData, valgtVar)
{
  stabel=FALSE; incl_N=FALSE; incl_pst=FALSE; retn= 'V'; tittel <- ''; inkl_konf=0; antDes=1;
  cexgr <- 1.0; grtxt <- ''; grtxt2 <- ''; subtxt <- ''; VarTxt <- ''; AntVar=NA; NVar=NA;


  RegData$Variabel <- NA
  if (valgtVar == 'PasientAlder') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = T), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    tittel <- 'Aldersfordeling'
    gr <- c(0, seq(25, 85, 10), 130)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    subtxt <- 'Aldersgrupper'
    grtxt <- levels(RegData$VariabelGr)
    grtxt[1] <- paste0('<', as.character(gr[2]))
    grtxt[length(grtxt)] <- paste0('>', as.character(gr[length(gr)-1]))
  }

  if (valgtVar == 'Etiologi') {
    retn <- 'H'
    Ukjente <- unique(RegData$PasientID[which(RegData$ForlopsType1Num %in% 1:2 & RegData$Ukjent==1)])
    RegData <- RegData[RegData$ForlopsType1Num %in% 1:2 & RegData$Ukjent==0, ]
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
    # SamletPrPID[SamletPrPID==-Inf] <- NA
    AntVar <- colSums(SamletPrPID[,-1], na.rm = T)
    NVar<-rep(N, length(AntVar))
    grtxt <- c("Konservativ", "Irrigasjon", "Tibialisstimulering", "Analinjeksjon", "SNM", "Sfinkterplastikk",
               "Rectopexi", "Kirurgi for Rektumprolaps", "Gracilisplastikk", "Stomi", "Annen")
    tittel <- 'Tidligere behandling'
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
    RegData <- RegData[RegData$Ultralyd %in% 1:2, ]
#     RegData <- RegData[order(RegData$HovedDato, decreasing = T), ]
#     RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- c(0:5, 9)
    grtxt <- c('Ingen skade', 'Partiell defekt ytre sfinkter', 'Partiell ytre og fullvegg indre',
               'Fullveggsdefekt ytre sfinkter', 'Fullvegg ytre og indre sfinkter', 'Fullveggsdefekt indre sfinkter',
               'Ukjent resultat')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
    tittel <- 'Vurdering av sfinkterskade'
  }




  PlotParams <- list(RegData=RegData, tittel=tittel, grtxt=grtxt, grtxt2=grtxt2, subtxt=subtxt,
                     incl_N=incl_N, incl_pst=incl_pst, retn=retn, cexgr=cexgr, VarTxt=VarTxt, inkl_konf=inkl_konf,
                     AntVar=AntVar, NVar=NVar, antDes=antDes)

  return(invisible(PlotParams))
}
