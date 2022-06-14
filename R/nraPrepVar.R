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


  if (valgtVar == "tidligereKonservativ") {
    # RegData <- RegData[RegData$ForlopsType1Num %in% c(1,2) & RegData$ForlopsType2Num %in% c(1,2,5, NA), ]
    RegData$Variabel <- RegData$Konservativ_v2
    RegData <- RegData[!is.na(RegData$Variabel), ]
    grtxt <- c('Nei', 'Ja')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=0:1, labels = grtxt)
    retn <- 'V'
    tittel <- c("Andel forløp med tidligere", "konservativ behandling")
    VarTxt <- "forløp med tidligere konservativ behandling"
  }

  if (valgtVar == "Ultralyd_utfort") {
    RegData$Variabel <- RegData$Ultralyd
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData$Variabel[RegData$Variabel %in% 1:2] <- 1
    grtxt <- c('Nei', 'Ja')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=0:1, labels = grtxt)
    retn <- 'V'
    tittel <- c("Utført ultralyd")
    VarTxt <- "forløp med utført ultralyd"
  }

  if (valgtVar == "Indikator1_lekk_red50") {
    RegData$Variabel <- RegData[ , valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    grtxt <- c('Nei', 'Ja')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=0:1, labels = grtxt)
    retn <- 'V'
    tittel <- c("Prosentvis reduksjon", "i lekkasjeepisoder >= 50%")
    VarTxt <- "forløp med prosentvis reduksjon i lekkasjeepisoder >= 50%"
  }

  if (valgtVar == 'EQ5DAngst') {
    retn <- 'H'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- c('EQ5D - Angst og depresjon')
    gr <- rev(c(9, 1:5))
    grtxt <- rev(c("Ikke utfylt",
                   "Jeg er hverken engstelig eller deprimert",
                   "Jeg er litt engstelig eller deprimert",
                   "Jeg er middels engstelig eller deprimert",
                   "Jeg er svært engstelig eller deprimert",
                   "Jeg er ekstremt engstelig eller deprimert"))
    grtxt <- nra::wrap.it(grtxt, 27)
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

  if (valgtVar == 'EQ5DPersonligStell') {
    retn <- 'H'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- c('EQ5D - Personlig stell')
    gr <- rev(c(9, 1:5))
    grtxt <- rev(c("Ikke utfylt",
                   "Jeg har ingen problemer med personlig stell",
                   "Jeg har litt problemer med å vaske meg eller kle meg",
                   "Jeg har middels store problemer med å vaske meg eller kle meg",
                   "Jeg har store problemer med å vaske meg eller kle meg",
                   "Jeg er ute av stand til å vaske meg eller kle meg"))
    grtxt <- nra::wrap.it(grtxt, 27)
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

  if (valgtVar == 'EQ5DSmerte') {
    retn <- 'H'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- c('EQ5D - Smerte og ubehag')
    gr <- rev(c(9, 1:5))
    grtxt <- rev(c("Ikke utfylt",
                   "Jeg har hverken smerte eller ubehag",
                   "Jeg har litt smerte eller ubehag",
                   "Jeg har middels sterke smerte eller ubehag",
                   "Jeg har sterk smerte eller ubehag",
                   "Jeg har svært sterke smerte eller ubehag"))
    grtxt <- nra::wrap.it(grtxt, 27)
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

  if (valgtVar == 'EQ5DGange') {
    retn <- 'H'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- c('EQ5D - Gange')
    gr <- rev(c(9, 1:5))
    grtxt <- rev(c("Ikke utfylt",
                   "Jeg har ingen problemer med å gå omkring",
                   "Jeg har litt problemer med å gå omkring",
                   "Jeg har middels store problemer med å gå omkring",
                   "Jeg har store problemer med å gå omkring",
                   "Jeg er ute av stand til å gå omkring"))
    grtxt <- nra::wrap.it(grtxt, 27)
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

  if (valgtVar == 'EQ5DVanligeGjoeremaal') {
    retn <- 'H'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- c('EQ5D - Vanlige gjøremål')
    gr <- rev(c(9, 1:5))
    grtxt <- rev(c("Ikke utfylt",
                   "Jeg har ingen problemer med å utføre mine vanlige gjøremål",
                   "Jeg har litt problemer med å utføre mine vanlige gjøremål",
                   "Jeg har middels store problemer med å utføre mine vanlige gjøremål",
                   "Jeg har store problemer med å utføre mine vanlige gjøremål",
                   "Jeg er ute av stand til å utføre mine vanlige gjøremål"))
    grtxt <- nra::wrap.it(grtxt, 27)
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

  if (valgtVar == 'PGICEndring') {
    retn <- 'H'
    RegData$Variabel <- RegData$PGICEndringPost1
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- c('Endring i aktivitetsbegrensninger, symptomer,', 'følelser og generell livskvalitet ')
    gr <- rev(c(99, 0:6))
    grtxt <- rev(c("Ukjent", "Ingen endring (eller tilstanden \n har blitt verre)",
                   "Har det omtrent som før, nesten ingen \n endring i tilstand i det hele tatt",
                   "Noe bedring, men ingen merkbar \n endring har skjedd",
                   "Litt bedring, men denne endringen har \n ikke utgjort noen større forskjell",
                   "Moderat bedring og en liten, \n men merkbar forskjell",
                   "Bedre. Det har skjedd en definitiv endring \n som utgjør en verdifull forskjell",
                   "Mye bedre. Det har skjedd en betydelig endring \n til det bedre som utgjør all verdens forskjell"))
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

  if (valgtVar == 'PGICEndringLekkasje') {
    retn <- 'H'
    RegData$Variabel <- RegData$PGICEndringLekkasjePost1
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- 'Endring i lekkasjeplager'
    gr <- rev(c(99, 0:10))
    grtxt <- rev(c("99 = Ukjent",
                   "0 = Mye verre",
                   "1",
                   "2",
                   "3",
                   "4",
                   "5 = Ingen endring",
                   "6",
                   "7",
                   "8",
                   "9",
                   "10 = Mye bedre"))
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }


  if (valgtVar == 'BegrensSeksLiv') {
    retn <- 'V'
    RegData$Variabel <- RegData$BegrensSeksLiv
    RegData <- RegData[!is.na(RegData$Variabel), ]
    N <- sum(RegData$Variabel == 5, na.rm = T)
    tittel <- c("Begrenser du ditt seksualliv på grunn av mulige uhell/lekkasjer ",
                paste0("i forhold til avføring/lukt, ikke aktuelt for ", N, " pasienter"))
    gr <- 0:4
    grtxt <- c("Aldri", "Sjelden", "Av og til", "Vanligvis", "Alltid")
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

  if (valgtVar == 'ICIQ_hyppighet') {
    retn <- 'H'
    RegData$Variabel <- RegData$ICIQ_hyppighet
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- c("Hvor ofte lekker du urin")
    gr <- c(0:5, 9)
    grtxt <- c("Aldri", "Omtrent èn gang i uken \n eller sjeldnere", "2 - 3 ganger i uken",
               "ca. 1 gang per dag", "Flere ganger per dag", "Hele tiden", "Ukjent")
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

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
    RegData <- RegData[RegData$HovedDato <= "2020-11-15", ]
    RegData <- RegData[RegData$ForlopsType1Num == 2, ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[RegData$Onestage != 1, ]  # Fjerner Onestage
    tittel <- 'Komplikasjoner SNM test innen 30 dager'
    gr <- c(0,1,2,9)
    grtxt <- c('Ingen', 'Sårinfeksjon mistenkt', 'Sårinfeksjon bekreftet','Annet')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

  if (valgtVar == 'KomplikasjonT2') {
    retn <- 'H'
    RegData$Variabel <- RegData$KomplikasjonT2
    RegData <- RegData[RegData$HovedDato <= "2020-11-15", ]
    RegData <- RegData[RegData$ForlopsType1Num == 2, ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- 'Komplikasjoner SNM implantasjon innen 30 dager'
    gr <- c(0,1,2,9)
    grtxt <- c('Ingen', 'Sårinfeksjon mistenkt', 'Sårinfeksjon bekreftet','Annet')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

  if (valgtVar == 'KomplSNMtot') {
    retn <- 'H'
    RegData$Variabel <- pmax(RegData$Komplikasjon, RegData$KomplikasjonT2, na.rm = T)
    RegData <- RegData[RegData$HovedDato <= "2020-11-15", ]
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

  if (valgtVar == 'Komplikasjon_ny') {
    retn <- 'H'
    RegData$Variabel <- RegData$Komplikasjon
    RegData <- RegData[RegData$HovedDato > "2020-11-15", ]
    RegData <- RegData[RegData$ForlopsType1Num == 2, ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[RegData$Onestage != 1, ]  # Fjerner Onestage
    tittel <- 'Komplikasjoner SNM test innen 30 dager'
    gr <- c(0,2,3,98)
    grtxt <- c('Ingen', 'Sårinfeksjon bekreftet', 'Hematom som krever intervensjon', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

  if (valgtVar == 'KomplikasjonT2_ny') {
    retn <- 'H'
    RegData$Variabel <- RegData$KomplikasjonT2
    RegData <- RegData[RegData$HovedDato > "2020-11-15", ]
    RegData <- RegData[RegData$ForlopsType1Num == 2, ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- 'Komplikasjoner SNM implantasjon innen 30 dager'
    gr <- c(0,2,3,98)
    grtxt <- c('Ingen', 'Sårinfeksjon bekreftet', 'Hematom som krever intervensjon', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$Variabel, levels=gr, labels = grtxt)
  }

  if (valgtVar == 'KomplSNMtot_ny') {
    retn <- 'H'
    RegData$Variabel <- pmax(RegData$Komplikasjon, RegData$KomplikasjonT2, na.rm = T)
    RegData <- RegData[RegData$HovedDato <= "2020-11-15", ]
    RegData <- RegData[RegData$ForlopsType1Num == 2, ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData$Variabel[which(RegData$Variabel==98 & (RegData$Komplikasjon==2 | RegData$KomplikasjonT2==2))] <- 2   # Velg bekreftet eller mistenkt
    RegData$Variabel[which(RegData$Variabel==98 & (RegData$Komplikasjon==3 | RegData$KomplikasjonT2==3))] <- 3
    tittel <- 'Komplikasjoner SNM innen 30 dager'
    gr <- c(0,2,3,98)
    grtxt <- c('Ingen', 'Sårinfeksjon bekreftet', 'Hematom som krever intervensjon', 'Ukjent')
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

  if (valgtVar == 'Etiologi_v2') { ############ SKAL OGSÅ GJØRES PÅ INDIVIDNIVÅ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!11
    retn <- 'H'
    # Ukjente <- unique(RegData$PasientID[which(RegData$ForlopsType1Num %in% 1:2 & RegData$Ukjent==1)]) # Bør man sjekke om PasientID også finnes
    RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ]                        # i RegData?
    N <- dim(RegData)[1]
    RegData$TidlKirurgEndetarm_samlet <- RegData$PerinealAbscess | RegData$Hemoroidereksjon |
      RegData$Rectumreseksjon | RegData$Sfinkterotomi
    RegData$TidlKirurgEndetarm_samlet[!is.na(RegData$TidlKirurgEndetarm)] <-
      RegData$TidlKirurgEndetarm[!is.na(RegData$TidlKirurgEndetarm)]
    # RegData$etio_bool <- RegData$ObsteriskSkade | RegData$AnnetTraume | RegData$TidlKirurgEndetarm_samlet |
    #   RegData$AnnenBekkenKirurgi | RegData$NevrologiskSykdom | RegData$PeriferNervskade | RegData$AnnetEtiologi |
    #   RegData$KraftBehandling
    AntVar <- colSums(RegData[, c("ObsteriskSkade", "AnnetTraume","TidlKirurgEndetarm_samlet",
                                  "AnnenBekkenKirurgi", "NevrologiskSykdom",  "KraftBehandling",
                                  "AnnetEtiologi", "Ukjent")], na.rm = TRUE)
    # AntVar <- c(colSums(RegData[which(RegData$Ukjent == 0),
    #                             c("ObsteriskSkade", "AnnetTraume","TidlKirurgEndetarm_samlet",
    #                               "AnnenBekkenKirurgi", "NevrologiskSykdom",  "KraftBehandling",
    #                               "AnnetEtiologi")], na.rm = TRUE)
    # NVar<-c(colSums(!is.na(RegData[which(RegData$Ukjent == 0),
    #                              c("ObsteriskSkade", "AnnetTraume","TidlKirurgEndetarm_samlet",
    #                                "AnnenBekkenKirurgi", "NevrologiskSykdom",  "KraftBehandling",
    #                                "AnnetEtiologi")])), Ukjent = sum(RegData$Ukjent %in% 0:1))

    NVar<-rep(N, length(AntVar))
    grtxt <- c('Obsterisk skade', 'Annet traume', 'Tidligere kirurgi', 'Annen bekkenkirurgi', 'Nevrologisk sykdom',
               'Kreftbehandling', 'Annet', 'Ukjent')
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
    SamletPrPID[SamletPrPID==-Inf] <- NA
    AntVar <- colSums(SamletPrPID[,-1], na.rm = T)
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

  if (valgtVar == 'lekker_urin_naar') {
    retn <- 'H'
    # RegData <- RegData[RegData$ICIQ_hyppighet != 0 & !is.na(RegData$ICIQ_hyppighet), ]
    RegData <- RegData[RegData$ICIQ_hyppighet %in% 1:5, ]
    N <- dim(RegData)[1]
    AntVar <- colSums(RegData[, c("LekkerUrinAldri", "LekkerUrinFoerToalett", "LekkerUrinHoster",
                                  "LekkerUrinSover", "LekkerUrinFysiskAktiv", "LekkerUrinEtterToalett",
                                  "LekkerUrinIngenGrunn", "LekkerUrinHeleTiden", "LekkerUrinUkjent")], na.rm = T)
    NVar<-rep(N, length(AntVar))
    grtxt <- c("Aldri, jeg lekker ikke urin", "Lekker før jeg når toalettet",
               "Lekker når jeg hoster eller nyser", "Lekker når jeg sover",
               "Lekker når jeg er fysisk \n aktiv/trimmer",
               "Lekker når jeg er ferdig med å late \n vannet og har tatt på meg klærne",
               "Lekker uten noen opplagt grunn", "Lekker hele tiden", "Ukjent")
    tittel <- 'Når lekker du urin'
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
