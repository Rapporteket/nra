#' Denne funksjonen definerer en del nye (sammensatte) variabler relevante for rapporter i NRA
#' og rensker opp i andre.
#'
#'
#' @inheritParams nraFigAndeler
#'
#' @return Data En list med det filtrerte datasettet og sykehusnavnet som tilsvarer reshID
#'
#' @export

nraPreprosess <- function(RegData)
{
  # RegData <- RegData[RegData$BasisRegStatus==1,]
  RegData$HovedDato <- as.Date(RegData$HovedDato, format="%Y-%m-%d")
  RegData <- dplyr::mutate_at(RegData, c("FIRST_TIME_CLOSEDQ1A", "FIRST_TIME_CLOSEDQ1B",
                          "FIRST_TIME_CLOSEDQ2A", "FIRST_TIME_CLOSEDQ2AT2",
                          "FIRST_TIME_CLOSEDQ2B", "FIRST_TIME_CLOSEDQ3A"), as.Date)
  RegData <- dplyr::mutate(RegData, FIRST_TIME_CLOSED = pmax(FIRST_TIME_CLOSEDQ1A, FIRST_TIME_CLOSEDQ1B,
                                  FIRST_TIME_CLOSEDQ2A, FIRST_TIME_CLOSEDQ2AT2,
                                  FIRST_TIME_CLOSEDQ2B, FIRST_TIME_CLOSEDQ3A, na.rm = T))
  RegData <- dplyr::mutate(RegData, GjennomfortStandardisert_sml =
                             pmax(GjennomfortStandardisert, GjennomfortStandardisertT2, na.rm = T))
  RegData$Aar <- as.numeric(format(RegData$HovedDato, format="%Y"))
  RegData$Mnd <- as.numeric(format(RegData$HovedDato, format="%m"))
  RegData$Kvartal <- floor((RegData$Mnd - 1)/3)+1
  RegData$Halvaar <- floor((RegData$Mnd - 1)/6)+1
  RegData$SenterKortNavn <- trimws(RegData$SenterKortNavn)
  RegData$SenterKortNavn[RegData$SenterKortNavn=="Helse Ber"] <- "Haukeland"
  RegData$AvdRESH[which(RegData$AvdRESH==601233)] <- 601225
  # RegData$SenterKortNavn[RegData$AvdRESH==601233] <- "UNN Narvik" # Foreløpig, må fikses i registeret

  RegData$Sfinktervurdering <- NA
  RegData$Sfinktervurdering[RegData$Ultralyd==1 & RegData$PartiellDefekt==0 &
                              RegData$FullveggsdefektYtreSfinkter==0 &
                              RegData$FullveggsdefektIndreSfinkter==0] <- 0
  RegData$Sfinktervurdering[RegData$Ultralyd==1 & RegData$PartiellDefekt==1 &
                              RegData$FullveggsdefektYtreSfinkter==0 &
                              RegData$FullveggsdefektIndreSfinkter==0] <- 1
  RegData$Sfinktervurdering[RegData$Ultralyd==1 & RegData$PartiellDefekt==1 &
                              RegData$FullveggsdefektYtreSfinkter==0 &
                              RegData$FullveggsdefektIndreSfinkter %in% 1:2] <- 2
  RegData$Sfinktervurdering[RegData$Ultralyd==1 & RegData$PartiellDefekt==0 &
                              RegData$FullveggsdefektYtreSfinkter %in% 1:2 &
                              RegData$FullveggsdefektIndreSfinkter==0] <- 3
  RegData$Sfinktervurdering[RegData$Ultralyd==1 & RegData$PartiellDefekt==0 &
                              RegData$FullveggsdefektYtreSfinkter %in% 1:2 &
                              RegData$FullveggsdefektIndreSfinkter %in% 1:2] <- 4
  RegData$Sfinktervurdering[RegData$Ultralyd==1 & RegData$PartiellDefekt==0 &
                              RegData$FullveggsdefektYtreSfinkter==0 &
                              RegData$FullveggsdefektIndreSfinkter %in% 1:2] <- 5
  RegData$Sfinktervurdering[RegData$Ultralyd==2] <- 9
  RegData$Sfinktervurdering[RegData$Ultralyd==0] <- 99

  # RegData$Onestage <- factor(RegData$Onestage, levels = c(0,1), labels = c("Midl. elektr.", "Permanent"))
  RegData$Testprosedyre <- factor(RegData$Testprosedyre, levels = c(1,2), labels = c("Midl. elektr.", "Permanent"))

  RegData$InkontinensScore <- RegData$StMarksTotalScore
  RegData$InkontinensScore[is.na(RegData$InkontinensScore)] <- RegData$WexnerTotalScore[is.na(RegData$InkontinensScore)]

  # RegData$Indikator1_lekk_red50_v1 <- (RegData$InkontinensFoerTest - RegData$InkontinensUnderTest)/RegData$InkontinensFoerTest*100
  # RegData$Indikator1_lekk_red50_v1[is.nan(RegData$Indikator1_lekk_red50_v1)] <- 0
  # RegData$Indikator1_lekk_red50_v1 <- as.numeric(RegData$Indikator1_lekk_red50_v1 >= 50)
  #

  ##### Fjernes foreløpig  05.11.2021

  RegData$Indikator1_lekk_red50_v2 <- pmax((RegData$InkontinensFoerTest - RegData$InkontinensUnderTest)/RegData$InkontinensFoerTest*100) #,
                                           # (RegData$UrgencyFoerTest - RegData$UrgencyUnderTest)/RegData$UrgencyFoerTest*100, na.rm = T)
  RegData$Indikator1_lekk_red50_v2[is.nan(RegData$Indikator1_lekk_red50_v2)] <- 0
  RegData$red75_v2 <- as.numeric(RegData$Indikator1_lekk_red50_v2 >= 75)
  RegData$Indikator1_lekk_red50_v2 <- as.numeric(RegData$Indikator1_lekk_red50_v2 >= 50)

  RegData$Indikator1_lekk_red50_v3 <- pmax((RegData$UrgencyFoerTestMedLekkasje - RegData$UrgencyUnderTestLekkasje)/RegData$UrgencyFoerTestMedLekkasje*100,
                                           (RegData$UrgencyFoerPassivLekkasje - RegData$UrgencyUnderPassivLekkasje)/RegData$UrgencyFoerPassivLekkasje*100,
                                           (RegData$UrgencyFoerTestUtenLekkasje - RegData$UrgencyUnderUtenTestMedLekkasje)/RegData$UrgencyFoerTestUtenLekkasje*100,
                                           na.rm = T)
  RegData$Indikator1_lekk_red50_v3[is.nan(RegData$Indikator1_lekk_red50_v3)] <- 0
  RegData$red75_v3 <- as.numeric(RegData$Indikator1_lekk_red50_v3 >= 75)
  RegData$Indikator1_lekk_red50_v3 <- as.numeric(RegData$Indikator1_lekk_red50_v3 >= 50)

  RegData$Indikator1_lekk_red50 <- pmax(RegData$Indikator1_lekk_red50_v2, RegData$Indikator1_lekk_red50_v3, na.rm = T)
  RegData$red75 <- pmax(RegData$red75_v2, RegData$red75_v3, na.rm = T)

  RegData$Urinlekkasje_v2 <- RegData$ICIQ_hyppighet
  RegData$Urinlekkasje_v2[RegData$Urinlekkasje_v2 %in% 1:5] <- 1
  RegData$Urinlekkasje_v2[is.na(RegData$Urinlekkasje_v2)] <- RegData$Urinlekkasje[is.na(RegData$Urinlekkasje_v2)] # bruk nytt skjema når mulig
  # RegData$Urinlekkasje_v2 <- pmax(RegData$Urinlekkasje, RegData$Urinlekkasje_v2, na.rm = T)

  RegData$Konservativ_v2 <- pmax(RegData$Konservativ, RegData$Irrigasjon, RegData$Tibialisstimulering, na.rm = T)

  RegData$Prolapskirurgi <- pmax(RegData$KirurgiForRectumprolaps, RegData$KirurgiForRectumprolaps_v2, RegData$Rectopexi, na.rm = T)

  # Lager ny variabel for nyere komplikasjoner

  ### St. Marks og Wexner: Der det finnes St.Marks skal den brukes og Wexner fjernes :

  # fid <-RegData$ForlopsID[!is.na(RegData$StMarksTotalScore) & !is.na(RegData$WexnerTotalScore)& RegData$ForlopsType1Num %in% 1:2]
  # kfid <- RegData$KobletForlopsID[!is.na(RegData$StMarksTotalScore) & !is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num %in% 3:4]
  # aux1 <- RegData[RegData$ForlopsID %in% intersect(fid, kfid), c("ForlopsID", "StMarksTotalScore", "WexnerTotalScore")]
  # aux2 <- RegData[RegData$KobletForlopsID %in% intersect(fid, kfid),
  #                 c("KobletForlopsID", "ForlopsType1Num", "StMarksTotalScore", "WexnerTotalScore")]
  #
  # samlet <- merge(aux1, aux2[aux2$ForlopsType1Num %in% 3, ], by.x = "ForlopsID", by.y = "KobletForlopsID",
  #                 suffixes = c('', '_1aar'), all = T)

  RegData$WexnerTotalScore[!is.na(RegData$StMarksTotalScore) & !is.na(RegData$WexnerTotalScore)  & RegData$ForlopsType1Num %in% 1:2] <- NA



  return(invisible(RegData))
}
