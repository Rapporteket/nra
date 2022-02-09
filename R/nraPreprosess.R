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
  RegData$Aar <- as.numeric(format(RegData$HovedDato, format="%Y"))
  RegData$Mnd <- as.numeric(format(RegData$HovedDato, format="%m"))
  RegData$SenterKortNavn <- trimws(RegData$SenterKortNavn)
  RegData$SenterKortNavn[RegData$SenterKortNavn=="Helse Ber"] <- "Haukeland"
  RegData$SenterKortNavn[RegData$AvdRESH==601233] <- "UNN Narvik" # Foreløpig, må fikses i registeret

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

  RegData$Indikator1_lekk_red50_v3 <- pmax(# (RegData$UrgencyFoerTestUtenLekkasje - RegData$UrgencyUnderUtenTestMedLekkasje)/RegData$UrgencyFoerTestUtenLekkasje*100,
                                           (RegData$UrgencyFoerTestMedLekkasje - RegData$UrgencyUnderTestLekkasje)/RegData$UrgencyFoerTestMedLekkasje*100,
                                           (RegData$UrgencyFoerPassivLekkasje - RegData$UrgencyUnderPassivLekkasje)/RegData$UrgencyFoerPassivLekkasje*100,
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


  return(invisible(RegData))
}
