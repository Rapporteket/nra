#' Denne funksjonen beregner NRAs indikatorer fordelt over år
#'
#' Filtreringer skjer i funksjonen
#'
#' @inheritParams nraFigAndeler
#'
#' @return En dataramme med valgt indikator
#' @export

nraBeregnIndikator <- function(RegData, valgtVar) {

  kobl_resh_orgnr <- data.frame(resh = c(601225, 108162, 107440, 700116, 700922,
                                         111138, 107505, 4210588, 601233,
                                         114271),
                                orgnr = c(974795787, 974706490, 974749025,
                                          983971768, 974557746, 974724960,
                                          974116804, 974733013, 974795396,
                                          974703300),
                                shus = c("UNN", "Akershus", "St.Olav", "Østfold",
                                         "Haukeland", "Innlandet", "DS",
                                         "Kristiansand", "UNN Narvik",
                                         "Stavanger"))

  terskel <- 5 # minste antall man viser avdelingens resultater for
  maalRetn <- "hoy"
  xmax <- NA
  decreasing <- FALSE

  if (valgtVar == "andel_inform_oppf") {
    nraUtvalg <- nraUtvalg(RegData=RegData, forlopstype1=c(1,2))
    indikator <- nraUtvalg$RegData
    indikator <- indikator[indikator$ForlopsType2Num != 4 | is.na(indikator$ForlopsType2Num), ]
    indikator <- indikator[!is.na(indikator$InformertOppf), ]
    indikator$var <- indikator$InformertOppf
    indikator$denominator <- 1
    indikator$ind_id <- "nra_inform_oppf"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c("Andel som har fått informasjon", "om ett års oppfølging")
    maal <- 90
    minstekrav <-80
  }

  if (valgtVar == "andel_inform_oppf_sfinkt") {
    nraUtvalg <- nraUtvalg(RegData=RegData, forlopstype1=c(1))
    indikator <- nraUtvalg$RegData
    indikator <- indikator[indikator$ForlopsType2Num != 4 | is.na(indikator$ForlopsType2Num), ]
    indikator <- indikator[!is.na(indikator$InformertOppf), ]
    indikator$var <- indikator$InformertOppf
    indikator$denominator <- 1
    indikator$ind_id <- "nra_inform_oppf_sfinkt"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c("Andel sfinkterplastikkopererte som har fått", "informasjon om ett års oppfølging")
    maal <- 90
    minstekrav <-80
  }

  if (valgtVar == "andel_inform_oppf_snm") {
    nraUtvalg <- nraUtvalg(RegData=RegData, forlopstype1=c(2))
    indikator <- nraUtvalg$RegData
    indikator <- indikator[indikator$ForlopsType2Num != 4 | is.na(indikator$ForlopsType2Num), ]
    indikator <- indikator[!is.na(indikator$InformertOppf), ]
    indikator$var <- indikator$InformertOppf
    indikator$denominator <- 1
    indikator$ind_id <- "nra_inform_oppf_snm"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c("Andel SNM-pasienter som har fått", "informasjon om ett års oppfølging")
    maal <- 90
    minstekrav <-80
  }

  if (valgtVar == "Indikator_standardisert") {
    nraUtvalg <- nraUtvalg(RegData=RegData, forlopstype2 = c(1,2,3,5))
    indikator <- nraUtvalg$RegData
    indikator$var <- indikator$GjennomfortStandardisert_sml
    indikator <- indikator[!is.na(indikator$var), ]
    indikator <- indikator[!(indikator$RevisjonsProsedyre %in% c(1,4)), ]
    indikator$denominator <- 1
    indikator$ind_id <- "nra_standardisert"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c("Andel operert etter", "standardisert metode")
    maal <- 99
    minstekrav <-90
  }

  if (valgtVar == "Indikator_aktualitet") {
    nraUtvalg <- nraUtvalg(RegData=RegData, forlopstype1=c(1,2))
    RegData <- nraUtvalg$RegData
    RegData$tid_op_reg <- difftime(RegData$FIRST_TIME_CLOSED, RegData$HovedDato, units = "days")/30.437
    indikator <- RegData[!is.na(RegData$tid_op_reg), ]
    indikator$var <- 0
    indikator$var[indikator$tid_op_reg <=4] <- 1
    indikator$denominator <- 1
    indikator$ind_id <- "nra_aktualitet"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c("Andel skjema levert", "innen 4mnd postoperativt")
    maal <- 80
    minstekrav <-60
  }

  if (valgtVar == "Indikator_aktualitet_snm") {
    nraUtvalg <- nraUtvalg(RegData=RegData, forlopstype1=c(2))
    RegData <- nraUtvalg$RegData
    RegData$tid_op_reg <- difftime(RegData$FIRST_TIME_CLOSED, RegData$HovedDato, units = "days")/30.437
    indikator <- RegData[!is.na(RegData$tid_op_reg), ]
    indikator$var <- 0
    indikator$var[indikator$tid_op_reg <=4] <- 1
    indikator$denominator <- 1
    indikator$ind_id <- "nra_aktualitet_snm"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c("Andel skjema levert innen", "4mnd postoperativt etter SNM")
    maal <- 80
    minstekrav <-60
  }

  if (valgtVar == "Indikator_aktualitet_sfinkt") {
    nraUtvalg <- nraUtvalg(RegData=RegData, forlopstype1=c(1))
    RegData <- nraUtvalg$RegData
    RegData$tid_op_reg <- difftime(RegData$FIRST_TIME_CLOSED, RegData$HovedDato, units = "days")/30.437
    indikator <- RegData[!is.na(RegData$tid_op_reg), ]
    indikator$var <- 0
    indikator$var[indikator$tid_op_reg <=4] <- 1
    indikator$denominator <- 1
    indikator$ind_id <- "nra_aktualitet_sfinkt"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c("Andel skjema levert innen", "4mnd postoperativt etter sfinkterplastikk")
    maal <- 80
    minstekrav <-60
  }

  if (valgtVar == "Indikator1_lekk_red50") {
    nraUtvalg <- nraUtvalg(RegData=RegData, forlopstype1=2)
    RegData <- nraUtvalg$RegData
    indikator <- RegData[!is.na(RegData$Indikator1_lekk_red50),
                         c("Aar", "AvdRESH", "Indikator1_lekk_red50")]
    names(indikator)[names(indikator)=="Indikator1_lekk_red50"] <- "var"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$denominator <- 1
    indikator$ind_id <- "nra_50pst_lekkasjeredusjon"
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    tittel <- c("Andel med prosentvis reduksjon", "i lekkasjeepisoder \u2265 50%")
    maal <- 70
    minstekrav <- 50
  }

  if (valgtVar == "Ultralyd") {
    indikator <- RegData[which(RegData$Ultralyd %in% 0:2), ]
    indikator <- indikator[indikator$ForlopsType1Num %in% c(1,2) & indikator$ForlopsType2Num %in% c(1,2,5, NA), ]
    indikator <- indikator[ , c("Aar", "AvdRESH", "PasientID", "ForlopsID", "Sfinktervurdering")]
    indikator$var <- indikator$Sfinktervurdering
    indikator$var <- as.numeric(indikator$var != 99)
    indikator$denominator <- 1
    indikator$ind_id <- "nra_ultralyd"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- "Andel med utført ultralyd"
    maal <- 95
    minstekrav <-80
  }

  if (valgtVar == "Ultralyd_snm") {
    indikator <- RegData[which(RegData$Ultralyd %in% 0:2), ]
    indikator <- indikator[indikator$ForlopsType1Num %in% c(2) & indikator$ForlopsType2Num %in% c(1,2,5, NA), ]
    indikator <- indikator[ , c("Aar", "AvdRESH", "PasientID", "ForlopsID", "Sfinktervurdering")]
    indikator$var <- indikator$Sfinktervurdering
    indikator$var <- as.numeric(indikator$var != 99)
    indikator$denominator <- 1
    indikator$ind_id <- "nra_ultralyd_snm"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- "Andel med utført ultralyd - SNM"
    maal <- 95
    minstekrav <-80
  }

  if (valgtVar == "Ultralyd_sfinkt") {
    indikator <- RegData[which(RegData$Ultralyd %in% 0:2), ]
    indikator <- indikator[indikator$ForlopsType1Num %in% c(1) & indikator$ForlopsType2Num %in% c(1,2,5, NA), ]
    indikator <- indikator[ , c("Aar", "AvdRESH", "PasientID", "ForlopsID", "Sfinktervurdering")]
    indikator$var <- indikator$Sfinktervurdering
    indikator$var <- as.numeric(indikator$var != 99)
    indikator$denominator <- 1
    indikator$ind_id <- "nra_ultralyd_sfinkt"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- "Andel med utført ultralyd - Sfinkterplastikk"
    maal <- 95
    minstekrav <-80
  }

  if (valgtVar == "tidl_konservativ") {
    indikator <- RegData[RegData$ForlopsType1Num %in% c(1,2) & RegData$ForlopsType2Num %in% c(1,2,5, NA), ]
    indikator <- indikator[ , c("Aar", "AvdRESH", "PasientID", "ForlopsID", "Konservativ_v2")]
    indikator$var <- indikator$Konservativ_v2
    indikator <- indikator[!is.na(indikator$var), ]
    indikator <- indikator %>% group_by(PasientID, Aar, AvdRESH) %>%
      summarise(var = max(var),
                ForlopsID = ForlopsID[var==max(var)][1])
    indikator$denominator <- 1
    indikator$ind_id <- "nra_tidl_konservativ"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator <- indikator[, c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c("Andel med tidligere", "konservativ behandling")
    maal <- 90
    minstekrav <-80
  }

  if (valgtVar == "tidl_konservativ_snm") {
    indikator <- RegData[RegData$ForlopsType1Num %in% c(2) & RegData$ForlopsType2Num %in% c(1,2,5, NA), ]
    indikator <- indikator[ , c("Aar", "AvdRESH", "PasientID", "ForlopsID", "Konservativ_v2")]
    indikator$var <- indikator$Konservativ_v2
    indikator <- indikator[!is.na(indikator$var), ]
    indikator <- indikator %>% group_by(PasientID, Aar, AvdRESH) %>%
      summarise(var = max(var),
                ForlopsID = ForlopsID[var==max(var)][1])
    indikator$denominator <- 1
    indikator$ind_id <- "nra_tidl_konservativ_snm"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator <- indikator[, c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c("Andel SNM-pasienter med tidligere", "konservativ behandling")
    maal <- 90
    minstekrav <-80
  }

  if (valgtVar == "tidl_konservativ_sfinkt") {
    indikator <- RegData[RegData$ForlopsType1Num %in% c(1), ]
    indikator <- indikator[ , c("Aar", "AvdRESH", "PasientID", "ForlopsID", "Konservativ_v2")]
    indikator$var <- indikator$Konservativ_v2
    indikator <- indikator[!is.na(indikator$var), ]
    indikator <- indikator %>% group_by(PasientID, Aar, AvdRESH) %>%
      summarise(var = max(var),
                ForlopsID = ForlopsID[var==max(var)][1])
    indikator$denominator <- 1
    indikator$ind_id <- "nra_tidl_konservativ_sfinkt"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator <- indikator[, c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c("Andel sfinkterplastikkpasienter med", "tidligere konservativ behandling")
    maal <- 90
    minstekrav <-80
  }

  if (valgtVar == "saarinfeksjon") {
    RegData$Variabel <- pmax(RegData$Komplikasjon, RegData$KomplikasjonT2, na.rm = T)
    RegData <- RegData[RegData$ForlopsType1Num == 2, ] # Kun SNM
    RegData <- RegData[RegData$ForlopsType2Num %in% c(1,2,5,3), ] # Kun test positiv, test usikker, test negativ
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData$Variabel[which(RegData$Variabel==9 & (RegData$Komplikasjon==2 | RegData$KomplikasjonT2==2))] <- 2   # Velg bekreftet eller mistenkt
    RegData$Variabel[which(RegData$Variabel==9 & (RegData$Komplikasjon==1 | RegData$KomplikasjonT2==1))] <- 1
    RegData$Variabel[which(RegData$Variabel==98 & (RegData$Komplikasjon==2 | RegData$KomplikasjonT2==2))] <- 2   # Velg bekreftet eller mistenkt
    RegData$Variabel[which(RegData$Variabel==98 & (RegData$Komplikasjon==1 | RegData$KomplikasjonT2==1))] <- 1
    indikator <- RegData[ , c("Aar", "AvdRESH", "PasientID", "ForlopsID", "Variabel")]
    indikator$var <- as.numeric(indikator$Variabel == 2)
    indikator$denominator <- 1
    indikator$ind_id <- "nra_saarinfeksjon"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator <- indikator[, c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c("Andel bekreftet sårinfeksjon innen", "30 dager etter implantasjon")
    maal <- 4
    minstekrav <-6
    maalRetn <- "lav"
    decreasing <- TRUE
    xmax <- 5
  }


  if (valgtVar == "stmarks_9_1aar_snm") {
    predata <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num == 2 & RegData$ForlopsType2Num %in% c(2, 3)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_9_1aar_snm"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 9", "1 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "stmarks_9_5aar_snm") {
    predata <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num == 2 & RegData$ForlopsType2Num %in% c(2, 3)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_9_5aar_snm"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 9", "5 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "stmarks_12_1aar_snm") {
    predata <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num == 2 & RegData$ForlopsType2Num %in% c(2, 3)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_12_1aar_snm"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 12", "1 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "stmarks_12_5aar_snm") {
    predata <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num == 2 & RegData$ForlopsType2Num %in% c(2, 3)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_12_5aar_snm"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 12", "5 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }


  if (valgtVar == "stmarks_9_1aar_sfinkt") {
    predata <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_9_1aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 9", "1 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "stmarks_9_5aar_sfinkt") {
    predata <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_9_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 9", "5 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "stmarks_12_1aar_sfinkt") {
    predata <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_12_1aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 12", "1 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "stmarks_12_5aar_sfinkt") {
    predata <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_12_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 12", "5 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }



  if (valgtVar == "wexner_9_1aar_snm") {
    predata <- RegData[which(RegData$WexnerTotalScore>9 & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_9_1aar_snm"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Wexnerskår \u2264 9", "1 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "wexner_9_5aar_snm") {
    predata <- RegData[which(RegData$WexnerTotalScore>9 & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_9_5aar_snm"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Wexnerskår \u2264 9", "5 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "wexner_12_1aar_snm") {
    predata <- RegData[which(RegData$WexnerTotalScore>12 & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_12_1aar_snm"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Wexnerskår \u2264 12", "1 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "wexner_12_5aar_snm") {
    predata <- RegData[which(RegData$WexnerTotalScore>12 & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_12_5aar_snm"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Wexnerskår \u2264 12", "5 år etter operasjon med SNM")
    maal <- 50
    minstekrav <-30
  }



  if (valgtVar == "wexner_9_1aar_sfinkt") {
    predata <- RegData[which(RegData$WexnerTotalScore>9 & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_9_1aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Wexnerskår \u2264 9", "1 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "wexner_9_5aar_sfinkt") {
    predata <- RegData[which(RegData$WexnerTotalScore>9 & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_9_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Wexnerskår \u2264 9", "5 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "wexner_12_1aar_sfinkt") {
    predata <- RegData[which(RegData$WexnerTotalScore>12 & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_12_1aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Wexnerskår \u2264 12", "1 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "wexner_12_5aar_sfinkt") {
    predata <- RegData[which(RegData$WexnerTotalScore>12 & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_12_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Wexnerskår \u2264 12", "5 år etter sfinkterplastikk")
    maal <- 50
    minstekrav <-30
  }


  if (valgtVar == "nra_inkontinensscore_9_1aar_snm") {
    predata <- RegData[which(RegData$InkontinensScore>9 & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=9)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_9_1aar_snm"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Inkontinensscore \u2264 9", "1 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "nra_inkontinensscore_9_1aar_sfinkt") {
    predata <- RegData[which(RegData$InkontinensScore>9 & RegData$ForlopsType1Num == 1), ]

    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=9)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_9_1aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Inkontinensscore \u2264 9", "1 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "nra_inkontinensscore_12_1aar_snm") {
    predata <- RegData[which(RegData$InkontinensScore>12 & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=12)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_1aar_snm"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Inkontinensscore \u2264 12", "1 år etter operasjon med SNM")
    maal <- 50
    minstekrav <-30
  }

  if (valgtVar == "nra_inkontinensscore_12_1aar_sfinkt") {
    predata <- RegData[which(RegData$InkontinensScore>12 & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=12)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_1aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Inkontinensscore \u2264 12", "1 år etter sfinkterplastikk")
    maal <- 50
    minstekrav <-30
  }

  if (valgtVar == "nra_inkontinensscore_9_5aar_snm") {
    predata <- RegData[which(RegData$InkontinensScore>9 & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=9)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_9_5aar_snm"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Inkontinensscore \u2264 9", "5 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "nra_inkontinensscore_9_5aar_sfinkt") {
    predata <- RegData[which(RegData$InkontinensScore>9 & RegData$ForlopsType1Num == 1), ]

    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=9)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_9_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Inkontinensscore \u2264 9", "5 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "nra_inkontinensscore_12_5aar_snm") {
    predata <- RegData[which(RegData$InkontinensScore>12 & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=12)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_5aar_snm"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Inkontinensscore \u2264 12", "5 år etter operasjon med SNM")
    maal <- 50
    minstekrav <-30
  }

  if (valgtVar == "nra_inkontinensscore_12_5aar_sfinkt") {
    predata <- RegData[which(RegData$InkontinensScore>12 & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=12)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Inkontinensscore \u2264 12", "5 år etter sfinkterplastikk")
    maal <- 50
    minstekrav <-30
  }

  if (valgtVar == "nra_reduksjon_4_stmarks_1aar_sfinkt") {
    predata <- RegData[which(RegData$ForlopsType1Num == 1 & !is.na(RegData$StMarksTotalScore)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator <- indikator %>% mutate(var = ifelse(StMarksTotalScore - StMarksTotalScore_post > 4, 1, 0))
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_red4_1aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Minst 4 reduksjon i St. Marks", "1 år etter sfinkterplastikk")
    maal <- NA
    minstekrav <-NA
  }

  if (valgtVar == "nra_reduksjon_4_stmarks_5aar_sfinkt") {
    predata <- RegData[which(RegData$ForlopsType1Num == 1 & !is.na(RegData$StMarksTotalScore)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator <- indikator %>% mutate(var = ifelse(StMarksTotalScore - StMarksTotalScore_post > 4, 1, 0))
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Minst 4 reduksjon i St. Marks", "5 år etter sfinkterplastikk")
    maal <- NA
    minstekrav <-NA
  }

  if (valgtVar == "nra_reduksjon_4_stmarks_1aar_snm") {
    predata <- RegData[which(RegData$ForlopsType1Num == 2 & !is.na(RegData$StMarksTotalScore) &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator <- indikator %>% mutate(var = ifelse(StMarksTotalScore - StMarksTotalScore_post > 4, 1, 0))
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Minst 4 reduksjon i St. Marks", "1 år etter operasjon med SNM")
    maal <- NA
    minstekrav <-NA
  }

  if (valgtVar == "nra_reduksjon_4_stmarks_5aar_snm") {
    predata <- RegData[which(RegData$ForlopsType1Num == 2 & !is.na(RegData$StMarksTotalScore) &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator <- indikator %>% mutate(var = ifelse(StMarksTotalScore - StMarksTotalScore_post > 4, 1, 0))
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Minst 4 reduksjon i St. Marks", "5 år etter operasjon med SNM")
    maal <- NA
    minstekrav <-NA
  }

  if (valgtVar == "nra_reduksjon_40pst_stmarks_1aar_sfinkt") {
    predata <- RegData[which(RegData$ForlopsType1Num == 1 & !is.na(RegData$StMarksTotalScore)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator <- indikator %>% mutate(var = ifelse((StMarksTotalScore - StMarksTotalScore_post)/StMarksTotalScore > .4, 1, 0))
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Minst 40% reduksjon i St. Marks", "1 år etter sfinkterplastikk")
    maal <- NA
    minstekrav <-NA
  }

  if (valgtVar == "nra_reduksjon_40pst_stmarks_5aar_sfinkt") {
    predata <- RegData[which(RegData$ForlopsType1Num == 1 & !is.na(RegData$StMarksTotalScore)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator <- indikator %>% mutate(var = ifelse((StMarksTotalScore - StMarksTotalScore_post)/StMarksTotalScore > .4, 1, 0))
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Minst 40% reduksjon i St. Marks", "5 år etter sfinkterplastikk")
    maal <- NA
    minstekrav <-NA
  }

  if (valgtVar == "nra_reduksjon_40pst_stmarks_1aar_snm") {
    predata <- RegData[which(RegData$ForlopsType1Num == 2 & !is.na(RegData$StMarksTotalScore) &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator <- indikator %>% mutate(var = ifelse((StMarksTotalScore - StMarksTotalScore_post)/StMarksTotalScore > .4, 1, 0))
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Minst 40% reduksjon i St. Marks", "1 år etter operasjon med SNM")
    maal <- NA
    minstekrav <-NA
  }

  if (valgtVar == "nra_reduksjon_40pst_stmarks_5aar_snm") {
    predata <- RegData[which(RegData$ForlopsType1Num == 2 & !is.na(RegData$StMarksTotalScore) &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator <- indikator %>% mutate(var = ifelse((StMarksTotalScore - StMarksTotalScore_post)/StMarksTotalScore > .4, 1, 0))
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_5aar_sfinkt"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Minst 40% reduksjon i St. Marks", "5 år etter operasjon med SNM")
    maal <- NA
    minstekrav <-NA
  }


  ##############################################################################
  ################ Foreløpig, ad-hoc til årsrapport 2022 #######################
  ##############################################################################

  if (valgtVar == "stmarks_9_1aar_snm_v2") {
    predata <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 2 & RegData$ForlopsType2Num %in% c(2, 3)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_9_1aar_snm_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 9", "1 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "stmarks_9_5aar_snm_v2") {
    predata <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 2 & RegData$ForlopsType2Num %in% c(2, 3)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_9_5aar_snm_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 9", "5 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "stmarks_12_1aar_snm_v2") {
    predata <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 2 & RegData$ForlopsType2Num %in% c(2, 3)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_12_1aar_snm_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 12", "1 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "stmarks_12_5aar_snm_v2") {
    predata <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 2 & RegData$ForlopsType2Num %in% c(2, 3)), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_12_5aar_snm_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 12", "5 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }


  if (valgtVar == "stmarks_9_1aar_sfinkt_v2") {
    predata <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_9_1aar_sfinkt_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 9", "1 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "stmarks_9_5aar_sfinkt_v2") {
    predata <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_9_5aar_sfinkt_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 9", "5 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "stmarks_12_1aar_sfinkt_v2") {
    predata <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_12_1aar_sfinkt_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 12", "1 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "stmarks_12_5aar_sfinkt_v2") {
    predata <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$StMarksTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "StMarksTotalScore")],
                       oppfolging[, c("KobletForlopsID", "StMarksTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$StMarksTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$StMarksTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_stmarks_12_5aar_sfinkt_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("St. Mark's Inkontinensskår \u2264 12", "5 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }



  if (valgtVar == "wexner_9_1aar_snm_v2") {
    predata <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_9_1aar_snm_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Wexnerskår \u2264 9", "1 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "wexner_9_5aar_snm_v2") {
    predata <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_9_5aar_snm_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Wexnerskår \u2264 9", "5 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "wexner_12_1aar_snm_v2") {
    predata <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_12_1aar_snm_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Wexnerskår \u2264 12", "1 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "wexner_12_5aar_snm_v2") {
    predata <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_12_5aar_snm_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Wexnerskår \u2264 12", "5 år etter operasjon med SNM")
    maal <- 50
    minstekrav <-30
  }



  if (valgtVar == "wexner_9_1aar_sfinkt_v2") {
    predata <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_9_1aar_sfinkt_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Wexnerskår \u2264 9", "1 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "wexner_9_5aar_sfinkt_v2") {
    predata <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=9)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_9_5aar_sfinkt_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Wexnerskår \u2264 9", "5 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "wexner_12_1aar_sfinkt_v2") {
    predata <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_12_1aar_sfinkt_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Wexnerskår \u2264 12", "1 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "wexner_12_5aar_sfinkt_v2") {
    predata <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$WexnerTotalScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "WexnerTotalScore")],
                       oppfolging[, c("KobletForlopsID", "WexnerTotalScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$WexnerTotalScore_post<=12)] <- 1
    indikator$var[which(indikator$WexnerTotalScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_wexner_12_5aar_sfinkt_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Wexnerskår \u2264 12", "5 år etter sfinkterplastikk")
    maal <- 50
    minstekrav <-30
  }


  if (valgtVar == "nra_inkontinensscore_9_1aar_snm_v2") {
    predata <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=9)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_9_1aar_snm_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Inkontinensscore \u2264 9", "1 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "nra_inkontinensscore_9_1aar_sfinkt_v2") {
    predata <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 1), ]

    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=9)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_9_1aar_sfinkt_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Inkontinensscore \u2264 9", "1 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "nra_inkontinensscore_12_1aar_snm_v2") {
    predata <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=12)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_1aar_snm_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Inkontinensscore \u2264 12", "1 år etter operasjon med SNM")
    maal <- 50
    minstekrav <-30
  }

  if (valgtVar == "nra_inkontinensscore_12_1aar_sfinkt_v2") {
    predata <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 3), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=12)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_1aar_sfinkt_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 1
    tittel <- c("Inkontinensscore \u2264 12", "1 år etter sfinkterplastikk")
    maal <- 50
    minstekrav <-30
  }

  if (valgtVar == "nra_inkontinensscore_9_5aar_snm_v2") {
    predata <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=9)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_9_5aar_snm_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Inkontinensscore \u2264 9", "5 år etter operasjon med SNM")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "nra_inkontinensscore_9_5aar_sfinkt_v2") {
    predata <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 1), ]

    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=9)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>9)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_9_5aar_sfinkt_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Inkontinensscore \u2264 9", "5 år etter sfinkterplastikk")
    maal <- 30
    minstekrav <-20
  }

  if (valgtVar == "nra_inkontinensscore_12_5aar_snm_v2") {
    predata <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 2 &
                               RegData$ForlopsType2Num %in% c(2,3)), ]
    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=12)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_5aar_snm_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Inkontinensscore \u2264 12", "5 år etter operasjon med SNM")
    maal <- 50
    minstekrav <-30
  }

  if (valgtVar == "nra_inkontinensscore_12_5aar_sfinkt_v2") {
    predata <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 1), ]
    oppfolging <- RegData[which(!is.na(RegData$InkontinensScore) & RegData$ForlopsType1Num == 4), ]
    indikator <- merge(predata[, c("Aar", "AvdRESH", "PasientID", "ForlopsID", "InkontinensScore")],
                       oppfolging[, c("KobletForlopsID", "InkontinensScore")], by.x = "ForlopsID",
                       by.y = "KobletForlopsID", suffixes = c("", "_post"))
    indikator$var <- NA
    indikator$var[which(indikator$InkontinensScore_post<=12)] <- 1
    indikator$var[which(indikator$InkontinensScore_post>12)] <- 0
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- "nra_inkontinensscore_12_5aar_sfinkt_v2"
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    indikator$year <- indikator$year + 5
    tittel <- c("Inkontinensscore \u2264 12", "5 år etter sfinkterplastikk")
    maal <- 50
    minstekrav <-30
  }




  ##############################################################################
  ##############################################################################



  indikator$context <- "caregiver"

  utputt <- list(indikator=indikator, tittel=tittel, maal=maal, terskel=terskel,
                 maalRetn=maalRetn, xmax=xmax, decreasing=decreasing, minstekrav = minstekrav)

  return(utputt)

}


# if (valgtVar == "stmarks_9_1aar_snm") {
#   RegDataStr9 <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, 3, NA)), ]
#   RegDataStr9 <- dplyr::bind_rows(RegDataStr9, RegData[RegData$KobletForlopsID %in% RegDataStr9$ForlopsID, ])
#   RegDataStr9$var <- NA
#   RegDataStr9$var[which(RegDataStr9$StMarksTotalScore<=9)] <- 1
#   RegDataStr9$var[which(RegDataStr9$StMarksTotalScore>9)] <- 0
#   Oppfolging <- RegDataStr9[RegDataStr9$ForlopsType1Num == 3, ]
#   Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
#   Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr9$ForlopsID[RegDataStr9$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg
#   indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
#   indikator <- merge(indikator, RegDataStr9[RegDataStr9$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = "KobletForlopsID", by.y = "ForlopsID")
#   indikator$denominator <- 1
#   names(indikator)[names(indikator)=="Aar"] <- "year"
#   indikator$ind_id <- "nra_stmarks_9_1aar_snm"
#   indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
#   indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
#   indikator$year <- indikator$year + 1
#   tittel <- c("St. Mark's Inkontinensskår \u2264 9", "1 år etter operasjon med SNM")#c("St. Mark's Inkontinensskår \u2264 9", "1 år etter operasjon med SNM")
#   maal <- 30
#   minstekrav <-20
# }
# if (valgtVar == "stmarks_9_5aar_snm") {
#   RegDataStr9 <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, 3, NA)), ]
#   RegDataStr9 <- dplyr::bind_rows(RegDataStr9, RegData[RegData$KobletForlopsID %in% RegDataStr9$ForlopsID, ])
#   RegDataStr9$var <- NA
#   RegDataStr9$var[which(RegDataStr9$StMarksTotalScore<=9)] <- 1
#   RegDataStr9$var[which(RegDataStr9$StMarksTotalScore>9)] <- 0
#   Oppfolging <- RegDataStr9[RegDataStr9$ForlopsType1Num == 4, ]
#   Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
#   Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr9$ForlopsID[RegDataStr9$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg
#   indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
#   indikator <- merge(indikator, RegDataStr9[RegDataStr9$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = "KobletForlopsID", by.y = "ForlopsID")
#   indikator$denominator <- 1
#   names(indikator)[names(indikator)=="Aar"] <- "year"
#   indikator$ind_id <- "nra_stmarks_9_5aar_snm"
#   indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
#   indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
#   indikator$year <- indikator$year + 5
#   tittel <- c("St. Mark's Inkontinensskår \u2264 9", "5 år etter operasjon med SNM")
#   maal <- 30
#   minstekrav <-20
# }
# if (valgtVar == "stmarks_12_1aar_snm") {
#   RegDataStr12 <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, 3, NA)), ]
#   RegDataStr12 <- dplyr::bind_rows(RegDataStr12, RegData[RegData$KobletForlopsID %in% RegDataStr12$ForlopsID, ])
#   RegDataStr12$var <- NA
#   RegDataStr12$var[which(RegDataStr12$StMarksTotalScore<=12)] <- 1
#   RegDataStr12$var[which(RegDataStr12$StMarksTotalScore>12)] <- 0
#   Oppfolging <- RegDataStr12[RegDataStr12$ForlopsType1Num == 3, ]
#   Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
#   Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr12$ForlopsID[RegDataStr12$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg
#   indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
#   indikator <- merge(indikator, RegDataStr12[RegDataStr12$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = "KobletForlopsID", by.y = "ForlopsID")
#   indikator$denominator <- 1
#   names(indikator)[names(indikator)=="Aar"] <- "year"
#   indikator$ind_id <- "nra_stmarks_12_1aar_snm"
#   indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
#   indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator$year <- indikator$year + 1
#   indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
#   tittel <- c("St. Mark's Inkontinensskår \u2264 12", "1 år etter operasjon med SNM")
#   maal <- 50
#   minstekrav <-30
# }
# if (valgtVar == "stmarks_12_5aar_snm") {
#   RegDataStr12 <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, 3, NA)), ]
#   # RegDataStr12 <- RegData[which(RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, 3, NA)), ]
#   RegDataStr12 <- dplyr::bind_rows(RegDataStr12, RegData[RegData$KobletForlopsID %in% RegDataStr12$ForlopsID, ])
#   RegDataStr12$var <- NA
#   RegDataStr12$var[which(RegDataStr12$StMarksTotalScore<=12)] <- 1
#   RegDataStr12$var[which(RegDataStr12$StMarksTotalScore>12)] <- 0
#   Oppfolging <- RegDataStr12[RegDataStr12$ForlopsType1Num == 4, ]
#   Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
#   Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr12$ForlopsID[RegDataStr12$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg
#   indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
#   indikator <- merge(indikator, RegDataStr12[RegDataStr12$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = "KobletForlopsID", by.y = "ForlopsID")
#   indikator$denominator <- 1
#   names(indikator)[names(indikator)=="Aar"] <- "year"
#   indikator$ind_id <- "nra_stmarks_12_5aar_snm"
#   indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
#   indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
#   indikator$year <- indikator$year + 5
#   tittel <- c("St. Mark's Inkontinensskår \u2264 12", "5 år etter operasjon med SNM")
#   maal <- 50
#   minstekrav <-30
# }
# if (valgtVar == "stmarks_9_1aar_sfinkt") {
#   RegDataStr9 <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, 3, NA)), ]
#   RegDataStr9 <- dplyr::bind_rows(RegDataStr9, RegData[RegData$KobletForlopsID %in% RegDataStr9$ForlopsID, ])
#   RegDataStr9$var <- NA
#   RegDataStr9$var[which(RegDataStr9$StMarksTotalScore<=9)] <- 1
#   RegDataStr9$var[which(RegDataStr9$StMarksTotalScore>9)] <- 0
#   Oppfolging <- RegDataStr9[RegDataStr9$ForlopsType1Num == 3, ]
#   Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
#   Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr9$ForlopsID[RegDataStr9$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg
#   indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
#   indikator <- merge(indikator, RegDataStr9[RegDataStr9$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = "KobletForlopsID", by.y = "ForlopsID")
#   indikator$denominator <- 1
#   names(indikator)[names(indikator)=="Aar"] <- "year"
#   indikator$ind_id <- "nra_stmarks_9_1aar_sfinkt"
#   indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
#   indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
#   indikator$year <- indikator$year + 1
#   tittel <- c("St. Mark's Inkontinensskår \u2264 9", "1 år etter sfinkterplastikk")
#   maal <- 30
#   minstekrav <-20
# }
#
# if (valgtVar == "stmarks_9_5aar_sfinkt") {
#   RegDataStr9 <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, 3, NA)), ]
#   RegDataStr9 <- dplyr::bind_rows(RegDataStr9, RegData[RegData$KobletForlopsID %in% RegDataStr9$ForlopsID, ])
#   RegDataStr9$var <- NA
#   RegDataStr9$var[which(RegDataStr9$StMarksTotalScore<=9)] <- 1
#   RegDataStr9$var[which(RegDataStr9$StMarksTotalScore>9)] <- 0
#   Oppfolging <- RegDataStr9[RegDataStr9$ForlopsType1Num == 4, ]
#   Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
#   Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr9$ForlopsID[RegDataStr9$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg
#   indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
#   indikator <- merge(indikator, RegDataStr9[RegDataStr9$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = "KobletForlopsID", by.y = "ForlopsID")
#   indikator$denominator <- 1
#   names(indikator)[names(indikator)=="Aar"] <- "year"
#   indikator$ind_id <- "nra_stmarks_9_5aar_sfinkt"
#   indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
#   indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
#   indikator$year <- indikator$year + 5
#   tittel <- c("St. Mark's Inkontinensskår \u2264 9", "5 år etter sfinkterplastikk")
#   maal <- 30
#   minstekrav <-20
# }
#
# if (valgtVar == "stmarks_12_1aar_sfinkt") {
#   RegDataStr12 <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, 3, NA)), ]
#   RegDataStr12 <- dplyr::bind_rows(RegDataStr12, RegData[RegData$KobletForlopsID %in% RegDataStr12$ForlopsID, ])
#   RegDataStr12$var <- NA
#   RegDataStr12$var[which(RegDataStr12$StMarksTotalScore<=12)] <- 1
#   RegDataStr12$var[which(RegDataStr12$StMarksTotalScore>12)] <- 0
#   Oppfolging <- RegDataStr12[RegDataStr12$ForlopsType1Num == 3, ]
#   Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
#   Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr12$ForlopsID[RegDataStr12$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg
#   indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
#   indikator <- merge(indikator, RegDataStr12[RegDataStr12$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = "KobletForlopsID", by.y = "ForlopsID")
#   indikator$denominator <- 1
#   names(indikator)[names(indikator)=="Aar"] <- "year"
#   indikator$ind_id <- "nra_stmarks_12_1aar_sfinkt"
#   indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
#   indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
#   indikator$year <- indikator$year + 1
#   tittel <- c("St. Mark's Inkontinensskår \u2264 12", "1 år etter sfinkterplastikk")
#   maal <- 50
#   minstekrav <-30
# }
#
# if (valgtVar == "stmarks_12_5aar_sfinkt") {
#   RegDataStr12 <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, 3, NA)), ]
#   RegDataStr12 <- dplyr::bind_rows(RegDataStr12, RegData[RegData$KobletForlopsID %in% RegDataStr12$ForlopsID, ])
#   RegDataStr12$var <- NA
#   RegDataStr12$var[which(RegDataStr12$StMarksTotalScore<=12)] <- 1
#   RegDataStr12$var[which(RegDataStr12$StMarksTotalScore>12)] <- 0
#   Oppfolging <- RegDataStr12[RegDataStr12$ForlopsType1Num == 4, ]
#   Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
#   Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr12$ForlopsID[RegDataStr12$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg
#   indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
#   indikator <- merge(indikator, RegDataStr12[RegDataStr12$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = "KobletForlopsID", by.y = "ForlopsID")
#   indikator$denominator <- 1
#   names(indikator)[names(indikator)=="Aar"] <- "year"
#   indikator$ind_id <- "nra_stmarks_12_5aar_sfinkt"
#   indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
#   indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
#   indikator$year <- indikator$year + 5
#   tittel <- c("St. Mark's Inkontinensskår \u2264 12", "5 år etter sfinkterplastikk")
#   maal <- 50
#   minstekrav <-30
# }

# if (valgtVar == "wexner_12_5aar_sfinkt") {
#   RegDataStr12 <- RegData[which(RegData$WexnerTotalScore>12 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, 3, NA)), ]
#   RegDataStr12 <- dplyr::bind_rows(RegDataStr12, RegData[RegData$KobletForlopsID %in% RegDataStr12$ForlopsID, ])
#   RegDataStr12$var <- NA
#   RegDataStr12$var[which(RegDataStr12$WexnerTotalScore<=12)] <- 1
#   RegDataStr12$var[which(RegDataStr12$WexnerTotalScore>12)] <- 0
#   Oppfolging <- RegDataStr12[RegDataStr12$ForlopsType1Num == 4, ]
#   Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
#   Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr12$ForlopsID[RegDataStr12$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg
#   indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
#   indikator <- merge(indikator, RegDataStr12[RegDataStr12$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = "KobletForlopsID", by.y = "ForlopsID")
#   indikator$denominator <- 1
#   names(indikator)[names(indikator)=="Aar"] <- "year"
#   indikator$ind_id <- "nra_wexner_12_5aar_sfinkt"
#   indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
#   indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
#   indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
#   indikator$year <- indikator$year + 5
#   tittel <- c("Wexnerskår \u2264 12", "5 år etter sfinkterplastikk")
#   maal <- 50
#   minstekrav <-30
# }
