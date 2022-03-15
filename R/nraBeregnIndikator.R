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
                                         111138, 107505, 4210588, 601233),
                                orgnr = c(974795787, 974706490, 974749025,
                                          983971768, 974557746, 974724960,
                                          974116804, 974733013, 974795396),
                                shus = c("UNN", "Akershus", "St.Olav", "Østfold",
                                         "Haukeland", "Innlandet", "DS",
                                         "Kristiansand", "UNN Narvik"))

  terskel <- 5 # minste antall man viser avdelingens resultater for
  maalRetn <- "hoy"
  xmax <- NA
  decreasing <- FALSE

  if (valgtVar == "Indikator1_lekk_red50") {
    nraUtvalg <- nraUtvalg(RegData=RegData, forlopstype1=2)
    RegData <- nraUtvalg$RegData
    indikator <- RegData[!is.na(RegData$Indikator1_lekk_red50),
                         c("Aar", "AvdRESH", "Indikator1_lekk_red50")]
    names(indikator)[names(indikator)=="Indikator1_lekk_red50"] <- "var"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$denominator <- 1
    indikator$ind_id <- 'nra_50pst_lekkasjeredusjon'
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    tittel <- c('Andel med prosentvis reduksjon', 'i lekkasjeepisoder >= 50%')
    maal <- 70
  }

  if (valgtVar == "Ultralyd") {
    indikator <- RegData[which(RegData$Ultralyd %in% 0:2), ]
    indikator <- indikator[indikator$ForlopsType1Num %in% c(1,2) & indikator$ForlopsType2Num %in% c(1,2,5, NA), ]

    indikator <- indikator[ , c("Aar", "AvdRESH", "PasientID", "ForlopsID", "Sfinktervurdering")]
    indikator$var <- indikator$Sfinktervurdering
    indikator$var <- as.numeric(indikator$var != 99)
    indikator$denominator <- 1
    indikator$ind_id <- 'nra_ultralyd'
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- 'Andel med utført ultralyd'
    maal <- 95
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
    indikator$ind_id <- 'nra_tidl_konservativ'
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator <- indikator[, c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('Andel med tidligere', 'konservativ behandling')
    maal <- 90
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
    indikator$ind_id <- 'nra_saarinfeksjon'
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator <- indikator[, c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('Andel bekreftet sårinfeksjon innen', '30 dager etter implantasjon')
    maal <- 4
    maalRetn <- "lav"
    decreasing <- TRUE
    xmax <- 5
  }

  if (valgtVar == "stmarks_9_1aar_snm") {
    RegDataStr9 <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
    RegDataStr9 <- dplyr::bind_rows(RegDataStr9, RegData[RegData$KobletForlopsID %in% RegDataStr9$ForlopsID, ])
    RegDataStr9$var <- NA
    RegDataStr9$var[which(RegDataStr9$StMarksTotalScore<=9)] <- 1
    RegDataStr9$var[which(RegDataStr9$StMarksTotalScore>9)] <- 0
    Oppfolging <- RegDataStr9[RegDataStr9$ForlopsType1Num == 3, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr9$ForlopsID[RegDataStr9$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg
    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, RegDataStr9[RegDataStr9$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_stmarks_9_1aar_snm'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('St. Mark’s Inkontinensskår <=9', '1 år etter operasjon med SNM')
    maal <- 30
  }

  if (valgtVar == "stmarks_9_5aar_snm") {
    RegDataStr9 <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
    RegDataStr9 <- dplyr::bind_rows(RegDataStr9, RegData[RegData$KobletForlopsID %in% RegDataStr9$ForlopsID, ])
    RegDataStr9$var <- NA
    RegDataStr9$var[which(RegDataStr9$StMarksTotalScore<=9)] <- 1
    RegDataStr9$var[which(RegDataStr9$StMarksTotalScore>9)] <- 0
    Oppfolging <- RegDataStr9[RegDataStr9$ForlopsType1Num == 4, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr9$ForlopsID[RegDataStr9$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg
    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, RegDataStr9[RegDataStr9$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_stmarks_9_5aar_snm'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('St. Mark’s Inkontinensskår <=9', '5 år etter operasjon med SNM')
    maal <- 30
  }

  if (valgtVar == "stmarks_12_1aar_snm") {
    RegDataStr12 <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
    RegDataStr12 <- dplyr::bind_rows(RegDataStr12, RegData[RegData$KobletForlopsID %in% RegDataStr12$ForlopsID, ])
    RegDataStr12$var <- NA
    RegDataStr12$var[which(RegDataStr12$StMarksTotalScore<=12)] <- 1
    RegDataStr12$var[which(RegDataStr12$StMarksTotalScore>12)] <- 0
    Oppfolging <- RegDataStr12[RegDataStr12$ForlopsType1Num == 3, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr12$ForlopsID[RegDataStr12$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg
    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, RegDataStr12[RegDataStr12$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_stmarks_12_1aar_snm'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('St. Mark’s Inkontinensskår <=12', '1 år etter operasjon med SNM')
    maal <- 50
  }

  if (valgtVar == "stmarks_12_5aar_snm") {
    RegDataStr12 <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
    RegDataStr12 <- dplyr::bind_rows(RegDataStr12, RegData[RegData$KobletForlopsID %in% RegDataStr12$ForlopsID, ])
    RegDataStr12$var <- NA
    RegDataStr12$var[which(RegDataStr12$StMarksTotalScore<=12)] <- 1
    RegDataStr12$var[which(RegDataStr12$StMarksTotalScore>12)] <- 0
    Oppfolging <- RegDataStr12[RegDataStr12$ForlopsType1Num == 4, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr12$ForlopsID[RegDataStr12$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg
    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, RegDataStr12[RegDataStr12$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_stmarks_12_5aar_snm'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('St. Mark’s Inkontinensskår <=12', '5 år etter operasjon med SNM')
    maal <- 50
  }

  if (valgtVar == "stmarks_9_1aar_sfinkt") {
    RegDataStr9 <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
    RegDataStr9 <- dplyr::bind_rows(RegDataStr9, RegData[RegData$KobletForlopsID %in% RegDataStr9$ForlopsID, ])
    RegDataStr9$var <- NA
    RegDataStr9$var[which(RegDataStr9$StMarksTotalScore<=9)] <- 1
    RegDataStr9$var[which(RegDataStr9$StMarksTotalScore>9)] <- 0
    Oppfolging <- RegDataStr9[RegDataStr9$ForlopsType1Num == 3, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr9$ForlopsID[RegDataStr9$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg
    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, RegDataStr9[RegDataStr9$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_stmarks_9_1aar_sfinkt'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('St. Mark’s Inkontinensskår <=9', '1 år etter sfinkterplastikk')
    maal <- 30
  }

  if (valgtVar == "stmarks_9_5aar_sfinkt") {
    RegDataStr9 <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
    RegDataStr9 <- dplyr::bind_rows(RegDataStr9, RegData[RegData$KobletForlopsID %in% RegDataStr9$ForlopsID, ])
    RegDataStr9$var <- NA
    RegDataStr9$var[which(RegDataStr9$StMarksTotalScore<=9)] <- 1
    RegDataStr9$var[which(RegDataStr9$StMarksTotalScore>9)] <- 0
    Oppfolging <- RegDataStr9[RegDataStr9$ForlopsType1Num == 4, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr9$ForlopsID[RegDataStr9$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg
    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, RegDataStr9[RegDataStr9$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_stmarks_9_5aar_sfinkt'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('St. Mark’s Inkontinensskår <=9', '5 år etter sfinkterplastikk')
    maal <- 30
  }

  if (valgtVar == "stmarks_12_1aar_sfinkt") {
    RegDataStr12 <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
    RegDataStr12 <- dplyr::bind_rows(RegDataStr12, RegData[RegData$KobletForlopsID %in% RegDataStr12$ForlopsID, ])
    RegDataStr12$var <- NA
    RegDataStr12$var[which(RegDataStr12$StMarksTotalScore<=12)] <- 1
    RegDataStr12$var[which(RegDataStr12$StMarksTotalScore>12)] <- 0
    Oppfolging <- RegDataStr12[RegDataStr12$ForlopsType1Num == 3, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr12$ForlopsID[RegDataStr12$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg
    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, RegDataStr12[RegDataStr12$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_stmarks_12_1aar_sfinkt'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('St. Mark’s Inkontinensskår <=12', '1 år etter sfinkterplastikk')
    maal <- 50
  }

  if (valgtVar == "stmarks_12_5aar_sfinkt") {
    RegDataStr12 <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
    RegDataStr12 <- dplyr::bind_rows(RegDataStr12, RegData[RegData$KobletForlopsID %in% RegDataStr12$ForlopsID, ])
    RegDataStr12$var <- NA
    RegDataStr12$var[which(RegDataStr12$StMarksTotalScore<=12)] <- 1
    RegDataStr12$var[which(RegDataStr12$StMarksTotalScore>12)] <- 0
    Oppfolging <- RegDataStr12[RegDataStr12$ForlopsType1Num == 4, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr12$ForlopsID[RegDataStr12$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg
    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, RegDataStr12[RegDataStr12$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_stmarks_12_5aar_sfinkt'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('St. Mark’s Inkontinensskår <=12', '5 år etter sfinkterplastikk')
    maal <- 50
  }

  if (valgtVar == "wexner_9_1aar_snm") {
    RegDataStr9 <- RegData[which(RegData$WexnerTotalScore>9 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
    RegDataStr9 <- dplyr::bind_rows(RegDataStr9, RegData[RegData$KobletForlopsID %in% RegDataStr9$ForlopsID, ])
    RegDataStr9$var <- NA
    RegDataStr9$var[which(RegDataStr9$WexnerTotalScore<=9)] <- 1
    RegDataStr9$var[which(RegDataStr9$WexnerTotalScore>9)] <- 0
    Oppfolging <- RegDataStr9[RegDataStr9$ForlopsType1Num == 3, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr9$ForlopsID[RegDataStr9$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg
    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, RegDataStr9[RegDataStr9$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_wexner_9_1aar_snm'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('Wexnerskår <=9', '1 år etter operasjon med SNM')
    maal <- 30
  }

  if (valgtVar == "wexner_12_1aar_snm") {
    RegDataStr12 <- RegData[which(RegData$WexnerTotalScore>12 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
    RegDataStr12 <- dplyr::bind_rows(RegDataStr12, RegData[RegData$KobletForlopsID %in% RegDataStr12$ForlopsID, ])
    RegDataStr12$var <- NA
    RegDataStr12$var[which(RegDataStr12$WexnerTotalScore<=12)] <- 1
    RegDataStr12$var[which(RegDataStr12$WexnerTotalScore>12)] <- 0
    Oppfolging <- RegDataStr12[RegDataStr12$ForlopsType1Num == 3, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegDataStr12$ForlopsID[RegDataStr12$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg
    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, RegDataStr12[RegDataStr12$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_wexner_12_1aar_snm'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('Wexnerskår <=12', '1 år etter operasjon med SNM')
    maal <- 50
  }

  if (valgtVar == "nra_inkontinensscore_9_1aar_snm") {
    komboStr9 <- RegData[which(RegData$InkontinensScore>9 & RegData$ForlopsType1Num %in% 1:2 &
                                 RegData$ForlopsType2Num %in% c(2, NA)), ]
    komboStr9 <- dplyr::bind_rows(komboStr9, RegData[RegData$KobletForlopsID %in% komboStr9$ForlopsID, ])
    komboStr9$var <- NA
    komboStr9$var[which(komboStr9$InkontinensScore<=9)] <- 1
    komboStr9$var[which(komboStr9$InkontinensScore>9)] <- 0

    Oppfolging <- komboStr9[komboStr9$ForlopsType1Num == 3, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in%
                               komboStr9$ForlopsID[komboStr9$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg

    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, komboStr9[komboStr9$ForlopsType1Num == 2, c("ForlopsID", "Aar")],
                       by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_inkontinensscore_9_1aar_snm'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('Inkontinensscore <=9', '1 år etter operasjon med SNM')
    maal <- 30
  }

  if (valgtVar == "nra_inkontinensscore_9_1aar_sfinkt") {
    komboStr9 <- RegData[which(RegData$InkontinensScore>9 & RegData$ForlopsType1Num %in% 1:2 &
                                 RegData$ForlopsType2Num %in% c(2, NA)), ]
    komboStr9 <- dplyr::bind_rows(komboStr9, RegData[RegData$KobletForlopsID %in% komboStr9$ForlopsID, ])
    komboStr9$var <- NA
    komboStr9$var[which(komboStr9$InkontinensScore<=9)] <- 1
    komboStr9$var[which(komboStr9$InkontinensScore>9)] <- 0

    Oppfolging <- komboStr9[komboStr9$ForlopsType1Num == 3, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in%
                               komboStr9$ForlopsID[komboStr9$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg

    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, komboStr9[komboStr9$ForlopsType1Num == 1, c("ForlopsID", "Aar")],
                       by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_inkontinensscore_9_1aar_sfinkt'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('Inkontinensscore <=9', '1 år etter sfinkterplastikk')
    maal <- 30
  }

  if (valgtVar == "nra_inkontinensscore_12_1aar_snm") {
    komboStr9 <- RegData[which(RegData$InkontinensScore>9 & RegData$ForlopsType1Num %in% 1:2 &
                                 RegData$ForlopsType2Num %in% c(2, NA)), ]
    komboStr9 <- dplyr::bind_rows(komboStr9, RegData[RegData$KobletForlopsID %in% komboStr9$ForlopsID, ])
    komboStr9$var <- NA
    komboStr9$var[which(komboStr9$InkontinensScore<=12)] <- 1
    komboStr9$var[which(komboStr9$InkontinensScore>12)] <- 0

    Oppfolging <- komboStr9[komboStr9$ForlopsType1Num == 3, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in%
                               komboStr9$ForlopsID[komboStr9$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg

    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, komboStr9[komboStr9$ForlopsType1Num == 2, c("ForlopsID", "Aar")],
                       by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_inkontinensscore_12_1aar_snm'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('Inkontinensscore <=12', '1 år etter operasjon med SNM')
    maal <- 50
  }

  if (valgtVar == "nra_inkontinensscore_12_1aar_sfinkt") {
    komboStr9 <- RegData[which(RegData$InkontinensScore>9 & RegData$ForlopsType1Num %in% 1:2 &
                                 RegData$ForlopsType2Num %in% c(2, NA)), ]
    komboStr9 <- dplyr::bind_rows(komboStr9, RegData[RegData$KobletForlopsID %in% komboStr9$ForlopsID, ])
    komboStr9$var <- NA
    komboStr9$var[which(komboStr9$InkontinensScore<=12)] <- 1
    komboStr9$var[which(komboStr9$InkontinensScore>12)] <- 0

    Oppfolging <- komboStr9[komboStr9$ForlopsType1Num == 3, ]
    Oppfolging <- Oppfolging[!is.na(Oppfolging$var), ]
    Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in%
                               komboStr9$ForlopsID[komboStr9$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg

    indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "var")]
    indikator <- merge(indikator, komboStr9[komboStr9$ForlopsType1Num == 1, c("ForlopsID", "Aar")],
                       by.x = 'KobletForlopsID', by.y = 'ForlopsID')
    indikator$denominator <- 1
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$ind_id <- 'nra_inkontinensscore_12_1aar_sfinkt'
    indikator <- indikator[ , c("AvdRESH", "year", "var", "denominator", "ind_id")]
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator <- indikator[, c("orgnr",	"year",	"var",	"denominator",	"ind_id", "AvdRESH", "SenterKortNavn")]
    tittel <- c('Inkontinensscore <=12', '1 år etter sfinkterplastikk')
    maal <- 50
  }

  indikator$context <- "caregiver"

  utputt <- list(indikator=indikator, tittel=tittel, maal=maal, terskel=terskel,
                 maalRetn=maalRetn, xmax=xmax, decreasing=decreasing)

  return(utputt)

}

#
# names(Indikatorer) <- c("orgnr",	"year",	"var",	"denominator",	"ind_id")
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind1"] <- "nra_50pst_lekkasjeredusjon"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind2"] <- "nra_ultralyd"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind3"] <- "nra_saarinfeksjon"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind4"] <- "nra_stmarks_9_1aar_snm"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind5"] <- "nra_stmarks_9_5aar_snm"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind6"] <- "nra_stmarks_12_1aar_snm"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind7"] <- "nra_stmarks_12_5aar_snm"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind8"] <- "nra_stmarks_9_1aar_sfinkt"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind9"] <- "nra_stmarks_9_5aar_sfinkt"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind10"] <- "nra_stmarks_12_1aar_sfinkt"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind11"] <- "nra_stmarks_12_5aar_sfinkt"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind12"] <- "nra_wexner_9_1aar_snm"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind13"] <- "nra_wexner_12_1aar_snm"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind14"] <- "nra_inkontinensscore_9_1aar_snm"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind15"] <- "nra_inkontinensscore_9_1aar_sfinkt"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind16"] <- "nra_inkontinensscore_12_1aar_snm"
# Indikatorer$ind_id[Indikatorer$ind_id == "Ind17"] <- "nra_inkontinensscore_12_1aar_sfinkt"



