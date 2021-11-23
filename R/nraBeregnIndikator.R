#' Denne funksjonen beregner NRAs indikatorer fordelt over år
#'
#' Filtreringer skjer i funksjonen
#'
#' @inheritParams nraFigAndeler
#'
#' @return En dataramme med valgt indikator
#' @export

nraBeregnIndikator <- function(RegData, valgtVar) {

  kobl_resh_orgnr <- data.frame(resh = c(601225, 108162, 107440, 700116, 700922, 111138, 107505, 4210588),
                                orgnr = c(974795787, 974706490, 974749025, 983971768, 974557746, 974724960, 974116804, 974733013),
                                shus = c("UNN", "Akershus", "St.Olav", "Østfold", "Haukeland", "Innlandet", "DS", "Kristiansand"))


  if (valgtVar == "Indikator1_lekk_red50") {
    nraUtvalg <- nraUtvalg(RegData=RegData, forlopstype1=2)
    RegData <- nraUtvalg$RegData
    indikator <- RegData[!is.na(RegData$Indikator1_lekk_red50), c("Aar", "AvdRESH", "Indikator1_lekk_red50")]
    names(indikator)[names(indikator)=="Indikator1_lekk_red50"] <- "var"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$denominator <- 1
    indikator$ind_id <- 'nra_50pst_lekkasjeredusjon'
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    tittel <- c('Andel med prosentvis reduksjon', 'i lekkasjeepisoder >= 50%')
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
  }



indikator$context <- "caregiver"

return(indikator)

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



