#' Denne funksjonen beregner NRAs indikatorer fordelt over år
#'
#' Filtreringer skjer i funksjonen
#'
#' @inheritParams nraFigAndeler
#'
#' @return En dataramme med
#' @export

nraBeregnIndikator <- function(RegData, valgtVar) {

  kobl_resh_orgnr <- data.frame(resh = c(601225, 108162, 107440, 700116, 700922, 111138, 107505, 4210588),
                                orgnr = c(974795787, 974706490, 974749025, 983971768, 974557746, 974724960, 974116804, 974733013),
                                shus = c("UNN", "Akershus", "St.Olav", "Østfold", "Haukeland", "Innlandet", "DS", "Kristiansand"))
  nraUtvalg <- nraUtvalg(RegData=RegData, forlopstype1=2)
  RegData <- nraUtvalg$RegData

  if (valgtVar == "Indikator1_lekk_red50") {
    indikator <- RegData[!is.na(RegData$Indikator1_lekk_red50), c("Aar", "AvdRESH", "Indikator1_lekk_red50")]
    names(indikator)[names(indikator)=="Indikator1_lekk_red50"] <- "var"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$denominator <- 1
    indikator$ind_id <- 'nra_50pst_lekkasjeredusjon'
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]


    plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind1')]
    names(plotdata) <- c('ReshId', 'Aar', 'Teller')
    plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
  }





}


shus <- qmongrdata::SykehusNavnStruktur
# RegDataAlle[match(unique(Indikatorer$AvdRESH), RegDataAlle$AvdRESH), c("SenterKortNavn")]

kobl_resh_orgnr <- data.frame(resh = c(601225, 108162, 107440, 700116, 700922, 111138, 107505, 4210588),
                              orgnr = c(974795787, 974706490, 974749025, 983971768, 974557746, 974724960, 974116804, 974733013),
                              shus = c("UNN", "Akershus", "St.Olav", "Østfold", "Haukeland", "Innlandet", "DS", "Kristiansand"))

kobl_resh_orgnr$orgnr_navn <- shus$SykehusNavn[match(kobl_resh_orgnr$orgnr, shus$OrgNrShus)]
kobl_resh_orgnr$orgnr_navn[is.na(kobl_resh_orgnr$orgnr_navn)] <-
  shus$Hfkortnavn[match(kobl_resh_orgnr$orgnr[is.na(kobl_resh_orgnr$orgnr_navn)], shus$OrgNrHF)]

Indikatorer$orgnr <- kobl_resh_orgnr$orgnr[match(Indikatorer$AvdRESH, kobl_resh_orgnr$resh)]

Indikatorer <- Indikatorer[, c("orgnr", "Aar", "ind", "nevner", "Index")]
names(Indikatorer) <- c("orgnr",	"year",	"var",	"denominator",	"ind_id")
Indikatorer$ind_id[Indikatorer$ind_id == "Ind1"] <- "nra_50pst_lekkasjeredusjon"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind2"] <- "nra_ultralyd"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind3"] <- "nra_saarinfeksjon"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind4"] <- "nra_stmarks_9_1aar_snm"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind5"] <- "nra_stmarks_9_5aar_snm"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind6"] <- "nra_stmarks_12_1aar_snm"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind7"] <- "nra_stmarks_12_5aar_snm"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind8"] <- "nra_stmarks_9_1aar_sfinkt"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind9"] <- "nra_stmarks_9_5aar_sfinkt"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind10"] <- "nra_stmarks_12_1aar_sfinkt"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind11"] <- "nra_stmarks_12_5aar_sfinkt"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind12"] <- "nra_wexner_9_1aar_snm"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind13"] <- "nra_wexner_12_1aar_snm"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind14"] <- "nra_inkontinensscore_9_1aar_snm"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind15"] <- "nra_inkontinensscore_9_1aar_sfinkt"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind16"] <- "nra_inkontinensscore_12_1aar_snm"
Indikatorer$ind_id[Indikatorer$ind_id == "Ind17"] <- "nra_inkontinensscore_12_1aar_sfinkt"

Indikatorer$context <- "caregiver"







