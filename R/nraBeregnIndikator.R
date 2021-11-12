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
  nraUtvalg <- nraUtvalg(RegData=RegData, forlopstype1=2)
  RegData <- nraUtvalg$RegData

  if (valgtVar == "Indikator1_lekk_red50") {
    indikator <- RegData[!is.na(RegData$Indikator1_lekk_red50), c("Aar", "AvdRESH", "Indikator1_lekk_red50")]
    names(indikator)[names(indikator)=="Indikator1_lekk_red50"] <- "var"
    names(indikator)[names(indikator)=="Aar"] <- "year"
    indikator$denominator <- 1
    indikator$ind_id <- 'nra_50pst_lekkasjeredusjon'
    indikator$orgnr <- kobl_resh_orgnr$orgnr[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
    indikator$SenterKortNavn <- kobl_resh_orgnr$shus[match(indikator$AvdRESH, kobl_resh_orgnr$resh)]
  }



indikator$context <- "caregiver"

return(indikator)

}








