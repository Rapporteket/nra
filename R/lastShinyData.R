#' Last inn data for NRA sin shiny app
#'
#' Forusetter tilgang til database
#'
#' @return RegData En dataramme med registerets data
#'
#' @export
#
lastShinyData <- function() {

  if (rapbase::isRapContext()) {
    allevar <- nra::nraHentTabell("allevarnum")
    foversikt <- nra::nraHentTabell("forlopsoversikt")
    RegData <- merge(
      allevar,
      foversikt[, c("ForlopsID",
                    names(foversikt)[
                      !(names(foversikt) %in%
                          intersect(names(allevar), names(foversikt)))])],
      by = "ForlopsID")
    Skjemaoversikt <- nra::nraHentTabell("skjemaoversikt")
  }
RegData <- nra::nraPreprosess(RegData=RegData)

tmp <- merge(Skjemaoversikt[Skjemaoversikt$Skjemanavn == '1A Anamnese', ],
             Skjemaoversikt[Skjemaoversikt$Skjemanavn == '1B Symptom', ],
             by = 'ForlopsID', suffixes = c('', '1B'))
tmp <- merge(tmp,
             RegData[, c("ForlopsID", "PasientID", "ForlopsType1",
                         "ForlopsType1Num", "ForlopsType2", "ForlopsType2Num")],
             by='ForlopsID', all.x = T)
tmp <- merge(tmp,
             Skjemaoversikt[Skjemaoversikt$Skjemanavn %in% c('2A SNM-1'), ],
             suffixes = c('','SNM1'), by = 'ForlopsID', all.x = T)
tmp <- merge(tmp,
             Skjemaoversikt[Skjemaoversikt$Skjemanavn %in% c('2A SNM-2'), ],
             suffixes = c('','SNM2'), by = 'ForlopsID', all.x = T)
tmp <- merge(tmp,
             Skjemaoversikt[Skjemaoversikt$Skjemanavn %in% c('2B Sfinkter'), ],
             suffixes = c('','Sfinkter'), by = 'ForlopsID', all.x = T)
tmp2 <- merge(
  Skjemaoversikt[Skjemaoversikt$Skjemanavn %in% c('1B Oppfølging 1 år'),],
  RegData[, c("ForlopsID", "KobletForlopsID")], by='ForlopsID')
tmp <- merge(tmp, tmp2, suffixes = c('','Oppf1'), by.x = 'ForlopsID',
             by.y = "KobletForlopsID", all.x = T)
tmp2 <- merge(
  Skjemaoversikt[Skjemaoversikt$Skjemanavn %in% c('1B Oppfølging 5 år'),],
  RegData[, c("ForlopsID", "KobletForlopsID")], by='ForlopsID')
tmp <- merge(tmp, tmp2, suffixes = c('','Oppf5'),
             by.x = 'ForlopsID', by.y = "KobletForlopsID", all.x = T)

tmp <- tmp[, c("ForlopsType1", "ForlopsType1Num", "ForlopsType2",
               "ForlopsType2Num", "ForlopsID", "PasientID",
               "SkjemaStatus", "HovedDato", "Sykehusnavn", "AvdRESH",
               "SkjemaStatus1B", "SkjemaStatusSNM1",
               "SkjemaStatusSNM2", "SkjemaStatusSfinkter",
               "SkjemaStatusOppf1", "SkjemaStatusOppf5")]

tmp$ForlopsType1[
  is.na(tmp$ForlopsType1)][
    !is.na(tmp$SkjemaStatusSNM1[is.na(tmp$ForlopsType1)])] <- "SNM"
tmp$ForlopsType1Num[
  is.na(tmp$ForlopsType1Num)][!is.na(tmp$SkjemaStatusSNM1[
    is.na(tmp$ForlopsType1)])] <- "SNM"
tmp$ForlopsType1[
  is.na(tmp$ForlopsType1)][!is.na(tmp$SkjemaStatusSfinkter[
    is.na(tmp$ForlopsType1)])] <- "Sfinkterplastikk"

utdata <- list(RegData = RegData,
               Skjemaoversikt = Skjemaoversikt,
               skjema_utflatet = tmp)

}


