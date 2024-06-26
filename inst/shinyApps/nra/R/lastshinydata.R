# Last inn data i shiny app
#
# Skal fungere lokalt og på shiny-server
#
#
# @inheritParams nraFigAndeler
#
# @return RegData En dataramme med registerets data
#
# @export
#

lastshinydata <- function() {

  if (rapbase::isRapContext()) {
    # RegData <- nra::nraHentRegData()
    allevar <- nra::nraHentTabell("alleVarNum")
    foversikt <- nra::nraHentTabell("ForlopsOversikt")
    RegData <- merge(allevar, foversikt[, c("ForlopsID", names(foversikt)[!(names(foversikt) %in% intersect(names(allevar), names(foversikt)))])],
                     by = "ForlopsID")
    Skjemaoversikt <- nra::nraHentTabell("SkjemaOversikt")
  } else {
    RegData <- read.table('I:/nra/alleVarNum2021-11-09 13-16-38.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
    RegData <- RegData[, c('ForlopsID', 'Ukjent', 'AnnenBekkenKirurgi', 'AnnetTraume', 'Hemoroidereksjon', 'NevrologiskSykdom', 'ObsteriskSkade',
                           'PeriferNervskade', 'PerinealAbscess', 'Rectumreseksjon', 'Sfinkterotomi', 'AnnetEtiologi', 'Konservativ',
                           'Irrigasjon', 'Tibialisstimulering', 'AnalInjection', 'SNM', 'Sfinkterplastikk', 'Rectopexi',
                           'KirurgiForRectumprolaps', 'Gracilisplastikk', 'Stomi', 'AnnetTidligereBeh', "SenterKortNavn", "Symtomvarighet",
                           "Ultralyd", "PartiellDefekt", "FullveggsdefektYtreSfinkter", "FullveggsdefektIndreSfinkter", "GenQol",
                           "StMarksTotalScore", "QolSexualitet", "KobletForlopsID", "Tilfredshet", "Urinlekkasje", "Komplikasjon",
                           "KomplikasjonT2", "PostopKomplikasjoner", "Bloedning", "Saarinfeksjon", "Saardehisens", "InkontinensFoerTest",
                           "AvfoeringerFoerTest", "LekkasjedagerFoer", "InkontinensUnderTest", "UrgencyFoerTest", "UrgencyUnderTest",
                           "UrgencyFoerTestUtenLekkasje", "UrgencyFoerTestMedLekkasje", "UrgencyFoerPassivLekkasje", "UrgencyUnderPassivLekkasje",
                           "UrgencyUnderUtenTestMedLekkasje", "UrgencyUnderTestLekkasje", #"LekasjeFriFoerTest", "LekasjeFriUnderTest",
                           "AvfoeringerUnderTest", "LekkasjedagerUnder", 'OppfoelgingMulig', "ICIQ_hyppighet",
                           'ABD65', 'ABD652AT2','ABD60', "WexFastAvfoering", "WexBind", "WexFlytendeAvfoering", "WexLuft",
                           "WexLivsstilsendring", "WexnerTotalScore", "Onestage", "Testprosedyre", "KirurgiForRectumprolaps_v2",
                           "KunstigLukkMuskel")]

    ForlopData <- read.table('I:/nra/ForlopsOversikt2021-11-09 13-16-38.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
    ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                                 'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2', "OppflgRegStatus")]
    RegData <- merge(RegData, ForlopData, by = "ForlopsID", suffixes = c('', '_2'))
    Skjemaoversikt <- read.table('I:/nra/SkjemaOversikt2021-11-09 13-16-38.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
  }
  # Skjemaoversikt$SistLagretDato <- as.Date(Skjemaoversikt$SistLagretDato, format="%Y-%m-%d")
  # RegData$HovedDato[RegData$HovedDato == ''] <- as.character(Skjemaoversikt$SistLagretDato[Skjemaoversikt$ForlopsID %in%
  #                                                                                            RegData$ForlopsID[RegData$HovedDato == '']])
  RegData <- nra::nraPreprosess(RegData=RegData)


  tmp <- merge(Skjemaoversikt[Skjemaoversikt$Skjemanavn == '1A Anamnese', ], Skjemaoversikt[Skjemaoversikt$Skjemanavn == '1B Symptom', ],
               by = 'ForlopsID', suffixes = c('', '1B'))
  tmp <- merge(tmp, RegData[, c("ForlopsID", "PasientID", "ForlopsType1", "ForlopsType1Num", "ForlopsType2", "ForlopsType2Num")], by='ForlopsID', all.x = T)
  tmp <- merge(tmp, Skjemaoversikt[Skjemaoversikt$Skjemanavn %in% c('2A SNM-1'), ], suffixes = c('','SNM1'), by = 'ForlopsID', all.x = T)
  tmp <- merge(tmp, Skjemaoversikt[Skjemaoversikt$Skjemanavn %in% c('2A SNM-2'), ], suffixes = c('','SNM2'), by = 'ForlopsID', all.x = T)
  tmp <- merge(tmp, Skjemaoversikt[Skjemaoversikt$Skjemanavn %in% c('2B Sfinkter'), ], suffixes = c('','Sfinkter'), by = 'ForlopsID', all.x = T)
  tmp2 <- merge(Skjemaoversikt[Skjemaoversikt$Skjemanavn %in% c('1B Oppfølging 1 år'),], RegData[, c("ForlopsID", "KobletForlopsID")], by='ForlopsID')
  tmp <- merge(tmp, tmp2, suffixes = c('','Oppf1'), by.x = 'ForlopsID', by.y = "KobletForlopsID", all.x = T)
  tmp2 <- merge(Skjemaoversikt[Skjemaoversikt$Skjemanavn %in% c('1B Oppfølging 5 år'),], RegData[, c("ForlopsID", "KobletForlopsID")], by='ForlopsID')
  tmp <- merge(tmp, tmp2, suffixes = c('','Oppf5'), by.x = 'ForlopsID', by.y = "KobletForlopsID", all.x = T)

  tmp <- tmp[, c("ForlopsType1", "ForlopsType1Num", "ForlopsType2", "ForlopsType2Num", "ForlopsID", "PasientID",
                 "SkjemaStatus", "HovedDato", "Sykehusnavn", "AvdRESH", "SkjemaStatus1B", "SkjemaStatusSNM1",
                 "SkjemaStatusSNM2", "SkjemaStatusSfinkter", "SkjemaStatusOppf1", "SkjemaStatusOppf5")]

  tmp$ForlopsType1[is.na(tmp$ForlopsType1)][!is.na(tmp$SkjemaStatusSNM1[is.na(tmp$ForlopsType1)])] <- "SNM"
  tmp$ForlopsType1Num[is.na(tmp$ForlopsType1Num)][!is.na(tmp$SkjemaStatusSNM1[is.na(tmp$ForlopsType1)])] <- "SNM"
  tmp$ForlopsType1[is.na(tmp$ForlopsType1)][!is.na(tmp$SkjemaStatusSfinkter[is.na(tmp$ForlopsType1)])] <- "Sfinkterplastikk"

  utdata <- list(RegData = RegData, Skjemaoversikt = Skjemaoversikt, skjema_utflatet = tmp)

}


