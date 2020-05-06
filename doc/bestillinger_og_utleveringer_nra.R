library(nra)
library(tidyverse)
rm(list = ls())

RegData <- read.table('I:/nra/alleVarNum2020-01-07 09-52-48.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
RegData <- RegData[, c('ForlopsID', 'Ukjent', 'AnnenBekkenKirurgi', 'AnnetTraume', 'Hemoroidereksjon', 'NevrologiskSykdom', 'ObsteriskSkade',
                       'PeriferNervskade', 'PerinealAbscess', 'Rectumreseksjon', 'Sfinkterotomi', 'AnnetEtiologi', 'Konservativ',
                       'Irrigasjon', 'Tibialisstimulering', 'AnalInjection', 'SNM', 'Sfinkterplastikk', 'Rectopexi',
                       'KirurgiForRectumprolaps', 'Gracilisplastikk', 'Stomi', 'AnnetTidligereBeh', "SenterKortNavn", "Symtomvarighet",
                       "Ultralyd", "PartiellDefekt", "FullveggsdefektYtreSfinkter", "FullveggsdefektIndreSfinkter", "GenQol",
                       "StMarksTotalScore", "QolSexualitet", "KobletForlopsID", "Tilfredshet", "Urinlekkasje", "Komplikasjon",
                       "KomplikasjonT2", "PostopKomplikasjoner", "Bloedning", "Saarinfeksjon", "Saardehisens", "InkontinensFoerTest",
                       "UrgencyFoerTest", "AvfoeringerFoerTest", "LekkasjedagerFoer", "InkontinensUnderTest", "UrgencyUnderTest",
                       "AvfoeringerUnderTest", "LekkasjedagerUnder", 'OppfoelgingMulig',
                       'ABD65', 'ABD652AT2','ABD60', "WexFastAvfoering", "WexBind", "WexFlytendeAvfoering", "WexLuft",
                       "WexLivsstilsendring", "WexnerTotalScore")]

ForlopData <- read.table('I:/nra/ForlopsOversikt2020-01-07 09-52-46.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                             'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2', "OppflgRegStatus")]
RegData <- merge(RegData, ForlopData, by = "ForlopsID", suffixes = c('', '_2'))
Skjemaoversikt <- read.table('I:/nra/SkjemaOversikt2020-01-07 09-52-47.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)

RegData <- nra::nraPreprosess(RegData=RegData)

aux <- RegData[RegData$ForlopsType1Num %in% 1:2, ]

aux <- aux %>% group_by(PasientID) %>% summarise(snm = 2 %in% ForlopsType1Num,
                                              sfinkt = 1 %in% ForlopsType1Num,
                                              N =n())
aux <- aux[aux$snm & aux$sfinkt, ]

oppf_1aar <- merge(aux[,c("PasientID", 'N')], RegData[RegData$ForlopsType1Num==3, c('PasientID', "HovedDato", "ForlopsID")], by = 'PasientID', all.x = T)

oppf_5aar <- merge(aux[,c("PasientID", 'N')], RegData[RegData$ForlopsType1Num==5, c('PasientID', "HovedDato", "ForlopsID")], by = 'PasientID', all.x = T)









