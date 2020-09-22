library(nra)
library(tidyverse)
rm(list = ls())

RegData <- read.table('I:/nra/alleVarNum2020-09-04 15-00-46.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)

# pr pid, kan ha to abd60 , men kun 1 abd65
# write.csv2(tmp, "sedata.csv", fileEncoding = "Latin1")

# RegData$Aar <- format(as.Date(RegData$HovedDato), "%Y")
#
# tmp<-RegData[RegData$ForlopsType1Num==2 & RegData$Aar == 2020, ]
#
# table(RegData$JHGX00, useNA = 'ifany')
# table(RegData$JHGX002AT2, useNA = 'ifany')
#
#
# RegData[RegData$PatientID ==512, c("JHGX00", "JHGX002AT2")]
#

### Bestilling 08.09.2020 - 5-årsoppf sfinkter og SNM  ########################################

RegData <- read.table('I:/nra/alleVarNum2020-09-08.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
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
                       "WexLivsstilsendring", "WexnerTotalScore", "HovedForlopDato")]

ForlopData <- read.table('I:/nra/ForlopsOversikt2020-09-08.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                             'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2', "OppflgRegStatus")]
RegData <- merge(RegData, ForlopData, by = "ForlopsID", suffixes = c('', '_2'))
Skjemaoversikt <- read.table('I:/nra/SkjemaOversikt2020-01-07 09-52-47.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)

RegData <- nra::nraPreprosess(RegData=RegData)
aux <- RegData[RegData$ForlopsType1Num %in% 1:2, ]

oppf_5aar <- merge(aux[,c("PasientID", "ForlopsType1", "HovedDato")],
                   RegData[RegData$ForlopsType1Num==4, c('PasientID', "HovedDato", "HovedForlopDato", "ForlopsID")],
                   by = 'PasientID', all.x = T)
oppf_5aar <- oppf_5aar[which(oppf_5aar$HovedDato.x == oppf_5aar$HovedForlopDato), ]
oppf_5aar$Aar <- format(oppf_5aar$HovedDato.x, "%Y")

addmargins(table(oppf_5aar$ForlopsType1, oppf_5aar$Aar, useNA = 'ifany'))


### Bestilling januar  ########################################

RegData <- read.table('I:/nra/alleVarNum2020-09-08.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
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

ForlopData <- read.table('I:/nra/ForlopsOversikt2020-09-08.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                             'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2', "OppflgRegStatus")]
RegData <- merge(RegData, ForlopData, by = "ForlopsID", suffixes = c('', '_2'))
Skjemaoversikt <- read.table('I:/nra/SkjemaOversikt2020-09-08.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)

RegData <- nra::nraPreprosess(RegData=RegData)

aux_0 <- RegData[RegData$ForlopsType1Num %in% 1:2, ]

aux <- aux_0 %>% group_by(PasientID) %>% summarise(snm = 2 %in% ForlopsType1Num,
                                              sfinkt = 1 %in% ForlopsType1Num,
                                              N =n())
aux2 <- aux %>% group_by(PasientID) %>% summarise(ant_ulik =
                                                  N =n())

aux <- aux[aux$snm & aux$sfinkt, ]

oppf_1aar <- merge(aux[,c("PasientID", 'N')], RegData[RegData$ForlopsType1Num==3, c('PasientID', "HovedDato", "ForlopsID")], by = 'PasientID', all.x = T)

oppf_5aar <- merge(aux[,c("PasientID", 'N')], RegData[RegData$ForlopsType1Num==4, c('PasientID', "HovedDato", "ForlopsID")], by = 'PasientID', all.x = T)









