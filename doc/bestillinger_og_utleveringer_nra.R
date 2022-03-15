library(nra)
library(tidyverse)
rm(list = ls())

###### Registreringer med både St. Marks og Wexner #########################################
RegData <- nra::nraHentRegData()
RegData <- nra::nraPreprosess(RegData=RegData)
RegData <- RegData[!is.na(RegData$StMarksTotalScore) & !is.na(RegData$WexnerTotalScore), ]

forlop <- merge(RegData[RegData$ForlopsType1Num %in% 1:2, c("ForlopsID", "HovedDato", "ForlopsType1Num", "StMarksTotalScore", "WexnerTotalScore")],
                RegData[RegData$ForlopsType1Num %in% 3, c("KobletForlopsID", "HovedDato", "ForlopsType1Num", "StMarksTotalScore", "WexnerTotalScore")],
                by.x = "ForlopsID", by.y = "KobletForlopsID", suffixes = c("", "_oppf"))

##### Ny utkjøring, nå med EndeTilEndeSutur inkludert 22.11.2021 ###########################3
registryName <- "nra"
dbType <- "mysql"
query <- "SELECT * FROM alleVarNum"
RegData <- rapbase::loadRegData(registryName, query, dbType)
RegData_snm <- RegData[RegData$ForlopsType1Num == 2 & RegData$AvdRESH == 601225, ]
RegData_oppf <- RegData[RegData$KobletForlopsID %in% RegData_snm$ForlopsID, ]
utlevering <- dplyr::bind_rows(RegData_snm, RegData_oppf)

write.csv2(utlevering, "/home/rstudio/.ssh/utlevering_nra_snm_2022_02_02.csv", row.names = F, fileEncoding = "Latin1")


##### Ny utkjøring, nå med EndeTilEndeSutur inkludert 22.11.2021 ###########################3
registryName <- "nra"
dbType <- "mysql"
query <- "SELECT * FROM alleVarNum"
RegData <- rapbase::loadRegData(registryName, query, dbType)
RegData_sfinkt <- RegData[RegData$ForlopsType1Num == 1, ]
RegData_oppf <- RegData[RegData$KobletForlopsID %in% RegData_sfinkt$ForlopsID, ]
utlevering <- dplyr::bind_rows(RegData_sfinkt, RegData_oppf)

write.csv2(utlevering, "/home/rstudio/.ssh/utlevering_nra_2021_11_22.csv", row.names = F, fileEncoding = "Latin1")

### Stid Norderval
RegData <- read.table('I:/nra/alleVarNum2021-06-25 14-16-02.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
# ForlopData <- read.table('I:/nra/ForlopsOversikt2021-06-25 14-16-02.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
RegData_sfinkt <- RegData[RegData$ForlopsType1Num == 1, ]
RegData_oppf <- RegData[RegData$KobletForlopsID %in% RegData_sfinkt$ForlopsID, ]
utlevering <- dplyr::bind_rows(RegData_sfinkt, RegData_oppf)

write.csv2(utlevering, "I:/nra/utlevering_nra_2021_06_29.csv", row.names = F, fileEncoding = "Latin1")

### Mai Lisbeth 12.01.2021 ###############################
Skjemaoversikt <- read.table('I:/nra/SkjemaOversikt2021-02-12 11-10-47.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
Skjemaoversikt$SistLagretDato <- as.Date(Skjemaoversikt$SistLagretDato)
aux <- Skjemaoversikt[Skjemaoversikt$SistLagretDato == "2021-12-12", ]

RegData <- read.table('I:/nra/alleVar2021-02-12 11-10-47.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
aux <- RegData[RegData$ForlopsType1Num == 3, ]

write.csv2(RegData, "I:/nra/regdata.csv", row.names = F, fileEncoding = "Latin1")

### Ukjent bestilling ... ###############################
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









