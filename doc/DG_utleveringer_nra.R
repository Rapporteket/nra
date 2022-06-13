library(nra)
library(tidyverse)
rm(list = ls())

######## Utlevering for dekningsgradsanalyse NPR 13.06.2022 #################################
allevar <- nra::nraHentTabell("alleVarNum")
foversikt <- nra::nraHentTabell("ForlopsOversikt")
RegData <- merge(allevar, foversikt[, c("ForlopsID", names(foversikt)[!(names(foversikt) %in% intersect(names(allevar), names(foversikt)))])],
                 by = "ForlopsID")
RegData <- nraPreprosess(RegData=RegData)

dg_tall <- RegData[RegData$ForlopsType1Num %in% 1:2, c("PatientID", "AvdRESH", "SenterKortNavn", "ForlopsType1", "ForlopsType2",
                                                       "ProsedyreDato2A", "ProsedyreDato2AT2", "OperasjonsDato2B")]

dg_tall <- dg_tall[as.numeric(format(as.Date(dg_tall$ProsedyreDato2A, format = "%Y-%m-%d"), "%Y")) %in% 2020:2021 |
                     as.numeric(format(as.Date(dg_tall$ProsedyreDato2AT2, format = "%Y-%m-%d"), "%Y")) %in% 2020:2021 |
                     as.numeric(format(as.Date(dg_tall$OperasjonsDato2B, format = "%Y-%m-%d"), "%Y")) %in% 2020:2021, ]

dg_tall <- dg_tall[dg_tall$ForlopsType2 != "Eksplantasjon" | is.na(dg_tall$ForlopsType2), ]
dg_tall$prosedyre <- NA
dg_tall$prosedyre[dg_tall$ForlopsType1 == "Sfinkterplastikk"] <- "JHC10"
dg_tall$prosedyre[dg_tall$ForlopsType2 %in% c("Test positiv", "Revisjon")] <- "ABD60/ABD65/JHGX00"
dg_tall$prosedyre[dg_tall$ForlopsType2 %in% c("Test negativ", "Test usikker")] <- "ABD60/JHGX00"


write.csv2(dg_tall, "~/.ssh/nra/aktivitetsdata_nra.csv", row.names = F, fileEncoding = "Latin1", na = "")


kobl <- read.table("~/.ssh/nra/Norsk_Register_for_Analinkontinens_koblingstabell_datadump_13.06.2022.csv",
                   sep = ";", fileEncoding = "UTF-8", stringsAsFactors = F,
                   header = T, colClasses = 'character')


kobl <- kobl[kobl$PID %in% dg_tall$PatientID, ]
write.csv2(kobl, "~/.ssh/nra/koblingsfil_nra.csv", row.names = F, fileEncoding = "Latin1", na = "")








####### Figur til Mona Rydningen 02.12.2020 #########################################
RegData <- read.table('I:/nra/alleVarNum2020-12-02 12-16-58.txt', header=TRUE, sep=";", encoding = 'UTF-8')
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
                       "WexLivsstilsendring", "WexnerTotalScore", "Onestage", "Testprosedyre")]

ForlopData <- read.table('I:/nra/ForlopsOversikt2020-12-02 12-16-58.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                             'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2', "OppflgRegStatus")]

RegData <- merge(RegData, ForlopData, by = "ForlopsID", suffixes = c('', '_2'))
RegData <- nraPreprosess(RegData=RegData)

outfile <- 'StMarksTotalScore_aar_snm.pdf'
utdata <- nraGjsnPrePost(RegData[which(RegData$AvdRESH == 601225), ], valgtVar='StMarksTotalScore', datoFra='2013-01-01',
                         outfile = outfile, preprosess=F, grvar = 'Aar',
                        forlopstype1=2, reshID = 0,
                         sammenlign=1, inkl_konf=1, valgtShus='')

samletab <- bind_rows(as_tibble(utdata[[1]]), as_tibble(utdata[[2]]), as_tibble(utdata[[3]]))
samletab[7, ] <- as.list(unname(utdata[[8]]))
names(samletab) <- unname(utdata[[6]])
samletab$Var <- c("StMarksPre", "StMarksPost", "KIpreNedre", "KIpostNedre", "KIpreOvre", "KIpostOvre", "N")
samletab <- samletab[, c(9,1:8)]

write.csv2(samletab, "Tabell_Mona_02122020.csv", row.names = F)

######## Utlevering kvalitetskontroll UNN 24.11.2020 #################################

RegData <- read.table('I:/nra/alleVarNum2020-11-24 12-17-20.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- read.table('I:/nra/ForlopsOversikt2020-11-24 12-17-20.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                             'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2', "OppflgRegStatus")]
RegData <- merge(RegData, ForlopData, by = "ForlopsID", suffixes = c('', '_2'))
RegData <- nra::nraPreprosess(RegData=RegData)

unnData <- RegData[which(RegData$AvdRESH == 601225 & RegData$Aar >=2016), ]
# write.csv2(unnData, "unndata.csv", row.names = F, fileEncoding = "Latin1")

unnData <- merge(unnData[unnData$ForlopsType1Num %in% 2, ],
                 unnData[unnData$ForlopsType1Num %in% 3, c("KobletForlopsID", "StMarksTotalScore", "Urinlekkasje")],
      by.x = 'ForlopsID', by.y = 'KobletForlopsID', suffixes = c('Pre', 'Post1'), all.x = T)

unnData %>% group_by(Aar) %>% summarise("Antall Onestage" = sum(Onestage, na.rm = T),
                                        "Antall Onestage med revisjon" = sum(ForlopsType2Num[Onestage==1]==3, na.rm = T),
                                        "Antall Onestage med eksplantasjon" = sum(ForlopsType2Num[Onestage==1]==4, na.rm = T))

unnDataUt <- unnData[which(unnData$Onestage == 1), c("ForlopsID", "PasientID", "HovedDato", "Aar",
                                                     "Onestage", "ForlopsType1", "ForlopsType2",
                                                     "StMarksTotalScorePre", "StMarksTotalScorePost1",
                                                     "UrinlekkasjePre", "UrinlekkasjePost1")]

write.csv2(unnDataUt, "I:/nra/unndata24112020.csv", row.names = F, fileEncoding = "Latin1")

######## Utlevering NPR 17.09.2020 #################################

RegData <- read.table('I:/nra/alleVarNum2020-09-17 15-10-35.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- read.table('I:/nra/ForlopsOversikt2020-09-17 15-10-35.txt', header=TRUE, sep=";", encoding = 'UTF-8', stringsAsFactors = F)
ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                             'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2', "OppflgRegStatus")]
RegData <- merge(RegData, ForlopData, by = "ForlopsID", suffixes = c('', '_2'))
RegData <- nra::nraPreprosess(RegData=RegData)

dg_tall <- RegData[RegData$ForlopsType1Num %in% 1:2, c("PatientID", "AvdRESH", "SenterKortNavn", "ForlopsType1", "ForlopsType2",
                       "ProsedyreDato2A", "ProsedyreDato2AT2", "OperasjonsDato2B")]

dg_tall <- dg_tall[as.numeric(format(as.Date(dg_tall$ProsedyreDato2A, format = "%Y-%m-%d"), "%Y")) %in% 2018:2019 |
                     as.numeric(format(as.Date(dg_tall$ProsedyreDato2AT2, format = "%Y-%m-%d"), "%Y")) %in% 2018:2019 |
                     as.numeric(format(as.Date(dg_tall$OperasjonsDato2B, format = "%Y-%m-%d"), "%Y")) %in% 2018:2019, ]

dg_tall <- dg_tall[dg_tall$ForlopsType2 != "Eksplantasjon", ]
dg_tall$prosedyre <- NA
dg_tall$prosedyre[dg_tall$ForlopsType1 == "Sfinkterplastikk"] <- "JHC10"
dg_tall$prosedyre[dg_tall$ForlopsType2 %in% c("Test positiv", "Revisjon")] <- "ABD60/ABD65/JHGX00"
dg_tall$prosedyre[dg_tall$ForlopsType2 %in% c("Test negativ", "Test usikker")] <- "ABD60/JHGX00"


write.csv2(dg_tall, "I:/nra/aktivitetsdata_nra.csv", row.names = F, fileEncoding = "Latin1", na = "")


kobl <- read.table("I:/nra/nraPasienterList.csv", sep = ",", fileEncoding = "UTF-8", stringsAsFactors = F,
                   header = T, colClasses = 'character')


kobl <- kobl[kobl$PID %in% dg_tall$PatientID, ]
write.csv2(kobl, "I:/nra/koblingsfil_nra.csv", row.names = F, fileEncoding = "Latin1", na = "")

# dg_tall[which(dg_tall$Onestage==1),]
# tmp <- dg_tall[which(dg_tall$ForlopsType1=="Sfinkterplastikk"), ]














