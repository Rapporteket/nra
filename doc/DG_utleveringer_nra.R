library(nra)
library(tidyverse)
rm(list = ls())

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














