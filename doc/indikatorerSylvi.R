library(nra)
library(tidyverse)
rm(list = ls())

RegData <- read.table('I:/nra/alleVarNum2020-09-17 15-10-35.txt', header=TRUE, sep=";", encoding = 'UTF-8')
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

ForlopData <- read.table('I:/nra/ForlopsOversikt2020-09-17 15-10-35.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                             'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2', "OppflgRegStatus")]

RegData <- merge(RegData, ForlopData, by = "ForlopsID", suffixes = c('', '_2'))
RegData <- nraPreprosess(RegData=RegData)

RegDataAlle <- RegData

# RegData$SenterKortNavn <- iconv(RegData$SenterKortNavn, from = 'UTF-8', to = '')

reshID <- 700116 #  #Må sendes med til funksjon
minald <- 0  #alder, fra og med
maxald <- 130	#alder, til og med
erMann <- 99
datoFra <- '2011-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2019-12-31'
enhetsUtvalg <- 0 #0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
preprosess<-F
hentData <- F
forlopstype1=99
forlopstype2=99
valgtShus <- '' #c('601225', '700116')
utformat <- 'pdf'
rap_aar <- 2019

# Suksessrate test-prosedyre SNM

RegData <- RegData[RegData$ForlopsType1Num == 2, ]
RegData <- RegData[RegData$ForlopsType2Num == 2, ]
RegData <- RegData[!is.na(RegData$InkontinensFoerTest), ]

nraUtvalg <- nraUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                       minald=minald, maxald=maxald, erMann=erMann, valgtShus=valgtShus,
                       forlopstype1=forlopstype1, forlopstype2=forlopstype2)
RegData <- nraUtvalg$RegData
rm(nraUtvalg)
RegData <- RegData[-which(rowSums(is.na(RegData[, c("InkontinensFoerTest","InkontinensUnderTest")])) !=0 ), ]
indikator <- RegData[, c("Aar", "AvdRESH", "InkontinensFoerTest", "InkontinensUnderTest", "PasientID", "ForlopsID")]
indikator$ind <- (indikator$InkontinensFoerTest - indikator$InkontinensUnderTest)/indikator$InkontinensFoerTest*100
indikator$ind[is.nan(indikator$ind)] <- 0
indikator$ind <- as.numeric(indikator$ind >= 50)
indikator$nevner <- 1
indikator$Index <- 'Ind1'
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator <- indikator[, c("AvdRESH", "Aar", "ind", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind1', 'Nevner Ind1', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind1')]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator1.", utformat)
nraFigIndikator(plotdata, tittel = c('Andel med prosentvis reduksjon', 'i lekkasjeepisoper >= 50%'), terskel = 5, maal = 70,
                outfile=outfile)

ind1_50pstlekkasjereduksjon <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind1_50pstlekkasjereduksjon.csv', row.names = F)

# indikator1 %>% group_by(Aar, AvdRESH) %>% summarise("andel måloppnåelse" = mean(ind)*100,
#                                           N = n())


# Andel med utført ultralyd
RegData <- RegDataAlle[RegDataAlle$Aar <= rap_aar, ]
indikator <- RegData[which(RegData$Ultralyd %in% 0:2), ]
indikator <- indikator[indikator$ForlopsType1Num %in% c(1,2) & indikator$ForlopsType2Num %in% c(1,2,5, NA), ]

indikator <- indikator[ , c("Aar", "AvdRESH", "PasientID", "ForlopsID", "Sfinktervurdering")]
indikator$ind <- indikator$Sfinktervurdering
indikator$ind <- as.numeric(indikator$ind != 99)
indikator$nevner <- 1
indikator$Index <- 'Ind2'
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator <- indikator[, c("AvdRESH", "Aar", "ind", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind2', 'Nevner Ind2', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind2')]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator2.", utformat)
nraFigIndikator(plotdata, tittel = c('Andel med utført ultralyd'), terskel = 5, maal = 95, outfile=outfile)

ind2_ultralyd <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind2_ultralyd.csv', row.names = F)

# Sårinfeksjon innen 30 dager

RegData$Variabel <- pmax(RegData$Komplikasjon, RegData$KomplikasjonT2, na.rm = T)
RegData <- RegData[RegData$ForlopsType1Num == 2, ]
RegData <- RegData[!is.na(RegData$Variabel), ]
RegData$Variabel[which(RegData$Variabel==9 & (RegData$Komplikasjon==2 | RegData$KomplikasjonT2==2))] <- 2   # Velg bekreftet eller mistenkt
RegData$Variabel[which(RegData$Variabel==9 & (RegData$Komplikasjon==1 | RegData$KomplikasjonT2==1))] <- 1
RegData$Variabel[which(RegData$Variabel==98 & (RegData$Komplikasjon==2 | RegData$KomplikasjonT2==2))] <- 2   # Velg bekreftet eller mistenkt
RegData$Variabel[which(RegData$Variabel==98 & (RegData$Komplikasjon==1 | RegData$KomplikasjonT2==1))] <- 1

indikator <- RegData[ , c("Aar", "AvdRESH", "PasientID", "ForlopsID", "Variabel")]
indikator$ind <- as.numeric(indikator$Variabel == 2)
indikator$nevner <- 1
indikator$Index <- 'Ind3'
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator <- indikator[, c("AvdRESH", "Aar", "ind", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind3', 'Nevner Ind3', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind3')]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator3.", utformat)
nraFigIndikator(plotdata, tittel = c('Andel bekreftet sårinfeksjon innen', '30 dager etter implantasjon'), terskel = 5,
                maal = 4, maalretn = 'lav', decreasing = T, outfile = outfile)

ind3_saarinfeksjon <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind3_saarinfeksjon.csv', row.names = F)

##############################################

RegData <- RegDataAlle
RegDataStr9 <- rbind(RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num %in% 1:2), ],
                     RegData[which(RegData$ForlopsType1Num %in% 3:4),])
RegDataStr12 <- rbind(RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num %in% 1:2), ],
                      RegData[which(RegData$ForlopsType1Num %in% 3:4),])
WexDataStr9 <- rbind(RegData[which(RegData$WexnerTotalScore>9 & RegData$ForlopsType1Num %in% 1:2), ],
                     RegData[which(RegData$ForlopsType1Num %in% 3:4),])
WexDataStr12 <- rbind(RegData[which(RegData$WexnerTotalScore>12 & RegData$ForlopsType1Num %in% 1:2), ],
                      RegData[which(RegData$ForlopsType1Num %in% 3:4),])


# St. Mark’s Inkontinensskår <9 og 12, 1 og 5 år etter SNM
RegData <- RegDataStr9

RegData$Indikator <- NA
RegData$Indikator[which(RegData$StMarksTotalScore<=9)] <- 1
RegData$Indikator[which(RegData$StMarksTotalScore>9)] <- 0

Oppfolging <- RegData[RegData$ForlopsType1Num == 3, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind4'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind4', 'Nevner Ind4', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind4')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator4.", utformat)
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=9', '1 år etter operasjon med SNM'), terskel = 5,
                maal = 30, outfile=outfile)

ind4_stmarks_9_1aar_snm <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind4_stmarks_9_1aar_snm.csv', row.names = F)
##

Oppfolging <- RegData[RegData$ForlopsType1Num == 4, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind5'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind5', 'Nevner Ind5', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind5')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-5), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator5.", utformat)
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=9', '5 år etter operasjon med SNM'), terskel = 5,
                maal = 30, outfile=outfile)

ind5_stmarks_9_5aar_snm <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind5_stmarks_9_5aar_snm.csv', row.names = F)

##

RegData <- RegDataStr12

RegData$Indikator <- NA
RegData$Indikator[which(RegData$StMarksTotalScore<=12)] <- 1
RegData$Indikator[which(RegData$StMarksTotalScore>12)] <- 0

Oppfolging <- RegData[RegData$ForlopsType1Num == 3, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind6'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind6', 'Nevner Ind6', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind6')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator6.", utformat)
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=12', '1 år etter operasjon med SNM'), terskel = 5,
                maal = 50, outfile=outfile)

ind6_stmarks_12_1aar_snm <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind6_stmarks_12_1aar_snm.csv', row.names = F)
##

Oppfolging <- RegData[RegData$ForlopsType1Num == 4, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind7'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind7', 'Nevner Ind7', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind7')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-5), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator7.", utformat)
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=12', '5 år etter operasjon med SNM'), terskel = 5,
                maal = 50, outfile=outfile)

ind7_stmarks_12_5aar_snm <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind7_stmarks_12_5aar_snm.csv', row.names = F)
##

# St. Mark’s Inkontinensskår <9 og 12, 1 og 5 år etter Sfinkterplastikk
RegData <- RegDataStr9

RegData$Indikator <- NA
RegData$Indikator[which(RegData$StMarksTotalScore<=9)] <- 1
RegData$Indikator[which(RegData$StMarksTotalScore>9)] <- 0

Oppfolging <- RegData[RegData$ForlopsType1Num == 3, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind8'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind8', 'Nevner Ind8', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind8')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator8.", utformat)
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=9', '1 år etter sfinkterplastikk'), terskel = 5,
                maal = 30, outfile=outfile)

ind8_stmarks_9_1aar_sfinkt <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind8_stmarks_9_1aar_sfinkt.csv', row.names = F)
##

Oppfolging <- RegData[RegData$ForlopsType1Num == 4, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind9'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind9', 'Nevner Ind9', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind9')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-5), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator9.", utformat)
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=9', '5 år etter sfinkterplastikk'), terskel = 5,
                maal = 30, outfile=outfile)

ind9_stmarks_9_5aar_sfinkt <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind9_stmarks_9_5aar_sfinkt.csv', row.names = F)
##

RegData <- RegDataStr12

RegData$Indikator <- NA
RegData$Indikator[which(RegData$StMarksTotalScore<=12)] <- 1
RegData$Indikator[which(RegData$StMarksTotalScore>12)] <- 0

Oppfolging <- RegData[RegData$ForlopsType1Num == 3, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind10'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind10', 'Nevner Ind10', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind10')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator10.", utformat)
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=12', '1 år etter sfinkterplastikk'), terskel = 5,
                maal = 50, outfile=outfile)

ind10_stmarks_12_1aar_sfinkt <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind10_stmarks_12_1aar_sfinkt.csv', row.names = F)
##

Oppfolging <- RegData[RegData$ForlopsType1Num == 4, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind11'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind11', 'Nevner Ind11', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind11')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-5), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator11.", utformat)
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=12', '5 år etter sfinkterplastikk'), terskel = 5,
                maal = 30, outfile=outfile)

ind11_stmarks_12_5aar_sfinkt <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind11_stmarks_12_5aar_sfinkt.csv', row.names = F)
##

# Wexner Inkontinensskår <9 og 12, 1 år etter SNM

RegData <- WexDataStr9

RegData$Indikator <- NA
RegData$Indikator[which(RegData$WexnerTotalScore<=9)] <- 1
RegData$Indikator[which(RegData$WexnerTotalScore>9)] <- 0

Oppfolging <- RegData[RegData$ForlopsType1Num == 3, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind12'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind12', 'Nevner Ind12', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind12')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator12.", utformat)
nraFigIndikator(plotdata, tittel = c('Wexnerskår <=9', '1 år etter snm'), terskel = 5,
                maal = 30, outfile=outfile)

ind12_wexner_9_1aar_snm <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind12_wexner_9_1aar_snm.csv', row.names = F)
##

RegData <- WexDataStr12

RegData$Indikator <- NA
RegData$Indikator[which(RegData$WexnerTotalScore<=12)] <- 1
RegData$Indikator[which(RegData$WexnerTotalScore>12)] <- 0

Oppfolging <- RegData[RegData$ForlopsType1Num == 3, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind13'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind13', 'Nevner Ind13', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind13')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0("indikator13.", utformat)
nraFigIndikator(plotdata, tittel = c('Wexnerskår <=12', '1 år etter snm'), terskel = 5,
                maal = 50, outfile=outfile)

ind13_wexner_12_1aar_snm <- indikator
write.csv2(indikator,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind13_wexner_12_1aar_snm.csv', row.names = F)
##

kobling_resh_shusnavn <- data.frame(ReshID = unique(RegDataAlle$AvdRESH), Sykehusnavn = RegDataAlle$Sykehusnavn[match(unique(RegDataAlle$AvdRESH), RegDataAlle$AvdRESH)])

write.csv2(kobling_resh_shusnavn,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/kobling_resh_shusnavn.csv', row.names = F)

################## NØKKELTALL ######################################


nokkeltall <- RegDataAlle %>% group_by(Aar) %>%
  summarise('Antall SNM' = sum(ForlopsType1Num==2),
            'Antall sfinkt'  = sum(ForlopsType1Num==1),
            'Antall 1-årsoppf.' = sum(ForlopsType1Num==3),
            'Antall 5-årsoppf.' = sum(ForlopsType1Num==4),
            'Totalt' = n(),
            'Antall sykehus' = length(unique(AvdRESH)),
            'Gjennomsnittsalder' = mean(PasientAlder[ForlopsType1Num %in% 1:2]),
            'Andel 65 år og eldre' = sum(PasientAlder[ForlopsType1Num %in% 1:2]>=65)/sum(ForlopsType1Num %in% 1:2),
            'Andel med symptomvarighet mer enn 10 år' = sum(Symtomvarighet[ForlopsType1Num %in% 1:2]==4)/sum(ForlopsType1Num %in% 1:2)
  )

write.csv2(nokkeltall, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/nokkeltall_nra.csv', row.names = F)

########## Før -og ettertall #####################

RegData <- RegDataAlle
grvar='AvdRESH'
valgtVar <- 'Urinlekkasje'


prepostgjsntab <- function(grvar, valgtVar,RegData, datoFra, datoTil, minald, maxald, erMann,
                           forlopstype1, forlopstype2, valgtShus) {
  RegData$Grvar <- RegData[, grvar]
  RegData$Variabel <- RegData[, valgtVar]
  RegData <- RegData[!is.na(RegData$Variabel), ]
  if (valgtVar=='Urinlekkasje') {
    RegData <- RegData[RegData$Urinlekkasje != 9, ]
    RegData$Variabel <- 100*RegData$Variabel}

  ## Skill ut oppfølginger
  Oppfolging1 <- RegData[RegData$ForlopsType1Num == 3, ]
  Oppfolging2 <- RegData[RegData$ForlopsType1Num == 4, ]

  RegData <- RegData[RegData$ForlopsType1Num %in% 1:2, ]

  ## Gjør utvalg
  nraUtvalg <- nraUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil,
                         minald=minald, maxald=maxald, erMann=erMann,
                         forlopstype1=forlopstype1, forlopstype2=forlopstype2, valgtShus=valgtShus)
  RegData <- nraUtvalg$RegData

  RegData <- RegData[which(RegData$OppflgRegStatus %in% 1:2), ]
  Oppfolging1 <- Oppfolging1[Oppfolging1$KobletForlopsID %in% RegData$ForlopsID, ]
  RegData <- RegData[RegData$ForlopsID %in% Oppfolging1$KobletForlopsID, ]
  RegData <- merge(RegData[,c("PasientID", "Variabel", "Grvar", "ForlopsID", "ForlopsType1Num", "Aar")],
                   Oppfolging1[,c("Variabel", "KobletForlopsID", "ForlopsType1Num")], by.x = 'ForlopsID', by.y = 'KobletForlopsID',
                   suffixes = c('Pre', 'Post1'))
  if (valgtVar=='QolSexualitet') {
    Nuaktuelt <- length(RegData$VariabelPre[RegData$VariabelPre==99 | RegData$VariabelPost1==99])
    RegData <- RegData[RegData$VariabelPre!=99, ]
    RegData <- RegData[RegData$VariabelPost1!=99, ]
  }

  utdata <- RegData %>% group_by(Aar, Grvar) %>% summarise(gj.sn.pre = mean(VariabelPre),
                                                           gj.sn.post = mean(VariabelPost1),
                                                           N = n())
}

urin_snm <- prepostgjsntab(grvar=grvar, valgtVar=valgtVar, RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                           forlopstype1=2, forlopstype2=forlopstype2, valgtShus=valgtShus)

urin_sfinkter <- prepostgjsntab(grvar=grvar, valgtVar=valgtVar, RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                                forlopstype1=1, forlopstype2=forlopstype2, valgtShus=valgtShus)
valgtVar <- 'GenQol'
gql_snm <- prepostgjsntab(grvar=grvar, valgtVar=valgtVar, RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                          forlopstype1=2, forlopstype2=forlopstype2, valgtShus=valgtShus)

gql_sfinkter <- prepostgjsntab(grvar=grvar, valgtVar=valgtVar, RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, erMann=erMann,
                               forlopstype1=1, forlopstype2=forlopstype2, valgtShus=valgtShus)

RegData <- merge(RegData, RegData[which(RegData$ForlopsType1Num==3), c("Tilfredshet", "KobletForlopsID")], by.x = 'ForlopsID', by.y = 'KobletForlopsID',
                 suffixes = c('', 'Post1'), all.x = TRUE)

tilfredshet_1aar <- RegData[!is.na(RegData$TilfredshetPost1), ] %>% group_by(Aar, AvdRESH, TilfredshetPost1) %>% summarise(Antall = n())
tilfredshet_1aar <- spread(tilfredshet_1aar, key=TilfredshetPost1, value = Antall, fill = 0)


write.csv2(urin_snm, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/urin_snm.csv', row.names = F)
write.csv2(urin_sfinkter, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/urin_sfinkter.csv', row.names = F)
write.csv2(gql_snm, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/gql_snm.csv', row.names = F)
write.csv2(gql_sfinkter, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/gql_sfinkter.csv', row.names = F)
write.csv2(tilfredshet_1aar, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/tilfredshet_1aar.csv', row.names = F)

