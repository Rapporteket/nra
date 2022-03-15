library(nra)
library(tidyverse)
rm(list = ls())

RegData <- read.table('I:/nra/alleVarNum2021-09-03 12-07-14.txt', header=TRUE, sep=";", encoding = 'UTF-8')
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

ForlopData <- read.table('I:/nra/ForlopsOversikt2021-09-03 12-07-14.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                             'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2', "OppflgRegStatus")]

RegData <- merge(RegData, ForlopData, by = "ForlopsID", suffixes = c('', '_2'))
RegData <- nraPreprosess(RegData=RegData)
RegData$SenterKortNavn <- paste0(RegData$SenterKortNavn, ' ')

RegDataAlle <- RegData

# write.csv2(RegDataAlle, "utforsk.csv", row.names = F, fileEncoding = "Latin1")

rap_aar <- 2020
reshID <- 700116 #  #Må sendes med til funksjon
minald <- 0  #alder, fra og med
maxald <- 130	#alder, til og med
erMann <- 99
datoFra <- '2011-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- paste0(rap_aar, "-12-31")
enhetsUtvalg <- 0 #0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
preprosess<-F
hentData <- F
forlopstype1=99
forlopstype2=99
valgtShus <- '' #c('601225', '700116')
utformat <- 'wmf'
figfolder <- "C:/GIT/nra/inst/indikatorfig/"

# Suksessrate test-prosedyre SNM

# RegData <- RegData[RegData$ForlopsType1Num == 2, ]
# RegData <- RegData[RegData$ForlopsType2Num == 2, ]
# RegData <- RegData[!is.na(RegData$InkontinensFoerTest), ]

nraUtvalg <- nraUtvalg(RegData=RegDataAlle, datoFra=datoFra, datoTil=datoTil,
                       minald=minald, maxald=maxald, erMann=erMann, valgtShus=valgtShus,
                       forlopstype1=2, forlopstype2=forlopstype2)
RegData <- nraUtvalg$RegData
rm(nraUtvalg)

## Gammel versjon
# RegData <- RegData[-which(rowSums(is.na(RegData[, c("InkontinensFoerTest","InkontinensUnderTest")])) !=0 ), ]
# indikator <- RegData[, c("Aar", "AvdRESH", "InkontinensFoerTest", "InkontinensUnderTest", "PasientID", "ForlopsID")]
# indikator$ind <- (indikator$InkontinensFoerTest - indikator$InkontinensUnderTest)/indikator$InkontinensFoerTest*100
# indikator$ind[is.nan(indikator$ind)] <- 0
# indikator$ind <- as.numeric(indikator$ind >= 50)
## Ny versjon
indikator <- RegData[!is.na(RegData$Indikator1_lekk_red50), c("Aar", "AvdRESH", "Indikator1_lekk_red50", "PasientID", "ForlopsID")]
names(indikator)[names(indikator)=="Indikator1_lekk_red50"] <- "ind"

indikator$nevner <- 1
indikator$Index <- 'Ind1'
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator <- indikator[, c("AvdRESH", "Aar", "ind", "nevner", "Index", "AarID")]
Indikatorer <- indikator
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind1', 'Nevner Ind1', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind1')]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator1.", utformat))
nraFigIndikator(plotdata, tittel = c('Andel med prosentvis reduksjon', 'i lekkasjeepisoder >= 50%')
                , terskel = 5, maal = 70,
                outfile=outfile)

ind1_50pstlekkasjereduksjon <- indikator
# write.csv2(indikator,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind1_50pstlekkasjereduksjon.csv', row.names = F)

# indikator %>% group_by(Aar, ReshId) %>% summarise("andel måloppnåelse" = mean(`Teller Ind1`)*100,
#                                           N = n()) %>% filter(Aar >= 2018)


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
Indikatorer <- bind_rows(Indikatorer, indikator)
# Indikatorer <- indikator
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind2', 'Nevner Ind2', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind2')]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator2.", utformat))
nraFigIndikator(plotdata, tittel = c('Andel med utført ultralyd'), terskel = 5, maal = 95, outfile=outfile)

ind2_ultralyd <- indikator
# write.csv2(indikator,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind2_ultralyd.csv', row.names = F)

# Andel forløp med tidligere konservativ behandling
# RegData <- RegDataAlle[RegDataAlle$Aar <= rap_aar, ]
indikator <- RegDataAlle[RegDataAlle$Aar <= rap_aar, ]
indikator <- indikator[indikator$ForlopsType1Num %in% c(1,2) & indikator$ForlopsType2Num %in% c(1,2,5, NA), ]

indikator <- indikator[ , c("Aar", "AvdRESH", "PasientID", "ForlopsID", "Konservativ_v2")]
indikator$ind <- indikator$Konservativ_v2
indikator <- indikator[!is.na(indikator$ind), ]
indikator <- indikator %>% group_by(PasientID, Aar, AvdRESH) %>%
  summarise(ind = max(ind),
            ForlopsID = ForlopsID[ind==max(ind)][1])
indikator$nevner <- 1
indikator$Index <- 'Ind18'
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator <- indikator[, c("AvdRESH", "Aar", "ind", "nevner", "Index", "AarID")]
Indikatorer <- bind_rows(Indikatorer, indikator)
# Indikatorer <- indikator
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind18', 'Nevner Ind18', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind18')]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator18.", utformat))
nraFigIndikator(plotdata, tittel = c('Andel med tidligere', 'konservativ behandling'), terskel = 5, maal = 90, outfile=outfile)







# Sårinfeksjon innen 30 dager
RegData$Variabel <- pmax(RegData$Komplikasjon, RegData$KomplikasjonT2, na.rm = T)
RegData <- RegData[RegData$ForlopsType1Num == 2, ] # Kun SNM
RegData <- RegData[RegData$ForlopsType2Num %in% c(1,2,5,3), ] # Kun test positiv, test usikker, test negativ
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
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind3', 'Nevner Ind3', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind3')]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator3.", utformat))
nraFigIndikator(plotdata, tittel = c('Andel bekreftet sårinfeksjon innen', '30 dager etter implantasjon'), terskel = 5,
                maal = 4, maalretn = 'lav', decreasing = T, outfile = outfile, desimal = T, xmax=5)

ind3_saarinfeksjon <- indikator
# write.csv2(indikator,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind3_saarinfeksjon.csv', row.names = F)

##############################################

RegData <- RegDataAlle
# RegDataStr9 <- rbind(RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num %in% 1:2), ],
#                      RegData[which(RegData$ForlopsType1Num %in% 3:4),])
# RegDataStr12 <- rbind(RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num %in% 1:2), ],
#                       RegData[which(RegData$ForlopsType1Num %in% 3:4),])
RegDataStr9 <- RegData[which(RegData$StMarksTotalScore>9 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
RegDataStr9 <- dplyr::bind_rows(RegDataStr9, RegData[RegData$KobletForlopsID %in% RegDataStr9$ForlopsID, ])
RegDataStr12 <- RegData[which(RegData$StMarksTotalScore>12 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
RegDataStr12 <- dplyr::bind_rows(RegDataStr12, RegData[RegData$KobletForlopsID %in% RegDataStr12$ForlopsID, ])
# WexDataStr9 <- rbind(RegData[which(RegData$WexnerTotalScore>9 & RegData$ForlopsType1Num %in% 1:2), ],
#                      RegData[which(RegData$ForlopsType1Num %in% 3:4),])
# WexDataStr12 <- rbind(RegData[which(RegData$WexnerTotalScore>12 & RegData$ForlopsType1Num %in% 1:2), ],
#                       RegData[which(RegData$ForlopsType1Num %in% 3:4),])
WexDataStr9 <- RegData[which(RegData$WexnerTotalScore>9 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
WexDataStr9 <- dplyr::bind_rows(WexDataStr9, RegData[RegData$KobletForlopsID %in% WexDataStr9$ForlopsID, ])
WexDataStr12 <- RegData[which(RegData$WexnerTotalScore>12 & RegData$ForlopsType1Num %in% 1:2 & RegData$ForlopsType2Num %in% c(2, NA)), ]
WexDataStr12 <- dplyr::bind_rows(WexDataStr12, RegData[RegData$KobletForlopsID %in% WexDataStr12$ForlopsID, ])
# komboStr9 <- dplyr::bind_rows(RegDataStr9, RegData[which(RegData$WexnerTotalScore>9 & RegData$ForlopsType1Num %in% 1:2), ])
# komboStr12 <- dplyr::bind_rows(RegDataStr12, RegData[which(RegData$WexnerTotalScore>12 & RegData$ForlopsType1Num %in% 1:2), ])
komboStr9 <- dplyr::bind_rows(RegDataStr9, WexDataStr9)
komboStr9 <- komboStr9[match(unique(komboStr9$ForlopsID), komboStr9$ForlopsID), ]
komboStr12 <- dplyr::bind_rows(RegDataStr12, WexDataStr12)
komboStr12 <- komboStr12[match(unique(komboStr12$ForlopsID), komboStr12$ForlopsID), ]

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
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind4', 'Nevner Ind4', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind4')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator4.", utformat))
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=9', '1 år etter operasjon med SNM'), terskel = 5,
                maal = 30, outfile=outfile)

ind4_stmarks_9_1aar_snm <- indikator


Oppfolging <- RegData[RegData$ForlopsType1Num == 4, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind5'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind5', 'Nevner Ind5', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind5')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-5), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator5.", utformat))
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=9', '5 år etter operasjon med SNM'), terskel = 5,
                maal = 30, outfile=outfile)

ind5_stmarks_9_5aar_snm <- indikator
# write.csv2(indikator,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind5_stmarks_9_5aar_snm.csv', row.names = F)

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
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind6', 'Nevner Ind6', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind6')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator6.", utformat))
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=12', '1 år etter operasjon med SNM'), terskel = 5,
                maal = 50, outfile=outfile)

ind6_stmarks_12_1aar_snm <- indikator
# write.csv2(indikator,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind6_stmarks_12_1aar_snm.csv', row.names = F)
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
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind7', 'Nevner Ind7', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind7')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-5), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator7.", utformat))
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=12', '5 år etter operasjon med SNM'), terskel = 5,
                maal = 50, outfile=outfile)

ind7_stmarks_12_5aar_snm <- indikator
# write.csv2(indikator,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind7_stmarks_12_5aar_snm.csv', row.names = F)
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
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind8', 'Nevner Ind8', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind8')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator8.", utformat))
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=9', '1 år etter sfinkterplastikk'), terskel = 5,
                maal = 30, outfile=outfile)

ind8_stmarks_9_1aar_sfinkt <- indikator
# write.csv2(indikator,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind8_stmarks_9_1aar_sfinkt.csv', row.names = F)
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
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind9', 'Nevner Ind9', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind9')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-5), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator9.", utformat))
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=9', '5 år etter sfinkterplastikk'), terskel = 5,
                maal = 30, outfile=outfile)

ind9_stmarks_9_5aar_sfinkt <- indikator
# write.csv2(indikator,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind9_stmarks_9_5aar_sfinkt.csv', row.names = F)
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
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind10', 'Nevner Ind10', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind10')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator10.", utformat))
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=12', '1 år etter sfinkterplastikk'), terskel = 5,
                maal = 50, outfile=outfile)

ind10_stmarks_12_1aar_sfinkt <- indikator
# write.csv2(indikator,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind10_stmarks_12_1aar_sfinkt.csv', row.names = F)
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
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind11', 'Nevner Ind11', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind11')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-5), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator11.", utformat))
nraFigIndikator(plotdata, tittel = c('St. Mark’s Inkontinensskår <=12', '5 år etter sfinkterplastikk'), terskel = 5,
                maal = 50, outfile=outfile)

ind11_stmarks_12_5aar_sfinkt <- indikator
# write.csv2(indikator,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind11_stmarks_12_5aar_sfinkt.csv', row.names = F)
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
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind12', 'Nevner Ind12', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind12')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator12.", utformat))
nraFigIndikator(plotdata, tittel = c('Wexnerskår <=9', '1 år etter operasjon med SNM'), terskel = 5,
                maal = 30, outfile=outfile)

ind12_wexner_9_1aar_snm <- indikator
# write.csv2(indikator,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind12_wexner_9_1aar_snm.csv', row.names = F)
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
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind13', 'Nevner Ind13', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind13')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator13.", utformat))
nraFigIndikator(plotdata, tittel = c('Wexnerskår <=12', '1 år etter operasjon med SNM'), terskel = 5,
                maal = 50, outfile=outfile)

ind13_wexner_12_1aar_snm <- indikator




############ NYTT INKONTINENSSKÅR ##############################################################
################################################################################################

RegData <- komboStr9
RegData$Indikator <- NA
RegData$Indikator[which(RegData$InkontinensScore<=9)] <- 1
RegData$Indikator[which(RegData$InkontinensScore>9)] <- 0

Oppfolging <- RegData[RegData$ForlopsType1Num == 3, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind14'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind14', 'Nevner Ind14', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind14')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator14.", utformat))
nraFigIndikator(plotdata, tittel = c('Inkontinensskår <=9', '1 år etter operasjon med SNM'), terskel = 5,
                maal = 30, outfile=outfile)

RegData <- komboStr9
RegData$Indikator <- NA
RegData$Indikator[which(RegData$InkontinensScore<=9)] <- 1
RegData$Indikator[which(RegData$InkontinensScore>9)] <- 0

Oppfolging <- RegData[RegData$ForlopsType1Num == 3, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind15'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller ind15', 'Nevner Ind15', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller ind15')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator15.", utformat))
nraFigIndikator(plotdata, tittel = c('Inkontinensskår <=9', '1 år etter sfinkterplastikk'), terskel = 5,
                maal = 30, outfile=outfile)


RegData <- komboStr12
RegData$Indikator <- NA
RegData$Indikator[which(RegData$InkontinensScore<=12)] <- 1
RegData$Indikator[which(RegData$InkontinensScore>12)] <- 0

Oppfolging <- RegData[RegData$ForlopsType1Num == 3, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 2], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 2, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind16'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind16', 'Nevner Ind16', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind16')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator16.", utformat))
nraFigIndikator(plotdata, tittel = c('Inkontinensskår <=12', '1 år etter operasjon med SNM'), terskel = 5,
                maal = 50, outfile=outfile)


RegData <- komboStr12
RegData$Indikator <- NA
RegData$Indikator[which(RegData$InkontinensScore<=12)] <- 1
RegData$Indikator[which(RegData$InkontinensScore>12)] <- 0

Oppfolging <- RegData[RegData$ForlopsType1Num == 3, ]
Oppfolging <- Oppfolging[!is.na(Oppfolging$Indikator), ]
Oppfolging <- Oppfolging[Oppfolging$KobletForlopsID %in% RegData$ForlopsID[RegData$ForlopsType1Num == 1], ] # Bare inkluder oppfølginger der det finnes basisreg

indikator <- Oppfolging[, c("AvdRESH", "PasientID", "KobletForlopsID", "Indikator")]
indikator <- merge(indikator, RegData[RegData$ForlopsType1Num == 1, c("ForlopsID", "Aar")], by.x = 'KobletForlopsID', by.y = 'ForlopsID')
indikator$nevner <- 1
indikator$AarID <- paste0(indikator$Aar, indikator$AvdRESH)
indikator$Index <- 'Ind17'
indikator <- indikator[ , c("AvdRESH", "Aar", "Indikator", "nevner", "Index", "AarID")]
names(indikator)[names(indikator) == "Indikator"] <- "ind"
Indikatorer <- bind_rows(Indikatorer, indikator)
names(indikator) <- c('ReshId', 'Aar', 'Teller Ind17', 'Nevner Ind17', 'Indikator', 'AarID')

plotdata <- indikator[, c('ReshId', 'Aar', 'Teller Ind17')]
plotdata <- plotdata[plotdata$Aar <= (rap_aar-1), ]
names(plotdata) <- c('ReshId', 'Aar', 'Teller')
plotdata$SenterKortNavn <- RegData$SenterKortNavn[match(plotdata$ReshId, RegData$AvdRESH)]
outfile <- paste0(figfolder, paste0("indikator17.", utformat))
nraFigIndikator(plotdata, tittel = c('Inkontinensskår <=12', '1 år etter sfinkterplastikk'), terskel = 5,
                maal = 50, outfile=outfile)


################################################################################################
################################################################################################






# write.csv2(indikator,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/indikatorer/ind13_wexner_12_1aar_snm.csv', row.names = F)
##

kobling_resh_shusnavn <- data.frame(ReshID = unique(RegDataAlle$AvdRESH), Sykehusnavn = RegDataAlle$Sykehusnavn[match(unique(RegDataAlle$AvdRESH), RegDataAlle$AvdRESH)])

# write.csv2(kobling_resh_shusnavn,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/kobling_resh_shusnavn.csv', row.names = F)

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

# write.csv2(nokkeltall, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/nokkeltall_nra.csv', row.names = F)

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


# write.csv2(urin_snm, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/urin_snm.csv', row.names = F)
# write.csv2(urin_sfinkter, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/urin_sfinkter.csv', row.names = F)
# write.csv2(gql_snm, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/gql_snm.csv', row.names = F)
# write.csv2(gql_sfinkter, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/gql_sfinkter.csv', row.names = F)
# write.csv2(tilfredshet_1aar, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/tilfredshet_1aar.csv', row.names = F)


############## Data til Sykehusviser ############################################################
#################################################################################################
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

# write.csv2(Indikatorer, "I:/nra/indikatorer_shusviser_nra21102020.csv", row.names = F, fileEncoding = "UTF-8")
write.csv2(Indikatorer, "I:/nra/indikatorer_shusviser_nra09092021.csv", row.names = F, fileEncoding = "UTF-8")

dg_tall <- readxl::read_xlsx("Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Sykehusviser/Dekningsgrader/Dekningsgrad_NRA.xlsx",
                             sheet = "DG_Totalt m utregning")
dg_tall <- dg_tall[,c("Årstall", "ReshID", "Antall i NRA Sfinkter", "Antall i NPR sfinkter", "Antall i NRA SMN", "Antall i NPR SNM")]
dg_tall$ant_tot_nra <- dg_tall[["Antall i NRA SMN"]] + dg_tall[["Antall i NRA Sfinkter"]]
dg_tall$ant_tot_npr <- dg_tall[["Antall i NPR SNM"]] + dg_tall[["Antall i NPR sfinkter"]]

dg_tall_snm <- dg_tall[, c("Årstall", "ReshID", "Antall i NRA SMN", "Antall i NPR SNM")]
names(dg_tall_snm) <- c("year", "resh", "var", "denominator")
dg_tall_snm$ind_id <- "nra_dg_snm"

dg_tall_sfinkter <- dg_tall[, c("Årstall", "ReshID", "Antall i NRA Sfinkter", "Antall i NPR sfinkter")]
names(dg_tall_sfinkter) <- c("year", "resh", "var", "denominator")
dg_tall_sfinkter$ind_id <- "nra_dg_sfinkter"

dg_tall_tot <- dg_tall[, c("Årstall", "ReshID", "ant_tot_nra", "ant_tot_npr")]
names(dg_tall_tot) <- c("year", "resh", "var", "denominator")
dg_tall_tot$ind_id <- "nra_dg_total"

dg_samlet <- bind_rows(bind_rows(dg_tall_snm, dg_tall_sfinkter), dg_tall_tot)

dg_samlet <- dg_samlet[!is.na(dg_samlet$denominator), ]
dg_samlet <- dg_samlet[dg_samlet$resh != 10000, ]

dg_samlet$orgnr <- kobl_resh_orgnr$orgnr[match(dg_samlet$resh, kobl_resh_orgnr$resh)]
dg_samlet <- dg_samlet[!is.na(dg_samlet$denominator), ]
dg_samlet <- dg_samlet[dg_samlet$denominator!=0, ]
dg_samlet <- dg_samlet[,c(1,6,3,4,5)]

write.csv2(dg_samlet, "I:/nra/dg_shusviser27052021.csv", row.names = F, fileEncoding = "UTF-8")


# ind_beskr <- read.table("I:/nra/ind_nra.csv", sep = ";", header = T, fileEncoding = "UTF-8")
#
# ind_beskr$title[ind_beskr$id=="nra_dg_sfinkter"] <- "Dekningsgrad sfinkterplastikk"
# ind_beskr$title[ind_beskr$id=="nra_dg_snm"] <- "Dekningsgrad snm"
# ind_beskr$title[ind_beskr$id=="nra_dg_total"] <- "Dekningsgrad samlet"
#
# write.csv2(ind_beskr, "I:/nra/ind_beskr.csv", row.names = F, fileEncoding = "UTF-8", quote = F)

