library(nra)
library(tidyverse)
rm(list = ls())
hentData <- F

# Data til presentasjon høstmøte ######################################

RegData <- read.table('I:/nra/alleVarNum2018-10-22 09-56-18.txt', header=TRUE, sep=";", encoding = 'UFT-8', stringsAsFactors = F)
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

ForlopData <- read.table('I:/nra/ForlopsOversikt2018-10-22 09-56-19.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                             'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2', "OppflgRegStatus")]

RegData <- merge(RegData, ForlopData, by = "ForlopsID", suffixes = c('', '_2'))
RegData <- nraPreprosess(RegData=RegData)

## Skill ut oppfølginger
Oppfolging1 <- RegData[RegData$ForlopsType1Num == 3, ]

RegData <- RegData[which(RegData$ForlopsType1Num == 2), ]
Oppfolging1 <- Oppfolging1[Oppfolging1$KobletForlopsID %in% RegData$ForlopsID, ]
RegData <- RegData[RegData$ForlopsID %in% Oppfolging1$KobletForlopsID, ]


RegData <- merge(RegData[,c("PasientID", "StMarksTotalScore", "Urinlekkasje", "ForlopsID",
                            "AvdRESH", "SenterKortNavn", "HovedDato")],
                Oppfolging1[,c("StMarksTotalScore", "Urinlekkasje", "KobletForlopsID",
                                "HovedDato")], by.x = 'ForlopsID', by.y = 'KobletForlopsID',
                suffixes = c('Pre', 'Post'))

names(RegData)[names(RegData)=='HovedDatoPre'] <- 'Operasjonsdato'
names(RegData)[names(RegData)=='HovedDatoPost'] <- 'OppfDato'

RegData <- RegData[!(is.na(RegData$StMarksTotalScorePost) & is.na(RegData$UrinlekkasjePost)), ]

write.csv2(RegData, 'StmarksPlussUrin_PrePost.csv')

stmarks <- RegData[!(is.na(RegData$StMarksTotalScorePre) | is.na(RegData$StMarksTotalScorePost)), ]
stmarks$SenterKortNavn[stmarks$AvdRESH!=601225] <- 'Nasjonalt (minus UNN)'

stmarks$endring <- stmarks$StMarksTotalScorePre - stmarks$StMarksTotalScorePost
stmarks$periode <- NA
stmarks$periode[stmarks$Operasjonsdato >= '2016-01-01' & stmarks$Operasjonsdato <= '2017-12-31'] <- 2
stmarks$periode[stmarks$Operasjonsdato >= '2013-01-01' & stmarks$Operasjonsdato <= '2015-12-31'] <- 1
stmarks <- stmarks[stmarks$periode %in% 1:2, ]
# stmarks$periode[stmarks$Operasjonsdato >= '2018-01-01'] <- 3

UNNny_vs_Rest_ny <- stmarks[stmarks$periode==2, ] %>% group_by(SenterKortNavn) %>%
  summarise(gj.sn.for = mean(StMarksTotalScorePre),
            gj.sn.etter = mean(StMarksTotalScorePost),
            sd.for = sd(StMarksTotalScorePre),
            sd.etter = sd(StMarksTotalScorePost),
            endr = mean(endring),
            sd.endr = sd(endring),
            N = n())


UNNny_vs_UNNgml <- stmarks[stmarks$SenterKortNavn=='UNN', ] %>% group_by(periode) %>%
  summarise(gj.sn.for = mean(StMarksTotalScorePre),
            gj.sn.etter = mean(StMarksTotalScorePost),
            sd.for = sd(StMarksTotalScorePre),
            sd.etter = sd(StMarksTotalScorePost),
            endr = mean(endring),
            sd.endr = sd(endring),
            N = n())

Rest_ny_vs_Rest_gml <- stmarks[stmarks$SenterKortNavn!='UNN', ] %>% group_by(periode) %>%
  summarise(gj.sn.for = mean(StMarksTotalScorePre),
            gj.sn.etter = mean(StMarksTotalScorePost),
            sd.for = sd(StMarksTotalScorePre),
            sd.etter = sd(StMarksTotalScorePost),
            endr = mean(endring),
            sd.endr = sd(endring),
            N = n())

# Nasjonal_gml



samlet <- rbind(UNNny_vs_Rest_ny[c(2,1), c("gj.sn.for", "gj.sn.etter", "sd.for", "sd.etter", "endr", "sd.endr", "N")],
                UNNny_vs_UNNgml[c(1), c("gj.sn.for", "gj.sn.etter", "sd.for", "sd.etter", "endr", "sd.endr", "N")],
                Rest_ny_vs_Rest_gml[c(1), c("gj.sn.for", "gj.sn.etter", "sd.for", "sd.etter", "endr", "sd.endr", "N")])

samlet$id <- c('Unn ny prosedyre', 'Resten ny prosedyre', 'UNN gammel prosedyre', 'Resten gammel prosedyre')
samlet <- samlet[, c(8,1:7)]
samlet$KIned <- samlet$endr - qt(.975, samlet$N-1)*samlet$sd.endr/sqrt(samlet$N)
samlet$KIopp <- samlet$endr + qt(.975, samlet$N-1)*samlet$sd.endr/sqrt(samlet$N)
samlet$id <- paste0(samlet$id, ' (N=', samlet$N, ')')

write.csv2(samlet, 'oppsummering_tone.csv', row.names = F)

samlet$id <- wrap.it(samlet$id, 20)

samlet <- samlet[4:1, ]

# x11()
outfile <- 'pres_resultat_tone_22102018.pdf'
FigTypUt <- figtype(outfile, fargepalett='BlaaOff')

# vmarg <- max(0, strwidth(samlet$id, units='figure', cex=1)*0.75)
# par('fig'=c(vmarg, 1, 0, 1))
# par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1+length(tittel)-1)))
par('mar'=c(5.1, 8.1, 4.1, 1.1))

# x11()
ypos <- barplot(samlet$endr, horiz = T, border='white', col = FigTypUt$farger[3], main = c('Endring i St. Marks score', 'før operasjon og til 1-årsoppfølging' ),
                xlim=c(0,max(samlet$KIopp)*1.2), xlab = 'Gj.snittlig endring i St. Marks')
arrows(x0 = samlet$KIned, y0 = ypos, x1 = samlet$KIopp, y1 = ypos,
       length=0.5/max(ypos), code=3, angle=90, lwd=1.3, col='gray')

mtext(samlet$id, side=2, line=0, las=1, at=ypos, col=1)

dev.off()

# ypos <- barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
#                  #font.main=1, cex.main=1.3,
#                  # xlim=c(0,max(andeler, na.rm = T)*1.1),
#                  xlim=c(0,xmax),
#                  names.arg=rep('',dim(andeler)[1]),
#                  horiz=T, axes=F, space=c(0,0.3),
#                  col=soyleFarger, border=NA, xlab = 'Andel (%)')
# arrows(x0 = KI[1,], y0 = ypos, x1 = KI[2,], y1 = ypos,
#        length=0.5/max(ypos), code=3, angle=90, lwd=1.8, col='gray')
#

# aux <- stmarks %>% group_by(StMarksTotalScorePre) %>% summarise(gj.sn.etter = mean(StMarksTotalScorePost),
#                                                                 sd.etter = sd(StMarksTotalScorePost),
#                                                                 endr = mean(endring),
#                                                                 N = n())
# plot(aux$StMarksTotalScorePre, aux$endr)
# plot(stmarks$StMarksTotalScorePre, stmarks$endring)



####### til årsrapport ########################

RegData <- read.table('I:/nra/alleVarNum2018-09-14 08-32-47.txt', header=TRUE, sep=";", encoding = 'UFT-8')
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

ForlopData <- read.table('I:/nra/ForlopsOversikt2018-09-14 08-32-46.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                             'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2', "OppflgRegStatus")]

RegData <- merge(RegData, ForlopData, by = "ForlopsID", suffixes = c('', '_2'))
RegData <- nraPreprosess(RegData=RegData)


RegData2017 <- RegData[RegData$Aar==2017, ]

tilTone <- RegData2017[!is.na(RegData2017$UrgencyFoerTest), ]
tilTone <- tilTone[, c("PasientID", "ForlopsID")]
write.csv2(tilTone, 'pasienter.csv', row.names = F)   ## Liste over pasienter som er inkludert i smndagboktabell


table(RegData2017$ABD65[which(RegData2017$ForlopsType2Num==2)], useNA = 'ifany')
table(RegData2017$ABD652AT2[which(RegData2017$ForlopsType2Num==2)], useNA = 'ifany')


RegData2017[which(RegData2017$ForlopsType2Num==2 & RegData2017$ABD652AT2==0), ]

##
aux <- RegData[which(RegData$Aar %in% 2016:2017 & RegData$ForlopsType1Num==2), ]
# aux2 <- RegData[which(RegData$KobletForlopsID %in% aux$ForlopsID), ]
aux3 <- aux[which(aux$ForlopsID %in% RegData$KobletForlopsID), ]
table(aux3$Aar, useNA = 'ifany')
##
aux <- RegData[which(RegData$Aar %in% 2016:2017 & RegData$ForlopsType1Num==1), ]
# aux2 <- RegData[which(RegData$KobletForlopsID %in% aux$ForlopsID), ]
aux3 <- aux[which(aux$ForlopsID %in% RegData$KobletForlopsID), ]
table(aux3$Aar, useNA = 'ifany')

aux <- RegData[RegData$ForlopsType1Num == 3, ]

aux2 <- merge(aux, RegData, by.x = 'KobletForlopsID', by.y = 'ForlopsID', suffixes = c('', '_pre'))

table(aux$Aar, useNA = 'ifany')
table(aux2$Aar_pre, useNA = 'ifany')

tmp<-RegData2017[which(RegData2017$SenterKortNavn=='Innlandet' & RegData2017$ForlopsType1Num==1), ]


tmp2 <- RegData[which(RegData$KobletForlopsID %in% tmp$ForlopsID), ]
foer <- tmp[which(tmp$ForlopsID %in% tmp2$KobletForlopsID), ]
etter <- tmp2



