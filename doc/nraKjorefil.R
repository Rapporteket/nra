setwd('c:/GIT/nra/doc')
rm(list = ls())

RegData <- read.table('C:/SVN/jasper/nra/data/alleVarNum2016-04-22 08-17-00.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- read.table('C:/SVN/jasper/nra/data/ForlopsOversikt2016-04-22 08-17-00.txt', header=TRUE, sep=";", encoding = 'UFT-8')
RegData <- RegData[ , -which(names(RegData) %in% c('AvdRESH', "Sykehusnavn", "ForlopsType1", 'ForlopsType1Num', 'ForlopsType2',
                                                   'ForlopsType2Num', 'KobletForlopsID', 'ErOppfolging', 'KryptertFnr', 'PasientKjonn',
                                                   'PasientAlder', 'HovedDato', 'BasisRegStatus'))]
RegData <- merge(RegData, ForlopData, by = "ForlopsID")


reshID <- 601225 #  #Må sendes med til funksjon
minald <- 0  #alder, fra og med
maxald <- 130	#alder, til og med
erMann <- 99
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-12-31'
enhetsUtvalg <- 1 #0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
# valgtVar <- 'Etiologi'
valgtVar <- 'TidlBeh'
# valgtVar <- 'Alder'
outfile <- ''
preprosess<-T
hentData <- F
forlopstype1='2'
forlopstype2=''
valgtShus <- c('')

if (outfile == '') {x11()}
nraFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
              minald=minald, maxald=maxald, erMann=erMann, outfile=outfile,
              reshID=reshID, enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData,
              valgtShus = valgtShus, forlopstype1=forlopstype1, forlopstype2=forlopstype2)













utforsk <- RegData[which(RegData$KobletForlopsID.x %in% RegData$ForlopsID), ]


RegData[RegData$PasientID==24, c("PasientID", 'AnnenBekkenKirurgi', 'AnnetTraume', "Hemoroidereksjon",
                                 "NevrologiskSykdom", "ObsteriskSkade", "PeriferNervskade", "PerinealAbscess", "Rectumreseksjon",
                                 "Sfinkterotomi")]

# Etiologi <- read.table('C:/SVN/jasper/nra/data/Etiologi2016-04-18 08-37-06.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# SkjemaOversikt <- read.table('C:/SVN/jasper/nra/data/SkjemaOversikt2016-04-18 08-37-06.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# Sykehuskoder <- read.table('C:/SVN/jasper/nra/data/Sykehuskoder2016-04-18 08-37-06.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# TidligereBeh <- read.table('C:/SVN/jasper/nra/data/TidligereBeh2016-04-18 08-37-06.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# GammelData <- read.table('C:/SVN/jasper/nra/data/all_variables2015-10-27 14-38-35.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# write.csv2(RegData, file='nradataforviewing.csv', row.names = FALSE)
