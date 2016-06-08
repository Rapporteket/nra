setwd('c:/GIT/nra/doc')
rm(list = ls())

RegData <- read.table('C:/SVN/jasper/nra/data/alleVarNum2016-05-06 09-40-45.txt', header=TRUE, sep=";", encoding = 'UFT-8')
RegData <- RegData[, c('ForlopsID', 'Ukjent', 'AnnenBekkenKirurgi', 'AnnetTraume', 'Hemoroidereksjon', 'NevrologiskSykdom', 'ObsteriskSkade',
                       'PeriferNervskade', 'PerinealAbscess', 'Rectumreseksjon', 'Sfinkterotomi', 'AnnetEtiologi', 'Konservativ',
                       'Irrigasjon', 'Tibialisstimulering', 'AnalInjection', 'SNM', 'Sfinkterplastikk', 'Rectopexi',
                       'KirurgiForRectumprolaps', 'Gracilisplastikk', 'Stomi', 'AnnetTidligereBeh', "SenterKortNavn", "Symtomvarighet",
                       "Ultralyd", "PartiellDefekt", "FullveggsdefektYtreSfinkter", "FullveggsdefektIndreSfinkter", "GenQol",
                       "StMarksTotalScore", "QolSexualitet", "KobletForlopsID", "Tilfredshet", "Urinlekkasje", "Komplikasjon",
                       "KomplikasjonT2", "PostopKomplikasjoner", "Bloedning", "Saarinfeksjon", "Saardehisens", "InkontinensFoerTest",
                       "UrgencyFoerTest", "AvfoeringerFoerTest", "LekkasjedagerFoer", "InkontinensUnderTest", "UrgencyUnderTest",
                       "AvfoeringerUnderTest", "LekkasjedagerUnder")]

ForlopData <- read.table('C:/SVN/jasper/nra/data/ForlopsOversikt2016-05-06 09-40-45.txt', header=TRUE, sep=";", encoding = 'UFT-8')
ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                             'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2')]

RegData <- merge(RegData, ForlopData, by = "ForlopsID")


reshID <- 601225 #  #Må sendes med til funksjon
minald <- 0  #alder, fra og med
maxald <- 130	#alder, til og med
erMann <- 99
datoFra <- '2012-01-01'	 # min og max dato i utvalget vises alltid i figuren.
datoTil <- '2016-01-01'
enhetsUtvalg <- 1 #0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
# valgtVar <- 'Etiologi'
# valgtVar <- 'TidlBeh'
# valgtVar <- 'Tilfredshet'
# valgtVar <- 'Sfinktervurdering'
# valgtVar <- 'PasientAlder'
# valgtVar <- 'Komplikasjon'
# valgtVar <- 'KomplikasjonT2'
# valgtVar <- 'KomplSNMtot'
# valgtVar <- 'KomplSfinkter'
valgtVar <- 'Etiologi'
# valgtVar <- 'SNMdagbok'
outfile <- 'tmp2.pdf'
preprosess<-T
hentData <- F
forlopstype1=''
forlopstype2=''
valgtShus <- '' #c('601225', '700116')

if (outfile == '') {x11()}
tallgrunnlag <- nraFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
              minald=minald, maxald=maxald, erMann=erMann, outfile=outfile,
              reshID=reshID, enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData,
              valgtShus = valgtShus, forlopstype1=forlopstype1, forlopstype2=forlopstype2)


###############  St. Marks osv... ##################################


valgtVar <- 'StMarksTotalScore'
# valgtVar <- 'GenQol'
valgtVar <- 'QolSexualitet'
valgtVar <- 'Urinlekkasje'
sammenlign <- 1

if (outfile == '') {x11()}
nraGjsnPrePost(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil,
               minald=minald, maxald=maxald, erMann=erMann, outfile=outfile,
               reshID=reshID, preprosess=preprosess, hentData=hentData,
               forlopstype1=forlopstype1, forlopstype2=forlopstype2, sammenlign=sammenlign)



############# SNM-dagbok  ################################
enhetsUtvalg <- 2
valgtShus <- c('601225', '700116')
if (outfile == '') {x11()}
nraSNMdagbok(RegData=RegData, datoFra=datoFra, datoTil=datoTil, enhetsUtvalg=enhetsUtvalg, valgtShus = valgtShus,
             outfile = outfile, preprosess=preprosess, minald=minald, maxald=maxald,
             erMann=erMann, reshID=reshID, hentData=hentData, forlopstype1=forlopstype1, forlopstype2=forlopstype2)










###########
table(RegData$KomplikasjonT2[RegData$ForlopsType1Num==2 & RegData$ForlopsType2Num == 2], useNA = 'ifany')
# RegData[which(RegData$QolSexualitet==99), c('ForlopsID', 'PasientID', 'ForlopsType1', 'ForlopsType1Num', "HovedDato")]


tmp <- RegData[RegData$PatientID %in% as.numeric(names(sort(table(RegData$PatientID[!is.na(RegData$Symtomvarighet)], useNA = 'ifany'),
            decreasing = T))[1:15]) & RegData$ForlopsType1Num %in% 1:2, c("PatientID", "HovedDato", "TestSluttDato", "FyllDato1A", "FyllDato1B", "ForlopsType1", "ForlopsType2")]

tmp[order(tmp$PatientID), ]


tmp2 <- RegData[, c("Ultralyd", "PartiellDefekt", "FullveggsdefektYtreSfinkter", "FullveggsdefektIndreSfinkter")]



#
# utforsk <- RegData[which(RegData$KobletForlopsID.x %in% RegData$ForlopsID), ]
# RegData[RegData$PasientID==24, c("PasientID", 'AnnenBekkenKirurgi', 'AnnetTraume', "Hemoroidereksjon",
#                                  "NevrologiskSykdom", "ObsteriskSkade", "PeriferNervskade", "PerinealAbscess", "Rectumreseksjon",
#                                  "Sfinkterotomi")]

# Etiologi <- read.table('C:/SVN/jasper/nra/data/Etiologi2016-04-18 08-37-06.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# SkjemaOversikt <- read.table('C:/SVN/jasper/nra/data/SkjemaOversikt2016-04-18 08-37-06.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# Sykehuskoder <- read.table('C:/SVN/jasper/nra/data/Sykehuskoder2016-04-18 08-37-06.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# TidligereBeh <- read.table('C:/SVN/jasper/nra/data/TidligereBeh2016-04-18 08-37-06.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# GammelData <- read.table('C:/SVN/jasper/nra/data/all_variables2015-10-27 14-38-35.txt', header=TRUE, sep=";", encoding = 'UFT-8')
# write.csv2(RegData, file='nradataforviewing.csv', row.names = FALSE)
