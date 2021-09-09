library(nra)
library(tidyverse)
rm(list = ls())
hentData <- F

RegData <- read.table('I:/nra/alleVarNum2021-09-02 10-37-34.txt', header=TRUE, sep=";", encoding = 'UTF-8')
# RegData <- RegData[, c('ForlopsID', 'Ukjent', 'AnnenBekkenKirurgi', 'AnnetTraume', 'Hemoroidereksjon', 'NevrologiskSykdom', 'ObsteriskSkade',
#                        'PeriferNervskade', 'PerinealAbscess', 'Rectumreseksjon', 'Sfinkterotomi', 'AnnetEtiologi', 'Konservativ',
#                        'Irrigasjon', 'Tibialisstimulering', 'AnalInjection', 'SNM', 'Sfinkterplastikk', 'Rectopexi',
#                        'KirurgiForRectumprolaps', 'Gracilisplastikk', 'Stomi', 'AnnetTidligereBeh', "SenterKortNavn", "Symtomvarighet",
#                        "Ultralyd", "PartiellDefekt", "FullveggsdefektYtreSfinkter", "FullveggsdefektIndreSfinkter", "GenQol",
#                        "StMarksTotalScore", "QolSexualitet", "KobletForlopsID", "Tilfredshet", "Urinlekkasje", "Komplikasjon",
#                        "KomplikasjonT2", "PostopKomplikasjoner", "Bloedning", "Saarinfeksjon", "Saardehisens", "InkontinensFoerTest",
#                        "UrgencyFoerTest", "AvfoeringerFoerTest", "LekkasjedagerFoer", "InkontinensUnderTest", "UrgencyUnderTest",
#                        "AvfoeringerUnderTest", "LekkasjedagerUnder", 'OppfoelgingMulig',
#                        'ABD65', 'ABD652AT2','ABD60', "WexFastAvfoering", "WexBind", "WexFlytendeAvfoering", "WexLuft",
#                        "WexLivsstilsendring", "WexnerTotalScore", "Onestage", "Testprosedyre")]

ForlopData <- read.table('I:/nra/ForlopsOversikt2021-09-02 10-37-34.txt', header=TRUE, sep=";", encoding = 'UTF-8')
ForlopData <- ForlopData[, c('ForlopsID', 'HovedDato','PasientAlder', 'PasientID', 'AvdRESH', 'Sykehusnavn', 'ForlopsType1Num',
                             'ForlopsType2Num', 'ErMann', 'ForlopsType1', 'ForlopsType2', "OppflgRegStatus")]

RegData <- merge(RegData, ForlopData, by = "ForlopsID", suffixes = c('', '_2'))
RegData <- nraPreprosess(RegData=RegData)

rap_aar <- 2020

RegData_pre <- RegData[RegData$ForlopsType1Num %in% 1:2, ]

RegData_forlop_1 <- merge(RegData_pre, RegData[RegData$ForlopsType1Num %in% 3, ], by.x = "ForlopsID",
                          by.y = "KobletForlopsID", all.x = T, suffixes = c('', '_oppf'))

# St. Marks og Wexner

RegData_forlop_1$beggemangler <- (is.na(RegData_forlop_1$StMarksTotalScore) & is.na(RegData_forlop_1$WexnerTotalScore)) &
  (is.na(RegData_forlop_1$StMarksTotalScore_oppf) & is.na(RegData_forlop_1$WexnerTotalScore_oppf))
RegData_forlop_1$premangler_postutfylt <- (is.na(RegData_forlop_1$StMarksTotalScore) & is.na(RegData_forlop_1$WexnerTotalScore)) &
  (!is.na(RegData_forlop_1$StMarksTotalScore_oppf) | !is.na(RegData_forlop_1$WexnerTotalScore_oppf))
RegData_forlop_1$preutfylt_postmangler <- (!is.na(RegData_forlop_1$StMarksTotalScore) | !is.na(RegData_forlop_1$WexnerTotalScore)) &
  (is.na(RegData_forlop_1$StMarksTotalScore_oppf) & is.na(RegData_forlop_1$WexnerTotalScore_oppf))
RegData_forlop_1$beggeutfylt <- (!is.na(RegData_forlop_1$StMarksTotalScore) | !is.na(RegData_forlop_1$WexnerTotalScore)) &
  (!is.na(RegData_forlop_1$StMarksTotalScore_oppf) | !is.na(RegData_forlop_1$WexnerTotalScore_oppf))
RegData_forlop_1$ForlopsType2[RegData_forlop_1$ForlopsType1 == "Sfinkterplastikk"] <- "Sfinkterplastikk"

oppsum <- RegData_forlop_1 %>% group_by(Aar, SenterKortNavn, ForlopsType2) %>%
  summarise(beggemangler = sum(beggemangler),
            premangler_postutfylt = sum(premangler_postutfylt),
            preutfylt_postmangler = sum(preutfylt_postmangler),
            beggeutfylt = sum(beggeutfylt),
            ant_oppfskjema = sum(!is.na(ForlopsType1_oppf)),
            N=n())


utprint <- oppsum[oppsum$Aar %in% (rap_aar-1):(rap_aar-1), ]
write.csv2(utprint, "doc/kompletthetStMarksWexnerKombo.csv", row.names = F, fileEncoding = 'Latin1')

# Urinlekkasje

RegData_forlop_1$beggemangler <- is.na(RegData_forlop_1$Urinlekkasje_v2) & is.na(RegData_forlop_1$Urinlekkasje_v2_oppf)
RegData_forlop_1$premangler_postutfylt <- is.na(RegData_forlop_1$Urinlekkasje_v2) & !is.na(RegData_forlop_1$Urinlekkasje_v2_oppf)
RegData_forlop_1$preutfylt_postmangler <- !is.na(RegData_forlop_1$Urinlekkasje_v2) & is.na(RegData_forlop_1$Urinlekkasje_v2_oppf)
RegData_forlop_1$beggeutfylt <- !is.na(RegData_forlop_1$Urinlekkasje_v2) & !is.na(RegData_forlop_1$Urinlekkasje_v2_oppf)

oppsum <- RegData_forlop_1 %>% group_by(Aar, SenterKortNavn, ForlopsType2) %>%
  summarise(beggemangler = sum(beggemangler),
            premangler_postutfylt = sum(premangler_postutfylt),
            preutfylt_postmangler = sum(preutfylt_postmangler),
            beggeutfylt = sum(beggeutfylt),
            ant_oppfskjema = sum(!is.na(ForlopsType1_oppf)),
            N=n())

utprint <- oppsum[oppsum$Aar %in% (rap_aar-1):(rap_aar-1), ]
write.csv2(utprint, "doc/kompletthetUrinlekkasje.csv", row.names = F, fileEncoding = 'Latin1')

# Generell livskvalitet

RegData_forlop_1$beggemangler <- is.na(RegData_forlop_1$GenQol) & is.na(RegData_forlop_1$GenQol_oppf)
RegData_forlop_1$premangler_postutfylt <- is.na(RegData_forlop_1$GenQol) & !is.na(RegData_forlop_1$GenQol_oppf)
RegData_forlop_1$preutfylt_postmangler <- !is.na(RegData_forlop_1$GenQol) & is.na(RegData_forlop_1$GenQol_oppf)
RegData_forlop_1$beggeutfylt <- !is.na(RegData_forlop_1$GenQol) & !is.na(RegData_forlop_1$GenQol_oppf)

oppsum <- RegData_forlop_1 %>% group_by(Aar, SenterKortNavn, ForlopsType2) %>%
  summarise(beggemangler = sum(beggemangler),
            premangler_postutfylt = sum(premangler_postutfylt),
            preutfylt_postmangler = sum(preutfylt_postmangler),
            beggeutfylt = sum(beggeutfylt),
            ant_oppfskjema = sum(!is.na(ForlopsType1_oppf)),
            N=n())

utprint <- oppsum[oppsum$Aar %in% (rap_aar-1):(rap_aar-1), ]
write.csv2(utprint, "doc/kompletthetGenQol.csv", row.names = F, fileEncoding = 'Latin1')

# Seksuell livskvalitet

RegData_forlop_1$beggemangler <- is.na(RegData_forlop_1$QolSexualitet) & is.na(RegData_forlop_1$QolSexualitet_oppf)
RegData_forlop_1$premangler_postutfylt <- is.na(RegData_forlop_1$QolSexualitet) & !is.na(RegData_forlop_1$QolSexualitet_oppf)
RegData_forlop_1$preutfylt_postmangler <- !is.na(RegData_forlop_1$QolSexualitet) & is.na(RegData_forlop_1$QolSexualitet_oppf)
RegData_forlop_1$beggeutfylt <- !is.na(RegData_forlop_1$QolSexualitet) & !is.na(RegData_forlop_1$QolSexualitet_oppf)

oppsum <- RegData_forlop_1 %>% group_by(Aar, SenterKortNavn, ForlopsType2) %>%
  summarise(beggemangler = sum(beggemangler),
            premangler_postutfylt = sum(premangler_postutfylt),
            preutfylt_postmangler = sum(preutfylt_postmangler),
            beggeutfylt = sum(beggeutfylt),
            ant_oppfskjema = sum(!is.na(ForlopsType1_oppf)),
            N=n())

utprint <- oppsum[oppsum$Aar %in% (rap_aar-1):(rap_aar-1), ]
write.csv2(utprint, "doc/kompletthetQolSexualitet.csv", row.names = F, fileEncoding = 'Latin1')


# Tilfredshet med behandlingstilbud

oppsum <- RegData_forlop_1 %>% group_by(Aar, SenterKortNavn, ForlopsType2) %>%
  summarise(Tilfredshet_utfylt = sum(!is.na(Tilfredshet_oppf)),
            ant_oppfskjema = sum(!is.na(ForlopsType1_oppf)),
            N=n())

utprint <- oppsum[oppsum$Aar %in% (rap_aar-1):(rap_aar-1), ]
write.csv2(utprint, "doc/tilfredshet.csv", row.names = F, fileEncoding = 'Latin1')

# Endoanal ultralyd

oppsum <- RegData_forlop_1 %>% group_by(Aar, SenterKortNavn) %>%
  summarise(ultralyd_utfylt = sum(!is.na(Ultralyd)),
            N=n())
utprint <- oppsum[oppsum$Aar %in% rap_aar, ]
write.csv2(utprint, "doc/Ultralyd.csv", row.names = F, fileEncoding = 'Latin1')


# SNM dagbok
oppsum <- RegData_forlop_1[RegData_forlop_1$ForlopsType1Num==2, ] %>% group_by(Aar, SenterKortNavn) %>%
  summarise(snmdagbok_utfylt = sum(!is.na(InkontinensUnderTest)),
            N=n())
utprint <- oppsum[oppsum$Aar %in% rap_aar, ]
write.csv2(utprint, "doc/snmdagbok.csv", row.names = F, fileEncoding = 'Latin1')

## Komplikasjoner SNM
RegData_forlop_1$Variabel <- pmax(RegData_forlop_1$Komplikasjon, RegData_forlop_1$KomplikasjonT2, na.rm = T)
RegData_forlop_1$Variabel[which(RegData_forlop_1$Variabel==9 & (RegData_forlop_1$Komplikasjon==2 | RegData_forlop_1$KomplikasjonT2==2))] <- 2   # Velg bekreftet eller mistenkt
RegData_forlop_1$Variabel[which(RegData_forlop_1$Variabel==9 & (RegData_forlop_1$Komplikasjon==1 | RegData_forlop_1$KomplikasjonT2==1))] <- 1   # sårinfeksjon fremfor annet
RegData_forlop_1$Variabel[which(RegData_forlop_1$Variabel==98 & (RegData_forlop_1$Komplikasjon==2 | RegData_forlop_1$KomplikasjonT2==2))] <- 2   # Velg bekreftet eller mistenkt
RegData_forlop_1$Variabel[which(RegData_forlop_1$Variabel==98 & (RegData_forlop_1$Komplikasjon==1 | RegData_forlop_1$KomplikasjonT2==1))] <- 1

oppsum <- RegData_forlop_1[RegData_forlop_1$ForlopsType1Num==2, ] %>% group_by(Aar, SenterKortNavn) %>%
  summarise(Kompl_snm_utfylt = sum(!is.na(Variabel) & Variabel != 98),
            N=n())
utprint <- oppsum[oppsum$Aar %in% (rap_aar-1):rap_aar, ]
write.csv2(utprint, "doc/komplikasjoner_snm.csv", row.names = F, fileEncoding = 'Latin1')

# Symptomvarighet

oppsum <- RegData_forlop_1 %>% group_by(Aar, SenterKortNavn) %>%
  summarise(symptomvarighet_utfylt = sum(!is.na(Symtomvarighet)),
            N=n())
utprint <- oppsum[oppsum$Aar %in% (rap_aar-1):rap_aar, ]
write.csv2(utprint, "doc/Symtomvarighet.csv", row.names = F, fileEncoding = 'Latin1')


# Postmenopausal

oppsum <- RegData_forlop_1 %>% group_by(Aar, SenterKortNavn) %>%
  summarise(symptomvarighet_utfylt = sum(!is.na(Postmenopausal)),
            N=n())
utprint <- oppsum[oppsum$Aar %in% (rap_aar-1):rap_aar, ]
write.csv2(utprint, "doc/Postmenopausal.csv", row.names = F, fileEncoding = 'Latin1')

# Inkontinensscore Pre: Dekkes ikke dette av snm-dagbok?
# Elektrodeplassering: Hvilke variabler
# Anestesi: Hvordan skal det telles?
# !is.na(RegData_pre$Anestesi2A) | !is.na(RegData_pre$AnestesiT2) | !is.na(RegData_pre$Anestesi2B)
# Antatt etiologi: Hvordan kombinere variablene
# Tidligere behandling: Hvordan kombinere variablene







