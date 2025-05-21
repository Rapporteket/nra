library(nra)
library(tidyverse)
rm(list = ls())

allevar <- nra::nraHentTabell("allevarnum")
foversikt <- nra::nraHentTabell("forlopsoversikt")
RegData <- merge(
  allevar, foversikt[, c("ForlopsID",
                         names(foversikt)[!(names(foversikt) %in%
                                              intersect(names(allevar), names(foversikt)))])],
  by = "ForlopsID")
Skjemaoversikt <- nra::nraHentTabell("skjemaoversikt")
RegData <- nraPreprosess(RegData=RegData)

rap_aar <- 2024
variabler <- c("Andel operert etter standardisert metode" = "Indikator_standardisert",
               "Andel skjema levert innen 4mnd postoperativt" = "Indikator_aktualitet",
               "Andel skjema levert innen 4mnd postoperativt - SNM" = "Indikator_aktualitet_snm",
               "Andel skjema levert innen 4mnd postoperativt - Sfinkterplastikk" = "Indikator_aktualitet_sfinkt",
               "Prosentvis reduksjon i lekkasjeepisoder >= 50%" = "Indikator1_lekk_red50",
               "Utført ultralyd" = "Ultralyd",
               "Utført ultralyd - SNM" = "Ultralyd_snm",
               "Utført ultralyd - Sfinkterplastikk" = "Ultralyd_sfinkt",
               "Tidligere konservativ behandling" = "tidl_konservativ",
               "Tidligere konservativ behandling - SNM" = "tidl_konservativ_snm",
               "Tidligere konservativ behandling - Sfinkterplastikk" = "tidl_konservativ_sfinkt",
               "Bekreftet sårinfeksjon innen 30 dager etter implantasjon" = "saarinfeksjon",
               "St. Mark’s Inkontinensskår <=9 1 år etter operasjon med SNM" = "stmarks_9_1aar_snm",
               "St. Mark’s Inkontinensskår <=9 5 år etter operasjon med SNM" = "stmarks_9_5aar_snm",
               "St. Mark’s Inkontinensskår <=12 1 år etter operasjon med SNM" = "stmarks_12_1aar_snm",
               "St. Mark’s Inkontinensskår <=12 5 år etter operasjon med SNM" = "stmarks_12_5aar_snm",
               "St. Mark’s Inkontinensskår <=9 1 år etter sfinkterplastikk" = "stmarks_9_1aar_sfinkt",
               "St. Mark’s Inkontinensskår <=9 5 år etter sfinkterplastikk" = "stmarks_9_5aar_sfinkt",
               "St. Mark’s Inkontinensskår <=12 1 år etter sfinkterplastikk" = "stmarks_12_1aar_sfinkt",
               "St. Mark’s Inkontinensskår <=12 5 år etter sfinkterplastikk" = "stmarks_12_5aar_sfinkt",
               "Wexnerskår <=9 1 år etter operasjon med SNM" = "wexner_9_1aar_snm",
               "Wexnerskår <=12 1 år etter operasjon med SNM" = "wexner_12_1aar_snm",
               "Wexnerskår <=9 5 år etter operasjon med SNM" = "wexner_9_5aar_snm",
               "Wexnerskår <=12 5 år etter operasjon med SNM" = "wexner_12_5aar_snm",
               "Wexnerskår <=9 1 år etter sfinkterplastikk" = "wexner_9_1aar_sfinkt",
               "Wexnerskår <=12 1 år etter sfinkterplastikk" = "wexner_12_1aar_sfinkt",
               # "Wexnerskår <=9 5 år etter sfinkterplastikk" = "wexner_9_5aar_sfinkt",
               # "Wexnerskår <=12 5 år etter sfinkterplastikk" = "wexner_12_5aar_sfinkt",
               "Inkontinensskår <=9 1 år etter operasjon med SNM" = "nra_inkontinensscore_9_1aar_snm",
               "Inkontinensskår <=12 1 år etter operasjon med SNM" = "nra_inkontinensscore_12_1aar_snm",
               "Inkontinensskår <=9 1 år etter sfinkterplastikk" = "nra_inkontinensscore_9_1aar_sfinkt",
               "Inkontinensskår <=12 1 år etter sfinkterplastikk" = "nra_inkontinensscore_12_1aar_sfinkt",
               "Inkontinensskår <=9 5 år etter operasjon med SNM" = "nra_inkontinensscore_9_5aar_snm",
               "Inkontinensskår <=12 5 år etter operasjon med SNM" = "nra_inkontinensscore_12_5aar_snm",
               "Inkontinensskår <=9 5 år etter sfinkterplastikk" = "nra_inkontinensscore_9_5aar_sfinkt",
               "Inkontinensskår <=12 5 år etter sfinkterplastikk" = "nra_inkontinensscore_12_5aar_sfinkt",
               "Andel informert om ett års oppfølging" = "andel_inform_oppf",
               "Andel informert om ett års oppfølging - SNM" = "andel_inform_oppf_snm",
               "Andel informert om ett års oppfølging - Sfinkterplastikk" = "andel_inform_oppf_sfinkt",
               "nra_reduksjon_4_stmarks_1aar_sfinkt" = "nra_reduksjon_4_stmarks_1aar_sfinkt",
               "nra_reduksjon_4_stmarks_5aar_sfinkt" = "nra_reduksjon_4_stmarks_5aar_sfinkt",
               "nra_reduksjon_4_stmarks_1aar_snm" = "nra_reduksjon_4_stmarks_1aar_snm",
               "nra_reduksjon_4_stmarks_5aar_snm" = "nra_reduksjon_4_stmarks_5aar_snm",
               "nra_reduksjon_40pst_stmarks_1aar_sfinkt" = "nra_reduksjon_40pst_stmarks_1aar_sfinkt",
               "nra_reduksjon_40pst_stmarks_5aar_sfinkt" = "nra_reduksjon_40pst_stmarks_5aar_sfinkt",
               "nra_reduksjon_40pst_stmarks_1aar_snm" = "nra_reduksjon_40pst_stmarks_1aar_snm",
               "nra_reduksjon_40pst_stmarks_5aar_snm" = "nra_reduksjon_40pst_stmarks_5aar_snm",
               "St. Mark’s Inkontinensskår <=9 1 år etter operasjon med SNM - alle" = "stmarks_9_1aar_snm_v2",
               "St. Mark’s Inkontinensskår <=9 5 år etter operasjon med SNM - alle" = "stmarks_9_5aar_snm_v2",
               "St. Mark’s Inkontinensskår <=12 1 år etter operasjon med SNM - alle" = "stmarks_12_1aar_snm_v2",
               "St. Mark’s Inkontinensskår <=12 5 år etter operasjon med SNM - alle" = "stmarks_12_5aar_snm_v2",
               "St. Mark’s Inkontinensskår <=9 1 år etter sfinkterplastikk - alle" = "stmarks_9_1aar_sfinkt_v2",
               "St. Mark’s Inkontinensskår <=9 5 år etter sfinkterplastikk - alle" = "stmarks_9_5aar_sfinkt_v2",
               "St. Mark’s Inkontinensskår <=12 1 år etter sfinkterplastikk - alle" = "stmarks_12_1aar_sfinkt_v2",
               "St. Mark’s Inkontinensskår <=12 5 år etter sfinkterplastikk - alle" = "stmarks_12_5aar_sfinkt_v2",
               "Wexnerskår <=9 1 år etter operasjon med SNM - alle" = "wexner_9_1aar_snm_v2",
               "Wexnerskår <=12 1 år etter operasjon med SNM - alle" = "wexner_12_1aar_snm_v2",
               "Wexnerskår <=9 5 år etter operasjon med SNM - alle" = "wexner_9_5aar_snm_v2",
               "Wexnerskår <=12 5 år etter operasjon med SNM - alle" = "wexner_12_5aar_snm_v2",
               "Wexnerskår <=9 1 år etter sfinkterplastikk - alle" = "wexner_9_1aar_sfinkt_v2",
               "Wexnerskår <=12 1 år etter sfinkterplastikk - alle" = "wexner_12_1aar_sfinkt_v2",
               # "Wexnerskår <=9 5 år etter sfinkterplastikk - alle" = "wexner_9_5aar_sfinkt_v2",
               # "Wexnerskår <=12 5 år etter sfinkterplastikk - alle" = "wexner_12_5aar_sfinkt_v2",
               "Inkontinensskår <=9 1 år etter operasjon med SNM - alle" = "nra_inkontinensscore_9_1aar_snm_v2",
               "Inkontinensskår <=12 1 år etter operasjon med SNM - alle" = "nra_inkontinensscore_12_1aar_snm_v2",
               "Inkontinensskår <=9 1 år etter sfinkterplastikk - alle" = "nra_inkontinensscore_9_1aar_sfinkt_v2",
               "Inkontinensskår <=12 1 år etter sfinkterplastikk - alle" = "nra_inkontinensscore_12_1aar_sfinkt_v2",
               "Inkontinensskår <=9 5 år etter operasjon med SNM - alle" = "nra_inkontinensscore_9_5aar_snm_v2",
               "Inkontinensskår <=12 5 år etter operasjon med SNM - alle" = "nra_inkontinensscore_12_5aar_snm_v2",
               "Inkontinensskår <=9 5 år etter sfinkterplastikk - alle" = "nra_inkontinensscore_9_5aar_sfinkt_v2",
               "Inkontinensskår <=12 5 år etter sfinkterplastikk - alle" = "nra_inkontinensscore_12_5aar_sfinkt_v2"
)

figfolder <- "C:/Users/ibo600/OneDrive - Helse Nord RHF/SKDE/Register/nra/aarsrapport_2024/indikatorer_2024/"
if (!dir.exists(figfolder)) {
  dir.create(figfolder)
}


Indikatorer <- data.frame(year=numeric(), AvdRESH=character(), var=numeric(), denominator=numeric(), ind_id=character(),
                          orgnr=numeric(), SenterKortNavn=character(), context=character())

for (p in 1:length(variabler)){
  indikatordata <- nra::nraBeregnIndikator(RegData=RegData, valgtVar = variabler[p])
  TabellData <- indikatordata$indikator
  TabellData <- TabellData[which(TabellData$year <= rap_aar), ]
  Indikatorer <- dplyr::bind_rows(Indikatorer, TabellData)
  plotdata <- TabellData[, c('AvdRESH', 'year', 'var', "SenterKortNavn")]
  outfile <- paste0(figfolder, variabler[p], ".svg")

  nra::nraFigIndikator_v4(plotdata, tittel = indikatordata$tittel,
                          terskel = indikatordata$terskel, maal = indikatordata$maal,
                          minstekrav = indikatordata$minstekrav,
                          maalretn = indikatordata$maalRetn, xmax = indikatordata$xmax,
                          decreasing =indikatordata$decreasing, outfile=outfile)
}


Indikatorer <- Indikatorer %>%
  dplyr::filter(ind_id %in% c("nra_aktualitet", "nra_tidl_konservativ",
                              "nra_ultralyd", "nra_standardisert",
                              "nra_50pst_lekkasjeredusjon", "nra_saarinfeksjon",
                              "nra_inkontinensscore_9_1aar_snm", "nra_inkontinensscore_12_1aar_snm",
                              "nra_inkontinensscore_9_1aar_sfinkt", "nra_inkontinensscore_12_1aar_sfinkt",
                              "nra_inkontinensscore_9_5aar_snm", "nra_inkontinensscore_12_5aar_snm",
                              "nra_inkontinensscore_9_5aar_sfinkt", "nra_inkontinensscore_12_5aar_sfinkt",
                              "nra_inform_oppf"))

################## NØKKELTALL ######################################
library(tidyverse)

nokkeltall <- RegData %>%
  group_by(Aar) %>%
  filter(!is.na(Aar)) %>% # alle Aar registrert som NA tas ut
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

### For å eksportere nøkkeltall til excel-fil
# Bibliotek:
install.packages("openxlsx")
library(openxlsx)

# Eksport:
write.xlsx(nokkeltall, file = "C:/Users/ibo600/OneDrive - Helse Nord RHF/SKDE/Register/nra/nokkeltall.xlsx", asTable = TRUE)



dg_samlet <- read.csv2("~/mydata/nra/ind_imongr_20230623.csv") %>%
  dplyr::filter(substr(ind_id, 1, 6) == "nra_dg") %>%
  dplyr::mutate(var = ifelse(var > denominator, denominator, var))

kobl_resh_orgnr <- data.frame(resh = c(601225, 108162, 107440, 700116, 700922,
                                       111138, 107505, 4210588, 601233,
                                       114271),
                              orgnr = c(974795787, 974706490, 974749025,
                                        983971768, 974557746, 974724960,
                                        974116804, 974733013, 974795396,
                                        974703300),
                              shus = c("UNN", "Akershus", "St.Olav", "Østfold",
                                       "Haukeland", "Innlandet", "DS",
                                       "Kristiansand", "UNN Narvik",
                                       "Stavanger"))
# dg_2020_21 <- read.csv2(
#   "~/mydata/nra/DGA_begge_operasjonstyper_hf_aar_2020_2021_mRESHID.csv",
#   fileEncoding = "latin1") %>%
#   dplyr::filter(!is.na(AvdRESH))
# dg_2020_21$var <- dg_2020_21$Begge + dg_2020_21$Kun_NRA
# dg_2020_21$orgnr <- kobl_resh_orgnr$orgnr[match(dg_2020_21$AvdRESH, kobl_resh_orgnr$resh)]
# dg_2020_21 <- dg_2020_21 %>%
#   dplyr::rename(denominator = Total,
#                 year = aar) %>%
#   dplyr::mutate(context = "caregiver",
#                 ind_id = "nra_dg_total") %>%
#   dplyr::select(context, orgnr, year, var, denominator, ind_id)
#
# dg_samlet <- dplyr::bind_rows(dg_samlet, dg_2020_21)
#
# dg_2020_21 <- read.csv2(
#   "~/mydata/nra/DGA_JHC10_K628_hf_aar_2020_2021_mRESHID.csv",
#   fileEncoding = "latin1") %>%
#   dplyr::filter(!is.na(AvdRESH))
# dg_2020_21$var <- dg_2020_21$Begge + dg_2020_21$Kun_NRA
# dg_2020_21$orgnr <- kobl_resh_orgnr$orgnr[match(dg_2020_21$AvdRESH, kobl_resh_orgnr$resh)]
# dg_2020_21 <- dg_2020_21 %>%
#   dplyr::rename(denominator = Total,
#                 year = aar) %>%
#   dplyr::mutate(context = "caregiver",
#                 ind_id = "nra_dg_sfinkter") %>%
#   dplyr::select(context, orgnr, year, var, denominator, ind_id)
#
# dg_samlet <- dplyr::bind_rows(dg_samlet, dg_2020_21)
#
# dg_2020_21 <- read.csv2(
#   "~/mydata/nra/DGA_SMN_inkl_AEA20_AEA24_hf_aar_2020_2021_mRESHID.csv",
#   fileEncoding = "latin1") %>%
#   dplyr::filter(!is.na(AvdRESH))
# dg_2020_21$var <- dg_2020_21$Begge + dg_2020_21$Kun_NRA
# dg_2020_21$orgnr <- kobl_resh_orgnr$orgnr[match(dg_2020_21$AvdRESH, kobl_resh_orgnr$resh)]
# dg_2020_21 <- dg_2020_21 %>%
#   dplyr::rename(denominator = Total,
#                 year = aar) %>%
#   dplyr::mutate(context = "caregiver",
#                 ind_id = "nra_dg_snm") %>%
#   dplyr::select(context, orgnr, year, var, denominator, ind_id)
#
# dg_samlet <- dplyr::bind_rows(dg_samlet, dg_2020_21)

Indikatorer <- Indikatorer[ , c("year", "orgnr", "var", "denominator", "ind_id", "context")]
# Indikatorer <- dplyr::bind_rows(Indikatorer, dg_samlet)


write.csv2(Indikatorer, paste0("~/mydata/nra/nra_indikatorer_", Sys.Date(), ".csv"),
           row.names = F, fileEncoding = "UTF-8")

