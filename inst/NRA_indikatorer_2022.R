library(nra)
library(tidyverse)
rm(list = ls())

allevar <- nra::nraHentTabell("alleVarNum")
foversikt <- nra::nraHentTabell("ForlopsOversikt")
RegData <- merge(
  allevar, foversikt[, c("ForlopsID",
                         names(foversikt)[!(names(foversikt) %in%
                                              intersect(names(allevar), names(foversikt)))])],
                 by = "ForlopsID")
Skjemaoversikt <- nra::nraHentTabell("SkjemaOversikt")
RegData <- nraPreprosess(RegData=RegData)

rap_aar <- 2022
variabler <- c("Andel operert etter standardisert metode" = "Indikator_standardisert",
               "Andel skjema levert innen 4mnd postoperativt" = "Indikator_aktualitet",
               "Prosentvis reduksjon i lekkasjeepisoder >= 50%" = "Indikator1_lekk_red50",
               "Utført ultralyd" = "Ultralyd",
               "Tidligere konservativ behandling" = "tidl_konservativ",
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
               "Inkontinensskår <=9 1 år etter operasjon med SNM" = "nra_inkontinensscore_9_1aar_snm",
               "Inkontinensskår <=12 1 år etter operasjon med SNM" = "nra_inkontinensscore_12_1aar_snm",
               "Inkontinensskår <=9 1 år etter sfinkterplastikk" = "nra_inkontinensscore_9_1aar_sfinkt",
               "Inkontinensskår <=12 1 år etter sfinkterplastikk" = "nra_inkontinensscore_12_1aar_sfinkt",
               "Andel informert om ett års oppfølging" = "andel_inform_oppf",
               "Inkontinensskår <=9 5 år etter operasjon med SNM" = "nra_inkontinensscore_9_5aar_snm",
               "Inkontinensskår <=12 5 år etter operasjon med SNM" = "nra_inkontinensscore_12_5aar_snm",
               "Inkontinensskår <=9 5 år etter sfinkterplastikk" = "nra_inkontinensscore_9_5aar_sfinkt",
               "Inkontinensskår <=12 5 år etter sfinkterplastikk" = "nra_inkontinensscore_12_5aar_sfinkt",
               "nra_reduksjon_4_stmarks_1aar_sfinkt" = "nra_reduksjon_4_stmarks_1aar_sfinkt",
               "nra_reduksjon_4_stmarks_5aar_sfinkt" = "nra_reduksjon_4_stmarks_5aar_sfinkt",
               "nra_reduksjon_4_stmarks_1aar_snm" = "nra_reduksjon_4_stmarks_1aar_snm",
               "nra_reduksjon_4_stmarks_5aar_snm" = "nra_reduksjon_4_stmarks_5aar_snm",
               "nra_reduksjon_40pst_stmarks_1aar_sfinkt" = "nra_reduksjon_40pst_stmarks_1aar_sfinkt",
               "nra_reduksjon_40pst_stmarks_5aar_sfinkt" = "nra_reduksjon_40pst_stmarks_5aar_sfinkt",
               "nra_reduksjon_40pst_stmarks_1aar_snm" = "nra_reduksjon_40pst_stmarks_1aar_snm",
               "nra_reduksjon_40pst_stmarks_5aar_snm" = "nra_reduksjon_40pst_stmarks_5aar_snm"
               )
# ind_aar <- c(rap_aar, rap_aar, rap_aar, rap_aar, rap_aar, rap_aar, rap_aar-1, rap_aar-5, rap_aar-1, rap_aar-5, rap_aar-1,
#              rap_aar-5, rap_aar-1, rap_aar-5, rap_aar-1, rap_aar-1, rap_aar-1, rap_aar-1,
#              rap_aar-1, rap_aar-1, rap_aar, rap_aar-5, rap_aar-5, rap_aar-5, rap_aar-5)
figfolder <- "~/mydata/nra/nra_indikatorer_2022/"
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


nokkeltall <- RegData %>% group_by(Aar) %>%
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

dg_samlet <- read.csv2("~/mydata/nra/ind_imongr_20230623.csv") %>%
  dplyr::filter(substr(ind_id, 1, 6) == "nra_dg") %>%
  dplyr::mutate(var = ifelse(var > denominator, denominator, var))


Indikatorer <- Indikatorer[ , c("year", "orgnr", "var", "denominator", "ind_id", "context")]
Indikatorer <- dplyr::bind_rows(Indikatorer, dg_samlet)

write.csv2(Indikatorer, "~/mydata/nra/nra_ind_2022.csv",
           row.names = F, fileEncoding = "UTF-8")

