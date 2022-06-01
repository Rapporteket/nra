library(nra)
library(tidyverse)
rm(list = ls())

# RegData <- nra::nraHentRegData()
allevar <- nra::nraHentTabell("alleVarNum")
foversikt <- nra::nraHentTabell("ForlopsOversikt")
RegData <- merge(allevar, foversikt[, c("ForlopsID", names(foversikt)[!(names(foversikt) %in% intersect(names(allevar), names(foversikt)))])],
                 by = "ForlopsID")
Skjemaoversikt <- nra::nraHentTabell("SkjemaOversikt")
RegData <- nraPreprosess(RegData=RegData)
# RegData$SenterKortNavn <- paste0(RegData$SenterKortNavn, ' ')

rap_aar <- 2021
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
               "Inkontinensskår <=9 1 år etter operasjon med SNM V2" = "nra_inkontinensscore_9_1aar_snm_v2",
               "Inkontinensskår <=12 1 år etter operasjon med SNM" = "nra_inkontinensscore_12_1aar_snm",
               "Inkontinensskår <=9 1 år etter sfinkterplastikk" = "nra_inkontinensscore_9_1aar_sfinkt",
               "Inkontinensskår <=12 1 år etter sfinkterplastikk" = "nra_inkontinensscore_12_1aar_sfinkt",
               "Andel informert om ett års oppfølging" = "andel_inform_oppf")
ind_aar <- c(rap_aar, rap_aar, rap_aar, rap_aar, rap_aar, rap_aar, rap_aar-1, rap_aar-5, rap_aar-1, rap_aar-5, rap_aar-1,
             rap_aar-5, rap_aar-1, rap_aar-5, rap_aar-1, rap_aar-1, rap_aar-1,rap_aar-1, rap_aar-1,
             rap_aar-1, rap_aar-1, rap_aar)
figfolder <- "~/.ssh/nra/indikatorer_2021_v4/"
if (!dir.exists(figfolder)) {
  dir.create(figfolder)
}

Indikatorer <- data.frame(year=numeric(), AvdRESH=character(), var=numeric(), denominator=numeric(), ind_id=character(),
                          orgnr=numeric(), SenterKortNavn=character(), context=character())

for (p in 1:length(ind_aar)){
  indikatordata <- nra::nraBeregnIndikator(RegData=RegData, valgtVar = variabler[p])
  TabellData <- indikatordata$indikator
  TabellData <- TabellData[which(TabellData$year <= ind_aar[p]), ]
  Indikatorer <- dplyr::bind_rows(Indikatorer, TabellData)
  plotdata <- TabellData[, c('AvdRESH', 'year', 'var', "SenterKortNavn")]
  outfile <- paste0(figfolder, variabler[p], ".svg")

  nra::nraFigIndikator_v4(plotdata, tittel = indikatordata$tittel,
                          terskel = indikatordata$terskel, maal = indikatordata$maal,
                          minstekrav = indikatordata$minstekrav,
                          maalretn = indikatordata$maalRetn, xmax = indikatordata$xmax,
                          decreasing =indikatordata$decreasing, outfile=outfile)
}

p <- 21
indikatordata <- nra::nraBeregnIndikator(RegData=RegData, valgtVar = variabler[p])
TabellData <- indikatordata$indikator
TabellData <- TabellData[which(TabellData$year <= ind_aar[p]), ]
Indikatorer <- dplyr::bind_rows(Indikatorer, TabellData)
plotdata <- TabellData[, c('AvdRESH', 'year', 'var', "SenterKortNavn")]
outfile <- ""

nra::nraFigIndikator_v4(plotdata, tittel = indikatordata$tittel,
                        terskel = indikatordata$terskel, maal = indikatordata$maal,
                        minstekrav = indikatordata$minstekrav,
                        maalretn = indikatordata$maalRetn, xmax = indikatordata$xmax,
                        decreasing =indikatordata$decreasing, outfile=outfile)




# write.csv2(kobling_resh_shusnavn,
#            'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/kobling_resh_shusnavn.csv', row.names = F)

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

# write.csv2(nokkeltall, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/2. NRA/nokkeltall_nra.csv', row.names = F)
# write.csv2(Indikatorer, "I:/nra/indikatorer_shusviser_nra09092021.csv", row.names = F, fileEncoding = "UTF-8")

dg_tall <- readxl::read_xlsx("~/.ssh/nra/Dekningsgrad_NRA.xlsx",
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

dg_samlet$orgnr <- Indikatorer$orgnr[match(dg_samlet$resh, Indikatorer$AvdRESH)]


dg_samlet <- dg_samlet[!is.na(dg_samlet$denominator), ]
dg_samlet <- dg_samlet[dg_samlet$denominator!=0, ]
dg_samlet <- dg_samlet[,c(1,6,3,4,5)]

# write.csv2(dg_samlet, "I:/nra/dg_shusviser27052021.csv", row.names = F, fileEncoding = "UTF-8")


