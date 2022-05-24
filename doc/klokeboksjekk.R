library(nra)
library(tidyverse)
rm(list = ls())

RegData <- nra::nraHentRegData()
RegData <- nraPreprosess(RegData=RegData)

allevarnum <- nraHentTabell("alleVarNum")
forlopsoversikt <- nraHentTabell("ForlopsOversikt")

names(allevarnum)
names(forlopsoversikt)

klokebok <- readr::read_csv2("~/.ssh/nra/Norsk_Register_for_Analinkontinens_klokeboken_10.05.2022.csv")

length(unique(klokebok$navn_i_rapporteket))

felles <- intersect(klokebok$navn_i_rapporteket, names(allevarnum))
bare_allevar <- setdiff(names(allevarnum), klokebok$navn_i_rapporteket)
bare_klokebok <- setdiff(klokebok$navn_i_rapporteket, names(allevarnum))

# treff <- pmatch(bare_allevar, bare_klokebok)
# # treff2 <- charmatch(bare_allevar, bare_klokebok)
#
# data.frame(bare_allevar, bare_klokebok[treff])

treff2 <- stringdist::amatch(bare_allevar, bare_klokebok, method = "cosine", maxDist = .12)

delmatch <- data.frame(alleVarNum=bare_allevar, Klokebok=bare_klokebok[treff2])
write.csv2(delmatch, "~/.ssh/nra/mangler_i_klokebok.csv", row.names = F, fileEncoding = "Latin1", na = '')



