#etniciteit

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#------------------------------------------------------------------------------
#GBA persoon data
#------------------------------------------------------------------------------

#Bepalen locaties laatste dataset
gbapersoon_locatie <- list.files("G:/Bevolking/GBAPERSOONTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)

gbapersoon_locatie <- gbapersoon_locatie[length(gbapersoon_locatie)]

#Inlezen datasets met alleen relevante kolommen
persoon_kenmerken <- read_spss(gbapersoon_locatie, col_select = c(RINPERSOONS, RINPERSOON, GBAHERKOMSTGROEPERING, GBAGENERATIE))

persoon_kenmerken <- as.data.table(persoon_kenmerken)
rm(gbapersoon_locatie)

#------------------------------------------------------------------------------
#Koppelen
#------------------------------------------------------------------------------
#Koppeltabel
koppeltabel <- read_spss("K:/Utilities/Code_Listings/SSBreferentiebestanden/LANDAKTUEELREFV13.SAV", col_select = c("LAND", "ETNGRP"))

setnames(koppeltabel, 
         c("LAND", "ETNGRP"), 
         c("GBAHERKOMSTGROEPERING", "migratieachtergrond"))

koppeltabel <- as.data.table(koppeltabel)

koppeltabel[, migratieachtergrond := unlabelled(migratieachtergrond)]

etnische_achtergrond <- merge(persoon_kenmerken, koppeltabel, all.x = TRUE, by = c("GBAHERKOMSTGROEPERING"))

#------------------------------------------------------------------------------
#Opschonen
#------------------------------------------------------------------------------
etnische_achtergrond[, GBAHERKOMSTGROEPERING := NULL]
