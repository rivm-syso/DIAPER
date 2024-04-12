#Ouders van kinderen tot en met 2 jaar

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")
source("H:/PP/utils_PP.R")
library(fst)


# jaren <- c(2016:2022) 
# years <- jaren

#------------------------------------------------------------------------------
#GBA persoon data
#------------------------------------------------------------------------------

#Bepalen locaties laatste dataset
gbapersoon_locatie <- list.files("G:/Bevolking/GBAPERSOONTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)

gbapersoon_locatie <- gbapersoon_locatie[length(gbapersoon_locatie)]

#Inlezen datasets met alleen relevante kolommen
geboortejaar <- read_spss(gbapersoon_locatie, col_select = c(RINPERSOONS, RINPERSOON, GBAGEBOORTEJAAR))

geboortejaar <- as.data.table(geboortejaar)

kinderen02 <- data.table()

for(year in jaren){
  
  geboortes <- geboortejaar %>% filter(GBAGEBOORTEJAAR == year | GBAGEBOORTEJAAR == year -1| GBAGEBOORTEJAAR == year -2) 
  geboortes <- data.table(geboortes)
  geboortes[, jaar := year]
  geboortes <- unique(geboortes)
  
  kinderen02 <- rbind(kinderen02, geboortes)
  
  }


#------------------------------------------------------------------------------
# KINDOUDERTAB
#------------------------------------------------------------------------------

#toevoegen RIN moeder & vader 
#Bepalen locaties laatste dataset
kindouder_locatie <- list.files("G:/Bevolking/KINDOUDERTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)
kindouder_locatie <- kindouder_locatie[length(kindouder_locatie)]

#Inlezen datasets met alleen relevante kolommen
kindouder <- read_spss(kindouder_locatie)
kindouder <- as.data.table(kindouder)


setnames(kindouder, 
         c("RINPERSOONSpa", "RINPERSOONpa", "RINPERSOONSMa", "RINPERSOONMa"),
         c("Rinpersoons_VADER", "RINPERSOON_VADER", "Rinpersoons_MOEDER", "RINPERSOON_MOEDER"))


kindouder0_2 <- merge(kinderen02, kindouder, all.x = T, by= c("RINPERSOONS","RINPERSOON"))

setnames(kindouder0_2, 
         c("RINPERSOONS", "RINPERSOON"),
         c("Rinpersoons_KIND", "RINPERSOON_KIND"))


rm(geboortejaar, geboortes, kindouder, kinderen02)
