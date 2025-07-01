#Jeugdhulp

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Jaren
#jaren <- c(2017:2019)

#------------------------------------------------------------------------------
#Jeugdhulp data
#------------------------------------------------------------------------------


jeugdhulp <- inlezen_data("G:/GezondheidWelzijn/JGDHULPBUS", c("RINPERSOONS", "RINPERSOON"), "zvw")

jeugdhulp[, "jeugdhulp" := TRUE]

jeugdhulp[, jaar := as.numeric(as.character(jaar))]

jeugdhulp <- unique(jeugdhulp)


#------------------------------------------------------------------------------
#Ouder data
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset kindouder
kindouder_locatie <- list.files("G:/Bevolking/KINDOUDERTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)
kindouder_locatie <- kindouder_locatie[length(kindouder_locatie)]
#Inlezen datasets met alleen relevante kolommen
kindouder <- read_spss(kindouder_locatie)
kindouder <- as.data.table(kindouder)

jeugdhulp <- merge(jeugdhulp, kindouder, all.x = T, by= c("RINPERSOONS","RINPERSOON"))




#------------------------------------------------------------------------------
#Kinderen van MOEDER afgelopen 2 jaar jeugdzorg ontvangen
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset kindouder

jeugdhulpMA <- data.table()


for(year in jaren){
  
  JgdH <- jeugdhulp %>% filter(jaar == year -1| jaar == year -2) 
  JgdH <- JgdH %>% select(RINPERSOONSMa, RINPERSOONMa)
  
  JgdH[, jeugdhulp := T]
  JgdH[, jaar := year]
  
  JgdH <- unique(JgdH)
  
  jeugdhulpMA <- rbind(jeugdhulpMA, JgdH)
  
  }

jeugdhulpMA <- unique(jeugdhulpMA)

#------------------------------------------------------------------------------
#Kinderen van VADER afgelopen 2 jaar jeugdzorg ontvangen
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset kindouder

jeugdhulpPA <- data.table()


for(year in jaren){
  
  JgdH <- jeugdhulp %>% filter(jaar == year -1| jaar == year -2) 
  JgdH <- JgdH %>% select(RINPERSOONSpa , RINPERSOONpa )
  
  JgdH[, jeugdhulp := T]
  JgdH[, jaar := year]
  
  JgdH <- unique(JgdH)
  
  jeugdhulpPA <- rbind(jeugdhulpPA, JgdH)
  
}

jeugdhulpPA <- unique(jeugdhulpPA)

rm(jeugdhulp, JgdH, kindouder)

