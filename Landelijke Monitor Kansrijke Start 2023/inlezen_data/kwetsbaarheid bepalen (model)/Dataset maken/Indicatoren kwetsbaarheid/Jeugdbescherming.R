#Jeugbescherming 

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Jaren
#jaren <- c(2017:2019)

#------------------------------------------------------------------------------
#Jeugdhulp data
#------------------------------------------------------------------------------


Jeugbescherming <- inlezen_data("G:/VeiligheidREcht/JGDBESCHERMBUS", c("RINPERSOONS", "RINPERSOON"), "zvw")



Jeugbescherming[, "Jeugbescherming" := TRUE]

Jeugbescherming[, jaar := as.numeric(as.character(jaar))]

Jeugbescherming <- unique(Jeugbescherming)


#------------------------------------------------------------------------------
#Ouder data
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset kindouder
kindouder_locatie <- list.files("G:/Bevolking/KINDOUDERTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)
kindouder_locatie <- kindouder_locatie[length(kindouder_locatie)]
#Inlezen datasets met alleen relevante kolommen
kindouder <- read_spss(kindouder_locatie)
kindouder <- as.data.table(kindouder)

Jeugbescherming <- merge(Jeugbescherming, kindouder, all.x = T, by= c("RINPERSOONS","RINPERSOON"))




#------------------------------------------------------------------------------
#Kinderen van MOEDER afgelopen 2 jaar jeugdzorg ontvangen
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset kindouder

jeugdbeschMA <- data.table()


for(year in jaren){
  
  JgdH <- Jeugbescherming %>% filter(jaar == year -1| jaar == year -2) 
  JgdH <- JgdH %>% select(RINPERSOONSMa, RINPERSOONMa)
  
  JgdH[, Jeugbescherming := T]
  JgdH[, jaar := year]
  
  JgdH <- unique(JgdH)
  
  jeugdbeschMA <- rbind(jeugdbeschMA, JgdH)
  
}

jeugdbeschMA <- unique(jeugdbeschMA)

#------------------------------------------------------------------------------
#Kinderen van VADER afgelopen 2 jaar jeugdzorg ontvangen
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset kindouder

jeugdbeschPA <- data.table()


for(year in jaren){
  
  JgdH <- Jeugbescherming %>% filter(jaar == year -1| jaar == year -2) 
  JgdH <- JgdH %>% select(RINPERSOONSpa , RINPERSOONpa )
  
  JgdH[, Jeugbescherming := T]
  JgdH[, jaar := year]
  
  JgdH <- unique(JgdH)
  
  jeugdbeschPA <- rbind(jeugdbeschPA, JgdH)
  
}

jeugdbeschPA <- unique(jeugdbeschPA)

rm(Jeugbescherming, JgdH, kindouder)

