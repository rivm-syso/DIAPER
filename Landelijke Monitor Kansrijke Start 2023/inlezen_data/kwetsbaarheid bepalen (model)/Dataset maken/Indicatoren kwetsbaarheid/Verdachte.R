#Verdachte

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")
source("H:/PP/utils_PP.R")
#Jaren
#jaren <- c(2017:2019)

#------------------------------------------------------------------------------
#gedetineerde data
#------------------------------------------------------------------------------
Verdachte <- inlezen_data_PP("G:/VeiligheidRecht/VERDTAB/", c("RINPERSOONS", "RINPERSOON"), specific = "7jaar")

Verdachte[, "Verdachte" := TRUE]

Verdachte[, jaar := as.numeric(as.character(jaar))]

Verdachte[jaar == 1401, jaar := 2009]

Verdachten <- data.table()


for(year in jaren){
  
  Verdachte1 <- Verdachte %>% filter( jaar == year -1 | jaar == year -2 | jaar == year -3 | jaar == year -4 | jaar == year -5 | jaar == year -6 | jaar == year -7)
  Verdachte1 <- data.table(Verdachte1)
  Verdachte1[, jaar := year]
  
  Verdachten <- rbind(Verdachten, Verdachte1 )
  
}


rm(Verdachte1, Verdachte)
