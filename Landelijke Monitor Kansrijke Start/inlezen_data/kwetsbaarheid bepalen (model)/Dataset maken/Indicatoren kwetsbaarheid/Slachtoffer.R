#slachtoffer

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
slachtoffer <- inlezen_data_PP("G:/VeiligheidRecht/SLOTAB/", c("RINPERSOONS", "RINPERSOON"), specific = "5jaar")

slachtoffer[, "slachtoffer" := TRUE]

slachtoffer[, jaar := as.numeric(as.character(jaar))]


slachtoffers <- data.table()


for(year in jaren){
  
  slachtoffer1 <- slachtoffer %>% filter( jaar == year -1 | jaar == year -2 | jaar == year -3 | jaar == year -4 | jaar == year -5)
  slachtoffer1 <- data.table(slachtoffer1)
  slachtoffer1[, jaar := year]
  
  slachtoffers <- rbind(slachtoffers, slachtoffer1 )
  
}


rm(slachtoffer, slachtoffer1)
