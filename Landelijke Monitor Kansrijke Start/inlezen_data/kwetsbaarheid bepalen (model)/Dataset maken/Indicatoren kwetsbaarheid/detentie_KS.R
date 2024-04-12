#detentie

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")
source("H:/PP/utils_PP.R")
#Jaren
#jaren <- c(2017:2019)
years <- jaren
#------------------------------------------------------------------------------
#gedetineerde data
#------------------------------------------------------------------------------
detentie <- inlezen_data_PP("G:/VeiligheidRecht/GEDETINEERDENTAB/", c("RINPERSOONS", "RINPERSOON"), specific = "12jaar")

detentie[, "detentie" := TRUE]

detentie[, jaar := as.numeric(as.character(jaar))]
detentie <- unique(detentie)


gevangen <- data.table()


for(year in jaren){
  
  detentie1 <- detentie %>% filter(jaar == year -1 | jaar == year -2 | jaar == year -3 | jaar == year -4 | jaar == year -5 | jaar == year -6 | jaar == year -7 | jaar == year -8 | jaar == year -9 | jaar == year -10 | jaar == year -11 | jaar == year -12)
  detentie1 <- data.table(detentie1)
  detentie1[, jaar := year]
  
  gevangen <- rbind(gevangen, detentie1 )
  
}


rm(detentie, detentie1)
