#Verhuizingen 

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")
source("H:/PP/utils_PP.R")


#------------------------------------------------------------------------------
#Verhuizing data
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset
data_locatie <- list.files("G:/Bevolking/GBAADRESOBJECTBUS", pattern = ".sav", recursive = TRUE, full.names = TRUE)

data_locatie <- data_locatie[length(data_locatie)]

#Inlezen datasets met alleen relevante kolommen
data <- read_spss(data_locatie,  col_select = c(RINPERSOONS, RINPERSOON, GBADATUMEINDEADRESHOUDING ))
data <- data.table(data)
data[, jaarVerh := as.numeric(substr(GBADATUMEINDEADRESHOUDING,1, 4))]

verhuizingen <- data.table()

for(year in jaren){
  
  #Jaren selecteren waarover terug te kijken
  verhuizing <- data %>% filter(jaarVerh == year -1 | jaarVerh == year -2 | jaarVerh == year -3 | jaarVerh == year -4 | jaarVerh == year -5 )
  verhuizing <- data.table(verhuizing)
  verhuizing[, jaar := year]
  
  #tellen van aantal eind data adres per persoon in tijdsbestek
  verhuizing <- verhuizing %>% group_by(RINPERSOONS, RINPERSOON) %>% mutate( nVerhuis5J = n())
  
  #vehuizen variabele toevoegen met LCA catogori"en 
  verhuizing <- data.table(verhuizing)
  verhuizing[nVerhuis5J >=5, verhuizingen := 1]
  verhuizing[nVerhuis5J <=4, verhuizingen := 2]

  
  #schonen
  verhuizing[, GBADATUMEINDEADRESHOUDING := NULL]
  verhuizing[, jaarVerh := NULL]
  verhuizing <- unique(verhuizing)
  
  verhuizingen <- rbind(verhuizingen, verhuizing )
  
}



rm(data, verhuizing)
