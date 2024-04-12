#NaasteVerlies

#Verlies van ouder of kind in de laatste 5 jaar

#Verdachte

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")
source("H:/PP/utils_PP.R")
#Jaren
#jaren <- c(2017:2019)
years = jaren
#------------------------------------------------------------------------------
# data Ophalen
#------------------------------------------------------------------------------
#KIND ouder data

#Bepalen locaties laatste dataset
data_locatie <- list.files("G:/Bevolking/KINDOUDERTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)

data_locatie <- data_locatie[length(data_locatie)]

#Inlezen datasets met alleen relevante kolommen
kindOuder <- read_spss(data_locatie)

###OVERLIJDEN data
#Bepalen locaties laatste dataset
data_locatie <- list.files("G:/Bevolking/GBAOVERLIJDENTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)

data_locatie <- data_locatie[length(data_locatie)]

#Inlezen datasets met alleen relevante kolommen
Overlijden <- read_spss(data_locatie)

###overlijden data omzetten naar overlijdensjaar
Overlijden <- data.table(Overlijden)
Overlijden[, jaarOvlijd := as.numeric(substr(GBADatumOverlijden,1, 4))]
Overlijden[, GBADatumOverlijden := NULL]


### data koppellen
Naaste_overl <- merge(kindOuder, Overlijden, by.x = c("RINPERSOONS", "RINPERSOON"), by.y= c("RINPERSOONS", "RINPERSOON"), all.x=T)
Naaste_overl <- merge(Naaste_overl, Overlijden, by.x = c("RINPERSOONSMa", "RINPERSOONMa"), by.y= c("RINPERSOONS", "RINPERSOON"), all.x=T)
Naaste_overl <- merge(Naaste_overl, Overlijden, by.x = c("RINPERSOONSpa", "RINPERSOONpa"), by.y= c("RINPERSOONS", "RINPERSOON"), all.x=T)
setnames(
  Naaste_overl,
  c("jaarOvlijd.x", "jaarOvlijd.y", "jaarOvlijd"),
  c("jaarOvlijdKIND", "jaarOvlijdMa", "jaarOvlijdPa")
)

#------------------------------------------------------------------------------
# data verweken
#------------------------------------------------------------------------------ 
  
  VerliesOuders <- data.table()
  
for(year in jaren){
  
  #Personen in afgelopen 5 jaar overleden
  VerliesOuder <- Naaste_overl %>% filter( jaarOvlijdMa == year -1 | jaarOvlijdMa == year -2 | jaarOvlijdMa == year -3 | jaarOvlijdMa == year -4 | jaarOvlijdMa == year -5|
                                      jaarOvlijdPa == year -1 | jaarOvlijdPa == year -2 | jaarOvlijdPa == year -3 | jaarOvlijdPa == year -4 | jaarOvlijdPa == year -5 )
  VerliesOuder <- data.table(VerliesOuder)
  VerliesOuder[, DoodOuder5J := T] 
  VerliesOuder[, jaar := year]
  VerliesOuder <- VerliesOuder %>%  select(RINPERSOONS, RINPERSOON , DoodOuder5J, jaar)
  
  VerliesOuders <- rbind(VerliesOuders, VerliesOuder)

  }


VerliesKinder <- data.table()

for(year in jaren){
  
  #Personen in afgelopen 5 jaar overleden
  VerliesKind <- Naaste_overl %>% filter( jaarOvlijdKIND == year -1 | jaarOvlijdKIND == year -2 | jaarOvlijdKIND == year -3 | jaarOvlijdKIND == year -4 | jaarOvlijdKIND == year -5)
  VerliesKind <- data.table(VerliesKind)
  VerliesKind[, DoodKind5J := T] 
  VerliesKind[, jaar := year]
  VerliesKind <- VerliesKind %>%  select(RINPERSOONSMa, RINPERSOONMa , DoodKind5J, jaar)
  setnames(
    VerliesKind,
    c("RINPERSOONSMa", "RINPERSOONMa"),
    c("RINPERSOONS", "RINPERSOON") )
  
  VerliesKinder <- rbind(VerliesKinder, VerliesKind)
  
}


rm(Overlijden, kindOuder, Naaste_overl, VerliesOuder, VerliesKind )
  