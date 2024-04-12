#Scheidingen 

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

data <- inlezen_data_PP("G:/Bevolking/GBASCHEIDINGENMASSATAB/", c("SCHEIDINGJAAR", "RINPERSOONSPARTNER1S", "RINPERSOONPARTNER1S", "RINPERSOONSPARTNER2S", "RINPERSOONPARTNER2S"), specific =  "4jaar")
data[, jaar := as.numeric(as.character(jaar))]

#corigeren jaar 2013
data[jaar == 1407, jaar := 2013]
data[, scheiding := TRUE]

#van koppels naar individuen
data1 <- select(data, jaar, RINPERSOONSPARTNER1S, RINPERSOONPARTNER1S, scheiding)
data2 <- select(data, jaar, RINPERSOONSPARTNER2S, RINPERSOONPARTNER2S, scheiding)
names(data1) <- c('jaar', 'RINPERSOONS', 'RINPERSOON', 'scheiding')
names(data2) <- c('jaar', 'RINPERSOONS', 'RINPERSOON', 'scheiding')
scheiding1 <- rbind(data1,data2)
scheiding1 <- unique(scheiding1)


scheidingen <- data.table()

for(year in jaren){
  
  scheiding2 <- scheiding1 %>% filter(jaar == year -1| jaar == year -2| jaar == year -3| jaar == year -4) 
  scheiding2 <- data.table(scheiding2)
  scheiding2[, scheiding4J := T]
  scheiding2[, jaar := year]
  
  scheiding2 <- unique(scheiding2)
  
  scheidingen <- rbind(scheidingen, scheiding2)

  
}

scheidingen[, scheiding := NULL]
scheidingen <- unique(scheidingen)

rm(scheiding1, scheiding2)

