#Verslavingszorg 

#Over de jaren zijn niet dezelfde databronnen beschikbaar, daarom worden oude en nieuwe bestandsvormen gecomibineerd 


#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")
source("H:/PP/utils_PP.R")


#Relevant jaren
#jaren <- c(2018:2019)
years= 2011:2014

#Data bestanden zijn veranderd over de jaren 


#------------------------------------------------------------------------------
# Data inladen
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset
data1 <- inlezen_data_PP("G:/GezondheidWelzijn/GGZDBCTRAJECTENTAB/", c("RINPERSOONS", "RINPERSOON", "GGZDBCCircuit"))
data1 <- rename(data1, GGZDBCCircuitHOOFDDIAG = GGZDBCCircuit)
years= 2015:2016

data2 <- inlezen_data_PP("G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/", c("RINPERSOONS", "RINPERSOON", "GGZDBCCircuitHOOFDDIAG"))
data3 <- inlezen_data("G:/GezondheidWelzijn/GGZDBCTRAJECTENHOOFDDIAGTAB/", c("RINPERSOONS", "RINPERSOON", "GGZDBCCircuitHOOFDDIAG"))

data <- rbind(data1, data2)
data <- rbind(data, data3)

rm(data1, data2, data3)
#------------------------------------------------------------------------------
# selecteren
#------------------------------------------------------------------------------
data[GGZDBCCircuitHOOFDDIAG == '5', verslavingszorg := 1]

data_verslaaf <- data %>% filter(verslavingszorg == 1)


Verslavingszorg1 <- data.table()

for(year in jaren){
  
  Verslaaf <- data_verslaaf %>% filter(jaar == year -1| jaar == year -2| jaar == year -3| jaar == year -4| jaar == year -5| jaar == year -6) 
  Verslaaf[, jaar := year]
    Verslaaf <- unique(Verslaaf)
  
  Verslavingszorg1 <- rbind(Verslavingszorg1, Verslaaf)
  
  
}

Verslavingszorg1[, GGZDBCCircuitHOOFDDIAG  := NULL]
Verslavingszorg1 <- unique(Verslavingszorg1)



# nevendiagnose ----------------------------------------------------------


#Relevant jaren
#jaren <- c(2018:2019)
years= 2011:2014

#Data bestanden zijn veranderd over de jaren 


#------------------------------------------------------------------------------
# Data inladen
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset
data1 <- inlezen_data_PP("G:/GezondheidWelzijn/GGZDBCDiagnoseTAB/", c("RINPERSOONS", "RINPERSOON", "GGZDBCnevendiagnoseDSMIV"))
data1 <- setnames(data1, "GGZDBCnevendiagnoseDSMIV", "nevendiagnose")
years= 2015:2016

data2 <- inlezen_data_PP("G:/GezondheidWelzijn/GGZDBCDIAGNOSEHOOFDDIAGTAB/", c("RINPERSOONS", "RINPERSOON", "GGZDBCnevendiagnoseDSMIVHOOFDDIAG"))
data3 <- inlezen_data("G:/GezondheidWelzijn/GGZDBCDIAGNOSEHOOFDDIAGTAB/", c("RINPERSOONS", "RINPERSOON", "GGZDBCnevendiagnoseDSMIVHOOFDDIAG"))
data2 <- setnames(data2, "GGZDBCnevendiagnoseDSMIVHOOFDDIAG", "nevendiagnose")
data3 <- setnames(data3, "GGZDBCnevendiagnoseDSMIVHOOFDDIAG", 'nevendiagnose')


data <- rbind(data1, data2)
data <- rbind(data, data3)

rm(data1, data2, data3)
#------------------------------------------------------------------------------
# selecteren
#------------------------------------------------------------------------------

data[nevendiagnose == '5', verslavingszorg := 1]

data[, "verslavingszorg" := fcase(
  startsWith(nevendiagnose, "as1_4"), "1")]

data_verslaaf <- data %>% filter(verslavingszorg == 1)


Verslavingszorg2 <- data.table()

for(year in jaren){
  
  Verslaaf <- data_verslaaf %>% filter(jaar == year -1| jaar == year -2| jaar == year -3| jaar == year -4| jaar == year -5| jaar == year -6) 
  Verslaaf[, jaar := year]
  
  Verslaaf <- unique(Verslaaf)
  
  Verslavingszorg2 <- rbind(Verslavingszorg2, Verslaaf)
  
  
}

Verslavingszorg2[, nevendiagnose  := NULL]
Verslavingszorg2 <- unique(Verslavingszorg2)



# Verslavingsbronnen samenvoegen ------------------------------------------

Verslavingszorg <- rbind(Verslavingszorg1, Verslavingszorg2)
Verslavingszorg <- unique(Verslavingszorg)



rm(data, Verslavingszorg1, Verslavingszorg2 )

years <-  jaren
