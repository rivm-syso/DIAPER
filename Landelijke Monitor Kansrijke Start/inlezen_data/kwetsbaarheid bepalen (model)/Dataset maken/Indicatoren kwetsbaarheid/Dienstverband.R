#Dienstverband 

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Relevant jaren
#jaren <- c(2018:2019)





#------------------------------------------------------------------------------
# Data inladen
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset
data <- inlezen_data("G:/Spolis/SPOLISBUS/", c("RINPERSOONS", "RINPERSOON", "SCONTRACTSOORT", "SPOLISDIENSTVERBAND", "SBASISUREN"))
  
 
data <- data.table(data)

#------------------------------------------------------------------------------
# herschrijven variabelen
#------------------------------------------------------------------------------
data[SCONTRACTSOORT == "B", contractsoort := 1] #bepaalde tijd
data[SCONTRACTSOORT == "O", contractsoort := 2] #onbepaalde tijd
data[SCONTRACTSOORT == "N", contractsoort := 3] #niet van toepassing

data[SPOLISDIENSTVERBAND == "2", dienstverband := 1] #deeltijd
data[SPOLISDIENSTVERBAND == "1", dienstverband := 2] #voltijd

#------------------------------------------------------------------------------
# Opschonen
#------------------------------------------------------------------------------
data <- unique(data)
data <- data %>% select(RINPERSOONS, RINPERSOON, jaar, SBASISUREN, contractsoort,dienstverband)
gc()


# Let op: er staan duplicates in het databestand. Mensen kunnen namelijk meerdere contracten op hetzelfde moment hebben.
## Daarom: selecteren van de observatie met de meeste basisuren
Dienstverband <- data %>% group_by(RINPERSOONS, RINPERSOON, jaar) %>% arrange(SBASISUREN) %>% slice_tail() 
Dienstverband <- data.table(Dienstverband)
Dienstverband[, SBASISUREN := NULL]
rm(data)

gc()
Dienstverband <- unique(Dienstverband)



#jaar terugkijken (2020 wordt 2021 in finale set)
Dienstverband[, jaar := jaar+1]

