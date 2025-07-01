#RDW

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")
source("H:/PP/utils_PP.R")
#Jaren
#jaren <- c(2017:2019)
years <- jaren



#------------------------------------------------------------------------------
#RDW data
#------------------------------------------------------------------------------

RDW <- inlezen_data_PP("G:/VerkeerVervoer/RDWNPACTTAB/", c("RINPERSOONS", "RINPERSOON", "VRTGSOORT"))
RDW <- data.table(RDW)

#Enkel selectie actief park voertuigen
RDW <- RDW[VRTGSOORT == "1"| VRTGSOORT == "2"| VRTGSOORT == "3"| VRTGSOORT == "4"| VRTGSOORT == "5"| VRTGSOORT == "6"| VRTGSOORT == "7"| VRTGSOORT == "8"| VRTGSOORT == "9", ]


RDW[, "RDW" := TRUE]
RDW[, jaar := as.numeric(as.character(jaar))]


#------------------------------------------------------------------------------
# huishoudnr
#------------------------------------------------------------------------------#
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/huishoudnummer.R")

#------------------------------------------------------------------------------
#Koppelen
#------------------------------------------------------------------------------
RDW <- merge(RDW, huishoudkoppel_data, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
#------------------------------------------------------------------------------
#oppschonen 
#------------------------------------------------------------------------------
RDW[, VRTGSOORT   := NULL]

RDW <- unique(RDW)
