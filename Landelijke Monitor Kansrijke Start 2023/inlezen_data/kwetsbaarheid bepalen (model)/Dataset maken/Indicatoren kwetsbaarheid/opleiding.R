#Hoogst behaalde opleiding per jaar

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")


#------------------------------------------------------------------------------
#HOOGSTEOPLTAB
#------------------------------------------------------------------------------
jaren = 2016:2018
hoogstopl_data1 <- inlezen_data("G:/Onderwijs/HOOGSTEOPLTAB/", c("RINPERSOONS", "RINPERSOON", "OPLNIVSOI2016AGG4HBMETNIRWO"))
setnames(hoogstopl_data1, "OPLNIVSOI2016AGG4HBMETNIRWO", "OPLNIVSOI")

jaren = 2019:2022 #nieuwe jaren toevoegen
hoogstopl_data2 <- inlezen_data("G:/Onderwijs/HOOGSTEOPLTAB/", c("RINPERSOONS", "RINPERSOON", "OPLNIVSOI2021AGG4HBmetNIRWO"))
setnames(hoogstopl_data2, "OPLNIVSOI2021AGG4HBmetNIRWO", "OPLNIVSOI")


hoogstopl_data <- rbind(hoogstopl_data1, hoogstopl_data2)
#------------------------------------------------------------------------------
#Categorien toevoegen
#------------------------------------------------------------------------------

hoogstopl_data[, "opleidingsniveau" := fcase(
  startsWith(OPLNIVSOI, "1"), "laag",
  startsWith(OPLNIVSOI, "2"), "midden",
  startsWith(OPLNIVSOI, "3"), "hoog",
  OPLNIVSOI %in% c("----", "9999"), "onbekend"
)]

#------------------------------------------------------------------------------
#Labels omzetten
#------------------------------------------------------------------------------

hoogstopl_data[, OPLNIVSOI  := NULL]

hoogstopl_data[, jaar := as.numeric(as.character(jaar))]

#jaar terugkijken (2020 wordt 2021 in finale set)
hoogstopl_data[, jaar := jaar+1]

#------------------------------------------------------------------------------
#Opruimen
#------------------------------------------------------------------------------
rm(hoogstopl_data1, hoogstopl_data2)

