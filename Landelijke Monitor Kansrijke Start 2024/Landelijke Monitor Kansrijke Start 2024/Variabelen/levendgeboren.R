#Kraamzorg per moeder

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Jaren
#jaren <- c(2015:2020)

#------------------------------------------------------------------------------
#levendgeboren data
#------------------------------------------------------------------------------
levendgeboren_data <- inlezen_data("G:/Bevolking/GBALEVENDGEBORENENMASSATAB", c("RINPERSOONS", "RINPERSOON"))

#dubbele eruit halen (2015 komt 2 keer voor)
levendgeboren_data <-unique(levendgeboren_data)

#------------------------------------------------------------------------------
#ouders koppelen
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset KINDOUDER
kindouder_locatie <- list.files("G:/Bevolking/KINDOUDERTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)

kindouder_locatie <- kindouder_locatie[length(kindouder_locatie)]

#Inlezen datasets met alleen relevante kolommen
kindouder <- read_spss(kindouder_locatie)

kindouder <- as.data.table(kindouder)

#koppelen
levendgeboren_ouders <- merge(levendgeboren_data, kindouder, all.x = TRUE, by = c("RINPERSOONS", "RINPERSOON"))
levendgeboren_ouders[, XKOPPELNUMMER := NULL]

levendgeboren_ouders[, jaar := as.numeric(as.character(jaar))]

#------------------------------------------------------------------------------
#opruimen
#------------------------------------------------------------------------------
rm(levendgeboren_data, kindouder_locatie, kindouder)
