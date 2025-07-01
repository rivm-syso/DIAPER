#CBS pariteit

#Paritetis bepaling doormiddel van data afkomstig uit het BRP


#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#jaren = 2016:2020


# Data ophalen ------------------------------------------------------------

#Levend geborenen - Inlezen datasets met alleen relevante kolommen
levend_geb <- inlezen_data("G:/Bevolking/GBALEVENDGEBORENENMASSATAB", kolommen = c("RINPERSOONS", "RINPERSOON", 
                                                              "RANGNUMMERMOEDER"))

levend_geb <- as.data.table(levend_geb)

#Dood geborenen - Inlezen datasets met alleen relevante kolommen
dood_geb <- inlezen_data("G:/Bevolking/DGGBAPERSOONTAB", kolommen = c("RINPERSOONS", "RINPERSOON", 
                                                                                   "DGRANGNUMMERMOEDER"))

dood_geb <- as.data.table(dood_geb)
setnames(dood_geb, "DGRANGNUMMERMOEDER", "RANGNUMMERMOEDER")


pariteit <- rbind(levend_geb, dood_geb)

# Pariteit ----------------------------------------------------------------
pariteit[, par := as.numeric(as.character(RANGNUMMERMOEDER)) - 1]

#cleanen
pariteit[, RANGNUMMERMOEDER := NULL]
rm(levend_geb, dood_geb)
