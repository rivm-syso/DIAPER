#aanwezig in asielcohort

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#------------------------------------------------------------------------------
#GBA persoon data
#------------------------------------------------------------------------------

#Bepalen locaties laatste dataset
gbapersoon_locatie <- list.files("G:/Bevolking/GBAPERSOONTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)

gbapersoon_locatie <- gbapersoon_locatie[length(gbapersoon_locatie)]

#Inlezen datasets met alleen relevante kolommen
personen <- read_spss(gbapersoon_locatie, col_select = c(RINPERSOONS, RINPERSOON))

personen <- as.data.table(personen)
rm(gbapersoon_locatie)

#------------------------------------------------------------------------------
#asielcohort data
#------------------------------------------------------------------------------

#Bepalen locaties laatste dataset
asiel_locatie <- list.files("G:/Maatwerk/ASIELCOHORT", pattern = ".sav", recursive = TRUE, full.names = TRUE)

asiel_locatie <- asiel_locatie[length(asiel_locatie)]

#Inlezen datasets met alleen relevante kolommen
asielcohort <- read_spss(asiel_locatie, col_select = c(RINPERSOONS, RINPERSOON, Pop_COAIND))

asielcohort <- as.data.table(asielcohort)

rm(asiel_locatie)

#Variabelen met dat aangeeft aanwezig in asielcohort
asielcohort <- asielcohort[Pop_COAIND == 1, "asielcohort" := TRUE]

#------------------------------------------------------------------------------
#koppelen
#------------------------------------------------------------------------------
asielcohort <- merge(personen, asielcohort, 
                        all.x = TRUE, 
                        by = c("RINPERSOONS", "RINPERSOON"))

asielcohort[, asielcohort := ifelse(asielcohort == TRUE, asielcohort, FALSE)]

#------------------------------------------------------------------------------
#opruimen
#------------------------------------------------------------------------------
asielcohort <- unique(asielcohort, by = c("RINPERSOONS", "RINPERSOON"))

rm(personen)
