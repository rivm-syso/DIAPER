#Inlezen en opschonen van medicijn data

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Jaren
#jaren <- c(2017:2019)

#------------------------------------------------------------------------------
#ZVW zorgkosten data
#------------------------------------------------------------------------------
med_gebruik <- inlezen_data("G:/GezondheidWelzijn/MEDICIJNTAB", 
                           c("RINPERSOONS", "RINPERSOON", "ATC4"))

#------------------------------------------------------------------------------
#opschonen
#------------------------------------------------------------------------------
med_gebruik[, ATC4 := as.character(ATC4)]

#alle personen vaststellen die een  mentale klachten hebben gehad
psych_gebruik <- med_gebruik[, "med_psych_klachten" := ifelse(ATC4 %in% c("N05A", "N05B", "N05C", "N06A", "N06B", "n06C"), TRUE, FALSE)]
psych_gebruik <- psych_gebruik[med_psych_klachten == TRUE][, ATC4 := NULL]
psych_gebruik <- unique(psych_gebruik) #per persoon 1 regel met ja of nee

#Personen met hoog medicijn gebruik vaststelen (>5 medicijnen)
hoog_gebruik <- med_gebruik[, .N, by = .(jaar, RINPERSOONS, RINPERSOON)] #aantal rijen per persoon per jaar tellen
hoog_gebruik[, "hoog_gebruik" := ifelse(N >= 5, TRUE, FALSE)][, N := NULL]

#Koppelen van psych aand hoog gebruik
med_gebruik <- merge(hoog_gebruik, psych_gebruik, 
                     all.x = TRUE,
                     by = c("jaar", "RINPERSOONS", "RINPERSOON"))

med_gebruik[, med_psych_klachten := ifelse(is.na(med_psych_klachten), FALSE, med_psych_klachten)]

med_gebruik[, jaar := as.numeric(as.character(jaar))]

#opruimen
rm(psych_gebruik)
