#Huishoudnummer


#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")


#------------------------------------------------------------------------------
#GBA persoon data
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset
data_locatie <- list.files("G:/InkomenBestedingen/INHATAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)

inhatab_locatie <- data_locatie[grep("TABV", data_locatie)]
huishoudkoppel_locatie <- data_locatie[grep("KOPPELPERSOONHUISHOUDEN", data_locatie)]

#Jaren filteren
inhatab_locatie <- inhatab_locatie[grep(paste(jaren, collapse = "|"), inhatab_locatie)]
huishoudkoppel_locatie <- huishoudkoppel_locatie[grep(paste(jaren, collapse = "|"), huishoudkoppel_locatie)]

#unieke jaren selecteren
inhatab_locatie <- as.data.table(inhatab_locatie) #omzetten naar data.table om te kunnen filteren
inhatab_locatie[, "jaar" := str_extract(basename(inhatab_locatie), "\\d{4}")] #jaar kolom maken
inhatab_locatie <- unique(inhatab_locatie, fromLast = TRUE, by = "jaar") #laatste versie van elk jaar pakken
inhatab_locatie <- list(inhatab_locatie$inhatab_locatie)

huishoudkoppel_locatie <- as.data.table(huishoudkoppel_locatie) #omzetten naar data.table om te kunnen filteren
huishoudkoppel_locatie[, "jaar" := str_extract(basename(huishoudkoppel_locatie), "\\d{4}")] #jaar kolom maken
huishoudkoppel_locatie <- unique(huishoudkoppel_locatie, fromLast = TRUE, by = "jaar") #laatste versie van elk jaar pakken
huishoudkoppel_locatie <- list(huishoudkoppel_locatie$huishoudkoppel_locatie)

#Koppelen van RIN aan hoofdkostwinner
huishoudkoppel_list <-  lapply(huishoudkoppel_locatie[[1]], read_spss)
huishoudkoppel_data <- rbindlist(huishoudkoppel_list, idcol = "jaar")

rm(huishoudkoppel_list)

huishoudkoppel_data[, jaar := factor(jaar, labels = str_extract(basename(huishoudkoppel_locatie[[1]]), "\\d{4}"))]
huishoudkoppel_data[, jaar := as.numeric(as.character(jaar))]

