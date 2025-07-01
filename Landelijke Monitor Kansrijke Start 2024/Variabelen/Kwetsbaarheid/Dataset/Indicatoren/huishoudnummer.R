#Huishoud nummer


#Workdirectory
setwd("H:/Data proces/")


#Utilities
source("src/utils.R")

# #------------------------------------------------------------------------------
# #Huishoudesamenstelling data
# #------------------------------------------------------------------------------
# #Bepalen locaties laatste dataset
# hh_locatie <- list.files("G:/Bevolking/GBAHUISHOUDENSBUS", pattern = ".sav", recursive = TRUE, full.names = TRUE)
# 
# hh_locatie <- hh_locatie[length(hh_locatie)]
# 
# #Inlezen datasets met alleen relevante kolommen
# hhsamenstelling <- read_spss(hh_locatie, col_select = c(RINPERSOONS, RINPERSOON, DATUMAANVANGHH,
#                                                         DATUMEINDEHH, HUISHOUDNR))
# 
# hhsamenstelling <- as.data.table(hhsamenstelling)
# 
# #------------------------------------------------------------------------------
# #Opschonen
# #------------------------------------------------------------------------------
# #per jaar moet een regel aangemaakt worden met de status van het huishouden van dat jaar
# #gekozen voor 01-01 als peildatum
# 
# #Functie aanmaken voor relevante jaren
# hh_filter <- function(jaar) {
#   str_c("\"", jaar, "\" = ifelse(DATUMAANVANGHH <= ", jaar, "0101 & DATUMEINDEHH >= ", jaar, "0101, TRUE, FALSE),")
# }
# 
# #aanmaken string voor filter
# filter_string <- str_c(sapply(jaren, hh_filter), collapse = " ")
# 
# filter_string <- substr(filter_string, 1, nchar(filter_string) - 1)
# 
# #Filteren
# hhsamenstelling[
#   #Data omzetten in numeric om te kunnen filteren
#   , c("DATUMAANVANGHH", "DATUMEINDEHH") :=
#     .(as.numeric(DATUMAANVANGHH), as.numeric(DATUMEINDEHH))
# ]
# 
# #Rijen voor eerste jaar alvast weghalen
# hhsamenstelling <- hhsamenstelling[
#   DATUMEINDEHH >= as.numeric(str_c(min(jaren), "1231"))
# ]
# 
# #Filter runnen voor alle jaren
# eval(parse(text=sprintf("hhsamenstelling[,':=' (%s)]", filter_string)))
# 
# #Data omzetten naar langer format om makkelijk te kunnen koppelen en filteren
# hhsamenstelling <- melt(hhsamenstelling, measure.vars = c(6:length(hhsamenstelling)),
#                         variable.name = "jaar", value.name = "keep")
# 
# #Selecten van alleen regels met adressen
# hhsamenstelling <- hhsamenstelling[keep == TRUE]
# 
# #Kolommen verwijderen
# hhsamenstelling[, c("keep", "DATUMAANVANGHH", "DATUMEINDEHH") := NULL]
# 
# hhsamenstelling[, jaar := as.numeric(as.character(jaar))]
# 
# #GM opruimen
# rm(filter_string, hh_locatie, hh_filter)


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

