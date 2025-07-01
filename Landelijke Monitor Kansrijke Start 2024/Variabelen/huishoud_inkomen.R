#Huishoudomvang en huishoudinkomen
#Datasets: INHATAB

#Utilities
source("utils.R")

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

#Inlezen inkomen met relevante kolommen
inhatab_list <- lapply(inhatab_locatie[[1]], read_spss, 
                       col_select = c(RINPERSOONSHKW, RINPERSOONHKW, INHP100HGESTES, INHARMLAG, INHAHL, INHSAMHH, INHARMLAGL))

#Datasets combineren tot 1 lange set
inhatab_data <- rbindlist(inhatab_list, idcol = "jaar")
rm(inhatab_list)

#Toevoegen jaar in id
inhatab_data[, jaar := factor(jaar, labels = str_extract(basename(inhatab_locatie[[1]]), "\\d{4}"))]

#Omzetten in gewenst variabelen
inhatab_data[, ':=' (
  "ink_kwint" = fcase(
    INHP100HGESTES <= 20, 1,
    INHP100HGESTES > 20 & INHP100HGESTES <= 40, 2,
    INHP100HGESTES > 40 & INHP100HGESTES <= 60, 3,
    INHP100HGESTES > 60 & INHP100HGESTES <= 80, 4,
    INHP100HGESTES > 80, 5),
  "ink_20p" = ifelse(INHP100HGESTES < 20, 1, 0),
  "ink_10p" = ifelse(INHP100HGESTES < 0, NA, #uitsluiting -3:-1 type huishoudens
                     ifelse(INHP100HGESTES < 10, 1, 0)), 
  "lage_inkomensgrens" = ifelse(INHARMLAG < 0, NA, #uitsluiting -3:-1 type huishoudens
                                ifelse(INHARMLAG <= 100, 1, 0)), 
  # "omvang_huishouden" = ifelse(INHAHL == 99, NA, INHAHL),
  # "eenouder" = ifelse(INHSAMHH %in% c(41:43, 55:57), 1, 0),
  # "ink_Kstart" = fcase(
  #   INHP100HGESTES < 10, 1,
  #   INHP100HGESTES >= 10 & INHP100HGESTES <= 90, 2,
  #   INHP100HGESTES > 90, 3,
  #   is.na(INHP100HGESTES) | INHP100HGESTES < 0, 4),
  "lage_inkomensgrens_langdurig" = ifelse(INHARMLAGL < 0, NA, #uitsluiting -4:-1 type huishoudens
                                          ifelse(INHARMLAGL <= 100, 1, 0)) 
)]

inhatab_data[, c("INHARMLAG", "INHSAMHH", "INHARMLAGL") := NULL]

#Koppelen van RIN aan hoofdkostwinner
huishoudkoppel_list <-  lapply(huishoudkoppel_locatie[[1]], read_spss)
huishoudkoppel_data <- rbindlist(huishoudkoppel_list, idcol = "jaar")

rm(huishoudkoppel_list)

huishoudkoppel_data[, jaar := factor(jaar, labels = str_extract(basename(huishoudkoppel_locatie[[1]]), "\\d{4}"))]

huishoud_inkomen <- merge(huishoudkoppel_data, inhatab_data, all.x = TRUE, 
                          by = c("RINPERSOONSHKW", "RINPERSOONHKW", "jaar"))

#Alleen eindbestand overhouden
rm(huishoudkoppel_data, inhatab_data)

#Koppel variabelen verwijderen
huishoud_inkomen[, c("RINPERSOONSHKW", "RINPERSOONHKW") := NULL]

huishoud_inkomen[, jaar := as.numeric(as.character(jaar))]

#------------------------------------------------------------------------------
#gegevens opslaan in een temp map
#------------------------------------------------------------------------------
write.csv2(huishoud_inkomen, "temp/huishoud_inkomen.csv")
