#locatie van personen vaststellen

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Jaren
#jaren <- c(2019:2020)

#locatie indeling
#gemeente_indeling <- "gem2021"

#------------------------------------------------------------------------------
#GBA adres data
#------------------------------------------------------------------------------

#Bepalen locatie laatste dataset
gbaadres_locatie <- list.files("G:/Bevolking/GBAADRESOBJECTBUS", pattern = ".sav", recursive = TRUE, full.names = TRUE)
gbaadres_locatie <- gbaadres_locatie[length(gbaadres_locatie)]

#Laatste/nieuwste bestand inlezen
locatie <- read_spss(gbaadres_locatie)

locatie <- as.data.table(locatie)

#Data opschonen
#Functie aanmaken voor relevante jaren
locatie_filter <- function(jaar) {
  str_c("\"", jaar, "\" = ifelse(GBADATUMAANVANGADRESHOUDING <= ", jaar, "0630 & GBADATUMEINDEADRESHOUDING >= ", jaar, "0630, TRUE, FALSE),")
}

#aanmaken string voor filter
filter_string <- str_c(sapply(jaren, locatie_filter), collapse = " ")

filter_string <- substr(filter_string, 1, nchar(filter_string) - 1)

#Filteren
locatie[
  #Data omzetten in numeric om te kunnen filteren
  , c("GBADATUMAANVANGADRESHOUDING", "GBADATUMEINDEADRESHOUDING") :=
    .(as.numeric(GBADATUMAANVANGADRESHOUDING), as.numeric(GBADATUMEINDEADRESHOUDING))
  ]

#Rijen voor eerste jaar alvast weghalen
locatie <- locatie[
  GBADATUMEINDEADRESHOUDING >= as.numeric(str_c(min(jaren), "0630"))
]

#Filter runnen voor alle jaren
eval(parse(text=sprintf("locatie[,':=' (%s)]", filter_string)))

#Data omzetten naar langer format om makkelijk te kunnen koppelen en filteren
locatie <- melt(locatie, measure.vars = c(7:length(locatie)),
                 variable.name = "jaar", value.name = "keep")

#Selecten van alleen regels met adressen
locatie <- locatie[keep == TRUE]

#Kolommen verwijderen
locatie[, c("keep", "GBADATUMAANVANGADRESHOUDING", "GBADATUMEINDEADRESHOUDING") := NULL]

#------------------------------------------------------------------------------
#PC4 toevoegen
#------------------------------------------------------------------------------
#Bepalen locatie laatste dataset
pc4_locatie <- list.files("G:/BouwenWonen/VSLPOSTCODEBUS", pattern = ".sav", recursive = TRUE, full.names = TRUE)
pc4_locatie <- pc4_locatie[length(pc4_locatie)]

#Laatste/nieuwste bestand inlezen
pc4 <- read_spss(pc4_locatie)

pc4 <- as.data.table(pc4)

#Filteren
pc4[
  #Data omzetten in numeric om te kunnen filteren
  , c("DATUMAANVPOSTCODENUMADRES", "DATUMEINDPOSTCODENUMADRES") :=
    .(as.numeric(DATUMAANVPOSTCODENUMADRES), as.numeric(DATUMEINDPOSTCODENUMADRES))
  ]

#Rijen voor eerste jaar alvast weghalen
pc4 <- pc4[
  DATUMEINDPOSTCODENUMADRES >= as.numeric(str_c(min(jaren), "0630"))
  ]

#Functie aanmaken voor relevante jaren
pc4_filter <- function(jaar) {
  str_c("\"", jaar, "\" = ifelse(DATUMAANVPOSTCODENUMADRES <= ", jaar, "0630 & DATUMEINDPOSTCODENUMADRES >= ", jaar, "0630, TRUE, FALSE),")
}

#aanmaken string voor filter
pc4_string <- str_c(sapply(jaren, pc4_filter), collapse = " ")

pc4_string <- substr(pc4_string, 1, nchar(pc4_string) - 1)

#Filter runnen voor alle jaren
eval(parse(text=sprintf("pc4[,':=' (%s)]", pc4_string)))

#Data omzetten naar langer format om makkelijk te kunnen koppelen en filteren
pc4 <- melt(pc4, measure.vars = c(6:length(pc4)),
                variable.name = "jaar", value.name = "keep")

pc4[, c("DATUMAANVPOSTCODENUMADRES", "DATUMEINDPOSTCODENUMADRES") := NULL]

#Selecten van alleen regels met adressen
pc4 <- pc4[keep == TRUE]

#samenvoegen
locatie <- merge(locatie, pc4, all.x = TRUE, by = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", "jaar"))

setnames(locatie, "POSTCODENUM", "PC4")

rm(pc4)

#------------------------------------------------------------------------------
#gemeente toevoegen met VSLGWB
#------------------------------------------------------------------------------

#Bepalen locatie laatste dataset
vslgwb_locatie <- list.files("G:/BouwenWonen/VSLGWBTAB/", pattern = ".sav", recursive = TRUE, full.names = TRUE)
vslgwb_locatie <- vslgwb_locatie[length(vslgwb_locatie)]

vslgwb <- read_spss(vslgwb_locatie, col_select = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", gemeente_indeling))

vslgwb <- as.data.table(vslgwb)

locatie <- merge(locatie, vslgwb, by = c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER"),
                  all.x = TRUE)

#opruimen
locatie[, c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", "keep") := NULL]
locatie[, jaar := as.numeric(as.character(jaar))]
locatie[, PC4 := as.numeric(as.character(PC4))]

rm(vslgwb, filter_string, pc4_string, pc4_filter, gbaadres_locatie, vslgwb_locatie, pc4_locatie, locatie_filter)
