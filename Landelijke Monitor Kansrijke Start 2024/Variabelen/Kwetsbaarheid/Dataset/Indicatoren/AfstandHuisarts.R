#Afstand huisarts

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#jaren
#jaren <- c(2015:2020)

#Gemeente indeling
#gemeente_indeling <- "gem2022"


#Bepalen locaties laatste dataset
data_locatie <- list.files("G:/Bevolking/GBAADRESOBJECTBUS", pattern = ".sav", recursive = TRUE, full.names = TRUE)

data_locatie <- data_locatie[length(data_locatie)]

locatie <- read_spss(data_locatie)
locatie <- data.table(locatie)


#data opschonen
locatie_filter <- function(jaar){
  str_c("\"", jaar, "\" = ifelse(GBADATUMAANVANGADRESHOUDING  <= ", jaar, "0630 & GBADATUMEINDEADRESHOUDING >= ", jaar, "0630, TRUE, FALSE),")
}

#Aanmaken string voor filter
filter_string <- str_c(sapply(jaren, locatie_filter), collapse = " ")
filter_string <- substr(filter_string, 1, nchar(filter_string) - 1)


#Filteren
locatie[
  #data omzetten in numeric om te filteren
  , c("GBADATUMAANVANGADRESHOUDING", "GBADATUMEINDEADRESHOUDING") := 
    .(as.numeric(GBADATUMAANVANGADRESHOUDING), as.numeric(GBADATUMEINDEADRESHOUDING))
  ]

#rijen voor eerste jaar alvast weghalen 
locatie <- locatie[
  GBADATUMEINDEADRESHOUDING >= as.numeric(str_c(min(jaren), "0630"))
]

#Filter runnen voor alle jaren
eval(parse(text=sprintf("locatie[,':=' (%s)]", filter_string)))

locatie <- melt(locatie, measure.vars = c(7:length(locatie)),
                variable.name = "jaar", value.name = "keep")

locatie <- locatie[keep == T]
locatie[, jaar := as.numeric(as.character(jaar))]

#kolommen verwijderen
locatie[, c("keep", "GBADATUMAANVANGADRESHOUDING", "GBADATUMEINDEADRESHOUDING") := NULL]


#Nabijheid zorg inladen
nabijheid_zorg <- inlezen_data("G:/BouwenWonen/NABIJHEIDZORGTAB", c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", "VZAANTHARTSPR03KM"))

#koppelen
nabijheid_zorg <- merge(locatie, nabijheid_zorg, all.x = T, by = c("jaar", "SOORTOBJECTNUMMER", "RINOBJECTNUMMER"))
nabijheid_zorg <- data.table(nabijheid_zorg)


#Data afstand HA 2021 nog niet aanwezig vandaar 2020 afstand tot huisarts pakken vanaf locatie
# nabijheid_zorg20 <- merge(locatie, nabijheid_zorg, all.x = T, by = c("jaar", "SOORTOBJECTNUMMER", "RINOBJECTNUMMER"))
# nabijheid_zorg20 <- data.table(nabijheid_zorg20)
# 
# nabijheid_zorg21  <- nabijheid_zorg %>% filter(jaar == 2020)
# nabijheid_zorg21[, jaar := 2021]
# nabijheid_zorg21 <- merge(nabijheid_zorg20, nabijheid_zorg21, all.x = T, by = c("jaar", "SOORTOBJECTNUMMER", "RINOBJECTNUMMER"))
# nabijheid_zorg <-  nabijheid_zorg21
# nabijheid_zorg <-  data.table(nabijheid_zorg)

#huisart binnen 3km
nabijheid_zorg[is.na(VZAANTHARTSPR03KM), VZAANTHARTSPR03KM := 0]
nabijheid_zorg[, HA3km := F]
nabijheid_zorg[VZAANTHARTSPR03KM > 0  , HA3km := T]
# nabijheid_zorg[VZAANTHARTSPR03KM.x > 0 | VZAANTHARTSPR03KM.y >0 , HA3km := T]

#schonen data
HA_zorg <- nabijheid_zorg[, c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER" ) := NULL]

#tijdelijk bestand opslaan 
#write.fst(HA_zorg, "H:/PP/temp_data/DT_nabijheid_HA.fst")


