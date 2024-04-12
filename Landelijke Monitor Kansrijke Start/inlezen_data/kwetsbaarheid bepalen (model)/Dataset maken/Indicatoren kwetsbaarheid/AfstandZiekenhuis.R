#Afstand Ziekenhuis

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
nabijheid_zorg <- inlezen_data("G:/BouwenWonen/NABIJHEIDZORGTAB", c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", "VZAFSTANDZIEKHEXCLBP",  "VZAANTZIEKHEXCLBP05KM"))


#Data afstand HA 2021 nog niet aanwezig vandaar 2020 afstand tot huisarts pakken vanaf locatie
nabijheid_zorg20 <- merge(locatie, nabijheid_zorg, all.x = T, by = c("jaar", "SOORTOBJECTNUMMER", "RINOBJECTNUMMER"))
nabijheid_zorg20 <- data.table(nabijheid_zorg20)

nabijheid_zorg21  <- nabijheid_zorg %>% filter(jaar == 2020)
nabijheid_zorg21[, jaar := 2021]
nabijheid_zorg21 <- merge(nabijheid_zorg20, nabijheid_zorg21, all.x = T, by = c("jaar", "SOORTOBJECTNUMMER", "RINOBJECTNUMMER"))
nabijheid_zorg <-  nabijheid_zorg21
nabijheid_zorg <-  data.table(nabijheid_zorg)

nabijheid_zorg[jaar < 2021, VZAANTZIEKHEXCLBP05KM := VZAANTZIEKHEXCLBP05KM.x  ]
nabijheid_zorg[jaar < 2021, VZAFSTANDZIEKHEXCLBP := VZAFSTANDZIEKHEXCLBP.x  ]
nabijheid_zorg[jaar == 2021, VZAANTZIEKHEXCLBP05KM := VZAANTZIEKHEXCLBP05KM.y  ]
nabijheid_zorg[jaar == 2021, VZAFSTANDZIEKHEXCLBP := VZAFSTANDZIEKHEXCLBP.y  ]


#Ziekenhuis binnen 5km
nabijheid_zorg[VZAANTZIEKHEXCLBP05KM == 0, ZH5km := F]
nabijheid_zorg[VZAANTZIEKHEXCLBP05KM > 0, ZH5km := T]

#schonen data
ZH_afstand <- nabijheid_zorg[, c("SOORTOBJECTNUMMER", "RINOBJECTNUMMER", "VZAANTZIEKHEXCLBP05KM.x",  "VZAANTZIEKHEXCLBP05KM.y",  "VZAFSTANDZIEKHEXCLBP.x",  "VZAFSTANDZIEKHEXCLBP.y") := NULL]




rm( nabijheid_zorg)

