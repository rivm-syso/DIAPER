#burgerlijke staat

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Jaren
#jaren <- c(2017:2019)

#------------------------------------------------------------------------------
#burgerlijke staat data
#------------------------------------------------------------------------------
#Bepalen locatie laatste dataset
burg_locatie <- list.files("G:/Bevolking/VRLGBABURGERLIJKESTAATBUS", pattern = ".sav", recursive = TRUE, full.names = TRUE)
burg_locatie <- burg_locatie[length(burg_locatie)]

#Laatste/nieuwste bestand inlezen
burgerlijke_staat <- read_spss(burg_locatie, 
                               col_select = c("RINPERSOONS", "RINPERSOON", "VRLGBAAANVANGBURGERLIJKESTAAT",
                                              "VRLGBAEINDEBURGERLIJKESTAAT", "VRLGBABURGERLIJKESTAATNW"))

burgerlijke_staat <- as.data.table(burgerlijke_staat)

#------------------------------------------------------------------------------
#bepalen per jaar
#------------------------------------------------------------------------------
#Data opschonen
#Functie aanmaken voor relevante jaren
staat_filter <- function(jaar) {
  str_c("\"", jaar, "\" = ifelse(VRLGBAAANVANGBURGERLIJKESTAAT <= ", jaar, "0630 & VRLGBAEINDEBURGERLIJKESTAAT >= ", jaar, "0630, TRUE, FALSE),")
}

#aanmaken string voor filter
filter_string <- str_c(sapply(jaren, staat_filter), collapse = " ")

filter_string <- substr(filter_string, 1, nchar(filter_string) - 1)

#Filteren
burgerlijke_staat[
  #Data omzetten in numeric om te kunnen filteren
  , c("VRLGBAAANVANGBURGERLIJKESTAAT", "VRLGBAEINDEBURGERLIJKESTAAT") :=
    .(as.numeric(VRLGBAAANVANGBURGERLIJKESTAAT), as.numeric(VRLGBAEINDEBURGERLIJKESTAAT))
  ]

#Rijen voor eerste jaar alvast weghalen
burgerlijke_staat <- burgerlijke_staat[
  VRLGBAEINDEBURGERLIJKESTAAT >= as.numeric(str_c(min(jaren), "0630"))
  ]

#Filter runnen voor alle jaren
eval(parse(text=sprintf("burgerlijke_staat[,':=' (%s)]", filter_string)))

#Data omzetten naar langer format om makkelijk te kunnen koppelen en filteren
burgerlijke_staat <- melt(burgerlijke_staat, measure.vars = c(6:length(burgerlijke_staat)),
                          variable.name = "jaar", value.name = "keep")

#Selecten van alleen regels met adressen
burgerlijke_staat <- burgerlijke_staat[keep == TRUE]

#Kolommen verwijderen
burgerlijke_staat[, c("keep", "VRLGBAAANVANGBURGERLIJKESTAAT", "VRLGBAEINDEBURGERLIJKESTAAT") := NULL]

#------------------------------------------------------------------------------
#opschonen
#------------------------------------------------------------------------------
burgerlijke_staat[, "burgerlijk_staat" := unlabelled(VRLGBABURGERLIJKESTAATNW)]

burgerlijke_staat[, VRLGBABURGERLIJKESTAATNW := NULL]

burgerlijke_staat[, jaar := as.numeric(as.character(jaar))]
