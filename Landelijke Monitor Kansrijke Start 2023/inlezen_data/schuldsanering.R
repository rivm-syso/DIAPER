#schuldsanering

#Utilities
source("utils.R")

#------------------------------------------------------------------------------
#schuldsanering data
#------------------------------------------------------------------------------
#Bepalen locatie laatste dataset
schuld_locatie <- list.files("G:/VeiligheidRecht/WSNPPERSBUS", pattern = ".sav", recursive = TRUE, full.names = TRUE)
schuld_locatie <- schuld_locatie[length(schuld_locatie)] 

#Laatste/nieuwste bestand inlezen
schuldsanering <- read_spss(schuld_locatie)

schuldsanering <- as.data.table(schuldsanering)

schuldsanering[, c("Wsnprbnk", "Wsnpsrtbeein", "Wsnpbewid") := NULL]

#Data opschonen
#Functie aanmaken voor relevante jaren
schuld_filter <- function(jaar) {
  str_c("\"", jaar, "\" = ifelse(Wsnpdatumtoelaat <= ", jaar, " & Wsnpdatumbeein >= ", jaar, ", TRUE, FALSE),")
}

#aanmaken string voor filter
filter_string <- str_c(sapply(jaren, schuld_filter), collapse = " ")

filter_string <- substr(filter_string, 1, nchar(filter_string) - 1)

#Filteren
schuldsanering[
  #Data in alleen jaren omzetten en numeric om te kunnen filteren
  , ":=" (
    Wsnpdatumtoelaat = as.numeric(str_sub(Wsnpdatumtoelaat, 1, 4)),
    Wsnpdatumbeein = as.numeric(str_sub(Wsnpdatumbeein, 1, 4))
  )]

#Rijen voor eerste jaar alvast weghalen
schuldsanering <- schuldsanering[
  Wsnpdatumbeein >= min(jaren)
]

#Filter runnen voor alle jaren
eval(parse(text=sprintf("schuldsanering[,':=' (%s)]", filter_string)))

#Data omzetten naar langer format om makkelijk te kunnen koppelen en filteren
schuldsanering <- melt(schuldsanering, measure.vars = c(5:length(schuldsanering)),
                       variable.name = "jaar", value.name = "keep")

#Selecten van alleen regels met adressen
schuldsanering <- schuldsanering[keep == TRUE]

#afwerken
schuldsanering[, c("keep", "Wsnpdatumtoelaat", "Wsnpdatumbeein") := NULL]

schuldsanering[, jaar := as.numeric(as.character(jaar))]

schuldsanering[, schuldsanering := 1]
