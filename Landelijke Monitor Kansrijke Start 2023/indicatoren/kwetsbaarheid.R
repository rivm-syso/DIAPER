# Kinderen geboren in een gezin in een kwetsbare situatie

# Teller: het aantal levendgeborenen waarvan de moeder op basis van een combinatie van 
# verschillende aanwezige risicofactoren en een gebrek aan beschermende factoren (benoemd 
# onder Figuur 18) te maken heeft met meervoudige kwetsbaarheid.

# Noemer: het aantal levendgeborenen

#utilities
source("utils.R")

#------------------------------------------------------------------------------
#data inlezen
#------------------------------------------------------------------------------
source("inlezen_data/locatie.R")
source("inlezen_data/perined.R")

kwetsbaarheid <- read.csv2("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/meervoudig_kwetsbaar.csv") %>% 
  mutate(RINPERSOON_KIND = as.character(RINPERSOON_KIND))

#------------------------------------------------------------------------------
#data koppelen
#------------------------------------------------------------------------------
kwetsbaar_geboren <- merge(kwetsbaarheid[, c("Rinpersoons_KIND", "RINPERSOON_KIND", "voorspelling_kwetsbaar",
                                             "RINPERSOONS", "RINPERSOON")],
                           perined_data[, c("RINPERSOONS_Kind", "Rinpersoon_Kind", "jaar")],
                           all.x = TRUE,
                           by.x = c("Rinpersoons_KIND", "RINPERSOON_KIND"),
                           by.y = c("RINPERSOONS_Kind", "Rinpersoon_Kind"))

kwetsbaar_geboren <- merge(kwetsbaar_geboren, locatie, all.x = TRUE,
                           by = c("jaar", "RINPERSOONS", "RINPERSOON"))

kwetsbaar_geboren <- as.data.table(kwetsbaar_geboren)

#------------------------------------------------------------------------------
#var berekenen
#------------------------------------------------------------------------------
kwetsbaar_geboren[, kwetsbaar := ifelse(voorspelling_kwetsbaar == "ja", 1, 0)]

kwets_gemeente <- kwetsbaar_geboren[!is.na(voorspelling_kwetsbaar), .(
  kwetsbaar = sum(kwetsbaar, na.rm = TRUE),
  totaal = .N
), by = c(gemeente_indeling, "jaar")]

kwets_nl <- kwetsbaar_geboren[!is.na(voorspelling_kwetsbaar), .(
  kwetsbaar = sum(kwetsbaar, na.rm = TRUE),
  totaal = .N
), by = c("jaar")][, gem2022 := "NL01"]

kwets_output <- rbindlist(
  list(kwets_gemeente, kwets_nl),
  use.names = TRUE
)

#Cijfers onder de 10 op missing zetten
cols <- names(kwets_output)[4:ncol(kwets_output)]

kwets_output[, (cols) := lapply(.SD, function(x) ifelse(x < 10, NA, x)), .SDcols = cols]

#percentage berekenen
kwets_output[, perc := kwetsbaar*100/totaal]

#------------------------------------------------------------------------------
#output wegschrijven
#------------------------------------------------------------------------------
save_data(kwets_output, "kwetsbaarheid_landelijk_gemeentelijk")
