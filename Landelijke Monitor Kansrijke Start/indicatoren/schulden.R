# Vrouwen met problematische schulden in het jaar van de bevalling

# Teller: het aantal vrouwen bevallen na een zwangerschap van 24 weken of meer, met in het 
# jaar van bevalling een registratie in de schuldsanering en/of met een betalingsachterstand 
# van zes maanden of meer bij de zorgverzekeraar.

# Noemer: het aantal vrouwen bevallen na een zwangerschap van 24 weken of meer

#utilities
source("utils.R")

#------------------------------------------------------------------------------
#data inlezen
#------------------------------------------------------------------------------
source("inlezen_data/perined.R")
source("inlezen_data/locatie.R")
source("inlezen_data/schuldsanering.R")
source("inlezen_data/zvw_wanbetaler.R")

kwetsbaarheid <- read.csv2("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/meervoudig_kwetsbaar.csv") %>% 
  mutate(RINPERSOON_KIND = as.character(RINPERSOON_KIND))

#------------------------------------------------------------------------------
#data koppelen
#------------------------------------------------------------------------------
schulden <- Reduce(function(d1, d2) merge(d1 ,d2, all.x = TRUE, 
                                          by.x = c("jaar", "RINPERSOONS_Moeder", "Rinpersoon_Moeder"),
                                          by.y = c("jaar", "RINPERSOONS", "RINPERSOON")),
                   list(perined_data, locatie, schuldsanering, zvw_wanbetaler))

schulden <- merge(schulden,
                  kwetsbaarheid[, c("Rinpersoons_KIND", "RINPERSOON_KIND", "voorspelling_kwetsbaar")],
                  all.x = TRUE,
                  by.x = c("RINPERSOONS_Kind", "Rinpersoon_Kind"),
                  by.y = c("Rinpersoons_KIND", "RINPERSOON_KIND"))


#------------------------------------------------------------------------------
#var berekenen
#------------------------------------------------------------------------------
schulden[, ':='(
  schulden = ifelse(schuldsanering == 1 | zvw_wanbetaler == 1, 1, 0),
  schulsanering = ifelse(schuldsanering == 1, 1, 0),
  zvw_wanbetaler = ifelse(zvw_wanbetaler == 1, 1, 0))]

#------------------------------------------------------------------------------
#variabelen berekenen
#------------------------------------------------------------------------------
schulden_nl <- schulden[, .(
  schulden = sum(schulden, na.rm = TRUE),
  totaal = .N
), by = c("jaar")][, gem2022 := "NL01"]

schulden_nl[, schulden := ifelse(schulden < 10, NA, schulden)]

#percentage berekenen
schulden_nl[, perc := schulden*100/totaal]

#5 jarige gemiddelden op gemeente niveau voor laatste jaar
schulden_gem5 <- schulden[
  jaar %in% c((max(jaren) - 4): max(jaren)), #laatste 5 jaar selecteren
    .(
      schulden = sum(schulden, na.rm = TRUE),
      totaal = .N
    ), by = c(gemeente_indeling)]

schulden_gem5[, schulden := ifelse(schulden < 10, NA, schulden)]
schulden_gem5[, perc := (schulden / totaal) * 100]

#------------------------------------------------------------------------------
#kwetsbaarheid uitsplitsing
#------------------------------------------------------------------------------
schulden_kwets <- schulden[!is.na(voorspelling_kwetsbaar), .(
  schulden = sum(schulden, na.rm = TRUE),
  totaal = .N
), by = c("jaar", "voorspelling_kwetsbaar")]

schulden_kwets[, schulden := ifelse(schulden < 10, NA, schulden)]

schulden_kwets <- arrange(schulden_kwets, jaar, 
                          voorspelling_kwetsbaar) %>% 
  mutate(perc = schulden*100/totaal)

#------------------------------------------------------------------------------
#output wegschrijven
#------------------------------------------------------------------------------
save_data(schulden_nl, "schulden_lijn_landelijk")
save_data(schulden_gem5, "schulden_lijn_gemeentelijk5")
save_data(schulden_kwets, "schulden_lijn_kwetsbaarheid")
