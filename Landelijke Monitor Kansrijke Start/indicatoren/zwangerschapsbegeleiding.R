# Start zwangerschapsbegeleiding ná 10e week zwangerschap

# Teller: het aantal zwangerschappen van 24 weken of meer waarbij zwangerschapsbegeleiding 
# startte na de 10e week van de zwangerschap**.

# Noemer: het aantal zwangerschappen van 24 weken of meer.

#utilities
source("utils.R")

#------------------------------------------------------------------------------
#data inlezen
#------------------------------------------------------------------------------
source("inlezen_data/perined.R")
source("inlezen_data/locatie.R")

kwetsbaarheid <- read.csv2("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/meervoudig_kwetsbaar.csv") %>% 
  mutate(RINPERSOON_KIND = as.character(RINPERSOON_KIND))

#------------------------------------------------------------------------------
#var selecteren en doelgroep selecteren
#------------------------------------------------------------------------------
zwangerschapbegeleiding <- perined_data %>% 
  select(RINPERSOONS_Moeder, Rinpersoon_Moeder, RINPERSOONS_Kind, Rinpersoon_Kind,
         jaar, amddd1ond, rangnummer, amww)

#namen aanpassen
setnames(zwangerschapbegeleiding,
         c("amww", "amddd1ond"),
         c("zwngschp_duur_weken", "dag_eerst_onderzoek"))

#unieke moeders die langer dan 24 weken zwanger zijn
zwangerschapbegeleiding <- unique(zwangerschapbegeleiding[zwngschp_duur_weken >= 24 & rangnummer == 1], 
                                  by = c("jaar", "RINPERSOONS_Moeder", "Rinpersoon_Moeder"))

#------------------------------------------------------------------------------
#locatie koppelen
#------------------------------------------------------------------------------
zwangerschapbegeleiding <- merge(zwangerschapbegeleiding,
                                 locatie,
                                 all.x = TRUE,
                                 by.x = c("jaar", "RINPERSOONS_Moeder", "Rinpersoon_Moeder"),
                                 by.y = c("jaar", "RINPERSOONS", "RINPERSOON"))

#------------------------------------------------------------------------------
#kwetsbaarheid koppelen
#------------------------------------------------------------------------------
zwangerschapbegeleiding <- merge(zwangerschapbegeleiding,
                                 kwetsbaarheid[, c("Rinpersoons_KIND", "RINPERSOON_KIND", "voorspelling_kwetsbaar")],
                                 all.x = TRUE,
                                 by.x = c("RINPERSOONS_Kind", "Rinpersoon_Kind"),
                                 by.y = c("Rinpersoons_KIND", "RINPERSOON_KIND"))

#------------------------------------------------------------------------------
#variabelen berekenen
#------------------------------------------------------------------------------
zwangerschapbegeleiding[, "begeleiding_na_10w" := ifelse(dag_eerst_onderzoek > 70, 1, 0)]

zb_gemeente <- zwangerschapbegeleiding[, .(
  begeleiding_na_10w = sum(begeleiding_na_10w, na.rm = TRUE),
  totaal = .N
), by = c(gemeente_indeling, "jaar")]

zb_nl <- zwangerschapbegeleiding[, .(
  begeleiding_na_10w = sum(begeleiding_na_10w, na.rm = TRUE),
  totaal = .N
), by = c("jaar")][, gem2022 := "NL01"]

zb_output <- rbindlist(
  list(zb_gemeente, zb_nl),
  use.names = TRUE
)

#Cijfers onder de 10 op missing zetten
cols <- names(zb_output)[4:ncol(zb_output)]

zb_output[, (cols) := lapply(.SD, function(x) ifelse(x < 10, NA, x)), .SDcols = cols]

#percentage berekenen
zb_output[, perc := begeleiding_na_10w*100/totaal]

#------------------------------------------------------------------------------
#kwetsbaarheid uitsplitsing
#------------------------------------------------------------------------------
zb_kwets <- zwangerschapbegeleiding[!is.na(voorspelling_kwetsbaar), .(
  begeleiding_na_10w = sum(begeleiding_na_10w, na.rm = TRUE),
  totaal = .N
), by = c("jaar", "voorspelling_kwetsbaar")]

#Cijfers onder de 10 op missing zetten
cols <- names(zb_kwets)[4:ncol(zb_kwets)]

zb_kwets[, (cols) := lapply(.SD, function(x) ifelse(x < 10, NA, x)), .SDcols = cols]

zb_kwets <- arrange(zb_kwets, jaar, voorspelling_kwetsbaar) %>% 
  mutate(perc = begeleiding_na_10w*100/totaal)

#------------------------------------------------------------------------------
#output wegschrijven
#------------------------------------------------------------------------------
save_data(zb_output, "zwangerschapsbegeleiding_lijn_landelijk_gemeentelijk")
save_data(zb_kwets, "zwangerschapsbegeleiding_lijn_kwetsbaarheid")
