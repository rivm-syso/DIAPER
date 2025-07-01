# Uithuisplaatsingen van kinderen tot de leeftijd van twee jaar

# Teller: het aantal kinderen tot twee jaar dat op enig moment ten minste een dag een jeugdbeschermingsmaatregel 
# heeft ontvangen, overlappend met jeugdhulp met verblijf.

# Noemer: het aantal kinderen tot twee jaar

#utilities
source("utils.R")

#------------------------------------------------------------------------------
#data inlezen
#------------------------------------------------------------------------------
source("inlezen_data/locatie.R")
source("inlezen_data/leeftijd.R")
source("inlezen_data/uithuisplaatsing.R")

kwetsbaarheid <- read.csv2("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/meervoudig_kwetsbaar.csv") %>% 
  mutate(RINPERSOON_KIND = as.character(RINPERSOON_KIND))

#------------------------------------------------------------------------------
#doelgroep aanmaken, kinderen tm 2 selecteren
#------------------------------------------------------------------------------
pop_tot_2 <- leeftijd[, c("RINPERSOONSkind", "RINPERSOONkind", "gb_kind",
                          "RINPERSOONSpa", "RINPERSOONpa", "RINPERSOONSMa", "RINPERSOONMa")]

#onnodige rijen weggooien, langer dan twee jaar geleden onder het eerste jaar
min_datum <- str_c(min(jaren) - 2, "1231")

pop_tot_2 <- pop_tot_2[gb_kind >= as.Date(min_datum, format = "%Y%m%d")]

#opschonen naar jaar
pop_tot_2[, geboorte_jaar := as.numeric(year(gb_kind))]

#loopen over de jaren om de doelgroep aan te maken
doelgroep_tot_2_selecteren <- function(x) {
  data <- pop_tot_2
  
  data[, ':=' (
    jaar = x,
    keep = ifelse(geboorte_jaar %in% c(x - 1, x), TRUE, FALSE)
  )]
  
  data <- data[keep == TRUE] #filteren
  data[, keep := NULL] #filter var opruimen
  
  return(data)
}

pop_tot_2 <- rbindlist(
  lapply(jaren, doelgroep_tot_2_selecteren)
)

#------------------------------------------------------------------------------
#koppelen data
#------------------------------------------------------------------------------
pop_tot_2 <- merge(pop_tot_2, locatie, all.x = TRUE, 
                   by.x = c("jaar", "RINPERSOONSMa", "RINPERSOONMa"),
                   by.y = c("jaar", "RINPERSOONS", "RINPERSOON"))

pop_tot_2 <- merge(pop_tot_2, uithuis, all.x = TRUE, 
                   by.x = c("jaar", "RINPERSOONSkind", "RINPERSOONkind"),
                   by.y = c("jaar", "RINPERSOONS", "RINPERSOON"))

pop_tot_2 <- merge(pop_tot_2,
                   kwetsbaarheid[, c("Rinpersoons_KIND", "RINPERSOON_KIND", "voorspelling_kwetsbaar")],
                   all.x = TRUE,
                   by.x = c("RINPERSOONSkind", "RINPERSOONkind"),
                   by.y = c("Rinpersoons_KIND", "RINPERSOON_KIND"))

#------------------------------------------------------------------------------
#variabelen berekenen
#------------------------------------------------------------------------------
pop_tot_2[, uithuis := ifelse(is.na(uithuis), 0, 1)]

#landelijke cijfers per jaar
uithuisplaatsing_nl <- pop_tot_2[!is.na(RINPERSOONpa) | !is.na(RINPERSOONMa), .(
  uithuis = sum(uithuis, na.rm = TRUE),
  totaal = .N
), by = c("jaar")]

uithuisplaatsing_nl[, per_1000 := (uithuis / totaal) * 1000]

#5 jarige gemiddelden op gemeente niveau voor laatste jaar
uithuisplaatsing_gem5 <- pop_tot_2[
  jaar %in% c((max(jaren) - 4): max(jaren)) & #laatste 5 jaar selecteren
  (!is.na(RINPERSOONpa) | !is.na(RINPERSOONMa)), .(
  uithuis = sum(uithuis, na.rm = TRUE),
  totaal = .N
), by = c(gemeente_indeling)]

uithuisplaatsing_gem5[, uithuis := ifelse(uithuis < 10, NA, uithuis)]
uithuisplaatsing_gem5[, per_1000 := (uithuis / totaal) * 1000]

#------------------------------------------------------------------------------
#kwetsbaarheid uitsplitsing
#------------------------------------------------------------------------------
#landelijke cijfers per jaar
uithuisplaatsing_kwets <- pop_tot_2[(!is.na(RINPERSOONpa) | !is.na(RINPERSOONMa)) & !is.na(voorspelling_kwetsbaar), .(
  uithuis = sum(uithuis, na.rm = TRUE),
  totaal = .N
), by = c("jaar", "voorspelling_kwetsbaar")]

uithuisplaatsing_kwets <- arrange(uithuisplaatsing_kwets, jaar, voorspelling_kwetsbaar) %>% 
  mutate(per_1000 = uithuis*1000/totaal)

#------------------------------------------------------------------------------
#export
#------------------------------------------------------------------------------
save_data(uithuisplaatsing_nl, "uithuisplaatsing_landelijk")
save_data(uithuisplaatsing_gem5, str_c("uithuisplaatsing_gemeente_", max(jaren)))
save_data(uithuisplaatsing_kwets, "uithuisplaatsing_kwets")

#------------------------------------------------------------------------------
#opruimen
#------------------------------------------------------------------------------
rm(list = ls())
