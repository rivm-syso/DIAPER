# Vroeggeboorte en/of een laag geboortegewicht voor de duur van de zwangerschap (Big2)

# Teller: het aantal kinderen geboren na een zwangerschapsduur van 24 weken of meer met 
# een geboortegewicht onder geboortegewichtpercentiel 10 (Hoftiezer et al., 2019) en/of een 
# zwangerschapsduur van minder dan 37 weken.

# Noemer: het aantal kinderen geboren na een zwangerschapsduur van 24 weken of meer.

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
big2 <- perined_data %>% 
  select(RINPERSOONS_Moeder, Rinpersoon_Moeder, RINPERSOONS_Kind, Rinpersoon_Kind,
         jaar, amww, hoftiezer, rangnummer)

#namen aanpassen
setnames(big2, c("amww"), c("zwngschp_duur_weken")) 

#langer dan 24 weken zwanger zijn
big2 <- big2[zwngschp_duur_weken >= 24]

#------------------------------------------------------------------------------
#locatie koppelen
#------------------------------------------------------------------------------
big2 <- merge(big2,
              locatie,
              all.x = TRUE,
              by.x = c("jaar", "RINPERSOONS_Moeder", "Rinpersoon_Moeder"),
              by.y = c("jaar", "RINPERSOONS", "RINPERSOON"))

#------------------------------------------------------------------------------
#kwetsbaarheid koppelen
#------------------------------------------------------------------------------
big2 <- merge(big2,
              kwetsbaarheid[, c("Rinpersoons_KIND", "RINPERSOON_KIND", "voorspelling_kwetsbaar")],
              all.x = TRUE,
              by.x = c("RINPERSOONS_Kind", "Rinpersoon_Kind"),
              by.y = c("Rinpersoons_KIND", "RINPERSOON_KIND"))

#------------------------------------------------------------------------------
#var aanmaken
#------------------------------------------------------------------------------
big2[, ':=' (
  vroeggeboorte = ifelse(zwngschp_duur_weken < 37, 1, 0),
  laag_gewicht = ifelse(hoftiezer < 10, 1, 0)
)]

big2[, big2 := ifelse(vroeggeboorte == 1 | laag_gewicht == 1, 1, 0)]

big2_gem <- big2[, .(
  vroeggeboorte = sum(vroeggeboorte, na.rm = TRUE),
  laag_gewicht = sum(laag_gewicht, na.rm = TRUE),
  big2 = sum(big2, na.rm = TRUE),
  totaal_big2 = .N
), by = c(gemeente_indeling, "jaar")]

big2_nl <- big2[, .(
  vroeggeboorte = sum(vroeggeboorte, na.rm = TRUE),
  laag_gewicht = sum(laag_gewicht, na.rm = TRUE),
  big2 = sum(big2, na.rm = TRUE),
  totaal_big2 = .N
), by = c("jaar")][, gem2022 := "NL01"]

output_big2 <- rbindlist(
  list(big2_gem, big2_nl),
  use.names = TRUE
)

#Cijfers onder de 10 op missing zetten
cols <- names(output_big2)[4:ncol(output_big2)]

output_big2[, (cols) := lapply(.SD, function(x) ifelse(x < 10, NA, x)), .SDcols = cols]

#percentage berekenen
output_big2[, ':=' (
  perc_vroeg = vroeggeboorte*100/totaal_big2,
  perc_lg = laag_gewicht*100/totaal_big2,
  perc_big2 = big2*100/totaal_big2
)]

#------------------------------------------------------------------------------
#kwetsbaarheid uitsplitsing
#------------------------------------------------------------------------------
big2_kwets <- big2[!is.na(voorspelling_kwetsbaar), .(
  vroeggeboorte = sum(vroeggeboorte, na.rm = TRUE),
  laag_gewicht = sum(laag_gewicht, na.rm = TRUE),
  big2 = sum(big2, na.rm = TRUE),
  totaal_big2 = .N
), by = c("jaar", "voorspelling_kwetsbaar")]

#Cijfers onder de 10 op missing zetten
cols <- names(big2_kwets)[4:ncol(big2_kwets)]

big2_kwets[, (cols) := lapply(.SD, function(x) ifelse(x < 10, NA, x)), .SDcols = cols]

big2_kwets[, ':=' (
  perc_vroeg = vroeggeboorte*100/totaal_big2,
  perc_lg = laag_gewicht*100/totaal_big2,
  perc_big2 = big2*100/totaal_big2
)]

big2_kwets <- arrange(big2_kwets, jaar, voorspelling_kwetsbaar)

#------------------------------------------------------------------------------
#output wegschrijven
#------------------------------------------------------------------------------
save_data(output_big2, "big2_landelijk_gem_lijn_gemeentelijk")
save_data(big2_kwets, "big2_lijn_kwetsbaarheid")
