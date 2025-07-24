#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Jaren
jaren <- c(2015:2022)

#locatie indeling
gemeente_indeling <- "gem2023"

#------------------------------------------------------------------------------
#CBS data
#------------------------------------------------------------------------------
source("src/indicatoren/locatie.R")

#------------------------------------------------------------------------------
#kraamzorgmoeders Vektis
#------------------------------------------------------------------------------
#inlezen tot 2020

#inlezen vektis dat met benodigde kolommen voor kraamzorg
gb_zorg_moeders_tot_2020 <- read_spss("L:/8099Vektis_04Geboortezorgmoedersdetails_JulyCBKV1.sav",
                                      col_select = c("RinPersoons", "Rinpersoon", "beginmaand_prestatie", "omschrijving"))

gb_zorg_moeders_tot_2020 <- as.data.table(gb_zorg_moeders_tot_2020)
setnames(gb_zorg_moeders_tot_2020, c("RinPersoons", "Rinpersoon"), c("RINPERSOONS", "RINPERSOON"))

#inlezen 2021 en 2022

#inlezen vektis dat met benodigde kolommen voor kraamzorg
gb_zorg_moeders_2021_2022 <- read_spss("L:/8099Vektis_04Geboortezorgmoedersdetails202312CBKV1.sav",
                                       col_select = c("RINPERSOONS", "Rinpersoon", "beginmaand_prestatie", "omschrijving"))

gb_zorg_moeders_2021_2022 <- as.data.table(gb_zorg_moeders_2021_2022)
setnames(gb_zorg_moeders_2021_2022, c("Rinpersoon"), c("RINPERSOON"))

#data samenvoegen
vektis_koppel <- rbindlist(
  list(gb_zorg_moeders_tot_2020, gb_zorg_moeders_2021_2022),
  use.names = TRUE
)

vektis_koppel <- vektis_koppel[, ':=' (
  jaar = as.numeric(str_sub(beginmaand_prestatie, 1, 4))
)]

#------------------------------------------------------------------------------
#data koppelen
#------------------------------------------------------------------------------
vektis_consult <- merge(
  vektis_koppel,
  locatie,
  all.x = TRUE,
  by = c("jaar", "RINPERSOON", "RINPERSOONS")
)

#------------------------------------------------------------------------------
#var berekenen
#------------------------------------------------------------------------------
#data bekijken
table(vektis_consult$omschrijving)

#variabelen voor consulten aanmaken
vektis_consult[, preconceptieconsult := ifelse(str_detect(omschrijving, "Preconceptie"), 1, 0)]

#vaststellen welke precon hebben gehad tov totaal aantal moeders
results_consult <- merge(
  unique(vektis_consult[, c("RINPERSOONS", "RINPERSOON", "jaar", "gem2023")]), #alle unieke bevallingen per traject per jaar
  unique(vektis_consult[preconceptieconsult == 1, c("RINPERSOONS", "RINPERSOON", "jaar", "gem2023", "preconceptieconsult")]), #alle moeders die kraamzorg hebben gekregen per traject per jaar
  all.x = TRUE,
  by = c("RINPERSOONS", "RINPERSOON", "jaar", "gem2023")
)

#als een moeder NA heeft, dan heeft ze dus geen precon gehad
results_consult[, ':=' (
  preconceptieconsult = ifelse(is.na(preconceptieconsult), 0 , preconceptieconsult)
)]

#aantallen per gemeente berekenen
results_precon <- results_consult[, .(
  preconceptieconsult = sum(preconceptieconsult), 
  totaal = .N
), by = c("jaar", "gem2023")
]
  
#regio's toevoegen
regios_mapping <- read.csv2("H:/Data proces/data/koppel tabellen/20240220_map_gem_regio_gem2023.csv")

results_precon <- results_precon %>%
  rename(geo_id = gem2023) %>% 
  mutate(geo_id = str_c("GM", geo_id))

results_precon_regio <- results_precon %>% 
  left_join(regios_mapping, relationship = "many-to-many") %>% 
  select(-geo_id) %>% 
  rename(geo_id = geo_id_map) %>% 
  group_by(geo_id, jaar) %>% 
  summarise(across(everything(), sum))

results_precon <- rbind(results_precon, results_precon_regio)

#onder de 10 op missing zetten
results_precon[, preconceptieconsult := ifelse(preconceptieconsult < 10, NA, preconceptieconsult )]
results_precon[, totaal := ifelse(totaal < 10, NA, totaal )]

results_precon[, perc := (preconceptieconsult / totaal) * 100]

#------------------------------------------------------------------------------
#export
#------------------------------------------------------------------------------
write.csv2(results_precon, "H:/Data proces/src/projecten/KS Basisset/20240305_preconceptie_gem2023.csv")
