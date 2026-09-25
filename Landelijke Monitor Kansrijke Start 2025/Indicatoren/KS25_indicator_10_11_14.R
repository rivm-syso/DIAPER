# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Indicator 14
# kwetsbaarheid
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
library(xlsx)
library(data.table)
library(fst)
library(dplyr)
library(tidyr)

setwd("H:/Data proces/src/projecten/KS Basisset/Landelijke monitor 2025")

#jaren
jaren <- c(2017:2024)

#------------------------------------------------------------------------------
# Inlezen KS_2025 dataset met Perined
#------------------------------------------------------------------------------
KS_2025 <- read.fst("H:/Data proces/src/projecten/KS Basisset/Landelijke monitor 2025/Data/KS2025_dataset_perined.fst")
KS_2025 <- data.table(KS_2025)

DT <- data.table(KS_2025)

#------------------------------------------------------------------------------
# Checks of het goed is gegaan
#------------------------------------------------------------------------------

DT %>% count(jaar)
DT[ is.na(RINPERSOONS_Kind) ] %>% count(jaar)
DT[ is.na(RINPERSOONS_Moeder) ] %>% count(jaar)

DT %>% count(jaar, RINPERSOONS_Kind)
DT %>% count(sterfte, RINPERSOONS_Kind)

DT %>% count(jaar, voorspelling_kwetsbaar)
DT %>% count(jaar, RINPERSOONS_Kind, voorspelling_kwetsbaar)

#######################
###
### Definities
###
######################
DT <- DT %>% mutate(schuldsanering_ouders = ifelse(schuldsanering_Ma == "1" | schuldsanering_Pa == "1", 1,0),
                              zvw_wanbetaler_ouders  = ifelse(zvw_wanbetaler_Ma  == "1" | zvw_wanbetaler_Pa  == "1", 1,0),
                              schulden_ouders  = ifelse(schulden_Ma  == "1" | schulden_Pa  == "1", 1,0),
                              kosten_GGZ_ouders = ifelse(kosten_GGZ_Ma  == "1" | kosten_GGZ_Pa  == "1", 1,0),
                              psy_medicatie_ouders = ifelse(med_psych_klachten_Ma  == "1" | med_psych_klachten_Pa  == "1", 1,0),
                              psy_problematiek_ouders = ifelse(psy_problematiek_Ma   == "1" | psy_problematiek_Pa  == "1", 1,0))


###############################
###
### Indicator 10 - Schulden
###
###############################

names(DT)

DT %>% count(jaar, schulden_ouders)

### Tellingen

out <- DT %>% group_by(jaar, schulden_ouders) %>%
  summarise(
    Aantal = n()
  ) %>%
  # mutate(percentage = round(Aantal / sum(Aantal) * 100, 3)) %>%
  ungroup() %>%
  pivot_wider(names_from = jaar, values_from = Aantal )

write.xlsx(out, file = "./Output/KS2025_indicator_10_schulden_ouders.xlsx", append = FALSE, sheetName = "Fig1_trend")

out <- DT %>% group_by(jaar, voorspelling_kwetsbaar, schulden_ouders) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= jaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_10_schulden_ouders.xlsx", append = TRUE, sheetName = "Fig1_trend_kwetsbaar")

# Tellingen per gemeente

out <- DT[ jaar %in% c(2020:2024)] %>% group_by(gem2023, schulden_ouders) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= schulden_ouders, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_10_schulden_ouders.xlsx", append = TRUE, sheetName = "Fig2_per_gemeente_2020_2024")

out <- DT %>% group_by(jaar, ink_kwint_Ma, schulden_ouders) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= jaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_10_schulden_ouders.xlsx", append = TRUE, sheetName = "Fig3_Huishoudinkomen")

out <- DT %>% group_by(jaar, opleidingsniveau_Ma, schulden_ouders) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= jaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_10_schulden_ouders.xlsx", append = TRUE, sheetName = "Fig4_Opleidingsniveau moeder")


###############################
###
### Indicator 11 - psy_problematiek_ouders
### let op, aantallen 2024 nog niet compleet
###
###############################

names(DT)

DT %>% count(jaar, psy_problematiek_ouders)

### Tellingen

out <- DT[jaar < 2024] %>% group_by(jaar, psy_problematiek_ouders) %>%
  summarise(
    Aantal = n()
  ) %>%
  # mutate(percentage = round(Aantal / sum(Aantal) * 100, 3)) %>%
  ungroup() %>%
  pivot_wider(names_from = jaar, values_from = Aantal )

write.xlsx(out, file = "./Output/KS2025_indicator_11_psy_problematiek_ouders.xlsx", append = FALSE, sheetName = "Fig1_trend")

out <- DT[jaar < 2024] %>% group_by(jaar, voorspelling_kwetsbaar, psy_problematiek_ouders) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= jaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_11_psy_problematiek_ouders.xlsx", append = TRUE, sheetName = "Fig1_trend_kwetsbaar")

# Tellingen per gemeente

out <- DT[ jaar %in% c(2019:2023)] %>% group_by(gem2023, psy_problematiek_ouders) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= psy_problematiek_ouders, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_11_psy_problematiek_ouders.xlsx", append = TRUE, sheetName = "Fig2_per_gemeente_2019_2023")

out <- DT[jaar < 2024] %>% group_by(jaar, ink_kwint_Ma, psy_problematiek_ouders) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= jaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_11_psy_problematiek_ouders.xlsx", append = TRUE, sheetName = "Fig3_Huishoudinkomen")

out <- DT[jaar < 2024] %>% group_by(jaar, opleidingsniveau_Ma, psy_problematiek_ouders) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= jaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_11_psy_problematiek_ouders.xlsx", append = TRUE, sheetName = "Fig4_Opleidingsniveau moeder")



###############################
###
### Indicator 14 - kwetsbaarheid 
###
###############################

names(DT)

DT %>% count(jaar, voorspelling_kwetsbaar)

### Tellingen

out <- DT %>% group_by(jaar, voorspelling_kwetsbaar) %>%
  summarise(
    Aantal = n()
  ) %>%
  # mutate(percentage = round(Aantal / sum(Aantal) * 100, 3)) %>%
  ungroup() %>%
  pivot_wider(names_from = jaar, values_from = Aantal )

write.xlsx(out, file = "./Output/KS2025_indicator_14_kwetsbaarheid.xlsx", append = FALSE, sheetName = "Fig1_trend")

# Tellingen per gemeente

out <- DT[ jaar %in% c(2020:2024)] %>% group_by(gem2023, voorspelling_kwetsbaar) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= voorspelling_kwetsbaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_14_kwetsbaarheid.xlsx", append = TRUE, sheetName = "Fig2_per_gemeente_2020_2024")

out <- DT[ jaar == 2024] %>% group_by(gem2023, voorspelling_kwetsbaar) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= voorspelling_kwetsbaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_14_kwetsbaarheid.xlsx", append = TRUE, sheetName = "Fig2_per_gemeente_2024")
