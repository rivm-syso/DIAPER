# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Indicator 9
# Start zwangerschapsbegeleiding na de 10e week zwangerschap
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
library(xlsx)
library(data.table)
library(fst)
library(dplyr)

setwd("H:/Data proces/src/projecten/KS Basisset/Landelijke monitor 2025")

#jaren
jaren <- c(2017:2024)

#------------------------------------------------------------------------------
# Inlezen KS_2025 dataset met Perined
#------------------------------------------------------------------------------
KS_2025 <- read.fst("H:/Data proces/src/projecten/KS Basisset/Landelijke monitor 2025/Data/KS2025_dataset_perined.fst")
KS_2025 <- data.table(KS_2025)

#------------------------------------------------------------------------------
# Keep only zwangerschapsduur 24+ weken
#------------------------------------------------------------------------------
DT <- KS_2025[ amww >= 24 ]

DT <- data.table(DT)

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

###############################
###
### Zwangerschapsbegeleiding
###
###############################

names(DT)
DT %>% count(jaar, amddd1ond)

# 10 weken zwangerschapsduur = 10*7 = 70 dagen 
DT[ , "start_begeleiding_na_10wk" := ifelse( amddd1ond > 70, TRUE, FALSE)]

DT %>% count(jaar, start_begeleiding_na_10wk)

out <- DT %>% group_by(jaar, start_begeleiding_na_10wk) %>%
  summarise(
    Aantal = n()
  ) %>%
  # mutate(percentage = round(Aantal / sum(Aantal) * 100, 3)) %>%
  ungroup() %>%
  pivot_wider(names_from = jaar, values_from = Aantal )

write.xlsx(out, file = "./Output/KS2025_indicator_9_zw_begeleiding.xlsx", append = FALSE, sheetName = "Fig1_trend")


out <- DT %>% group_by(jaar, voorspelling_kwetsbaar, start_begeleiding_na_10wk) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= jaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_9_zw_begeleiding.xlsx", append = TRUE, sheetName = "Fig1_trend_kwetsbaar")


out <- DT[ jaar %in% c(2020:2024)] %>% group_by(gem2023, start_begeleiding_na_10wk) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= start_begeleiding_na_10wk, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_9_zw_begeleiding.xlsx", append = TRUE, sheetName = "Fig2_kaartje_gemeente")

out <- DT %>% group_by(jaar, ink_kwint_Ma, start_begeleiding_na_10wk) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= jaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_9_zw_begeleiding.xlsx", append = TRUE, sheetName = "Fig3_Huishoudinkomen")

out <- DT %>% group_by(jaar, opleidingsniveau_Ma, start_begeleiding_na_10wk) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= jaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_9_zw_begeleiding.xlsx", append = TRUE, sheetName = "Fig4_Opleidingsniveau moeder")


###############################
###
### Vroeggeboorte
###
###############################

names(DT)
DT[amww < 37] %>% count(jaar)
DT[hoftiezer < 10] %>% count(jaar)

DT[ , "Vroeggeboorte_37wk" := ifelse( amww < 37, TRUE, FALSE)]
DT[ , "SGA" := ifelse( hoftiezer < 10, TRUE, FALSE)]
DT[ , "BIG2" := ifelse( Vroeggeboorte_37wk | SGA, TRUE, FALSE)]

DT %>% count(jaar, Vroeggeboorte_37wk)
DT %>% count(jaar, SGA)
DT %>% count(jaar, BIG2)

out <- DT %>% group_by(jaar, Vroeggeboorte_37wk) %>%
  summarise(
    Aantal = n()
  ) %>%
  # mutate(percentage = round(Aantal / sum(Aantal) * 100, 3)) %>%
  ungroup() %>%
  pivot_wider(names_from = jaar, values_from = Aantal )

write.xlsx(out, file = "./Output/KS2025_indicator_15_BIG2.xlsx", append = FALSE, sheetName = "Fig1_trend_vroeggeboorte")

out <- DT %>% group_by(jaar, SGA) %>%
  summarise(
    Aantal = n()
  ) %>%
  # mutate(percentage = round(Aantal / sum(Aantal) * 100, 3)) %>%
  ungroup() %>%
  pivot_wider(names_from = jaar, values_from = Aantal )

write.xlsx(out, file = "./Output/KS2025_indicator_15_BIG2.xlsx", append = TRUE, sheetName = "Fig1_trend_SGA")

out <- DT %>% group_by(jaar, BIG2) %>%
  summarise(
    Aantal = n()
  ) %>%
  # mutate(percentage = round(Aantal / sum(Aantal) * 100, 3)) %>%
  ungroup() %>%
  pivot_wider(names_from = jaar, values_from = Aantal )

write.xlsx(out, file = "./Output/KS2025_indicator_15_BIG2.xlsx", append = TRUE, sheetName = "Fig1_trend_BIG2")


out <- DT %>% group_by(jaar, voorspelling_kwetsbaar, BIG2) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= jaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_15_BIG2.xlsx", append = TRUE, sheetName = "Fig1_trend_kwetsbaar")


out <- DT[ jaar %in% c(2020:2024)] %>% group_by(gem2023, BIG2) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= BIG2, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_15_BIG2.xlsx", append = TRUE, sheetName = "Fig2_kaartje_gemeente")

out <- DT %>% group_by(jaar, ink_kwint_Ma, BIG2) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= jaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_15_BIG2.xlsx", append = TRUE, sheetName = "Fig3_Huishoudinkomen")

out <- DT %>% group_by(jaar, opleidingsniveau_Ma, BIG2) %>%
  summarise(
    Aantal = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from= jaar, values_from = Aantal)

write.xlsx(out, file = "./Output/KS2025_indicator_15_BIG2.xlsx", append = TRUE, sheetName = "Fig4_Opleidingsniveau moeder")


