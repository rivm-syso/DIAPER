##############################################################
###
###     Landelijke monitor Kansrijke Start 2025
###
###     Opbouw dataset
###
###
#############################################################

#set wd
setwd("H:/Data proces/")
#Utilities
source("src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/utils_pp.R")
library(fst)
library("writexl")
library(data.table)
library(dplyr)
library(tidyr)


gemeente_indeling <- "gem2023"


#jaren (kinderen geboren 2017-2024)
jaren <- c(2017:2024) 
years <- jaren


# Basis populatie ---------------------------------------------------------
#Levend & doodgeboren (>24 weken) kinderen
source("./src/indicatoren/levend_doodgeboren2.0.R")
Ma <- Geboren_dood_levend %>% select(RINPERSOONS, RINPERSOON, RINPERSOONSMa, RINPERSOONMa, jaar) %>% 
  rename(RINPERSOONS_Kind = RINPERSOONS, RINPERSOON_Kind = RINPERSOON, RINPERSOONS = RINPERSOONSMa, RINPERSOON = RINPERSOONMa) %>% 
  mutate(jaar_voor_bevalling = jaar -1,
         ouder = "Ma")
Pa <- Geboren_dood_levend %>% select(RINPERSOONS, RINPERSOON, RINPERSOONSpa, RINPERSOONpa, jaar) %>% 
  rename(RINPERSOONS_Kind = RINPERSOONS, RINPERSOON_Kind = RINPERSOON, RINPERSOONS = RINPERSOONSpa , RINPERSOON = RINPERSOONpa) %>% 
  mutate(jaar_voor_bevalling = jaar -1,
         ouder = "Pa")
Ma <- unique(Ma)
Pa <- unique(Pa)

DT <- rbind(Ma,Pa)

DT <- DT %>% mutate(RINPERSOON = ifelse(RINPERSOON == "---------", NA, RINPERSOON))


# Schulden problematiek ---------------------------------------------------
source("./src/indicatoren/schuldsanering.R")
source("./src/indicatoren/zvw_wanbetaler.R")
DT <- merge(DT, schuldsanering, by = c("RINPERSOONS", "RINPERSOON",  "jaar"), all.x =T)
DT <- merge(DT, zvw_wanbetaler, by = c("RINPERSOONS", "RINPERSOON",  "jaar"), all.x =T)
DT <- DT %>% mutate(schuldsanering = ifelse(is.na(schuldsanering), 0, schuldsanering),
                    zvw_wanbetaler = ifelse(is.na(zvw_wanbetaler), 0, 1),
                    schulden = ifelse(schuldsanering == 1 | zvw_wanbetaler == 1, 1, 0))

# Psychische problematiek -------------------------------------------------
source("./src/indicatoren/ggz_kosten.R")
source("./src/indicatoren/medicijn_gebruik.R")
DT <- merge(DT, ggz_kosten, by = c("RINPERSOONS", "RINPERSOON",  "jaar"), all.x =T)
DT <- merge(DT, med_gebruik, by = c("RINPERSOONS", "RINPERSOON",  "jaar"), all.x =T)
DT <- DT %>% mutate(kosten_GGZ = ifelse(is.na(kosten_GGZ), 0, kosten_GGZ),
                    med_psych_klachten = ifelse(is.na(med_psych_klachten), 0, med_psych_klachten),
                    psy_problematiek = ifelse(kosten_GGZ == 1 | med_psych_klachten == 1, 1, 0))


# Opleiding ---------------------------------------------------------------
source("./src/indicatoren/opleiding.R")
DT <- merge(DT, hoogstopl_data, by = c("RINPERSOONS", "RINPERSOON",  "jaar"), all.x =T)

# Inkomen -----------------------------------------------------------------
source("./src/indicatoren/huishoud_inkomen.R")
DT <- merge(DT, huishoud_inkomen[, c("RINPERSOONS", "RINPERSOON",  "jaar", "ink_kwint")], by = c("RINPERSOONS", "RINPERSOON",  "jaar"), all.x =T)

# Locatie -----------------------------------------------------------------
#locatie kind
source("./src/indicatoren/locatie.R")
DT <- merge(DT, locatie, by = c("RINPERSOONS", "RINPERSOON",  "jaar"), all.x =T)


# Kwetsbaarheid -----------------------------------------------------------
kwetsbaarheid <- fread("./src/projecten/Kwetsbaarheid/2024 update/meervoudig_kwetsbaar.csv", colClasses = list(character = c("RINPERSOON", "RINPERSOON_KIND")))
DT <- merge(DT, kwetsbaarheid[,c("Rinpersoons_KIND", "RINPERSOON_KIND", "voorspelling_kwetsbaar", "kans_kwetsbaar")], 
            by.x = c("RINPERSOONS_Kind", "RINPERSOON_Kind"), by.y = c("Rinpersoons_KIND", "RINPERSOON_KIND"), all.x =T)


kwetsbaarheid24 <- read.fst("./src/projecten/Kwetsbaarheid/Voorspelling kwetsbaar geboortejaar 2024/Data/meervoudig_kwetsbaar24_v1.fst")
DT <- merge(DT, kwetsbaarheid24[,c("Rinpersoons_KIND", "RINPERSOON_KIND", "voorspelling_kwetsbaar", "kans_kwetsbaar")], 
            by.x = c("RINPERSOONS_Kind", "RINPERSOON_Kind"), by.y = c("Rinpersoons_KIND", "RINPERSOON_KIND"), all.x =T)

DT <- DT %>% mutate(voorspelling_kwetsbaar = ifelse(jaar <2024, as.character(voorspelling_kwetsbaar.x), as.character(voorspelling_kwetsbaar.y) ))#tot 23 oude en voor 24 nieuwe kwetsbaarheid

# Data opslaan ------------------------------------------------------------

write.fst(DT, "./src/projecten/KS Basisset/Landelijke monitor 2025/KS2025_dataset.fst")

DT <- read.fst("./src/projecten/KS Basisset/Landelijke monitor 2025/KS2025_dataset.fst")

#------------------------------------------------------------------------------
# voeg samen
#------------------------------------------------------------------------------
DT <- DT %>% select(RINPERSOONS_Kind, RINPERSOON_Kind, RINPERSOONS, RINPERSOON, jaar, ouder, schuldsanering,zvw_wanbetaler, schulden,
                    kosten_GGZ, med_psych_klachten, psy_problematiek, opleidingsniveau, ink_kwint,PC4, gem2023, wc2023,  voorspelling_kwetsbaar )

# Data omzetten naar wide format ------------------------------------------
#set kinderen
DT_kind <- DT %>% filter(ouder == "Ma") %>%  select(RINPERSOONS_Kind, RINPERSOON_Kind, jaar, gem2023, wc2023,  voorspelling_kwetsbaar) %>% unique()
#check
DT_kind_dubbel <- DT_kind %>% group_by( RINPERSOONS_Kind, RINPERSOON_Kind, jaar) %>% summarise(n=n()) %>% filter(n>1) #4 kinderen dubbel
DT_kind <- DT_kind %>% group_by( RINPERSOONS_Kind, RINPERSOON_Kind, jaar) %>% slice_head(n=1) %>% ungroup()

#set ouders
DT_ouders <- DT %>% select(RINPERSOONS_Kind, RINPERSOON_Kind, RINPERSOONS, RINPERSOON,jaar, ouder, schuldsanering,zvw_wanbetaler, schulden,
                           kosten_GGZ, med_psych_klachten, psy_problematiek, opleidingsniveau, ink_kwint )
DT_ouders <- DT_ouders %>% group_by( RINPERSOONS_Kind, RINPERSOON_Kind, jaar, ouder) %>% slice_head(n=1) %>% ungroup()

#ouders omzetten
DT_ouders_wide <- pivot_wider(DT_ouders, id_cols = c( RINPERSOONS_Kind, RINPERSOON_Kind, jaar), names_from = ouder,
                              values_from = c(RINPERSOONS, RINPERSOON,schuldsanering,zvw_wanbetaler, schulden,
                                              kosten_GGZ, med_psych_klachten, psy_problematiek, opleidingsniveau, ink_kwint))
# 1 set met beide ouders per kind
DT_kind <- DT_kind %>% left_join(DT_ouders_wide)

#------------------------------------------------------------------------------
# inlezen Perined tm 2024
#------------------------------------------------------------------------------
perined_data <- read.csv2("L:/3204_SGZ_PERINED25.42_20260423v1.csv")
perined_data <- data.table(perined_data)

names(perined_data)

perined_data[, ':=' (RINPERSOONS_Moeder = ifelse(CBKSoortNr_Moeder == "", NA, CBKSoortNr_Moeder),
                     RINPERSOON_Moeder = ifelse (Rinpersoon_Moeder == "" | Rinpersoon_Moeder == "000000000", NA, Rinpersoon_Moeder))]

perined_data[, ':=' (RINPERSOONS_Kind = ifelse(CBKSoortNr_Kind == "", NA, CBKSoortNr_Kind),
                     RINPERSOON_Kind = ifelse (Rinpersoon_Kind == "" | Rinpersoon_Kind == "000000000", NA, Rinpersoon_Kind))]

perined_data[ , ':=' ( CBKSoortNr_Kind = NULL, Rinpersoon_Kind = NULL, CBKSoortNr_Moeder = NULL, Rinpersoon_Moeder = NULL)]

perined_data[, ':='(
  RINPERSOON_Kind = ifelse( !is.na(RINPERSOON_Kind), sprintf("%09d", as.numeric(RINPERSOON_Kind)) , NA),
  RINPERSOON_Moeder = ifelse( !is.na(RINPERSOON_Moeder), sprintf("%09d", as.numeric(RINPERSOON_Moeder)), NA) )]

#------
# voeg toe aan de dataset
#---------------------
DT_kind <- merge(DT_kind, perined_data, by = c("RINPERSOONS_Kind", "RINPERSOON_Kind"), all.x = T) 
DT_kind <- data.table(DT_kind)

names(DT_kind)
#-----------------
# Check basis aantallen
#--------------
DT_kind[jaar.x != jaar.y] %>% count(jaar.x)

# gebruik altijd jaar.x
DT_kind[ , "jaar" := jaar.x]

DT_kind %>% count(jaar, RINPERSOONS_Kind)
DT_kind[ amww >= 24] %>% count(jaar, RINPERSOONS_Kind)

#---------------------------
# Opslaan
#---------------------------
write.fst(DT_kind, "./src/projecten/KS Basisset/Landelijke monitor 2025/Data/KS2025_dataset_perined.fst")


names(DT)
