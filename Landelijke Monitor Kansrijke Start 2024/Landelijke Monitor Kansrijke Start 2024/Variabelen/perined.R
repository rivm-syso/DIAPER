#inlezen Perined

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#jaren
#jaren <- c(2016:2021)

#Gemeente indeling
#gemeente_indeling <- "gem2021"

#------------------------------------------------------------------------------
##Perined inlezen
#------------------------------------------------------------------------------
#Inlezen data met relevante kolommen
perined_data_2000_2020 <- read_spss("G:/Maatwerk/PERINED_RIVM/Perined2000_2021CBKV1.sav",
                          col_select = c("RINPERSOONS_Moeder", "Rinpersoon_Moeder",
                                         perined_variabelen))
perined_data_2000_2020 <- perined_data_2000_2020[perined_data_2000_2020$jaar <= 2020,]

perined_data_2021_2022 <- read_spss("L:/8099Perined2021_2022RIVMCBKV1.sav",
                                    col_select = c("RINPERSOONS_Moeder", "Rinpersoon_Moeder",
                                                   perined_variabelen))
perined_data_2021_2022 <- perined_data_2021_2022[perined_data_2021_2022$jaar == 2021,]

perined_data = rbind(perined_data_2000_2020, perined_data_2021_2022)

rm(perined_data_2000_2020)
rm(perined_data_2021_2022)
gc()

perined_data <- as.data.table(perined_data)

setnames(perined_data, 
         c("RINPERSOONS_Moeder", "Rinpersoon_Moeder"),
         c("RINPERSOONS", "RINPERSOON"))

#lege RIN omzetten naar missings
perined_data[, ':=' (RINPERSOONS = ifelse(RINPERSOONS == "", NA, RINPERSOONS),
                     RINPERSOON = ifelse (RINPERSOON == "" | RINPERSOON == "000000000", NA, RINPERSOON))]

#jaren filteren
#perined_data <- perined_data[jaar %in% jaren]
