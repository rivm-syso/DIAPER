#inlezen Perined data

#Utilities
source("utils.R")

#------------------------------------------------------------------------------
##Perined inlezen
#------------------------------------------------------------------------------
#Inlezen data
perined_data_tm_2020 <- read_spss("G:/Maatwerk/PERINED_RIVM/Perined2000_2021CBKV1.sav") %>% 
  filter(jaar < 2021) #anders zit 2021 er twee keer in
perined_data_2021_2022 <- read_spss("L:/8099Perined2021_2022RIVMCBKV1.sav")

perined_data <- rbindlist(
  list(perined_data_tm_2020, perined_data_2021_2022),
  use.names = TRUE,
  fill = TRUE
)

rm(perined_data_tm_2020, perined_data_2021_2022)

perined_data <- as.data.table(perined_data)

#lege RIN omzetten naar missings
perined_data[, ':=' (RINPERSOONS_Moeder = ifelse(RINPERSOONS_Moeder == "", NA, RINPERSOONS_Moeder),
                     Rinpersoon_Moeder = ifelse (Rinpersoon_Moeder == "" | Rinpersoon_Moeder == "000000000", NA, Rinpersoon_Moeder))]

#jaren filteren
perined_data <- perined_data[jaar %in% jaren]
