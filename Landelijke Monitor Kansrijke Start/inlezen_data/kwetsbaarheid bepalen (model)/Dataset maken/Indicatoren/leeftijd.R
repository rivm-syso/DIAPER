#Leeftijd van moeder en vader bij bevalling


#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#------------------------------------------------------------------------------
#GBA persoon data
#------------------------------------------------------------------------------

#Bepalen locaties laatste dataset
gbapersoon_locatie <- list.files("G:/Bevolking/GBAPERSOONTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)

gbapersoon_locatie <- gbapersoon_locatie[length(gbapersoon_locatie)]

#Inlezen datasets met alleen relevante kolommen
geboortedatum <- read_spss(gbapersoon_locatie, col_select = c(RINPERSOONS, RINPERSOON, GBAGEBOORTEJAAR, 
                                                              GBAGEBOORTEMAAND, GBAGEBOORTEDAG, 
                                                              GBAGEBOORTEJAARMOEDER:GBAGEBOORTEDAGVADER))

geboortedatum <- as.data.table(geboortedatum)

#Opschonen naar datum
geboortedatum[, ':=' (
  "gb_kind" = as.Date(str_c(GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGEBOORTEDAG), format = "%Y%m%d"),
  "gb_moeder" = as.Date(str_c(GBAGEBOORTEJAARMOEDER, GBAGEBOORTEMAANDMOEDER, GBAGEBOORTEDAGMOEDER), format = "%Y%m%d"),
  "gb_vader" = as.Date(str_c(GBAGEBOORTEJAARVADER, GBAGEBOORTEMAANDVADER, GBAGEBOORTEDAGVADER), format = "%Y%m%d")
)]

geboortedatum <- geboortedatum[, .(RINPERSOONS, RINPERSOON, gb_kind, gb_moeder, gb_vader)]

#Leeftijd bij bevalling berekenen
geboortedatum[, ':=' (
  "lft_bevalling_moeder" = floor(as.double(difftime(gb_kind, gb_moeder, units = "days")) / 365.25),
  "lft_bevalling_vader" = floor(as.double(difftime(gb_kind, gb_vader, units = "days")) / 365.25)
)]

#Categorien toevoegen
geboortedatum[, ':=' (
  "lft_bevalling_moeder_cat6" = fcase(
    lft_bevalling_moeder < 20, "< 20",
    lft_bevalling_moeder >= 20 & lft_bevalling_moeder <= 24, "20 - 24",
    lft_bevalling_moeder >= 25 & lft_bevalling_moeder <= 29, "25 - 29",
    lft_bevalling_moeder >= 30 & lft_bevalling_moeder <= 34, "30 - 34",
    lft_bevalling_moeder >= 35 & lft_bevalling_moeder <= 39, "35 - 39",
    lft_bevalling_moeder >= 40, ">= 40"
  ),
  "lft_bevalling_vader_cat6" = fcase(
    lft_bevalling_vader < 20, "< 20",
    lft_bevalling_vader >= 20 & lft_bevalling_vader <= 24, "20 - 24",
    lft_bevalling_vader >= 25 & lft_bevalling_vader <= 29, "25 - 29",
    lft_bevalling_vader >= 30 & lft_bevalling_vader <= 34, "30 - 34",
    lft_bevalling_vader >= 35 & lft_bevalling_vader <= 39, "35 - 39",
    lft_bevalling_vader >= 40, ">= 40"
  )
)]

#------------------------------------------------------------------------------
#Kinder ouder tab
#------------------------------------------------------------------------------
#Bepalen locaties laatste dataset
kindouder_locatie <- list.files("G:/Bevolking/KINDOUDERTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)

kindouder_locatie <- kindouder_locatie[length(kindouder_locatie)]

#Inlezen datasets met alleen relevante kolommen
kindouder <- read_spss(kindouder_locatie)

kindouder <- as.data.table(kindouder)

#------------------------------------------------------------------------------
#koppelen ouder aan kinderen
#------------------------------------------------------------------------------
leeftijd <- merge(geboortedatum, kindouder, by = c("RINPERSOONS", "RINPERSOON"))

#------------------------------------------------------------------------------
#Ordenen
#------------------------------------------------------------------------------
rm(geboortedatum, kindouder)

setnames(leeftijd,
         c("RINPERSOONS", "RINPERSOON"),
         c("RINPERSOONSkind", "RINPERSOONkind"))
