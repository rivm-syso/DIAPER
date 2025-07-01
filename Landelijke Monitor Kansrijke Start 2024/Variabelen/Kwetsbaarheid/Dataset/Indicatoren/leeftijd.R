#Leeftijd van moeder en vader bij bevalling
# Lianne 2024- 10 juli, check wat er gebeurt als een kind doodgeboren wordt en niet in GBAPERSOONTAB voorkomt.... 
# wordt de geboortedatum van de ouders en de leeftijd wel toegevoegd?

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


# Dood geborenen  ---------------------------------------------------------
#Doodgeboren kinderen staan niet in de GBApersoontab en moeten daarom additioneel toegevoegd worden

#Dood geborenen - Inlezen datasets met alleen relevante kolommen
dood_geb <- inlezen_data("G:/Bevolking/DGGBAPERSOONTAB", kolommen = c("RINPERSOONS", "RINPERSOON", 
                                                                      "DGDATUMPARTUS"))

#toevoegen RIN ouders 
#Bepalen locaties laatste dataset
kindouder_locatie <- list.files("G:/Bevolking/KINDOUDERTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)
kindouder_locatie <- kindouder_locatie[length(kindouder_locatie)]

#Inlezen datasets met alleen relevante kolommen
kindouder <- read_spss(kindouder_locatie)
kindouder <- as.data.table(kindouder)

#Koppelen moeder aan doodgeborenen
dood_geb <- merge(dood_geb, kindouder[, c("RINPERSOONS", "RINPERSOON", "RINPERSOONSMa", "RINPERSOONMa")],
            all.x = T, by= c("RINPERSOONS", "RINPERSOON"))
#koppelen geboortedatum moeder aan dood geboren kind
dood_geb <- merge(dood_geb, geboortedatum[,c("RINPERSOONS", "RINPERSOON", "GBAGEBOORTEJAAR", "GBAGEBOORTEMAAND", "GBAGEBOORTEDAG")],
                  by.x = c("RINPERSOONSMa", "RINPERSOONMa"), by.y= c("RINPERSOONS", "RINPERSOON"), all.x= T)

#Opschonen naar datum
dood_geb[, ':=' (
  "gb_kind" = as.Date(DGDATUMPARTUS, format = "%Y%m%d"),
  "gb_moeder" = as.Date(str_c(GBAGEBOORTEJAAR, GBAGEBOORTEMAAND, GBAGEBOORTEDAG), format = "%Y%m%d")
)] 

#Enkel relevante kolommen behouden
dood_geb <- dood_geb[, .(RINPERSOONS, RINPERSOON, gb_kind, gb_moeder)]


# Data schonen ------------------------------------------------------------

geboortedatum <- geboortedatum[, .(RINPERSOONS, RINPERSOON, gb_kind, gb_moeder)]

geboortedatum <- rbind(geboortedatum, dood_geb)

#Leeftijd bij bevalling berekenen
geboortedatum[, ':=' (
  "lft_bevalling_moeder" = floor(as.double(difftime(gb_kind, gb_moeder, units = "days")) / 365.25)#,
  #"lft_bevalling_vader" = floor(as.double(difftime(gb_kind, gb_vader, units = "days")) / 365.25)
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
  )#,
  # "lft_bevalling_vader_cat6" = fcase(
  #   lft_bevalling_vader < 20, "< 20",
  #   lft_bevalling_vader >= 20 & lft_bevalling_vader <= 24, "20 - 24",
  #   lft_bevalling_vader >= 25 & lft_bevalling_vader <= 29, "25 - 29",
  #   lft_bevalling_vader >= 30 & lft_bevalling_vader <= 34, "30 - 34",
  #   lft_bevalling_vader >= 35 & lft_bevalling_vader <= 39, "35 - 39",
  #   lft_bevalling_vader >= 40, ">= 40"
  # )
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
