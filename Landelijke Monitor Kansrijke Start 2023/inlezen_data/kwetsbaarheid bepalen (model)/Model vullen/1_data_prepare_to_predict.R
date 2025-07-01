# Script om de data te bekijken en voorbereiden op het in kaart brengen van % kwetsbaar per jaar

# Indicatoren KS basisset
library(fst)

# Workdirectory
setwd("H:/Data proces/")

# Utilities
source("src/utils.R")
source("H:/PP/utils_PP.R")


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# inlezen data
#------------------------------------------------------------------------------
DT <- read.fst("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Data/analysebestand_kwetsbaarheid.fst")
# hier kan ik ook het bestand _incl_missing uploaden om te zien hoeveel Perined en CBS variabelen er missen
DT <- data.table(DT)

# nieuwe naam dataset
analysebestand_kwetsbaar <- DT

# selecteren relevante variabelen
analysebestand_kwetsbaar <- DT[, c(
  "RINPERSOONS", "RINPERSOON", "Rinpersoons_KIND", "RINPERSOON_KIND", "jaar", "leeftijd_cat", "etniciteit_cat", "Par_3cat", "asielmigrant", "opleidingsniveau_cat", "inkomen_cat",
  "SECM", "financieleproblemen", "contractsoort", "dienstverband", "eenoudergezin", "gehuwdpaar", "scheiding", "aantalprshh_cat", "jgdondersteuning",
  "zvwkosten_totaal_cat", "huisartskosten_cat", "ziekenhuiskosten_cat", "woningBezit",
  "Hoogmedicijngebruik", "LVBregistratie", "verslavingszorg", "GGZ", "Verdachte_misdrijf", "Slachtoffer_misdrijf", "gedetineerd", "Aantalverhuis",
  "overlijdennaaste5jaar", "Motorvoertuig", "huisartsen", "leefbaarheidsscore"
)]

# inladen databestand LCA om namen te checken
load("H:/LCA kwetsbaarheid/LCA Merge bestanden/Zwangere vrouwen incl partner/Script LCA zwangere vrouwen volledig/LCA_dataset_final_kort.RData")
names(LCA_dataset_final_kort)

# aanpassen namen  naar LCA (straks namen vanuit LCA nog aanpassen naar algemene namen zonder jaartallen etc.)
analysebestand_kwetsbaar <- analysebestand_kwetsbaar %>% rename(
  "par_3cat" = "Par_3cat",
  "hoogmedicijngebruik" = "Hoogmedicijngebruik",
  "verdachte_misdrijf" = "Verdachte_misdrijf",
  "slachtoffer_misdrijf" = "Slachtoffer_misdrijf",
  "motorvoertuig" = "Motorvoertuig",
  "aantalverhuis" = "Aantalverhuis",
  "overlijdennaaste" = "overlijdennaaste5jaar",
  "woningbezit" = "woningBezit"
)

# Alles omzetten naar factor
analysebestand_kwetsbaar <- analysebestand_kwetsbaar %>% modify_if(is.numeric, as.factor)

# Controle databestand
analysebestand_kwetsbaar %>% summary()
analysebestand_kwetsbaar %>% names()
analysebestand_kwetsbaar %>% glimpse()
analysebestand_kwetsbaar %>% str()

# om eerste keer indeling te maken: eerst namen gelijk maken (straks engelstalige namen...)
names(LCA_dataset_final_kort)
names(analysebestand_kwetsbaar)

analysebestand_kwetsbaar <- analysebestand_kwetsbaar %>% rename(
  "asielmigrant2014_2018" = "asielmigrant",
  "opleidingsniveau" = "opleidingsniveau_cat",
  "inkomen_2016_cat" = "inkomen_cat",
  "SECMokt16" = "SECM",
  "financieleproblemen_2016" = "financieleproblemen",
  "contractsoort_okt2016" = "contractsoort",
  "dienstverband_okt2016" = "dienstverband",
  "eenoudergezin_okt2016" = "eenoudergezin",
  "gehuwdpaar_okt2016" = "gehuwdpaar",
  "scheiding20132016" = "scheiding",
  "aantalprshh_okt2016_cat" = "aantalprshh_cat",
  "jgdondersteuning20152016" = "jgdondersteuning",
  "zvw_161514_kwint" = "zvwkosten_totaal_cat",
  "kostenhuisarts2016_kwint" = "huisartskosten_cat",
  "zkh_161514_cat" = "ziekenhuiskosten_cat",
  "hoogmedicijngebruik2016" = "hoogmedicijngebruik",
  "verslavingszorginclnevendiagnose20112016" = "verslavingszorg",
  "GGZ_2016" = "GGZ",
  "verdachte_misdrijf_2010_2016" = "verdachte_misdrijf",
  "slachtoffer_misdrijf_2012_2016" = "slachtoffer_misdrijf",
  "gedetineerd20052016" = "gedetineerd",
  "aantalverhuisokt20112016" = "aantalverhuis",
  "overlijdennaaste5jaarokt16" = "overlijdennaaste",
  "motorvoertuig_2017" = "motorvoertuig",
  "huisartsenokt16" = "huisartsen",
  "leefbaarheidsscoreokt16" = "leefbaarheidsscore",
  "woningbezit_2017" = "woningbezit"
)

# Eerst complete cases (later missende data imputeren d.m.v. MICE)
analysebestand_kwetsbaar_cc <- analysebestand_kwetsbaar %>% filter(complete.cases(.))

# Check of het uitmaakt wanneer je eerste de kinderen zonder rin-verwijderd zodat je meer mensen over houdt
# (die gegevens zijn niet noodzakelijk) -> nee, zelfde aantal 
# analysebestand_kwetsbaar_zonderrin <- analysebestand_kwetsbaar %>% select(-Rinpersoons_KIND, -RINPERSOON_KIND)
# analysebestand_kwetsbaar_zonderrin_cc <- analysebestand_kwetsbaar_zonderrin %>% filter(complete.cases(.))
# rm(analysebestand_kwetsbaar_zonderrin, analysebestand_kwetsbaar_zonderrin_cc)

# opslaan databestand
setwd("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/")
save(analysebestand_kwetsbaar_cc, file = "analysebestand_kwetsbaar_cc.RData")
save(analysebestand_kwetsbaar, file = "analysebestand_kwetsbaar.RData")
