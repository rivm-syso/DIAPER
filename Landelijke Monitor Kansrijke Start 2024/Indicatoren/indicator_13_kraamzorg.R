#Kraamzorg na de bevalling
# Indicator 13 (kraamzorg): Deze cijfers zijn door het RIVM berekend in DIAPER en 
# gebaseerd op declaratiedata van zorgverzekeraars via Vektis en niet-openbare microdata van het CBS. 
# De studiepopulatie bestaat per jaar uit alle nieuwe ouders (zwangeren) van levendgeborenen. 
# Per jaar is berekend voor welk deel van deze ouders de zorgverzekeraars geen kraamzorg declaraties hebben ontvangen. 
# De cijfers zijn gecorrigeerd voor vermoedelijk langdurig ziekenhuisverblijf na de bevalling, 
# aan de hand van diagnose-behandelcombinaties.  

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
source("src/indicatoren/levendgeboren.R")
source("src/indicatoren/locatie.R")
source("src/indicatoren/huishoud_inkomen.R")
source("src/indicatoren/doodgeboren.R")
source("src/indicatoren/opleiding.R")

#------------------------------------------------------------------------------
#kraamzorgmoeders Vektis 
#------------------------------------------------------------------------------
#inlezen tot 2020

#inlezen vektis dat met benodigde kolommen voor kraamzorg
gb_zorg_moeders_tot_2020 <- read_spss("L:/8099Vektis_03Geboortezorgmoeders_JulyCBKV1.sav",
                                      col_select = c("RinPersoons", "Rinpersoon", "trajectid",
                                                     "prestatiemaand", "ib_indeling", "aantal"))

gb_zorg_moeders_tot_2020 <- as.data.table(gb_zorg_moeders_tot_2020)
setnames(gb_zorg_moeders_tot_2020, c("RinPersoons", "Rinpersoon"), c("RINPERSOONS", "RINPERSOON"))

bevallingen_tot_2020 <- read_spss("L:/8099Vektis_01MoedersEnbevallingenenmiskramen_JulyCBKV1.sav",
                                  col_select = c("RinPersoons", "Rinpersoon", "trajectid",
                                                 "bevallingsmaand"))

bevallingen_tot_2020 <- as.data.table(bevallingen_tot_2020)
setnames(bevallingen_tot_2020, c("RinPersoons", "Rinpersoon"), c("RINPERSOONS", "RINPERSOON"))

ind_kz_tot_2020 <- read_spss("L:/8099Vektis_07indicatiesurenkraamzorg_JulyCBKV1.sav")

ind_kz_tot_2020 <- as.data.table(ind_kz_tot_2020)
setnames(ind_kz_tot_2020, c("RinPersoons", "Rinpersoon"), c("RINPERSOONS", "RINPERSOON"))

#inlezen 2021 en 2022

#inlezen vektis dat met benodigde kolommen voor kraamzorg
gb_zorg_moeders_2021_2022 <- read_spss("L:/8099Vektis_03Geboortezorgmoeders202312CBKV1.sav",
                                       col_select = c("RINPERSOONS", "Rinpersoon", "trajectid",
                                                      "prestatiemaand", "ib_indeling", "aantal"))

gb_zorg_moeders_2021_2022 <- as.data.table(gb_zorg_moeders_2021_2022)
setnames(gb_zorg_moeders_2021_2022, c("Rinpersoon"), c("RINPERSOON"))

bevallingen_2021_2022 <- read_spss("L:/8099Vektis_01Moedersenbevallingenenmiskramen202312CBKV1.sav",
                                   col_select = c("RINPERSOONS", "Rinpersoon", "trajectid",
                                                  "bevallingsmaand"))

bevallingen_2021_2022 <- as.data.table(bevallingen_2021_2022)
setnames(bevallingen_2021_2022, c("Rinpersoon"), c("RINPERSOON"))

ind_kz_2021_2022 <- read_spss("L:/8099Vektis_07indicatiesurenkraamzorg202312CBKV1.sav")

ind_kz_2021_2022 <- as.data.table(ind_kz_2021_2022)
setnames(ind_kz_2021_2022, c("Rinpersoon"), c("RINPERSOON"))

#data samenvoegen
vektis_koppel_tot_2020 <- merge(gb_zorg_moeders_tot_2020, 
                                bevallingen_tot_2020[!is.na(trajectid)], #traject id moet bekend zijn om bevallingsmaand vast te stellen
                                all.x = FALSE, 
                                by = c("RINPERSOONS", "RINPERSOON", "trajectid"))

vektis_koppel_2021_2022 <- merge(gb_zorg_moeders_2021_2022, 
                                 bevallingen_2021_2022[!is.na(trajectid)], #traject id moet bekend zijn om bevallingsmaand vast te stellen
                                 all.x = FALSE, 
                                 by = c("RINPERSOONS", "RINPERSOON", "trajectid"))


vektis_koppel <- rbindlist(
  list(vektis_koppel_tot_2020, vektis_koppel_2021_2022),
  use.names = TRUE
)

vektis_koppel <- vektis_koppel[, ':=' (
  jaar = as.numeric(str_sub(bevallingsmaand, 1, 4)),
  kraamzorg = ifelse(!is.na(trajectid) & ib_indeling == "Kraamzorg per uur" & bevallingsmaand <= prestatiemaand, 1, 0)
)]

#------------------------------------------------------------------------------
#kwetsbaarheid inlezen
#------------------------------------------------------------------------------

#Inlezen laatste variant kwetsbaarheid
kwetsbaarheid <- fread("H:/Data proces/data/Kwetsbaarheid/meervoudig_kwetsbaarks24_v2.csv", colClasses = list(character = c("RINPERSOON", "RINPERSOON_KIND")))

#------------------------------------------------------------------------------
#var berekenen met uitsplitsing langdurige verblijf
#------------------------------------------------------------------------------

langdurig_verblijf <- fread("H:/Data proces/src/projecten/KS kraamzorg indicator/langdurigVerblijf1623.csv", colClasses = list(character = "RINPERSOON"))
langdurig_verblijf <- data.table(langdurig_verblijf)
langdurig_verblijf <- langdurig_verblijf[langdurig_verblijf_alt == 1,] #DBCs selecteren voor huidige selectie

#vaststellen welke bevallingen kraamzorg hebben gehad
kraamzorg_lv_basis <- merge(
  unique(vektis_koppel[, c("RINPERSOONS", "RINPERSOON", "jaar", "trajectid")]), #alle unieke bevallingen per traject per jaar
  unique(vektis_koppel[kraamzorg == 1, c("RINPERSOONS", "RINPERSOON", "jaar", "trajectid", "kraamzorg")]), #alle moeders die kraamzorg hebben gekregen per traject per jaar
  all.x = TRUE,
  by = c("RINPERSOONS", "RINPERSOON", "trajectid", "jaar")
)

#als een moeder NA heeft, dan heeft ze dus geen kraamzorg gehad
kraamzorg_lv_basis[, ':=' (
  kraamzorg = ifelse(is.na(kraamzorg), 0 , kraamzorg)
)]

#koppelen aan moeders die langdurig verbijf hebben gehad
leevGeb_langVerbl<- merge(levendgeboren_ouders,  
                          langdurig_verblijf[, c("RINPERSOONS", "RINPERSOON", "jaar")][, ld_verblijf_kind := "Ja"], 
                          all.x = TRUE,
                          by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                          by.y = c("RINPERSOONS", "RINPERSOON", "jaar"))


#koppelen alle levendgeborenen en Vektis moeders
kraamzorg_lv_basis <- merge(kraamzorg_lv_basis, leevGeb_langVerbl, all = FALSE, #Vektis moet ook levendgeboren zijn
                         by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                         by.y = c("RINPERSOONSMa", "RINPERSOONMa", "jaar"))


setnames(kraamzorg_lv_basis, c("RINPERSOONS.y", "RINPERSOON.y"), c("RINPERSOONS_KIND", "RINPERSOON_KIND" ))

#koppelen aan kinderen die langdurig verbijf hebben gehad
kraamzorg_lv_basis <- merge(kraamzorg_lv_basis, 
                         langdurig_verblijf[, c("RINPERSOONS", "RINPERSOON", "jaar")][, ld_verblijf_moeder := "Ja"], 
                         all.x = TRUE,
                         by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                         by.y = c("RINPERSOONS", "RINPERSOON", "jaar"))

kraamzorg_lv_basis[, ld_verblijf := ifelse(ld_verblijf_kind == "Ja" | ld_verblijf_moeder == "Ja", "Ja", "Nee")]
kraamzorg_lv_basis[, ld_verblijf := ifelse(is.na(ld_verblijf), "Nee", ld_verblijf)]
kraamzorg_lv_basis <- unique(kraamzorg_lv_basis)

#var berekenen en totalen toevoegen
kraamzorg_lv_nl <- merge(
  kraamzorg_lv_basis[kraamzorg == 0, .N, c("jaar", "ld_verblijf")],
  kraamzorg_lv_basis[, .N, c("jaar", "ld_verblijf")],
  by = c("jaar", "ld_verblijf")
)

setnames(kraamzorg_lv_nl,
         c("N.x", "N.y"),
         c("geen_kraamzorg", "tot_levendgeboren"))

kraamzorg_lv_nl <- kraamzorg_lv_nl %>% 
  mutate(
    geen_kraamzorg = ifelse(geen_kraamzorg < 10, NA, geen_kraamzorg),
    tot_levendgeboren = ifelse(geen_kraamzorg < 10, NA, tot_levendgeboren)
  )
kraamzorg_lv_nl <- data.table(kraamzorg_lv_nl)
kraamzorg_lv_nl[, per_geen_kraamzorg := (geen_kraamzorg / tot_levendgeboren) * 100]



#export
write.csv2(kraamzorg_lv_nl, "src/projecten/KS kraamzorg indicator/output 2024 met indicatie/kraamzorg_langdurig_verblijf_nl_2.csv")



kraamzorg_lv_basis <- kraamzorg_lv_basis %>% filter(ld_verblijf == "Nee")


#------------------------------------------------------------------------------
#var berekenen met uitsplitsing kwetsbaarheid
#------------------------------------------------------------------------------



#koppelen aan moeders die langdurig verbijf hebben gehad
kraamzorg_lvb_kw<- merge(kraamzorg_lv_basis,  
                   kwetsbaarheid[, c("Rinpersoons_KIND", "RINPERSOON_KIND", "voorspelling_kwetsbaar" )], 
                   all.x = TRUE,
                   by.x = c("RINPERSOONS_KIND", "RINPERSOON_KIND"),
                   by.y = c("Rinpersoons_KIND", "RINPERSOON_KIND"))



#var berekenen en totalen toevoegen
kraamzorg_lvb_kw <- merge(
  kraamzorg_lvb_kw[kraamzorg == 0, .N, c("jaar", "voorspelling_kwetsbaar")],
  kraamzorg_lvb_kw[, .N, c("jaar", "voorspelling_kwetsbaar")],
  by = c("jaar", "voorspelling_kwetsbaar")
)

setnames(kraamzorg_lvb_kw,
         c("N.x", "N.y"),
         c("geen_kraamzorg", "tot_levendgeboren"))

kraamzorg_lvb_kw <- kraamzorg_lvb_kw %>% 
  mutate(
    geen_kraamzorg = ifelse(geen_kraamzorg < 10, NA, geen_kraamzorg),
    tot_levendgeboren = ifelse(geen_kraamzorg < 10, NA, tot_levendgeboren)
  )
kraamzorg_lvb_kw <- data.table(kraamzorg_lvb_kw)
kraamzorg_lvb_kw[, per_geen_kraamzorg := (geen_kraamzorg / tot_levendgeboren) * 100]


#export
write.csv2(kraamzorg_lvb_kw, "src/projecten/KS kraamzorg indicator/output 2024 met indicatie/kraamzorg_kwetsbaar_lvb.csv")




#------------------------------------------------------------------------------
#kraamzorg per gemeente (gem 5 jaar)
#------------------------------------------------------------------------------
gemeente_lvb <- merge(vektis_koppel,
                  locatie,
                  all.x = TRUE,
                  by = c("RINPERSOONS", "RINPERSOON", "jaar"))

gemeente_lvb <- gemeente_lvb[, ':=' (
  jaar = as.numeric(str_sub(bevallingsmaand, 1, 4)),
  kraamzorg = ifelse(!is.na(trajectid) & ib_indeling == "Kraamzorg per uur" & bevallingsmaand <= prestatiemaand, 1, 0)
)]

#laatste 5 jaar selecteren
gemeente_lvb <- gemeente_lvb[jaar %in% (max(jaren) - 4):max(jaren)]

#vaststellen welke bevallingen kraamzorg hebben gehad
gemeente_lvb <- merge(
  unique(gemeente_lvb[, c("RINPERSOONS", "RINPERSOON", "trajectid", "jaar", "gem2023")]), #alle unieke bevallingen per traject per jaar
  unique(gemeente_lvb[kraamzorg == 1, c("RINPERSOONS", "RINPERSOON", "trajectid", "gem2023", "jaar", "kraamzorg")]), #alle moeders die kraamzorg hebben gekregen per traject per jaar
  all.x = TRUE,
  by = c("RINPERSOONS", "RINPERSOON", "trajectid", "jaar","gem2023")
)

#als een moeder NA heeft, dan heeft ze dus geen kraamzorg gehad
gemeente_lvb[, ':=' (
  kraamzorg = ifelse(is.na(kraamzorg), 0 , kraamzorg)
)]

#koppelen alle levend geboren en Vektis moeders
gemeente_lvb <- merge(gemeente_lvb, levendgeboren_ouders, all = FALSE, #Vektis moet ook levendgeboren zijn
                  by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                  by.y = c("RINPERSOONSMa", "RINPERSOONMa", "jaar"))

gemeente_lvb <- merge(gemeente_lvb, kraamzorg_lv_basis[,c("RINPERSOONS_KIND", "RINPERSOON_KIND", "jaar")], all = FALSE, #gaat enkel om zwangerschappen waar geen langdurig ziekenhuisverblijf is
                 by.x = c("RINPERSOONS.y", "RINPERSOON.y", "jaar"),
                 by.y = c("RINPERSOONS_KIND", "RINPERSOON_KIND", "jaar"))

#var berekenen en totalen toevoegen
gemeente_lvb <- merge(
  gemeente_lvb[kraamzorg == 0, .N, by = gem2023],
  gemeente_lvb[, .N, by = gem2023],
  by = "gem2023"
)

setnames(gemeente_lvb,
         c("N.x", "N.y"),
         c("geen_kraamzorg", "tot_levendgeboren"))

gemeente_lvb <- gemeente_lvb %>% 
  mutate(
    geen_kraamzorg = ifelse(geen_kraamzorg < 10, NA, geen_kraamzorg),
    tot_levendgeboren = ifelse(geen_kraamzorg < 10, NA, tot_levendgeboren)
  )


gemeente_lvb[, per_geen_kraamzorg := (geen_kraamzorg / tot_levendgeboren) * 100]

#export
write.csv2(gemeente_lvb, "src/projecten/KS kraamzorg indicator/output 2024 met indicatie/kraamzorg_gemeente_lvb.csv")



#------------------------------------------------------------------------------
#kraamzorg uitgesplitst naar opleiding moeder
#------------------------------------------------------------------------------



#koppelen alle levend geboren en Vektis moeders
kraamzorg_lvb_opl <- merge(kraamzorg_lv_basis, hoogstopl_data[, c("jaar", "RINPERSOONS", "RINPERSOON", "opleidingsniveau")], all = FALSE, #Vektis moet ook levendgeboren zijn
                       by = c("RINPERSOONS", "RINPERSOON", "jaar"))




#var berekenen en totalen toevoegen
kraamzorg_lvb_opl <- merge(
  kraamzorg_lvb_opl[kraamzorg == 0, .N, c("jaar", "opleidingsniveau")],
  kraamzorg_lvb_opl[, .N, c("jaar", "opleidingsniveau")],
  by = c("jaar", "opleidingsniveau")
)

setnames(kraamzorg_lvb_opl,
         c("N.x", "N.y"),
         c("geen_kraamzorg", "tot_levendgeboren"))

kraamzorg_lvb_opl <- kraamzorg_lvb_opl %>% 
  mutate(
    geen_kraamzorg = ifelse(geen_kraamzorg < 10, NA, geen_kraamzorg),
    tot_levendgeboren = ifelse(geen_kraamzorg < 10, NA, tot_levendgeboren)
  )
kraamzorg_lvb_opl <- data.table(kraamzorg_lvb_opl)
kraamzorg_lvb_opl[, per_geen_kraamzorg := (geen_kraamzorg / tot_levendgeboren) * 100]



#export
write.csv2(kraamzorg_lvb_opl, "src/projecten/KS kraamzorg indicator/output 2024 met indicatie/kraamzorg_opleiding_lvb.csv")




#------------------------------------------------------------------------------
#kraamzorg uitgesplitst naar HH inkomen
#------------------------------------------------------------------------------



#koppelen alle levend geboren en Vektis moeders
kraamzorg_lvb_ink <- merge(kraamzorg_lv_basis, huishoud_inkomen[, c("jaar", "RINPERSOONS", "RINPERSOON", "ink_kwint")], all = FALSE, #Vektis moet ook levendgeboren zijn
                       by = c("RINPERSOONS", "RINPERSOON", "jaar"))


#var berekenen en totalen toevoegen
kraamzorg_lvb_ink <- merge(
  kraamzorg_lvb_ink[kraamzorg == 0, .N, c("jaar", "ink_kwint")],
  kraamzorg_lvb_ink[, .N, c("jaar", "ink_kwint")],
  by = c("jaar", "ink_kwint")
)

setnames(kraamzorg_lvb_ink,
         c("N.x", "N.y"),
         c("geen_kraamzorg", "tot_levendgeboren"))

kraamzorg_lvb_ink <- kraamzorg_lvb_ink %>% 
  mutate(
    geen_kraamzorg = ifelse(geen_kraamzorg < 10, NA, geen_kraamzorg),
    tot_levendgeboren = ifelse(geen_kraamzorg < 10, NA, tot_levendgeboren)
  )
kraamzorg_lvb_ink <- data.table(kraamzorg_lvb_ink)
kraamzorg_lvb_ink[, per_geen_kraamzorg := (geen_kraamzorg / tot_levendgeboren) * 100]



#export
write.csv2(kraamzorg_lvb_ink, "src/projecten/KS kraamzorg indicator/output 2024 met indicatie/kraamzorg_hhinkomen.csv")







