# Gezinnen die geen kraamzorg gebruiken

# Teller: het aantal levendgeborenen waarvan de moeder geen declaratie voor kraamzorg had 
# in de periode ná de geboorte.

# Noemer: het aantal levendgeborenen.

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Jaren
jaren <- c(2015:2022)

#locatie indeling
gemeente_indeling <- "gem2022"

#------------------------------------------------------------------------------
#CBS data
#------------------------------------------------------------------------------
source("src/indicatoren/levendgeboren.R")
source("src/indicatoren/locatie.R")
source("src/indicatoren/GGZ.R")
source("src/indicatoren/huishoud_inkomen.R")
source("src/indicatoren/zvw_wanbetaler.R")
source("src/indicatoren/schuldsanering.R")
source("src/indicatoren/doodgeboren.R")

#------------------------------------------------------------------------------
#kraamzorgmoeders Vektis
#------------------------------------------------------------------------------
#inlezen tot 2020

#inlezen vektis data met benodigde kolommen voor kraamzorg
  # 1. Moeders, bevallingen en miskramen (2016-2020)
bevallingen_tot_2020 <- read_spss("L:/8099Vektis_01MoedersEnbevallingenenmiskramen_JulyCBKV1.sav",
                         col_select = c("RinPersoons", "Rinpersoon", "trajectid",
                                        "bevallingsmaand"))

bevallingen_tot_2020 <- as.data.table(bevallingen_tot_2020)
setnames(bevallingen_tot_2020, c("RinPersoons", "Rinpersoon"), c("RINPERSOONS", "RINPERSOON"))  

  # 3. Declaraties geboortezorg per fase zwangerschap (2016-2020)
gb_zorg_moeders_tot_2020 <- read_spss("L:/8099Vektis_03Geboortezorgmoeders_JulyCBKV1.sav",
                             col_select = c("RinPersoons", "Rinpersoon", "trajectid",
                                            "prestatiemaand", "ib_indeling", "aantal"))

gb_zorg_moeders_tot_2020 <- as.data.table(gb_zorg_moeders_tot_2020)
setnames(gb_zorg_moeders_tot_2020, c("RinPersoons", "Rinpersoon"), c("RINPERSOONS", "RINPERSOON"))


  #7. Kraamzorg indicatie en geleverde zorg (2016-2020)
ind_kz_tot_2020 <- read_spss("L:/8099Vektis_07indicatiesurenkraamzorg_JulyCBKV1.sav")

ind_kz_tot_2020 <- as.data.table(ind_kz_tot_2020)
setnames(ind_kz_tot_2020, c("RinPersoons", "Rinpersoon"), c("RINPERSOONS", "RINPERSOON"))


#inlezen 2021 en 2022 (losse bestanden)

#inlezen vektis dat met benodigde kolommen voor kraamzorg (2021-2022)
  # 1. Moeders, bevallingen en miskramen
bevallingen_2021_2022 <- read_spss("L:/8099Vektis_01Moedersenbevallingenenmiskramen202312CBKV1.sav",
                         col_select = c("RINPERSOONS", "Rinpersoon", "trajectid",
                                        "bevallingsmaand"))

bevallingen_2021_2022 <- as.data.table(bevallingen_2021_2022)
setnames(bevallingen_2021_2022, c("Rinpersoon"), c("RINPERSOON"))

  # 3. Declaraties geboortezorg per fase zwangerschap (2021-2022)
gb_zorg_moeders_2021_2022 <- read_spss("L:/8099Vektis_03Geboortezorgmoeders202312CBKV1.sav",
                                        col_select = c("RINPERSOONS", "Rinpersoon", "trajectid",
                                            "prestatiemaand", "ib_indeling", "aantal"))

gb_zorg_moeders_2021_2022 <- as.data.table(gb_zorg_moeders_2021_2022)
setnames(gb_zorg_moeders_2021_2022, c("Rinpersoon"), c("RINPERSOON"))


  #7. Kraamzorg indicatie en geleverde zorg (2021-2022)
ind_kz_2021_2022 <- read_spss("L:/8099Vektis_07indicatiesurenkraamzorg202312CBKV1.sav")

ind_kz_2021_2022 <- as.data.table(ind_kz_2021_2022)
setnames(ind_kz_2021_2022, c("Rinpersoon"), c("RINPERSOON"))

#Data verschillende jaren samenvoegen 
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

#------------------------------------------------------------------------------
#kwetsbaarheid inlezen
#------------------------------------------------------------------------------
library(fst)

kwetsbaarheid <- read.fst("H:/Naar 8099 & 9358/meervoudig_kwetsbaar2015_2021.fst")

#------------------------------------------------------------------------------
#algemene variabele berekenen - Percentage gezinnen dat geen gebruik van kraamzorg maakt
#------------------------------------------------------------------------------
vektis_koppel <- vektis_koppel[, ':=' (
  jaar = as.numeric(str_sub(bevallingsmaand, 1, 4)),
  kraamzorg = ifelse(!is.na(trajectid) & ib_indeling == "Kraamzorg per uur" & bevallingsmaand <= prestatiemaand, 1, 0)
)]

#vaststellen welke bevallingen kraamzorg hebben gehad
kraamzorg_nl <- merge(
  unique(vektis_koppel[, c("RINPERSOONS", "RINPERSOON", "jaar", "trajectid")]), #alle unieke bevallingen per traject per jaar
  unique(vektis_koppel[kraamzorg == 1, c("RINPERSOONS", "RINPERSOON", "jaar", "trajectid", "kraamzorg")]), #alle moeders die kraamzorg hebben gekregen per traject per jaar
  all.x = TRUE,
  by = c("RINPERSOONS", "RINPERSOON", "trajectid", "jaar")
)

#als een moeder NA heeft, dan heeft ze dus geen kraamzorg gehad
kraamzorg_nl[, ':=' (
  kraamzorg = ifelse(is.na(kraamzorg), 0 , kraamzorg)
)]

#koppelen alle levend geboren en Vektis moeders
kraamzorg_nl <- merge(kraamzorg_nl, levendgeboren_ouders, all = FALSE, #Vektis moet ook levendgeboren zijn
                      by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                      by.y = c("RINPERSOONSMa", "RINPERSOONMa", "jaar"))

#var berekenen en totalen toevoegen
kraamzorg_nl <- merge(
  kraamzorg_nl[kraamzorg == 0, .N, by = jaar],
  kraamzorg_nl[, .N, by = jaar],
  by = "jaar"
)

setnames(kraamzorg_nl,
         c("N.x", "N.y"),
         c("geen_kraamzorg", "tot_levendgeboren"))

kraamzorg_nl[, per_geen_kraamzorg := (geen_kraamzorg / tot_levendgeboren) * 100]

#export
write.csv2(kraamzorg_nl, "src/projecten/KS kraamzorg indicator/output 2023 met indicatie/kraamzorg_nl.csv")

#------------------------------------------------------------------------------
#var berekenen met uitsplitsing langdurige verblijf
#------------------------------------------------------------------------------
# Wanneer een van de volgende DBC's is gedeclareerd voor moeder of kind wordt een langdurig verblijf na geboorte veronderstelt:
  #159899012, 159899011, 990016382, 990016383, 990017011, 990017012, 990017015, 990017016, 990017021, 990017022, 990017023, 990017024, 
  #990017025, 990017026, 990017029, 990017030, 990017033, 990017034, 990017037, 990017038, 990017040, 990017041, 990017042, 990017043, 
  #990017046,  990017047, 990017048, 990017049

langdurig_verblijf <- list.files("H:/Data proces/data/MSZPrestatieKraam", pattern = ".csv", recursive = TRUE, full.names = TRUE)
langdurig_verblijf <- lapply(langdurig_verblijf, read_csv)
langdurig_verblijf <- rbindlist(langdurig_verblijf)
langdurig_verblijf[, jaar := as.numeric(str_sub(VEKTMSZEinddatumPrest, 1, 4))]

#vaststellen welke bevallingen kraamzorg hebben gehad
kraamzorg_lv_nl <- merge(
  unique(vektis_koppel[, c("RINPERSOONS", "RINPERSOON", "jaar", "trajectid")]), #alle unieke bevallingen per traject per jaar
  unique(vektis_koppel[kraamzorg == 1, c("RINPERSOONS", "RINPERSOON", "jaar", "trajectid", "kraamzorg")]), #alle moeders die kraamzorg hebben gekregen per traject per jaar
  all.x = TRUE,
  by = c("RINPERSOONS", "RINPERSOON", "trajectid", "jaar")
)

#als een moeder NA heeft, dan heeft ze dus geen kraamzorg gehad
kraamzorg_lv_nl[, ':=' (
  kraamzorg = ifelse(is.na(kraamzorg), 0 , kraamzorg)
)]

#koppelen aan moeders die langdurig verbijf hebben gehad
kraamzorg_lv_nl <- merge(kraamzorg_lv_nl, 
                         langdurig_verblijf[, c("rinpersoons", "rinpersoon", "jaar")][, ld_verblijf_moeder := "Ja"], 
                         all.x = TRUE,
                      by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                      by.y = c("rinpersoons", "rinpersoon", "jaar"))

#koppelen aan kinderen die langdurig verbijf hebben gehad
leevGeb_langVerbl<- merge(levendgeboren_ouders, 
                         langdurig_verblijf[, c("rinpersoons", "rinpersoon", "jaar", "ld_verblijf")][, ld_verblijf_kind := "Ja"], 
                         all.x = TRUE,
                         by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                         by.y = c("rinpersoons", "rinpersoon", "jaar"))

kraamzorg_lv_nl[, ld_verblijf := ifelse(ld_verblijf_kind == "Ja" | ld_verblijf_moeder == "Ja", "Ja", "Nee")]
kraamzorg_lv_nl[, ld_verblijf := ifelse(is.na(ld_verblijf), "Nee", ld_verblijf)]

#koppelen alle levend geboren en Vektis moeders
kraamzorg_lv_nl <- merge(kraamzorg_lv_nl, levendgeboren_ouders, all = FALSE, #Vektis moet ook levendgeboren zijn
                         by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                         by.y = c("RINPERSOONSMa", "RINPERSOONMa", "jaar"))

#var berekenen en totalen toevoegen
kraamzorg_lv_nl <- merge(
  kraamzorg_lv_nl[kraamzorg == 0, .N, c("jaar", "ld_verblijf")],
  kraamzorg_lv_nl[, .N, c("jaar", "ld_verblijf")],
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
  
kraamzorg_lv_nl[, per_geen_kraamzorg := (geen_kraamzorg / tot_levendgeboren) * 100]

#export
write.csv2(kraamzorg_lv_nl, "src/projecten/KS kraamzorg indicator/output 2023 met indicatie/kraamzorg_langdurig_verblijf_nl.csv")


#------------------------------------------------------------------------------
#indicatie aantal uur kraamzorg koppelen 
#------------------------------------------------------------------------------
indicatie_koppel_tot_2020 <- merge(gb_zorg_moeders_tot_2020%>% filter(!is.na(trajectid)), 
                                   ind_kz_tot_2020[!is.na(trajectid)], #traject id moet bekend zijn om bevallingsmaand vast te stellen
                                   all.x = TRUE, 
                                   by = c("RINPERSOONS", "RINPERSOON", "trajectid"),
                                   allow.cartesian=TRUE)

indicatie_koppel_2021_2022 <- merge(gb_zorg_moeders_2021_2022 %>% filter(!is.na(trajectid)), 
                                    ind_kz_2021_2022[!is.na(trajectid)], #traject id moet bekend zijn om bevallingsmaand vast te stellen
                                    all.x = TRUE, 
                                    by = c("RINPERSOONS", "RINPERSOON", "trajectid"),
                                    allow.cartesian=TRUE)


indicatie_koppel <- rbindlist(
  list(indicatie_koppel_tot_2020, indicatie_koppel_2021_2022),
  use.names = TRUE
)

indicatie_koppel <- indicatie_koppel[, ':=' (
  jaar = as.numeric(str_sub(bevallingsmaand, 1, 4)),
  kraamzorg = ifelse(!is.na(trajectid) & ib_indeling == "Kraamzorg per uur" & bevallingsmaand <= prestatiemaand, 1, 0),
  indicatie = fcase(
    omvang_indicatie == 0, "0",
    omvang_indicatie > 0 & omvang_indicatie < (24*60), "0 - 24",
    omvang_indicatie == (24*60), "24",
    omvang_indicatie > (24*60) & omvang_indicatie < (49*60), "24 - 49",
    omvang_indicatie > (49*60), "meer dan 49"
  ),
  ontvangen = fcase(
    aantal == 0, "0",
    aantal > 0 & omvang_indicatie < (24*60), "0 - 24",
    aantal == (24*60), "24",
    aantal > (24*60) & omvang_indicatie < (49*60), "24 - 49",
    aantal > (49*60), "meer dan 49"
  )
)]

#vaststellen welke bevallingen kraamzorg hebben gehad
indicatie_koppel <- merge(
  unique(indicatie_koppel[, c("RINPERSOONS", "RINPERSOON", "jaar", "trajectid", "indicatie", "ontvangen")]), #alle unieke bevallingen per traject per jaar
  unique(indicatie_koppel[kraamzorg == 1, c("RINPERSOONS", "RINPERSOON", "jaar", "trajectid", "kraamzorg", "indicatie", "ontvangen")]), #alle moeders die kraamzorg hebben gekregen per traject per jaar
  all.x = TRUE,
  by = c("RINPERSOONS", "RINPERSOON", "trajectid", "jaar",  "indicatie", "ontvangen")
)

#als een moeder NA heeft, dan heeft ze dus geen kraamzorg gehad
indicatie_koppel[, ':=' (
  kraamzorg = ifelse(is.na(kraamzorg), 0 , kraamzorg)
)]

#koppelen alle levend geboren en Vektis moeders
indicatie_koppel <- merge(indicatie_koppel, levendgeboren_ouders, all = FALSE, #Vektis moet ook levendgeboren zijn
                          by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                          by.y = c("RINPERSOONSMa", "RINPERSOONMa", "jaar"))

#------------------------------------------------------------------------------
#var berekenen met uitsplitsing indicatie uren
#------------------------------------------------------------------------------
#indicatie var berekenen en totalen toevoegen
indicatie <- merge(
  indicatie_koppel[kraamzorg == 0, .N, c("jaar", "indicatie")],
  indicatie_koppel[, .N, c("jaar", "indicatie")],
  by = c("jaar", "indicatie")
)

setnames(indicatie,
         c("N.x", "N.y"),
         c("geen_kraamzorg", "tot_levendgeboren"))

indicatie <- indicatie %>% 
  mutate(
    geen_kraamzorg = ifelse(geen_kraamzorg < 10, NA, geen_kraamzorg),
    tot_levendgeboren = ifelse(geen_kraamzorg < 10, NA, tot_levendgeboren)
  )

indicatie[, per_geen_kraamzorg := (geen_kraamzorg / tot_levendgeboren) * 100]

#export
write.csv2(indicatie, "src/projecten/KS kraamzorg indicator/output 2023 met indicatie/kraamzorg_indicatie_nl.csv")

#------------------------------------------------------------------------------
#var berekenen met uitsplitsing geleverde uren
#------------------------------------------------------------------------------
geleverd <- merge(
  indicatie_koppel[, .N, c("jaar", "ontvangen")],
  indicatie_koppel[, .N, c("jaar")],
  by = c("jaar")
)

setnames(geleverd,
         c("N.x", "N.y"),
         c("aantal", "tot_levendgeboren_met_kraamzorg"))

geleverd <- geleverd %>% 
  mutate(
    aantal = ifelse(aantal < 10, NA, aantal),
    tot_levendgeboren_met_kraamzorg = ifelse(tot_levendgeboren_met_kraamzorg < 10, NA, tot_levendgeboren_met_kraamzorg)
  )

geleverd[, perc := (aantal / tot_levendgeboren_met_kraamzorg) * 100]

#export
write.csv2(geleverd, "src/projecten/KS kraamzorg indicator/output 2023 met indicatie/kraamzorg_geleverd_nl.csv")

#------------------------------------------------------------------------------
#var berekenen met uitsplitsing geleverde uren
#------------------------------------------------------------------------------
meer_minder_tot_2020 <- merge(ind_kz_tot_2020[!is.na(trajectid)],
                              gb_zorg_moeders_tot_2020[!is.na(trajectid) & ib_indeling == "Kraamzorg per uur"],
                              all.x = TRUE, 
                              by = c("RINPERSOONS", "RINPERSOON", "trajectid"))

meer_minder_2021_2022 <- merge(ind_kz_2021_2022[!is.na(trajectid)],
                               gb_zorg_moeders_2021_2022[!is.na(trajectid) & ib_indeling == "Kraamzorg per uur"],
                               all.x = TRUE, 
                               by = c("RINPERSOONS", "RINPERSOON", "trajectid"))


meer_minder <- rbindlist(
  list(meer_minder_tot_2020, meer_minder_tot_2020),
  use.names = TRUE
)

meer_minder[, jaar := as.numeric(str_sub(bevallingsmaand, 1, 4))]

#koppelen alle levend geboren en Vektis moeders
meer_minder <- merge(meer_minder, levendgeboren_ouders, all = FALSE, #Vektis moet ook levendgeboren zijn
                          by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                          by.y = c("RINPERSOONSMa", "RINPERSOONMa", "jaar"))

meer_minder[, geleverde_kraamzorg := fcase(
  omvang_indicatie < aantal, "meer geleverd",
  omvang_indicatie == aantal,"geleverd naar indicatie",
  omvang_indicatie > aantal, "minder geleverd"
)]

meer_minder <- merge(
  meer_minder[, .N, c("jaar", "geleverde_kraamzorg")],
  meer_minder[, .N, c("jaar")],
  by = c("jaar")
)

setnames(meer_minder,
         c("N.x", "N.y"),
         c("aantal", "totaal"))

meer_minder <- meer_minder %>% 
  mutate(
    aantal = ifelse(aantal < 10, NA, aantal),
    totaal = ifelse(totaal < 10, NA, totaal)
  )

meer_minder[, perc := (aantal / totaal) * 100]

#export
write.csv2(meer_minder, "src/projecten/KS kraamzorg indicator/output 2023 met indicatie/kraamzorg_meer_minder_geleverd_nl.csv")

#------------------------------------------------------------------------------
#var berekenen met uitsplitsing kwetsbaarheid
#------------------------------------------------------------------------------
kwetsbaar <- list(vektis_koppel, 
                  kwetsbaarheid) %>% 
  reduce(left_join, by = c("RINPERSOONS", "RINPERSOON"), keep = FALSE)

kwetsbaar <- kwetsbaar[, ':=' (
  jaar = as.numeric(str_sub(bevallingsmaand, 1, 4)),
  kraamzorg = ifelse(!is.na(trajectid) & ib_indeling == "Kraamzorg per uur" & bevallingsmaand <= prestatiemaand, 1, 0)
)]

#vaststellen welke bevallingen kraamzorg hebben gehad
kwetsbaar <- merge(
  unique(kwetsbaar[, c("RINPERSOONS", "RINPERSOON", "jaar", "trajectid", "voorspelling_kwetsbaar")]), #alle unieke bevallingen per traject per jaar
  unique(kwetsbaar[kraamzorg == 1, c("RINPERSOONS", "RINPERSOON", "jaar", "trajectid", "kraamzorg","voorspelling_kwetsbaar")]), #alle moeders die kraamzorg hebben gekregen per traject per jaar
  all.x = TRUE,
  by = c("RINPERSOONS", "RINPERSOON", "trajectid", "jaar",  "voorspelling_kwetsbaar")
)

#als een moeder NA heeft, dan heeft ze dus geen kraamzorg gehad
kwetsbaar[, ':=' (
  kraamzorg = ifelse(is.na(kraamzorg), 0 , kraamzorg)
)]

#koppelen alle levend geboren en Vektis moeders
kwetsbaar <- merge(kwetsbaar, levendgeboren_ouders, all = FALSE, #Vektis moet ook levendgeboren zijn
                          by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                          by.y = c("RINPERSOONSMa", "RINPERSOONMa", "jaar"))

#indicatie var berekenen en totalen toevoegen
kwetsbaar <- merge(
  kwetsbaar[kraamzorg == 0, .N, c("jaar", "voorspelling_kwetsbaar")],
  kwetsbaar[, .N, c("jaar", "voorspelling_kwetsbaar")],
  by = c("jaar", "voorspelling_kwetsbaar")
)

setnames(kwetsbaar,
         c("N.x", "N.y"),
         c("geen_kraamzorg", "tot_levendgeboren"))

kwetsbaar <- kwetsbaar %>% 
  mutate(
    geen_kraamzorg = ifelse(geen_kraamzorg < 10, NA, geen_kraamzorg),
    tot_levendgeboren = ifelse(geen_kraamzorg < 10, NA, tot_levendgeboren)
  )

kwetsbaar[, per_geen_kraamzorg := (geen_kraamzorg / tot_levendgeboren) * 100]

#export
write.csv2(kwetsbaar, "src/projecten/KS kraamzorg indicator/output 2023 met indicatie/kraamzorg_kwetsbaarheid_nl.csv")

#------------------------------------------------------------------------------
#kraamzorg per gemeente (gem 5 jaar)
#------------------------------------------------------------------------------
gemeente <- merge(vektis_koppel,
                  locatie,
                  all.x = TRUE,
                  by = c("RINPERSOONS", "RINPERSOON", "jaar"))

gemeente <- gemeente[, ':=' (
  jaar = as.numeric(str_sub(bevallingsmaand, 1, 4)),
  kraamzorg = ifelse(!is.na(trajectid) & ib_indeling == "Kraamzorg per uur" & bevallingsmaand <= prestatiemaand, 1, 0)
)]

#laatste 5 jaar selecteren
gemeente <- gemeente[jaar %in% (max(jaren) - 4):max(jaren)]

#vaststellen welke bevallingen kraamzorg hebben gehad
gemeente <- merge(
  unique(gemeente[, c("RINPERSOONS", "RINPERSOON", "trajectid", "jaar", "gem2022")]), #alle unieke bevallingen per traject per jaar
  unique(gemeente[kraamzorg == 1, c("RINPERSOONS", "RINPERSOON", "trajectid", "gem2022", "jaar", "kraamzorg")]), #alle moeders die kraamzorg hebben gekregen per traject per jaar
  all.x = TRUE,
  by = c("RINPERSOONS", "RINPERSOON", "trajectid", "jaar","gem2022")
)

#als een moeder NA heeft, dan heeft ze dus geen kraamzorg gehad
gemeente[, ':=' (
  kraamzorg = ifelse(is.na(kraamzorg), 0 , kraamzorg)
)]

#koppelen alle levend geboren en Vektis moeders
gemeente <- merge(gemeente, levendgeboren_ouders, all = FALSE, #Vektis moet ook levendgeboren zijn
                  by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                  by.y = c("RINPERSOONSMa", "RINPERSOONMa", "jaar"))

#var berekenen en totalen toevoegen
gemeente <- merge(
  gemeente[kraamzorg == 0, .N, by = gem2022],
  gemeente[, .N, by = gem2022],
  by = "gem2022"
)

setnames(gemeente,
         c("N.x", "N.y"),
         c("geen_kraamzorg", "tot_levendgeboren"))

gemeente <- gemeente %>% 
  mutate(
    geen_kraamzorg = ifelse(geen_kraamzorg < 10, NA, geen_kraamzorg),
    tot_levendgeboren = ifelse(geen_kraamzorg < 10, NA, tot_levendgeboren)
  )

gemeente[, per_geen_kraamzorg := (geen_kraamzorg / tot_levendgeboren) * 100]

#export
write.csv2(gemeente, "src/projecten/KS kraamzorg indicator/output 2023 met indicatie/kraamzorg_gemeente.csv")

#------------------------------------------------------------------------------
#kraamzorg uitgesplitst naar eenoudergezin, opleiding moeder, huishoudinkomen en een of beide ouders ontvangt GGZ
#------------------------------------------------------------------------------
uitsplitsing_demo <- list(vektis_koppel, 
                          hoogstopl_data,
                          huishoud_inkomen,
                          ggz) %>% 
  reduce(left_join, by = c("RINPERSOONS", "RINPERSOON", "jaar"))

uitsplitsing_demo <- uitsplitsing_demo[, ':=' (
  jaar = as.numeric(str_sub(bevallingsmaand, 1, 4)),
  kraamzorg = ifelse(!is.na(trajectid) & ib_indeling == "Kraamzorg per uur" & bevallingsmaand <= prestatiemaand, 1, 0)
)]

uitsplitsing_demo <- uitsplitsing_demo[, ':=' (
  ggz = ifelse(!is.na(kosten_GGZ), 1, 0),
  laag_opgeleid = ifelse(opleidingsniveau == "laag", 1, 0),
  eenoudergezin = ifelse(hh_samenstelling == "eenouder", 1, 0)
)]

uitsplitsing_uitdraaien <- function(var) {
  #vaststellen welke bevallingen kraamzorg hebben gehad
  demo <- merge(
    unique(uitsplitsing_demo[, c("RINPERSOONS", "RINPERSOON", "jaar", "trajectid", var)]), #alle unieke bevallingen per traject per jaar
    unique(uitsplitsing_demo[kraamzorg == 1, c("RINPERSOONS", "RINPERSOON", "jaar", "trajectid", "kraamzorg",var)]), #alle moeders die kraamzorg hebben gekregen per traject per jaar
    all.x = TRUE,
    by = c("RINPERSOONS", "RINPERSOON", "trajectid", "jaar",  var)
  )
  
  #als een moeder NA heeft, dan heeft ze dus geen kraamzorg gehad
  demo[, ':=' (
    kraamzorg = ifelse(is.na(kraamzorg), 0 , kraamzorg)
  )]
  
  #koppelen alle levend geboren en Vektis moeders
  demo <- merge(demo, levendgeboren_ouders, all = FALSE, #Vektis moet ook levendgeboren zijn
                by.x = c("RINPERSOONS", "RINPERSOON", "jaar"),
                by.y = c("RINPERSOONSMa", "RINPERSOONMa", "jaar"))
  
  #indicatie var berekenen en totalen toevoegen
  demo <- demo %>% 
    filter(!is.na(var)) %>%
    group_by(jaar, kraamzorg) %>% 
    mutate(perc = mean(eval(parse(text = var)), na.rm = TRUE),
           aantal = sum(eval(parse(text = var))),
           totaal = n())
  
  #export
  write.csv2(demo, str_c("src/projecten/KS kraamzorg indicator/output 2023 met indicatie/kraamzorg_", var, "_nl.csv")) 
}




