# Indicator 10 (problematische schulden): 
# Deze cijfers zijn door het RIVM berekend in DIAPER en gebaseerd op gegevens van niet-openbare microdata van het CBS. 
# De studiepopulatie bestaat per jaar uit alle ouders (zwangere en partner) van levend- en 
# doodgeborenen (na een zwangerschap van minstens 24 weken) 
# die op 31 december van het betreffende jaar in de BasisRegistratie Personen (BRP) ingeschreven staan. 
# Per jaar is berekend welk deel van deze personen hulp heeft gekregen via de schuldsanering volgens de Wet schuldsanering  natuurlijke personen (Wsnp) 
# en/of bekend is als wanbetaler bij de zorgverzekering volgens de Zorgverzekeringswet 
# waarbij een verzekerde als wanbetaler wordt aangemerkt als de verzekerde minimaal 6 zes maanden geen premie voor de basisverzekering betaald heeft.  

# Indicator 11 (psychische problemen): 
# Deze cijfers zijn door het RIVM berekend in DIAPER en gebaseerd op gegevens van niet-openbare microdata van het CBS. 
# De studiepopulatie is dezelfde als die van indicator 10. Per jaar is berekend welk deel van deze personen zorguitgaven heeft gehad binnen de GGZ 
# en/of angst/depressie medicatie heeft gebruikt (bepaald op basis van declaraties voor de kosten van medicijngebruik).  

# Indicator 14 (meervoudig kwetsbare situatie): 
# Deze cijfers zijn door het RIVM berekend in DIAPER en gebaseerd op niet-openbare microdata van het CBS. 
# De studiepopulatie is dezelfde als die van indicator 10. 
# Bij de bepaling van de kwetsbare situatie zijn per zwangere de volgende gegevens gebruikt uit de niet-openbare microdata: 
# leeftijd, etniciteit, aantal bevallingen, asielzoekersstatus, opleidingsniveau, huishoudinkomen, sociaaleconomische positie (bron van inkomen), 
# schulden, type arbeidscontract, gezinssituatie, burgerlijke staat, scheiding, gezinsgrootte, jeugdondersteuning in gezin, 
# zorguitgaven (totaal, huisarts, ziekenhuis), medicatiegebruik, verslaving, licht verstandelijke beperking, 
# slachtoffer/ verdachte van misdrijf, detentie, verhuizingen, verlies partner of kind, woning- en motorvoertuigbezit, 
# afstand tot huisarts, leefbaarheid van de buurt. 

#gemeentelijk per jaar voor Regiobeeld
#populatie: zwangeren

#teller: zwangere vrouwen in jaar in kwestie die voldoen aan de variabele
#noemer: zwangere vrouwen in jaar in kwestie

#utilities
source("utils.R")

#lege data.frames
pshyc_totaal_ouders <- data.frame()
pshyc_inkomen_ouders <- data.frame()
pshyc_opleiding_ouders <- data.frame()
pshyc_kwetsbaar_ouders <- data.frame()
pshyc_gem23_ouders <- data.frame()

schulden_totaal_ouders <- data.frame()
schulden_inkomen_ouders <- data.frame()
schulden_opleiding_ouders <- data.frame()
schulden_kwetsbaar_ouders <- data.frame()
schulden_gem2023_ouders <- data.frame()

kwetsbaar_totaal <- data.frame()
kwetsbaar_inkomen <- data.frame()
kwetsbaar_opleiding <- data.frame()
kwetsbaar_gem2023 <- data.frame()

# Loop per datajaar data inlezen en variabelen uitdraaien ------------------

for(jaar in jaren){ 
  #------------------------------------------------------------------------------
  #INLEZEN DATA
  #------------------------------------------------------------------------------
  bevolking_met_leeftijd <- fread(paste0("H:/GIT data proces/ks data proces/@PP/Inlees data 2023/Leeftijd_met_regiocodes_", j, ".csv"), 
                    colClasses = list(character = c("RINPERSOON",
                                                    "gem2023", 
                                                    "PC4",
                                                    "RINPERSOONpa", 
                                                    "RINPERSOONMa")))
  if(!"huishoud_inkomen" %in% ls(envir = .GlobalEnv)) {
    source("inlezen_data/2025/huishoud_inkomen.R")
  }

  if(!"ggzkosten" %in% ls(envir = .GlobalEnv)) {

    ggzkosten <- fread("H:/GIT data proces/ks data proces/@PP/Inlees data 2023/ggzkosten.csv", colClasses = list(character = "RINPERSOON"))
  }

  if(!"kwetsbaarheid" %in% ls(envir = .GlobalEnv)) {
    kwetsbaarheid <- read.fst("H:/Data proces/src/projecten/Kwetsbaarheid/2024 update/Data/meervoudig_kwetsbaar24_v2.fst")
  }

  hoogstopl_data <- fread(paste0("H:/GIT data proces/ks data proces/@PP/Inlees data 2023/Opleiding_", j, ".csv"), colClasses = list(character = "RINPERSOON"))
  med_gebruik <- fread(paste0("H:/GIT data proces/ks data proces/@PP/Inlees data 2023/Medicijn_gebruik_", j,".csv"), colClasses = list(character = "RINPERSOON"))
  schuldsanering <- fread(paste0("H:/GIT data proces/ks data proces/@PP/Inlees data 2023/Schuldsanering_", j,".csv"))
  zvw_wanbetaler <- fread(paste0("H:/GIT data proces/ks data proces/@PP/Inlees data 2023/Zvw_wanbetaler_", j,".csv"), colClasses = list(character = "RINPERSOON"))

  #------------------------------------------------------------------------------
  #OPBOUWEN POPULATIE ZWANGERE VROUWEN
  # #------------------------------------------------------------------------------


  # maak populatie zwangeren door alle kinderen te selecteren die in de gevraagde jaren zijn geboren (levend en dood)
  # De moeders van deze kinderen zijn de zwangeren
  zwangeren <- bevolking_met_leeftijd %>%
    filter(year(geboortedatum) == jaar) %>%
    rename(RINPERSOON_KIND = RINPERSOON, RINPERSOONS_KIND = RINPERSOONS) %>% #kinderen definieren 
    rename(RINPERSOON = RINPERSOONMa, RINPERSOONS = RINPERSOONSMa) #moeders primaire RIN  


  #Unieke moeders per kalender jaar
  zwangeren <- zwangeren %>% group_by(RINPERSOON) %>% slice_head(n=1) %>% ungroup()

  rm(bevolking_met_leeftijd)

  #------------------------------------------------------------------------------
  #KOPPELEN DATA
  #------------------------------------------------------------------------------
  #variabelen moeder koppelen
  zwangeren <- Reduce(function(df1, df2) merge(df1 ,df2, all.x = TRUE, 
                                              by = c("jaar", "RINPERSOONS", "RINPERSOON")),
                      list(zwangeren, 
                          med_gebruik[, c("jaar", "RINPERSOONS", "RINPERSOON", "med_psych_klachten", "Gebruik_depress_angst_medicatie")],
                          ggzkosten,
                          schuldsanering, 
                          zvw_wanbetaler,
                          huishoud_inkomen[, c("jaar", "RINPERSOONS", "RINPERSOON", "ink_kwint", "ink_10p", "lage_inkomensgrens", "lage_inkomensgrens_langdurig")],
                          hoogstopl_data[, c("jaar", "RINPERSOONS", "RINPERSOON", "opleidingsniveau")]
                        ))
  #variabelen vader koppelen
  zwangeren <- Reduce(function(df1, df2) merge(df1 ,df2, all.x = TRUE, suffixes = c("_Ma", "_Pa"),
                                              by.x = c("jaar", "RINPERSOONSpa", "RINPERSOONpa"),
                                              by.y = c("jaar", "RINPERSOONS", "RINPERSOON")),
                      list(zwangeren, 
                          med_gebruik[, c("jaar", "RINPERSOONS", "RINPERSOON", "med_psych_klachten", "Gebruik_depress_angst_medicatie")],
                          ggzkosten,
                          schuldsanering, 
                          zvw_wanbetaler,
                          ggzkosten,
                          huishoud_inkomen[, c("jaar", "RINPERSOONS", "RINPERSOON", "ink_kwint", "ink_10p", "lage_inkomensgrens", "lage_inkomensgrens_langdurig")],
                          hoogstopl_data[, c("jaar", "RINPERSOONS", "RINPERSOON", "opleidingsniveau")]
                      ))

  #kwetsbaarheid op RIN kind toevoegen
  zwangeren <- merge(zwangeren, kwetsbaarheid, by.x= c("RINPERSOONS_KIND", "RINPERSOON_KIND") ,by.y= c("Rinpersoons_KIND", "RINPERSOON_KIND"), all.x =T)
  zwangeren <- data.table(zwangeren)

  #binaire waarden toevoegen
  zwangeren[, ':=' (
    schulden_Ma = ifelse(schuldsanering_Ma == 1 | zvw_wanbetaler_Ma == TRUE, 1, 0),
    psych_prob_Ma = ifelse(kosten_GGZ_Ma == 1 | med_psych_klachten_Ma == 1, 1, 0),
    laag_opgeleid_Ma = ifelse(opleidingsniveau_Ma == "laag", 1, 0),
    schulden_Pa = ifelse(schuldsanering_Pa == 1 | zvw_wanbetaler_Pa == TRUE, 1, 0),
    laag_opgeleid_Pa = ifelse(opleidingsniveau_Pa == "laag", 1, 0),
    kwetsbaar = ifelse(voorspelling_kwetsbaar == "ja",1,0)
  )]

  #Waar relevant NA's op 0 zetten
  zwangeren[, schulden_Ma := ifelse(is.na(schulden_Ma), 0, schulden_Ma)]
  zwangeren[, schulden_Pa := ifelse(is.na(schulden_Pa), 0, schulden_Pa)]

  #Waarden en/of vader/moeder positief
  zwangeren[, ':=' (
    psych_prob_ouders = ifelse(psych_prob_Ma == 1 | psych_prob_Pa == 1, 1,0),
    schulden_ouders =ifelse(schulden_Ma == 1 | schulden_Pa ==1, 1,0),
    )]

  #waarden op 0 zetten wanneer niet 1
  zwangeren[is.na(psych_prob_ouders), psych_prob_ouders := 0]
  zwangeren[is.na(schulden_ouders), schulden_ouders := 0]

  # percentages per jaar ----------------------------------------------------

  #aantallen met uitsplitsingen psychische problematiek 
  psych_prob_ouders_totaal_ouders <-  zwangeren %>% group_by(psych_prob_ouders) %>% summarise(n = n()) %>% mutate(jaar = j) %>% mutate(n = ifelse(n <10, "<10", as.character(n)))
  psych_prob_ouders_inkomen_ouders <-  zwangeren %>% group_by(ink_kwint_Ma, psych_prob_ouders) %>% summarise(n = n()) %>% mutate(jaar = j) %>% mutate(n = ifelse(n <10, "<10", as.character(n)))
  psych_prob_ouders_opleiding_ouders <-  zwangeren %>% group_by(opleidingsniveau_Ma, psych_prob_ouders) %>% summarise(n = n()) %>% mutate( jaar = j) %>%mutate(n = ifelse(n <10, "<10", as.character(n)))
  psych_prob_ouders_kwetsbaar_ouders <-  zwangeren %>% group_by(kwetsbaar, psych_prob_ouders) %>% summarise(n = n()) %>% mutate(jaar = j) %>% mutate(n = ifelse(n <10, "<10", as.character(n)))
  psych_prob_ouders_gem23_ouders <-  zwangeren %>% group_by(gem2023, psych_prob_ouders) %>% summarise(n = n()) %>% mutate(jaar = j)  %>% pivot_wider(id_cols = "gem2023", names_glue = "{.value}_{psych_prob_ouders}" , names_from = "psych_prob_ouders", values_from = "n") %>%  mutate(n_1 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_1)), n_0 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_0)))  %>% mutate(jaar = j)                  
 
  #aantallen met uitsplitsingen schulden problematiek 
  schulden_prob_ouders_totaal_ouders <-  zwangeren %>% group_by(schulden_ouders) %>% summarise(n = n()) %>% mutate( jaar = j) %>% mutate(n = ifelse(n <10, "<10", as.character(n)))
  schulden_prob_ouders_inkomen_ouders <-  zwangeren %>% group_by(ink_kwint_Ma, schulden_ouders) %>% summarise(n = n()) %>% mutate(jaar = j) %>%mutate(n = ifelse(n <10, "<10", as.character(n)))
  schulden_prob_ouders_opleiding_ouders <-  zwangeren %>% group_by(opleidingsniveau_Ma, schulden_ouders) %>% summarise(n = n()) %>% mutate(jaar = j) %>% mutate(n = ifelse(n <10, "<10", as.character(n)))
  schulden_prob_ouders_kwetsbaar_ouders <-  zwangeren %>% group_by(kwetsbaar, schulden_ouders) %>% summarise(n = n()) %>% mutate( jaar = j) %>% mutate(n = ifelse(n <10, "<10", as.character(n)))
  schulden_prob_ouders_gem23_ouders <-  zwangeren %>% group_by(gem2023, schulden_ouders) %>% summarise(n = n()) %>% mutate(jaar = j) %>% pivot_wider(id_cols = "gem2023", names_glue = "{.value}_{schulden_ouders}" , names_from = "schulden_ouders", values_from = "n") %>%  mutate(n_1 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_1)), n_0 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_0))) %>% mutate(jaar = j)

  #aantallen met uitsplitsingen kwetsbaar  
  kwetsbaar_moeder_totaal <-  zwangeren %>% group_by(kwetsbaar) %>% summarise(n = n()) %>% mutate(jaar = j) %>% mutate(n = ifelse(n <10, "<10", as.character(n)))
  kwetsbaar_moeder_inkomen <-  zwangeren %>% group_by(ink_kwint_Ma, kwetsbaar) %>% summarise(n = n()) %>% mutate(jaar = j) %>% mutate(n = ifelse(n <10, "<10", as.character(n)))
  kwetsbaar_moeder_opleiding <-  zwangeren %>% group_by(opleidingsniveau_Ma, kwetsbaar) %>% summarise(n = n()) %>% mutate(jaar = j) %>%mutate(n = ifelse(n <10, "<10", as.character(n)))
  kwetsbaar_moeder_gem2023 <-  zwangeren %>% group_by(gem2023, kwetsbaar) %>% summarise(n = n()) %>% mutate(jaar = j) %>% pivot_wider(id_cols = "gem2023", names_glue = "{.value}_{kwetsbaar}" , names_from = "kwetsbaar", values_from = "n") %>%  mutate(n_1 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_1)), n_0 = ifelse(n_1 <10 | n_0 <10, "<10", as.character(n_0))) %>% mutate(jaar = j)                   

 #koppelen jaar bestanden aan cummulatieve bestanden

  pshyc_totaal_ouders <- rbind(pshyc_totaal_ouders,psych_prob_ouders_totaal_ouders)
  pshyc_inkomen_ouders <- rbind(pshyc_inkomen_ouders,psych_prob_ouders_inkomen_ouders)
  pshyc_opleiding_ouders <- rbind(pshyc_opleiding_ouders,psych_prob_ouders_opleiding_ouders)
  pshyc_kwetsbaar_ouders <- rbind(pshyc_kwetsbaar_ouders,psych_prob_ouders_kwetsbaar_ouders)
  pshyc_gem23_ouders <- rbind(pshyc_gem23_ouders,psych_prob_ouders_gem23_ouders)

  schulden_totaal_ouders <- rbind(schulden_totaal_ouders,schulden_prob_ouders_totaal_ouders)
  schulden_inkomen_ouders <- rbind(schulden_inkomen_ouders,schulden_prob_ouders_inkomen_ouders)
  schulden_opleiding_ouders <- rbind(schulden_opleiding_ouders,schulden_prob_ouders_opleiding_ouders)
  schulden_kwetsbaar_ouders <- rbind(schulden_kwetsbaar_ouders,schulden_prob_ouders_kwetsbaar_ouders)
  schulden_gem2023_ouders <- rbind(schulden_gem2023_ouders,schulden_prob_ouders_gem23_ouders)

  kwetsbaar_totaal <- rbind(kwetsbaar_totaal,kwetsbaar_moeder_totaal)
  kwetsbaar_inkomen <- rbind(kwetsbaar_inkomen, kwetsbaar_moeder_inkomen)
  kwetsbaar_opleiding <- rbind(kwetsbaar_opleiding,kwetsbaar_moeder_opleiding)
  kwetsbaar_gem2023 <- rbind(kwetsbaar_gem2023,kwetsbaar_moeder_gem2023)

  }

#------------------------------------------------------------------------------
#EXPORT 
#------------------------------------------------------------------------------

rm(zwangeren)

#lijst met alle data.frames en daarmee uitkomsten uit enviroment
listframes <- ls()[sapply(ls(), function(x) is.data.frame(get(x)))]

#Data opslaan
for(df in listframes){
  write(get(df), paste0("Output/KS24/", df, ".csv"))
}

