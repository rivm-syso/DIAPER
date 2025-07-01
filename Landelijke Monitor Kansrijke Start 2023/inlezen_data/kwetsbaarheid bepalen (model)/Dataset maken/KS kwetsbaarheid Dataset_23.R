#Indicatoren kwetsbaarheid

#Voor het bepalen van of een persoon in een meervoudige kwetsbare situatie zit voor de zwangerschap
#Hiertoe is een model ontwikkeld door  Molenaar J. et al (2023) icm met Gezondheidsmonitor data (resultaten hiervan in Kansrrijke Start monitors '21 en '22)
#Dit bestand is de basis voor het vormen van een landelijk dekkend beeld van meervoudige kwetsbaarheid bij vrouwen voor de zwangerschap
#Hiertoe wordt landelijke geboorte registratie data (Perined) gebruikt en verrijkt met data demografisch en socioeconomische data vanuit de CBS-Microdata, 
#voor veel bestanden wordt er gekeken naar het jaar voorafgaand aan het de zwangerschap aangezienn we inzicht willen krijgen op risico's voor de start van de zwangerschap
#Dit databestand wordt gebruikt als input voor het ontwikkelde model om een landelijk beeld te geven van het aandeel vrouwen die meervoudig kwetsbaar zijn
#Resultaten hiervan zijn eerder gepubliceerd in de Kansrrijke Start monitor 2023 en is ingediend als wetenschappelijk artikel


#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")
source("H:/PP/utils_PP.R")
library(fst)


jaren <- c(2014:2022) #1 jaar ervoor meenemen ivm terugkijken
years <- jaren
#Gemeente indeling
gemeente_indeling <- "gem2022"

#------------------------------------------------------------------------------
#inlezen tijdelijke data
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
##Perined inlezen
#------------------------------------------------------------------------------

#Tijdelijk bestand aangeleverd voor de jaren 2021 en 2022 
  #koppeling van 2021 is beter in het nieuw toegevoegde bestand (deze gebruiken ipv oude)
  #2022 data is nog incompleet en mist voornamelijk thuisbevallingen
    #De verwachting is dat eind Q1 2024 een nieuwe Perined aanlevering komt met waarbij 2024 beter gevuld zou moeten zijn

#Perined variabelen
perined_variabelen <- c("par", "geboortegew", "sterfte", "amww", "type_partus", "fluxus", "nicuopname", "hoftiezer")

#Inlezen data met relevante kolommen 14-20 
perined_data <- read_spss("G:/Maatwerk/PERINED_RIVM/Perined2000_2021CBKV1.sav",
                          col_select = c("RINPERSOONS_Kind","Rinpersoon_Kind",  "RINPERSOONS_Moeder","Rinpersoon_Moeder","jaar",
                                         perined_variabelen))
perined_data <- as.data.table(perined_data)

#jaren filteren
perined_data <- perined_data[jaar %in% 2014:2020]

 setnames(perined_data, 
         c("RINPERSOONS_Moeder", "Rinpersoon_Moeder",  "RINPERSOONS_Kind","Rinpersoon_Kind"),
         c("RINPERSOONS", "RINPERSOON", "Rinpersoons_KIND","RINPERSOON_KIND"))


#lege RIN omzetten naar missings
perined_data[, ':=' (RINPERSOONS = ifelse(RINPERSOONS == "", NA, RINPERSOONS),
                     RINPERSOON = ifelse (RINPERSOON == "" | RINPERSOON == "000000000", NA, RINPERSOON))]


#Inlezen data met relevante kolommen 21-22
perined_data2122 <- read_spss("L:/8099Perined2021_2022RIVMCBKV1.sav",
                          col_select = c("RINPERSOONS_Kind","Rinpersoon_Kind",  "RINPERSOONS_Moeder","Rinpersoon_Moeder","jaar",
                                         perined_variabelen))
perined_data2122 <- data.table(perined_data2122)

setnames(perined_data2122, 
         c("RINPERSOONS_Moeder", "Rinpersoon_Moeder",  "RINPERSOONS_Kind","Rinpersoon_Kind"),
         c("RINPERSOONS", "RINPERSOON", "Rinpersoons_KIND","RINPERSOON_KIND"))


#lege RIN omzetten naar missings
perined_data2122[, ':=' (RINPERSOONS = ifelse(RINPERSOONS == "", NA, RINPERSOONS),
                     RINPERSOON = ifelse (RINPERSOON == "" | RINPERSOON == "000000000", NA, RINPERSOON))]

# Twee verschillende Perined bestanden samenvoegen
DT <- rbind(perined_data, perined_data2122)
DT <- unique(DT)

rm(perined_data, perined_data2122)

#toevoegen RIN vader 
#Bepalen locaties laatste dataset
kindouder_locatie <- list.files("G:/Bevolking/KINDOUDERTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)
kindouder_locatie <- kindouder_locatie[length(kindouder_locatie)]

#Inlezen datasets met alleen relevante kolommen
kindouder <- read_spss(kindouder_locatie)
kindouder <- as.data.table(kindouder)

#namen omzetten
setnames(kindouder, 
         c("RINPERSOONS", "RINPERSOON"),
         c("Rinpersoons_KIND", "RINPERSOON_KIND"))


DT <- merge(DT, kindouder[, c("Rinpersoons_KIND","RINPERSOON_KIND", "RINPERSOONSpa","RINPERSOONpa")],
             all.x = T, by= c("Rinpersoons_KIND","RINPERSOON_KIND"))

rm(kindouder)

#------------------------------------------------------------------------------
##CSB inlezen
#------------------------------------------------------------------------------

#huishoudnummer
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/huishoudnummer.R")
DT <- merge(DT, huishoudkoppel_data,  all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar") )
rm(huishoudkoppel_data)

#------------------------------------------------------------------------------
##CSBS Individuele karakteristieken 
#------------------------------------------------------------------------------

#Leeftijd op moment bevalling
source("src/indicatoren/leeftijd.R")

DT <- merge(DT, leeftijd[, c("RINPERSOONSkind","RINPERSOONkind", "lft_bevalling_moeder", "lft_bevalling_vader")], all.x= T, by.x= c("Rinpersoons_KIND", "RINPERSOON_KIND"), by.y= c("RINPERSOONSkind","RINPERSOONkind"))
rm(leeftijd)
  
  #leeftijdscategorie (kwetsbaarheidscatogoriën)
DT[lft_bevalling_moeder <24, leeftijd_cat := 1]
DT[lft_bevalling_moeder >= 24 & lft_bevalling_moeder <= 35, leeftijd_cat := 3]
DT[lft_bevalling_moeder > 35, leeftijd_cat := 2]


#Herkomstland (oude indeling ivm indeling bij gebruik ontwikkelen model)
source("src/indicatoren/etniciteit.R")
etnische_achtergrond[migratieachtergrond == "Autochtoon", etniciteit := 3]
etnische_achtergrond[migratieachtergrond == "Onbekend", etniciteit := NA]
etnische_achtergrond[migratieachtergrond == "Overige westerse landen", etniciteit := 2]
etnische_achtergrond[migratieachtergrond == "Overige niet-westerse landen", etniciteit := 1 ]
etnische_achtergrond[migratieachtergrond == "Suriname", etniciteit := 1]
etnische_achtergrond[migratieachtergrond == "Marokko", etniciteit := 1]
etnische_achtergrond[migratieachtergrond == "Voormalige Nederlandse Antillen en Aruba", etniciteit := 1]
etnische_achtergrond[migratieachtergrond == "Turkije", etniciteit := 1]

DT <- merge(DT, etnische_achtergrond[, c("RINPERSOONS","RINPERSOON", "etniciteit")], all.x= T, by= c("RINPERSOONS","RINPERSOON"))
rm(etnische_achtergrond)

#Etniciteit cat
DT[, etniciteit_cat := etniciteit]


#pariteit 
DT[par == 0, Par_3cat := 1]
DT[par > 0, Par_3cat := 2]


#Asielzoeker
source("src/indicatoren/asielcohort.R")

DT <- merge(DT, asielcohort[, c("RINPERSOONS","RINPERSOON", "asielcohort")], all.x = T, by= c("RINPERSOONS","RINPERSOON"))
DT[is.na(asielcohort), asielcohort := FALSE]
rm(asielcohort)

  #asiel cat
DT[asielcohort == T, asielmigrant := 1]
DT[asielcohort == F, asielmigrant := 2]

#------------------------------------------------------------------------------
##CBS Sociaal economische karakteristieken 
#------------------------------------------------------------------------------

#Opleidingsniveau  
source("src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/opleiding.R") #jaren aangepast
DT <- merge(DT, hoogstopl_data, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(hoogstopl_data)
  #Opleidingscat 
DT[opleidingsniveau == "laag", opleidingsniveau_cat := 1]
DT[opleidingsniveau == "midden", opleidingsniveau_cat := 2]
DT[opleidingsniveau == "hoog", opleidingsniveau_cat := 3]
jaren = years


#Huishoudinkomen 
source("src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/huishoud_inkomen.R") 
DT <- merge(DT, huishoud_inkomen[, c("RINPERSOONS","RINPERSOON", "jaar","ink_Kstart")], all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(huishoud_inkomen, huishoudkoppel_locatie, inhatab_locatie)
  #inkomen cat
DT[, inkomen_cat := ink_Kstart]

#Sociaal economische positie
  #inkomstenbron
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/Inkomensbron.R")
DT <- merge(DT, SECMBUS_filt, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
DT <- merge(DT, SECM2[, c("RINPERSOONS","RINPERSOON", "jaar","nBaanwissel")], all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(SECM2, SECMBUS)
DT <- data.table(DT)
DT[, SECMverandering := 3]
DT[nBaanwissel == 1, SECMverandering := 2]
DT[nBaanwissel > 1, SECMverandering := 1]

#Samenvoegen geen inkomen & uitkering
DT[SECM == 1 | SECM == 2, SECM_cat := 1]
DT[SECM == 3, SECM_cat := 2]
DT[SECM == 4, SECM_cat := 3]
DT[, SECM := SECM_cat]

#Schulden (ZVW wanbetaler) 
source("src/indicatoren/zvw_wanbetaler.R")
  #jaar terugkijken
zvw_wanbetaler[, jaar := jaar+1]
DT <- merge(DT, zvw_wanbetaler, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(zvw_wanbetaler)

#Schulden (schuldsanering) 
source("src/indicatoren/schuldsanering.R")
  #jaar terugkijken
schuldsanering[, jaar := jaar+1]
DT <- merge(DT, schuldsanering, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(schuldsanering)


  #financi?le problemen 
DT[, financieleproblemen := 2]
DT[zvw_wanbetaler ==T | schuldsanering ==T, financieleproblemen := 1]




#Vast contract (werk)  ######## OP ZWARE SERVER DRAAIEN  ######## 
# &  Voltijds werkend 
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/Dienstverband.R")
DT <- merge(DT, Dienstverband, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(Dienstverband)

#c cat 
DT[is.na(contractsoort), contractsoort := 1] #geen baan/missing
DT[contractsoort == 3, contractsoort := 1]   #Niet van toepassing (bepaald/onbepaalde tijd)

#dienstverband
DT[dienstverband ==1, d_cat :=1] #deeltijd
DT[dienstverband ==2, d_cat :=2] #voltijd
DT[is.na(dienstverband), d_cat :=1] #geen baan/missing
DT[, dienstverband := d_cat]



#------------------------------------------------------------------------------
## Huishouden  karakteristieken 
#------------------------------------------------------------------------------


# Type huishouden & 
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/huishoud_inkomen.R")
DT <- merge(DT, huishoud_inkomen[, c("RINPERSOONS","RINPERSOON", "jaar","omvang_huishouden" , "eenouder") ], all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(huishoud_inkomen)
  #eenouder gezin (dit halen we ook uit het Inhatab-bestand)
DT[, eenoudergezin := 2]
DT[eenouder == 1 | omvang_huishouden == 1, eenoudergezin := 1]
  #aantal pers. huishouden
DT[omvang_huishouden >= 6, aantalprshh_cat := 1]
DT[omvang_huishouden < 6, aantalprshh_cat := 2]


# Huwelijksstatus
source("src/indicatoren/burgerlijke_staat.R")
#jaar terugkijken
burgerlijke_staat[, jaar := jaar+1]
DT <- merge(DT, burgerlijke_staat, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(burgerlijke_staat)
DT[, gehuwdpaar := 1]
DT[burgerlijk_staat == "Gehuwd", gehuwdpaar := 2]


# Scheiding afelopen 4 jaar 
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/Scheidingen.R")
DT <- merge(DT, scheidingen, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(scheidingen)
  #scheid cat
DT[, scheiding := 2]
DT[scheiding4J== T, scheiding := 1]



# Jeugdhulp
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/Jeugdhulp.R")
DT <- merge(DT, jeugdhulpMA, all.x = T, by.x= c("RINPERSOONS","RINPERSOON", "jaar"), by.y= c("RINPERSOONSMa","RINPERSOONMa", "jaar"))
DT <- merge(DT, jeugdhulpPA, all.x = T, by.x= c("RINPERSOONSpa","RINPERSOONpa", "jaar"), by.y= c("RINPERSOONSpa","RINPERSOONpa", "jaar"))
DT[jeugdhulp.x ==T, jeugdhulp := T]
DT[jeugdhulp.y ==T, jeugdhulp := T]
DT[is.na(jeugdhulp), jeugdhulp := F]
DT[, jeugdhulp.x := NULL]
DT[, jeugdhulp.y := NULL]
rm(jeugdhulpMA, jeugdhulpPA)

#Jeugdbescherming
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/Jeugdbescherming.R")
DT <- merge(DT, jeugdbeschMA, all.x = T, by.x= c("RINPERSOONS","RINPERSOON", "jaar"), by.y= c("RINPERSOONSMa","RINPERSOONMa", "jaar"))
DT <- merge(DT, jeugdbeschPA, all.x = T, by.x= c("RINPERSOONSpa","RINPERSOONpa", "jaar"), by.y= c("RINPERSOONSpa","RINPERSOONpa", "jaar"))
DT[Jeugbescherming.x ==T, Jeugdbescherming := T]
DT[Jeugbescherming.y ==T, Jeugdbescherming := T]
DT[is.na(Jeugdbescherming), Jeugdbescherming := F]
DT[, Jeugbescherming.x := NULL]
DT[, Jeugbescherming.y := NULL]
rm(jeugdbeschMA, jeugdbeschPA)

#Jeugdondersteuning
DT[, jgdondersteuning := 2]
DT[jeugdhulp == T | Jeugdbescherming == T , jgdondersteuning := 1 ]

#------------------------------------------------------------------------------
## Zorgkosten & gebruik
#------------------------------------------------------------------------------
##zware server##
#Totale zvwkosten
  #Huisartskosten
  #ziekenhuiskosten
  #GGZKosten   
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/zorgkosten_kwetsbaarheid.R")
DT <- merge(DT, zorgkosten, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(zorgkosten)
gc()

#kwintielen toevoegen  (kwintielen per jaar ipv over totaal toevoegen)
DT[, zk_2j_percentiel := ntile(zorgkosten_totaal , 100), jaar]
DT[, zk_2j_kwintiel := ntile(zorgkosten_totaal , 5), jaar]
DT[, HA_2j_kwintiel := ntile(zorgkosten_huisarts, 5), jaar]
DT[, ZH_2j_deciel := ntile(zorgkosten_ziekenhuis, 10), jaar]

#zorgkosten_totaal_cat
DT[zk_2j_kwintiel == 5, zvwkosten_totaal_cat := 1]
DT[zk_2j_kwintiel < 5, zvwkosten_totaal_cat := 2]

#huisartskosten 
DT[HA_2j_kwintiel == 5, huisartskosten_cat := 1]
DT[HA_2j_kwintiel < 5, huisartskosten_cat := 2]

#ziekenhuis 
DT[ZH_2j_deciel == 10, ziekenhuiskosten_cat := 1]
DT[ZH_2j_deciel < 10, ziekenhuiskosten_cat := 2]
DT[is.na(ZH_2j_deciel), ziekenhuiskosten_cat := 2]




#medicatie gebruik (hoog gebruik) 
source("src/indicatoren/medicijn_gebruik.R")
  #jaar terugkijken
med_gebruik[, jaar := jaar+1]
#hoog_gebruik[, jaar := jaar+1]
DT <- merge(DT, med_gebruik, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
#DT <- merge(DT, hoog_gebruik, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(med_gebruik, hoog_gebruik)
DT[, Hoogmedicijngebruik := 2]
DT[hoog_gebruik == T, Hoogmedicijngebruik := 1]

#Verslavingszorg 
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/Verslavingszorg.R")
DT <- merge(DT, Verslavingszorg, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(Verslavingszorg)

DT[is.na(verslavingszorg), verslavingszorg := 2]

#------------------------------------------------------------------------------
## Psychosociale karakteristieken
#------------------------------------------------------------------------------
# -	Kosten GGZ | ziektekosten

# Lichtelijk verstandlijk beparkt (LVB)
#gebruik oude data
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/LVB.R")
DT <-  merge(DT, LVB, all.x = T, by= c("RINPERSOONS","RINPERSOON"))
rm(LVB)
DT[, LVBregistratie := 2]
DT[LVB == T, LVBregistratie := 1]



#gebruik ggz (op basis van zorgkosten)
DT[kosten_GGZ == 1, GGZ := 1]
DT[kosten_GGZ == 0, GGZ := 2]

#------------------------------------------------------------------------------
## Life events
#------------------------------------------------------------------------------
# Delict/misdrijf verachte 
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/Verdachte.R")
DT <- merge(DT, Verdachten, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(Verdachten)
DT[, Verdachte_misdrijf := 2]
DT[Verdachte ==T, Verdachte_misdrijf := 1]

# Slachtoffer misdrijf  
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/Slachtoffer.R")
DT <- merge(DT, slachtoffers, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(slachtoffers)
DT[, Slachtoffer_misdrijf := 2]
DT[slachtoffer ==T, Slachtoffer_misdrijf := 1]

# Gevangen gezeten 
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/detentie_KS.R")
DT <- merge(DT, gevangen, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(gevangen)
DT[, gedetineerd := 2]
DT[detentie  == T, gedetineerd := 1]


# Frequent verhuizer
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/Verhuizingen.R")
DT <- merge(DT, verhuizingen, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
DT <- data.table(DT)
rm(verhuizingen)
DT[, Aantalverhuis := 2]
DT[verhuizingen == 1, Aantalverhuis := 1]


# Verlies familielid (ouder/kind)  
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/NaasteVerlies.R")
DT <- merge(DT, VerliesKinder, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
DT <- merge(DT, VerliesOuders, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))

#combi variabele kind of ouder verloren
DT <- data.table(DT)
DT[DoodOuder5J == T | DoodKind5J == T, VerliesNaaste := T]
DT[, overlijdennaaste5jaar := 2]
DT[VerliesNaaste == T, overlijdennaaste5jaar := 1]

rm(VerliesKinder, VerliesOuders)

#------------------------------------------------------------------------------
## Living conditions
#------------------------------------------------------------------------------

# WoningBezit 
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/WoningBezit.R")
DT <- merge(DT, huishoud_huisbezit, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(huishoud_huisbezit)
DT[woningBezit == 3, woningBezit :=1]

#Voertuig bezit 
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/RDW.R")
DT <- merge(DT, RDW[, c("RINPERSOONS","RINPERSOON", "jaar", "RDW")], all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
RDW[, c("RINPERSOONS", "RINPERSOON") := NULL]
RDW <- unique(RDW)
DT <- merge(DT, RDW[, c("RINPERSOONSHKW", "RINPERSOONHKW", "jaar", "RDW")], all.x = T,c("RINPERSOONSHKW", "RINPERSOONHKW", "jaar"))

DT[is.na(RINPERSOONHKW), RDW.y :=F]
setnames(DT, 
         c("RDW.x", "RDW.y"),
         c("RDW_persoon", "RDW_huishouden"))
DT[, Motorvoertuig := 1 ]
DT[RDW_persoon ==T | RDW_huishouden ==T, Motorvoertuig := 2 ]
rm(RDW)


#Nabijheid huisarts !!!zware server!!!!
source("H:/Data proces/src/projecten/Kwetsbaarheid/Kwetbaarheidsindicatoren/AfstandHuisarts.R")
DT <- merge(DT, HA_zorg, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(HA_zorg)
DT[HA3km == T, huisartsen := 2]
DT[HA3km == F, huisartsen := 1]

DT[, VZAANTHARTSPR03KM := NULL]
DT[, HA3km := NULL]

#Leefbaarheid buurt !!!zware server!!!!
  #leefbarometer is in 2020 vernieuwd naar 3.0 versie, waarbij zij eerdere jaren hebben doorgerekend naar de nieuwe methodiek. Deze uitkomsten kunnen een kleine afwijking hebben met eerdere uitkomsten

source("src/indicatoren/leefbaarheid.R")
source("src/indicatoren/leefbaarheid3.0.R")
DT <- merge(DT, leefbaarheid, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(leefbaarheid)
DT[lage_leefbaarheid == 1, leefbaarheidsscore := 1]
DT[lage_leefbaarheid == 0, leefbaarheidsscore := 2]

#------------------------------------------------------------------------------
## Uitkomstmaten (Perined)
#------------------------------------------------------------------------------


#Vroeggeboorte
DT[amww >= 37, vroeggeboorte := 2]
DT[amww > 23 & amww <37, vroeggeboorte := 1]


#Hoftiezer
DT[hoftiezer >= 10, Hoft10 := 2]
DT[hoftiezer < 10, Hoft10 := 1]

#Hoftiezer &/| vroeggeboorte
DT[, BIG2 := 2]
DT[vroeggeboorte == T | Hoft10 == T, BIG2 := 1]

#NICU opname
  #NICU opname == nicuopname
DT[nicuopname == 1, nicu := 1]
DT[nicuopname == 0, nicu := 2]

#primaire keizersnede
DT[, PrimKeizer := 2]
DT[type_partus == "200148001", PrimKeizer := 1]

#secundaire keizersnede
DT[, SecKeizer := 2]
DT[type_partus == "200149009", SecKeizer := 1]

#Wel/geen kraam (VEKTIS!!!) CODE ROY 

#fluxus
  #fluxus == fluxus
DT[fluxus == 1, fluxus_cat := 1]
DT[fluxus == 0, fluxus_cat := 2]

#------------------------------------------------------------------------------
## Analysebestand
#------------------------------------------------------------------------------
  #Indicatoren selecteren
analysebestand <- DT[, c("RINPERSOONS","RINPERSOON", "Rinpersoons_KIND","RINPERSOON_KIND","jaar", "leeftijd_cat", "etniciteit_cat", "Par_3cat", "asielmigrant", "opleidingsniveau_cat",  "inkomen_cat", 
                         "SECM_cat", "financieleproblemen", "contractsoort", "dienstverband", "eenoudergezin", "gehuwdpaar", "scheiding", "aantalprshh_cat", "jgdondersteuning", "zvwkosten_totaal_cat", "huisartskosten_cat", "ziekenhuiskosten_cat",
                         "Hoogmedicijngebruik", "verslavingszorg", "LVBregistratie" , "GGZ", "Verdachte_misdrijf", "Slachtoffer_misdrijf", "gedetineerd", "woningBezit", "Aantalverhuis", "overlijdennaaste5jaar", "Motorvoertuig", "huisartsen", "leefbaarheidsscore",
                         "vroeggeboorte", "Hoft10", "nicu", "PrimKeizer", "SecKeizer", "fluxus_cat", "par")]

analysebestand <-  data.table(analysebestand)

analysebestand[, SECM := SECM_cat]
analysebestand[, SECM_cat := NULL]

#Alle indicatoren als factor 
analysebestand[, leeftijd_cat := as.factor(leeftijd_cat)]
analysebestand[, etniciteit_cat := as.factor(etniciteit_cat)]
analysebestand[, Par_3cat := as.factor(Par_3cat)]
analysebestand[, asielmigrant := as.factor(asielmigrant)]
analysebestand[, opleidingsniveau_cat := as.factor(opleidingsniveau_cat)]
analysebestand[, SECM := as.factor(SECM)]
analysebestand[, financieleproblemen := as.factor(financieleproblemen)]
analysebestand[, contractsoort := as.factor(contractsoort)]
analysebestand[, dienstverband := as.factor(dienstverband)]
analysebestand[, eenoudergezin := as.factor(eenoudergezin)]
analysebestand[, woningBezit := as.factor(woningBezit)]

analysebestand[, gehuwdpaar := as.factor(gehuwdpaar)]
analysebestand[, scheiding := as.factor(scheiding)]
analysebestand[, aantalprshh_cat := as.factor(aantalprshh_cat)]
analysebestand[, jgdondersteuning := as.factor(jgdondersteuning)]
analysebestand[, zvwkosten_totaal_cat := as.factor(zvwkosten_totaal_cat)]
analysebestand[, huisartskosten_cat := as.factor(huisartskosten_cat)]
analysebestand[, ziekenhuiskosten_cat := as.factor(ziekenhuiskosten_cat)]
analysebestand[, Hoogmedicijngebruik := as.factor(Hoogmedicijngebruik)]
analysebestand[, verslavingszorg := as.factor(verslavingszorg)]
analysebestand[, LVBregistratie := as.factor(LVBregistratie)]
analysebestand[, inkomen_cat := as.factor(inkomen_cat)]

analysebestand[, GGZ := as.factor(GGZ)]
analysebestand[, Verdachte_misdrijf := as.factor(Verdachte_misdrijf)]
analysebestand[, Slachtoffer_misdrijf := as.factor(Slachtoffer_misdrijf)]
analysebestand[, gedetineerd := as.factor(gedetineerd)]
analysebestand[, Aantalverhuis := as.factor(Aantalverhuis)]
analysebestand[, overlijdennaaste5jaar := as.factor(overlijdennaaste5jaar)]
analysebestand[, Motorvoertuig := as.factor(Motorvoertuig)]
analysebestand[, Hoogmedicijngebruik := as.factor(Hoogmedicijngebruik)]
analysebestand[, huisartsen := as.factor(huisartsen)]
analysebestand[, leefbaarheidsscore := as.factor(leefbaarheidsscore)]

analysebestand[, vroeggeboorte := as.factor(vroeggeboorte)]
analysebestand[, Hoft10 := as.factor(Hoft10)]
analysebestand[, nicu := as.factor(nicu)]
analysebestand[, PrimKeizer := as.factor(PrimKeizer)]
analysebestand[, SecKeizer := as.factor(SecKeizer)]
analysebestand[, fluxus_cat := as.factor(fluxus_cat)]
analysebestand[, par := as.factor(par)]


#enkel personen met een RIN & uniek
analysebestand <-  analysebestand[!is.na(RINPERSOON), ]

analysebestand <-  unique(analysebestand)

analysebestand <- analysebestand[jaar %in% 2016:2022]



