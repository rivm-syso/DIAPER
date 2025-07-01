#Indicatoren kwetsbaarheid 2024

#Voor het bepalen van of een persoon in een meervoudige kwetsbare situatie zit voor de zwangerschap
#Hiertoe is een model ontwikkeld door  Molenaar J. et al (2023) icm met Gezondheidsmonitor data (resultaten hiervan in Kansrrijke Start monitors '21, '22 en '23)
#Dit bestand is de basis voor het vormen van een landelijk dekkend beeld van meervoudige kwetsbaarheid bij vrouwen voor de zwangerschap
#Hiertoe wordt landelijke geboorte registratie data (Perined) gebruikt en verrijkt met data demografisch en socioeconomische data vanuit de CBS-Microdata, 
#voor veel bestanden wordt er gekeken naar het jaar voorafgaand aan het de zwangerschap aangezien we inzicht willen krijgen op risico's voor de start van de zwangerschap
#Dit databestand wordt gebruikt als input voor het ontwikkelde model om een landelijk beeld te geven van het aandeel vrouwen die meervoudig kwetsbaar zijn
#Resultaten hiervan zijn eerder gepubliceerd in de Kansrijke Start monitor 2023 en is ingediend als wetenschappelijk artikel

#In deze versie worden de geboortecijfers uit het BRP (CBS) als basis genomen, voorgaande versies hadden Perined als basis bestand.

#Utilities
source("src/utils.R")

library(fst)

jaren <- c(2016:2023) #1 jaar ervoor meenemen ivm terugkijken
years <- jaren
#Gemeente indeling
gemeente_indeling <- "gem2025"

#------------------------------------------------------------------------------
##Geboortes inlezen
#------------------------------------------------------------------------------

#Levend en doodgeborenen

levendgeboren <- inlezen_data("G:/Bevolking/GBALEVENDGEBORENENMASSATAB", c("RINPERSOONS", "RINPERSOON"))
doodgeboren <- inlezen_data("G:/Bevolking/DGGBAPERSOONTAB", c("RINPERSOONS", "RINPERSOON"))
geboren <- rbind(levendgeboren, doodgeboren)

rm(levendgeboren, doodgeboren)

# Ouders koppelen ---------------------------------------------------------

#toevoegen RIN ouders 
#Bepalen locaties laatste dataset
kindouder_locatie <- list.files("G:/Bevolking/KINDOUDERTAB", pattern = ".sav", recursive = TRUE, full.names = TRUE)
kindouder_locatie <- kindouder_locatie[length(kindouder_locatie)]

#Inlezen datasets met alleen relevante kolommen
kindouder <- read_spss(kindouder_locatie)
kindouder <- as.data.table(kindouder)

#Koppelen
DT <- merge(geboren, kindouder[, c("RINPERSOONS", "RINPERSOON", "RINPERSOONSpa", "RINPERSOONpa", "RINPERSOONSMa", "RINPERSOONMa")],
             all.x = T, by= c("RINPERSOONS", "RINPERSOON"))

#namen omzetten
setnames(DT, 
         c("RINPERSOONS", "RINPERSOON", "RINPERSOONSMa", "RINPERSOONMa", "RINPERSOONSpa", "RINPERSOONpa"),
         c("Rinpersoons_KIND", "RINPERSOON_KIND", "RINPERSOONS", "RINPERSOON", "RINPERSOONS_VADER", "RINPERSOON_VADER"))
rm(kindouder, geboren)

#------------------------------------------------------------------------------
##CSB inlezen
#------------------------------------------------------------------------------

#huishoudnummer
source("./Kwetbaarheidsindicatoren/huishoudnummer.R")
DT <- merge(DT, huishoudkoppel_data,  all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar") )
rm(huishoudkoppel_data)

#------------------------------------------------------------------------------
##CBS Individuele karakteristieken 
#------------------------------------------------------------------------------

#Leeftijd op moment bevalling
source("./indicatoren/leeftijd.R")

DT <- merge(DT, leeftijd[, c("RINPERSOONSkind","RINPERSOONkind", "lft_bevalling_moeder")], all.x= T, by.x= c("Rinpersoons_KIND", "RINPERSOON_KIND"), by.y= c("RINPERSOONSkind","RINPERSOONkind"))
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
DT <- data.table(DT)

#Etniciteit cat
DT[, etniciteit_cat := etniciteit]



# #pariteit  !!!!! NIEUWE METHODE !!!!!!!
source("./Kwetbaarheidsindicatoren/CBS pariteit_2.0.R")
DT <- merge(DT, pariteit[,c("RINPERSOONS","RINPERSOON", "par")], by.x =c("Rinpersoons_KIND", "RINPERSOON_KIND"),  by.y= c("RINPERSOONS","RINPERSOON"), all.x =T)
DT[par == 0, Par_3cat := 1]
DT[par > 0, Par_3cat := 2]
rm(pariteit)


#Asielzoeker
source("./indicatoren/asielcohort.R")

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
source("./Kwetbaarheidsindicatoren/opleiding.R") #jaren aangepast
DT <- merge(DT, hoogstopl_data, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(hoogstopl_data)
  #Opleidingscat 
DT[opleidingsniveau == "laag", opleidingsniveau_cat := 1]
DT[opleidingsniveau == "midden", opleidingsniveau_cat := 2]
DT[opleidingsniveau == "hoog", opleidingsniveau_cat := 3]
jaren = years


#Huishoudinkomen 
source("./Kwetbaarheidsindicatoren/huishoud_inkomen.R") 
DT <- merge(DT, huishoud_inkomen[, c("RINPERSOONS","RINPERSOON", "jaar","ink_Kstart")], all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(huishoud_inkomen, huishoudkoppel_locatie, inhatab_locatie)
  #inkomen cat
DT[, inkomen_cat := ink_Kstart]

#Sociaal economische positie
  #inkomstenbron
source("./Kwetbaarheidsindicatoren/Inkomensbron.R")


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
source("./indicatoren/zvw_wanbetaler.R")
  #jaar terugkijken
zvw_wanbetaler[, jaar := jaar+1]
DT <- merge(DT, zvw_wanbetaler, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(zvw_wanbetaler)

#Schulden (schuldsanering) 
source("./indicatoren/schuldsanering.R")
  #jaar terugkijken
schuldsanering[, jaar := jaar+1]
DT <- merge(DT, schuldsanering, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(schuldsanering)


  #financi?le problemen 
DT[, financieleproblemen := 2]
DT[zvw_wanbetaler ==T | schuldsanering ==T, financieleproblemen := 1]




#Vast contract (werk)  ######## OP ZWARE SERVER DRAAIEN  ######## 
# &  Voltijds werkend 
source("./Kwetbaarheidsindicatoren/Dienstverband.R")
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
source("./Kwetbaarheidsindicatoren/huishoud_inkomen.R")
DT <- merge(DT, huishoud_inkomen[, c("RINPERSOONS","RINPERSOON", "jaar","omvang_huishouden" , "eenouder") ], all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(huishoud_inkomen)
  #eenouder gezin (dit halen we ook uit het Inhatab-bestand)
DT[, eenoudergezin := 2]
DT[eenouder == 1 | omvang_huishouden == 1, eenoudergezin := 1]
  #aantal pers. huishouden
DT[omvang_huishouden >= 6, aantalprshh_cat := 1]
DT[omvang_huishouden < 6, aantalprshh_cat := 2]


# Huwelijksstatus
source("./indicatoren/burgerlijke_staat.R")
#jaar terugkijken
burgerlijke_staat[, jaar := jaar+1]
DT <- merge(DT, burgerlijke_staat, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(burgerlijke_staat)
DT[, gehuwdpaar := 1]
DT[burgerlijk_staat == "Gehuwd", gehuwdpaar := 2]


# Scheiding afelopen 4 jaar 
source("./Kwetbaarheidsindicatoren/Scheidingen.R")
DT <- merge(DT, scheidingen, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(scheidingen)
  #scheid cat
DT[, scheiding := 2]
DT[scheiding4J== T, scheiding := 1]



# Jeugdhulp
source("./Kwetbaarheidsindicatoren/Jeugdhulp.R")
DT <- merge(DT, jeugdhulpMA, all.x = T, by.x= c("RINPERSOONS","RINPERSOON", "jaar"), by.y= c("RINPERSOONSMa","RINPERSOONMa", "jaar"))
DT <- merge(DT, jeugdhulpPA, all.x = T, by.x= c("RINPERSOONS_VADER","RINPERSOON_VADER", "jaar"), by.y= c("RINPERSOONSpa","RINPERSOONpa", "jaar"))
DT[jeugdhulp.x ==T, jeugdhulp := T]
DT[jeugdhulp.y ==T, jeugdhulp := T]
DT[is.na(jeugdhulp), jeugdhulp := F]
DT[, jeugdhulp.x := NULL]
DT[, jeugdhulp.y := NULL]
rm(jeugdhulpMA, jeugdhulpPA)

#Jeugdbescherming
source("./Kwetbaarheidsindicatoren/Jeugdbescherming.R")
DT <- merge(DT, jeugdbeschMA, all.x = T, by.x= c("RINPERSOONS","RINPERSOON", "jaar"), by.y= c("RINPERSOONSMa","RINPERSOONMa", "jaar"))
DT <- merge(DT, jeugdbeschPA, all.x = T, by.x= c("RINPERSOONS_VADER","RINPERSOON_VADER", "jaar"), by.y= c("RINPERSOONSpa","RINPERSOONpa", "jaar"))
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
source("./Kwetbaarheidsindicatoren/zorgkosten_kwetsbaarheid.R")
DT <- merge(DT, zorgkosten, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(zorgkosten)
gc()


#In het geval missen laatste jaar in zorgkosten
source("./Kwetbaarheidsindicatoren/zorgkosten_kwetsbaarheid_laatste_jaar_bij_missend jaar.R")
DT <- merge(DT, zorgkost[, c("RINPERSOONS","RINPERSOON", "jaar", "zorgkosten_totaal_n", "zorgkosten_huisarts_n", "zorgkosten_ziekenhuis_n")], all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
DT[ jaar == max(jaren), zorgkosten_totaal := zorgkosten_totaal_n]
DT[ jaar == max(jaren), zorgkosten_huisarts := zorgkosten_huisarts_n]
DT[ jaar == max(jaren), zorgkosten_ziekenhuis := zorgkosten_ziekenhuis_n]
DT <- merge(DT, ggz_kosten22, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
DT[ jaar == 2023, kosten_GGZ := kosten_GGZ22]


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
source("./indicatoren/medicijn_gebruik.R")
  #jaar terugkijken
med_gebruik[, jaar := jaar+1]
#hoog_gebruik[, jaar := jaar+1]
DT <- merge(DT, med_gebruik, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
#DT <- merge(DT, hoog_gebruik, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(med_gebruik, hoog_gebruik)
DT[, Hoogmedicijngebruik := 2]
DT[hoog_gebruik == T, Hoogmedicijngebruik := 1]

#Verslavingszorg 
source("./Kwetbaarheidsindicatoren/Verslavingszorg.R")
DT <- merge(DT, Verslavingszorg, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(Verslavingszorg)

DT[is.na(verslavingszorg), verslavingszorg := 2]

#------------------------------------------------------------------------------
## Psychosociale karakteristieken
#------------------------------------------------------------------------------
# -	Kosten GGZ | ziektekosten

# Lichtelijk verstandlijk beparkt (LVB)
#gebruik oude data
source("./Kwetbaarheidsindicatoren/LVB.R")
DT <-  merge(DT, LVB, all.x = T, by= c("RINPERSOONS","RINPERSOON"))
rm(LVB)
DT[, LVBregistratie := 2]
DT[LVB == T, LVBregistratie := 1]

#Aanvullend 22 toevoegen
# ggz_kosten[,kosten_GGZ_n := kosten_GGZ]
# DT <- merge(DT, ggz_kosten[, c("RINPERSOONS","RINPERSOON", "jaar", "kosten_GGZ_n")], all.x = T, c("RINPERSOONS","RINPERSOON", "jaar"))
# DT[jaar == 2022, kosten_GGZ := kosten_GGZ_n ]

#gebruik ggz (op basis van zorgkosten)
DT[kosten_GGZ == 1, GGZ := 1]
DT[kosten_GGZ == 0, GGZ := 2]


#inlezen data ggz 
ggzkosten <- fread("./ggzkostenKS.csv", colClasses = list(character = "RINPERSOON"))
ggzkosten <- data.table(ggzkosten)
ggzkosten[, jaar := jaar +1] #jaar opplussen, kijken bij KS jaar terug - dus ggzkosten in voorgaande jaar
ggzkosten[, kosten_GGZa := kosten_GGZ]

DT <- merge(DT, ggzkosten[, c("RINPERSOON", "RINPERSOONS", "jaar", "kosten_GGZa")], by = c("RINPERSOON", "RINPERSOONS", "jaar"), all.x = T)
DT[kosten_GGZa == 1, GGZ := 1]
DT[kosten_GGZa == 0, GGZ := 2]




#------------------------------------------------------------------------------
## Life events
#------------------------------------------------------------------------------
# Delict/misdrijf verachte 
source("./Kwetbaarheidsindicatoren/Verdachte.R")
DT <- merge(DT, Verdachten, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(Verdachten)
DT[, Verdachte_misdrijf := 2]
DT[Verdachte ==T, Verdachte_misdrijf := 1]

# Slachtoffer misdrijf  
source("./Kwetbaarheidsindicatoren/Slachtoffer.R")
DT <- merge(DT, slachtoffers, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(slachtoffers)
DT[, Slachtoffer_misdrijf := 2]
DT[slachtoffer ==T, Slachtoffer_misdrijf := 1]

# Gevangen gezeten 
source("./Kwetbaarheidsindicatoren/detentie_KS.R")
DT <- merge(DT, gevangen, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(gevangen)
DT[, gedetineerd := 2]
DT[detentie  == T, gedetineerd := 1]


# Frequent verhuizer
source("./Kwetbaarheidsindicatoren/Verhuizingen.R")
DT <- merge(DT, verhuizingen, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
DT <- data.table(DT)
rm(verhuizingen)
DT[, Aantalverhuis := 2]
DT[verhuizingen == 1, Aantalverhuis := 1]


# Verlies familielid (ouder/kind)  
source("./Kwetbaarheidsindicatoren/NaasteVerlies.R")
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
source("./Kwetbaarheidsindicatoren/WoningBezit.R")
DT <- merge(DT, huishoud_huisbezit, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(huishoud_huisbezit)
DT[woningBezit == 3, woningBezit :=1]

#Voertuig bezit 
source("./Kwetbaarheidsindicatoren/RDW.R")
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
source("./Kwetbaarheidsindicatoren/AfstandHuisarts.R")
DT <- merge(DT, HA_zorg, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(HA_zorg)
DT[HA3km == T, huisartsen := 2]
DT[HA3km == F, huisartsen := 1]

DT[, VZAANTHARTSPR03KM := NULL]
DT[, HA3km := NULL]

#Leefbaarheid buurt !!!zware server!!!!
  #leefbarometer is in 2020 vernieuwd naar 3.0 versie, waarbij zij eerdere jaren hebben doorgerekend naar de nieuwe methodiek. Deze uitkomsten kunnen een kleine afwijking hebben met eerdere uitkomsten

source("./indicatoren/leefbaarheid3.0.R")
DT <- merge(DT, leefbaarheid, all.x = T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(leefbaarheid)
DT[lage_leefbaarheid == 1, leefbaarheidsscore := 1]
DT[lage_leefbaarheid == 0, leefbaarheidsscore := 2]

#------------------------------------------------------------------------------
## Uitkomstmaten (Perined)
#------------------------------------------------------------------------------

#Niet meegenomen

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

analysebestand <- analysebestand[jaar %in% 2016:2023]

write.fst(analysebestand, "./2024 update/analysebestand24V6.fst")


