#ZVW zorgkosten

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")

#Jaren
#jaren <- c(2017:2019)

#### !!!! nog toevoegen dat wanneer zwangerschap in twee jaar ervoor was dat 2 jaar extra teruggekeken moet worden !!!!!!!

#------------------------------------------------------------------------------
#ZVW zorgkosten data
#------------------------------------------------------------------------------
zorgkosten <- inlezen_data("G:/GezondheidWelzijn/ZVWZORGKOSTENTAB", 
                           specific = "zvw")

#kolommen ZVWKTOTAAL eruit halen om dubbel tellen te voorkomen
zorgkosten[, c("ZVWKOPHOOGFACTOR", "ZVWKTOTAAL") := NULL]

#totalen bij elkaar optellen
zorgkosten[, "zvwkosten_totaal" := rowSums(.SD, na.rm = TRUE), .SDcols = !c(1:3)]

#zwanger in jaar door te kijken naar zorgkosten geboortezorg
zorgkosten[, zwanger := F]
zorgkosten[ZVWKGEBOORTEZORG  > 0, zwanger := T]


#data opsplitsen voor berekening
zvw_kosten <- zorgkosten[, c("jaar", "RINPERSOONS", "RINPERSOON", "zvwkosten_totaal", "zwanger")]

ggz_kosten <- zorgkosten[jaar >= min(jaren), 
                         c("jaar", "RINPERSOONS", "RINPERSOON", "ZVWKGENBASGGZ", "ZVWKSPECGGZ", "zwanger")]

huisartskosten <-zorgkosten[,c("jaar", "RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS", "zwanger")]

ziekenhuiskosten <-zorgkosten[, c("jaar", "RINPERSOONS", "RINPERSOON", "ZVWKZIEKENHUIS", "zwanger")]

#rm(zorgkosten)

#GGZ kosten
setnames(
  ggz_kosten,
  c("ZVWKGENBASGGZ", "ZVWKSPECGGZ"),
  c("kosten_basis_GGZ", "kosten_spec_GGZ")
)

ggz_kosten[, ':=' (
  kosten_basis_GGZ = ifelse(kosten_basis_GGZ > 0, 1, 0),
  kosten_spec_GGZ = ifelse(kosten_spec_GGZ > 0, 1, 0),
  kosten_GGZ = ifelse(kosten_spec_GGZ > 0 | kosten_basis_GGZ > 0, 1, 0)
)]

#overige kolommen verwijderen
ggz_kosten <- ggz_kosten[, c("jaar", "RINPERSOONS", "RINPERSOON", "kosten_basis_GGZ",
                             "kosten_spec_GGZ", "kosten_GGZ")]

#zorgkosten van 1 jaar geleden naar huidige jaar zetten
# zvw_kosten[, jaar := jaar + 1]
# 
# # zvw_kosten <- zvw_kosten[jaar %in% jaren] #alleen jaren houden
# 
# 
# # huisartskosten <- huisartskosten[jaar %in% jaren] #alleen jaren houden
# 
# ziekenhuiskosten[, jaar := jaar + 1]
# ziekenhuiskosten <- ziekenhuiskosten[jaar %in% jaren] #alleen jaren houden


#wanneer vrouwen zwanger jaeren in jaar x dan kijken naar kosten in jaar x-1





############ Totale kosten ########
totaal <- data.table()

for(year in jaren){
  
  totalekost <- zvw_kosten %>% filter(jaar == year-1 | jaar == year -2)
  #1 jaar terug kijken naar zorgkosten
  
  totalekost[, jaar := jaar + 1]
  totalekost <- data.table(totalekost)
  #wanneer zwangerschaps kosten gemaakt worden in jaar dan ordening op 99 anders 1
  totalekost[zwanger==F & jaar == year, ord := 1]
  totalekost[zwanger==T & jaar == year, ord := 99]
  
  #ordening toevoegen (laatste jaar 1, jaar eerder 2)
  totalekost[jaar == year-1, ord :=2]
  #   #jaren gelijk trekken
  totalekost[jaar == year-1, jaar := jaar + 1]
  
  
  # #rij verwijderen als keep ==F

  totalekost <-totalekost %>% group_by(RINPERSOONS, RINPERSOON) %>% slice_min(order_by = ord, n=1) %>% ungroup()
  
  
  totaal <- rbind(totaal, totalekost)
}


############ Ziekenhuis kosten ########
ziekenhuis <- data.table()

for(year in jaren){
  
  ziekHkost <- ziekenhuiskosten %>% filter(jaar == year-1 | jaar == year -2)
  #1 jaar terug kijken naar zorgkosten
  
  ziekHkost[, jaar := jaar + 1]
  ziekHkost <- data.table(ziekHkost)
  #wanneer zwangerschaps kosten gemaakt worden in jaar dan ordening op 99 anders 1
  ziekHkost[zwanger==F & jaar == year, ord := 1]
  ziekHkost[zwanger==T & jaar == year, ord := 99]
  
  #ordening toevoegen (laatste jaar 1, jaar eerder 2)
  ziekHkost[jaar == year-1, ord :=2]
  #   #jaren gelijk trekken
  ziekHkost[jaar == year-1, jaar := jaar + 1]
  
  
  # #rij verwijderen als keep ==F
  
  ziekHkost <-ziekHkost %>% group_by(RINPERSOONS, RINPERSOON) %>% slice_min(order_by = ord, n=1) %>% ungroup()
  
  
  ziekenhuis <- rbind(ziekenhuis, ziekHkost)
}


############ HUISARTS kosten ########
huisarts <- data.table()

for(year in jaren){
  
  huisartskost <- huisartskosten %>% filter(jaar == year-1 | jaar == year -2)
    #1 jaar terug kijken naar zorgkosten
  
    huisartskost[, jaar := jaar + 1]
    huisartskost <- data.table(huisartskost)
    #wanneer zwangerschaps kosten gemaakt worden in jaar dan ordening op 99 anders 1
    huisartskost[zwanger==F & jaar == year, ord := 1]
    huisartskost[zwanger==T & jaar == year, ord := 99]
    
    #ordening toevoegen (laatste jaar 1, jaar eerder 2)
    huisartskost[jaar == year-1, ord :=2]
  #   #jaren gelijk trekken
    huisartskost[jaar == year-1, jaar := jaar + 1]
      
    
    # #rij verwijderen als keep ==F
    # huisartskost <- huisartskost[ keep == T | is.na(keep), ]
    huisartskost <-huisartskost %>% group_by(RINPERSOONS, RINPERSOON) %>% slice_min(order_by = ord, n=1) %>% ungroup()


huisarts <- rbind(huisarts, huisartskost)
}

#enkel benodigde jaren
totaal <- totaal[jaar %in% jaren] #alleen jaren houden
ziekenhuis <- ziekenhuis[jaar %in% jaren] #alleen jaren houden
huisarts <- huisarts[jaar %in% jaren] #alleen jaren houden


#GGZ en zorgkosten weer samenvoegen
zorgkosten <- merge(totaal, ggz_kosten, all.x = TRUE, by = c("RINPERSOONS", "RINPERSOON", "jaar"))
zorgkosten <- merge(zorgkosten, huisarts[, c("jaar", "RINPERSOONS", "RINPERSOON", "ZVWKHUISARTS")], all.x = TRUE, by = c("RINPERSOONS", "RINPERSOON", "jaar"))
zorgkosten <- merge(zorgkosten, ziekenhuis[, c("jaar", "RINPERSOONS", "RINPERSOON", "ZVWKZIEKENHUIS")], all.x = TRUE, by = c("RINPERSOONS", "RINPERSOON", "jaar"))
#------------------------------------------------------------------------------
#kwintielen toevoegen
#------------------------------------------------------------------------------

# zorgkosten[, zk_2j_percentiel := ntile(zvwkosten_totaal, 100)]
# zorgkosten[, zk_2j_kwintiel := ntile(zvwkosten_totaal, 5)]
# zorgkosten[, HA_2j_kwintiel := ntile(ZVWKHUISARTS, 5)]
# zorgkosten[, ZH_2j_deciel := ntile(ZVWKZIEKENHUIS, 10)]

#------------------------------------------------------------------------------
#opruimen
#------------------------------------------------------------------------------
zorgkosten[, jaar := as.numeric(as.character(jaar))]
zorgkosten[, zwanger := NULL]
zorgkosten[, ord := NULL]
# zorgkosten[, ZVWKHUISARTS := NULL]
# zorgkosten[, ZVWKZIEKENHUIS  := NULL]

zorgkosten <- unique(zorgkosten)


rm(ggz_kosten, zvw_kosten, huisartskosten, ziekenhuiskosten)



