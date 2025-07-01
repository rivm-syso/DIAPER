#ZVW zorgkosten laatste jaar additioneel (mits jaar nog niet beschikbaar is)

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")


jaren = 2023
years = jaren
#------------------------------------------------------------------------------
#ZVW zorgkosten data
#------------------------------------------------------------------------------
zorgkosten <- inlezen_data("G:/GezondheidWelzijn/ZVWZORGKOSTENTAB", 
                           specific = "zvw")

#zorgkosten21 <- inlezen_data("G:/GezondheidWelzijn/ZVWZORGKOSTENTAB2021", 
#                             specific = "zvw")


##21 met de rest samenvoegen samenvoegen
#zorgkosten <- rbind(zorgkosten, zorgkosten21)

#kolommen ZVWKTOTAAL eruit halen om dubbel tellen te voorkomen
zorgkosten[, c("ZVWKOPHOOGFACTOR", "ZVWKTOTAAL") := NULL]

#totalen bij elkaar optellen
zorgkosten[, "zvwkosten_totaal" := rowSums(.SD, na.rm = TRUE), .SDcols = !c(1:3)]

#zwanger in jaar door te kijken naar zorgkosten geboortezorg
#Een jaar voor basisjaar
# zorgkosten_min1 <- zorgkosten[jaar <=(max(jaar)-1)]
# zorgkosten_min1 <- zorgkosten_min1[, jaar := jaar + 1]
zorgkosten[, zwanger := F]
zorgkosten[ZVWKGEBOORTEZORG  > 0, zwanger := T]
zorgkostenZW <- zorgkosten %>% mutate(jaar = jaar +1)
#zorgkosten_min1 <- zorgkosten_min1[,c("RINPERSOONS","RINPERSOON", "jaar", "zwanger")]
#zorgkosten <- merge(zorgkosten, zorgkosten_min1, all=T, by= c("RINPERSOONS","RINPERSOON", "jaar"))

zorgkost <- DT[jaar == max(jaren), c("RINPERSOONS","RINPERSOON", "jaar")]
zorgkost <- merge(zorgkost, zorgkostenZW[,c("RINPERSOONS","RINPERSOON", "jaar", "zwanger")], all.x=T, by= c("RINPERSOONS","RINPERSOON", "jaar"))

gc()





#------------------------------------------------------------------------------
# GGZ kosten
#------------------------------------------------------------------------------
#data opsplitsen voor berekening
#ggz_kosten <- zorgkosten[jaar <=(max(jaar)-1),]
#ggz_kosten <- zorgkosten[, jaar := jaar + 1]


ggz_kosten <- zorgkosten[, c("jaar", "RINPERSOONS", "RINPERSOON", "ZVWKGENBASGGZ", "ZVWKSPECGGZ" )]
setnames(
  ggz_kosten,
  c("ZVWKGENBASGGZ", "ZVWKSPECGGZ"),
  c("kosten_basis_GGZ", "kosten_spec_GGZ")
)

ggz_kosten22 <- zorgkosten[jaar == 2022, c("jaar", "RINPERSOONS", "RINPERSOON", "ZVWKGGZZPMTOTAAL")]
ggz_kosten22[, kosten_GGZ22 := ifelse(ZVWKGGZZPMTOTAAL > 0, 1, 0)]
  
# #overige kolommen verwijderen
# ggz_kosten <- ggz_kosten[, c("jaar", "RINPERSOONS", "RINPERSOON", "kosten_basis_GGZ",
#                              "kosten_spec_GGZ", "kosten_GGZ")]
ggz_kosten22[, jaar := jaar +1 ]
gc()

#------------------------------------------------------------------------------
#Zorgkosten terugzetten 1 jaar terugkijken & 2 jaar als 1 jaar terug zwangerschapskosten zijn
#------------------------------------------------------------------------------
#kollmmen maken
#twee jaar voor basisjaar
zorgkosten_min2 <- zorgkosten[jaar <=(max(jaar)-1)]
zorgkosten_min2 <- zorgkosten_min2[, jaar := jaar + 2]
setnames(zorgkosten_min2, c("zvwkosten_totaal", "ZVWKHUISARTS", "ZVWKZIEKENHUIS"), 
         c("zvwkosten_totaal_min2", "ZVWKHUISARTS_min2", "ZVWKZIEKENHUIS_min2"))

#Een jaar voor basisjaar
zorgkosten_min1 <- zorgkosten[jaar <=(max(jaar))]
zorgkosten_min1 <- zorgkosten_min1[, jaar := jaar + 1]
setnames(zorgkosten_min1, c("zvwkosten_totaal", "ZVWKHUISARTS", "ZVWKZIEKENHUIS"), 
         c("zvwkosten_totaal_min1", "ZVWKHUISARTS_min1", "ZVWKZIEKENHUIS_min1"))

#Kolommen koppelen
zorgkost <- merge(zorgkost, zorgkosten_min1[,c("RINPERSOONS","RINPERSOON", "jaar", "zvwkosten_totaal_min1", "ZVWKHUISARTS_min1", "ZVWKZIEKENHUIS_min1")], all=T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
zorgkost <- merge(zorgkost, zorgkosten_min2[,c("RINPERSOONS","RINPERSOON", "jaar", "zvwkosten_totaal_min2", "ZVWKHUISARTS_min2", "ZVWKZIEKENHUIS_min2")], all=T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(zorgkosten_min1, zorgkosten_min2)

gc()
zorgkost <- data.table(zorgkost)
#wanneer zwanger(schapskosten) in het vorige dan pakken we terug op 2 jaar ipv 1 jaar geleden
#Totaal zvw
zorgkost[, zorgkosten_totaal := zvwkosten_totaal_min1]
zorgkost[zwanger == T , zorgkosten_totaal := zvwkosten_totaal_min2 ]

#Totaal ziekenhuis
zorgkost[, zorgkosten_ziekenhuis := ZVWKZIEKENHUIS_min1]
zorgkost[zwanger == T , zorgkosten_ziekenhuis := ZVWKZIEKENHUIS_min2]

#Totaal huisarts
zorgkost[, zorgkosten_huisarts := ZVWKHUISARTS_min1]
zorgkost[zwanger == T , zorgkosten_huisarts := ZVWKHUISARTS_min2]
zorgkost <- zorgkost[jaar == max(jaren),]

gc()


#GGZ en zorgkosten weer samenvoegen
zorgkost <- merge(zorgkost, ggz_kosten, all.x = TRUE, by = c("RINPERSOONS", "RINPERSOON", "jaar"))

#------------------------------------------------------------------------------
#kwintielen toevoegen
#------------------------------------------------------------------------------

# zorgkosten[, zk_2j_percentiel := ntile(zorgkosten_totaal, 100)]
# zorgkosten[, zk_2j_kwintiel := ntile(zorgkosten_totaal, 5)]
# zorgkosten[, HA_2j_kwintiel := ntile(zorgkosten_huisarts, 5)]
# zorgkosten[, ZH_2j_deciel := ntile(zorgkosten_ziekenhuis, 10)]

#------------------------------------------------------------------------------
#opruimen
#------------------------------------------------------------------------------
zorgkost[, jaar := as.numeric(as.character(jaar))]
zorgkost <- zorgkost[jaar == years,]

zorgkost <- zorgkost[, c("RINPERSOONS", "RINPERSOON", "jaar", "kosten_basis_GGZ", "kosten_spec_GGZ", "kosten_GGZ", 
                             "zorgkosten_totaal", "zorgkosten_huisarts", "zorgkosten_ziekenhuis")]

zorgkost <- unique(zorgkost)
setnames(zorgkost, c("zorgkosten_totaal", "zorgkosten_huisarts", "zorgkosten_ziekenhuis"),
         c("zorgkosten_totaal_n", "zorgkosten_huisarts_n", "zorgkosten_ziekenhuis_n"))


#write.fst(zorgkost, "H:/PP/zk_KS2423.fst")


