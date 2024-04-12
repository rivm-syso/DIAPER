#ZVW zorgkosten

#Workdirectory
setwd("H:/Data proces/")

#Utilities
source("src/utils.R")



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
#Een jaar voor basisjaar
zorgkosten_min1 <- zorgkosten[jaar <=(max(jaar)-1)]
zorgkosten_min1 <- zorgkosten_min1[, jaar := jaar + 1]
zorgkosten_min1[, zwanger := F]
zorgkosten_min1[ZVWKGEBOORTEZORG  > 0, zwanger := T]
zorgkosten_min1 <- zorgkosten_min1[,c("RINPERSOONS","RINPERSOON", "jaar", "zwanger")]
zorgkosten <- merge(zorgkosten, zorgkosten_min1, all=T, by= c("RINPERSOONS","RINPERSOON", "jaar"))

gc()





#------------------------------------------------------------------------------
# GGZ kosten
#------------------------------------------------------------------------------
#data opsplitsen voor berekening
ggz_kosten <- zorgkosten[jaar <=(max(jaar)-1)]
ggz_kosten <- ggz_kosten[, jaar := jaar + 1]

ggz_kosten <- zorgkosten[jaar >= min(jaren), 
                         c("jaar", "RINPERSOONS", "RINPERSOON", "ZVWKGENBASGGZ", "ZVWKSPECGGZ", "zwanger")]
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
gc()

#------------------------------------------------------------------------------
#Zorgkosten terugzetten 1 jaar terugkijken & 2 jaar als 1 jaar terug zwangerschapskosten zijn
#------------------------------------------------------------------------------
#kollmmen maken
#twee jaar voor basisjaar
zorgkosten_min2 <- zorgkosten[jaar <=(max(jaar)-2)]
zorgkosten_min2 <- zorgkosten_min2[, jaar := jaar + 2]
setnames(zorgkosten_min2, c("zvwkosten_totaal", "ZVWKHUISARTS", "ZVWKZIEKENHUIS"), 
         c("zvwkosten_totaal_min2", "ZVWKHUISARTS_min2", "ZVWKZIEKENHUIS_min2"))

#Een jaar voor basisjaar
zorgkosten_min1 <- zorgkosten[jaar <=(max(jaar)-1)]
zorgkosten_min1 <- zorgkosten_min1[, jaar := jaar + 1]
setnames(zorgkosten_min1, c("zvwkosten_totaal", "ZVWKHUISARTS", "ZVWKZIEKENHUIS"), 
         c("zvwkosten_totaal_min1", "ZVWKHUISARTS_min1", "ZVWKZIEKENHUIS_min1"))

#Kolommen koppelen
zorgkosten <- merge(zorgkosten, zorgkosten_min1[,c("RINPERSOONS","RINPERSOON", "jaar", "zvwkosten_totaal_min1", "ZVWKHUISARTS_min1", "ZVWKZIEKENHUIS_min1")], all=T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
zorgkosten <- merge(zorgkosten, zorgkosten_min2[,c("RINPERSOONS","RINPERSOON", "jaar", "zvwkosten_totaal_min2", "ZVWKHUISARTS_min2", "ZVWKZIEKENHUIS_min2")], all=T, by= c("RINPERSOONS","RINPERSOON", "jaar"))
rm(zorgkosten_min1, zorgkosten_min2)

gc()

#wanneer zwanger(schapskosten) in het vorige dan pakken we terug op 2 jaar ipv 1 jaar geleden
#Totaal zvw
zorgkosten[, zorgkosten_totaal := zvwkosten_totaal_min1]
zorgkosten[zwanger == T , zorgkosten_totaal := zvwkosten_totaal_min2 ]

#Totaal ziekenhuis
zorgkosten[, zorgkosten_ziekenhuis := ZVWKZIEKENHUIS_min1]
zorgkosten[zwanger == T , zorgkosten_ziekenhuis := ZVWKZIEKENHUIS_min2]

#Totaal huisarts
zorgkosten[, zorgkosten_huisarts := ZVWKHUISARTS_min1]
zorgkosten[zwanger == T , zorgkosten_huisarts := ZVWKZIEKENHUIS_min2]

gc()



#GGZ en zorgkosten weer samenvoegen
zorgkosten <- merge(zorgkosten, ggz_kosten, all.x = TRUE, by = c("RINPERSOONS", "RINPERSOON", "jaar"))

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
zorgkosten[, jaar := as.numeric(as.character(jaar))]
zorgkosten <- zorgkosten[jaar == years,]

zorgkosten <- zorgkosten[, c("RINPERSOONS", "RINPERSOON", "jaar", "kosten_basis_GGZ", "kosten_spec_GGZ", "kosten_GGZ", 
                         "zorgkosten_totaal", "zorgkosten_huisarts", "zorgkosten_ziekenhuis")]

zorgkosten <- unique(zorgkosten)



