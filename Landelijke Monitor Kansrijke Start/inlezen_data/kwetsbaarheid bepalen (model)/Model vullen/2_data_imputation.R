# Imputeren data (MICE)

# We gaan er van uit dat we voldoende registratiegegevens hebben op basis waarvan we kunnen imputeren 
# Dit doen we obv alle CBS-variabelen die we meenemen in de analyse

# Imputeren data met MICE
library(mice)
library(skimr)

# Eerst databestand aanmaken 
analysebestand_kwetsbaar_di <- analysebestand_kwetsbaar
# Todo: schrijven functie: n_missing opgeteld delen door totaal aan personen (RINPERSOON) x variabelen. is.na functie gebruiken en daar som over nemen.

# Missende waarden 
skim(analysebestand_kwetsbaar_di)

# Seed = random nummer zodat imputatie herhaald kan worden met eenzelfde effect
# Predictormatix = helpt bij verkorten imputatietijd. Kan op basis van een minimale
# correlatie tussen variabelen, of je kunt zelf aangeven welke variabelen helpen bij
# het imputeren van andere variabelen.
# Method = specificeren van imputatie methode. Wanneer niet gespecificeerd, kiest mice zelf.
# m  = aantal imputatiesets

# met predictormatrix 
pred <- quickpred(analysebestand_kwetsbaar_di, mincor = 0.05)

## Vijf datasets imputeren, met seed en zonder predictormatrix
imputed <- mice(analysebestand_kwetsbaar_di, seed = 2016, m = 5, printFlag = TRUE, predictorMatrix = pred)

imputed$imp$opleidingsniveau # random check bij variabelen welke waarden zijn geimputeerd
imputed$imp$SECM

# Nieuwe databestanden in environment van imputatie-sets
kwetsbaar_imputed1 <- complete(imputed,1)
kwetsbaar_imputed2 <- complete(imputed,2)
kwetsbaar_imputed3 <- complete(imputed,3)
kwetsbaar_imputed4 <- complete(imputed,4)
kwetsbaar_imputed5 <- complete(imputed,5)

# Opslaan datasets
write.fst(kwetsbaar_imputed1, "H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Data/kwetsbaar_imputed1.fst")
write.fst(kwetsbaar_imputed2, "H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Data/kwetsbaar_imputed2.fst")
write.fst(kwetsbaar_imputed3, "H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Data/kwetsbaar_imputed3.fst")
write.fst(kwetsbaar_imputed4, "H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Data/kwetsbaar_imputed4.fst")
write.fst(kwetsbaar_imputed5, "H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Data/kwetsbaar_imputed5.fst")


