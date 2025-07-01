# Script om de data te bekijken en voorbereiden op trainen van modellen

# benodigde libraries
library(tidyverse)
library(corrr)
library(rcompanion)


# inladen databestand
load("H:/LCA kwetsbaarheid/LCA Merge bestanden/Zwangere vrouwen incl partner/Script LCA zwangere vrouwen volledig/LCA_dataset_final_kort.RData")
dataset_cbs <- LCA_dataset_final_kort

# TODO deze uitzetten
# nepdataset met variabelen om script te runnen
# dataset_cbs <- readRDS("data/mock_dataset_cbs")


# Controle databestand
dataset_cbs %>% summary()
dataset_cbs %>% names()
dataset_cbs %>% glimpse()
dataset_cbs %>% str()
dataset_cbs %>% dim()

# alles omzetten in factor voor pred_class_5 en naamklasse
dataset_cbs <- dataset_cbs %>% mutate(
  pred_class_5 = as.factor(pred_class_5),
  naamklasse = as.factor(naamklasse)
)
dataset_cbs %>% glimpse() # check -> gelukt

# Aanpassingen dataset
# Nieuwe variabele aanmaken: multidimensionaal kwetsbaar ja/nee:
dataset_cbs <- dataset_cbs %>%
  mutate(
    multidimensionaal = if_else(
      condition = naamklasse == "kwetsbaar", true = "ja", false = "nee"
    ) %>% as.factor() # omzetten naar factor
  ) %>%
  select(-naamklasse, -pred_class_5) # verwijderen onnodige variabelen voor latere stappen

dataset_cbs %>% glimpse() # check -> gelukt

# Kiezen van 1 van onderstaande opties:

# 1. alle variabelen (CBS + gemon)
# verwijderen rinpersoon en rinpersoon voor analyse
# dataset_cbs <- dataset_cbs %>% select(-rinpersoon, -rinpersoons)
# 2. alleen CBS-variabelen
# verwijderen gezondheidsmonitordata uit dataset
gm_variables <- c(
  "erv_gez_2cat",
  "beperkt_gez",
  "langdurig_ziekte",
  "depr_matighoog",
  "regie_3cat",
  "BMI_cat",
  "eenz_cat",
  "moeite_ink",
  "beweegrichtlijn",
  "overmatig_dr_2006",
  "roker"
)

gm_variables %>% saveRDS("data/vector_gm_variable_names")

dataset_cbs <- dataset_cbs %>%
  select(
    -rinpersoon,
    -rinpersoons
  )

# cbs + GM variables
dataset_cbs %>% saveRDS("data/dataset_cbs_gm")


# verwijder gm variables
dataset_cbs <- dataset_cbs %>%
  select(
    -all_of(gm_variables)
  )

# obtain cbs variables from column names with gm variables and target variable removed
cbs_variables <- names(dataset_cbs)[names(dataset_cbs) != "multidimensionaal"]
cbs_variables %>% saveRDS("data/vector_cbs_variable_names")


# opslaan bewerkte dataset
dataset_cbs %>% saveRDS("data/dataset_cbs")

# 3. Test met verwijderen variabelen met weinig cases (<40 of <50)
# dataset_cbs <- dataset_cbs %>% select(-rinpersoon, -rinpersoons, -erv_gez_2cat, -beperkt_gez, -langdurig_ziekte, -depr_matighoog, -regie_3cat, -BMI_cat, -eenz_cat, -moeite_ink, -beweegrichtlijn, -overmatig_dr_2006, -roker, -asielmigrant2014_2018, -verslavingszorginclnevendiagnose20112016, -gedetineerd20052016)
# 4. Test met slechts 2 variabelen, om te controleren of de AUC laag uitvalt
# dataset_cbs <- dataset_cbs %>% select(aantalprshh_okt2016_cat, gehuwdpaar_okt2016, multidimensionaal)
# 5. Test met alleen gezondheidsmonitor-variabelen
# dataset_cbs <- dataset_cbs %>% select(erv_gez_2cat, beperkt_gez, langdurig_ziekte, depr_matighoog, regie_3cat, BMI_cat, eenz_cat, moeite_ink, beweegrichtlijn, overmatig_dr_2006, roker, multidimensionaal)

# check aantal en percentage kwetsbaar
dataset_cbs %>%
  count(multidimensionaal) %>%
  mutate(prop = n / sum(n))

# Controle assumpties
# Aantal observaties per variabele -> zie tabel 2. in vorig artikel
# Multicollineariteit
# 1. selecteer data en zorg dat alles factor is
dataset_cbs_col <- dataset_cbs %>%
  mutate(across(where(is.character), .fns = as.factor))
# 2. Bereken Cramer's V (kan met extra argumenten)
colpair_map(dataset_cbs_col, cramerV, digits = 2, bias.correct = TRUE) # digits = 2 betekent 2 cijfers achter komma

# Multicollineariteit manier 2
# cv.test = function(x, y) {
#  CV = sqrt(chisq.test(x, y, correct = TRUE)$statistic /
#              (length(x) * (min(length(unique(x)), length(unique(y))) - 1)))
#  return(as.numeric(CV))
# }
# df_test <- colpair_map(dataset_cbs_col, cv.test)
# df_test %>% select(multidimensionaal)
# View(df_test)
