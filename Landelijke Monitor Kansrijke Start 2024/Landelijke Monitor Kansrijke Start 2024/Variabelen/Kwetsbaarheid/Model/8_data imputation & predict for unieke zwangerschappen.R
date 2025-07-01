# Script unieke zwangerschappen 


# In onze eerdere scripts hebben we kwetsbaarheid voorspeld voor iedere observatie in Perined
# Eigenlijk willen we unieke zwangerschappen (=rangnummer 1 in Perined) overhouden


# Databestand met unieke zwangerschappen inladen (see figures & tables voor opbouw)

#na archivering bestand verplaatst naar H:/Data proces/data/LCA kwetsbaarheid/......
load("H:/LCA kwetsbaarheid/LCA Merge bestanden/Zwangere vrouwen alleen landelijke registratie-data (vervolg LCA)/analysebestand_kwetsbaar_uniek_zwangerschap.RData")


###################
# complete cases voorspellen (percentages per jaar en methode) 
###################

library(tidyverse)
library(tidymodels)
library(glmnet)
library(fst)
library(data.table)


# complete cases bestand maken 

analysebestand_kwetsbaar_uniek_zwangerschap_cc <- analysebestand_kwetsbaar_uniek_zwangerschap %>% filter(complete.cases(.))


# gebruik maken van 7_predict_imputed en aanpassen naar nieuwe dataset 


# zelfde variabelen als bij trainen model (dus bijv. rinpersoon er uit etc)
# voor later gebruik
years <- analysebestand_kwetsbaar_uniek_zwangerschap_cc %>%
  select(jaar)

analysebestand_kwetsbaar_uniek_zwangerschap_cc_clean <- analysebestand_kwetsbaar_uniek_zwangerschap_cc
analysebestand_kwetsbaar_uniek_zwangerschap_cc <- analysebestand_kwetsbaar_uniek_zwangerschap_cc %>%
  select(
    -RINPERSOONS,
    -RINPERSOON,
    -Rinpersoons_KIND, 
    -RINPERSOON_KIND,
    -jaar # ook jaartal er uit: straks verschillende jaren laten zien
  )

############## random forest met tidymodels
# lees het model in
final_rf <- readRDS("data/final_rf")

# uitrekenen prevalentie
pred_list_rf <- calculate_prevalence_tidy(
  model = final_rf$rf_final_fit,
  probability_threshold = final_rf$probability_opt,
  dataset = analysebestand_kwetsbaar_uniek_zwangerschap_cc,
  model_name = "random_forest",
  dataset_name = "complete_case"
)
df_with_year <- tibble(
  jaar = years,
  voorspelling_kwetsbaar = pred_list_rf$prediction_labels
)
df_with_year
df_with_year %>%
  saveRDS("data/completecase_predictions_with_year_random_forest_kwetsbaar_ja_uniek_zwangerschap.RDS")

pred_list_rf$prevalence

# aantallen opslaan 
df_with_year_RF <- as.data.frame(df_with_year)
table(df_with_year_RF$jaar)
table(df_with_year_RF$voorspelling_kwetsbaar)
table(df_with_year_RF$voorspelling_kwetsbaar, df_with_year_RF$jaar) # foutmelding
df_with_year_RF %>% 
  group_by(jaar) %>% 
  table(df_with_year_RF$voorspelling_kwetsbaar) # deze duurt best lang

kwetsbaar_per_jaar_cc <- imputed_prevalence_per_year %>%
  summary_prevalence(group_by_vars = jaar)


############## LASSO met glmnet
# lees het geprepareerde recept in
lr_prep <- readRDS("data/lr_prep")

# lees het model in
final_glmnet <- readRDS("data/final_glmnet")

# uitrekenen prevalentie # soms foutmelding: let dan op dat je eerder de variabele 'jaar' niet verwijderd uit de dataset
pred_list_glmnet <- calculate_prevalence_lasso(
  model = final_glmnet$glmnet_obj,
  lambda_opt = final_glmnet$lambda_opt,
  lr_prep = lr_prep,
  probability_threshold = final_glmnet$probability_opt,
  dataset = analysebestand_kwetsbaar_uniek_zwangerschap_cc,
  model_name = "lasso",
  dataset_name = "complete_case"
)

df_with_year <- tibble(
  jaar = years,
  voorspelling_kwetsbaar = pred_list_glmnet$prediction_labels
)
df_with_year %>%
  saveRDS("data/completecase_predictions_with_year_lasso_kwetsbaar_ja_uniek_zwangerschap.RDS")

pred_list_glmnet$prevalence


############# xgboost met tidymodels
# lees het model in
final_xgb <- readRDS("data/final_xg")
# lees het recept in
xgb_prep <- readRDS("data/xgb_prep_data")

# pas recept toe op jouw dataset
dataset_new <- xgb_prep %>%  # dataset_new vervangen door analysebestand_kwetsbaar_uniek_zwangerschap_cc?
  bake(analysebestand_kwetsbaar_uniek_zwangerschap_cc) 

# uitrekenen prevalentie
pred_list_xgb <- calculate_prevalence_tidy(
  model = final_xgb$xg_final_fit,
  probability_threshold = final_xgb$probability_opt,
  dataset = dataset_new,  # dataset_new vervangen door analysebestand_kwetsbaar_uniek_zwangerschap_cc?
  model_name = "xgboost",
  dataset_name = "complete_case"
)
df_with_year <- tibble(
  jaar = years,
  voorspelling_kwetsbaar = pred_list_xgb$prediction_labels
)
df_with_year %>%
  saveRDS("data/completecase_predictions_with_year_xgb_kwetsbaar_ja_uniek_zwangerschap.RDS")

pred_list_xgb$prevalence


##############################################################
## Opslaan voorspellingen (zie figures and tables) 
# merk op: geen sd want maar een dataset
library(tidymodels)
library(vip)
library(readxl)
library(xlsx)
library(data.table)

####### random forest
df_rf <-
  readRDS("data/completecase_predictions_with_year_random_forest_kwetsbaar_ja_uniek_zwangerschap.RDS")
# over alle jaren
calculate_prevalence_over_datasets(list(df_rf), 
                                   group_by_vars = NULL) %>%
  summary_prevalence(group_by_vars = NULL)
# per jaar
calculate_prevalence_over_datasets(list(df_rf), 
                                   group_by_vars = jaar) %>%
  summary_prevalence(group_by_vars = jaar)

# Opslaan 
prevalence <- calculate_prevalence_over_datasets(list(df_rf), 
                                                 group_by_vars = jaar) %>%
  summary_prevalence(group_by_vars = jaar)
prevalence <- as.data.frame(prevalence)
prevalence <- data.table(prevalence)
write.xlsx(prevalence, "data/prevalence_cc_uniek_zwangerschap.xlsx", append = F, sheetName = "prevalence_cc_RF" )


####### lasso
df_lasso <-
  readRDS("data/completecase_predictions_with_year_lasso_kwetsbaar_ja_uniek_zwangerschap.RDS")
# over alle jaren
calculate_prevalence_over_datasets(list(df_lasso), 
                                   group_by_vars = NULL) %>%
  summary_prevalence(group_by_vars = NULL)
# per jaar
calculate_prevalence_over_datasets(list(df_lasso), 
                                   group_by_vars = jaar) %>%
  summary_prevalence(group_by_vars = jaar)  

# Opslaan 
prevalence <- calculate_prevalence_over_datasets(list(df_lasso), 
                                                 group_by_vars = jaar) %>%
  summary_prevalence(group_by_vars = jaar)  
prevalence <- as.data.frame(prevalence)
prevalence <- data.table(prevalence)
write.xlsx(prevalence, "data/prevalence_cc_uniek_zwangerschap.xlsx", append = T, sheetName = "prevalence_cc_lasso" )


####### xgboost
df_xgb <-
  readRDS("data/completecase_predictions_with_year_xgb_kwetsbaar_ja_uniek_zwangerschap.RDS")
# over alle jaren
calculate_prevalence_over_datasets(list(df_xgb), 
                                   group_by_vars = NULL) %>%
  summary_prevalence(group_by_vars = NULL)
# per jaar
calculate_prevalence_over_datasets(list(df_xgb), 
                                   group_by_vars = jaar) %>%
  summary_prevalence(group_by_vars = jaar) 

# Opslaan 
prevalence <- calculate_prevalence_over_datasets(list(df_xgb), 
                                                 group_by_vars = jaar) %>%
  summary_prevalence(group_by_vars = jaar) 
prevalence <- as.data.frame(prevalence)
prevalence <- data.table(prevalence)
write.xlsx(prevalence, "data/prevalence_cc_uniek_zwangerschap.xlsx", append = T, sheetName = "prevalence_cc_xgb" )




#############
## Data- imputeren 
############

# Op dezelfde manier als in 6_data_imputation, met MICE 
library(mice)
library(skimr)

analysebestand_kwetsbaar_uniek_zwangerschap_di <- analysebestand_kwetsbaar_uniek_zwangerschap
skim(analysebestand_kwetsbaar_uniek_zwangerschap_di)
analysebestand_kwetsbaar_uniek_zwangerschap_di <- analysebestand_kwetsbaar_uniek_zwangerschap_di %>% select(-rangnummer)

pred <- quickpred(analysebestand_kwetsbaar_uniek_zwangerschap_di, mincor = 0.05)

## Vijf datasets imputeren, met seed en zonder predictormatrix
imputed_uniek <- mice(analysebestand_kwetsbaar_uniek_zwangerschap_di, seed = 2016, m = 5, printFlag = TRUE, predictorMatrix = pred)

imputed_uniek$imp$opleidingsniveau # random check bij variabelen welke waarden zijn geimputeerd
imputed_uniek$imp$SECM

# Nieuwe databestanden in environment van imputatie-sets
kwetsbaar_imputed_uniek1 <- complete(imputed_uniek,1)
kwetsbaar_imputed_uniek2 <- complete(imputed_uniek,2)
kwetsbaar_imputed_uniek3 <- complete(imputed_uniek,3)
kwetsbaar_imputed_uniek4 <- complete(imputed_uniek,4)
kwetsbaar_imputed_uniek5 <- complete(imputed_uniek,5)

# Opslaan in onze map 
write.fst(kwetsbaar_imputed_uniek1, "data/kwetsbaar_imputed_uniek1.fst")
write.fst(kwetsbaar_imputed_uniek2, "data/kwetsbaar_imputed_uniek2.fst")
write.fst(kwetsbaar_imputed_uniek3, "data/kwetsbaar_imputed_uniek3.fst")
write.fst(kwetsbaar_imputed_uniek4, "data/kwetsbaar_imputed_uniek4.fst")
write.fst(kwetsbaar_imputed_uniek5, "data/kwetsbaar_imputed_uniek5.fst")


##########
## Predict imputed 
##########

# Volgen script 7_predict_imputed 

library(glue)
library(writexl)
library(dplyr)
library(xlsx)
library(fst)

source("utils_predict.R")
source("utils_metrics.R")

# inlezen geimputeerde datasets
# de voorbereide datasets in een lijst zetten
datalist <- 1:5 %>%
  purrr::map(\(dataset_number) {
    imp_dataset <- read.fst(glue::glue("H:/LCA kwetsbaarheid/LCA Merge bestanden/Zwangere vrouwen alleen landelijke registratie-data (vervolg LCA)/data/kwetsbaar_imputed_uniek{dataset_number}.fst"))
    #Zelfde variabelen als bij trainen model (dus bijv. rinpersoon er uit etc)
    imp_dataset_model <- imp_dataset %>%
      select(
        -RINPERSOONS,
        -RINPERSOON,
        #-Rinpersoons_KIND, # deze evt aanzetten wanneer rinpersoons_kind er wel in zit
        #-RINPERSOON_KIND, # deze evt aanzetten wanneer rinpersoon_kind er wel in zit
        -jaar # ook jaartal er uit: straks verschillende jaren laten zien
      )
  })


names(datalist) <- c(
  "kwetsbaar_imputed_uniek1",
  "kwetsbaar_imputed_uniek2",
  "kwetsbaar_imputed_uniek3",
  "kwetsbaar_imputed_uniek4",
  "kwetsbaar_imputed_uniek5"
)

# jaartallen voor later gebruik:
# is voor elke geimputeerde dataset hetzelfde, we pakken de kolom van dataset1
df_extract_additional_info <- read.fst("data/kwetsbaar_imputed_uniek1.fst")
years <- df_extract_additional_info %>%
  select(jaar)

# TODO: gemeentenummers toevoegen om het % per gemeente te krijgen > nu apart gevraagd aan RH


##################################################################
#### predict with random forest -uniek 

# prevalentie random forest
# lees het model in
random_forest_cbs <- readRDS("data/final_rf")

# bereken de prevalentie per geimputeerde dataset en stop ze in een lijst
imputed_pred_list_rf <- datalist %>%
  purrr::imap(
    .f = function(.x, .y) {
      calculate_prevalence_tidy(
        model = random_forest_cbs$rf_final_fit,
        probability_threshold = random_forest_cbs$probability_opt,
        dataset = .x,
        model_name = "random_forest",
        dataset_name = .y
      )
    }
  )


# voorspellingen met jaartal voor elke geimputeerde dataset: lijst
imputed_preds_with_year <- imputed_pred_list_rf %>%
  purrr::map(
    \(list_pred) { # calculate prevalence per year for each imputed dataset
      df <- tibble(
        # TODO: hier evt. de gemeentes bij zetten: gemeente = gemeente
        jaar = years,
        voorspelling_kwetsbaar = list_pred$prediction_labels,
      )
    }
  )


# prevalentie over alle jaren: gemiddelde en sd
imputed_overall_prevalence <- calculate_prevalence_over_datasets(imputed_preds_with_year, 
                                                                 group_by_vars = NULL)

# gemiddelde en sd
imputed_overall_prevalence <- imputed_overall_prevalence %>%
  summary_prevalence(group_by_vars = NULL)
# bekijken
imputed_overall_prevalence

imputed_prevalence_per_year <- calculate_prevalence_over_datasets(
  imputed_preds_with_year,
  group_by_vars = jaar
) 
# bekijken
imputed_prevalence_per_year

# gemiddelde en standaard deviatie per jaar over de vijf geimputeerde datasets
kwetsbaar_per_jaar <- imputed_prevalence_per_year %>%
  summary_prevalence(group_by_vars = jaar)
#bekijken
kwetsbaar_per_jaar

# opslaan
imputed_preds_with_year %>%
  saveRDS("data/imputed_predictions_with_year_random_forest_kwetsbaar_uniek_ja.RDS")

imputed_overall_prevalence %>% 
  saveRDS("data/imputed_prevalence_per_year_random_forest_kwetsbaar_uniek_ja.RDS")

imputed_prevalence_per_year %>%
  saveRDS("data/imputed_prevalence_per_year_random_forest_kwetsbaar_uniek_ja.RDS")


# volgende stap: koppelen van nieuwe variabele aan de dataset (ie 7_predict_imputed)
# en in figures and tables kijken welke volgende stappen we moeten nemen om de percentages in een Excel-bestandje te krijgen. 

########## random forest

# lees in lijst van voorspelde labels per geimputeerde dataset
imputed_preds_with_year <- readRDS("data/imputed_predictions_with_year_random_forest_kwetsbaar_uniek_ja.RDS")

# prevalentie over alle jaren: gemiddelde en sd
calculate_prevalence_over_datasets(imputed_preds_with_year, 
                                   group_by_vars = NULL) %>%
  summary_prevalence(group_by_vars = NULL)

# aantallen per groep (hier dataset imputed1)
aantallen_imputed_uniek1 <- imputed_preds_with_year$kwetsbaar_imputed_uniek1 %>% 
  group_by(jaar$jaar) %>% 
  count(voorspelling_kwetsbaar)
aantallen_imputed_uniek1


# prevalentie per jaar 
calculate_prevalence_over_datasets(imputed_preds_with_year, 
                                   group_by_vars = jaar) %>%
  summary_prevalence(group_by_vars = jaar)

# prevalentie per geimputeerde dataset
imputed_prevalence <- readRDS("data/imputed_prevalence_per_year_random_forest_kwetsbaar_uniek_ja.rds")
imputed_prevalence


# Opslaan 
prevalence <- calculate_prevalence_over_datasets(imputed_preds_with_year, 
                                                 group_by_vars = jaar) %>%
  summary_prevalence(group_by_vars = jaar)
library(data.table)
prevalence <- as.data.frame(prevalence)
prevalence <- data.table(prevalence)
aantallen_imputed_uniek1 <- data.table(aantallen_imputed_uniek1)
imputed_prevalence <- data.table(imputed_prevalence)
write.xlsx(prevalence, "data/prevalence_uniek.xlsx", append = F, sheetName = "prevalence_uniek_RF" )
write.xlsx(aantallen_imputed_uniek1, "data/prevalence_uniek.xlsx", append = T, sheetName = "n_imputed1_uniek_RF" )
write.xlsx(imputed_prevalence, "data/prevalence_uniek.xlsx", append = T, sheetName = "prev_imputedsets_uniek_RF" )


##################################################################
#### predict with Lasso -uniek 

############### xgboost
# prevalentie xgboost
# lees het model in
xgb_cbs <- readRDS("data/final_xg")

# bereken de prevalentie per geimputeerde dataset en stop ze in een lijst
imputed_pred_list_xgb <- datalist %>%
  purrr::imap(
    .f = function(.x, .y) {
      calculate_prevalence_tidy(
        model = xgb_cbs$xg_final_fit,
        probability_threshold = xgb_cbs$probability_opt,
        dataset = .x, model_name = "xgboost",
        dataset_name = .y
      )
    }
  )
# voorspellingen met jaartal voor elke geimputeerde dataset: lijst
imputed_preds_with_year <- imputed_pred_list_xgb %>%
  purrr::map(
    \(list_pred) { # calculate prevalence per year for each imputed dataset
      df <- tibble(
        # TODO: Joyce: hier evt de gemeentes bij zetten gemeente = gemeente
        jaar = years,
        voorspelling_kwetsbaar = list_pred$prediction_labels,
      )
    }
  )


# prevalentie over alle jaren: gemiddelde en sd
imputed_overall_prevalence <- calculate_prevalence_over_datasets(imputed_preds_with_year, 
                                                                 group_by_vars = NULL)

# gemiddelde en sd
imputed_overall_prevalence <- imputed_overall_prevalence %>%
  summary_prevalence(group_by_vars = NULL)
# bekijken
imputed_overall_prevalence

imputed_prevalence_per_year <- calculate_prevalence_over_datasets(
  imputed_preds_with_year,
  group_by_vars = jaar
) 
# bekijken
imputed_prevalence_per_year

# gemiddelde en standaard deviatie per jaar over de vijf geimputeerde datasets
kwetsbaar_per_jaar <- imputed_prevalence_per_year %>%
  summary_prevalence(group_by_vars = jaar)
#bekijken
kwetsbaar_per_jaar

# opslaan
imputed_preds_with_year %>%
  saveRDS("data/imputed_predictions_with_year_xgb_kwetsbaar_uniek_ja.RDS")

imputed_overall_prevalence %>% 
  saveRDS("data/imputed_prevalence_per_year_xgb_kwetsbaar_uniek_ja.RDS")

imputed_prevalence_per_year %>%
  saveRDS("data/imputed_prevalence_per_year_xgb_kwetsbaar_uniek_ja.RDS")


# vanuit figures_and_tables
# lees in lijst van voorspelde labels per geimputeerde dataset
imputed_preds_with_year_xgb <- readRDS("data/imputed_predictions_with_year_xgb_kwetsbaar_uniek_ja.RDS")

# prevalentie over alle jaren: gemiddelde en sd
calculate_prevalence_over_datasets(imputed_preds_with_year_xgb, 
                                   group_by_vars = NULL) %>%
  summary_prevalence(group_by_vars = NULL)

# aantallen per groep (hier dataset imputed1)
aantallen_imputed_uniek1 <- imputed_preds_with_year_xgb$kwetsbaar_imputed_uniek1 %>% 
  group_by(jaar$jaar) %>% 
  count(voorspelling_kwetsbaar)
aantallen_imputed_uniek1

# prevalentie per jaar 
calculate_prevalence_over_datasets(imputed_preds_with_year_xgb, 
                                   group_by_vars = jaar) %>%
  summary_prevalence(group_by_vars = jaar)

# prevalentie per geimputeerde dataset
imputed_prevalence <- readRDS("data/imputed_prevalence_per_year_xgb_kwetsbaar_uniek_ja.rds")
imputed_prevalence

# Opslaan 
prevalence <- calculate_prevalence_over_datasets(imputed_preds_with_year_xgb, 
                                                 group_by_vars = jaar) %>%
  summary_prevalence(group_by_vars = jaar)
prevalence <- as.data.frame(prevalence)
prevalence <- data.table(prevalence)
aantallen_imputed_uniek1 <- data.table(aantallen_imputed_uniek1)
imputed_prevalence <- data.table(imputed_prevalence)
write.xlsx(prevalence, "data/prevalence_uniek.xlsx", append = T, sheetName = "prevalence_uniek_xgb" )
write.xlsx(aantallen_imputed_uniek1, "data/prevalence_uniek.xlsx", append = T, sheetName = "n_imputed1_uniek_xgb" )
write.xlsx(imputed_prevalence, "data/prevalence_uniek.xlsx", append = T, sheetName = "prev_imputedsets_uniek_xgb" )



############## lasso
library(glmnet) # let op: waarschijnlijk zit er een fout in de combi tussen glmnet en een ander package, daarom hier inladen
# prevalentie lasso
# lees het model in
lasso_cbs <- readRDS("data/final_glmnet")
# lees het recept in
lr_prep <- readRDS("data/lr_prep")

# bereken de prevalentie per geimputeerde dataset en stop ze in een lijst
imputed_pred_list_lasso <- datalist %>% purrr::imap(
  .f = function(.x, .y) {
    calculate_prevalence_lasso(
      model = lasso_cbs$glmnet_obj,
      lambda_opt = lasso_cbs$lambda_opt,
      lr_prep = lr_prep,
      probability_threshold = lasso_cbs$probability_opt,
      dataset = .x,
      model_name = "lasso",
      dataset_name = .y
    )
  }
)

# voor de zekerheid opnieuw years bestand maken
df_extract_additional_info <- read.fst("data/kwetsbaar_imputed_uniek1.fst")
years <- df_extract_additional_info %>%
  select(jaar)

# voorspellingen met jaartal voor elke geimputeerde dataset: lijst
imputed_preds_with_year <- imputed_pred_list_lasso %>%
  purrr::map(
    \(list_pred) { # calculate prevalence per year for each imputed dataset
      df <- tibble(
        # TODO hier evt. de gemeentes bij zetten gemeente = gemeente
        jaar = years,
        voorspelling_kwetsbaar = list_pred$prediction_labels,
      )
    }
  )


# prevalentie over alle jaren: gemiddelde en sd
imputed_overall_prevalence <- calculate_prevalence_over_datasets(imputed_preds_with_year, 
                                                                 group_by_vars = NULL)

# gemiddelde en sd
imputed_overall_prevalence <- imputed_overall_prevalence %>%
  summary_prevalence(group_by_vars = NULL)
# bekijken
imputed_overall_prevalence

imputed_prevalence_per_year <- calculate_prevalence_over_datasets(
  imputed_preds_with_year,
  group_by_vars = jaar
) 
# bekijken
imputed_prevalence_per_year

# gemiddelde en standaard deviatie per jaar over de vijf geimputeerde datasets
kwetsbaar_per_jaar <- imputed_prevalence_per_year %>%
  summary_prevalence(group_by_vars = jaar)
#bekijken
kwetsbaar_per_jaar

# opslaan
imputed_preds_with_year %>%
  saveRDS("data/imputed_predictions_with_year_lasso_kwetsbaar_uniek_ja.RDS")

imputed_overall_prevalence %>% 
  saveRDS("data/imputed_prevalence_per_year_lasso_kwetsbaar_uniek_ja.RDS")

imputed_prevalence_per_year %>%
  saveRDS("data/imputed_prevalence_per_year_lasso_kwetsbaar_uniek_ja.RDS")


# vanuit figures and tables: 
########### lasso
# lees in lijst van voorspelde labels per geimputeerde dataset
imputed_preds_with_year_lasso <- readRDS("data/imputed_predictions_with_year_lasso_kwetsbaar_uniek_ja.RDS")

# prevalentie over alle jaren: gemiddelde en sd
calculate_prevalence_over_datasets(imputed_preds_with_year_lasso, 
                                   group_by_vars = NULL) %>%
  summary_prevalence(group_by_vars = NULL)

# aantallen per groep (hier dataset imputed1)
aantallen_imputed_uniek1 <- imputed_preds_with_year$kwetsbaar_imputed_uniek1 %>% 
  group_by(jaar$jaar) %>% 
  count(voorspelling_kwetsbaar)
aantallen_imputed_uniek1

# prevalentie per jaar 
calculate_prevalence_over_datasets(imputed_preds_with_year_lasso, 
                                   group_by_vars = jaar) %>%
  summary_prevalence(group_by_vars = jaar)

# prevalentie per geimputeerde dataset
imputed_prevalence <- readRDS("data/imputed_prevalence_per_year_lasso_kwetsbaar_uniek_ja.rds")
imputed_prevalence

# Opslaan 
prevalence <- calculate_prevalence_over_datasets(imputed_preds_with_year_lasso, 
                                                 group_by_vars = jaar) %>%
  summary_prevalence(group_by_vars = jaar)
prevalence <- as.data.frame(prevalence)
prevalence <- data.table(prevalence)
aantallen_imputed1 <- data.table(aantallen_imputed1)
imputed_prevalence <- data.table(imputed_prevalence)
write.xlsx(prevalence, "data/prevalence_uniek.xlsx", append = T, sheetName = "prevalence_uniek_lasso" )
write.xlsx(aantallen_imputed_uniek1, "data/prevalence_uniek.xlsx", append = T, sheetName = "n_imputed1_uniek_lasso" )
write.xlsx(imputed_prevalence, "data/prevalence_uniek.xlsx", append = T, sheetName = "prev_imputedsets_uniek_lasso" )


