library(glue)
library(writexl)
library(dplyr)
library(xlsx)
library(fst)

source("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Zwangere vrouwen alleen landelijke registratie-data (vervolg LCA)/utils_predict.R")
source("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Zwangere vrouwen alleen landelijke registratie-data (vervolg LCA)/utils_metrics.R")

# je imputatie dataset voorbereiden zodat alle variabelen matchen met de dataset
# voor het voorspelmodel. Hierin is aangenomen dat ze dezelfde naam als eerder hebben.
# Als je ze anders noemt moet je ze ook aanpassen in datalist

# inlezen geimputeerde datasets
# de voorbereide datasets in een lijst zetten
datalist <- 1:5 %>%
  purrr::map(\(dataset_number) {
    imp_dataset <- read.fst(glue::glue("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Data/kwetsbaar_imputed{dataset_number}.fst"))
    #Zelfde variabelen als bij trainen model (dus bijv. rinpersoon er uit etc)
    imp_dataset_model <- imp_dataset %>%
      select(
        -RINPERSOONS,
        -RINPERSOON,
        -jaar # ook jaartal er uit: straks verschillende jaren laten zien
      )
  })

names(datalist) <- c(
  "kwetsbaar_imputed1",
  "kwetsbaar_imputed2",
  "kwetsbaar_imputed3",
  "kwetsbaar_imputed4",
  "kwetsbaar_imputed5"
)

# jaartallen voor later gebruik:
# is voor elke geimputeerde dataset hetzelfde, we pakken de kolom van dataset1
df_extract_additional_info <- read.fst("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Data/kwetsbaar_imputed1.fst")
years <- df_extract_additional_info %>%
  select(jaar)




##################################################################
#### predict with random forest

# prevalentie random forest
# lees het model in
random_forest_cbs <- readRDS("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Zwangere vrouwen alleen landelijke registratie-data (vervolg LCA)/data/final_rf")

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
  saveRDS("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Data/imputed_predictions_with_year_random_forest_kwetsbaar_ja.RDS")

imputed_overall_prevalence %>% 
  saveRDS("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Data/imputed_prevalence_overall_random_forest_kwetsbaar_ja.RDS")

imputed_prevalence_per_year %>%
  saveRDS("H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Data/imputed_prevalence_per_year_random_forest_kwetsbaar_ja.RDS")

write.xlsx(imputed_overall_prevalence, "H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/percentage_kwets_tot_pJaar.xlsx", sheetName = "Totaal", append = F)
write.xlsx(kwetsbaar_per_jaar, file=  "H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/percentage_kwets_tot_pJaar.xlsx",append = T, sheetName = "Per jaar" )

write.xlsx(kwetsbaar_per_jaar, "H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/percentage_kwets_tot_pJaar23.xlsx", sheetName = "Totaal", append = F)



# Opslaan variabele kwetsbaar voor koppeling aan andere cijfers
# Eerst rinpersoon(s) van het kind toevoegen, omdat de koppeling anders mogelijk moeilijker wordt
# In DT staat Rinpersoons_kind en RINPERSOON_KIND
# In analysebestand_kwetsbaar_imputed1_jaar staat nu de variabele 'voorspelling kwetsbaar'
# en mogelijk ook nog de variabele kanskwetsbaar
analysebestand_kwetsbaar_imputed1_jaar <- datalist[[1]]

analysebestand_kwetsbaar_imputed1_jaar <- analysebestand_kwetsbaar_imputed1_jaar %>%
  bind_cols(
    RINPERSOONS = DT$RINPERSOONS,
    RINPERSOON = DT$RINPERSOON,
    Rinpersoons_KIND = DT$Rinpersoons_KIND,
    RINPERSOON_KIND = DT$RINPERSOON_KIND
  )

setnames(analysebestand_kwetsbaar_imputed1_jaar, 
         c("Rinpersoons_KIND...1", "RINPERSOON_KIND...2"),
         c("Rinpersoons_KIND", "RINPERSOON_KIND"))

# make dataset with prediction labels and probabilities
meervoudig_kwetsbaar <- analysebestand_kwetsbaar_imputed1_jaar %>%
  select(
    RINPERSOONS,
    RINPERSOON,
    Rinpersoons_KIND,
    RINPERSOON_KIND, 
  ) %>%
  mutate(
    voorspelling_kwetsbaar = imputed_pred_list_rf[[1]]$prediction_labels,
    kans_kwetsbaar = imputed_pred_list_rf[[1]]$prediction_probabilities %>% pull(.pred_ja)
  )

# save to disk
write.fst(
  meervoudig_kwetsbaar,
  "H:/Data proces/src/projecten/Kwetsbaarheid/2023 update/Data/meervoudig_kwetsbaar23.fst" # opslaan in map voor gebruik in andere studies
)
write.csv2(meervoudig_kwetsbaar,
  file = "meervoudig_kwetsbaar.csv"
)

table(meervoudig_kwetsbaar$voorspelling_kwetsbaar)


