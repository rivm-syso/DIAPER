library(glue)
library(tidymodels)


calculate_prevalence_tidy <- function(model, probability_threshold, dataset, 
                                      model_name, dataset_name) {
  # dit geeft de kans .pred_ja op "ja" en .pred_nee op "nee" met .pred_ja + .pred_nee == 1
  preds <- model %>%
    predict(dataset, type = "prob")
  preds %>% saveRDS(glue("data/predictions_{model_name}_{dataset_name}.rds"))
  
  # zet voorspellingen om naar klasses op basis van beste afkapwaarde
  pred_labels <- to_labels(
    preds %>% pull(.pred_ja),
    probability_threshold
  )
  
  # uitrekenen prevalentie kwetsbaar
  prevalence_ja <- pred_labels %>%
    as_tibble() %>%
    count(value) %>%
    mutate(prop = n / sum(n)) %>%
    filter(value == "ja") %>%
    pull(prop)
  
  return(list(
    prediction_probabilities = preds,
    prediction_labels = pred_labels,
    prevalence = prevalence_ja)
  )
}



calculate_prevalence_lasso <- function(model, lambda_opt, lr_prep, 
                                       probability_threshold, dataset, 
                                       model_name, dataset_name) {
  # voorspellen van nieuwe data "dataset_new"
  # eerst data in goede vorm zetten
  X <- lr_prep %>% 
    bake(new_data = dataset) # vervangen door juiste dataset
  X <- model.matrix(~., X)
  # klasses voorspellen
  # type = "response" geeft de kans op multidimensionaal
  preds <- predict(
    model,
    newx = X,
    type = "response",
    s = lambda_opt)
  # glmnet voorspelt kans op "nee", dus zetten we om naar kans op "ja"
  preds <- 1 - preds
  preds %>% saveRDS(glue("data/predictions_{model_name}_{dataset_name}.rds"))
  
  # zet voorspellingen om naar klasses op basis van beste afkapwaarde
  pred_labels <- to_labels(
    preds,
    probability_threshold
  )
  
  # uitrekenen prevalentie kwetsbaar
  prevalence_ja <- pred_labels %>%
    as_tibble() %>%
    count(value) %>%
    mutate(prop = n / sum(n)) %>%
    filter(value == "ja") %>%
    pull(prop)
  
  return(list(
    prediction_probabilities = preds,
    prediction_labels = pred_labels,
    prevalence = prevalence_ja)
  )
}


calculate_prevalence_over_datasets <- function(datalist, group_by_vars) {
  datalist %>%
    purrr::imap(
      \(df_pred, .name) {
        df_pred %>%
          mutate(dataset = .name) %>%
          group_by(dataset, {{ group_by_vars }}) %>%
          count(voorspelling_kwetsbaar) %>%
          mutate(prop = n / sum(n)) %>%
          filter(voorspelling_kwetsbaar == "ja") %>%
          select({{ group_by_vars }}, prop, dataset)
      }
    ) %>%
    bind_rows() %>%
    pivot_longer(cols = !c({{ group_by_vars }}, dataset),
                 names_to = "name",
                 values_to = "prevalentie") %>%
    select(-name)
}


summary_prevalence <- function(df_prevalence, group_by_vars) {
  df_prevalence %>%
    group_by({{ group_by_vars }}) %>%
    summarise(
      avg = mean(prevalentie),
      sd = sd(prevalentie)
    )
}

