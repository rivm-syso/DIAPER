library(yardstick)


################# hulpfunctie om kans naar klasse om te zetten
## gegeven afkapwaarde threshold
## predicted_pos_probabilities is de kans op multidimensionaal == "ja"
# het label is "ja" als de kans groter of gelijk aan de afkapwaarde is
to_labels <- function(predicted_pos_probabilities, threshold) {
  labels <- if_else(
    predicted_pos_probabilities >= threshold, "ja", "nee"
  ) %>% factor(levels = c("ja", "nee"))
  return(labels)
}

# hulpfunctie om de score uit te rekenen voor gegeven drempelwaarden
# default is de f1-score
calculate_scores <- function(preds_class_ja, 
                             truth_vector, 
                             threshold_probabilties,
                             beta = 1) {
  scores <- purrr::map_dbl(
    .x = threshold_probabilties,
    .f = function(p_threshold) calculate_fbeta_score(
      p_threshold, 
      preds_class_ja, 
      truth_vector, 
      beta = beta)
  )
  # remove NA values
  scores <- scores[!is.na(scores)]
  return(scores)
}

# calculate f-beta score for given threshold, predictions and truth
# default is beta = 1, ie the f1 measure
calculate_fbeta_score <- function(p_threshold, 
                                  preds_class_ja, 
                                  truth_vector, 
                                  beta) {
  # convert probability to class label for given threshold probability
  pred_labels <- to_labels(preds_class_ja, p_threshold)
  # put in dataframe to use in evaluation of metric
  df_tmp <- bind_cols(truth = truth_vector, pred_class = pred_labels)
  # Joyce: je zou hier een andere metriek kunnen kiezen als je wilt
  # of bijvoorbeeld beta = 1 (default) aanpassen hieronder als je bijvoorbeeld
  # wilt dat recall beta keer zo belangrijk is als precision
  metric_value <- yardstick::f_meas(
    data = df_tmp, 
    truth = truth, 
    estimate = pred_class,
    beta = beta
  ) %>%
    suppressWarnings() %>%
    pull(.estimate)
    
  return(metric_value)
}


# multi metric used in main script to calculate several measures in one go
multi_metric <- metric_set(recall, 
                           precision, 
                           f_meas, 
                           specificity) # f measure neemt gemiddelde tussen recall en precision: kunnen we gebruiken om drempel van 0.5 te tunen

