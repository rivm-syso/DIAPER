# script om de data op te delen in train en test set voor modellen

# benodigde libraries
library(tidymodels)

source("utils_testset.R")


# laad de data in 
# deze is in 0_data_prepare.R aangemaakt
dataset_cbs <- readRDS("data/dataset_cbs")

#### kruisvalidatie voor de test set
set.seed(97)
cv_test <- vfold_cv(dataset_cbs, v = 6, strata = multidimensionaal)

# voor elke fold toevoegen wat de train_ids zijn
cv_test <- cv_test %>% mutate(
  train_ids = splits %>% purrr::map(function(x) x$in_id)
)


# determine fold_ids of vfold cross validation of each training set
cv_test %>% 
  pull(splits) %>%
  imap(
    function(splits, .y) {
      data_train <- training(splits)
      fold_ids <- determine_fold_ids(data_train)
      fold_ids %>% saveRDS(glue::glue("data/fold_ids_{.y}.rds"))
    }
  )

