comparing_models_function <- function(x) {
  
  models_successful <- x[!str_detect(names(x), "failure_reason")]
  
  prepare_model_comparison <- lapply(models_successful[str_detect(names(models_successful), "metrics_")], function(x)
    x %>% bind_rows()) %>%
    bind_rows() 
  
}

prepare_model_comparison <- lapply(combined, function(x) comparing_models_function(x))

find_null <- list()

for(i in 1:length(prepare_model_comparison)) {
  
  find_null[i] <- nrow(prepare_model_comparison[[i]])
  
  if(find_null[i] == 0) find_null[i] = NULL
  
}

a <- vector()

for(i in 1:length(find_null)) {
  
  a[i] <- !is.null(find_null[[i]])
  
}

empty_models_removed <- prepare_model_comparison[a]

compare_auc <- function(x) {
  
  model_comparison <- x %>%
    mutate(ignore_model = mean(.estimate, na.rm = FALSE),
           ignore_model = case_when(is.na(ignore_model) ~ "AUC uninterpretable",
                                    TRUE ~ "AUC interpretable"))
  
  highest_auc <- model_comparison %>% 
    filter(.metric == "roc_auc") %>%
    filter(.estimate == max(.estimate)) %>%
    filter(.estimate != 1)
  
  return(highest_auc)
  
}

combined_models_auc <- lapply(empty_models_removed, function(x) compare_auc(x))

find_null <- list()

for(i in 1:length(combined_models_auc)) {
  
  find_null[i] <- nrow(combined_models_auc[[i]])
  
  if(find_null[i] == 1) find_null[i] = NULL
  
}

a <- vector()

for(i in 1:length(find_null)) {
  
  a[i] <- !is.null(find_null[[i]])
  
}

participants_with_auc <- names(combined_models_auc[!a])
vip_for_participants <- lapply(combined[names(combined) %in% participants_with_auc], function(x) {
  
  participant <- x[str_detect(names(x), "vip")]
  
  if(is.element(TRUE, str_detect(names(participant), "rf"))) {
    
    rf = tibble(rf_variable = participant$final_rf_vip[[1]][["Variable"]],
                rf_importance = participant$final_rf_vip[[1]][["Importance"]])
    
  } else {
    
    rf = tibble(rf_variable = NA,
                rf_importance = NA)
  }
  
  if(is.element(TRUE, str_detect(names(participant), "svm"))) {
    
    svm = tibble(svm_variable = participant$final_svm_vip[[1]][["Variable"]],
                 svm_importance = participant$final_svm_vip[[1]][["Importance"]])
    
  } else {
    
    svm = tibble(svm_variable = NA,
                 svm_importance = NA)
    
  }
  
  if(is.element(TRUE, str_detect(names(participant), "elnet"))) {
    
    elnet = tibble(elnet_variable = participant$final_elnet_vip[[1]][["Variable"]],
                   elnet_importance = participant$final_elnet_vip[[1]][["Importance"]],
                   elnet_sign = participant$final_elnet_vip[[1]][["Sign"]])
    
  } else {
    
    elnet = tibble(elnet_variable = NA,
                   elnet_importance = NA,
                   elnet_sign = NA)
    
  }
  
  results <- bind_cols(rf, svm, elnet)
  
  return(results)
}
)

combined_tibble <- list()

for(i in 1:length(vip_for_participants)) {
  
  combined_tibble[i] <- list(vip_for_participants[[i]] %>%
                               mutate(participant_id = unique(names(vip_for_participants[i])),
                                      rank = 1:10))
  
}

final <- bind_rows(combined_tibble)