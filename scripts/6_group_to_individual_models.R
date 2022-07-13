# load packages -----------------------------------------------------------

source(here::here("scripts", "0_load_packages.R"))

doParallel::registerDoParallel()

# load data ---------------------------------------------------------------

data_clean <- read_rds(here::here("data", "clean_data.rds"))

# restrict the sample to participants with >=20 craving entries

data <- data_clean %>%
  group_by(account_id) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 20)

# select vars to include

exclude <- c("adjusted_quit_date", "adjusted_craving_record_created", 
             "ttfc_1", "ttfc_2", "ttfc_3", "ttfc_4",
             "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", 
             "morning", "midday", "evening", "night", 
             "event_nr", "n")

df <- data %>%
  select(-all_of(exclude))

df$mins_since_prev_rep[is.na(df$mins_since_prev_rep)] <- 0

# split into separate lists based on ID

df_split <- df %>%
  group_by(account_id) %>%
  group_split()

# split into training and testing datasets --------------------------------

df_split_training <- list()

for(i in 1:length(df_split)) {
  
  df_split_training[[i]] <- df_split[-i] %>% # leaves participant i out of the training set
    bind_rows() %>%
    mutate(train_split = "training")
  
  df_split_training[[i]] <- df_split_training[[i]] %>% # uses participant i for the testing set
    bind_rows(., df_split[i] %>%
                bind_rows() %>%
                mutate(train_split = "testing"))
  
  df_split_training[[i]] <- df_split_training[[i]] %>%
    select(-c("account_id"))
}

write_rds(df_split_training, here("data", "df_split_training.rds"))

# fit best-performing group-level model to individual data ---------------------------

df_model_fit <- list()

for (i in 1:length(df_split_training)) {
  
  train_data <- df_split_training[[i]] %>%
    filter(train_split == "training") %>%
    select(-train_split)
  
  test_data <- df_split_training[[i]] %>%
    filter(train_split == "testing") %>%
    select(-train_split)
  
  rf_rec <- recipe(craving_did_smoke ~ ., data = train_data) %>%
    step_dummy(all_nominal(), -all_outcomes()) %>%
    step_normalize(cigs_per_day, craving_severity, days_since_qd, mins_since_prev_rep) %>%
    step_downsample(craving_did_smoke, skip = TRUE) # manages class imbalances
  
  rf_prep <- prep(rf_rec)
  rf_juiced <- juice(rf_prep)
  
  xgb_spec <- boost_tree(
    trees = 1000, 
    tree_depth = 6, min_n = 8, 
    loss_reduction = 0.853,                   
    sample_size = 0.858, mtry = 23,         
    learn_rate = 0.0277,                        
  ) %>% 
    set_engine("xgboost") %>% 
    set_mode("classification")
  
  wf_xgb <- workflow() %>%
    add_recipe(rf_rec) %>%
    add_model(xgb_spec)
  
  final_xgb_workflow_fit <- fit(wf_xgb, data = train_data)
  
  test_data$.pred_false <- predict(final_xgb_workflow_fit, new_data = test_data, type = "prob")$.pred_false
  
  roc_test_error <- tryCatch(roc(test_data$craving_did_smoke, test_data$.pred_false), error = function(e) e)
  
  if(any(class(roc_test_error) == "error")) {
    
    message("Error")
    
  } else {
    
    roc_obj <- roc(test_data$craving_did_smoke, test_data$.pred_false)
    auc <- auc(roc_obj)
    ci.auc_lower <- ci.auc(roc_obj)[1]
    ci.auc_upper <- ci.auc(roc_obj)[3]
    
    df_model_fit[[i]] <- list(auc = auc,
                              ci.auc_lower = ci.auc_lower,
                              ci.auc_upper = ci.auc_upper)
  }
}

write_rds(df_model_fit, here("data", "df_model_fit.rds"))

# summarise model performance for each individual -------------------------

df_model_fit <- read_rds(here::here("data", "df_model_fit.rds"))

model_performance <- tibble(auc = unlist(map(df_model_fit, "auc")))

mean(model_performance$auc)
sd(model_performance$auc)
range(model_performance$auc)
