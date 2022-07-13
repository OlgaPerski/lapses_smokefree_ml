# load packages -----------------------------------------------------------

source(here::here("scripts", "0_load_packages.R"))

doParallel::registerDoParallel()

# load data ---------------------------------------------------------------

data_clean <- read_rds(here::here("data", "clean_data.rds"))

# restrict the sample to participants with >=20 craving entries ----------------------

data <- data_clean %>%
  group_by(account_id) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 20)

# select vars to include and impute missing

exclude <- c("adjusted_quit_date", "adjusted_craving_record_created", 
             "ttfc_1", "ttfc_2", "ttfc_3", "ttfc_4",
             "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", 
             "morning", "midday", "evening", "night", 
             "event_nr", "n")

df <- data %>%
  select(-all_of(exclude))

df$mins_since_prev_rep[is.na(df$mins_since_prev_rep)] <- 0

# remove IDs with all/no lapses

df <- df %>%
  group_by(account_id) %>%
  mutate(lapse_events = n()) %>%
  mutate(prop_lapses = sum(craving_did_smoke == "true", na.rm = T)/lapse_events) %>%
  filter(prop_lapses > 0 & prop_lapses < 1)

# select individual IDs and tune models

all_individuals <- as.character(unique(df$account_id))

# ID1 ---------------------------------------------------------------------

df1 <- df %>%
  filter(account_id == all_individuals[1]) %>%
  ungroup() %>%
  select(-c("account_id", "cigs_per_day", "time_to_first_cig", "lapse_events", "prop_lapses"))

data_split <- initial_split(df2, prop = .80)
train_data <- training(data_split)
test_data  <- testing(data_split)

rf_rec <- recipe(craving_did_smoke ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(craving_severity, days_since_qd, mins_since_prev_rep) %>%
  step_upsample(craving_did_smoke) # manages class imbalances

rf_prep <- prep(rf_rec)
rf_juiced <- juice(rf_prep)

rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf_rf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)

folds <- vfold_cv(train_data, v = 10)

rf_res <-
  tune_grid(
    tune_wf_rf,
    resamples = folds,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, sens, spec, accuracy)
  )

rf_res %>% select_best("roc_auc")

best_rf_auc <-
  select_best(rf_res, "roc_auc")

final_rf <- finalize_model(
  rf_spec,
  best_rf_auc
)  

final_rf_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(final_rf)

final_rf_res <- final_rf_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))

metrics_rf <- final_rf_res %>%
  collect_metrics()

# ID2 ---------------------------------------------------------------------

df2 <- df %>%
  filter(account_id == all_individuals[2]) %>%
  ungroup() %>%
  select(-c("account_id", "cigs_per_day", "time_to_first_cig", "lapse_events", "prop_lapses"))

data_split <- initial_split(df2, prop = .80)
train_data <- training(data_split)
test_data  <- testing(data_split)

rf_rec <- recipe(craving_did_smoke ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(craving_severity, days_since_qd, mins_since_prev_rep) %>%
  step_upsample(craving_did_smoke) # manages class imbalances

rf_prep <- prep(rf_rec)
rf_juiced <- juice(rf_prep)

rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf_rf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)

folds <- vfold_cv(train_data, v = 10)

rf_res <-
  tune_grid(
    tune_wf_rf,
    resamples = folds,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, sens, spec, accuracy)
  )

rf_res %>% select_best("roc_auc")

best_rf_auc <-
  select_best(rf_res, "roc_auc")

final_rf <- finalize_model(
  rf_spec,
  best_rf_auc
)  

final_rf_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(final_rf)

final_rf_res <- final_rf_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))

metrics_rf <- final_rf_res %>%
  collect_metrics()

# ID3 ---------------------------------------------------------------------

df3 <- df %>%
  filter(account_id == all_individuals[3]) %>%
  ungroup() %>%
  select(-c("account_id", "cigs_per_day", "time_to_first_cig", "lapse_events", "prop_lapses"))

data_split <- initial_split(df3, prop = .80)
train_data <- training(data_split)
summary(train_data) # no lapses in the train data
test_data  <- testing(data_split)

rf_rec <- recipe(craving_did_smoke ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(craving_severity, days_since_qd, mins_since_prev_rep) %>%
  step_upsample(craving_did_smoke) # manages class imbalances

rf_prep <- prep(rf_rec)
rf_juiced <- juice(rf_prep)

rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf_rf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)

folds <- vfold_cv(train_data, v = 10)

rf_res <-
  tune_grid(
    tune_wf_rf,
    resamples = folds,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, sens, spec, accuracy)
  )

rf_res %>% select_best("roc_auc")

best_rf_auc <-
  select_best(rf_res, "roc_auc")

final_rf <- finalize_model(
  rf_spec,
  best_rf_auc
)  

final_rf_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(final_rf)

final_rf_res <- final_rf_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))

metrics_rf <- final_rf_res %>%
  collect_metrics()

# ID4 ---------------------------------------------------------------------

df4 <- df %>%
  filter(account_id == all_individuals[4]) %>%
  ungroup() %>%
  select(-c("account_id", "cigs_per_day", "time_to_first_cig", "lapse_events", "prop_lapses", "days_since_qd" , "wkday_craving", "time_of_day"))

data_split <- initial_split(df4, prop = .80)
train_data <- training(data_split)
summary(train_data)
test_data  <- testing(data_split)

rf_rec <- recipe(craving_did_smoke ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(craving_severity, mins_since_prev_rep) %>%
  step_upsample(craving_did_smoke) # manages class imbalances

rf_prep <- prep(rf_rec)
rf_juiced <- juice(rf_prep)

rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf_rf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)

folds <- vfold_cv(train_data, v = 10)

rf_res <-
  tune_grid(
    tune_wf_rf,
    resamples = folds,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, sens, spec, accuracy)
  )

rf_res %>% select_best("roc_auc")

best_rf_auc <-
  select_best(rf_res, "roc_auc")

final_rf <- finalize_model(
  rf_spec,
  best_rf_auc
)  

final_rf_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(final_rf)

final_rf_res <- final_rf_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))

metrics_rf <- final_rf_res %>%
  collect_metrics()

# ID5 ---------------------------------------------------------------------

df5 <- df %>%
  filter(account_id == all_individuals[5]) %>%
  ungroup() %>%
  select(-c("account_id", "cigs_per_day", "time_to_first_cig", "lapse_events", "prop_lapses"))

data_split <- initial_split(df5, prop = .80)
train_data <- training(data_split)
summary(train_data)
test_data  <- testing(data_split)

rf_rec <- recipe(craving_did_smoke ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(craving_severity, days_since_qd, mins_since_prev_rep) %>%
  step_upsample(craving_did_smoke) # manages class imbalances

rf_prep <- prep(rf_rec)
rf_juiced <- juice(rf_prep)

rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf_rf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)

folds <- vfold_cv(train_data, v = 10)

rf_res <-
  tune_grid(
    tune_wf_rf,
    resamples = folds,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, sens, spec, accuracy)
  )

rf_res %>% select_best("roc_auc")

best_rf_auc <-
  select_best(rf_res, "roc_auc")

final_rf <- finalize_model(
  rf_spec,
  best_rf_auc
)  

final_rf_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(final_rf)

final_rf_res <- final_rf_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))

metrics_rf <- final_rf_res %>%
  collect_metrics()

# ID6 ---------------------------------------------------------------------

lapse_count <- df %>%
  group_by(account_id, craving_did_smoke) %>%
  filter(craving_did_smoke != "false") %>%
  summarise(n_lapses = n()) %>%
  filter(n_lapses == 4) %>%
  pull(account_id)

lapse_count <- as.character(droplevels(lapse_count))

df6 <- df %>%
  ungroup() %>%
  filter(account_id == lapse_count[1]) %>%
  select(-c("account_id", "cigs_per_day", "time_to_first_cig", "lapse_events", "prop_lapses"))

data_split <- initial_split(df6, prop = .80, strata = craving_did_smoke)
train_data <- training(data_split)
summary(train_data)
test_data  <- testing(data_split)
summary(test_data)

rf_rec <- recipe(craving_did_smoke ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(craving_severity, days_since_qd, mins_since_prev_rep) %>%
  step_upsample(craving_did_smoke) # manages class imbalances

rf_prep <- prep(rf_rec)
rf_juiced <- juice(rf_prep)

rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf_rf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)

folds <- vfold_cv(train_data, v = 10)

rf_res <-
  tune_grid(
    tune_wf_rf,
    resamples = folds,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, sens, spec, accuracy)
  )

rf_res %>% select_best("roc_auc")

best_rf_auc <-
  select_best(rf_res, "roc_auc")

final_rf <- finalize_model(
  rf_spec,
  best_rf_auc
)  

final_rf_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(final_rf)

final_rf_res <- final_rf_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))

metrics_rf <- final_rf_res %>%
  collect_metrics()

# ID7 ---------------------------------------------------------------------

lapse_count <- df %>%
  group_by(account_id, craving_did_smoke) %>%
  filter(craving_did_smoke != "false") %>%
  summarise(n_lapses = n()) %>%
  filter(n_lapses == 6) %>%
  pull(account_id)

lapse_count <- as.character(droplevels(lapse_count))

df7 <- df %>%
  ungroup() %>%
  filter(account_id == lapse_count[1]) %>%
  select(-c("account_id", "cigs_per_day", "time_to_first_cig", "lapse_events", "prop_lapses"))

data_split <- initial_split(df7, prop = .80)
train_data <- training(data_split)
summary(train_data)
test_data  <- testing(data_split)
summary(test_data)

rf_rec <- recipe(craving_did_smoke ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(craving_severity, days_since_qd, mins_since_prev_rep) %>%
  step_upsample(craving_did_smoke) # manages class imbalances

rf_prep <- prep(rf_rec)
rf_juiced <- juice(rf_prep)

rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

tune_wf_rf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(rf_spec)

folds <- vfold_cv(train_data, v = 10)

rf_res <-
  tune_grid(
    tune_wf_rf,
    resamples = folds,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, sens, spec, accuracy)
  )

rf_res %>% select_best("roc_auc")

best_rf_auc <-
  select_best(rf_res, "roc_auc")

final_rf <- finalize_model(
  rf_spec,
  best_rf_auc
)  

final_rf_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(final_rf)

final_rf_res <- final_rf_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))

metrics_rf <- final_rf_res %>%
  collect_metrics()
