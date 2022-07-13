# load packages -----------------------------------------------------------

source(here::here("scripts", "0_load_packages.R"))

# load data ---------------------------------------------------------------

data_clean <- read_rds(here::here("data", "clean_data.rds"))

doParallel::registerDoParallel()

# restrict the sample to participants with >=20 craving entries ----------------------

data <- data_clean %>%
  group_by(account_id) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 20)

# select vars to include and impute missing

exclude <- c("account_id", "adjusted_quit_date", "adjusted_craving_record_created", 
             "ttfc_1", "ttfc_2", "ttfc_3", "ttfc_4",
             "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun", 
             "morning", "midday", "evening", "night", 
             "event_nr", "n", "prior_event_lapse")

df <- data %>%
  select(-all_of(exclude))

df$mins_since_prev_rep[is.na(df$mins_since_prev_rep)] <- 0

# split data into training and testing ------------------------------------

set.seed(555)

data_split <- initial_split(df, prop = .80)
train_data <- training(data_split)
test_data  <- testing(data_split)

train_data %>% 
  count(craving_did_smoke) %>% 
  mutate(prop = n/sum(n)) # check balance of levels within outcome var

# set up recipe

rec <- recipe(craving_did_smoke ~ ., data = train_data) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_normalize(cigs_per_day, craving_severity, days_since_qd) %>%
  step_downsample(craving_did_smoke) # manages class imbalances

rf_prep <- prep(rec)
rf_juiced <- juice(rf_prep)

rf_juiced %>% 
  count(craving_did_smoke) %>% 
  mutate(prop = n/sum(n)) # check balance of levels within outcome var

# set up tuning grids ------------------------------------------------------

# random forest

rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

# support vector machine

svm_spec <- svm_rbf(
  cost = tune(),
  rbf_sigma = tune()
) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

# elastic net

elnet_spec <- logistic_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_mode("classification") %>%
  set_engine("glmnet")

# xgboost

xgb_spec <- boost_tree(
  trees = 1000, 
  tree_depth = tune(), min_n = tune(), 
  loss_reduction = tune(),                     ## first three hyperparameters: model complexity
  sample_size = tune(), mtry = tune(),         ## randomness
  learn_rate = tune(),                         ## step size
) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# set up possible values to try through a space-filling design

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), train_data),
  learn_rate(),
  size = 30
)

# set up workflows ---------------------------------------------------------

# random forest

tune_wf_rf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(rf_spec)

# support vector machine

tune_wf_svm <- workflow() %>%
  add_recipe(rec) %>%
  add_model(svm_spec)

# elastic net

tune_wf_elnet <- workflow() %>%
  add_recipe(rec) %>%
  add_model(elnet_spec)

# xgboost

tune_wf_xgb <- workflow() %>%
  add_recipe(rec) %>%
  add_model(xgb_spec)

# train hyperparameters ------------------------------------------------------------

folds <- vfold_cv(train_data, v = 10)

# random forest

rf_res <-
  tune_grid(
    tune_wf_rf,
    resamples = folds,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, sens, spec, accuracy)
  )

rf_res %>% collect_metrics()
rf_res %>% select_best("roc_auc")

best_rf_auc <-
  select_best(rf_res, "roc_auc")

final_rf <- finalize_model(
  rf_spec,
  best_rf_auc
)

# support vector machine

svm_res <-
  tune_grid(
    tune_wf_svm,
    resamples = folds,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, sens, spec, accuracy)
  )

svm_res %>% collect_metrics()
svm_res %>% select_best("roc_auc")

best_svm_auc <-
  select_best(svm_res, "roc_auc")

final_svm <- finalize_model(
  svm_spec,
  best_svm_auc
)

# elastic net

elnet_res <-
  tune_grid(
    tune_wf_elnet,
    resamples = folds,
    grid = 10,
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, sens, spec, accuracy)
  )

elnet_res %>% collect_metrics()
elnet_res %>% select_best("roc_auc")

best_elnet_auc <-
  select_best(elnet_res, "roc_auc")

final_elnet <- finalize_model(
  elnet_spec,
  best_elnet_auc
)

# xgboost

xgb_res <- tune_grid(
  tune_wf_xgb,
  resamples = folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE)
)

xgb_res %>% collect_metrics()
xgb_res %>% select_best("roc_auc")

best_xgb_auc <-
  select_best(xgb_res, "roc_auc")

final_xgb <- finalize_model(
  xgb_spec,
  best_xgb_auc
)

# examine feature importance ----------------------------------------------

# random forest

vip_rf_no_prior_lapse <- final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(craving_did_smoke ~ .,
      data = juice(rf_prep)) %>%
  vip()

if(!file.exists(here("outputs", "vip_rf_no_prior_lapse.png"))) ggsave(vip_rf_no_prior_lapse, filename = here("outputs", "vip_rf_no_prior_lapse.png"), 
                                                       dpi = 320, height = 8, width = 10)

# support vector machine

svm_spec_best <- svm_rbf(
  cost = best_svm_auc$cost, #input values from best performing SVM above
  rbf_sigma = best_svm_auc$rbf_sigma
) %>%
  set_mode("classification") %>%
  set_engine("kernlab")

svm_fit <- workflow() %>%
  add_model(svm_spec_best) %>%
  add_formula(craving_did_smoke ~ .) %>%
  fit(juice(rf_prep)) 

vip_svm_no_prior_lapse <- svm_fit %>%
  extract_fit_parsnip() %>%
  vip(method = "permute", 
      target = "craving_did_smoke", metric = "auc", reference_class = "true",
      pred_wrapper = kernlab::predict, train = juice(rf_prep))

if(!file.exists(here("outputs", "vip_svm_no_prior_lapse.png"))) ggsave(vip_svm_no_prior_lapse, filename = here("outputs", "vip_svm_no_prior_lapse.png"), 
                                                        dpi = 320, height = 8, width = 10)

# elastic net

vip_elnet_no_prior_lapse <- final_elnet %>%
  set_engine("glmnet", importance = "permutation") %>%
  fit(craving_did_smoke ~ .,
      data = juice(rf_prep)) %>%
  vip()

if(!file.exists(here("outputs", "vip_elnet_no_prior_lapse.png"))) ggsave(vip_elnet_no_prior_lapse, filename = here("outputs", "vip_elnet_no_prior_lapse.png"), 
                                                          dpi = 320, height = 8, width = 10)

# xgboost

vip_xgb_no_prior_lapse <- final_xgb %>%
  set_engine("xgboost") %>%
  fit(craving_did_smoke ~ .,
      data = juice(rf_prep)) %>%
  vip()

if(!file.exists(here("outputs", "vip_xgb_no_prior_lapse.png"))) ggsave(vip_xgb_no_prior_lapse, filename = here("outputs", "vip_xgb_no_prior_lapse.png"), 
                                                        dpi = 320, height = 8, width = 10)

# fit final model to test data --------------------------------------------

# random forest

final_rf_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(final_rf)

final_rf_res <- final_rf_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))

final_rf_res %>%
  collect_metrics()

roc_rf <- final_rf_res %>%
  collect_predictions() %>%
  select(.pred_false, craving_did_smoke)

roc_rf_obj <- roc(roc_rf$craving_did_smoke, roc_rf$.pred_false)
auc(roc_rf_obj)
ci.auc(roc_rf_obj)

# support vector machine

final_svm_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(final_svm)

final_svm_res <- final_svm_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))

final_svm_res %>%
  collect_metrics()

roc_svm <- final_svm_res %>%
  collect_predictions() %>%
  select(.pred_false, craving_did_smoke)

roc_svm_obj <- roc(roc_svm$craving_did_smoke, roc_svm$.pred_false)
auc(roc_svm_obj)
ci.auc(roc_svm_obj)

# elastic net

final_elnet_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(final_elnet)

final_elnet_res <- final_elnet_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))

final_elnet_res %>%
  collect_metrics()

roc_elnet <- final_elnet_res %>%
  collect_predictions() %>%
  select(.pred_false, craving_did_smoke)

roc_elnet_obj <- roc(roc_elnet$craving_did_smoke, roc_elnet$.pred_false)
auc(roc_elnet_obj)
ci.auc(roc_elnet_obj)

# xgboost

final_xgb_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(final_xgb)

final_xgb_res <- final_xgb_wf %>%
  last_fit(data_split, metrics = metric_set(roc_auc, sens, spec, accuracy))

final_xgb_res %>%
  collect_metrics()

roc_xgb <- final_xgb_res %>%
  collect_predictions() %>%
  select(.pred_false, craving_did_smoke)

roc_xgb_obj <- roc(roc_xgb$craving_did_smoke, roc_xgb$.pred_false)
auc(roc_xgb_obj)
ci.auc(roc_xgb_obj)

# generate confusion matrices ---------------------------------------------

# random forest

final_rf_res %>%
  collect_predictions() %>%
  conf_mat(truth = craving_did_smoke, estimate = .pred_class)

# support vector machine

final_svm_res %>%
  collect_predictions() %>%
  conf_mat(truth = craving_did_smoke, estimate = .pred_class)

# elastic net

final_elnet_res %>%
  collect_predictions() %>%
  conf_mat(truth = craving_did_smoke, estimate = .pred_class)

# xgboost

final_xgb_res %>%
  collect_predictions() %>%
  conf_mat(truth = craving_did_smoke, estimate = .pred_class)

# prepare for model comparison --------------------------------------------

# random forest

rf_auc <-
  final_rf_res %>%
  collect_predictions() %>%
  roc_curve(craving_did_smoke, .pred_false) %>%
  mutate(model = "Random Forest")

# support vector machine

svm_auc <-
  final_svm_res %>%
  collect_predictions() %>%
  roc_curve(craving_did_smoke, .pred_false) %>%
  mutate(model = "Support Vector Machine")

# elastic net

elnet_auc <-
  final_elnet_res %>%
  collect_predictions() %>%
  roc_curve(craving_did_smoke, .pred_false) %>%
  mutate(model = "Penalised Logistic Regression")

# xgboost

xgb_auc <-
  final_xgb_res %>%
  collect_predictions() %>%
  roc_curve(craving_did_smoke, .pred_false) %>%
  mutate(model = "XGBoost")

# compare model performance

combined_roc_no_prior_lapse <- bind_rows(rf_auc, elnet_auc, svm_auc, xgb_auc) %>%
  mutate(model = as_factor(model), 
         model = fct_relevel(model, levels = c("Random Forest", "XGBoost", "Support Vector Machine", "Penalised Logistic Regression"))) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) +
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6) +
  xlab("1 - Specificity") + 
  ylab("Sensitivity") +
  labs(col = "Model") +
  theme_minimal()

if(!file.exists(here("outputs", "plot_combined_roc_no_prior_lapse.png"))) ggsave(combined_roc_no_prior_lapse, filename = here("outputs", "plot_combined_roc_no_prior_lapse.png"), 
                                                                  dpi = 320, height = 8, width = 10)
