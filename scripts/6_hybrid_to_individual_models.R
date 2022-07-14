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
             "event_nr", "n")

df <- data %>%
  select(-all_of(exclude))

df$mins_since_prev_rep[is.na(df$mins_since_prev_rep)] <- 0

# remove IDs with all/no lapses

df_split <- df %>%
  group_by(account_id) %>%
  mutate(lapse_events = n()) %>%
  mutate(prop_lapses = sum(craving_did_smoke == "true", na.rm = T)/lapse_events) %>%
  filter(prop_lapses > 0 & prop_lapses < 1) %>%
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
  
  df_split_training[[i]] <- df_split_training[[i]] %>% # select random 20% of testing rows and included in training set
    mutate(row_number = row_number())
  
  tmp <- tibble()
  
  tmp <- df_split_training[[i]][sample(which(df_split_training[[i]]$train_split=="testing"),
                                     round(0.2*length(which(df_split_training[[i]]$train_split=="testing")))),]
  tmp_2 <- tibble()
  
  tmp_2 <- anti_join(df_split_training[[i]], tmp, by = "row_number")
  
  tmp_3 <- tibble()
  
  tmp_3 <- tmp %>%
    mutate(train_split = "training")
  
  account_id <- as.character(unique(tmp_3$account_id))
  
  df_split_training[[i]] <- bind_rows(tmp_2, tmp_3)
  
  df_split_training[[i]] <- df_split_training[[i]] %>%
    select(-c("account_id", "lapse_events", "prop_lapses", "row_number"))
  
  names(df_split_training)[[i]] <- account_id
  
}

# fit best-performing group-level model to individual data ---------------------------

df_model_fit_hybrid <- list()

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
  
  rf_spec <- rand_forest(
    mtry = 3,
    trees = 500,
    min_n = 13
  ) %>%
    set_mode("classification") %>%
    set_engine("ranger")
  
  wf_rf <- workflow() %>%
    add_recipe(rf_rec) %>%
    add_model(rf_spec)
  
  final_rf_workflow_fit <- fit(wf_rf, data = train_data)
  
  test_data$.pred_true <- predict(final_rf_workflow_fit, new_data = test_data, type = "prob")$.pred_true
  
  roc_test_error <- tryCatch(roc(test_data$craving_did_smoke, test_data$.pred_true), error = function(e) e)
  
  if(any(class(roc_test_error) == "error")) {
    
    message("Error")
    
  } else {
    
    roc_obj <- roc(test_data$craving_did_smoke, test_data$.pred_true)
    auc <- auc(roc_obj)
    
    df_model_fit_hybrid[[i]] <- list(auc = auc)
    
    names(df_model_fit_hybrid)[[i]] <- names(df_split_training)[[i]]
  }

}

write_rds(df_model_fit_hybrid, here("data", "df_model_fit_hybrid.rds"))

# summarise model performance for each individual -------------------------

df_model_fit_hybrid <- read_rds(here::here("data", "df_model_fit_hybrid.rds"))

model_performance_hybrid <- tibble(account_id = names(df_model_fit_hybrid[lengths(df_model_fit_hybrid) >= 1]),
                                   auc = unlist(map(df_model_fit_hybrid, "auc")))

median(model_performance_hybrid$auc)
range(model_performance_hybrid$auc)

# compare to group model --------------------------------------------------

group_auc <- read_rds(here("data", "group models", "group_to_individual.rds"))

hybrid_to_group <- model_performance_hybrid %>%
  select(account_id, model_auc = auc) %>%
  left_join(group_auc %>%
              rename(group_auc = auc), by = c("account_id")) %>%
  mutate(hybrid_preferred = model_auc >= group_auc)

sum(hybrid_to_group$hybrid_preferred == TRUE)/184*100
sum(hybrid_to_group$hybrid_preferred == FALSE)/184*100

hybrid_to_group_plot <- hybrid_to_group %>%
  ggplot() +
  geom_point(aes(x = group_auc, y = model_auc, colour = hybrid_preferred)) +
  geom_abline() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  scale_colour_viridis_d() +
  theme_bw() +
  labs(x = "Group-level algorithm AUC",
       y = "Hybrid algorithm AUC") +
  theme(legend.position = "none")

if(!file.exists(here("outputs", "hybrid models", "hybrid_to_group_plot.png"))) ggsave(hybrid_to_group_plot, 
                                                                                              filename = here("outputs", "hybrid models", "hybrid_to_group_plot.png"),
                                                                                              dpi = 320, height = 8, width = 10)
