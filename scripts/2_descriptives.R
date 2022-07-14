# load packages -----------------------------------------------------------

source(here::here("scripts", "0_load_packages.R"))

#load data ---------------------------------------------------------------

data_clean <- read_rds(here::here("data", "clean_data.rds"))

#sample characteristics --------------------------------------------------

#entire sample

entire_sample <- data_clean %>%
  group_by(account_id) %>%
  filter(row_number()==1)

entire_sample %>%
  janitor::tabyl(time_to_first_cig)

mean(entire_sample$cigs_per_day)
sd(entire_sample$cigs_per_day)

nr_craving_entries_1a <- data_clean %>%
  group_by(account_id) %>%
  summarise(count = n())

median(nr_craving_entries_1a$count)
range(nr_craving_entries_1a$count)

#lapses

lapse_count_1a <- data_clean %>%
  group_by(account_id, craving_did_smoke) %>%
  summarise(n_lapses = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = craving_did_smoke, values_from = n_lapses) %>%
  filter(true > 5)

#non-lapses

lapse_count_2a <- data_clean %>%
  group_by(account_id, craving_did_smoke) %>%
  summarise(n_lapses = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = craving_did_smoke, values_from = n_lapses) %>%
  filter(false > 5)

#lapses and non-lapses

lapse_count_3a <- data_clean %>%
  group_by(account_id, craving_did_smoke) %>%
  summarise(n_lapses = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = craving_did_smoke, values_from = n_lapses) %>%
  filter(false > 5 & true > 5)

#sample with 10+ craving entries

sample_10 <- data_clean %>%
  group_by(account_id) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 10)

sample_10_descr <- sample_10 %>%
  group_by(account_id) %>%
  filter(row_number()==1)

sample_10_descr %>%
  janitor::tabyl(time_to_first_cig)

mean(sample_10_descr$cigs_per_day)
sd(sample_10_descr$cigs_per_day)

nr_craving_entries_1c <- sample_10 %>%
  group_by(account_id) %>%
  summarise(count = n())

median(nr_craving_entries_1c$count)
range(nr_craving_entries_1c$count)

#lapses

lapse_count_1c <- sample_10 %>%
  group_by(account_id, craving_did_smoke) %>%
  summarise(n_lapses = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = craving_did_smoke, values_from = n_lapses) %>%
  filter(true > 5)

#non-lapses

lapse_count_2c <- sample_10 %>%
  group_by(account_id, craving_did_smoke) %>%
  summarise(n_lapses = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = craving_did_smoke, values_from = n_lapses) %>%
  filter(false > 5)

#lapses and non-lapses

lapse_count_3c <- sample_10 %>%
  group_by(account_id, craving_did_smoke) %>%
  summarise(n_lapses = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = craving_did_smoke, values_from = n_lapses) %>%
  filter(false > 5 & true > 5)

#sample with 20+ craving entries

sample_20 <- data_clean %>%
  group_by(account_id) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n >= 20)

sample_20_descr <- sample_20 %>%
  group_by(account_id) %>%
  filter(row_number()==1)

sample_20_descr %>%
  janitor::tabyl(time_to_first_cig)

mean(sample_20_descr$cigs_per_day)
sd(sample_20_descr$cigs_per_day)

nr_craving_entries_1b <- sample_20 %>%
  group_by(account_id) %>%
  summarise(count = n())

median(nr_craving_entries_1b$count)
range(nr_craving_entries_1b$count)

#lapses

lapse_count_1b <- sample_20 %>%
  group_by(account_id, craving_did_smoke) %>%
  summarise(n_lapses = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = craving_did_smoke, values_from = n_lapses) %>%
  filter(true > 5)

#non-lapses

lapse_count_2b <- sample_20 %>%
  group_by(account_id, craving_did_smoke) %>%
  summarise(n_lapses = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = craving_did_smoke, values_from = n_lapses) %>%
  filter(false > 5)

#lapses and non-lapses

lapse_count_3b <- sample_20 %>%
  group_by(account_id, craving_did_smoke) %>%
  summarise(n_lapses = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = craving_did_smoke, values_from = n_lapses) %>%
  filter(false > 5 & true > 5)

#continued descriptives for participants with 20+ craving entries --------

#proportion lapses vs. non-lapses

sample_20 <- droplevels(sample_20)

sum(sample_20$craving_did_smoke == "true")/37002*100
sum(sample_20$craving_did_smoke == "false")/37002*100

#proportion lapses vs. non-lapses, split by participant

lapses <- sample_20 %>%
  janitor::tabyl(account_id, craving_did_smoke) %>%
  mutate(prop = round(true/(false+true), digits = 2))

sum(lapses$prop == 0)/791*100
sum(lapses$prop == 1)/791*100
sum(lapses$prop > 0.7)/791*100
sum(lapses$prop > 0.6)/791*100
sum(lapses$prop > 0.5)/791*100

plot_prop_lapses <- ggplot(lapses, aes(prop)) + 
  geom_histogram(colour = "black", fill = "white") +
  geom_vline(aes(xintercept = median(prop)), col='blue', linetype="dashed", size=1) +
  xlab("Proportion lapses (vs. non-lapses)") + 
  ylab("Number of participants") +
  theme_minimal()

if(!file.exists(here("outputs", "descriptives", "supp_fig_prop_lapses.png"))) ggsave(plot_prop_lapses, filename = here("outputs", "descriptives", "supp_fig_prop_lapses.png"), dpi = 320, height = 8, width = 10)

#plot maximum days since quit date

days_since_quit <- sample_20 %>%
  group_by(account_id) %>%
  summarise(days = max(days_since_qd))

plot_days <- ggplot(days_since_quit, aes(days)) + 
  geom_histogram(colour = "black", fill = "white") +
  geom_vline(aes(xintercept = median(days)), col='blue', linetype="dashed", size=1) +
  xlab("Maximum days since the quit date") + 
  ylab("Number of participants") +
  theme_minimal()

if(!file.exists(here("outputs", "descriptives", "supp_fig_days_since_qd.png"))) ggsave(plot_days, filename = here("outputs", "descriptives", "supp_fig_days_since_qd.png"), dpi = 320, height = 8, width = 10)

#plot median minutes between craving entries

sample_20$mins_since_prev_rep[is.na(sample_20$mins_since_prev_rep)] <- 0 #as each participant's first craving entry lacks a value (can't be computed), impute 0

median(sample_20$mins_since_prev_rep, na.rm = TRUE)
range(sample_20$mins_since_prev_rep, na.rm = TRUE)

mins_between_entries <- sample_20 %>%
  group_by(account_id) %>%
  summarise(mins = median(mins_since_prev_rep))

plot_mins <- ggplot(mins_between_entries, aes(mins)) + 
  geom_histogram(colour = "black", fill = "white") +
  geom_vline(aes(xintercept = median(mins)), col='blue', linetype="dashed", size=1) +
  xlab("Median minutes since previous craving entry") + 
  ylab("Number of participants") +
  theme_minimal()

if(!file.exists(here("outputs", "descriptives", "supp_fig_median_mins_between_entries.png"))) ggsave(plot_mins, filename = here("outputs", "descriptives", "supp_fig_median_mins_between_entries.png"), dpi = 320, height = 8, width = 10)

#number of participants who reported smoking on their last craving entry

sample_20 %>%
  group_by(account_id) %>%
  summarise_all(last) %>%
  janitor::tabyl(craving_did_smoke)

#proportion of lapses with the subsequent craving entry being a lapse

next_event_lapse <- sample_20 %>%
  group_by(account_id) %>%
  mutate(next_event_lapse = as.factor(case_when(lead(craving_did_smoke) == "true" ~ "yes",
                                                TRUE ~ "no")))

check <- next_event_lapse %>%
  select(craving_did_smoke, next_event_lapse)

sum(check$craving_did_smoke == "true") # number of lapses
sum(check$craving_did_smoke == "true" & check$next_event_lapse == "yes")/2815*100 # prop lapses followed by another lapse

#proportion of lapses with the predictor variables endorsed (vs. not) ----------------

exclude <- c("adjusted_quit_date", "adjusted_craving_record_created", "n", "account_id", "cigs_per_day", 
             "craving_severity", "event_nr", "days_since_qd", "mins_since_prev_rep")

df <- sample_20 %>%
    group_by(account_id) %>%
    mutate(lapse_events = n(),
           prop_lapses = sum(craving_did_smoke == "true", na.rm = T)/lapse_events) %>%
  select(-any_of(exclude))

plot_list <- list()

for(each in 3:ncol(df)) {
  column <- names(df[, each])
  legend_lab <- sub('.*\\_', '', names(df[, each]))
  x_var <- names(df[, 2])
  xlabs <- paste(str_to_sentence(levels(df$craving_did_smoke)),"\n(N=",table(df$craving_did_smoke),")",sep="")
  
  plot_list[[each]] <- ggplot(data = df, aes_string(x = x_var, fill = column)) +
  geom_bar(position = "fill") +
  scale_y_continuous(name = "Proportion") +
  scale_x_discrete(labels = xlabs) +
    labs(fill = legend_lab)

}

plot_1 <- plot_grid(plotlist = plot_list[3:15])
plot_2 <- plot_grid(plotlist = plot_list[16:28])
plot_3 <- plot_grid(plotlist = plot_list[29:36])

if(!file.exists(here("outputs", "descriptives", "supp_fig_1_pred_vars.png"))) ggsave(plot_1, filename = here("outputs", "descriptives", "supp_fig_1_pred_vars.png"), dpi = 320, height = 14, width = 20)
if(!file.exists(here("outputs", "descriptives", "supp_fig_2_pred_vars.png"))) ggsave(plot_2, filename = here("outputs", "descriptives", "supp_fig_2_pred_vars.png"), dpi = 320, height = 14, width = 20)
if(!file.exists(here("outputs", "descriptives", "supp_fig_3_pred_vars.png"))) ggsave(plot_1, filename = here("outputs", "descriptives", "supp_fig_3_pred_vars.png"), dpi = 320, height = 14, width = 20)

#cigarettes per day and lapses

cpd_lapses <- ggplot(data = sample_20, aes(x = cigs_per_day, fill = craving_did_smoke)) +
  geom_histogram()

if(!file.exists(here("outputs", "descriptives", "supp_fig_cpd_lapses.png"))) ggsave(cpd_lapses, filename = here("outputs", "descriptives", "supp_fig_cpd_lapses.png"), dpi = 320, height = 14, width = 20)

#craving severity and lapses

craving_severity_lapses <- ggplot(data = sample_20, aes(x = craving_severity, fill = craving_did_smoke)) +
  geom_histogram()

if(!file.exists(here("outputs", "descriptives", "supp_fig_craving_severity_lapses.png"))) ggsave(craving_severity_lapses, filename = here("outputs", "descriptives", "supp_fig_craving_severity_lapses.png"), dpi = 320, height = 14, width = 20)

#days since quit date and lapses

days_since_qd_lapses <- ggplot(data = sample_20, aes(x = days_since_qd, fill = craving_did_smoke)) +
  geom_histogram()

if(!file.exists(here("outputs", "descriptives", "supp_fig_days_since_qd_lapses.png"))) ggsave(days_since_qd_lapses, filename = here("outputs", "descriptives", "supp_fig_days_since_qd_lapses.png"), dpi = 320, height = 14, width = 20)